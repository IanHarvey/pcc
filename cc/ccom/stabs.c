#if 0
static char *sccsid ="@(#)stab.c	1.13 (Berkeley) 12/11/87";
#endif
/*
 * Symbolic debugging info interface.
 *
 * Here we generate pseudo-ops that cause the assembler to put
 * symbolic debugging information into the object file.
 */

#include "pass1.h"

#include <sys/types.h>
#include <stab.h>

#define	STABHASH	256
#define	INTNUM		1	/* internal number of type "int" */
#define	BIT2BYTE(x)	((x)/SZCHAR)

/*
 * Local type mapping
 * Types are defined as a typeword, a dimension pointer (in the case
 * of arrays) and struct/union/enum declarations.
 * Function prototypes are ignored.
 */
static struct stabtype {
	struct stabtype *next;	/* linked list */
	TWORD type;		/* pcc type number */
	union dimfun *df;	/* dimension of arrays */
	struct suedef *sue;	/* struct/union/enum declarations */
	int num;		/* local type number */
} *stabhash[STABHASH];
static int ntypes;
static char *curfun;
static int stablbl = 10;

void ptype(char *name, int num, int inhnum, long long min, long long max);
struct stabtype *addtype(TWORD, union dimfun *, struct suedef *);
struct stabtype *findtype(TWORD t, union dimfun *df, struct suedef *sue);
void printtype(struct symtab *s);

/*
 * Output type definitions for the stab debugging format.
 * Note that "int" is always internal number 1.
 */
void
stabs_init()
{
	struct stabtype *st;

#define	ADDTYPE(y) addtype(y, NULL, MKSUE(y))

	ptype("int", ADDTYPE(INT)->num, INTNUM, MIN_INT, MAX_INT);

	st = ADDTYPE(CHAR);
	ptype("char", st->num, st->num, 0, MAX_CHAR);
	ptype("short", ADDTYPE(SHORT)->num, INTNUM, MIN_SHORT, MAX_SHORT);
	ptype("long", ADDTYPE(LONG)->num, INTNUM, MIN_LONG, MAX_LONG);
	ptype("long long", ADDTYPE(LONGLONG)->num, INTNUM,
	     MIN_LONGLONG, MAX_LONGLONG);
	ptype("unsigned char", ADDTYPE(UCHAR)->num, INTNUM, 0, MAX_UCHAR);
	ptype("unsigned short", ADDTYPE(USHORT)->num, INTNUM, 0, MAX_USHORT);
	ptype("unsigned int", ADDTYPE(UNSIGNED)->num, INTNUM, 0, MAX_UNSIGNED);
	ptype("unsigned long", ADDTYPE(ULONG)->num, INTNUM, 0, MAX_ULONG);
	ptype("unsigned long long", ADDTYPE(ULONGLONG)->num, INTNUM,
	    0, MAX_ULONGLONG);

	ptype("float", ADDTYPE(FLOAT)->num, INTNUM, 4, 0);
	ptype("double", ADDTYPE(DOUBLE)->num, INTNUM, 8, 0);
	ptype("long double", ADDTYPE(LDOUBLE)->num, INTNUM, 12, 0);
	st = ADDTYPE(VOID);
	printf("	.stabs \"void:t%d=r%d\",%d,0,0,0\n",
	    st->num, st->num, N_LSYM);

}

/*
 * Print a type in stabs format
 */
void
ptype(char *name, int num, int inhnum, long long min, long long max)
{
	printf("	.stabs \"%s:t%d=r%d;%lld;%lld;\",%d,0,0,0\n",
	    name, num, inhnum, min, max, N_LSYM);
}

/*
 * Add a new local type to the hash table.
 * The search key is the (type, df, sue) triple.
 */
struct stabtype *
addtype(TWORD t, union dimfun *df, struct suedef *sue)
{
	struct stabtype *st;

	st = permalloc(sizeof(struct stabtype));
	st->type = t;
	st->df = df;
	st->sue = sue;
	st->num = ++ntypes;
	st->next = stabhash[t & (STABHASH-1)];
	stabhash[t & (STABHASH-1)] = st;
	return st;
}

/*
 * Search for a given type and return a type pointer (or NULL).
 */
struct stabtype *
findtype(TWORD t, union dimfun *df, struct suedef *sue)
{
	struct stabtype *st;
	union dimfun *dw, *dx;
	TWORD tw;

	st = stabhash[t & (STABHASH-1)];
	for (; st; st = st->next) {
		if (t != st->type || sue != st->sue)
			continue;
		/* Ok, type and sue matches, check dimensions */
		if (st->df == NULL)
			return st; /* no arrays, got match */
		dw = st->df;
		dx = df;
		tw = t;
		for (; tw > BTMASK; tw = DECREF(tw)) {
			if (ISARY(tw)) {
				if (dw->ddim == dx->ddim)
					dw++, dx++;
				else
					break;
			}
		}
		if (tw <= BTMASK)
			return st;
	}
	return NULL;
}

/*
 * Print current line number.
 */
void
stabs_line(int line)
{
	send_passt(IP_LOCCTR, PROG);
	printf("	.stabn %d,0,%d," STABLBL "-%s\n", N_SLINE, line,
	    stablbl, curfun);
	printf(STABLBL ":\n", stablbl++);
}

/*
 * Start of block.
 */
void
stabs_lbrac(int blklvl)
{
	printf("	.stabn %d,0,%d," STABLBL "-%s\n",
	    N_LBRAC, blklvl, stablbl, curfun);
	printf(STABLBL ":\n", stablbl++);
}

/*
 * End of block.
 */
void
stabs_rbrac(int blklvl)
{
	printf("	.stabn %d,0,%d," STABLBL "-%s\n",
	    N_RBRAC, blklvl, stablbl, curfun);
	printf(STABLBL ":\n", stablbl++);
}

/*
 * Print current file and set mark.
 */
void
stabs_file(char *fname)
{
	static char *mainfile;

	send_passt(IP_LOCCTR, PROG);
	if (mainfile == NULL)
		mainfile = fname; /* first call */
	printf("	.stabs	\"%s\",%d,0,0," STABLBL "\n",
	    fname, fname == mainfile ? N_SO : N_SOL, stablbl);
	printf(STABLBL ":\n", stablbl++);
}

/*
 * Print beginning of function.
 */
void
stabs_func(struct symtab *s)
{
	curfun = s->sname;
	printf("	.stabs  \"%s:%c", s->sname,
	    s->sclass == STATIC ? 'f' : 'F');
	printtype(s);
	printf("\",%d,0,%d,%s\n", N_FUN,
	    BIT2BYTE(s->ssue->suesize), exname(s->sname));
}

/*
 * Print a (complex) type.
 * Will also create subtypes.
 * Printed string is like "20=*21=*1".
 */
void
printtype(struct symtab *s)
{
	struct stabtype *st;
	union dimfun *df = s->sdf;
	struct suedef *sue = s->ssue;
	TWORD t = s->stype;

	/* Print out not-yet-found types */
	if (ISFTN(t))
		t = DECREF(t);
	st = findtype(t, df, sue);
	while (st == NULL && t > BTMASK) {
		st = addtype(t, df, sue);
		printf("%d=", st->num);
		if (ISFTN(t))
			putchar('f');
		else if (ISPTR(t))
			putchar('*');
		else if (ISARY(t))
			printf("ar%d;0;%d;", INTNUM, df->ddim-1);
		else
			cerror("printtype: notype");
		if (ISARY(t))
			df++;
		t = DECREF(t);
		st = findtype(t, df, sue);
	}
	/* print out basic type. may have to be entered in case of sue */
	if (st == NULL) {
/* 		cerror("fix printtype"); */
		printf("%d", 1);
	} else
		printf("%d", st->num);
}

void
stabs_newsym(struct symtab *s)
{

	if (ISFTN(s->stype))
		return; /* functions are handled separate */

	if (s->sclass == STNAME || s->sclass == UNAME || s->sclass == MOS ||
	    s->sclass == ENAME || s->sclass == MOU || s->sclass == MOE ||
	    s->sclass == TYPEDEF || (s->sclass | FIELD))
		return; /* XXX - fix structs */

	printf("	.stabs \"%s:", s->sname);
	switch (s->sclass) {
	case AUTO:
		printtype(s);
		printf("\",%d,0,%d,%d\n", N_LSYM, BIT2BYTE(s->ssue->suesize),
		   BIT2BYTE(s->soffset));
		break;

	case STATIC:
		putchar(blevel ? 'V' : 'S');
		printtype(s);
		printf("\",%d,0,%d,", N_LCSYM, BIT2BYTE(s->ssue->suesize));
		if (blevel)
			printf(LABFMT "\n", s->soffset);
		else
			printf("%s\n", exname(s->sname));
		break;

	case EXTERN:
	case EXTDEF:
		putchar('G');
		printtype(s);
		printf("\",%d,0,%d,0\n", N_GSYM, BIT2BYTE(s->ssue->suesize));
		break;

	default:
		cerror("fix stab_newsym; class %d", s->sclass);
	}
}

void
stabs_chgsym(struct symtab *s)
{
}

/* --------------------------------------------- */

#define private static
#define and &&
#define or ||
#define not !
#define div /
#define mod %
#define nil 0

/*
 * sdf = arrindex, tarray (struct dimfun)
 */

#define bytes(bits) ((bits) / SZCHAR)
#define bsize(p) bytes(p->ssue->suesize)	/* size in bytes of a symbol */

#define NILINDEX -1
#define	NILDF	(union dimfun *)0
#define	NILSUE	(struct suedef *)0
#define	NILSTR	(char *)0
static struct suedef forw;
#define FORWARD (&forw)

typedef int Boolean;

#define false 0
#define true 1

extern int ddebug;
extern int gflag;

#ifndef STABLBL
#error macdefs.h must define STABLBL
#endif

int stabLCSYM;

static int entertype(TWORD, union dimfun *, struct suedef *, char *);
static void gentype(struct symtab *sym);
static void geninfo(register struct symtab *p);
static void genstruct(TWORD, int, struct suedef *, char *name, int);

/*
 * Generate debugging info for a parameter.
 * The offset isn't known when it is first entered into the symbol table
 * since the types are read later.
 */

void
fixarg(struct symtab *p)
{
return;
    if (gflag) {
	printf("\t.stabs\t\"%s:p", p->sname);
	gentype(p);
	printf("\",0x%x,0,%d,%d\n", N_PSYM, bsize(p), bytes(argoff));
    }
}

/*
 * Determine if the given symbol is a global array with dimension 0,
 * which only makes sense if it's dimension is to be given later.
 * We therefore currently do not generate symbol information for
 * such entries.
 */

#define isglobal(class) ( \
    class == EXTDEF or class == EXTERN or class == STATIC \
)

private Boolean
zero_length_array(register struct symtab *p)
{
    Boolean b;
    int t;

    if (not isglobal(p->sclass)) {
	b = false;
    } else {
	t = p->stype;
	if (ISFTN(t)) {
	    t = DECREF(t);
	}
	b = (Boolean) (ISARY(t) and p->sdf->ddim == 0);
    }
    return b;
}

/*
 * Generate debugging info for a given symbol.
 */

void
outstab(struct symtab *sym)
{
    register struct symtab *p;
    char *classname = 0;
    Boolean ignore;

    if (gflag and not zero_length_array(sym)) {
	ignore = false;
	p = sym;
	switch (p->sclass) {
	case REGISTER:
	    classname = "r";
	    break;

	/*
	 * Locals are the default class.
	 */
	case AUTO:
	    classname = "";
	    break;

	case STATIC:
	    if (ISFTN(p->stype)) {
		ignore = true;
	    } else if (p->slevel <= 1) {
		classname = "S";
	    } else {
		classname = "V";
	    }
	    break;

	case EXTDEF:
	case EXTERN:
	    if (ISFTN(p->stype)) {
		ignore = true;
	    } else {
		classname = "G";
	    }
	    break;

	case TYPEDEF:
	    classname = "t";
	    break;

	case PARAM:
	case MOS:
	case MOU:
	case MOE:
	    ignore = true;
	    break;

	case ENAME:
	case UNAME:
	case STNAME:
	    (void) entertype(p->stype, NILDF, FORWARD, p->sname);
	    ignore = true;
	    break;

	default:
	    if ((p->sclass&FIELD) == 0) {
		printf("/* no info for %s (%d) */\n", p->sname, p->sclass);
	    }
	    ignore = true;
	    break;
	}
	if (not ignore) {
	    printf("\t.stabs\t\"%s:%s", p->sname, classname);
	    gentype(p);
	    geninfo(p);
	}
    }
}

/*
 * Since type names are lost in the travels and because C has
 * structural type equivalence we keep a table of type words that
 * we've already seen.  The first time we see a type, it is assigned
 * (inline) a number and future references just list that number.
 * Structures, unions, enums, and arrays must be handled carefully
 * since not all the necessary information is in the type word.
 */

typedef struct Typeid *Typeid;

struct Typeid {
    TWORD tword;		/* plain type */
	union dimfun *tdf;	/* dimension */
	struct suedef *tsue;
	char *tname;		/* identifier (name) */
    int tnum;
    Typeid chain;
};

#define TABLESIZE 2003

static int tcount = 1;
static int t_int;
static Typeid typetable[TABLESIZE];

static Typeid typelookup(TWORD, union dimfun *, struct suedef *, char *);
static void reentertype(Typeid, TWORD, union dimfun *, struct suedef *, char *);

/*
 * Look for the given type word in the type table.
 */
static Typeid
typelookup(type, adf, sue, name)
TWORD type;
	union dimfun *adf;
	struct suedef *sue;
char *name;
{
    register TWORD tword;
	union dimfun *i1, *i2;
    Typeid t;

    t = typetable[type mod TABLESIZE];
    while (t != nil) {
	if (t->tword == type and
	  sue == t->tsue and name == t->tname) {
	    if (adf == NILDF) {
		break;
	    } else {
		tword = type;
		i1 = adf;
		i2 = t->tdf;
		while (ISARY(tword) and i1->ddim == i2->ddim) {
		    ++i1;
		    ++i2;
		    tword >>= TSHIFT;
		}
		if (!ISARY(tword)) {
		    break;
		}
	    }
	}
	t = t->chain;
    }
    return t;
}

/*
 * Enter a type word and associated symtab indices into the type table.
 */

static int
entertype(TWORD type, union dimfun *adf, struct suedef *sue, char *strp)
{
    register Typeid t;
    register int i;

    t = (Typeid) permalloc(sizeof(struct Typeid));
    t->tword = type;
	t->tdf = adf;
	t->tsue = sue;
	t->tname = strp;
    t->tnum = tcount;
    ++tcount;
    i = type mod TABLESIZE;
    t->chain = typetable[i];
    typetable[i] = t;
    return t->tnum;
}

/*
 * Change the information associated with a type table entry.
 * Since I'm lazy this just creates a new entry with the number
 * as the old one.
 */

static void
reentertype(typeid, type, adf, sue, name)
Typeid typeid;
TWORD type;
	union dimfun *adf;
	struct suedef *sue;
char *name;
{
    register Typeid t;
    register int i;

    t = (Typeid) permalloc(sizeof(struct Typeid));
    t->tword = type;
	t->tdf = adf;
	t->tsue = sue;
	t->tname = name;
    t->tnum = typeid->tnum;
    i = type mod TABLESIZE;
    t->chain = typetable[i];
    typetable[i] = t;
}

/*
 * Generate debugging information for the given type of the given symbol.
 */

static void
gentype(struct symtab *sym)
{
    register struct symtab *p;
    register TWORD t;
    register TWORD basictype;
    register Typeid typeid;
	union dimfun *sdf, *i;
	struct suedef *sue;
	char *name;

    p = sym;
    t = p->stype;
    if (ISFTN(t)) {
	t = DECREF(t);
    }
    basictype = BTYPE(t);
    if (ISARY(t)) {
	sdf = p->sdf;
    } else {
	sdf = NILDF;
    }
    if (basictype == STRTY or basictype == UNIONTY or basictype == ENUMTY) {
	sue = p->ssue;
	if (sue == NILSUE) {
	    sue = FORWARD;
	    name = p->sname;
	} else {
	    name = NILSTR;
	}
    } else {
	sue = NILSUE;
	name = NILSTR;
    }
    i = sdf;
    typeid = typelookup(t, sdf, sue, name);
    while (t != basictype and typeid == nil) {
	printf("%d=", entertype(t, i, sue, name));
	switch (t&TMASK) {
	case PTR:
	    printf("*");
	    break;

	case FTN:
	    printf("f");
	    break;

	case ARY:
	    printf("ar%d;0;%d;", t_int, i->ddim - 1);
		i++;
	    break;
	}
	t = DECREF(t);
	if (i == NILDF && ISARY(t)) {
	    i = p->sdf;
	}
	if (t == basictype) {
	    typeid = typelookup(t, NILDF, sue, name);
	} else {
	    typeid = typelookup(t, i, sue, name);
	}
    }
    if (typeid == nil) {
	if (sue == FORWARD) {
	    typeid = typelookup(t, NILDF, FORWARD, p->sname);
	    if (typeid == nil) {
		cerror("unbelievable forward reference");
	    }
	    printf("%d", typeid->tnum);
	} else {
	    genstruct(t, NILINDEX, sue, p->sname, bsize(p));
	}
    } else {
	printf("%d", typeid->tnum);
    }
}

/*
 * Generate type information for structures, unions, and enumerations.
 */

static void
genstruct(t, structid, sue, name, size)
TWORD t;
int structid;
struct suedef *sue;
char *name;
int size;
{
    register struct symtab *field, **ix;
    int id;

    if (structid == NILINDEX) {
	id = entertype(t, NILDF, sue, NILSTR);
    } else {
	id = structid;
    }
    switch (t) {
    case STRTY:
    case UNIONTY:
	printf("%d=%c%d", id, t == STRTY ? 's' : 'u', size);
	ix = sue->suelem;
	if (ix)
	  while (*ix != NULL) {
	    field = *ix;
	    printf("%s:", field->sname);
	    gentype(field);
	    if (field->sclass > FIELD) {
		printf(",%d,%d;", field->soffset, field->sclass - FIELD);
	    } else {
		printf(",%d,%lld;", field->soffset,
		    tsize(field->stype, field->sdf, field->ssue));
	    }
	    ++ix;
	}
	putchar(';');
	break;

    case ENUMTY:
	printf("%d=e", id);
	ix = sue->suelem;
	while (ix != NULL) {
	    field = *ix;
	    printf("%s:%d,", field->sname, field->soffset);
	    ix++;
	}
	putchar(';');
	break;

    default:
	cerror("couldn't find basic type %d for %s\n", t, name);
	break;
    }
}

/*
 * Generate offset and size info.
 */

private void
geninfo(register struct symtab *p)
{
    int stabtype;

    if (p == nil) {
	printf("\",0x%x,0,0,0\n", N_LSYM);
    } else {
	switch (p->sclass) {
	    case EXTERN:
	    case EXTDEF:
		if (ISFTN(p->stype)) {
		    printf("\",0x%x,0,%d,%s\n", N_FUN, bsize(p), exname(p->sname));
		} else {
		    printf("\",0x%x,0,%d,0\n", N_GSYM, bsize(p));
		}
		break;

	    case STATIC:
		stabtype = p->sclass == STATIC ? N_LCSYM : N_STSYM;
		if (ISFTN(p->stype)) {
		    printf("\",0x%x,0,%d,%s\n", N_FUN, bsize(p),
			exname(p->sname));
		} else if (p->slevel > 1) {
		    printf("\",0x%x,0,%d," LABFMT "\n", stabtype, bsize(p),
			p->soffset);
		} else {
		    printf("\",0x%x,0,%d,%s\n", stabtype, bsize(p),
			exname(p->sname));
		}
		break;

	    case REGISTER:
		printf("\",0x%x,0,%d,%d\n", N_RSYM, bsize(p), p->soffset);
		break;

	    case PARAM:
		printf("\",0x%x,0,%d,%d\n", N_PSYM, bsize(p), bytes(argoff));
		break;

	    default:
		printf("\",0x%x,0,%d,%d\n", N_LSYM, bsize(p), bytes(p->soffset));
		break;
	}
    }
}

/*
 * Generate information for a newly-defined structure.
 */

/*ARGSUSED*/
void
outstruct(struct symtab *p, struct suedef *sue)
{
    register Typeid typeid;
    register int t;

return;
	if (p == NULL || !gflag)
		return;

	typeid = typelookup(p->stype, NILDF, FORWARD, p->sname);
	if (typeid == nil) {
		t = 0;
	} else {
		t = typeid->tnum;
		reentertype(typeid, p->stype, NILDF, sue, NILSTR);
	}
	printf("\t.stabs\t\"%s:T", p->sname);
	genstruct(p->stype, t, sue, p->sname, bsize(p));
	geninfo(p);

}
