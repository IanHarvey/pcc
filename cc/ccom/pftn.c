#if 0
static char *sccsid ="@(#)pftn.c	1.29 (Berkeley) 6/18/90";
#endif

# include "pass1.h"

# include <stdlib.h>

unsigned int offsz;

struct symtab *spname;
struct symtab *cftnsp;
static int strunem;			/* currently parsed member */

struct params;

/*
 * Linked list stack while reading in structs.
 */
struct rstack {
	struct	rstack *rnext;
	int	rinstruct;
	int	rclass;
	int	rstrucoff;
	struct	params *rlparam;
	struct	symtab *rsym;
};

/*
 * Linked list for parameter (and struct elements) declaration.
 */
static struct params {
	struct params *next, *prev;
	struct symtab *sym;
} *lpole, *lparam;
static int nparams;

/*
 * Struct used in array initialisation.
 */
static struct instk {
	struct	instk *in_prev;	/* linked list */
	int	in_sz;   	/* size of array element */
	struct	symtab **in_xp;  /* member in structure initializations */
	int	in_n;  		/* number of initializations seen */
	struct	suedef *in_sue;
	int	*in_d;		/* dimoff */
	TWORD	in_t;		/* type */
	struct	symtab *in_sym; /* stab index */
	int	in_fl;	/* flag which says if this level is controlled by {} */
	OFFSZ	in_off;		/* offset of the beginning of this level */
} *pstk;

/* defines used for getting things off of the initialization stack */

static NODE *arrstk[10];
static int arrstkp;

void fixtype(NODE *p, int class);
int fixclass(int class, TWORD type);
int falloc(struct symtab *p, int w, int new, NODE *pty);
int oalloc(struct symtab *p, int *poff);
static void dynalloc(struct symtab *p, int *poff);
void inforce(OFFSZ n);
void vfdalign(int n);
static void instk(struct symtab *p, TWORD t, int *d, struct suedef *, OFFSZ off);
void gotscal(void);
static void ssave(struct symtab *);

int ddebug = 0;

void
defid(NODE *q, int class)
{
	struct symtab *p;
	TWORD type;
	TWORD stp;
	int scl;
	int *dsym, *ddef;
	int slev, temp;
	int changed;

	if (q == NIL)
		return;  /* an error was detected */

	if (q < node || q >= &node[TREESZ])
		cerror("defid call");

	p = q->n_sp;

# ifndef BUG1
	if (ddebug) {
		printf("defid(%s (%p), ", p->sname, p);
		tprint(q->n_type);
		printf(", %s, (%p,%p)), level %d\n", scnames(class),
		    q->n_dim, q->n_sue, blevel);
	}
# endif

	fixtype(q, class);

	type = q->n_type;
	class = fixclass(class, type);

	stp = p->stype;
	slev = p->slevel;

# ifndef BUG1
	if (ddebug) {
		printf("	modified to ");
		tprint(type);
		printf(", %s\n", scnames(class));
		printf("	previous def'n: ");
		tprint(stp);
		printf(", %s, (%p,%p)), level %d\n",
		    scnames(p->sclass), p->sdim, p->ssue, slev);
	}
# endif

	if (stp == FTN && p->sclass == SNULL)
		goto enter;

	if (blevel==1 && stp!=FARG)
		switch (class) {
		default:
			if (!(class&FIELD))
				uerror( "declared argument %s is missing",
				    p->sname );
		case MOS:
		case STNAME:
		case MOU:
		case UNAME:
		case MOE:
		case ENAME:
		case TYPEDEF:
			;
	}
	if (stp == UNDEF|| stp == FARG)
		goto enter;

	if (type != stp)
		goto mismatch;

	if (blevel > slev && (class == AUTO || class == REGISTER))
		/* new scope */
		goto mismatch;

	/* test (and possibly adjust) dimensions */
	dsym = p->sdim;
	ddef = q->n_dim;
	changed = 0;
	for( temp=type; temp&TMASK; temp = DECREF(temp) ){
		if( ISARY(temp) ){
			if (*dsym == 0) {
				*dsym = *ddef;
				changed = 1;
			} else if (*ddef != 0 && *dsym != *ddef) {
				goto mismatch;
			}
			++dsym;
			++ddef;
		}
	}

	if (changed) {
		FIXDEF(p);
	}

	/* check that redeclarations are to the same structure */
	if ((temp == STRTY || temp == UNIONTY || temp == ENUMTY) &&
	    p->ssue != q->n_sue &&
	    class != STNAME && class != UNAME && class != ENAME) {
		goto mismatch;
	}

	scl = p->sclass;

# ifndef BUG1
	if (ddebug)
		printf("	previous class: %s\n", scnames(scl));
# endif

	if (class&FIELD) {
		/* redefinition */
		if (!falloc(p, class&FLDSIZ, 1, NIL)) {
			/* successful allocation */
			ssave(p);
			return;
		}
		/* blew it: resume at end of switch... */
	} else switch(class) {

	case EXTERN:
		switch( scl ){
		case STATIC:
		case USTATIC:
			if( slev==0 ) return;
			break;
		case EXTDEF:
		case EXTERN:
		case FORTRAN:
		case UFORTRAN:
			return;
			}
		break;

	case STATIC:
		if (scl==USTATIC || (scl==EXTERN && blevel==0)) {
			p->sclass = STATIC;
			if (ISFTN(type))
				cftnsp = p;
			return;
		}
		break;

	case USTATIC:
		if (scl==STATIC || scl==USTATIC)
			return;
		break;

	case TYPEDEF:
		if (scl == class)
			return;
		break;

	case UFORTRAN:
		if (scl == UFORTRAN || scl == FORTRAN)
			return;
		break;

	case FORTRAN:
		if (scl == UFORTRAN) {
			p->sclass = FORTRAN;
			if (ISFTN(type))
				cftnsp = p;
			return;
		}
		break;

	case MOU:
	case MOS:
		if (scl == class) {
			if (oalloc(p, &strucoff))
				break;
			if (class == MOU)
				strucoff = 0;
			ssave(p);
			return;
		}
		break;

	case MOE:
		break;

	case EXTDEF:
		switch (scl) {
		case EXTERN:
			p->sclass = EXTDEF;
			if (ISFTN(type))
				cftnsp = p;
			return;
		case USTATIC:
			p->sclass = STATIC;
			if (ISFTN(type))
				cftnsp = p;
			return;
		}
		break;

	case STNAME:
	case UNAME:
	case ENAME:
		if (scl != class)
			break;
		if (p->ssue->suesize == 0)
			return;  /* previous entry just a mention */
		break;

	case PARAM:
	case AUTO:
	case REGISTER:
		;  /* mismatch.. */
	}

	mismatch:

	if (blevel > slev && class != EXTERN && class != FORTRAN &&
	    class != UFORTRAN) {
		p = hide(p);
		q->n_sp = p;
		goto enter;
	}
	uerror("redeclaration of %s", p->sname);
	if (class==EXTDEF && ISFTN(type))
		cftnsp = p;
	return;

	enter:  /* make a new entry */

# ifndef BUG1
	if(ddebug)
		printf("	new entry made\n");
# endif
	if (type == UNDEF)
		uerror("void type for %s", p->sname);
	p->stype = type;
	p->sclass = class;
	p->slevel = blevel;
	p->soffset = NOOFFSET;
	p->suse = lineno;
	if (class == STNAME || class == UNAME || class == ENAME) {
		p->ssue = permalloc(sizeof(struct suedef));
		p->ssue->suesize = 0;
		p->ssue->suelem = NULL; 
		p->ssue->suealign = ALSTRUCT;
	} else {
		switch (BTYPE(type)) {
		case STRTY:
		case UNIONTY:
		case ENUMTY:
			p->ssue = q->n_sue;
			break;
		default:
			p->ssue = MKSUE(BTYPE(type));
		}
	}

	/* copy dimensions */

	p->sdim = q->n_dim;

	/* allocate offsets */
	if (class&FIELD) {
		(void) falloc(p, class&FLDSIZ, 0, NIL);  /* new entry */
		ssave(p);
	} else switch (class) {

	case AUTO:
		if (arrstkp)
			dynalloc(p, &autooff);
		else
			(void) oalloc(p, &autooff);
		break;
	case STATIC:
	case EXTDEF:
		p->soffset = getlab();
		if (class == STATIC && blevel > 0)
			p->sflags |= SLABEL;
		if (ISFTN(type))
			cftnsp = p;
		break;

	case EXTERN:
	case UFORTRAN:
	case FORTRAN:
		p->soffset = getlab();
		p->slevel = 0;
		break;
	case MOU:
	case MOS:
		(void) oalloc( p, &strucoff );
		if( class == MOU ) strucoff = 0;
		ssave(p);
		break;

	case MOE:
		p->soffset = strucoff++;
		ssave(p);
		break;
	case REGISTER:
		p->soffset = regvar--;
		if (blevel == 1)
			p->sflags |= SSET;
		if (regvar < minrvar)
			minrvar = regvar;
		break;
	}

	if (p->slevel > 0 && (p->sflags & SMASK) == SNORMAL)
		schedremove(p);

	/* user-supplied routine to fix up new definitions */
	FIXDEF(p);

# ifndef BUG1
	if (ddebug)
		printf( "	sdim, ssue, offset: %p, %p, %d\n",
		    p->sdim, p->ssue, p->soffset);
# endif

}

void
ssave(struct symtab *sym)
{
	struct params *p;

	p = tmpalloc(sizeof(struct params));
	p->next = NULL;
	p->sym = sym;

	if (lparam == NULL) {
		p->prev = (struct params *)&lpole;
		lpole = p;
	} else {
		lparam->next = p;
		p->prev = lparam;
	}
	lparam = p;
}

/*
 * end of function
 */
void
ftnend()
{
	extern struct savbc *savbc;

	if (retlab != NOLAB && nerrors == 0) /* inside a real function */
		efcode();

	checkst(0);
	retstat = 0;
	tcheck();
	brklab = contlab = retlab = NOLAB;
	flostat = 0;
	if (nerrors == 0) {
		if (savbc != NULL)
			cerror("bcsave error");
		if (lparam != NULL)
			cerror("parameter reset error");
		if( swx != 0 ) cerror( "switch error");
	}
	savbc = NULL;
	lparam = NULL;
	autooff = AUTOINIT;
	minrvar = regvar = MAXRVAR;
	reached = 1;
	swx = 0;
	swp = swtab;
	tmpfree(); /* Release memory resources */
	(void) locctr(DATA);
}

void
dclargs()
{
	struct params *a;
	struct symtab *p, **parr;
	NODE *q;
	int i;

	argoff = ARGINIT;
# ifndef BUG1
	if (ddebug > 2)
		printf("dclargs()\n");
# endif

	/* Generate a list for bfcode() */
	parr = tmpalloc(sizeof(struct symtab *) * nparams);
	for (a = lparam, i = 0; a != NULL && a != (struct params *)&lpole;
	    a = a->prev) {
		p = parr[i++] = a->sym;
# ifndef BUG1
		if (ddebug > 2) {
			printf("\t%s (%p) ",p->sname, p);
			tprint(p->stype);
			printf("\n");
		}
# endif
		if (p->stype == FARG) {
			q = block(FREE, NIL, NIL, INT, 0, MKSUE(INT));
			q->n_sp = p;
			defid(q, PARAM);
		}
		FIXARG(p); /* local arg hook, eg. for sym. debugger */
	  /* always set aside space, even for register arguments */
		oalloc(p, &argoff);
	}
	cendarg();
	(void) locctr(PROG);
	defalign(ALINT);
	ftnno = getlab();
	bfcode(parr, nparams);
	lparam = NULL;
	nparams = 0;
}

/*
 * reference to a structure or union, with no definition
 */
NODE *
rstruct(char *tag, int soru)
{
	struct symtab *p;
	NODE *q;

	p = (struct symtab *)lookup(tag, STAGNAME);
	switch (p->stype) {

	case UNDEF:
	def:
		q = block(FREE, NIL, NIL, 0, 0, 0);
		q->n_sp = p;
		q->n_type = (soru&INSTRUCT) ? STRTY :
		    ((soru&INUNION) ? UNIONTY : ENUMTY);
		defid(q, (soru&INSTRUCT) ? STNAME :
		    ((soru&INUNION) ? UNAME : ENAME));
		break;

	case STRTY:
		if (soru & INSTRUCT)
			break;
		goto def;

	case UNIONTY:
		if (soru & INUNION)
			break;
		goto def;

	case ENUMTY:
		if (!(soru&(INUNION|INSTRUCT)))
			break;
		goto def;

	}
	q = mkty(p->stype, 0, p->ssue);
	q->n_sue = p->ssue;
	return q;
}

void
moedef(char *name)
{
	NODE *q;

	q = block(FREE, NIL, NIL, MOETY, 0, 0);
	q->n_sp = lookup(name, 0);
	defid(q, MOE);
}

/*
 * begining of structure or union declaration
 */
struct rstack *
bstruct(char *name, int soru)
{
	struct rstack *r;
	struct symtab *s;
	NODE *q;

	if (name != NULL)
		s = lookup(name, STAGNAME);
	else
		s = NULL;

	r = tmpalloc(sizeof(struct rstack));
	r->rinstruct = instruct;
	r->rclass = strunem;
	r->rstrucoff = strucoff;

	strucoff = 0;
	instruct = soru;
	q = block(FREE, NIL, NIL, 0, 0, 0);
	q->n_sp = s;
	if (instruct==INSTRUCT) {
		strunem = MOS;
		q->n_type = STRTY;
		if (s != NULL)
			defid(q, STNAME);
	} else if(instruct == INUNION) {
		strunem = MOU;
		q->n_type = UNIONTY;
		if (s != NULL)
			defid(q, UNAME);
	} else { /* enum */
		strunem = MOE;
		q->n_type = ENUMTY;
		if (s != NULL)
			defid(q, ENAME);
	}
	r->rsym = q->n_sp;
	r->rlparam = lparam;

	/* the "real" definition is where the members are seen */
	if (s != NULL)
		s->suse = lineno;
	return r;
}

/*
 * Called after a struct is declared to restore the environment.
 */
NODE *
dclstruct(struct rstack *r)
{
	NODE *n;
	struct params *l, *m;
	struct suedef *sue;
	struct symtab *p;
	int al, sa, sz;
	TWORD temp;
	int i, high, low;

	if (r->rsym == NULL) {
		sue = permalloc(sizeof(struct suedef));
		sue->suesize = 0;
		sue->suealign = ALSTRUCT;
	} else
		sue = r->rsym->ssue;

#ifdef PCC_DEBUG
	if (ddebug)
		printf("dclstruct(%s)\n", r->rsym ? r->rsym->sname : "??");
#endif
	temp = (instruct&INSTRUCT)?STRTY:((instruct&INUNION)?UNIONTY:ENUMTY);
	instruct = r->rinstruct;
	strunem = r->rclass;
	al = ALSTRUCT;

	high = low = 0;

	if ((l = r->rlparam) == NULL)
		l = lpole;
	else
		l = l->next;

	/* memory for the element array must be allocated first */
	for (m = l, i = 1; m != NULL; m = m->next)
		i++;
	sue->suelem = permalloc(sizeof(struct symtab *) * i);

	for (i = 0; l != NULL; l = l->next) {
		sue->suelem[i++] = p = l->sym;

		if (p == NULL)
			cerror("gummy structure member");
		if (temp == ENUMTY) {
			if (p->soffset < low)
				low = p->soffset;
			if (p->soffset > high)
				high = p->soffset;
			p->ssue = sue;
			continue;
		}
		sa = talign(p->stype, p->ssue);
		if (p->sclass & FIELD) {
			sz = p->sclass&FLDSIZ;
		} else {
			sz = tsize(p->stype, p->sdim, p->ssue);
		}
		if (sz > strucoff)
			strucoff = sz;  /* for use with unions */
		/*
		 * set al, the alignment, to the lcm of the alignments
		 * of the members.
		 */
		SETOFF(al, sa);
	}
	sue->suelem[i] = NULL;
	SETOFF(strucoff, al);

	if (temp == ENUMTY) {
		TWORD ty;

#ifdef ENUMSIZE
		ty = ENUMSIZE(high,low);
#else
		if ((char)high == high && (char)low == low)
			ty = ctype(CHAR);
		else if ((short)high == high && (short)low == low)
			ty = ctype(SHORT);
		else
			ty = ctype(INT);
#endif
		strucoff = tsize(ty, 0, MKSUE(ty));
		sue->suealign = al = talign(ty, MKSUE(ty));
	}

	sue->suesize = strucoff;
	sue->suealign = al;

//	FIXSTRUCT(szindex, oparam); /* local hook, eg. for sym debugger */
#ifdef PCC_DEBUG
	if (ddebug>1) {
		int i;

		printf("\tsize %d align %d elem %p\n",
		    sue->suesize, sue->suealign, sue->suelem);
		for (i = 0; sue->suelem[i] != NULL; ++i) {
			printf("\tmember %s(%p)\n",
			    sue->suelem[i]->sname, sue->suelem[i]);
		}
	}
#endif

	strucoff = r->rstrucoff;
	if ((lparam = r->rlparam) != NULL)
		lparam->next = NULL;
	n = mkty(temp, 0, sue);
	return n;
}

/*
 * error printing routine in parser
 */
void yyerror(char *s);
void
yyerror(char *s)
{
	uerror(s);
}

void yyaccpt(void);
void
yyaccpt(void)
{
	ftnend();
}

void
ftnarg(char *name)
{
	struct symtab *s = lookup(name, 0);

	blevel = 1; /* Always */

	switch (s->stype) {
	case UNDEF:
		/* this parameter, entered at scan */
		break;
	case FARG:
		uerror("redeclaration of formal parameter, %s", s->sname);
		/* fall thru */
	case FTN:
		/* the name of this function matches parm */
		/* fall thru */
	default:
		s = hide(s);
		break;
	case TNULL:
		/* unused entry, fill it */
		;
	}
	s->stype = FARG;
	s->sclass = PARAM;

	ssave(s);
	nparams++;
}

/*
 * compute the alignment of an object with type ty, sizeoff index s
 */
int
talign(unsigned int ty, struct suedef *sue)
{
	int i;

	if(sue == NULL && ty!=INT && ty!=CHAR && ty!=SHORT &&
	    ty!=UNSIGNED && ty!=UCHAR && ty!=USHORT) {
		return(fldal(ty));
	}

	for( i=0; i<=(SZINT-BTSHIFT-1); i+=TSHIFT ){
		switch( (ty>>i)&TMASK ){

		case FTN:
			uerror( "can't assign to function" );
			return( ALCHAR );
		case PTR:
			return( ALPOINT );
		case ARY:
			continue;
		case 0:
			break;
			}
		}

	switch( BTYPE(ty) ){

	case UNIONTY:
	case ENUMTY:
	case STRTY:
		return((unsigned int)sue->suealign);
	case CHAR:
	case UCHAR:
		return (ALCHAR);
	case FLOAT:
		return (ALFLOAT);
	case DOUBLE:
		return (ALDOUBLE);
	case LONGLONG:
	case ULONGLONG:
		return (ALLONGLONG);
	case LONG:
	case ULONG:
		return (ALLONG);
	case SHORT:
	case USHORT:
		return (ALSHORT);
	default:
		return (ALINT);
	}
}

/* compute the size associated with type ty,
 *  dimoff d, and sizoff s */
/* BETTER NOT BE CALLED WHEN t, d, and s REFER TO A BIT FIELD... */
OFFSZ
tsize(TWORD ty, int *d, struct suedef *sue)
{

	int i;
	OFFSZ mult;

	mult = 1;

	for( i=0; i<=(SZINT-BTSHIFT-1); i+=TSHIFT ){
		switch( (ty>>i)&TMASK ){

		case FTN:
			/* cerror( "compiler takes size of function"); */
			uerror( "can't take size of function" );
			return( SZCHAR );
		case PTR:
			return( SZPOINT * mult );
		case ARY:
			mult *= (unsigned int) *d++;
			continue;
		case 0:
			break;

			}
		}

	if (sue == NULL)
		cerror("bad tsize sue");
	if (ty != STRTY && ty != UNIONTY) {
		if (sue->suesize == 0) {
			uerror("unknown size");
			return(SZINT);
		}
	} else {
		if (sue->suelem == NULL)
			uerror("unknown structure/union");
	}

	return((unsigned int)sue->suesize * mult);
}

/*
 * force inoff to have the value n
 */
void
inforce(OFFSZ n)
{
	/* inoff is updated to have the value n */
	OFFSZ wb;
	int rest;
	/* rest is used to do a lot of conversion to ints... */

	if( inoff == n ) return;
	if (inoff > n)
		cerror("initialization alignment error: inoff %lld n %lld",
		    inoff, n);

	wb = inoff;
	SETOFF( wb, SZINT );

	/* wb now has the next higher word boundary */

	if( wb >= n ){ /* in the same word */
		rest = n - inoff;
		vfdzero( rest );
		return;
		}

	/* otherwise, extend inoff to be word aligned */

	rest = wb - inoff;
	vfdzero( rest );

	/* now, skip full words until near to n */

	rest = (n-inoff)/SZINT;
	zecode( rest );

	/* now, the remainder of the last word */

	rest = n-inoff;
	vfdzero( rest );
	if( inoff != n ) cerror( "inoff error");

	}

/*
 * make inoff have the offset the next alignment of n
 */
void
vfdalign(int n)
{
	OFFSZ m;

	m = inoff;
	SETOFF( m, n );
	inforce( m );
}


int idebug = 0;

int ibseen = 0;  /* the number of } constructions which have been filled */

int ifull = 0; /* 1 if all initializers have been seen */

int iclass;  /* storage class of thing being initialized */

int ilocctr = 0;  /* location counter for current initialization */

/*
 * beginning of initilization; set location ctr and set type
 */
void
beginit(struct symtab *p, int class)
{
# ifndef BUG1
	if (idebug >= 3)
		printf("beginit(), symtab = %p\n", p);
# endif

	iclass = p->sclass;
	if (class == EXTERN || class == FORTRAN)
		iclass = EXTERN;
	switch (iclass) {

	case UNAME:
	case EXTERN:
		return;
	case AUTO:
	case REGISTER:
		break;
	case EXTDEF:
	case STATIC:
		ilocctr = ISARY(p->stype)?ADATA:DATA;
		if (nerrors == 0) {
			(void) locctr(ilocctr);
			defalign(talign(p->stype, p->ssue));
			defnam(p);
		}
	}

	inoff = 0;
	ibseen = 0;
	ifull = 0;

	pstk = 0;

	instk(p, p->stype, p->sdim, p->ssue, inoff);

}

/*
 * make a new entry on the parameter stack to initialize p
 */
void
instk(struct symtab *p, TWORD t, int *d, struct suedef *sue, OFFSZ off)
{
	struct instk *sp;

	for (;;) {
# ifndef BUG1
		if (idebug)
			printf("instk((%p, %o,%p,%p, %lld)\n",
			    p, t, d, sue, (long long)off);
# endif

		/* save information on the stack */
		sp = tmpalloc(sizeof(struct instk));
		sp->in_prev = pstk;
		pstk = sp;

		pstk->in_fl = 0;	/* { flag */
		pstk->in_sym = p;
		pstk->in_t = t;
		pstk->in_d = d;
		pstk->in_sue = sue;
		pstk->in_n = 0;  /* number seen */
		pstk->in_xp = (t == STRTY || t == UNIONTY) ? sue->suelem : NULL;
		pstk->in_off = off;/* offset at the beginning of this element */

		/* if t is an array, DECREF(t) can't be a field */
		/* in_sz has size of array elements, and -size for fields */
		if (ISARY(t)) {
			pstk->in_sz = tsize(DECREF(t), d+1, sue);
		} else if (p->sclass & FIELD){
			pstk->in_sz = - (p->sclass & FLDSIZ);
		} else {
			pstk->in_sz = 0;
		}

		if ((iclass==AUTO || iclass == REGISTER) &&
		    (ISARY(t) || t==STRTY))
			uerror("no automatic aggregate initialization");

		/* now, if this is not a scalar, put on another element */

		if (ISARY(t)) {
			t = DECREF(t);
			++d;
			continue;
		} else if (t == STRTY || t == UNIONTY) {
			if (pstk->in_sue == 0) {
				uerror("can't initialize undefined %s",
				    t == STRTY ? "structure" : "union");
				iclass = -1;
				return;
			}
			p = *pstk->in_xp;
			if (((p->sclass != MOS && t == STRTY) ||
			    (p->sclass != MOU && t == UNIONTY)) &&
			    !(p->sclass&FIELD))
				cerror("insane %s member list",
				    t == STRTY ? "structure" : "union");
			t = p->stype;
			d = p->sdim;
			sue = p->ssue;
			off += p->soffset;
			continue;
		} else
			return;
	}
}

/*
 * Write last part of string.
 */
NODE *
strend(char *str)
{
	struct symtab *s;
	int lxarg, i, val, strtemp, strlab;
	char *wr = str;
	NODE *p;

	i = 0;
	if ((iclass == EXTDEF || iclass==STATIC) &&
	    (pstk->in_t == CHAR || pstk->in_t == UCHAR) &&
	    pstk->in_prev != NULL && ISARY(pstk->in_prev->in_t)) {
		/* treat "abc" as { 'a', 'b', 'c', 0 } */
		ilbrace();  /* simulate { */
		inforce(pstk->in_off);
		/*
		 * if the array is inflexible (not top level),
		 * pass in the size and be prepared to throw away
		 * unwanted initializers
		 */

		lxarg = pstk->in_prev->in_prev != NULL ?
		    *pstk->in_prev->in_d : 0;
		while (*wr != 0) {
			if (*wr++ == '\\')
				val = esccon(&wr);
			else
				val = wr[-1];
			if (lxarg == 0 || i < lxarg)
				putbyte(val);
			else if (i == lxarg)
				werror("non-null byte ignored in string"
				    "initializer");
			i++;
		}

		if (lxarg == 0 || i < lxarg)
			putbyte(0);
		irbrace();  /* simulate } */
		return(NIL);
	}
	/* make a label, and get the contents and stash them away */
	if (iclass != SNULL) { /* initializing */
		/* fill out previous word, to permit pointer */
		vfdalign(ALPOINT);
	}

	if (isinlining)
		goto inl;

	/* If an identical string is already emitted, just forget this one */
	str = addstring(str);		/* enter string in string table */
	s = lookup(str, SSTRING);	/* check for existance */

	if (s->soffset == 0) { /* No string */
		s->sclass = ILABEL;

		 /* set up location counter */
inl:		strtemp = locctr(blevel==0 ? ISTRNG : STRNG);
		deflab(strlab = getlab());
		if (isinlining == 0)
			s->soffset = strlab;

		i = 0;
		while (*wr != 0) {
			if (*wr++ == '\\')
				val = esccon(&wr);
			else
				val = wr[-1];
			bycode(val, i);
			i++;
		}
		bycode(0, i++);
		bycode(-1, i);
		(void) locctr(blevel==0 ? ilocctr : strtemp);
	} else {
		strlab = s->soffset;
		i = strlen(str)+1;
	}

	p = buildtree(STRING, NIL, NIL);
	p->n_dim = tmpalloc(sizeof(int));
	*p->n_dim = i;
	if (isinlining) {
		p->n_sp = permalloc(sizeof(struct symtab_hdr));
		p->n_sp->sclass = ILABEL;
		p->n_sp->soffset = strlab;
	} else
		p->n_sp = s;

	return(p);
}

/*
 * simulate byte v appearing in a list of integer values
 */
void
putbyte(int v)
{
	NODE *p;
	p = bcon(v);
	incode( p, SZCHAR );
	tfree( p );
	gotscal();
}

void
endinit(void)
{
	struct suedef *sue;
	TWORD t;
	int *d, n, *d1;

# ifndef BUG1
	if (idebug)
		printf("endinit(), inoff = %lld\n", (long long)inoff);
# endif

	switch( iclass ){

	case EXTERN:
	case AUTO:
	case REGISTER:
	case -1:
		return;
		}

	while (pstk->in_prev)
		pstk = pstk->in_prev;

	t = pstk->in_t;
	d = pstk->in_d;
	sue = pstk->in_sue;
	n = pstk->in_n;

	if( ISARY(t) ){
		d1 = d;

		vfdalign(pstk->in_sz);  /* fill out part of the last element, if needed */
		n = inoff/pstk->in_sz;  /* real number of initializers */
		if (*d1 >= n) {
			/* once again, t is an array, so no fields */
			inforce(tsize(t, d, sue));
			n = *d1;
		}
		if (*d1 != 0 && *d1 != n)
			uerror("too many initializers");
		if (n == 0)
			werror("empty array declaration");
		*d = n;
		if (*d1 == 0) {
			FIXDEF(pstk->in_sym);
		}
	}

	else if (t == STRTY || t == UNIONTY) {
		/* clearly not fields either */
		inforce( tsize( t, d, sue ) );
	} else if (n > 1)
		uerror("bad scalar initialization");
	else
	/* this will never be called with a field element... */
		inforce(tsize(t, d, sue));

	lparam = NULL;
	vfdalign( AL_INIT );
	inoff = 0;
	iclass = SNULL;

}

/*
 * called from the grammar if we must punt during initialization
 * stolen from endinit()
 */
void
fixinit(void)
{
	while (pstk->in_prev)
		pstk = pstk->in_prev;
	lparam = NULL;
	vfdalign( AL_INIT );
	inoff = 0;
	iclass = SNULL;
}

/*
 * take care of generating a value for the initializer p
 * inoff has the current offset (last bit written)
 * in the current word being generated
 */
void
doinit(NODE *p)
{
	NODE *u;
	struct suedef *sue;
	int sz, *d;
	TWORD t;
	int o;

	/* note: size of an individual initializer is assumed to fit into an int */

	if( iclass < 0 ) goto leave;
	if( iclass == EXTERN || iclass == UNAME ){
		uerror( "cannot initialize extern or union" );
		iclass = -1;
		goto leave;
		}

	if( iclass == AUTO || iclass == REGISTER ){
		/* do the initialization and get out, without regard 
		    for filing out the variable with zeros, etc. */
		bccode();
		spname = pstk->in_sym;
		p = buildtree( ASSIGN, buildtree( NAME, NIL, NIL ), p );
		ecomp(p);
		return;
		}

	if( p == NIL ) return;  /* for throwing away strings that have been turned into lists */

	if( ifull ){
		uerror( "too many initializers" );
		iclass = -1;
		goto leave;
		}
	if( ibseen ){
		uerror( "} expected");
		goto leave;
		}

# ifndef BUG1
	if (idebug > 1)
		printf("doinit(%p)\n", p);
# endif

	t = pstk->in_t;  /* type required */
	d = pstk->in_d;
	sue = pstk->in_sue;
	if (pstk->in_sz < 0) {  /* bit field */
		sz = -pstk->in_sz;
	} else {
		sz = tsize( t, d, sue );
	}

	inforce( pstk->in_off );

	u = block(NAME, NIL,NIL, t, d, sue);
	p = buildtree( ASSIGN, u, p );
	p->n_left->n_op = FREE;
	p->n_left = p->n_right;
	p->n_right = NIL;
	p->n_left = optim( p->n_left );
	o = p->n_left->n_op;
	if( o == UNARY AND ){
		o = p->n_left->n_op = FREE;
		p->n_left = p->n_left->n_left;
		}
	p->n_op = INIT;

	if( sz < SZINT ){ /* special case: bit fields, etc. */
		if (o != ICON || p->n_left->n_sp != NULL)
			uerror( "illegal initialization" );
		else
			incode( p->n_left, sz );
	} else if( o == FCON ){
		fincode( p->n_left->n_fcon, sz );
	} else if( o == DCON ){
		fincode( p->n_left->n_dcon, sz );
	} else {
		p = optim(p);
		if( p->n_left->n_op != ICON )
			uerror( "illegal initialization" );
		else
			cinit( p, sz );
	}

	gotscal();

	leave:
	tfree(p);
}

void
gotscal(void)
{
	int t, n;
	struct symtab *p;
	OFFSZ temp;

	for( ; pstk->in_prev != NULL; ) {

		if( pstk->in_fl ) ++ibseen;

		pstk = pstk->in_prev;
		
		t = pstk->in_t;

		if( t == STRTY || t == UNIONTY){
			++pstk->in_xp;
			if ((p = *pstk->in_xp) == NULL)
				continue;

			/* otherwise, put next element on the stack */
			instk(p, p->stype, p->sdim, p->ssue,
			    p->soffset + pstk->in_off);
			return;
		} else if( ISARY(t) ){
			n = ++pstk->in_n;
			if (n >= *pstk->in_d && pstk->in_prev != NULL)
				continue;

			/* put the new element onto the stack */

			temp = pstk->in_sz;
			instk(pstk->in_sym, (TWORD)DECREF(pstk->in_t),
			    pstk->in_d+1, pstk->in_sue, pstk->in_off+n*temp);
			return;
		}

	}
	ifull = 1;
}

/*
 * process an initializer's left brace
 */
void
ilbrace()
{
	int t;
	struct instk *temp;

	temp = pstk;

	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {

		t = pstk->in_t;
		if (t != UNIONTY && t != STRTY && !ISARY(t))
			continue; /* not an aggregate */
		if (pstk->in_fl) { /* already associated with a { */
			if (pstk->in_n)
				uerror( "illegal {");
			continue;
		}

		/* we have one ... */
		pstk->in_fl = 1;
		break;
	}

	/* cannot find one */
	/* ignore such right braces */

	pstk = temp;
}

/*
 * called when a '}' is seen
 */
void
irbrace()
{
# ifndef BUG1
	if (idebug)
		printf( "irbrace(): lparam = %p on entry\n", lparam);
# endif

	if (ibseen) {
		--ibseen;
		return;
	}

	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {
		if(!pstk->in_fl)
			continue;

		/* we have one now */

		pstk->in_fl = 0;  /* cancel { */
		gotscal();  /* take it away... */
		return;
	}

	/* these right braces match ignored left braces: throw out */
	ifull = 1;
}

/*
 * update the offset pointed to by poff; return the
 * offset of a value of size `size', alignment `alignment',
 * given that off is increasing
 */
int
upoff(int size, int alignment, int *poff)
{
	int off;

	off = *poff;
	SETOFF(off, alignment);
	if ((offsz-off) <  size) {
		if (instruct != INSTRUCT)
			cerror("too many local variables");
		else
			cerror("Structure too large");
	}
	*poff = off+size;
	return (off);
}

/*
 * allocate p with offset *poff, and update *poff
 */
int
oalloc(struct symtab *p, int *poff )
{
	int al, off, tsz;
	int noff;

	al = talign(p->stype, p->ssue);
	noff = off = *poff;
	tsz = tsize(p->stype, p->sdim, p->ssue);
#ifdef BACKAUTO
	if (p->sclass == AUTO) {
		if ((offsz-off) < tsz)
			cerror("too many local variables");
		noff = off + tsz;
		SETOFF(noff, al);
		off = -noff;
	} else
#endif
#ifdef PARAMS_UPWARD
	if (p->sclass == PARAM) {
		if ((offsz-off) < tsz)
			cerror("too many parameters");
		noff = off + tsz;
		if (tsz < SZINT)
			al = ALINT;
		SETOFF(noff, al);
		off = -noff;

	} else
#endif
	if (p->sclass == PARAM && (tsz < SZINT)) {
		off = upoff(SZINT, ALINT, &noff);
#ifndef RTOLBYTES
		off = noff - tsz;
#endif
	} else {
		off = upoff(tsz, al, &noff);
	}

	if (p->sclass != REGISTER) {
	/* in case we are allocating stack space for register arguments */
		if (p->soffset == NOOFFSET)
			p->soffset = off;
		else if(off != p->soffset)
			return(1);
	}

	*poff = noff;
	return(0);
}

/*
 * Allocate space on the stack for dynamic arrays.
 * Strategy is as follows:
 * - first entry is a pointer to the dynamic datatype.
 * - if it's a one-dimensional array this will be the only entry used.
 * - if it's a multi-dimensional array the following (numdim-1) integers
 *   will contain the sizes to multiply the indexes with.
 * - code to write the dimension sizes this will be generated here.
 * - code to allocate space on the stack will be generated here.
 */
static void
dynalloc(struct symtab *p, int *poff)
{
	struct suedef sue;
//	struct symtab *q;
	NODE *n, *nn;
	OFFSZ ptroff, argoff;
	TWORD t;
//	int al, off, tsz;
	int i;

	bccode(); /* Init code generation */
	/*
	 * Setup space on the stack, one pointer to the array space
	 * and n-1 integers for the array sizes.
	 */
	ptroff = upoff(tsize(PTR, 0, &sue), talign(PTR, &sue), poff);
	if (arrstkp > 1) {
		int tab[2];
		tab[0] = arrstkp-1;
		tab[1] = INT;
		argoff = upoff(tsize(ARY+INT, tab, NULL),
		    talign(ARY+INT, 0), poff);
	}

	/*
	 * Set the initial pointer the same as the stack pointer.
	 * Assume that the stack pointer is correctly aligned already.
	 */
	p->soffset = ptroff;
	p->stype = INCREF(p->stype);
	spname = p;
	nn = buildtree(NAME, NIL, NIL);

	/*
	 * Calculate the size of the array to be allocated on stack.
	 * Save the sizes on the stack while doing this.
	 */
	n = arrstk[0];
	i = 0;

	if (arrstkp != 1)
		cerror("dynalloc: no multidim arrays");
#if 0
	while (++i < arrstkp) {
		
		sp = clocal(block(PLUS, stknode(INCREF(STRTY), 0, 0),
		    offcon(argoff + (i-1) * ALINT, INT, 0, INT), INT, 0, INT);
		sp = buildtree(UNARY MUL, sp, NIL);

		n = buildtree(ASSIGN, sp, arrstk[i]);
	}

	sp = block(PCONV, stknode(INCREF(STRTY), 0, 0), NIL,
	    INCREF(BTYPE(p->stype)), p->dimoff, p->sizoff);
	n = buildtree(PLUS, sp, n);

#endif

	/* get the underlying size without ARYs */
	t = p->stype;
	while (ISARY(t))
		t = DECREF(t);

	/* Write it onto the stack */
	spalloc(nn, n, tsize(t, 0, p->ssue));
	p->sflags |= SDYNARRAY;
	arrstkp = 0;
}

/*
 * allocate a field of width w
 * new is 0 if new entry, 1 if redefinition, -1 if alignment
 */
int
falloc(struct symtab *p, int w, int new, NODE *pty)
{
	int al,sz,type;

	type = (new<0)? pty->n_type : p->stype;

	/* this must be fixed to use the current type in alignments */
	switch( new<0?pty->n_type:p->stype ){

	case ENUMTY: {
		struct suedef *sue;
		sue = new < 0 ? pty->n_sue : p->ssue;
		al = sue->suealign;
		sz = sue->suesize;
		break;
	}

	case CHAR:
	case UCHAR:
		al = ALCHAR;
		sz = SZCHAR;
		break;

	case SHORT:
	case USHORT:
		al = ALSHORT;
		sz = SZSHORT;
		break;

	case INT:
	case UNSIGNED:
		al = ALINT;
		sz = SZINT;
		break;

	default:
		if( new < 0 ) {
			uerror( "illegal field type" );
			al = ALINT;
			}
		else {
			al = fldal( p->stype );
			sz =SZINT;
			}
		}

	if( w > sz ) {
		uerror( "field too big");
		w = sz;
		}

	if( w == 0 ){ /* align only */
		SETOFF( strucoff, al );
		if( new >= 0 ) uerror( "zero size field");
		return(0);
		}

	if( strucoff%al + w > sz ) SETOFF( strucoff, al );
	if( new < 0 ) {
		if( (offsz-strucoff) < w )
			cerror("structure too large");
		strucoff += w;  /* we know it will fit */
		return(0);
		}

	/* establish the field */

	if( new == 1 ) { /* previous definition */
		if( p->soffset != strucoff || p->sclass != (FIELD|w) ) return(1);
		}
	p->soffset = strucoff;
	if( (offsz-strucoff) < w ) cerror("structure too large");
	strucoff += w;
	p->stype = type;
	fldty( p );
	return(0);
}

/*
 * handle unitialized declarations
 * assumed to be not functions
 */
void
nidcl(NODE *p, int class)
{
	int commflag;  /* flag for labelled common declarations */

	commflag = 0;

	/* compute class */
	if (class == SNULL) {
		if (blevel > 1)
			class = AUTO;
		else if (blevel != 0 || instruct)
			cerror( "nidcl error" );
		else { /* blevel = 0 */
			class = noinit();
			if (class == EXTERN)
				commflag = 1;
		}
	}
#ifdef LCOMM
	/* hack so stab will come out as LCSYM rather than STSYM */
	if (class == STATIC) {
		extern int stabLCSYM;
		stabLCSYM = 1;
	}
#endif

	defid(p, class);

	/* if an array is not initialized, no empty dimension */
	if (class != EXTERN && class != TYPEDEF &&
	    ISARY(p->n_type) && *p->n_dim == 0)
		uerror("null storage definition");

#ifndef LCOMM
	if (class==EXTDEF || class==STATIC)
#else
	if (class==STATIC) {
		struct symtab *s = &stab[p->n_rval];
		extern int stabLCSYM;
		int sz = tsize(s->stype, s->dimoff, s->sizoff)/SZCHAR;
		
		stabLCSYM = 0;
		if (sz % sizeof (int))
			sz += sizeof (int) - (sz % sizeof (int));
		if (s->slevel > 1)
			printf("	.lcomm	L%d,%d\n", s->soffset, sz);
		else
			printf("	.lcomm	%s,%d\n", exname(s->sname), sz);
	} else if (class == EXTDEF)
#endif
	{
		/* simulate initialization by 0 */
		beginit(p->n_sp, class);
		endinit();
	}
	if (commflag)
		commdec(p->n_sp);
}

/*
 * Merges a type tree into one type. Returns one type node with merged types
 * and class stored in the su field. Frees all other nodes.
 * XXX - classes in typedefs?
 */
NODE *
typenode(NODE *p)
{
	int class = 0, adj, noun, sign;

	adj = INT;	/* INT, LONG or SHORT */
	noun = UNDEF;	/* INT, CHAR or FLOAT */
	sign = 0;	/* 0, SIGNED or UNSIGNED */

	/* Remove initial QUALIFIERs */
	if (p && p->n_op == QUALIFIER) {
		p->n_op = FREE;
		p = p->n_left;
	}

	/* Handle initial classes special */
	if (p && p->n_op == CLASS) {
		class = p->n_type;
		p->n_op = FREE;
		p = p->n_left;
	}

	/* Remove more QUALIFIERs */
	if (p && p->n_op == QUALIFIER) {
		p->n_op = FREE;
		p = p->n_left;
	}

	if (p && p->n_op == TYPE && p->n_left == NIL) {
#ifdef CHAR_UNSIGNED
		if (p->n_type == CHAR)
			p->n_type = UCHAR;
#endif
		p->n_su = class;
		return p;
	}

	while (p != NIL) { 
		if (p->n_op == QUALIFIER) /* Skip const/volatile */
			goto next;
		if (p->n_op == CLASS) {
			if (class != 0)
				uerror("too many storage classes");
			class = p->n_type;
			goto next;
		}
		if (p->n_op != TYPE)
			cerror("typenode got notype %d", p->n_op);
		switch (p->n_type) {
		case SIGNED:
		case UNSIGNED:
			if (sign != 0)
				goto bad;
			sign = p->n_type;
			break;
		case LONG:
			if (adj == LONG) {
				adj = LONGLONG;
				break;
			}
			/* FALLTHROUGH */
		case SHORT:
			if (adj != INT)
				goto bad;
			adj = p->n_type;
			break;
		case INT:
		case CHAR:
		case FLOAT:
			if (noun != UNDEF)
				goto bad;
			noun = p->n_type;
			break;
		default:
			goto bad;
		}
	next:
		p->n_op = FREE;
		p = p->n_left;
	}

#ifdef CHAR_UNSIGNED
	if (noun == CHAR && sign == 0)
		sign = UNSIGNED;
#endif
	if (noun == UNDEF) {
		noun = INT;
	} else if (noun == FLOAT) {
		if (sign != 0 || adj == SHORT)
			goto bad;
		noun = (adj == LONG ? DOUBLE : FLOAT);
	} else if (noun == CHAR && adj != INT)
		goto bad;

	if (adj != INT)
		noun = adj;
	if (sign == UNSIGNED)
		noun += (UNSIGNED-INT);

	p = block(TYPE, NIL, NIL, noun, 0, 0);
	if (strunem != 0)
		class = strunem;
	p->n_su = class;
	return p;

bad:	uerror("illegal type combination");
	return mkty(INT, 0, 0);
}

static struct tylnk {
	struct tylnk *next;
	int dim;
} tylnk, *tylkp;
static int ntdim;

static void
tylkadd(int dim)
{
	tylkp->next = tmpalloc(sizeof(struct tylnk));
	tylkp = tylkp->next;
	tylkp->next = NULL;
	tylkp->dim = dim;
	ntdim++;
}

/* merge type typ with identifier idp  */
NODE *
tymerge(NODE *typ, NODE *idp)
{
	struct tylnk *base;
	unsigned int t;
	int i, *j;

	if (typ->n_op != TYPE)
		cerror( "tymerge: arg 1" );
	if (idp == NIL)
		return( NIL );

# ifndef BUG1
	if( ddebug > 2 ) fwalk( idp, eprint, 0 );
# endif

	idp->n_type = typ->n_type;

	tylkp = &tylnk;
	tylkp->next = NULL;
	ntdim = 0;

	tyreduce(idp);
	idp->n_sue = typ->n_sue;

	for (t = typ->n_type, j = typ->n_dim; t&TMASK; t = DECREF(t))
		if (ISARY(t))
			tylkadd(*j++);

	if (ntdim) {
		int *a = permalloc(sizeof(int) * ntdim);
		for (i = 0, base = tylnk.next; base; base = base->next, i++)
			a[i] = base->dim;
		idp->n_dim = a;
	} else
		idp->n_dim = NULL;

	/* now idp is a single node: fix up type */

	idp->n_type = ctype(idp->n_type);

	/* in case ctype has rewritten things */
	if ((t = BTYPE(idp->n_type)) != STRTY && t != UNIONTY && t != ENUMTY)
		idp->n_sue = MKSUE(t);

	return(idp);
}

/*
 * build a type, and stash away dimensions,
 * from a parse tree of the declaration
 * the type is build top down, the dimensions bottom up
 */
void
tyreduce(NODE *p)
{
	NODE *q;
	int o, temp;
	unsigned int t;

	o = p->n_op;
	p->n_op = FREE;

	if (o == NAME)
		return;

	t = INCREF(p->n_type);
	switch (o) {
	case UNARY CALL:
		t += (FTN-PTR);
		break;
	case LB:
		t += (ARY-PTR);
		if (p->n_right->n_op != ICON) {
			q = p->n_right;
			o = RB;
		} else {
			temp = p->n_right->n_lval;
			p->n_right->n_op = FREE;
			if (temp == 0 && p->n_left->n_op == LB)
				uerror("null dimension");
		}
		break;
	}

	p->n_left->n_type = t;
	tyreduce(p->n_left);

	if (o == LB)
		tylkadd(temp);
	if (o == RB) {
		tylkadd(-1);
		arrstk[arrstkp++] = q;
	}

	p->n_sp = p->n_left->n_sp;
	p->n_type = p->n_left->n_type;
}

void
fixtype(NODE *p, int class)
{
	unsigned int t, type;
	int mod1, mod2;
	/* fix up the types, and check for legality */

	if( (type = p->n_type) == UNDEF ) return;
	if ((mod2 = (type&TMASK))) {
		t = DECREF(type);
		while( mod1=mod2, mod2 = (t&TMASK) ){
			if( mod1 == ARY && mod2 == FTN ){
				uerror( "array of functions is illegal" );
				type = 0;
				}
			else if( mod1 == FTN && ( mod2 == ARY || mod2 == FTN ) ){
				uerror( "function returns illegal type" );
				type = 0;
				}
			t = DECREF(t);
			}
		}

	/* detect function arguments, watching out for structure declarations */
	/* for example, beware of f(x) struct { int a[10]; } *x; { ... } */
	/* the danger is that "a" will be converted to a pointer */

	if( class==SNULL && blevel==1 && !(instruct&(INSTRUCT|INUNION)) )
		class = PARAM;
	if (class == PARAM || (class == REGISTER && blevel == 1)) {
		if (type == FLOAT)
			type = DOUBLE;
		else if (ISARY(type)) {
			++p->n_dim;
			type += (PTR-ARY);
		} else if (ISFTN(type)) {
			werror("a function is declared as an argument");
			type = INCREF(type);
		}

	}

	if (instruct && ISFTN(type)) {
		uerror("function illegal in structure or union");
		type = INCREF(type);
	}
	p->n_type = type;
}

/*
 * give undefined version of class
 */
int
uclass(int class)
{
	if (class == SNULL)
		return(EXTERN);
	else if (class == STATIC)
		return(USTATIC);
	else if (class == FORTRAN)
		return(UFORTRAN);
	else
		return(class);
}

int
fixclass(int class, TWORD type)
{
	/* first, fix null class */
	if( class == SNULL ){
		if( instruct&INSTRUCT ) class = MOS;
		else if( instruct&INUNION ) class = MOU;
		else if( blevel == 0 ) class = EXTDEF;
		else if( blevel == 1 ) class = PARAM;
		else class = AUTO;

		}

	/* now, do general checking */

	if( ISFTN( type ) ){
		switch( class ) {
		default:
			uerror( "function has illegal storage class" );
		case AUTO:
			class = EXTERN;
		case EXTERN:
		case EXTDEF:
		case FORTRAN:
		case TYPEDEF:
		case STATIC:
		case UFORTRAN:
		case USTATIC:
			;
			}
		}

	if( class&FIELD ){
		if( !(instruct&INSTRUCT) ) uerror( "illegal use of field" );
		return( class );
		}

	switch( class ){

	case MOU:
		if( !(instruct&INUNION) ) uerror( "illegal MOU class" );
		return( class );

	case MOS:
		if( !(instruct&INSTRUCT) ) uerror( "illegal MOS class" );
		return( class );

	case MOE:
		if( instruct & (INSTRUCT|INUNION) ) uerror( "illegal MOE class" );
		return( class );

	case REGISTER:
		if( blevel == 0 ) uerror( "illegal register declaration" );
		else if( regvar >= MINRVAR && cisreg( type ) ) return( class );
		if( blevel == 1 ) return( PARAM );
		else return( AUTO );

	case AUTO:
		if( blevel < 2 ) uerror( "illegal ULABEL class" );
		return( class );

	case PARAM:
		if( blevel != 1 ) uerror( "illegal PARAM class" );
		return( class );

	case UFORTRAN:
	case FORTRAN:
# ifdef NOFORTRAN
			NOFORTRAN;    /* a condition which can regulate the FORTRAN usage */
# endif
		if( !ISFTN(type) ) uerror( "fortran declaration must apply to function" );
		else {
			type = DECREF(type);
			if( ISFTN(type) || ISARY(type) || ISPTR(type) ) {
				uerror( "fortran function has wrong type" );
				}
			}
	case EXTERN:
	case STATIC:
	case EXTDEF:
	case TYPEDEF:
	case USTATIC:
		if( blevel == 1 ){
			uerror( "illegal USTATIC class" );
			return( PARAM );
			}
	case STNAME:
	case UNAME:
	case ENAME:
		return( class );

	default:
		cerror( "illegal class: %d", class );
		/* NOTREACHED */

	}
	return 0; /* XXX */
}

/*
 * Generates a goto statement; sets up label number etc.
 */
void
gotolabel(char *name)
{
	struct symtab *s = lookup(name, SLBLNAME);

	if (s->soffset == 0)
		s->soffset = -getlab();
	branch(s->soffset < 0 ? -s->soffset : s->soffset);
}

/*
 * Sets a label for gotos.
 */
void
deflabel(char *name)
{
	struct symtab *s = lookup(name, SLBLNAME);

	if (s->soffset > 0)
		uerror("label '%s' redefined", name);
	if (s->soffset == 0)
		s->soffset = getlab();
	if (s->soffset < 0)
		s->soffset = -s->soffset;
	locctr(PROG);
	deflab(s->soffset);
}

#ifdef PCC_DEBUG
/* if not debugging, checkst is a macro */
void
checkst(int lev)
{
#if 0
	int i, j;
	struct symtab *p, *q;

	for (i=0, p=stab; i<SYMTSZ; ++i, ++p) {
		if (p->stype == TNULL)
			continue;
		j = lookup(p->sname, 0);
		if (j != i) {
			q = &stab[j];
			if (q->stype == UNDEF || q->slevel <= p->slevel)
				cerror("check error: %s", q->sname);
		} else if (p->slevel > lev)
			cerror("%s check at level %d", p->sname, lev);
	}
#endif
}
#endif

void
clearst(int lev)
{
	int temp;

	temp = lineno;
	aobeg();

	symclear(lev); /* Clean ut the symbol table */

	lineno = temp;
	aoend();
}

struct symtab *
getsymtab(char *name, int flags)
{
	struct symtab *s;

	if (flags & STEMP)
		s = tmpalloc(sizeof(struct symtab));
	else
		s = permalloc(sizeof(struct symtab));
	s->sname = name;
	s->snext = NULL;
	s->stype = UNDEF;
	s->sclass = SNULL;
	s->sflags = flags & SMASK;
	s->soffset = 0;
	s->s_argn = 0;
	return s;
}
