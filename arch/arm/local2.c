#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "pass1.h"
#include "pass2.h"

/*
 * add/sub/...
 *
 * Param given:
 */
void
hopcode(int f, int o)
{
	char *str;

	switch (o) {
	case PLUS:
		str = "add";
		break;
	case MINUS:
		str = "sub";
		break;
	case AND:
		str = "and";
		break;
	case OR:
		str = "orr";
		break;
	case ER:
		str = "eor";
		break;
	default:
		comperr("hopcode2: %d", o);
		str = 0; /* XXX gcc */
	}
	printf("%s%c", str, f);
}

/*
 * Return type size in bytes.  Used by R2REGS, arg 2 to offset().
 */
int
tlen(NODE *p)
{
	switch(p->n_type) {
		case CHAR:
		case UCHAR:
			return(1);

		case SHORT:
		case USHORT:
			return(SZSHORT/SZCHAR);

		case DOUBLE:
			return(SZDOUBLE/SZCHAR);

		case INT:
		case UNSIGNED:
		case LONG:
		case ULONG:
			return(SZINT/SZCHAR);

		case LONGLONG:
		case ULONGLONG:
			return SZLONGLONG/SZCHAR;

		default:
			if (!ISPTR(p->n_type))
				comperr("tlen type %d not pointer");
			return SZPOINT(p->n_type)/SZCHAR;
		}
}

/*
 * Emit code to compare two longlong numbers.
 */
static void
twollcomp(NODE *p)
{
	int o = p->n_op;
	int s = getlab();
	int e = p->n_label;
	int cb1, cb2;

	if (o >= ULE)
		o -= (ULE-LE);
	switch (o) {
	case NE:
		cb1 = 0;
		cb2 = NE;
		break;
	case EQ:
		cb1 = NE;
		cb2 = 0;
		break;
	case LE:
	case LT:
		cb1 = GT;
		cb2 = LT;
		break;
	case GE:
	case GT:
		cb1 = LT;
		cb2 = GT;
		break;
	
	default:
		cb1 = cb2 = 0; /* XXX gcc */
	}
	if (p->n_op >= ULE)
		cb1 += 4, cb2 += 4;
	expand(p, 0, "\ttst UR,UL		@ compare 64-bit values (upper)\n");
	if (cb1) cbgen(cb1, s);
	if (cb2) cbgen(cb2, e);
	expand(p, 0, "\ttst AR,AL		@ (and lower)\n");
	cbgen(p->n_op, e);
	deflab(s);
}

#if 0
/*
 * Assign to a bitfield.
 * Clumsy at least, but what to do?
 */
static void
bfasg(NODE *p)
{
	NODE *fn = p->n_left;
	int shift = UPKFOFF(fn->n_rval);
	int fsz = UPKFSZ(fn->n_rval);
	int andval, tch = 0;

	/* get instruction size */
	switch (p->n_type) {
	case CHAR: case UCHAR: tch = 'b'; break;
	case SHORT: case USHORT: tch = 'w'; break;
	case INT: case UNSIGNED: tch = 'l'; break;
	default: comperr("bfasg");
	}

	/* put src into a temporary reg */
	fprintf(stdout, "	mov%c ", tch);
	adrput(stdout, getlr(p, 'R'));
	fprintf(stdout, ",");
	adrput(stdout, getlr(p, '1'));
	fprintf(stdout, "\n");

	/* AND away the bits from dest */
	andval = ~(((1 << fsz) - 1) << shift);
	fprintf(stdout, "	and%c $%d,", tch, andval);
	adrput(stdout, fn->n_left);
	fprintf(stdout, "\n");

	/* AND away unwanted bits from src */
	andval = ((1 << fsz) - 1);
	fprintf(stdout, "	and%c $%d,", tch, andval);
	adrput(stdout, getlr(p, '1'));
	fprintf(stdout, "\n");

	/* SHIFT left src number of bits */
	if (shift) {
		fprintf(stdout, "	sal%c $%d,", tch, shift);
		adrput(stdout, getlr(p, '1'));
		fprintf(stdout, "\n");
	}

	/* OR in src to dest */
	fprintf(stdout, "	or%c ", tch);
	adrput(stdout, getlr(p, '1'));
	fprintf(stdout, ",");
	adrput(stdout, fn->n_left);
	fprintf(stdout, "\n");
}
#endif

/*
 * Push a structure on stack as argument.
 * the scratch registers are already free here
 */
static void
stasg(NODE *p)
{
	printf("\tldr %s,=%d\n", rnames[R3], p->n_stsize);
	printf("\tadd %s,%s," CONFMT "\n", rnames[R0], rnames[p->n_left->n_rval], p->n_left->n_lval);
	printf("\tbl %s\n", exname("memcpy"));
}

static void
shasg(NODE *p)
{
	if (p->n_op == LS && p->n_right->n_lval < 32) {
		expand(p, INBREG, "\tmov A1,AL,asr 32-AR        ; 64-bit left-shift\n");
		expand(p, INBREG, "\tmov U1,UL,asr AR\n");
		expand(p, INBREG, "\tor U1,U1,A1\n");
		expand(p, INBREG, "\tmove A1,AL,asr AR\n");
	} else if (p->n_op == LS) {
		expand(p, INBREG, "\tldr A1,=0	; 64-bit left-shift\n");
		expand(p, INBREG, "\tmov U1,AL,asr AR-32\n");
	} else if (p->n_op == RS && p->n_right->n_lval < 32) {
		expand(p, INBREG, "\tmov U1,UL,asr 32-AR        ; 64-bit right-shift\n");
		expand(p, INBREG, "\tmov A1,AL,asr AR\n");
		expand(p, INBREG, "\tor A1,A1,U1\n");
		expand(p, INBREG, "\tmov U1,UL,asr AR\n");
	} else if (p->n_op == RS) {
		expand(p, INBREG, "\tldr U1,=0	; 64-bit right-shift\n");
		expand(p, INBREG, "\tmov A1,UL,asr AR-32\n");
	}
}

#if 0
/*
 * Compare two floating point numbers.
 */
static void
fcomp(NODE *p)  
{
	if (p->n_left->n_op == REG) {
		if (p->n_su & DORIGHT)
			expand(p, 0, "	fxch\n");
		expand(p, 0, "	fucompp\n");	/* emit compare insn  */
	} else if (p->n_left->n_type == DOUBLE)
		expand(p, 0, "	fcompl AL\n");	/* emit compare insn  */
	else if (p->n_left->n_type == FLOAT)
		expand(p, 0, "	fcomp AL\n");	/* emit compare insn  */
	else
		comperr("bad compare %p\n", p);
	expand(p, 0, "	fnstsw %ax\n");	/* move status reg to ax */
	
	switch (p->n_op) {
	case EQ:
		expand(p, 0, "	andb $64,%ah\n	jne LC\n");
		break;
	case NE:
		expand(p, 0, "	andb $64,%ah\n	je LC\n");
		break;
	case LE:
		expand(p, 0, "	andb $65,%ah\n	cmpb $1,%ah\n	jne LC\n");
		break;
	case LT:
		expand(p, 0, "	andb $65,%ah\n	je LC\n");
		break;
	case GT:
		expand(p, 0, "	andb $1,%ah\n	jne LC\n");
		break;
	case GE:
		expand(p, 0, "	andb $65,%ah\n	jne LC\n");
		break;
	default:
		comperr("fcomp op %d\n", p->n_op);
	}
}
#endif

#if 0
/*
 * Convert an unsigned long long to floating point number.
 */
static void
ulltofp(NODE *p)
{
	static int loadlab;
	int jmplab;

	if (loadlab == 0) {
		loadlab = getlab();
		expand(p, 0, "	.data\n");
		printf(LABFMT ":	.long 0,0x80000000,0x403f\n", loadlab);
		expand(p, 0, "	.text\n");
	}
	jmplab = getlab();
	expand(p, 0, "	pushl UL\n	pushl AL\n");
	expand(p, 0, "	fildq (%esp)\n");
	expand(p, 0, "	addl $8,%esp\n");
	expand(p, 0, "	cmpl $0,UL\n");
	printf("	jge " LABFMT "\n", jmplab);
	printf("	fldt " LABFMT "\n", loadlab);
	printf("	faddp %%st,%%st(1)\n");
	printf(LABFMT ":\n", jmplab);
}
#endif

static int
argsiz(NODE *p)
{
	TWORD t = p->n_type;

	if (t < LONGLONG || t == FLOAT || t > BTMASK)
		return 4;
	if (t == LONGLONG || t == ULONGLONG || t == DOUBLE)
		return 8;
	if (t == LDOUBLE)
		return 12;
	if (t == STRTY || t == UNIONTY)
		return p->n_stsize;
	comperr("argsiz");
	return 0;
}

void
zzzcode(NODE *p, int c)
{
#if 0
	NODE *r, *l;
	int pr, lr, s;
#endif
	char *ch;
	int pr;

	switch (c) {

	case 'C':  /* remove from stack after subroutine call */
		pr = p->n_qual;
		if (p->n_op == STCALL || p->n_op == USTCALL)
			pr += 4;
		if (p->n_op == UCALL)
			return; /* XXX remove ZC from UCALL */
		if (pr > 0)
			printf("\tadd %s,%s,#%d\n", rnames[SP], rnames[SP], pr);
		break;

        case 'I':               /* init constant */
                if (p->n_name[0] != '\0')
                        comperr("named init");
                fprintf(stdout, "=%lld", p->n_lval);
                break;

	case 'D': /* Long long comparision */
		twollcomp(p);
		break;

#if 0
	case 'E': /* Assign to bitfield */
		bfasg(p);
		break;

	case 'G': /* Floating point compare */
		fcomp(p);
		break;

	case 'J': /* convert unsigned long long to floating point */
		ulltofp(p);
		break;

	case 'N': /* output extended reg name */
		printf("%s", rnames[getlr(p, '1')->n_rval]);
		break;
#endif

	case 'O': /* 64-bit left and right shift operators */
		shasg(p);
		break;

	case 'E': /* print out emulated ops */

                if (p->n_op == DIV && p->n_type == ULONGLONG) ch = "udiv";
                else if (p->n_op == DIV) ch = "div";
                else if (p->n_op == MUL) ch = "mul";
                else if (p->n_op == MOD && p->n_type == ULONGLONG) ch = "umod";
                else if (p->n_op == MOD) ch = "mod";
                else if (p->n_op == RS && p->n_type == ULONGLONG) ch = "lshr";
                else if (p->n_op == RS) ch = "ashr";
                else if (p->n_op == LS) ch = "ashl";
                else ch = 0, comperr("ZE");
                printf("\tbl __%sdi3\n", ch);
                break;

	case 'Q': /* emit struct assign */
		stasg(p);
		break;

	default:
		comperr("zzzcode %c", c);
	}
}

/*ARGSUSED*/
int
rewfld(NODE *p)
{
	return(1);
}

/*
 * Does the bitfield shape match?
 */
int
flshape(NODE *p)
{
	int o = p->n_op;

	if (o == OREG || o == REG || o == NAME)
		return SRDIR; /* Direct match */
	if (o == UMUL && shumul(p->n_left))
		return SROREG; /* Convert into oreg */
	return SRREG; /* put it into a register */
}

/* INTEMP shapes must not contain any temporary registers */
/* XXX should this go away now? */
int
shtemp(NODE *p)
{
	return 0;
#if 0
	int r;

	if (p->n_op == STARG )
		p = p->n_left;

	switch (p->n_op) {
	case REG:
		return (!istreg(p->n_rval));

	case OREG:
		r = p->n_rval;
		if (R2TEST(r)) {
			if (istreg(R2UPK1(r)))
				return(0);
			r = R2UPK2(r);
		}
		return (!istreg(r));

	case UMUL:
		p = p->n_left;
		return (p->n_op != UMUL && shtemp(p));
	}

	if (optype(p->n_op) != LTYPE)
		return(0);
	return(1);
#endif
}

void
adrcon(CONSZ val)
{
	printf(CONFMT, val);
}

void
conput(FILE *fp, NODE *p)
{
	char *s;
	int val = p->n_lval;

	switch (p->n_op) {
	case ICON:
#if 0
		if (p->n_sp)
			printf(" [class=%d,level=%d] ", p->n_sp->sclass, p->n_sp->slevel);
#endif
		if (p->n_sp == NULL || (p->n_sp->sclass == ILABEL ||
		   (p->n_sp->sclass == STATIC && p->n_sp->slevel > 0)))
			s = p->n_name;
		else
			s = exname(p->n_name);
			
		if (*s != '\0') {
			fprintf(fp, "%s", s);
			if (val > 0)
				fprintf(fp, "+%d", val);
			else if (val < 0)
				fprintf(fp, "-%d", -val);
		} else
			fprintf(fp, CONFMT, (CONSZ)val);
		return;

	default:
		comperr("illegal conput, p %p", p);
	}
}

/*ARGSUSED*/
void
insput(NODE *p)
{
	comperr("insput");
}

/*
 * Write out the upper address, like the upper register of a 2-register
 * reference, or the next memory location.
 */
void
upput(NODE *p, int size)
{

	size /= SZCHAR;
	switch (p->n_op) {
	case REG:
		fprintf(stdout, "%s", rnames[p->n_rval-R0R1+1]);
		break;

	case NAME:
	case OREG:
		p->n_lval += size;
		adrput(stdout, p);
		p->n_lval -= size;
		break;
	case ICON:
		fprintf(stdout, CONFMT, p->n_lval >> 32);
		break;
	default:
		comperr("upput bad op %d size %d", p->n_op, size);
	}
}

void
adrput(FILE *io, NODE *p)
{
	int r;
	/* output an address, with offsets, from p */

	if (p->n_op == FLD)
		p = p->n_left;

	switch (p->n_op) {

	case NAME:
		if (p->n_name[0] != '\0') {
			fputs(p->n_name, io);
			if (p->n_lval != 0)
				fprintf(io, "+" CONFMT, p->n_lval);
		} else
			fprintf(io, CONFMT, p->n_lval);
		return;

	case OREG:
		r = p->n_rval;
		fprintf(io, "[%s,#%d]", rnames[p->n_rval], (int)p->n_lval);
		return;

	case ICON:
		/* addressable value of the constant */
		conput(io, p);
		return;

	case MOVE:
	case REG:
		switch (p->n_type) {
		case LONGLONG:
		case ULONGLONG:
			fprintf(stdout, "%s", rnames[p->n_rval-R0R1]);
			break;
		default:
			fprintf(io, "%s", rnames[p->n_rval]);
		}
		return;

	default:
		comperr("illegal address, op %d, node %p", p->n_op, p);
		return;

	}
}

/*
 * these mnemonics match the order of the preprocessor decls
 * EQ, NE, LE, LT, GE, GT, ULE, ULT, UGE, UGT
 */

static char *
ccbranches[] = {
	"beq",		/* branch if equal */
	"bne",		/* branch if not-equal */
	"ble",		/* branch if less-than-or-equal */
	"blt",		/* branch if less-than */
	"bge",		/* branch if greater-than-or-equal */
	"bgt",		/* branch if greater-than */
	/* what should these be ? */
	"bls",		/* branch if lower-than-or-same */
	"blo",		/* branch if lower-than */
	"bhs",		/* branch if higher-than-or-same */
	"bhi",		/* branch if higher-than */
};

/*   printf conditional and unconditional branches */
void
cbgen(int o, int lab)
{
	if (o < EQ || o > UGT)
		comperr("bad conditional branch: %s", opst[o]);
	printf("\t%s " LABFMT "\n", ccbranches[o-EQ], lab);
}

/*
 * Must store floats in memory if there are two function calls involved.
 */
static int
storefloat(struct interpass *ip, NODE *p)
{
	int l, r;

	switch (optype(p->n_op)) {
	case BITYPE:
		l = storefloat(ip, p->n_left);
		r = storefloat(ip, p->n_right);
		if (p->n_op == CM)
			return 0; /* arguments, don't care */
		if (callop(p->n_op))
			return 1; /* found one */
#define ISF(p) ((p)->n_type == FLOAT || (p)->n_type == DOUBLE || \
	(p)->n_type == LDOUBLE)
		if (ISF(p->n_left) && ISF(p->n_right) && l && r) {
			/* must store one. store left */
			struct interpass *nip;
			TWORD t = p->n_left->n_type;
			NODE *ll;
			int off;

                	off = BITOOR(freetemp(szty(t)));
                	ll = mklnode(OREG, off, FPREG, t);
			nip = ipnode(mkbinode(ASSIGN, ll, p->n_left, t));
			p->n_left = mklnode(OREG, off, FPREG, t);
                	DLIST_INSERT_BEFORE(ip, nip, qelem);
		}
		return l|r;

	case UTYPE:
		l = storefloat(ip, p->n_left);
		if (callop(p->n_op))
			l = 1;
		return l;
	default:
		return 0;
	}
}

struct addrsymb {
	DLIST_ENTRY(addrsymb) link;
	struct symtab *orig;
	struct symtab *new;
};
struct addrsymb addrsymblist;

static void
prtaddr(NODE *p)
{
	NODE *l = p->n_left;
	struct addrsymb *el;
	int found = 0;
	int lab;

	if (p->n_op != ADDROF || l->n_op != NAME)
		return;

	/* write address to byte stream */

	DLIST_FOREACH(el, &addrsymblist, link) {
		if (el->orig == l->n_sp) {
			found = 1;
			break;
		}
	}

	if (!found) {
		setloc1(PROG);
		defalign(SZPOINT(l->n_type));
		deflab1(lab = getlab());
		printf("\t.word ");
		adrput(stdout, l);
		printf("\n");
		el = tmpalloc(sizeof(struct addrsymb));
		el->orig = l->n_sp;
 		el->new = tmpalloc(sizeof(struct symtab_hdr));
		el->new->sclass = ILABEL;
		el->new->soffset = lab;
		el->new->sflags = 0;
		DLIST_INSERT_BEFORE(&addrsymblist, el, link);
	}

	nfree(l);
	p->n_op = NAME;
	p->n_lval = 0;
	p->n_sp = el->new;
	p2tree(p);
}

void
myreader(struct interpass *ipole)
{
	struct interpass *ip;

	DLIST_INIT(&addrsymblist, link);

	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		walkf(ip->ip_node, prtaddr);
	}
	if (x2debug)
		printip(ipole);
}

/*
 * Remove some PCONVs after OREGs are created.
 */
static void
pconv2(NODE *p)
{
	NODE *q;

	if (p->n_op == PLUS) {
		if (p->n_type == (PTR+SHORT) || p->n_type == (PTR+USHORT)) {
			if (p->n_right->n_op != ICON)
				return;
			if (p->n_left->n_op != PCONV)
				return;
			if (p->n_left->n_left->n_op != OREG)
				return;
			q = p->n_left->n_left;
			nfree(p->n_left);
			p->n_left = q;
			/*
			 * This will be converted to another OREG later.
			 */
		}
	}
}

void
mycanon(NODE *p)
{
	walkf(p, pconv2);
}

void
myoptim(struct interpass *ipp)
{
}

/*
 * Register move: move contents of register 's' to register 'r'.
 */
void
rmove(int s, int d, TWORD t)
{
        switch (t) {
        case LONGLONG:
        case ULONGLONG:
#define PRINTREG(x, y) fprintf(stdout, "%s", rnames[(x)-(R0R1-(y))]);
                if (s == d+1) {
                        /* dh = sl, copy low word first */
                        printf("\tmov "); PRINTREG(d,0); PRINTREG(s,0);
                        printf("\tmov "); PRINTREG(d,1); PRINTREG(s,1);
                } else {
                        /* copy high word first */
                        printf("\tmov "); PRINTREG(d,1); PRINTREG(s,1);
                        printf("\tmov "); PRINTREG(d,0); PRINTREG(s,0);
                }
                printf("\n");
#undef PRINTREG
                break;
        case LDOUBLE:
#ifdef notdef
                /* a=b()*c(); will generate this */
                comperr("bad float rmove: %d %d", s, d);
#endif
                break;
        default:
		printf("\tmov %s,%s	@ rmove\n", rnames[d], rnames[s]);
        }
}

/*
 * Can we assign a register from class 'c', given the set
 * of number of assigned registers in each class 'r'.
 *
 * On ARM, we have:
 *	11 CLASSA registers
 *	10  CLASSB registers
 *	
 */
int
COLORMAP(int c, int *r)
{
	int num = 0;	/* number of registers used */

#if 0
	static const char classes[] = { 'X', 'A', 'B', 'C', 'D' };
	printf("COLORMAP: requested class %c\n", classes[c]);
	printf("COLORMAP: class A: %d\n", r[CLASSA]);
	printf("COLORMAP: class B: %d\n", r[CLASSB]);
#endif

	switch (c) {
	case CLASSA:
		num += r[CLASSA];
		num += 2*r[CLASSB];
		return num < 11;
	case CLASSB:
		num += 2*r[CLASSB];
		num += r[CLASSA];
		return num < 10;
	}
	assert(0);
	return 0; /* XXX gcc */
}

/*
 * Return a class suitable for a specific type.
 */
int
gclass(TWORD t)
{
	if (t == FLOAT || t == DOUBLE || t == LDOUBLE)
		return CLASSB;
	return CLASSA;
}

/*
 * Calculate argument sizes.
 */
void
lastcall(NODE *p)
{
	NODE *op = p;
	int size = 0;

	p->n_qual = 0;
	if (p->n_op != CALL && p->n_op != FORTCALL && p->n_op != STCALL)
		return;
	for (p = p->n_right; p->n_op == CM; p = p->n_left)
		size += argsiz(p->n_right);
	size += argsiz(p);
	op->n_qual = size - 16; /* XXX */
}

/*
 * Special shapes.
 */
int
special(NODE *p, int shape)
{
	int o = p->n_op;

	switch (shape) {
	case SFUNCALL:
		if (o == STCALL || o == USTCALL)
			return SRREG;
		break;
	}
	return SRNOPE;
}
