# if 0
static char *sccsid ="@(#)local2.c	1.39 (Berkeley) 5/11/88";
# endif

# include "pass1.h"	/* XXX - for p1print() */
# include "pass2.h"
# include <ctype.h>

# define putstr(s)	fputs((s), stdout)

void sconv(NODE *p, int);
void acon(NODE *p);
int argsize(NODE *p);
void genargs(NODE *p);


void
lineid(int l, char *fn)
{
	/* identify line l and file fn */
	printf("#	line %d, file %s\n", l, fn);
}

/*
 * End of block.
 */
void
eobl2()
{
	register OFFSZ spoff;	/* offset from stack pointer */
	extern int ftlab1, ftlab2;
	extern  int retlab;	/* From pass 1 */

	spoff = maxoff;
	if (spoff >= AUTOINIT)
		spoff -= AUTOINIT;
	spoff /= SZINT;
	/* return from function code */
	p1print("L%d:\n", retlab);
	p1print("	move 017,016\n");
	p1print("	pop 017,016\n");
	p1print("	popj 017,\n");

	/* Prolog code */
	p1print("L%d:\n", ftlab1);
	p1print("	push 017,016\n");
	p1print("	move 016,017\n");
	if (spoff)
		p1print("	addi 017,%llo\n", spoff);
	p1print("	jrst L%d\n", ftlab2);
}

/*
 * add/sub/...
 *
 * Param given:
 *	R - Register
 *	M - Memory
 *	C - Constant
 */
void
hopcode(int f, int o)
{
	char *str;
	char *mod = "";

	if (asgop(o))
		o = NOASG o;
	switch (f) {
	case 'R':
		break;
	case 'M':
		mod = "m";
		break;
	case 'C':
		mod = "i";
		break;
	default:
		cerror("hopcode %c", f);
	}

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
		str = "ior";
		break;
	case ER:
		str = "xor";
		break;
	case LS:
		str = "lsh";
		mod = "";
		break;
	default:
		cerror("hopcode2: %d", o);
	}
	printf("%s%s", str, mod);
}

char *
rnames[] = {  /* keyed to register number tokens */
	"0", "01", "02", "03", "04", "05", "06", "07",
	"010", "011", "012", "013", "014", "015", "016", "017",
};

int rstatus[] = {
	0, SAREG|STAREG, SAREG|STAREG, SAREG|STAREG,
	SAREG|STAREG, SAREG|STAREG, SAREG|STAREG, SAREG|STAREG,
	SAREG, SAREG, SAREG, SAREG, SAREG, SAREG, SAREG, SAREG,
};

int
tlen(p) NODE *p;
{
	switch(p->in.type) {
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
			if (!ISPTR(p->in.type))
				cerror("tlen type %d not pointer");
			return SZPOINT/SZCHAR;
		}
}

static char *
ccbranches[] = {
	"jumpe",	/* jumpe */
	"jumpn",	/* jumpn */
	"jumple",	/* jumple */
	"jumpl",	/* jumpl */
	"jumpge",	/* jumpge */
	"jumpg",	/* jumpg */
	"jumple",	/* jumple (jlequ) */
	"jumpl",	/* jumpl (jlssu) */
	"jumpge",	/* jumpge (jgequ) */
	"jumpg",	/* jumpg (jgtru) */
};

static char *
binskip[] = {
	"e",	/* jumpe */
	"n",	/* jumpn */
	"le",	/* jumple */
	"l",	/* jumpl */
	"ge",	/* jumpge */
	"g",	/* jumpg */
};

/*
 * Do a binary comparision, and jump accordingly.
 */
static void
twocomp(NODE *p)
{
	int o = p->in.op;
	extern int negrel[];
	int isscon = 0, iscon = p->in.right->in.op == ICON;

	if (o < EQ || o > GT)
		cerror("bad binary conditional branch: %s", opst[o]);

	if (iscon)
		isscon = p->in.right->tn.lval >= 0 &&
		    p->in.right->tn.lval < 01000000;

	printf("	ca%c%s ", iscon && isscon ? 'i' : 'm',
	    binskip[negrel[o-EQ]-EQ]);
	adrput(getlr(p, 'L'));
	putchar(',');
	if (iscon && (isscon == 0)) {
		printf("[ .long ");
		adrput(getlr(p, 'R'));
		putchar(']');
	} else
		adrput(getlr(p, 'R'));
	printf("\n	jrst L%d\n", p->bn.label);
}

/*
 * Compare byte/word pointers.
 * XXX - do not work for highest bit set in address
 */
static void
ptrcomp(NODE *p)
{
	printf("	rot "); adrput(getlr(p, 'L')); printf(",6\n");
	printf("	rot "); adrput(getlr(p, 'R')); printf(",6\n");
	twocomp(p);
}

/*
 * Do a binary comparision of two long long, and jump accordingly.
 * XXX - can optimize for constants.
 */
static void     
twollcomp(NODE *p)
{       
	int o = p->in.op;
	int iscon = p->in.right->in.op == ICON;
	int m;

	if (o < EQ || o > GT)
		cerror("bad long long conditional branch: %s", opst[o]);

	/* Special strategy for equal/not equal */
	if (o == EQ || o == NE) {
		if (o == EQ)
			m = getlab();
		printf("	came ");
		upput(getlr(p, 'L'), SZLONG);
		putchar(',');
		if (iscon)
			printf("[ .long ");
		upput(getlr(p, 'R'), SZLONG);
		if (iscon)
			putchar(']');
		printf("\n	jrst L%d\n", o == EQ ? m : p->bn.label);
		printf("	cam%c ", o == EQ ? 'n' : 'e');
		adrput(getlr(p, 'L'));
		putchar(',');
		if (iscon)
			printf("[ .long ");
		adrput(getlr(p, 'R'));
		if (iscon)
			putchar(']');
		printf("\n	jrst L%d\n", p->bn.label);
		if (o == EQ)
			printf("L%d:\n", m);
		return;
	}
	/* First test upper */
	printf("	cam%ce ", o == GT || o == GE ? 'l' : 'g');
	upput(getlr(p, 'L'), SZLONG);
	putchar(',');
	if (iscon)
		printf("[ .long ");
	upput(getlr(p, 'R'), SZLONG);
	if (iscon)
		putchar(']');
	printf("\n	jrst L%d\n", p->bn.label);

	/* Test equality */
	printf("	came ");
	upput(getlr(p, 'L'), SZLONG);
	putchar(',');
	if (iscon)
		printf("[ .long ");
	upput(getlr(p, 'R'), SZLONG);
	if (iscon)
		putchar(']');
	printf("\n	jrst L%d\n", m = getlab());

	/* Test lower. Only works with pdp10 format for longlongs */
	printf("	cam%c%c ", o == GT || o == GE ? 'l' : 'g',
	    o == LT || o == GT ? 'e' : ' ');
	adrput(getlr(p, 'L'));
	putchar(',');
	if (iscon)  
		printf("[ .long ");
	adrput(getlr(p, 'R'));
	if (iscon)
		putchar(']');
	printf("\n	jrst L%d\n", p->bn.label);
	printf("L%d:\n", m);
}

/*
 * Print the correct instruction for constants.
 */
static void
constput(NODE *p)
{
	CONSZ val = p->in.right->tn.lval;
	int reg = p->in.left->tn.rval;

	/* Only numeric constant */
	if (p->in.right->in.name == '\0') {
		if (val == 0) {
			printf("movei %s,0", rnames[reg]);
		} else if ((val & 0777777000000) == 0) {
			printf("movei %s,0%llo", rnames[reg], val);
		} else if ((val & 0777777) == 0) {
			printf("hrlzi %s,0%llo", rnames[reg], val >> 18);
		} else {
			printf("move %s,[ .long 0%llo]", rnames[reg], val);
		}
		/* Can have more tests here, hrloi etc */
		return;
	} else {
		if (val == 0)
			printf("move %s,[ .long %s]", rnames[reg],
			    p->in.right->in.name);
		else
			printf("move %s,[ .long %s+0%llo]", rnames[reg],
			    p->in.right->in.name, val);
	}
}

/*
 * Return true if the constant can be bundled in an instruction (immediate).
 */
static int
oneinstr(NODE *p)
{
	if (p->in.name[0] != '\0')
		return 0;
	if ((p->tn.lval & 0777777000000ULL) != 0)
		return 0;
	return 1;
}

/*
 * Handle xor of constants separate.
 * Emit two instructions instead of one extra memory reference.
 */
static void
emitxor(NODE *p)               
{                       
	CONSZ val;
	int reg;

	if (p->in.op != EREQ)
		cerror("emitxor");
	if (p->in.right->in.op != ICON)
		cerror("emitxor2");
	val = p->in.right->tn.lval;
	reg = p->in.left->tn.rval;
	if (val & 0777777)
		printf("	trc 0%o,0%llo\n", reg, val & 0777777);
	if (val & 0777777000000)
		printf("	tlc 0%o,0%llo\n", reg, (val >> 18) & 0777777);
}

/*
 * Print an instruction that takes care of a byte or short (less than 36 bits)
 */
static void
outvbyte(NODE *p)
{
	NODE *l = p->in.left;
	int lval, bsz, boff;

	lval = l->tn.lval;
	l->tn.lval &= 0777777;
	bsz = (lval >> 18) & 077;
	boff = (lval >> 24) & 077;

	if ((bsz == 18) && (boff == 0 || boff == 18)) {
		printf("hr%cm", boff ? 'r' : 'l');
		return;
	}
	cerror("outvbyte: bsz %d boff %d", bsz, boff);
}

/*
 * Emit a "load short" instruction, from OREG to REG.
 * If reg is 016 (frame pointer), it can be converted 
 * to a halfword instruction, otherwise use ldb.
 * Sign extension must also be done here.
 */
static void
emitshort(NODE *p)
{
	CONSZ off = p->tn.lval;
	int reg = p->tn.rval;
	int issigned = !ISUNSIGNED(p->in.type);
	int ischar = BTYPE(p->in.type) == CHAR || BTYPE(p->in.type) == UCHAR;

	if (reg) { /* Can emit halfword instructions */
		if (off < 0) { /* argument, use move instead */
			printf("	move ");
		} else if (ischar) {
			printf("	ldb ");
			adrput(getlr(p, '1'));
			printf(",[ .long 0%02o11%02o%06o ]\n",
			    (int)(27-(9*(off&3))), reg, (int)off/4);
			if (issigned) {
				printf("	lsh ");
				adrput(getlr(p, '1'));
				printf(",033\n	ash ");
				adrput(getlr(p, '1'));
				printf(",-033\n");
			}
			return;
		} else {
#ifdef notyet
			printf("	h%cr%c ", off & 1 ? 'r' : 'l',
			    issigned ? 'e' : 'z');
#endif
			printf("	ldb ");
			adrput(getlr(p, '1'));
			printf(",[ .long 0%02o22%02o%06o ]\n",
			    (int)(18-(18*(off&1))), reg, (int)off/2);
			if (issigned) {
				printf("	hrre ");
				adrput(getlr(p, '1'));
				putchar(',');
				adrput(getlr(p, '1'));
				putchar('\n');
			}
			return;
		}
		p->tn.lval /= 2;
	} else {
		if (off != 0)
			cerror("emitshort with off");
		printf("	ldb ");
		adrput(getlr(p, '1'));
		printf(",%s\n", rnames[reg]);
		if (issigned) {
			if (ischar) {
				printf("	lsh ");
				adrput(getlr(p, '1'));
				printf(",033\n	ash ");
				adrput(getlr(p, '1'));
				printf(",-033\n");
			} else {
				printf("	hrre ");
				adrput(getlr(p, '1'));
				putchar(',');
				adrput(getlr(p, '1'));
				putchar('\n');
			}
		}
		return;
	}
	adrput(getlr(p, '1'));
	putchar(',');
	adrput(getlr(p, 'L'));
	putchar('\n');
}

/*
 * Store a short from a register. Destination is a OREG.
 */
static void
storeshort(NODE *p)
{
	NODE *l = p->in.left;
	CONSZ off = l->tn.lval;
	int reg = l->tn.rval;
	int ischar = BTYPE(p->in.type) == CHAR || BTYPE(p->in.type) == UCHAR;

	if (l->in.op == NAME) {
		if (ischar) {
			printf("	dpb ");
			adrput(getlr(p, 'R'));
			printf(",[ .long 0%02o%010o+%s ]\n",
			    070+((int)off&3), (int)(off/4), l->in.name);
			return;
		}
		printf("	hr%cm ", off & 1 ? 'r' : 'l');
		l->tn.lval /= 2;
		adrput(getlr(p, 'R'));
		putchar(',');   
		adrput(getlr(p, 'L'));
		putchar('\n');
		return;
	}

	if (off || reg == STKREG) { /* Can emit halfword instructions */
		if (off < 0) { /* argument, use move instead */
			printf("	movem ");
		} else if (ischar) {
			printf("	dpb ");
			adrput(getlr(p, '1'));
			printf(",[ .long 0%02o11%02o%06o ]\n",
			    (int)(27-(9*(off&3))), reg, (int)off/4);
			return;
		} else {
			printf("	hr%cm ", off & 1 ? 'r' : 'l');
		}
		l->tn.lval /= 2;
		adrput(getlr(p, 'R'));
		putchar(',');
		adrput(getlr(p, 'L'));
	} else {
		printf("	dpb ");
		adrput(getlr(p, 'R'));
		putchar(',');
		l = getlr(p, 'L');
		l->in.op = REG;
		adrput(l);
		l->in.op = OREG;
	}
	putchar('\n');
}

/*
 * Add an int to a pointer. Both args are in registers, store value
 * in the right register.
 */
static void     
addtoptr(NODE *p) 
{                   
	NODE *l = p->in.left;
	int pp = l->in.type & TMASK1; /* pointer to pointer */
	int ty = l->in.type;
	int ischar = BTYPE(ty) == CHAR || BTYPE(ty) == UCHAR;

	if (!ischar && BTYPE(ty) != SHORT && BTYPE(ty) != USHORT)
		cerror("addtoptr != CHAR/SHORT");
	printf("	ad%s ", pp ? "d" : "jbp");
	adrput(getlr(p, 'R'));
	putchar(',');
	adrput(getlr(p, 'L'));
	putchar('\n');
}

/*
 * Add a constant to a pointer.
 */
static void     
addcontoptr(NODE *p) 
{                   
	NODE *l = p->in.left;
	int pp = l->in.type & TMASK1; /* pointer to pointer */
	int ty = l->in.type;
	int ischar = BTYPE(ty) == CHAR || BTYPE(ty) == UCHAR;

	if (!ischar && BTYPE(ty) != SHORT && BTYPE(ty) != USHORT)
		cerror("addtoptr != SHORT/CHAR");
	if (pp) {
		printf("	addi ");
		adrput(getlr(p, 'L'));
		putchar(',');
		adrput(getlr(p, 'R'));
		putchar('\n');
		if ((p->in.type & TMASK1) == 0) {
			/* Downgrading to pointer to short */
			/* Must make short pointer */
			printf("	tlo ");
			adrput(getlr(p, 'L'));
			if (ischar)
				printf(",0700000\n");
			else
				printf(",0740000\n");
		}
	} else {
		CONSZ off = p->in.right->tn.lval;

		if (off == 0)
			return; /* Should be taken care of in clocal() */
		printf("	addi ");
		adrput(getlr(p, 'L'));
		printf(",0%llo\n", off >> 1);
		if (off & 1) {
			printf("	ibp ");
			adrput(getlr(p, 'L'));
			printf("\n");
		}
	}
}

/*
 * Add a constant to a char pointer and return it in a scratch reg.
 */
static void     
addconandcharptr(NODE *p) 
{                   
	NODE *l = p->in.left;
	int ty = l->in.type;
	CONSZ off = p->in.right->tn.lval;

	if (BTYPE(ty) != CHAR && BTYPE(ty) != UCHAR)
		cerror("addconandcharptr != CHAR");
	if (l->tn.rval == STKREG) {
		printf("	xmovei ");
		adrput(getlr(p, '1'));
		printf(",0%llo(0%o)\n", off >> 2, l->tn.rval);
		printf("	tlo ");
		adrput(getlr(p, '1'));
		printf(",0%o0000\n", (int)(off & 3) + 070);
	} else {
		if (off >= 0 && off <= 0777777) {
			printf("	movei ");
			adrput(getlr(p, '1'));
			printf(",0%llo\n", off);
		} else {
			printf("	move ");
			adrput(getlr(p, '1'));
			printf(",[ .long 0%llo ]\n", off & 0777777777777);
		}
		printf("	adjbp ");
		adrput(getlr(p, '1'));
		printf(",0%o\n", l->tn.rval);
	}
}

/*
 * Multiply a register with a constant.
 */
static void     
imuli(NODE *p)
{
	NODE *r = p->in.right;

	if (r->tn.lval >= 0 && r->tn.lval <= 0777777) {
		printf("	imuli ");
		adrput(getlr(p, 'L'));
		printf(",0%llo\n", r->tn.lval);
	} else {
		printf("	imul ");
		adrput(getlr(p, 'L'));
		printf(",[ .long 0%llo ]\n", r->tn.lval & 0777777777777);
	}
}

/*
 * Divide a register with a constant.
 */
static void     
idivi(NODE *p)
{
	NODE *r = p->in.right;

	if (r->tn.lval >= 0 && r->tn.lval <= 0777777) {
		printf("	idivi ");
		adrput(getlr(p, '1'));
		printf(",0%llo\n", r->tn.lval);
	} else {
		printf("	idiv ");
		adrput(getlr(p, '1'));
		printf(",[ .long 0%llo ]\n", r->tn.lval & 0777777777777);
	}
}

/*
 * move a constant into a register.
 */
static void
xmovei(NODE *p)
{
	/*
	 * Trick: If this is an unnamed constant, just move it directly,
	 * otherwise use xmovei to get section number.
	 */
	if (p->in.name[0] == '\0' || p->tn.lval > 0777777) {
		printf("	");
		zzzcode(p, 'D');
		putchar(' ');
		adrput(getlr(p, '1'));
		putchar(',');
		zzzcode(p, 'E');
	} else {
		printf("	xmovei ");
		adrput(getlr(p, '1'));
		printf(",%s", p->in.name);
		if (p->tn.lval != 0)
			printf("+0%llo", p->tn.lval);
	}
	putchar('\n');
}

void
zzzcode(NODE *p, int c)
{
	int m;

	switch (c) {
	case 'A':
		printf("\tZA: ");
		break;
	case 'C':
		constput(p);
		break;

	case 'D': /* Find out which type of const load insn to use */
		if (p->in.op != ICON)
			cerror("zzzcode not ICON");
		if (p->in.name[0] == '\0') {
			if ((p->tn.lval <= 0777777) && (p->tn.lval > -0777777))
				printf("movei");
			else if ((p->tn.lval & 0777777) == 0)
				printf("hrlzi");
			else
				printf("move");
		} else
			printf("move");
		break;

	case 'E': /* Print correct constant expression */
		if (p->in.name[0] == '\0') {
			if ((p->tn.lval <= 0777777) && (p->tn.lval > -0777777))
				printf("0%llo", p->tn.lval);
			else if ((p->tn.lval & 0777777) == 0)
				printf("0%llo", p->tn.lval >> 18);
			else
				printf("[ .long 0%llo]", p->tn.lval);
		} else {
			if (p->tn.lval == 0)
				printf("[ .long %s]", p->in.name);
			else
				printf("[ .long %s+0%llo]",
				    p->in.name, p->tn.lval);
		}
		break;

	case 'O': /* Print long long expression. Can be made more efficient */
		if (p->in.name[0] != '\0')
			cerror("longlong in name");
		printf("[ .long 0%llo,0%llo ]",
		    p->tn.lval & 0777777777777,
		    (p->tn.lval >> 36) & 0777777777777);
		break;

	case 'F': /* Print an "opsimp" instruction based on its const type */
		hopcode(oneinstr(p->in.right) ? 'C' : 'R', p->in.op);
		break;

	case 'G': /* Print a constant expression based on its const type */
		p = p->in.right;
		if (oneinstr(p)) {
			printf("0%llo", p->tn.lval);
		} else {
			if (p->in.name[0] == '\0') {
				printf("[ .long 0%llo ]",
				    p->tn.lval & 0777777777777ULL);
			} else {
				if (p->tn.lval == 0)
					printf("[ .long %s ]", p->in.name);
				else
					printf("[ .long %s+0%llo]", p->in.name,
					    p->tn.lval, 0777777777777ULL);
			}
		}
		break;

	case 'H': /* Print a small constant */
		p = p->in.right;
		printf("0%llo", p->tn.lval & 0777777);
		break;

	case 'I':
		p = p->in.left;
		/* FALLTHROUGH */
	case 'K':
		if (p->in.name[0] != '\0')
			putstr(p->in.name);
		if (p->tn.lval != 0) {
			putchar('+');
			printf("0%llo", p->tn.lval & 0777777777777);
		}
		break;

	case 'J':
		outvbyte(p);
		break;

	case 'L':
		zzzcode(p->in.left, 'T');
		break;

	case 'T':
		if (p->in.op != OREG)
			cerror("ZT");
		p->in.op = REG;
		adrput(p);
		p->in.op = OREG;
		break;

	case 'M':
		sconv( p, c == 'M' );
		break;

	case 'N':  /* logical ops, turned into 0-1 */
		/* use register given by register 1 */
		cbgen(0, m = getlab(), 'I');
		deflab(p->bn.label);
		printf("	setz %s\n", rnames[getlr(p, '1')->tn.rval]);
		deflab(m);
		break;

	case 'Q': /* two-param long long comparisions */
		twollcomp(p);
		break;

	case 'R': /* two-param conditionals */
		twocomp(p);
		break;

	case 'S':
		emitxor(p);
		break;

	case 'U':
		emitshort(p);
		break;
		
	case 'V':
		storeshort(p);
		break;

	case 'W':
		addtoptr(p);
		break;

	case 'X':
		addcontoptr(p);
		break;

	case 'Y':
		addconandcharptr(p);
		break;

	case 'Z':
		ptrcomp(p);
		break;

	case 'a':
		imuli(p);
		break;

	case 'b':
		idivi(p);
		break;

	case 'c':
		xmovei(p);
		break;

	default:
		cerror("zzzcode %c", c);
	}
}

/*
 * Convert between two data types.
 */
void
sconv(NODE *p, int forarg)
{
}

/*
 * Output code to move a value between two registers.
 * XXX - longlong?
 */
void
rmove(int rt, int rs, TWORD t)
{
	printf("\t%s %s,%s\n", (t == DOUBLE ? "dmove" : "move"),
	    rnames[rt], rnames[rs]);
}

struct respref respref[] = {
	{ INTAREG|INTBREG,	INTAREG|INTBREG, },
	{ INAREG|INBREG,	INAREG|INBREG|SOREG|STARREG|STARNM|SNAME|SCON,},
	{ INTEMP,	INTEMP, },
	{ FORARG,	FORARG, },
	{ INTEMP,	INTAREG|INAREG|INTBREG|INBREG|SOREG|STARREG|STARNM, },
	{ 0,	0 },
};

/* set up temporary registers */
void
setregs()
{
	fregs = 7;	/* 7 free regs on PDP10 (1-7) */
}

/*ARGSUSED*/
int
rewfld(NODE *p)
{
	return(1);
}

/*ARGSUSED*/
int
callreg(NODE *p)
{
	return(1);
}

int canaddr(NODE *);
int
canaddr(NODE *p)
{
	int o = p->in.op;

	if (o==NAME || o==REG || o==ICON || o==OREG ||
	    (o==UNARY MUL && shumul(p->in.left)))
		return(1);
	return(0);
}

int
flshape(NODE *p)
{
	register int o = p->in.op;

	return (o == REG || o == NAME || o == ICON ||
		(o == OREG && (!R2TEST(p->tn.rval) || tlen(p) == 1)));
}

/* INTEMP shapes must not contain any temporary registers */
int
shtemp(NODE *p)
{
	int r;

	if (p->in.op == STARG )
		p = p->in.left;

	switch (p->in.op) {
	case REG:
		return (!istreg(p->tn.rval));

	case OREG:
		r = p->tn.rval;
		if (R2TEST(r)) {
			if (istreg(R2UPK1(r)))
				return(0);
			r = R2UPK2(r);
		}
		return (!istreg(r));

	case UNARY MUL:
		p = p->in.left;
		return (p->in.op != UNARY MUL && shtemp(p));
	}

	if (optype(p->in.op) != LTYPE)
		return(0);
	return(1);
}

int
shumul(NODE *p)
{
	register int o;

	if (x2debug) {
		int val;
		printf("shumul(%p)\n", p);
		eprint(p, 0, &val, &val);
	}

	o = p->in.op;
#if 0
	if (o == NAME || (o == OREG && !R2TEST(p->tn.rval)) || o == ICON)
		return(STARNM);
#endif

	if ((o == INCR || o == ASG MINUS) &&
	    (p->in.left->in.op == REG && p->in.right->in.op == ICON) &&
	    p->in.right->in.name[0] == '\0') {
		switch (p->in.type) {
			case CHAR|PTR:
			case UCHAR|PTR:
				o = 1;
				break;

			case SHORT|PTR:
			case USHORT|PTR:
				o = 2;
				break;

			case INT|PTR:
			case UNSIGNED|PTR:
			case LONG|PTR:
			case ULONG|PTR:
			case FLOAT|PTR:
				o = 4;
				break;

			case DOUBLE|PTR:
			case LONGLONG|PTR:
			case ULONGLONG|PTR:
				o = 8;
				break;

			default:
				if (ISPTR(p->in.type) &&
				     ISPTR(DECREF(p->in.type))) {
					o = 4;
					break;
				} else
					return(0);
		}
		return( p->in.right->tn.lval == o ? STARREG : 0);
	}

	return( 0 );
}

void
adrcon(CONSZ val)
{
	cerror("adrcon: val %llo\n", val);
}

void
conput(NODE *p)
{
	switch (p->in.op) {
	case ICON:
		acon(p);
		if (p->in.name[0] != '\0')
			printf("+%s", p->in.name);
		return;

	case REG:
		putstr(rnames[p->tn.rval]);
		return;

	default:
		cerror("illegal conput");
	}
}

/*ARGSUSED*/
void
insput(NODE *p)
{
	cerror("insput");
}

/*
 * Write out the upper address, like the upper register of a 2-register
 * reference, or the next memory location.
 */
void
upput(NODE *p, int size)
{
	switch (p->in.op) {
	case REG:
		if (size != SZLONG)
			cerror("upput REG not SZLONG");
		putstr(rnames[p->tn.rval + 1]);
		break;

	case NAME:
	case OREG:
		p->tn.lval++;
		adrput(p);
		p->tn.lval--;
		break;

	default:
		cerror("upput bad op %d size %d", p->in.op, size);
	}
}

void
adrput(NODE *p)
{
	int r;
	/* output an address, with offsets, from p */

	if (p->in.op == FLD)
		p = p->in.left;

	switch (p->in.op) {

	case NAME:
		zzzcode(p, 'K');
		return;

	case OREG:
		r = p->tn.rval;
#if 0
		if (R2TEST(r)) { /* double indexing */
			register int flags;

			flags = R2UPK3(r);
			if (flags & 1)
				putchar('*');
			if (flags & 4)
				putchar('-');
			if (p->tn.lval != 0 || p->in.name[0] != '\0')
				acon(p);
			if (R2UPK1(r) != 100)
				printf("(%s)", rnames[R2UPK1(r)]);
			if (flags & 2)
				putchar('+');
			printf("[%s]", rnames[R2UPK2(r)]);
			return;
		}
#endif
		if (R2TEST(r))
			cerror("adrput: unwanted double indexing: r %o", r);
		acon(p);
		if (p->in.name[0] != '\0')
			printf("+%s", p->in.name);
		printf("(%s)", rnames[p->tn.rval]);
		return;
	case ICON:
		/* addressable value of the constant */
		if (p->tn.lval != 0) {
			acon(p);
			if (p->in.name[0] != '\0')
				putchar('+');
		}
		if (p->in.name[0] != '\0')
			printf("%s", p->in.name);
		if (p->in.name[0] == '\0' && p->tn.lval == 0)
			putchar('0');
		return;

	case REG:
		putstr(rnames[p->tn.rval]);
		return;

	default:
		cerror("illegal address, op %d", p->in.op);
		return;

	}
}

/*
 * print out a constant
*/
void
acon(NODE *p)
{
	if (p->tn.lval < 0 && p->tn.lval > -0777777777777ULL)
		printf("-" CONFMT, -p->tn.lval);
	else
		printf(CONFMT, p->tn.lval);
}

int
genscall(NODE *p, int cookie)
{
	/* structure valued call */
	return(gencall(p, cookie));
}

/*
 * generate the call given by p
 */
/*ARGSUSED*/
int
gencall(NODE *p, int cookie)
{

	NODE *p1;
	int temp, temp1, m;

	temp = p->in.right ? argsize(p->in.right) : 0;

	if (p->in.op == STCALL || p->in.op == UNARY STCALL) {
		/* set aside room for structure return */

		temp1 = p->stn.stsize > temp ? p->stn.stsize : temp;
	}

	SETOFF(temp1,4);

	 /* make temp node, put offset in, and generate args */
	if (p->in.right)
		genargs(p->in.right);

	/*
	 * Verify that pushj can be emitted.
	 */
	p1 = p->in.left;
	switch (p1->in.op) {
	case ICON:
	case REG:
	case OREG:
	case NAME:
		break;
	default:
		order(p1, INAREG);
	}

	p->in.op = UNARY CALL;
	m = match(p, INTAREG|INTBREG);

	/* Remove args (if any) from stack */
	if (temp)
		printf("	subi 017,%o\n", temp);

	return(m != MDONE);
}


/*   printf conditional and unconditional branches */
void
cbgen(int o,int lab,int mode )
{
	if (o != 0 && (o < EQ || o > GT))
		cerror("bad conditional branch: %s", opst[o]);
	printf("	%s 0,L%d\n", o == 0 ? "jrst" : ccbranches[o-EQ], lab);
}

/* we have failed to match p with cookie; try another */
int
nextcook(NODE *p, int cookie)
{
	if (cookie == FORREW)
		return(0);  /* hopeless! */
	if (!(cookie&(INTAREG|INTBREG)))
		return(INTAREG|INTBREG);
	if (!(cookie&INTEMP) && asgop(p->in.op))
		return(INTEMP|INAREG|INTAREG|INTBREG|INBREG);
	return(FORREW);
}

int
lastchance(NODE *p, int cook)
{
	/* forget it! */
	return(0);
}

static void
unaryops(NODE *p)
{
	NODE *q;
	char *fn;

	switch (p->in.op) {
	case UNARY MINUS:
		fn = "__negdi2";
		break;

	case COMPL:
		fn = "__one_cmpldi2";
		break;
	default:
		return;
	}

	p->in.op = CALL;
	p->in.right = p->in.left;
	p->in.left = q = talloc();
	q->in.op = ICON;
	q->in.rall = NOPREF;
	q->in.type = INCREF(FTN + p->in.type);
	q->in.name = fn;
	q->tn.lval = 0;
	q->tn.rval = 0;
}

/* added by jwf */
struct functbl {
	int fop;
	TWORD ftype;
	char *func;
} opfunc[] = {
	{ DIV,		TANY,	"__divdi3", },
	{ MOD,		TANY,	"__moddi3", },
	{ MUL,		TANY,	"__muldi3", },
	{ PLUS,		TANY,	"__adddi3", },
	{ MINUS,	TANY,	"__subdi3", },
	{ RS,		TANY,	"__ashrdi3", },
	{ ASG DIV,	TANY,	"__divdi3", },
	{ ASG MOD,	TANY,	"__moddi3", },
	{ ASG MUL,	TANY,	"__muldi3", },
	{ ASG PLUS,	TANY,	"__adddi3", },
	{ ASG MINUS,	TANY,	"__subdi3", },
	{ ASG RS,	TANY,	"__ashrdi3", },
	{ 0,	0,	0 },
};

int e2print(NODE *p, int down, int *a, int *b);
void hardops(NODE *p);
void
hardops(NODE *p)
{
	/* change hard to do operators into function calls.  */
	NODE *q;
	TWORD t;
	struct functbl *f;
	int o;
	NODE *old,*temp;

	o = p->in.op;
	t = p->in.type;


	if (!ISLONGLONG(t))
		return;

	if (optype(o) != BITYPE)
		return unaryops(p);

	for (f = opfunc; f->fop; f++) {
		if (o == f->fop)
			goto convert;
	}
	return;

	convert:
	/*
	 * If it's a "a += b" style operator, rewrite it to "a = a + b".
	 */
	if (asgop(o)) {
		old = NIL;
		switch (p->in.left->in.op) {

		case UNARY MUL:
			q = p->in.left;
			/*
			 * rewrite (lval /= rval); as
			 *  ((*temp) = udiv((*(temp = &lval)), rval));
			 * else the compiler will evaluate lval twice.
			 */

			/* first allocate a temp storage */
			temp = talloc();
			temp->in.op = OREG;
			temp->tn.rval = TMPREG;
			temp->tn.lval = BITOOR(freetemp(1));
			temp->in.type = INCREF(p->in.type);
			temp->in.name = "";
			old = q->in.left;
			q->in.left = temp;

			/* FALLTHROUGH */
		case REG:
		case NAME:
		case OREG:
			/* change ASG OP to a simple OP */
			q = talloc();
			q->in.op = NOASG p->in.op;
			q->in.rall = NOPREF;
			q->in.type = p->in.type;
			q->in.left = tcopy(p->in.left);
			q->in.right = p->in.right;
			p->in.op = ASSIGN;
			p->in.right = q;
			p = q;

			/* on the right side only - replace *temp with
			 *(temp = &lval), build the assignment node */
			if (old) {
				temp = q->in.left; /* the "*" node */
				q = talloc();
				q->in.op = ASSIGN;
				q->in.left = temp->in.left;
				q->in.right = old;
				q->in.type = old->in.type;
				q->in.name = "";
				temp->in.left = q;
			}
			break;

		default:
			cerror( "hardops: can't compute & LHS" );
			}
		}

	/* build comma op for args to function */
	q = talloc();
	q->in.op = CM;
	q->in.rall = NOPREF;
	q->in.type = INT;
	q->in.left = p->in.left;
	q->in.right = p->in.right;
	p->in.op = CALL;
	p->in.right = q;

	/* put function name in left node of call */
	p->in.left = q = talloc();
	q->in.op = ICON;
	q->in.rall = NOPREF;
	q->in.type = INCREF(FTN + p->in.type);
	q->in.name = f->func;
	if (ISUNSIGNED(t)) {
		switch (o) {
		case DIV:
			q->in.name = "__udivdi3";
			break;
		case MOD:
			q->in.name = "__umoddi3";
			break;
		}
	}

	q->tn.lval = 0;
	q->tn.rval = 0;
}

/*
 * Do some local optimizations that must be done after optim is called.
 */
static void
optim2(NODE *p)
{
	int op = p->in.op;
	int m, ml;
	NODE *l;

	/* Remove redundant PCONV's */
	if (op == PCONV) {
		l = p->in.left;
		m = BTYPE(p->in.type);
		ml = BTYPE(l->in.type);
		if ((m == INT || m == LONG || m == LONGLONG || m == FLOAT ||
		    m == DOUBLE || m == STRTY || m == UNIONTY || m == ENUMTY ||
		    m == UNSIGNED || m == ULONG || m == ULONGLONG) &&
		    (ml == INT || ml == LONG || ml == LONGLONG || ml == FLOAT ||
		    ml == DOUBLE || ml == STRTY || ml == UNIONTY || 
		    ml == ENUMTY || ml == UNSIGNED || ml == ULONG ||
		    ml == ULONGLONG)) {
			ncopy(p, l);
			l->in.op = FREE;
			op = p->in.op;
		} else
		if (ISPTR(DECREF(p->in.type)) &&
		    (l->in.type == INCREF(STRTY))) {
			ncopy(p, l);
			l->in.op = FREE;
			op = p->in.op;
		} else
		if (ISPTR(DECREF(l->in.type)) &&
		    (p->in.type == INCREF(INT) ||
		    p->in.type == INCREF(STRTY) ||
		    p->in.type == INCREF(UNSIGNED))) {
			ncopy(p, l);
			l->in.op = FREE;
			op = p->in.op;
		}

	}
	/* Add constands, similar to the one in optim() */
	if (op == PLUS && p->in.right->in.op == ICON) {
		l = p->in.left;
		if (l->in.op == PLUS && l->in.right->in.op == ICON &&
		    (p->in.right->tn.name[0] == '\0' ||
		     l->in.right->tn.name[0] == '\0')) {
			l->in.right->tn.lval += p->in.right->tn.lval;
			if (l->in.right->tn.name[0] == '\0')
				l->in.right->tn.name = p->in.right->tn.name;
			p->in.right->in.op = FREE;
			ncopy(p, l);
			l->in.op = FREE;
		}
	}

	/* Convert "PTR undef" (void *) to "PTR uchar" */
	if (BTYPE(p->in.type) == UNDEF)
		p->in.type |= UCHAR;
	if (op == ICON) {
		if ((p->in.type == (PTR|CHAR) || p->in.type == (PTR|UCHAR))
		    && p->tn.lval == 0)
			p->tn.lval = 0700000000000;
		if ((p->in.type == (PTR|SHORT) || p->in.type == (PTR|USHORT))
		    && p->tn.lval == 0)
			p->tn.lval = 0750000000000;
	}
		
}

void
myreader(NODE *p)
{
	int e2print(NODE *p, int down, int *a, int *b);
	walkf(p, hardops);	/* convert ops to function calls */
	walkf(p, optim2);
	if (x2debug) {
		printf("myreader final tree:\n");
		fwalk(p, e2print, 0);
	}
}
