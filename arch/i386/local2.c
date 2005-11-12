/*	$Id$	*/
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


# include "pass2.h"
# include <ctype.h>

void acon(NODE *p);
int argsize(NODE *p);
void genargs(NODE *p);
static void sconv(NODE *p);

void
lineid(int l, char *fn)
{
	/* identify line l and file fn */
	printf("#	line %d, file %s\n", l, fn);
}

void
deflab(int label)
{
	printf(LABFMT ":\n", label);
}

static int regoff[7];
static TWORD ftype;

/*
 * Print out the prolog assembler.
 * addto and regoff are already calculated.
 */
static void
prtprolog(struct interpass_prolog *ipp, int addto)
{
	int i, j;

	printf("	pushl %%ebp\n");
	printf("	movl %%esp,%%ebp\n");
	if (addto)
		printf("	subl $%d,%%esp\n", addto);
	for (i = ipp->ipp_regs, j = 0; i; i >>= 1, j++)
		if (i & 1)
			fprintf(stdout, "	movl %s,-%d(%s)\n",
			    rnames[j], regoff[j], rnames[FPREG]);
}

/*
 * calculate stack size and offsets */
static int
offcalc(struct interpass_prolog *ipp)
{
	int i, j, addto;

	addto = p2maxautooff;
	if (addto >= AUTOINIT)
		addto -= AUTOINIT;
	addto /= SZCHAR;
	for (i = ipp->ipp_regs, j = 0; i ; i >>= 1, j++) {
		if (i & 1) {
			addto += SZINT/SZCHAR;
			regoff[j] = addto;
		}
	}
	return addto;
}

void
prologue(struct interpass_prolog *ipp)
{
	int addto;

	ftype = ipp->ipp_type;
	if (ipp->ipp_vis)
		printf("	.globl %s\n", ipp->ipp_name);
	printf("	.align 4\n");
	printf("%s:\n", ipp->ipp_name);
	/*
	 * We here know what register to save and how much to 
	 * add to the stack.
	 */
	addto = offcalc(ipp);
	prtprolog(ipp, addto);
}

void
eoftn(struct interpass_prolog *ipp)
{
	int i, j;

	if (ipp->ipp_ip.ip_lbl == 0)
		return; /* no code needs to be generated */

	/* return from function code */
	for (i = ipp->ipp_regs, j = 0; i ; i >>= 1, j++) {
		if (i & 1)
			fprintf(stdout, "	movl -%d(%s),%s\n",
			    regoff[j], rnames[FPREG], rnames[j]);
			
	}

	/* struct return needs special treatment */
	if (ftype == STRTY || ftype == UNIONTY) {
		printf("	movl 8(%%ebp),%%eax\n");
		printf("	leave\n");
		printf("	ret $4\n");
	} else {
		printf("	leave\n");
		printf("	ret\n");
	}
}

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
		str = "or";
		break;
	case ER:
		str = "xor";
		break;
	default:
		comperr("hopcode2: %d", o);
		str = 0; /* XXX gcc */
	}
	printf("%s%c", str, f);
}

char *rnames[] = {
	"%eax", "%edx", "%ecx", "%esi", "%edi", "%ebx", "%ebp", "%esp",
	"%al", "%ah", "%dl", "%dh", "%cl", "%ch", "%bl", "%bh",
	"eaxedx", "eaxecx", "eaxebx", "eaxesi", "eaxedi", "edxecx",
	"edxebx", "edxesi", "edxedi", "ecxebx", "ecxesi", "ecxedi",
	"ebxesi", "ebxedi", "esiedi",
	"%st0", "%st1", "%st2", "%st3", "%st4", "%st5", "%st6", "%st7",
};

int
tlen(p) NODE *p;
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
	expand(p, 0, "	cmpl UR,UL\n");
	if (cb1) cbgen(cb1, s);
	if (cb2) cbgen(cb2, e);
	expand(p, 0, "	cmpl AR,AL\n");
	cbgen(p->n_op, e);
	deflab(s);
}

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
	zzzcode(p, 'R');
	fprintf(stdout, ",");
	zzzcode(p, '1');
	fprintf(stdout, "\n");

	/* AND away the bits from dest */
	andval = ~(((1 << fsz) - 1) << shift);
	fprintf(stdout, "	and%c $%d,", tch, andval);
	adrput(stdout, fn->n_left);
	fprintf(stdout, "\n");

	/* AND away unwanted bits from src */
	andval = ((1 << fsz) - 1);
	fprintf(stdout, "	and%c $%d,", tch, andval);
	zzzcode(p, '1');
	fprintf(stdout, "\n");

	/* SHIFT left src number of bits */
	if (shift) {
		fprintf(stdout, "	sal%c $%d,", tch, shift);
		zzzcode(p, '1');
		fprintf(stdout, "\n");
	}

	/* OR in src to dest */
	fprintf(stdout, "	or%c ", tch);
	zzzcode(p, '1');
	fprintf(stdout, ",");
	adrput(stdout, fn->n_left);
	fprintf(stdout, "\n");
}

/*
 * Push a structure on stack as argument.
 * the scratch registers are already free here
 */
static void
starg(NODE *p)
{
	FILE *fp = stdout;

	if (p->n_left->n_op == REG && p->n_left->n_type == PTR+STRTY)
		return; /* already on stack */
	fprintf(fp, "	subl $%d,%%esp\n", p->n_stsize);
	fprintf(fp, "	pushl $%d\n", p->n_stsize);
	expand(p, 0, "	pushl AL\n");
	expand(p, 0, "	leal 8(%esp),A1\n");
	expand(p, 0, "	pushl A1\n");
	fprintf(fp, "	call memcpy\n");
	fprintf(fp, "	addl $12,%%esp\n");
}

/*
 * Compare two floating point numbers.
 */
static void
fcomp(NODE *p)  
{
	
	if (p->n_left->n_op == REG)
		expand(p, 0, "	fucompp\n");	/* emit compare insn  */
	else if (p->n_left->n_type == DOUBLE)
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

void
zzzcode(NODE *p, int c)
{
	NODE *r;

	switch (c) {
	case 'A':
		/*
		 * Shift operations. Either the right node is a constant
		 * or a register, in the latter case it must be %cl.
		 */
		p = p->n_right;
		if (p->n_op == ICON)
			printf("$" CONFMT, p->n_lval);
		else if (p->n_op != REG || p->n_rval != 2) /* CX */
			comperr("bad shift reg");
		else
			printf("%%cl"); 
		break;

	case 'B':
		/*
		 * Print conversion chars for loading into register.
		 */
		p = getlr(p, 'R');
		switch (p->n_type) {
		case SHORT: printf("swl"); break;
		case USHORT: printf("zwl"); break;
		case CHAR: printf("sbl"); break;
		case UCHAR: printf("zbl"); break;
		default: comperr("ZB: %d", p->n_type);
		}
		break;

	case 'C':  /* remove from stack after subroutine call */
		if (p->n_rval)
			printf("	addl $%d, %s\n",
			    p->n_rval*4, rnames[STKREG]);
		break;

	case 'D': /* Long long comparision */
		twollcomp(p);
		break;

	case 'E': /* Assign to bitfield */
		bfasg(p);
		break;

	case 'F': /* Structure argument */
		starg(p);
		break;

	case 'G': /* Floating point compare */
		fcomp(p);
		break;

	case 'H': /* Fix correct order of sub from stack */
		/* Check which leg was evaluated first */
		if ((p->n_su & DORIGHT) == 0)
			putchar('r');
		break;

	case 'I': /* high part of init constant */
		if (p->n_name[0] != '\0')
			comperr("named highword");
		fprintf(stdout, CONFMT, (p->n_lval >> 32) & 0xffffffff);
		break;

	case 'J': /* convert unsigned long long to floating point */
		ulltofp(p);
		break;

	case 'K': /* do scalar casts */
		sconv(p);
		break;

	case 'L':
	case 'R':
	case '1':
		/*
		 * Prints out a register of small type, like %al.
		 * Type is determined by op.
		 */
		r = getlr(p, c);
		if (r->n_op != REG && r->n_op != MOVE)
			adrput(stdout, r);
		comperr("zzz LR1");
#if 0
		else if (r->n_type == SHORT || r->n_type == USHORT)
			printf("%%%cx", rnames[r->n_rval][2]);
		else if (r->n_type == CHAR || r->n_type == UCHAR)
			printf("%%%cl", rnames[r->n_rval][2]);
		else
			printf("%s", rnames[r->n_rval]);
#endif
		break;

	default:
		comperr("zzzcode %c", c);
	}
}

/*
 * Generate scalar cast code.
 */
void
sconv(NODE *p)
{
	NODE *q = p->n_left;
	int s,d;

	s = 0, d = 0;
	switch (p->n_type) {
	case CHAR:
	case UCHAR:
		d = 'b';
		break;

	case SHORT:
	case USHORT:
		d = 'w';
		break;

	case INT:
	case UNSIGNED:
		d = 'l';
		break;
	default:
		comperr("unsupported sconv, type %x", p->n_type);
	}
	switch (q->n_type) {
	case CHAR:
	case UCHAR:
		s = 'b';
		break;

	case SHORT:
	case USHORT:
		s = 'w';
		break;

	case INT:
	case UNSIGNED:
		s = 'l';
		break;
	default:
		comperr("unsupported sconv src, type %x", q->n_type);
	}

	printf("	mov%c%c%c ", ISUNSIGNED(p->n_type) ? 'z' : 's', s, d);
	zzzcode(p, 'L');
	putchar(',');
	adrput(stdout, getlr(p, '1'));
	putchar('\n');
}

/* set up temporary registers */
void
setregs()
{
	fregs = 3;	/* 3 free regs on x86 (0-2) */
}

/*ARGSUSED*/
int
rewfld(NODE *p)
{
	return(1);
}

int canaddr(NODE *);
int
canaddr(NODE *p)
{
	int o = p->n_op;

	if (o==NAME || o==REG || o==ICON || o==OREG ||
	    (o==UMUL && shumul(p->n_left)))
		return(1);
	return(0);
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
	printf("$" CONFMT, val);
}

void
conput(FILE *fp, NODE *p)
{
	int val = p->n_lval;

	switch (p->n_op) {
	case ICON:
		if (p->n_name[0] != '\0') {
			fprintf(fp, "%s", p->n_name);
			if (val)
				fprintf(fp, "+%d", val);
		} else
			fprintf(fp, "%d", val);
		return;

	default:
		comperr("illegal conput");
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
		comperr("upput");
#if 0
		fputs(rnames[p->n_rval + 1], stdout);
#endif
		break;

	case NAME:
	case OREG:
		p->n_lval += size;
		adrput(stdout, p);
		p->n_lval -= size;
		break;
	case ICON:
		fprintf(stdout, "$" CONFMT, p->n_lval >> 32);
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
		if (p->n_name[0] != '\0')
			fputs(p->n_name, io);
		if (p->n_lval != 0)
			fprintf(io, "+" CONFMT, p->n_lval);
		return;

	case OREG:
		r = p->n_rval;
		if (p->n_lval)
			fprintf(io, "%d", (int)p->n_lval);
		fprintf(io, "(%s)", rnames[p->n_rval]);
		return;
	case ICON:
		/* addressable value of the constant */
		fputc('$', io);
		conput(io, p);
		return;

	case MOVE:
	case REG:
		fprintf(io, "%s", rnames[p->n_rval]);
		return;

	default:
		comperr("illegal address, op %d, node %p", p->n_op, p);
		return;

	}
}

static char *
ccbranches[] = {
	"je",		/* jumpe */
	"jne",		/* jumpn */
	"jle",		/* jumple */
	"jl",		/* jumpl */
	"jge",		/* jumpge */
	"jg",		/* jumpg */
	"jbe",		/* jumple (jlequ) */
	"jb",		/* jumpl (jlssu) */
	"jae",		/* jumpge (jgequ) */
	"ja",		/* jumpg (jgtru) */
};


/*   printf conditional and unconditional branches */
void
cbgen(int o, int lab)
{
	if (o < EQ || o > UGT)
		comperr("bad conditional branch: %s", opst[o]);
	printf("	%s " LABFMT "\n", ccbranches[o-EQ], lab);
}

#if 0
/*
 * Do some local optimizations that must be done after optim is called.
 */
static void
optim2(NODE *p)
{
	int op = p->n_op;
	int m, ml;
	NODE *l;

	/* Remove redundant PCONV's */
	if (op == PCONV) {
		l = p->n_left;
		m = BTYPE(p->n_type);
		ml = BTYPE(l->n_type);
		if ((m == INT || m == LONG || m == LONGLONG || m == FLOAT ||
		    m == DOUBLE || m == STRTY || m == UNIONTY || m == ENUMTY ||
		    m == UNSIGNED || m == ULONG || m == ULONGLONG) &&
		    (ml == INT || ml == LONG || ml == LONGLONG || ml == FLOAT ||
		    ml == DOUBLE || ml == STRTY || ml == UNIONTY || 
		    ml == ENUMTY || ml == UNSIGNED || ml == ULONG ||
		    ml == ULONGLONG) && ISPTR(l->n_type)) {
			*p = *l;
			nfree(l);
			op = p->n_op;
		} else
		if (ISPTR(DECREF(p->n_type)) &&
		    (l->n_type == INCREF(STRTY))) {
			*p = *l;
			nfree(l);
			op = p->n_op;
		} else
		if (ISPTR(DECREF(l->n_type)) &&
		    (p->n_type == INCREF(INT) ||
		    p->n_type == INCREF(STRTY) ||
		    p->n_type == INCREF(UNSIGNED))) {
			*p = *l;
			nfree(l);
			op = p->n_op;
		}

	}
	/* Add constands, similar to the one in optim() */
	if (op == PLUS && p->n_right->n_op == ICON) {
		l = p->n_left;
		if (l->n_op == PLUS && l->n_right->n_op == ICON &&
		    (p->n_right->n_name[0] == '\0' ||
		     l->n_right->n_name[0] == '\0')) {
			l->n_right->n_lval += p->n_right->n_lval;
			if (l->n_right->n_name[0] == '\0')
				l->n_right->n_name = p->n_right->n_name;
			nfree(p->n_right);
			*p = *l;
			nfree(l);
		}
	}

	/* Convert "PTR undef" (void *) to "PTR uchar" */
	/* XXX - should be done in MI code */
	if (BTYPE(p->n_type) == VOID)
		p->n_type = (p->n_type & ~BTMASK) | UCHAR;
}
#endif

static void
myhardops(NODE *p)
{
	int ty = optype(p->n_op);
	NODE *l, *r, *q;

	if (ty == UTYPE)
		return myhardops(p->n_left);
	if (ty != BITYPE)
		return;
	myhardops(p->n_right);
	if (p->n_op != STASG)
		return;

	/*
	 * If the structure size to copy is less than 32 byte, let it
	 * be and generate move instructions later.  Otherwise convert it 
	 * to memcpy() calls, unless it has a STCALL function as its
	 * right node, in which case it is untouched.
	 * STCALL returns are handled special.
	 */
	if (p->n_right->n_op == STCALL || p->n_right->n_op == USTCALL)
		return;
	l = p->n_left;
	if (l->n_op == UMUL)
		l = nfree(l);
	else if (l->n_op == NAME) {
		l->n_op = ICON; /* Constant reference */
		l->n_type = INCREF(l->n_type);
	} else
		comperr("myhardops");
	r = p->n_right;
	q = mkbinode(CM, l, r, 0);
	q = mkbinode(CM, q, mklnode(ICON, p->n_stsize, 0, INT), 0);
	p->n_op = CALL;
	p->n_right = q;
	p->n_left = mklnode(ICON, 0, 0, 0);
	p->n_left->n_name = "memcpy";
}

void
myreader(NODE *p)
{
	int e2print(NODE *p, int down, int *a, int *b);
//	walkf(p, optim2);
	myhardops(p);
	if (x2debug) {
		printf("myreader final tree:\n");
		fwalk(p, e2print, 0);
	}
}

/*
 * Remove some PCONVs after OREGs are created.
 */
static void
pconv2(NODE *p)
{
	NODE *q;

	if (p->n_op == PLUS) {
		if (p->n_type == (PTR|SHORT) || p->n_type == (PTR|USHORT)) {
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
mygenregs(NODE *p)
{
	if (p->n_op == MINUS && p->n_type == DOUBLE &&
	    (p->n_su & (LMASK|RMASK)) == (LREG|RREG)) {
		p->n_su |= DORIGHT;
	}
	/* Must walk down correct node first for logops to work */
	if (p->n_op != CBRANCH)
		return;
	p = p->n_left;
	if ((p->n_su & (LMASK|RMASK)) != (LREG|RREG))
		return;
	p->n_su &= ~DORIGHT;
}

/*
 * Remove last goto.
 */
void
myoptim(struct interpass *ip)
{
#if 0
	while (ip->sqelem.sqe_next->type != IP_EPILOG)
		ip = ip->sqelem.sqe_next;
	if (ip->type != IP_NODE || ip->ip_node->n_op != GOTO)
		comperr("myoptim");
	tfree(ip->ip_node);
	*ip = *ip->sqelem.sqe_next;
#endif
}

struct hardops hardops[] = {
	{ MUL, LONGLONG, "__muldi3" },
	{ MUL, ULONGLONG, "__muldi3" },
	{ DIV, LONGLONG, "__divdi3" },
	{ DIV, ULONGLONG, "__udivdi3" },
	{ MOD, LONGLONG, "__moddi3" },
	{ MOD, ULONGLONG, "__umoddi3" },
	{ RS, LONGLONG, "__ashrdi3" },
	{ RS, ULONGLONG, "__lshrdi3" },
	{ LS, LONGLONG, "__ashldi3" },
	{ LS, ULONGLONG, "__ashldi3" },
#if 0
	{ STASG, PTR+STRTY, "memcpy" },
	{ STASG, PTR+UNIONTY, "memcpy" },
#endif
	{ 0 },
};

void
rmove(int s, int d, int c)
{
	printf("	movl %s,%s\n", rnames[s], rnames[d]);
}

#ifdef MULTICLASS
/*
 * Return all elements in class class as bits.
 */
int
classmask(int class)
{
	switch (class) {
	case CLASSA:
		return AREGS;
	case CLASSB:
		return BREGS;
	case CLASSC:
		return CREGS;
	case CLASSD:
		return DREGS;
	default:
		comperr("classmask: bad class %d", class);
	}
	return 0; /* XXX */
}

int
tclassmask(int class)
{
	switch (class) {
	case CLASSA:
		return TAREGS;
	case CLASSB:
		return TBREGS;
	case CLASSC:
		return TCREGS;
	case CLASSD:
		return TDREGS;
	default:
		comperr("tclassmask: bad class %d", class);
	}
	return 0; /* XXX */
}

#define	R REGBIT
/*
 * Which registers in class a are unusable if the adj register in
 * class c is already used.
 */
static int a_c_adj[] = {
	R(EAX)|R(EDX), R(EAX)|R(ECX), R(EAX)|R(EBX), R(EAX)|R(ESI), 
	R(EAX)|R(EDI), R(EDX)|R(ECX), R(EDX)|R(EBX), R(EDX)|R(ESI), 
	R(EDX)|R(EDI), R(ECX)|R(EBX), R(ECX)|R(ESI), R(ECX)|R(EDI), 
	R(EBX)|R(ESI), R(EBX)|R(EDI), R(ESI)|R(EDI),
};

static int b_c_adj[] = {
	R(AL)|R(AH)|R(DL)|R(DH), R(AL)|R(AH)|R(CL)|R(CH),
	R(AL)|R(AH)|R(BL)|R(BH), R(AL)|R(AH), R(AL)|R(AH),
	R(DL)|R(DH)|R(CL)|R(CH), R(DL)|R(DH)|R(BL)|R(BH),
	R(DL)|R(DH), R(DL)|R(DH), R(CL)|R(CH)|R(BL)|R(BH), 
	R(CL)|R(CH), R(CL)|R(CH), R(BL)|R(BH), R(BL)|R(BH), 0,
};

static int c_a_adj[] = {
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI), /* EAX */
	R(EAXEDX)|R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI), /* EDX */
	R(EAXECX)|R(EDXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI), /* ECX */
	R(EAXEBX)|R(EDXEBX)|R(ECXEBX)|R(EBXESI)|R(EBXEDI), /* EBX */
	R(EAXESI)|R(EDXESI)|R(ECXESI)|R(EBXESI)|R(ESIEDI), /* ESI */
	R(EAXEDI)|R(EDXEDI)|R(ECXEDI)|R(EBXEDI)|R(ESIEDI), /* EDI */
};

static int c_b_adj[] = {
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI), /* AL */
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI), /* AH */
	R(EAXEDX)|R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI), /* DL */
	R(EAXEDX)|R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI), /* DH */
	R(EAXECX)|R(EDXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI), /* CL */
	R(EAXECX)|R(EDXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI), /* CH */
	R(EAXEBX)|R(EDXEBX)|R(ECXEBX)|R(EBXESI)|R(EBXEDI), /* BL */
	R(EAXEBX)|R(EDXEBX)|R(ECXEBX)|R(EBXESI)|R(EBXEDI), /* BH */
};

static int c_c_adj[] = {
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI)| /* EAXEDX */
	R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI),
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI)| /* EAXECX */
	R(EDXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI),
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI)| /* EAXEBX */
	R(EDXEBX)|R(ECXEBX)|R(EBXESI)|R(EBXEDI),
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI)| /* EAXESI */
	R(EDXESI)|R(ECXESI)|R(EBXESI)|R(ESIEDI),
	R(EAXEDX)|R(EAXECX)|R(EAXEBX)|R(EAXESI)|R(EAXEDI)| /* EAXEDI */
	R(EDXEDI)|R(ECXEDI)|R(EBXEDI)|R(ESIEDI),
	R(EAXEDX)|R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI)| /* EDXECX */
	R(EAXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI),
	R(EAXEDX)|R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI)| /* EDXEBX */
	R(EAXEBX)|R(ECXEBX)|R(EBXESI)|R(EBXEDI),
	R(EAXEDX)|R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI)| /* EDXESI */
	R(EAXESI)|R(ECXESI)|R(EBXESI)|R(ESIEDI),
	R(EAXEDX)|R(EDXECX)|R(EDXEBX)|R(EDXESI)|R(EDXEDI)| /* EDXEDI */
	R(EAXEDI)|R(ECXEDI)|R(EBXEDI)|R(ESIEDI),
	R(EAXECX)|R(EDXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI)| /* ECXEBX */
	R(EAXEBX)|R(EDXEBX)|R(EBXESI)|R(EBXEDI),
	R(EAXECX)|R(EDXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI)| /* ECXESI */
	R(EAXESI)|R(EDXESI)|R(EBXESI)|R(ESIEDI),
	R(EAXECX)|R(EDXECX)|R(ECXEBX)|R(ECXESI)|R(ECXEDI)| /* ECXEDI */
	R(EAXEDI)|R(EDXEDI)|R(EBXEDI)|R(ESIEDI),
	R(EAXEBX)|R(EDXEBX)|R(ECXEBX)|R(EBXESI)|R(EBXEDI)| /* EBXESI */
	R(EAXESI)|R(EDXESI)|R(ECXESI)|R(ESIEDI),
	R(EAXEBX)|R(EDXEBX)|R(ECXEBX)|R(EBXESI)|R(EBXEDI)| /* EBXEDI */
	R(EAXEDI)|R(EDXEDI)|R(ECXEDI)|R(ESIEDI),
	R(EAXESI)|R(EDXESI)|R(ECXESI)|R(EBXESI)|R(ESIEDI)| /* ESIEDI */
	R(EAXEDI)|R(EDXEDI)|R(ECXEDI)|R(EBXEDI),
};

/*
 * Return a bitfield of all registers in thisclass that is aliased
 * by an element adjnum in adjclass.
 */
int
aliasmap(int thisclass, int adjnum, int adjclass)
{
	switch (thisclass) {
	case CLASSA:
		switch (adjclass) {
		case CLASSA:
			return REGBIT(adjnum);
		case CLASSB:
			return REGBIT((adjnum>>1));
		case CLASSC:
			return a_c_adj[adjnum];
		default:
			return 0;
		}
	case CLASSB:
		switch (adjclass) {
		case CLASSA:
			adjnum <<= 1;
			adjnum = REGBIT(adjnum)|REGBIT(adjnum+1);
			return adjnum & ~BREGS;
		case CLASSB:
			return REGBIT(adjnum);
		case CLASSC:
			return b_c_adj[adjnum];
		default:
			return 0;
		}
	case CLASSC:
		switch (adjclass) {
		case CLASSA:
			return c_a_adj[adjnum];
		case CLASSB:
			return c_b_adj[adjnum];
		case CLASSC:
			return c_c_adj[adjnum];
		default:
			return 0;
		}
	case CLASSD:
		if (adjclass == CLASSD)
			return REGBIT(adjnum);
		return 0;
	}
	return 0;
}

char colormap[NUMCLASS][NUMAREG][NUMBREG][NUMCREG][NUMDREG];
/*
 * Initialize array to do a quich search of the colorability.
 */
void
cmapinit()
{
	int a, b, c, d, i, r;

	for (i = 0; i < NUMCLASS; i++) {
	for (a = 0; a < NUMAREG; a++) {
	for (b = 0; b < NUMBREG; b++) {
	for (c = 0; c < NUMCREG; c++) {
	for (d = 0; d < NUMDREG; d++) {
		r = aliasmap(i+1, a, CLASSA) |
		    aliasmap(i+1, b, CLASSB) |
		    aliasmap(i+1, c, CLASSC) |
		    aliasmap(i+1, d, CLASSD);
		colormap[i][a][b][c][d] = (r ^ AREGS)!=0;
if (colormap[i][a][b][c][d] < 0 || colormap[i][a][b][c][d] > 1)
	comperr("colormap");
	}
	}
	}
	}
	}
}
int regK[] = { 0, NUMAREG, NUMBREG, NUMCREG, NUMDREG };

int
type2class(int t)
{
	if (t == CHAR || t == UCHAR)
		return CLASSB;
	if (t == LONGLONG || t == ULONGLONG)
		return CLASSC;
	if (t == FLOAT || t == DOUBLE || t == LDOUBLE)
		return CLASSD;
	return CLASSA;
}

int rgoff[5] = { 0, 0, 8, 8, 15 };
#endif
