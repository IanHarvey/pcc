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

# define putstr(s)	fputs((s), stdout)

void acon(NODE *p);
int argsize(NODE *p);
void genargs(NODE *p);

static int ftlab1, ftlab2;

void
lineid(int l, char *fn)
{
	/* identify line l and file fn */
	printf("#	line %d, file %s\n", l, fn);
}

void
defname(char *name, int visib)
{
	printf("	.align 4\n");
	if (visib)
		printf("	.globl %s\n", name);
	printf("%s:\n", name);
}

void
deflab(int label)
{
	printf(LABFMT ":\n", label);
}

static int isoptim;

void
prologue(int regs, int autos)
{
	int addto;

	if (regs < 0 || autos < 0) {
		/*
		 * non-optimized code, jump to epilogue for code generation.
		 */
		ftlab1 = getlab();
		ftlab2 = getlab();
		printf("	jmp " LABFMT "\n", ftlab1);
		deflab(ftlab2);
	} else {
		/*
		 * We here know what register to save and how much to 
		 * add to the stack.
		 */
		addto = (maxautooff - AUTOINIT)/SZCHAR;
		printf("	pushl %%ebp\n");
		printf("	movl %%esp,%%ebp\n");
		if (addto)
			printf("	subl $%d,%%esp\n", addto);
		isoptim = 1;
	}
}

/*
 * End of block.
 */
void
eoftn(int regs, int autos, int retlab)
{
	register OFFSZ spoff;	/* offset from stack pointer */

	spoff = maxautooff;
	if (spoff >= AUTOINIT)
		spoff -= AUTOINIT;
	spoff /= SZCHAR;
	/* return from function code */
	deflab(retlab);
	printf("	leave\n");
	printf("	ret\n");

	/* Prolog code */
	if (isoptim == 0) {
		deflab(ftlab1);
		printf("	pushl %%ebp\n");
		printf("	movl %%esp,%%ebp\n");
		if (spoff)
			printf("	subl $%lld,%%esp\n", spoff);
		printf("	jmp " LABFMT "\n", ftlab2);
	}
	isoptim = 0;
}

static char *loctbl[] = { "text", "data", "data", "text", "text", "stab" };

void
setlocc(int locctr)
{
	static int lastloc;

	if (locctr == lastloc)
		return;

	lastloc = locctr;
	printf("	.%s\n", loctbl[locctr]);
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
		cerror("hopcode2: %d", o);
	}
	printf("%s%c", str, f);
}

char *
rnames[] = {  /* keyed to register number tokens */
	"%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp",
};

int rstatus[] = {
	STAREG, STAREG, STAREG, STAREG, SAREG, SAREG, 0, 0,
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
				cerror("tlen type %d not pointer");
			return SZPOINT/SZCHAR;
		}
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
			cerror("bad shift reg");
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
		default: cerror("ZB: %d", p->n_type);
		}
		break;

	case 'C':  /* remove from stack after subroutine call */
		if (p->n_rval)
			printf("	addl $%d, %s\n",
			    p->n_rval/SZCHAR, rnames[STKREG]);
		break;
		

	case 'L':
	case 'R':
	case '1':
		/*
		 * Prints out a register of small type, like %al.
		 * Type is determined by op.
		 */
		r = getlr(p, c);
		if (r->n_op != REG)
			adrput(stdout, r);
		else if (p->n_type == SHORT || p->n_type == USHORT)
			printf("%%%cx", rnames[r->n_rval][2]);
		else
			printf("%%%cl", rnames[r->n_rval][2]);
		break;

	default:
		cerror("zzzcode %c", c);
	}
}

/* set up temporary registers */
void
setregs()
{
	fregs = 4;	/* 4 free regs on x86 (0-3) */
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

int
flshape(NODE *p)
{
	register int o = p->n_op;

	return (o == REG || o == NAME || o == ICON ||
		(o == OREG && (!R2TEST(p->n_rval) || tlen(p) == 1)));
}

/* INTEMP shapes must not contain any temporary registers */
int
shtemp(NODE *p)
{
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

	o = p->n_op;
#if 0
	if (o == NAME || (o == OREG && !R2TEST(p->n_rval)) || o == ICON)
		return(STARNM);
#endif

	if ((o == INCR || o == MINUS) &&
	    (p->n_left->n_op == REG && p->n_right->n_op == ICON) &&
	    p->n_right->n_name[0] == '\0') {
		switch (p->n_type) {
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
				if (ISPTR(p->n_type) &&
				     ISPTR(DECREF(p->n_type))) {
					o = 4;
					break;
				} else
					return(0);
		}
		return( p->n_right->n_lval == o ? STARREG : 0);
	}

	return( 0 );
}

void
adrcon(CONSZ val)
{
	printf("$" CONFMT, val);
}

void
conput(NODE *p)
{
	switch (p->n_op) {
	case ICON:
		if (p->n_lval != 0) {
			printf(CONFMT, p->n_lval);
			if (p->n_name[0] != '\0')
				putchar('+');
		}
		if (p->n_name[0] != '\0')
			printf("%s", p->n_name);
		if (p->n_name[0] == '\0' && p->n_lval == 0)
			putchar('0');
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

	size /= SZLONG;
	switch (p->n_op) {
	case REG:
		putstr(rnames[p->n_rval + size]);
		break;

	case NAME:
	case OREG:
		p->n_lval += size;
		adrput(stdout, p);
		p->n_lval -= size;
		break;
	case ICON:
		printf(CONFMT, p->n_lval >> (36 * size));
		break;
	default:
		cerror("upput bad op %d size %d", p->n_op, size);
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
		if (p->n_lval) {
			p->n_op = ICON;
			conput(p);
			p->n_op = OREG;
		}
		fprintf(io, "(%s)", rnames[p->n_rval]);
		return;
	case ICON:
		/* addressable value of the constant */
		putchar('$');
		conput(p);
		return;

	case REG:
		putstr(rnames[p->n_rval]);
		return;

	default:
		comperr("illegal address, op %d", p->n_op);
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
		cerror("bad conditional branch: %s", opst[o]);
	printf("	%s " LABFMT "\n", ccbranches[o-EQ], lab);
}

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

void
myreader(NODE *p)
{
	int e2print(NODE *p, int down, int *a, int *b);
//	walkf(p, hardops);	/* convert ops to function calls */
	walkf(p, optim2);
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

/*
 * Remove last goto.
 */
void
myoptim(struct interpass *ip)
{
	while (ip->sqelem.sqe_next->type != IP_EPILOG)
		ip = ip->sqelem.sqe_next;
	if (ip->type != IP_NODE || ip->ip_node->n_op != GOTO)
		cerror("myoptim");
	tfree(ip->ip_node);
	*ip = *ip->sqelem.sqe_next;
}
