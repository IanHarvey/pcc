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

static int ftlab1, ftlab2;

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

static TWORD ftype;
static int addto;

void
prologue(int regs, int autos, TWORD t)
{
	ftype = t;

	if (regs > 0 && regs != MINRVAR)
		comperr("fix prologue register savings", regs);
	if (Oflag) {
		/* Optimizer running, save space on stack */
		addto = (maxautooff - AUTOINIT)/SZCHAR;
		printf("	push.w FB\n");
		printf("	mov.w SP,FB\n");
		if (addto)
			printf("	add.b #-%d,SP\n", addto);
	} else {
		/* non-optimized code, jump to epilogue for code generation */
		ftlab1 = getlab();
		ftlab2 = getlab();
		printf("	jmp " LABFMT "\n", ftlab1);
		deflab(ftlab2);
	}
}

/*
 * End of block.
 */
void
eoftn(int regs, int autos, int retlab)
{
	if (regs != MINRVAR)
		comperr("fix eoftn register savings %x", regs);

	if (Oflag == 0)
		addto = (maxautooff - AUTOINIT)/SZCHAR;

	/* return from function code */
	deflab(retlab);
	/* struct return needs special treatment */
	if (ftype == STRTY || ftype == UNIONTY) {
		comperr("fix struct return in eoftn");
#if 0
		printf("	movl 8(%%ebp),%%eax\n");
		printf("	leave\n");
		printf("	ret $4\n");
#endif
	} else {
		printf("	mov.w FP,SP\n");
		printf("	pop.w FP\n");
		printf("	rts\n");
	}

	/* Prolog code */
	if (Oflag == 0) {
		deflab(ftlab1);
		printf("	push.w FB\n");
		printf("	mov.w SP,FB\n");
		if (addto)
			printf("	add.b #-%d,SP\n", addto);
		printf("	jmp " LABFMT "\n", ftlab2);
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

char *
rnames[] = {  /* keyed to register number tokens */
	"R0", "R2", "R1", "R3", "A0", "A1", "FB", "USP",
};

int rstatus[] = {
	STAREG|SAREG, STAREG|SAREG, SAREG, SAREG,
	STBREG|SBREG, SBREG, 0, 0,
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
			return SZPOINT/SZCHAR;
		}
}

void
zzzcode(NODE *p, int c)
{
	switch (c) {
	case 'A': /* print negative shift constant */
		p = getlr(p, 'R');
		if (p->n_op != ICON)
			comperr("ZA bad use");
		p->n_lval = -p->n_lval;
		adrput(stdout, p);
		p->n_lval = -p->n_lval;
		break;

	default:
		comperr("bad zzzcode %c", c);
	}
}

/* set up temporary registers */
void
setregs()
{
	fregs = 3;	/* 3 free regs on m16c (0-2) */
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
		fputs(rnames[p->n_rval + 1], stdout);
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
		fprintf(io, "[%s]", rnames[p->n_rval]);
		return;
	case ICON:
		/* addressable value of the constant */
		fputc('#', io);
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

void
mygenregs(NODE *p)
{
#if 0
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

