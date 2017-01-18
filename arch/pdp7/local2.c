/*	$Id$	*/
/*
 * Copyright (c) 2017 Anders Magnusson (ragge@ludd.luth.se).
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
# include <string.h>

#if defined(PECOFFABI) || defined(MACHOABI) || defined(AOUTABI)
#define EXPREFIX	"_"
#else
#define EXPREFIX	""
#endif

static struct consts {
	struct consts *next;
	int num;
	CONSZ val;
	char *str;
} *copole;

static int curargs, curnum;


void
deflab(int label)
{
	printf(LABFMT ":\n", label);
}

void
prologue(struct interpass_prolog *ipp)
{
	printf("0\n");	/* return address */
}

void
eoftn(struct interpass_prolog *ipp)
{
	if (ipp->ipp_ip.ip_lbl)
		printf("	jmp %s i\n", ipp->ipp_name);
	while (copole) {
		printf("LC%d:	", copole->num);
		if (copole->str[0] != '\0') {
			printf("%s", copole->str);
			if (copole->val)
				printf("+%llo", copole->val);
		} else
			printf("%llo", copole->val);
		printf("\n");
		copole = copole->next;
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

int
fldexpand(NODE *p, int cookie, char **cp)
{
	comperr("fldexpand");
	return 0;
}

void
zzzcode(NODE *p, int c)
{
	struct consts *co;
	int i;

	switch (c) {
	case 'A': /* print out label arg address */
		printf("LA%d", curargs - p->n_rval);
		break;

	case 'B': /* put aside constant value for later use */
		co = tmpalloc(sizeof(struct consts));
		co->num = curnum++;
		co->val = getlval(p);
		co->str = p->n_name;
		co->next = copole;
		copole = co;
		printf("LC%d", co->num);
		break;

	case 'C': /* print out space for label args */
		for (i = 0; i < (int)p->n_qual; i++)
			printf("LA%d:	0\n", i+1);
		break;

	case 'D': /* print out NOP if not pointer */
		if (!ISPTR(p->n_type))
			printf("	nop\n");
		break;

	default:
		comperr("zzzcode %c", c);
	}
}

int canaddr(NODE *);
int
canaddr(NODE *p)
{
	int o = p->n_op;

	if (o==NAME || o==REG || o==ICON || o==OREG ||
	    (o==UMUL && shumul(p->n_left, SOREG)))
		return(1);
	return(0);
}

/*
 * Does the bitfield shape match?
 */
int
flshape(NODE *p)
{
	comperr("flshape");
	return 0;
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
	int val = (int)getlval(p);

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
		printf("%%%s", &rnames[p->n_rval][3]);
		break;

	case NAME:
	case OREG:
		setlval(p, getlval(p) + size);
		adrput(stdout, p);
		setlval(p, getlval(p) - size);
		break;
	case ICON:
		printf("$" CONFMT, getlval(p) >> 32);
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

	switch (p->n_op) {

	case NAME:
		if (p->n_name[0] != '\0') {
			fputs(p->n_name, io);
			if (getlval(p) != 0)
				fprintf(io, "+" CONFMT, getlval(p));
		} else
			fprintf(io, CONFMT, getlval(p));
		return;

	case OREG:
		r = p->n_rval;
		if (p->n_name[0])
			printf("%s%s", p->n_name, getlval(p) ? "+" : "");
		if (getlval(p))
			fprintf(io, "%d", (int)getlval(p));
		if (R2TEST(r)) {
			fprintf(io, "(%s,%s,4)", rnames[R2UPK1(r)],
			    rnames[R2UPK2(r)]);
		} else
			fprintf(io, "(%s)", rnames[p->n_rval]);
		return;
	case ICON:
#ifdef PCC_DEBUG
		/* Sanitycheck for PIC, to catch adressable constants */
		if (kflag && p->n_name[0] && 0) {
			static int foo;

			if (foo++ == 0) {
				printf("\nfailing...\n");
				fwalk(p, e2print, 0);
				comperr("pass2 conput");
			}
		}
#endif
		/* addressable value of the constant */
		fputc('$', io);
		conput(io, p);
		return;

	case REG:
		switch (p->n_type) {
		case LONGLONG:
		case ULONGLONG:
			fprintf(io, "%%%c%c%c", rnames[p->n_rval][0],
			    rnames[p->n_rval][1], rnames[p->n_rval][2]);
			break;
		case SHORT:
		case USHORT:
			fprintf(io, "%%%s", &rnames[p->n_rval][2]);
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

static char *
ccbranches[] = {
	"je",		/* jumpe */
	"jne",		/* jumpn */
	"jle",		/* jumple */
	"jl",		/* jumpl */
	"sma",		/* jumpge */
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
	printf("	%s\n", ccbranches[o-EQ]);
	printf("	jmp " LABFMT "\n", lab);
}

void
myreader(struct interpass *ipole)
{
	if (x2debug)
		printip(ipole);
}

void
mycanon(NODE *p)
{
}

void
myoptim(struct interpass *ip)
{
}

void
rmove(int s, int d, TWORD t)
{
	comperr("bad rmove: %d %d", s, d);
}

/*
 * For class c, find worst-case displacement of the number of
 * registers in the array r[] indexed by class.
 */
int
COLORMAP(int c, int *r)
{
	return !r[CLASSA];
}

char *rnames[] = {
	"AC", "FAKE1", "FAKE2",
};

/*
 * Return a class suitable for a specific type.
 */
int
gclass(TWORD t)
{
	return CLASSA;
}

/*
 * Calculate argument sizes.
 * Will count # of args before emitting them.
 */
void
lastcall(NODE *p)
{
	NODE *op = p;
	int size = 0;

	if (p->n_op != CALL && p->n_op != FORTCALL && p->n_op != STCALL)
		return;
	p->n_qual = 0;
	for (p = p->n_right; p->n_op == CM; p = p->n_left)
		p->n_right->n_rval = size++;
	p->n_rval = size++;
        op->n_qual = size;
	curargs = size;
}

/*
 * Special shapes.
 */
int
special(NODE *p, int shape)
{
	return SRNOPE;
}

/*
 * Target-dependent command-line options.
 */
void
mflags(char *str)
{
}

/*
 * Do something target-dependent for xasm arguments.
 */
int
myxasm(struct interpass *ip, NODE *p)
{
	return 0;
}

