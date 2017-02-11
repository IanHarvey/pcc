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
	int lblnum;
	CONSZ val;
	char *str;
	int printed;
} *copole;

struct ttemp {
	struct ttemp *next;
	int tno;
} *tpole;

static int curargs, prearg, minum, indirs;


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
	struct consts *co;

	if (ipp->ipp_ip.ip_lbl)
		printf("	jmp %s i\n", ipp->ipp_name);
	for (co = copole; co; co = co->next) {
		if (co->printed)
			continue;
		printf("LC%d:	", co->lblnum);
		if (co->str && co->str[0] != '\0') {
			printf("%s", co->str);
			if (co->val)
				printf("+0%llo", co->val);
		} else
			printf("0%llo", co->val);
		printf("\n");
		co->printed = 1;
	}
	for (; tpole; tpole = tpole->next)
		printf("LT%d:	0\n", tpole->tno);
	if (indirs)
		printf("Lindir:	0\n");
}

static int
addicon(int v, char *s)
{
	struct consts *co;

	for (co = copole; co; co = co->next) {
		if (co->val == v && co->str == s)
			return co->lblnum;
	}
	co = permalloc(sizeof(struct consts));
	memset(co, 0, sizeof(struct consts));
	co->val = v & 0777777;
	co->str = s;
	co->lblnum = minum++;
	co->next = copole;
	copole = co;
	return co->lblnum;
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
	NODE *q;
	int i;

	switch (c) {
	case 'A': /* print out label arg address */
		printf("LA%d%d", prearg, curargs - p->n_rval);
		break;

	case 'B': /* put aside constant value for later use */
		printf("LC%d", addicon(getlval(p), p->n_name));
		break;

	case 'C': /* print out space for label args */
		for (i = 0; i < (int)p->n_qual; i++)
			printf("LA%d%d:	0\n", prearg, i+1);
		break;

	case 'D': /* print out NOP if not pointer */
		if (!ISPTR(p->n_type))
			printf("	nop\n");
		break;

	case 'E': /* print a negative number used for comparison */
		printf("LC%d", addicon(-getlval(getlr(p, 'R')) & 0777777, 0));
		break;

	case 'F': /* mask chars with 0777 */
		printf("LC%d", addicon(0777, 0));
		break;

	case 'G': /* store byte to a name (with offset) */
		q = getlr(p, 'L');
		i = addicon(0, q->n_name);
		printf("	dac .+5\n");
		printf("	lac LC%d\n", i);
		printf("	rcl\n");
		printf("	tad LC%d\n", addicon(getlval(q), 0));
		printf("	jms sbyt; ..\n");
		break;

	case 'H': /* memory position to save indirect refs */
		indirs=1;
		printf("Lindir");
		break;

	case 'I': /* Gen code to add constant to memory pos */
		printf("	dac Lindir\n");
		printf("	lac Lindir i\n");
		printf("	tad LC%d\n", addicon(getlval(getlr(p, 'R')) & 0777777, 0));
		printf("	dac Lindir i\n");
		break;

	case 'J': /* add const */
		printf("LC%d", addicon(getlval(getlr(p, 'R')) & 0777777, 0));
		break;

	case 'K':
		if (regno(p->n_right) != AC)
			expand(p, FOREFF, "	lac AR\n");
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
			if (getlval(p) != 0) {
				OFFSZ l = getlval(p);
				if (p->n_type == CHAR || p->n_type == UCHAR)
					l >>= 1;
				fprintf(io, "+" CONFMT, l);
			}
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

	case UMUL:
		if (p->n_left->n_op != REG || regno(p->n_left) == 0)
			comperr("adrput");
		fprintf(io, "%s i", rnames[regno(p->n_left)]);
		break;

	default:
		comperr("illegal address, op %d, node %p", p->n_op, p);
		return;

	}
}

static char *
ccbranches[] = {
	"sna",		/* jumpe */
	"sza",		/* jumpn */
	"ERROR",	/* jumple */
	"spa",		/* jumpl */
	"sma",		/* jumpge */
	"sma sza",	/* jumpg */
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

#if 0
static void
exttemp(NODE *p, void *arg)
{
	struct ttemp *w;

	if (p->n_op != TEMP)
		return;
	for (w = tpole; w; w = w->next) {
		if (w->tno == regno(p))
			break;
	}
	if (w == NULL) {
		w = tmpcalloc(sizeof(struct ttemp));
		w->tno = regno(p);
		w->next = tpole;
		tpole = w;
	}
	p->n_op = NAME;
	p->n_name = tmpcalloc(10);
	sprintf(p->n_name, "LT%d", w->tno);
}
#endif

void
myreader(struct interpass *ipole)
{

#if 0
	struct interpass *ip;
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		walkf(ip->ip_node, exttemp, 0);
	}
#endif
	if (x2debug)
		printip(ipole);
}

void
mycanon(NODE *p)
{
	NODE *q;

	/* Avoid test for result > 0 */
	if (p->n_op == CBRANCH && p->n_left->n_op == LE) {
		/* Swap */
		p = p->n_left;
		q = p->n_left;
		p->n_left = p->n_right;
		p->n_right = q;
		p->n_op = GT;
	}
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
	if (c == CLASSA)
		return r[CLASSA] == 0;
	return r[CLASSB] < 7;
}

char *rnames[] = {
	"AC", "POS1", "POS1", "POS1", "POS1", "POS1", "POS1", "POS1", 
	"FP", "SP",
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
	prearg++;
}

/*
 * Special shapes.
 */
int
special(NODE *p, int shape)
{
	switch (shape) {
	case SLDFPSP:
		return regno(p) == FP || regno(p) == SP;
	}
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

