/*	$Id$	*/
/*
 * Copyright (c) 2008 Michael Shalayeff
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


# include "pass1.h"

NODE *funarg(NODE *, int *);
int argreg(TWORD, int *);

int lastloc = -1;

/*
 * Define everything needed to print out some data (or text).
 * This means segment, alignment, visibility, etc.
 */
void
defloc(struct symtab *sp)
{
	extern char *nextsect;
	static char *loctbl[] = { "text", "data", "section .rodata" };
	int weak = 0;
	char *name = NULL;
	TWORD t;
	int s;

	if (sp == NULL) {
		lastloc = -1;
		return;
	}
	t = sp->stype;
	s = ISFTN(t) ? PROG : ISCON(cqual(t, sp->squal)) ? RDATA : DATA;
#ifdef TLS
	if (sp->sflags & STLS) {
		if (s != DATA)
			cerror("non-data symbol in tls section");
		nextsect = ".tdata";
	}
#endif
#ifdef GCC_COMPAT
	{
		struct gcc_attrib *ga;

		if ((ga = gcc_get_attr(sp->ssue, GCC_ATYP_SECTION)) != NULL)
			nextsect = ga->a1.sarg;
		if ((ga = gcc_get_attr(sp->ssue, GCC_ATYP_WEAK)) != NULL)
			weak = 1;
	}
#endif

	if (nextsect) {
		printf("	.section %s\n", nextsect);
		nextsect = NULL;
		s = -1;
	} else if (s != lastloc)
		printf("	.%s\n", loctbl[s]);
	lastloc = s;
	while (ISARY(t))
		t = DECREF(t);
	s = ISFTN(t) ? ALINT : talign(t, sp->ssue);
	if (s > ALCHAR)
		printf("	.align %d\n", s/ALCHAR);
	if (weak || sp->sclass == EXTDEF || sp->slevel == 0 || ISFTN(t))
		if ((name = sp->soname) == NULL)
			name = exname(sp->sname);
	if (weak)
		printf("        .weak %s\n", name);
	else if (sp->sclass == EXTDEF)
		printf("	.globl %s\n", name);
	if (ISFTN(t))
		printf("\t.type %s,@function\n", name);
	if (sp->slevel == 0)
		printf("%s:\n", name);
	else
		printf(LABFMT ":\n", sp->soffset);
}

/*
 * code for the end of a function
 * deals with struct return here
 */
void
efcode()
{
	extern int gotnr;
	NODE *p, *q;

	gotnr = 0;	/* new number for next fun */
	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN)
		return;
	/* Create struct assignment */
	q = block(OREG, NIL, NIL, PTR+STRTY, 0, cftnsp->ssue);
	q->n_rval = RBP;
	q->n_lval = 8; /* return buffer offset */
	q = buildtree(UMUL, q, NIL);
	p = block(REG, NIL, NIL, PTR+STRTY, 0, cftnsp->ssue);
	p = buildtree(UMUL, p, NIL);
	p = buildtree(ASSIGN, q, p);
	ecomp(p);
}

/*
 * code for the beginning of a function; a is an array of
 * indices in symtab for the arguments; n is the number
 */
void
bfcode(struct symtab **sp, int cnt)
{
	extern int gotnr;
	NODE *n, *p, *q;
	int i, k;

	if (cftnsp->stype == STRTY+FTN || cftnsp->stype == UNIONTY+FTN) {
		/* Function returns struct, adjust arg offset */
		for (i = 0; i < cnt; i++) 
			sp[i]->soffset += SZPOINT(LONG);
	}

	if (kflag) {
		/* Put ebx in temporary */
		n = block(REG, NIL, NIL, INT, 0, MKSUE(INT));
		n->n_rval = RBX;
		p = tempnode(0, INT, 0, MKSUE(INT));
		gotnr = regno(p);
		ecomp(buildtree(ASSIGN, p, n));
	}

	/* recalculate the arg offset and create TEMP moves */
	for (k = 0, i = 0; i < cnt; i++) {

		if (sp[i] == NULL)
			continue;

		if (k < 6) {
			p = tempnode(0, sp[i]->stype, sp[i]->sdf, sp[i]->ssue);
			q = block(REG, NIL, NIL, sp[i]->stype, sp[i]->sdf, sp[i]->ssue);
			q->n_rval = argreg(sp[i]->stype, &k);
			p = buildtree(ASSIGN, p, q);
			sp[i]->soffset = regno(p->n_left);
			sp[i]->sflags |= STNODE;
			ecomp(p);
		} else {
			sp[i]->soffset += SZLONG * k;
			if (xtemps) {
				/* put stack args in temps if optimizing */
				p = tempnode(0, sp[i]->stype, sp[i]->sdf, sp[i]->ssue);
				p = buildtree(ASSIGN, p, buildtree(NAME, 0, 0));
				sp[i]->soffset = regno(p->n_left);
				sp[i]->sflags |= STNODE;
				ecomp(p);
			}
		}
	}
}


/*
 * by now, the automatics and register variables are allocated
 */
void
bccode()
{
	SETOFF(autooff, SZINT);
}

/* called just before final exit */
/* flag is 1 if errors, 0 if none */
void
ejobcode(int flag )
{
#define _MKSTR(x) #x
#define MKSTR(x) _MKSTR(x)
#define OS MKSTR(TARGOS)
        printf("\t.ident \"PCC: %s (%s)\"\n\t.end\n", PACKAGE_STRING, OS);
}

void
bjobcode()
{
}

static const int argregsi[] = { RDI, RSI, RDX, RCX, R09, R08 };

int
argreg(TWORD t, int *n)   
{
	switch (t) {
	case FLOAT:
	case DOUBLE:
	case LDOUBLE:
		/* return FR6 - *n - 2; */
		cerror("argreg");
		return 0;
	case LONGLONG:
	case ULONGLONG:
		/* TODO */;
	default:
		return argregsi[(*n)++];
	}
}

static NODE *
movtoreg(NODE *p, int rno)
{
	NODE *r;

	r = block(REG, NIL, NIL, p->n_type, 0, 0);
	regno(r) = rno;
	return clocal(buildtree(ASSIGN, r, p));
}  

static int nsse, ngpr;
enum { INTEGER = 1, INTMEM, SSE, SSEMEM, X87, STRREG, STRMEM };

/*
 * AMD64 parameter classification.
 */
static int
argtyp(NODE *p)
{
	TWORD t = p->n_type;
	int cl = 0;

	if (t <= ULONG) {
		cl = ngpr < 6 ? INTEGER : INTMEM;
	} else if (t == FLOAT || t == DOUBLE) {
		cl = nsse < 4 ? SSE : SSEMEM;
	} else if (t == LDOUBLE) {
		cl = X87; /* XXX */
	} else if (t == STRTY) {
		if (tsize(t, p->n_df, p->n_sue) > 4*SZLONG)
			cl = STRMEM;
		else
			cerror("clasif");
	} else
		cerror("FIXME: classify");
	return cl;
}

static void
argput(NODE *p)
{
	NODE *q;
	int typ, r;

	/* first arg may be struct return pointer */
	/* XXX - check if varargs; setup al */
	switch (typ = argtyp(p)) {
	case INTEGER:
	case SSE:
		q = talloc();
		*q = *p;
		if (typ == SSE)
			r = XMM0 + nsse++;
		else
			r = argregsi[ngpr++];
		q = movtoreg(q, r);
		*p = *q;
		nfree(q);
		break;
	case X87:
		cerror("no long double yet");
		break;
	case STRMEM:
		/* Struct moved to memory */
	case STRREG:
		/* Struct in registers */
	default:
		cerror("struct argument");
	}
}


/*
 * Called with a function call with arguments as argument.
 * This is done early in buildtree() and only done once.
 * Returns p.
 */
NODE *
funcode(NODE *p)
{

	nsse = ngpr = 0;
	listf(p->n_right, argput);
	return p;
}

/*
 * return the alignment of field of type t
 */
int
fldal(unsigned int t)
{
	uerror("illegal field type");
	return(ALINT);
}

/* fix up type of field p */
void
fldty(struct symtab *p)
{
}

/*
 * XXX - fix genswitch.
 */
int
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	return 0;
}
