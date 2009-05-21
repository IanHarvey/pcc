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
	if (sp->sclass == EXTDEF)
		printf("	.globl %s\n", exname(sp->soname));
	if (ISFTN(t))
		printf("\t.type %s,@function\n", exname(sp->soname));
	if (sp->slevel == 0)
		printf("%s:\n", exname(sp->soname));
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
	q->n_rval = EBP;
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
		n->n_rval = EBX;
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

NODE *
funarg(NODE *p, int *n)
{
	NODE *r;

	if (p->n_op == CM) {
		p->n_left = funarg(p->n_left, n);
		p->n_right = funarg(p->n_right, n);
		return p;
	}

	if (*n >= 6) {
		*n++;
		r = block(OREG, NIL, NIL, p->n_type|PTR, 0,
		    MKSUE(p->n_type|PTR));
		r->n_rval = RBP;
		r->n_lval = 16 + (*n - 6) * 8;
	} else {
		r = block(REG, NIL, NIL, p->n_type, 0, 0);
		r->n_lval = 0;
		r->n_rval = argreg(p->n_type, n);
	}
	p = block(ASSIGN, r, p, p->n_type, 0, 0);
	clocal(p);

	return p;
}  

/*
 * Called with a function call with arguments as argument.
 * This is done early in buildtree() and only done once.
 * Returns p.
 */
NODE *
funcode(NODE *p)
{
	extern int gotnr;
	NODE *r, *l;
	int n = 0;

	p->n_right = funarg(p->n_right, &n);

	if (kflag == 0)
		return p;
	/* Create an ASSIGN node for ebx */
	l = block(REG, NIL, NIL, INT, 0, MKSUE(INT));
	l->n_rval = EBX;
	l = buildtree(ASSIGN, l, tempnode(gotnr, INT, 0, MKSUE(INT)));
	if (p->n_right->n_op != CM) {
		p->n_right = block(CM, l, p->n_right, INT, 0, MKSUE(INT));
	} else {
		for (r = p->n_right; r->n_left->n_op == CM; r = r->n_left)
			;
		r->n_left = block(CM, l, r->n_left, INT, 0, MKSUE(INT));
	}
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
