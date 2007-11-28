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


/*
 * MIPS port by Jan Enoksson (janeno-1@student.ltu.se) and
 * Simon Olsson (simols-1@student.ltu.se) 2005.
 */

#include <assert.h>
#include "pass1.h"

/*
 * cause the alignment to become a multiple of n
 * never called for text segment.
 */
void
defalign(int n)
{
	n /= SZCHAR;
	if (n == 1)
		return;
	printf("\t.align %d\n", n);
}

/*
 * define the current location as the name p->sname
 * never called for text segment.
 */
void
defnam(struct symtab *p)
{
	char *c = p->sname;

#ifdef GCC_COMPAT
	c = gcc_findname(p);
#endif
	if (p->sclass == EXTDEF)
		printf("\t.globl %s\n", c);
#ifdef USE_GAS
	printf("\t.type %s,@object\n", c);
	printf("\t.size %s," CONFMT "\n", c, tsize(p->stype, p->sdf, p->ssue));
#endif
	printf("%s:\n", c);
}

static int rvnr;

/*
 * code for the end of a function
 * deals with struct return here
 */
void
efcode()
{
	NODE *p, *q;
	int tempnr;
	int ty;

	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN)
		return;

	ty = cftnsp->stype - FTN;

	q = block(REG, NIL, NIL, INCREF(ty), 0, cftnsp->ssue);
	q->n_rval = V0;
	p = tempnode(0, INCREF(ty), 0, cftnsp->ssue);
	tempnr = p->n_lval;
	p = buildtree(ASSIGN, p, q);
	ecomp(p);

	q = tempnode(tempnr, INCREF(ty), 0, cftnsp->ssue);
	q = buildtree(UMUL, q, NIL);

	p = tempnode(rvnr, INCREF(ty), 0, cftnsp->ssue);
	p = buildtree(UMUL, p, NIL);

	p = buildtree(ASSIGN, p, q);
	ecomp(p);

	q = tempnode(rvnr, INCREF(ty), 0, cftnsp->ssue);
	p = block(REG, NIL, NIL, INCREF(ty), 0, cftnsp->ssue);
	p->n_rval = V0;
	p = buildtree(ASSIGN, p, q);
	ecomp(p);
}

/*
 * code for the beginning of a function; a is an array of
 * indices in symtab for the arguments; n is the number
 */
void
bfcode(struct symtab **sp, int cnt)
{
	NODE *p, *q;
	int i, n, start = 0;
	int sz, tsz;

	/* assign hidden return structure to temporary */
	if (cftnsp->stype == STRTY+FTN || cftnsp->stype == UNIONTY+FTN) {
		p = tempnode(0, PTR+STRTY, 0, cftnsp->ssue);
		rvnr = p->n_lval;
		q = block(REG, NIL, NIL, PTR+STRTY, 0, cftnsp->ssue);
		q->n_rval = A0 + start++;
		p = buildtree(ASSIGN, p, q);
		ecomp(p);
	}

        /* recalculate the arg offset and create TEMP moves */
        for (n = start, i = 0; i < cnt; i++) {

		sz = tsize(sp[i]->stype, sp[i]->sdf, sp[i]->ssue) / SZINT;

		/* A structure argument */
		if (n < nargregs &&
		    (sp[i]->stype == STRTY || sp[i]->stype == UNIONTY)) {
			tsz = sz > nargregs - n ? nargregs - n : sz;
			spname = sp[i];
			for (i = 0; i < tsz; i++) {
				q = block(REG, NIL, NIL, INT, 0, MKSUE(INT));
				q->n_rval = A0 + n + i;
				p = block(REG, NIL, NIL, INT, 0, MKSUE(INT));
				p->n_rval = FP;
				p = block(PLUS, p, bcon(ARGINIT/SZCHAR+(n+i)*4),
				    INT, 0, MKSUE(INT));
				p = block(UMUL, p, NIL, INT, 0, MKSUE(INT));
				p = buildtree(ASSIGN, p, q);
				ecomp(p);
			}
		} else if (n + sz <= nargregs) {
			if (xtemps) {
	                        p = tempnode(0, sp[i]->stype,
				    sp[i]->sdf, sp[i]->ssue);
	                        spname = sp[i];
	                        q = block(REG, NIL, NIL,
	                            sp[i]->stype, sp[i]->sdf, sp[i]->ssue);
	                        q->n_rval = sz == 2 ? A0A1 + n : A0 + n;
	                        p = buildtree(ASSIGN, p, q);
	                        sp[i]->soffset = p->n_left->n_lval;
	                        sp[i]->sflags |= STNODE;
			} else {
				// sp[i]->soffset += ARGINIT;
				spname = sp[i];
				q = block(REG, NIL, NIL,
				    sp[i]->stype, sp[i]->sdf, sp[i]->ssue);
				q->n_rval = sz == 2 ? A0A1 + n : A0 + n;
                                p = buildtree(ASSIGN, buildtree(NAME, 0, 0), q);
			}
                        ecomp(p);
                } else {
                        // sp[i]->soffset += ARGINIT;
                        if (xtemps) {
                                /* put stack args in temps if optimizing */
                                spname = sp[i];
                                p = tempnode(0, sp[i]->stype,
                                    sp[i]->sdf, sp[i]->ssue);
                                p = buildtree(ASSIGN, p, buildtree(NAME, 0, 0));
                                sp[i]->soffset = p->n_left->n_lval;
                                sp[i]->sflags |= STNODE;
                                ecomp(p);
                        }
                
                }
                n += sz;
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
}

void
bjobcode()
{
	printf("\t.section .mdebug.abi32\n");
	printf("\t.previous\n");
	printf("\t.abicalls\n");
}

/*
 * Print character t at position i in one string, until t == -1.
 * Locctr & label is already defined.
 */
void
bycode(int t, int i)
{
	static int lastoctal = 0;

	/* put byte i+1 in a string */

	if (t < 0) {
		if (i != 0)
			puts("\"");
	} else {
		if (i == 0)
			printf("\t.asciiz \"");
		if (t == 0)
			return;
		else if (t == '\\' || t == '"') {
			lastoctal = 0;
			putchar('\\');
			putchar(t);
		} else if (t == 012) {
			printf("\\n");
		} else if (t < 040 || t >= 0177) {
			lastoctal++;
			printf("\\%o",t);
		} else if (lastoctal && '0' <= t && t <= '9') {
			lastoctal = 0;
			printf("\"\n\t.asciiz \"%c", t);
		} else {	
			lastoctal = 0;
			putchar(t);
		}
	}
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

static void
moveargs(NODE **p, int *regp)
{
        NODE *r = *p;
        NODE *t, *q;
        int sz, tsz, n;
        int regnum;

        if (r->n_op == CM) {
                moveargs(&r->n_left, regp);
                p = &r->n_right;
                r = r->n_right;
        }

        regnum = *regp;
	sz = tsize(r->n_type, r->n_df, r->n_sue) / SZINT;

	if (regnum <= A0 + nargregs && r->n_type == STRTY) {
		/* copy structure into registers */
		n = regnum - A0;
		tsz = sz > nargregs - n ? nargregs - n : sz;
		printf("[should copy %d words into registers]\n", tsz);

		while (tsz > 0) {
			q = block(REG, NIL, NIL, INT, 0, MKSUE(INT));
			q->n_rval = regnum + tsz;
                	q = buildtree(ASSIGN, q, r);
			r = block(CM, q, NIL, INT, 0, MKSUE(INT));
			tsz--;
		}
		t = r;

	} else if (regnum + sz <= A0 + nargregs) {
                t = block(REG, NIL, NIL, r->n_type, r->n_df, r->n_sue);
		switch(r->n_type) {
		case DOUBLE:
		case LDOUBLE:
			t->n_rval = regnum + F0;
			break;
		case LONGLONG:
		case ULONGLONG:
			t->n_rval = regnum + A0A1;
			break;
		default:
	                t->n_rval = regnum;
		}
                t = buildtree(ASSIGN, t, r);
        } else {
		if (r->n_op == STARG)
			t = r;
		else
			t = block(FUNARG, r, NIL, r->n_type, r->n_df, r->n_sue);
	}

	*p = t;
	*regp += sz;
}

/*
 * Called with a function call with arguments as argument.
 * This is done early in buildtree() and only done once.
 */
NODE *
funcode(NODE *p)
{
	int regnum = A0;
	NODE *l, *r, *t, *q;
	int ty;

	l = p->n_left;
	r = p->n_right;

	ty = DECREF(l->n_type);
	if (ty == STRTY+FTN || ty == UNIONTY+FTN) {
		ty = DECREF(l->n_type) - FTN;
		q = tempnode(0, ty, l->n_df, l->n_sue);
		q = buildtree(ADDROF, q, NIL);
		if (r->n_op != CM) {
			p->n_right = block(CM, q, r, INCREF(ty),
			    l->n_df, l->n_sue);
		} else {
			for (t = r; t->n_left->n_op == CM; t = t->n_left)
				;
			t->n_left = block(CM, q, t->n_left, INCREF(ty),
			    l->n_df, l->n_sue);
		}
	}

	moveargs(&p->n_right, &regnum);
	return p;
}
