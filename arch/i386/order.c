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

int canaddr(NODE *);

/*
 * should the assignment op p be stored,
 * given that it lies as the right operand of o
 * (or the left, if o==UNARY MUL)
 */
void
stoasg(NODE *p, int o)
{
	if (x2debug)
		printf("stoasg(%p, %o)\n", p, o);
}

/* should we delay the INCR or DECR operation p */
int
deltest(NODE *p)
{
	return 0;
}

/*
 * Check if p can be autoincremented.
 * XXX - nothing can be autoincremented for now.
 */
int
autoincr(NODE *p)
{
	return 0;
}

/* is it legal to make an OREG or NAME entry which has an
 * offset of off, (from a register of r), if the
 * resulting thing had type t */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	return(0);  /* YES */
}

/*
 * Turn a UMUL-referenced node into OREG.
 */
int
offstar(NODE *p)
{
	if (x2debug)
		printf("offstar(%p)\n", p);

	if( p->n_op == PLUS || p->n_op == MINUS ){
		if( p->n_right->n_op == ICON ){
			geninsn(p->n_left, INTAREG|INAREG);
			p->n_su = -1;
			return 1;
		}
	}
	geninsn(p, INTAREG|INAREG);
	return 0;
}

/*
 * Shape matches for UMUL.  Cooperates with offstar().
 */
int
shumul(NODE *p)
{

	if (x2debug)
		printf("shumul(%p)\n", p);

	/* Always turn it into OREG on x86 */
	return SOREG;
}

/*
 * Rewrite increment/decrement operation.
 */
int
setincr(NODE *p)
{
	if (x2debug)
		printf("setincr(%p)\n", p);

	return(0);
}

/*
 * Rewrite operations on binary operators (like +, -, etc...).
 * Called as a result of table lookup.
 */
int
setbin(NODE *p)
{

	if (x2debug)
		printf("setbin(%p)\n", p);
	return 0;

}

/* setup for assignment operator */
int
setasg(NODE *p, int cookie)
{
	if (x2debug)
		printf("setasg(%p)\n", p);
	return(0);
}

/* setup for unary operator */
int
setuni(NODE *p, int cookie)
{
	return 0;
}

/* register allocation */
regcode
regalloc(NODE *p, struct optab *q, int wantreg)
{
	regcode regc;

	if (q->op == SCONV && q->rtype == TLONGLONG) {
		/*
		 * cltd instruction; input in eax, out in eax+edx.
		 */
		if (regblk[EAX] & 1 || regblk[EDX] & 1)
			comperr("regalloc: needed regs inuse");
		regc = alloregs(p->n_left, EAX);
		if (REGNUM(regc) != EAX) {
			p->n_left = movenode(p->n_left, EAX);
			freeregs(regc);
		}
		MKREGC(regc, EAX, 2);
		regblk[EAX] |= 1;
		regblk[EDX] |= 1;
	} else if (q->op == DIV || q->op == MOD) {
		/*
		 * Single-precision div/mul.
		 * XXX - incorrect traverse order.
		 */
		if (regblk[EAX] & 1 || regblk[EDX] & 1)
			comperr("regalloc: needed regs inuse");
		regc = alloregs(p->n_left, EAX);
		if (REGNUM(regc) != EAX) {
			p->n_left = movenode(p->n_left, EAX);
			freeregs(regc);
			regblk[EAX] |= 1;
		}
		if ((p->n_su & RMASK) == RREG) {
			regc = alloregs(p->n_right, ECX);
			if (REGNUM(regc) != ECX)
				p->n_right = movenode(p->n_right, ECX);
			freeregs(regc);
		}
		
		if (q->op == DIV) {
			MKREGC(regc, EAX, 1);
		} else {
			MKREGC(regc, EDX, 1);
			regblk[EAX] &= ~1;
			regblk[EDX] |= 1;
		}
	} else
		comperr("regalloc: bad optab");
	p->n_rall = REGNUM(regc);
	return regc;
}
