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

#include <string.h>

int canaddr(NODE *);

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
 * Be careful about register classes, this is a place where classes change.
 * return 1 if oreg, 0 otherwise.
 */
int
offstar(NODE *p, int shape)
{
	if (x2debug)
		printf("offstar(%p)\n", p);

	if (p->n_op == REG || p->n_op == TEMP)
		return 1; /* Is already oreg */
	if( p->n_op == PLUS || p->n_op == MINUS ){
		if( p->n_right->n_op == ICON ){
			p->n_su = DOWNL;
			(void)geninsn(p->n_left, INAREG);
			return 1;
		}
	}
	(void)geninsn(p, INAREG);
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

/*
 * Special handling of some instruction register allocation.
 * - left is the register that left node wants.
 * - right is the register that right node wants.
 * - res is in which register the result will end up.
 * - mask is registers that will be clobbered.
 */
struct rspecial *
nspecial(struct optab *q)
{
	switch (q->op) {
	case STASG:
	case STARG:
		{
			static struct rspecial s[] = {
				{ NEVER, EAX }, { NEVER, EDX },
				{ NEVER, ECX }, { 0 } };
			return s;
		}

	case SCONV:
		if ((q->ltype & (TINT|TUNSIGNED|TSHORT|TUSHORT)) && 
		    q->rtype == (TCHAR|TUCHAR)) {
			static struct rspecial s[] = { 
				{ NOLEFT, ESI }, { NOLEFT, EDI }, { 0 } };
			return s;
		} else if ((q->ltype & (TINT|TUNSIGNED)) &&
		    q->rtype == TLONGLONG) {
			static struct rspecial s[] = {
				{ NLEFT, EAX }, { NRES, EAXEDX },
				{ NEVER, EAX }, { NEVER, EDX }, { 0 } };
			return s;
		} else if (q->ltype == TSHORT &&
		    q->rtype == (TLONGLONG|TULONGLONG)) {
			static struct rspecial s[] = {
				{ NLEFT, EAX }, { NRES, EAXEDX },
				{ NEVER, EAX }, { NEVER, EDX }, { 0 } };
			return s;
		} else if (q->ltype == TCHAR &&
		    q->rtype == (TLONGLONG|TULONGLONG)) {
			static struct rspecial s[] = {
				{ NRES, EAXEDX },
				{ NEVER, EAX }, { NEVER, EDX }, { 0 } };
			return s;
		}
		break;
	case DIV:
		if (q->lshape == SBREG) {
			static struct rspecial s[] = {
				{ NEVER, AL }, { NEVER, AH },
				{ NLEFT, AL }, { NRES, AL },
				{ NORIGHT, AH }, { NORIGHT, AL }, { 0 } };
				return s;
		} else if (q->lshape == SAREG) {
			static struct rspecial s[] = {
				{ NEVER, EAX }, { NEVER, EDX },
				{ NLEFT, EAX }, { NRES, EAX },
				{ NORIGHT, EDX }, { NORIGHT, EAX }, { 0 } };
			return s;
		} else if (q->lshape & SCREG) {
			static struct rspecial s[] = {
				{ NEVER, EAX }, { NEVER, EDX },
				{ NEVER, ECX }, { NRES, EAXEDX }, { 0 } };
			return s;
		}
		break;
	case MOD:
		if (q->lshape == SBREG) {
			static struct rspecial s[] = {
				{ NEVER, AL }, { NEVER, AH },
				{ NLEFT, AL }, { NRES, AH },
				{ NORIGHT, AH }, { NORIGHT, AL }, { 0 } };
			return s;
		} else if (q->lshape == SAREG) {
			static struct rspecial s[] = {
				{ NEVER, EAX }, { NEVER, EDX },
				{ NLEFT, EAX }, { NRES, EDX },
				{ NORIGHT, EDX }, { NORIGHT, EAX }, { 0 } };
			return s;
		} else if (q->lshape & SCREG) {
			static struct rspecial s[] = {
				{ NEVER, EAX }, { NEVER, EDX },
				{ NEVER, ECX }, { NRES, EAXEDX }, { 0 } };
			return s;
		}
		break;
	case MUL:
		if (q->lshape == SBREG) {
			static struct rspecial s[] = {
				{ NEVER, AL }, { NEVER, AH },
				{ NLEFT, AL }, { NRES, AL }, { 0 } };
			return s;
		} else if (q->lshape & SCREG) {
			static struct rspecial s[] = {
				{ NEVER, EAX }, { NEVER, EDX },
				{ NEVER, ECX }, { NRES, EAXEDX }, { 0 } };
			return s;
		}
		break;
	case LS:
	case RS:
		if (q->visit & (INAREG|INBREG)) {
			static struct rspecial s[] = {
				{ NRIGHT, ECX }, { 0 } };
			return s;
		} else if (q->visit & INCREG) {
			static struct rspecial s[] = {
				{ NEVER, EAX }, { NEVER, EDX },
				{ NEVER, ECX }, { NRES, EAXEDX }, { 0 } };
			return s;
		}
		break;

	default:
		break;
	}
	comperr("nspecial entry %d", q - table);
	return 0; /* XXX gcc */
}
