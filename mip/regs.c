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
 * Register allocation and assignment.
 * Will walk through a tree and assign reasonable registers to
 * each node in it.
 *
 * Basic principle of register assignment:
 *	- If the node has no preferences or registers allocated, 
 *	  do not allocate any registers but instead just recurse
 *	  down and see what we get.
 *	- Follow the evaluation order setup by sucomp().
 */

#include "pass2.h"

static int usedregs, freeregs;
static int regblk[REGSZ];

static int alloregs(NODE *p, int wantreg);
static int getregs(int nreg);
static NODE *movenode(NODE *p, int reg);

/*
 * Create a node that will do reg-reg move.
 */
NODE *
movenode(NODE *p, int reg)
{
	NODE *q = talloc();

	q->n_op = MOVE;
	q->n_rval = q->n_rall = reg;
	q->n_left = p;
	
	return q;
}

/*
 * Get nreg number of (consecutive) regsiters.
 */
int
getregs(int nreg)
{
	int i, j;

	for (i = 0; i < REGSZ-nreg; i++) {
		for (j = 0; j < nreg; j++) {
			if ((rstatus[i+j] & STAREG) == 0)
				break;
			if (regblk[i+j] != 0)
				break;
		}
		if (j != nreg)
			continue;
		for (j = 0; j < nreg; j++)
			regblk[i+j] = 1;
		return i;
	}
	return -1;
}

void
genregs(NODE *p)
{
	int i;

	for (i = 0; i < REGSZ; i++)
		regblk[i] = 0;
	usedregs = 0;
	freeregs = fregs;
	alloregs(p, NOPREF);

}

int
alloregs(NODE *p, int wantreg) 
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	int rall, nreg, rreg;

	/*
	 * Are there any allocation requirements?
	 * If so, registers must be available (is guaranteed by sucomp()).
	 */
	nreg = (q->needs & NACOUNT) * szty(p->n_type);

	if (nreg != 0) {
		/*
		 * Need a bunch of registers.
		 * Get free regs.  This cannot fail due to sucomp().
		 */
		if ((rall = getregs(nreg)) < 0)
			comperr("alloregs: failed alloc %d regs", nreg);

		if (p->n_su & DORIGHT) { /* Right leg first */
			int shused = 0;
			if ((p->n_su & RMASK) == RREG) { /* put in reg */
				if (q->needs & NASR) { /* May share with rght */
					rreg = alloregs(p->n_right, rall);
					if (rreg != rall)
						p->n_right =
						    movenode(p->n_right, rall);
					else
						shused++;
				} else
					rreg = alloregs(p->n_right, NOPREF);
			} else
				(void)alloregs(p->n_right, NOPREF);
			if ((p->n_su & LMASK) == LREG) {
				if ((q->needs & NASL) && (shused == 0)) {
					rreg = alloregs(p->n_left, rall);
					if (rreg != rall)
						p->n_left =
						    movenode(p->n_left, rall);
				} else
					rreg = alloregs(p->n_left, NOPREF);
			}
		} else {
			int shused = 0;
			if ((p->n_su & LMASK) == LREG) {
				if ((q->needs & NASL) && (shused == 0)) {
					rreg = alloregs(p->n_left, rall);
					if (rreg != rall)
						p->n_left =
						    movenode(p->n_left, rall);
					else
						shused++;
				} else
					rreg = alloregs(p->n_left, NOPREF);
			}
			if ((p->n_su & RMASK) == RREG) { /* put in reg */
				if ((q->needs & NASR) && (shused == 0)) {
					rreg = alloregs(p->n_right, rall);
					if (rreg != rall)
						p->n_right =
						    movenode(p->n_right, rall);
				} else
					rreg = alloregs(p->n_right, NOPREF);
			} else
				(void)alloregs(p->n_right, NOPREF);
		}
		p->n_rall = nreg;
		return nreg;
	} else
		comperr("alloregs");
	return 0; /* XXX */
}
