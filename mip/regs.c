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

static int usedregs;
static int regblk[REGSZ];

static int alloregs(NODE *p, int wantreg);
static int getregs(int nreg);
static NODE *movenode(NODE *p, int reg);

int finduni(NODE *p, int); /* XXX - used by movenode */

/*
 * Build a matrix to deal with different requirements of allocations.
 */
#define	R_DOR	000001
#define	R_RREG	000002
#define	R_LREG	000004
#define	R_NASL	000010
#define	R_NASR	000020
#define	R_PREF	000040
#define	R_RRGHT	000100
#define	R_RLEFT	000200
#define	R_RESC	000400

/*
 * Print the codeword in a readable format.
 */
static void
prtcword(int cword)
{
	static char *names[] = { "DORIGHT", "RREG", "LREG", "NASL", "NASR",
	    "NEEDREG", "RRIGHT", "RLEFT", "RESCx" };
	int i;

	for (i = 0; i < 9; i++)
		if (cword & (1 << i))
			printf("%s,", names[i]);
}

/*
 * Create a node that will do reg-reg move.
 */
NODE *
movenode(NODE *p, int reg)
{
	NODE *q = talloc();

	q->n_op = MOVE;
	q->n_type = p->n_type;
	q->n_rval = q->n_rall = reg;
	q->n_left = p;
	if ((q->n_su = finduni(q, 0)) == -1)
		cerror("movenode failed");
	q->n_su |= LREG;
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
	comperr("getregs: can't alloc %d regs", nreg);
	return 0; /* XXX */
}

/*
 * Free previously allocated registers.
 */
static void
freeregs(int reg, int cnt)
{
	int i;

	for (i = reg; i < reg+cnt; i++) {
		if ((regblk[i] & 1) == 0)
			comperr("freeregs: freeing free reg %d", i);
		regblk[i] &= ~1;
	}
}

void
genregs(NODE *p)
{
	int i;

	for (i = 0; i < REGSZ; i++)
		regblk[i] = 0;
	usedregs = 0;
	alloregs(p, NOPREF);

}

#if 0
int
asgregs(NODE *p, int wantreg)
{
	if (p->n_su & DORIGHT) {
		r = p->n_right;
		l = (p->n_su & LMASK) == LREG ? p->n_left : NULL;
	} else {
		r = (p->n_su & LMASK) == LREG ? p->n_left : NULL;
		l = (p->n_su & RMASK) == RREG ? p->n_right : NULL;
	}
}
#endif

/*
 * Check if nreg registers at position wreg are unused.
 */
static int
isfree(int wreg, int nreg)
{
	int i;

	if (wreg < 0)
		return 0;
	for (i = wreg; i < (wreg+nreg); i++)
		if ((rstatus[i] & STAREG) == 0 || (regblk[i] & 1) == 1)
			return 0;
	return 1; /* Free! */
}

/*
 * Find nreg free regs somewhere.
 */
static int 
findfree(int nreg)
{
	int i;

	for (i = 0; i < REGSZ-nreg; i++)
		if (isfree(i, nreg))
			return i;
	return -1;
}

/*
 * See if a wanted reg can be shared with regs alloced, taken in account that 
 * return regs may not be the expected.
 */
static int
canshare(NODE *p, struct optab *q, int wantreg)
{
	int nreg = (q->needs & NACOUNT);
	int sz = szty(p->n_type);

	if (nreg == 1)
		return wantreg; /* Fine! */
	if (q->rewrite & RESC1) {
		if (isfree(wantreg+sz, sz*(nreg-1)))
			return wantreg;
	} else if (q->rewrite & RESC2) {
		if (isfree(wantreg-sz, sz) &&
		    (nreg > 2 ? isfree(wantreg+sz, sz) : 1))
			return wantreg;
	} else /* if (q->rewrite & RESC3) */ {
		if (isfree(wantreg-sz*2, sz*2))
			return wantreg;
	}
	return getregs(nreg*sz);
}


int
alloregs(NODE *p, int wantreg) 
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	int nreg, sreg, size, rreg, rreg2, rreg3;
	int cword = 0;

	if (p->n_su == -1) /* For OREGs and similar */
		return alloregs(p->n_left, wantreg);
	/*
	 * Are there any allocation requirements?
	 * If so, registers must be available (is guaranteed by sucomp()).
	 */
	if (q->needs & NACOUNT) {
		nreg = q->needs & NACOUNT;
		size = szty(p->n_type);
		sreg = nreg * size;
		cword = R_PREF;
	}

	if (p->n_su & RREG)
		cword += R_RREG;
	if (p->n_su & LREG)
		cword += R_LREG;
	if (q->needs & NASL)
		cword += R_NASL;
	if (q->needs & NASR)
		cword += R_NASR;
	if (p->n_su & DORIGHT)
		cword += R_DOR;
	if (q->rewrite & RLEFT)
		cword += R_RLEFT;
	if (q->rewrite & RRIGHT)
		cword += R_RRGHT;
	if (q->rewrite & (RESC1|RESC2|RESC3))
		cword += R_RESC;

#ifdef PCC_DEBUG
	if (rdebug) {
		printf("%p) cword ", p);
		prtcword(cword);
		putchar('\n');
	}
#endif

	switch (cword) {
	case 0: /* No registers, ignore */
		return 0;

	case R_DOR+R_RRGHT+R_RREG: /* Typical for ASSIGN node */
	case R_RRGHT+R_RREG: /* Typical for ASSIGN node */
		rreg = alloregs(p->n_right, wantreg);
		break;

	case R_PREF+R_RESC: /* Leaf node that puts a value into a register */
	case R_PREF+R_RESC+R_NASR:
		if (wantreg != NOPREF)
			rreg = canshare(p, q, wantreg);
		else
			rreg = getregs(sreg);
		break;

	case R_RLEFT+R_LREG: /* Operate on left leg */
		rreg = alloregs(p->n_left, wantreg);
		break;

	case R_LREG+R_RREG+R_RRGHT: /* binop, reclaim right */
		(void)alloregs(p->n_left, NOPREF);
		rreg = alloregs(p->n_right, wantreg);
		freeregs(p->n_left->n_rall, szty(p->n_left->n_type));
		break;

	case R_DOR+R_RREG+R_LREG+R_NASL+R_PREF+R_RESC:
		/* l+r in reg, need regs, reclaim alloced regs, may share l */

		/* Traverse right first, it may not be shared */
		rreg3 = alloregs(p->n_right, NOPREF);
		if ((rreg = findfree(sreg)) < 0) {
			/* No regs found, must move around */
			rreg = getregs(szty(p->n_right->n_type));
			p->n_right = movenode(p->n_right, rreg);
			freeregs(rreg3, szty(p->n_type));
		}

		/* Now regs should exist */
		if (wantreg != NOPREF)
			rreg = canshare(p, q, wantreg);
		else
			rreg = getregs(sreg);
		/* Trick: Only keep given regs allocated */
		if (nreg > 1)
			freeregs(rreg+size, (nreg-1)*size);
		rreg2 = alloregs(p->n_left, rreg);
		if (rreg2 != rreg) { /* Could nog use same */
			/* XXX - rework later to ev. avoid movenode */
			p->n_left = movenode(p->n_left, rreg);
			freeregs(rreg2, szty(p->n_type));
		}
		break;

	case R_DOR+R_RREG+R_LREG+R_RLEFT:
		/* l+r in reg, reclaim left */
		(void)alloregs(p->n_right, NOPREF);
		rreg = alloregs(p->n_left, wantreg);
		freeregs(p->n_right->n_rall, szty(p->n_right->n_type));
		break;

	case R_RESC+R_NASL+R_PREF+R_RREG:
		/* Alloc regs, reclaim regs, put right in reg, may share left */



	default:
#ifdef PCC_DEBUG
		printf("%p) cword ", p);
		prtcword(cword);
		putchar('\n');
#endif
		comperr("alloregs");
	}
#if 0
	if (nreg != 0) {
		/*
		 * Need a bunch of registers.
		 * Get free regs.  This cannot fail due to sucomp(),
		 * if it returns -1 then the caller must shuffle();
		 */
		if ((rall = getregs(nreg)) < 0)
			return -1;

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
			}
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
			}
		}
		p->n_rall = rall;
		return rall;
	} else {
		if ((p->n_su & RMASK) != RREG && 
		    (p->n_su & LMASK) != LREG)
			return 0; /* Nothing to do */
		if (p->n_su & DORIGHT) {
			r = p->n_right;
			l = (p->n_su & LMASK) == LREG ? p->n_left : NULL;
		} else {
			r = (p->n_su & LMASK) == LREG ? p->n_left : NULL;
			l = (p->n_su & RMASK) == RREG ? p->n_right : NULL;
		}
		rreg = asgregs(r, l, wantreg);


		if (p->n_su & DORIGHT) { /* Right leg first */
			rreg = setregs(p->n_su, p->n_right, 
			int shused = 0;
			if ((p->n_su & RMASK) == RREG) { /* put in reg */
				if (q->needs & NASR) { /* May share with rght */
					rreg = alloregs(p->n_right, wantreg);
					if (wantreg != NOPREF &&rreg != wantreg)
						p->n_right =
						    movenode(p->n_right, wantreg);
					else
						shused++;
				} else
					rreg = alloregs(p->n_right, NOPREF);
			}
			if ((p->n_su & LMASK) == LREG) {
				if ((q->needs & NASL) && (shused == 0)) {
					rreg = alloregs(p->n_left, wantreg);
					if (rreg == -1)
						rreg = shuffle(p->n_right,
						    p->n_left, wantreg);
					if (wantreg != NOPREF &&rreg != wantreg)
						p->n_left =
						    movenode(p->n_left, wantreg);
				} else
					rreg = alloregs(p->n_left, NOPREF);
			}
		} else {
			int shused = 0;
			if ((p->n_su & LMASK) == LREG) {
				if ((q->needs & NASL) && (shused == 0)) {
					rreg = alloregs(p->n_left, wantreg);
					if (wantreg != NOPREF &&rreg != wantreg)
						p->n_left =
						    movenode(p->n_left, wantreg);
					else
						shused++;
				} else
					rreg = alloregs(p->n_left, NOPREF);
			}
			if ((p->n_su & RMASK) == RREG) { /* put in reg */
				if ((q->needs & NASR) && (shused == 0)) {
					rreg = alloregs(p->n_right, wantreg);
					if (wantreg != NOPREF &&rreg != wantreg)
						p->n_right =
						    movenode(p->n_right, wantreg);
				} else
					rreg = alloregs(p->n_right, NOPREF);
			}
		}
		p->n_rall = rreg;
		return rreg;
	}
#endif
	p->n_rall = rreg;
	return rreg;
}
