/*	$Id$	*/
/*
 * Copyright (c) 2005 Anders Magnusson (ragge@ludd.luth.se).
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
 *	- All allocation is done on the way up.
 *
 * The member n_rall holds the allocated register if the table entry
 * has needs. Return value used at rewriting is determined by the
 * reclaim field in the table.
 *
 * The type regcode keeps track of allocated registers used when
 * assigning registers. It is not stored in the node.
 *
 * alloregs() returns the return registers from each instruction.
 */

#include "pass2.h"
#include <strings.h>

static int usedregs;
int regblk[REGSZ];

static int isfree(int wreg, int nreg);
static void setused(int wreg, int nreg);
static int findfree(int nreg, int breg);

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
	    "PREF", "RRIGHT", "RLEFT", "RESCx" };
	int i, c;

	for (i = c = 0; i < 9; i++) {
		if ((cword & (1 << i)) == 0)
			continue;
		if (c)
			fputc(',', stderr);
		fprintf(stderr, "%s", names[i]);
		c = 1;
	}
}

/*
 * Print temp registers inuse in a readable format.
 */
static void
prtuse(void)
{
	int i, c;

	fprintf(stderr, " reguse=<");
	for (i = c = 0; i < REGSZ; i++) {
		if (istreg(i) == 0)
			continue;
		if ((regblk[i] & 1) == 0)
			continue;
		if (c)
			fputc(',', stderr);
		fprintf(stderr, "%s", rnames[i]);
		c = 1;
	}
	fputc('>', stderr);
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
	if ((q->n_su = finduni(q, INTAREG|INTBREG)) == -1)
		comperr("movenode failed, subnode=%p", p);
	q->n_su |= LREG;
	return q;
}

/*
 * Get nreg number of (consecutive) registers.
 * wantreg is a hint on which regs are preferred.
 * XXX - check allowed regs.
 */
regcode
getregs(int wantreg, int nreg, int breg)
{
	regcode regc;

	if ((wantreg == NOPREF) || !isfree(wantreg, nreg)) {
		if ((wantreg = findfree(nreg, breg)) < 0)
			comperr("getregs: can't alloc %d regs type %d",
			    nreg, breg);
	}
	setused(wantreg, nreg);
	MKREGC(regc, wantreg, nreg);

	return regc;
}

/*
 * Free previously allocated registers.
 */
void
freeregs(regcode regc)
{
	int reg = REGNUM(regc), cnt = REGSIZE(regc);
	int i;

	for (i = reg; i < reg+cnt; i++) {
		if ((regblk[i] & 1) == 0)
			comperr("freeregs: freeing free reg %d", i);
		regblk[i] &= ~1;
	}
}

/*
 * Free registers allocated by needs but not returned.
 * Returns regcode of the returned registers.
 */
static regcode
shave(regcode regc, int nreg, int rewrite)
{
	regcode regc2;
	int size;

	if (nreg <= 1)
		return regc; /* No unneccessary regs allocated */
	size = REGSIZE(regc)/nreg;
	if (rewrite & RESC1) {
		MKREGC(regc2, REGNUM(regc)+size, REGSIZE(regc)-size);
		MKREGC(regc, REGNUM(regc), size);
	} else if (rewrite & RESC2) {
		if (nreg > 2) {
			MKREGC(regc2, REGNUM(regc)+2*size, size);
			freeregs(regc2);
		}
		MKREGC(regc2, REGNUM(regc), size);
		MKREGC(regc, REGNUM(regc)+size, size);
	} else if (rewrite & RESC3) {
		MKREGC(regc2, REGNUM(regc), REGSIZE(regc)-size);
		MKREGC(regc, REGNUM(regc)+2*size, size);
	}
	freeregs(regc2);
	return regc;
}

void
genregs(NODE *p)
{
	regcode regc;
	int i;

	for (i = 0; i < REGSZ; i++)
		regblk[i] = 0;
	usedregs = 0;
	if (p->n_op == FORCE) {
		regc = alloregs(p, RETREG);
		if (REGNUM(regc) != RETREG)
			p->n_left = movenode(p->n_left, RETREG);
		freeregs(regc);
		setused(RETREG, REGSIZE(regc));
		MKREGC(regc, RETREG, REGSIZE(regc));
	} else
		regc = alloregs(p, NOPREF);

	/* Check that no unwanted registers are still allocated */
	freeregs(regc);
	for (i = 0; i < REGSZ; i++) {
		if (istreg(i) == 0)
			continue;
		if (regblk[i] & 1)
			comperr("register %d lost!", i);
	}
}

/*
 * Check if nreg registers at position wreg are unused.
 */
static int
isfree(int wreg, int nreg)
{
	int i, isb, ist;

	if (wreg < 0)
		return 0;
	isb = isbreg(wreg) != 0;
	ist = istreg(wreg) != 0;
	for (i = wreg; i < (wreg+nreg); i++) {
		if (isb != (isbreg(i) != 0))
			return 0;
		if (ist != (istreg(i) != 0))
			return 0;
		if ((regblk[i] & 1) == 1)
			return 0;
	}
	return 1; /* Free! */
}

/*
 * Set use bit on some registers.
 */
static void
setused(int wreg, int nreg)
{
	int i;

	if (wreg < 0)
		comperr("setused on reg %d size %d\n", wreg, nreg);
	for (i = wreg; i < (wreg+nreg); i++) {
		if (regblk[i] & 1)
			comperr("setused on used reg %d size %d\n", wreg, nreg);
		regblk[i] |= 1;
	}
}

/*
 * Find nreg free regs somewhere.
 */
static int 
findfree(int nreg, int breg)
{
	int i;

	for (i = 0; i < REGSZ-nreg; i++) {
		if ((breg && !isbreg(i)) || (!breg && isbreg(i)))
			continue;
		if (isfree(i, nreg))
			return i;
	}
	return -1;
}

/*
 * Be sure not to trash a non-temporary register.
 */
static NODE *
checkreg(regcode *reg, int wantreg, NODE *p)
{
	regcode regc;

	if (!istreg(REGNUM(*reg)) && wantreg != REGNUM(*reg)) {
		/* left is neither temporary, nor wanted and 
		 * is soon to be trashed. Must move */
		regc = getregs(NOPREF, REGSIZE(*reg), isbreg(wantreg));
		p = movenode(p, REGNUM(regc));
		freeregs(*reg);
		*reg = regc;
	}
	return p;
}

#ifdef notyet
/*
 * See if a wanted reg can be shared with regs alloced, taken in account that 
 * return regs may not be the expected.
 * wantreg refer to already allocated registers.
 * XXX - wantreg should be of type regcode.
 */
static regcode
canshare(NODE *p, struct optab *q, int wantreg)
{
	int nreg = (q->needs & NACOUNT);
	int sz = szty(p->n_type);
	regcode regc;

	if (nreg == 1) {
		MKREGC(regc,wantreg,sz); /* XXX what if different size? */
	} else if ((q->rewrite & RESC1) && isfree(wantreg+sz, sz*(nreg-1))) {
			MKREGC(regc, wantreg, sz);
			setused(wantreg+sz, sz*(nreg-1));
	} else if ((q->rewrite & RESC2) && isfree(wantreg-sz, sz) &&
		    (nreg > 2 ? isfree(wantreg+sz, sz) : 1)) {
			MKREGC(regc, wantreg, sz);
			setused(wantreg-sz, sz);
			if (nreg > 2)
				setused(wantreg+sz, sz);
	} else if ((q->rewrite & RESC3) && isfree(wantreg-sz*2, sz*2)) {
			MKREGC(regc, wantreg, sz);
			setused(wantreg-sz*2, sz*2);
	} else
		regc = getregs(NOPREF, nreg*sz);
	return regc;
}
#endif

regcode
alloregs(NODE *p, int wantreg) 
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	regcode regc, regc2, regc3;
	int i, size;
	int cword = 0, rallset = 0;
	int nareg, nbreg, sreg;
	NODE *r;

	if (p->n_su == -1) /* For OREGs and similar */
		return alloregs(p->n_left, wantreg);
	nbreg = nareg = sreg = size = 0; /* XXX gcc */
	/*
	 * There may be instructions that have very strange
	 * requirements on register allocation.
	 * Call machine-dependent code to get to know:
	 * - left input reg
	 * - right input reg
	 * - node needed reg
	 * - return reg
	 */
	if (q->needs & NSPECIAL)
		return regalloc(p, q, wantreg);

	/*
	 * Are there any allocation requirements?
	 * If so, registers must be available (is guaranteed by sucomp()).
	 */
	if (q->needs & (NACOUNT|NBCOUNT)) {
		int nr = q->needs & (NACOUNT|NBCOUNT);
		while (nr & NACOUNT) nareg++, nr -= NAREG;
#ifndef BREGS_STACK
		while (nr & NBCOUNT) nbreg++, nr -= NBREG;
#endif
		size = szty(p->n_type);
		sreg = nareg * size;
		sreg += nbreg * size;
		if (nareg && nbreg)
			comperr("%p: cannot alloc both AREG and BREG (yet)", p);
		cword = R_PREF;
	}

	if (p->n_su & RMASK)
		cword += R_RREG;
	if (p->n_su & LMASK)
		cword += R_LREG;
	if (q->needs & (NASL|NBSL))
		cword += R_NASL;
	if (q->needs & (NASR|NBSR))
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
		fprintf(stderr, "%p) cword ", p);
		prtcword(cword);
		prtuse();
		fputc('\n', stderr);
	}
#endif

	/*
	 * Some registers may not be allowed to use for storing a specific
	 * datatype, so check the wanted reg here.
	 */
	if (wantreg != NOPREF && mayuse(wantreg, p->n_type) == 0) {
		wantreg = findfree(szty(p->n_type), nbreg);
#ifdef PCC_DEBUG
		if (rdebug)
			fprintf(stderr, "wantreg changed to %s\n",
			    rnames[wantreg]);
#endif
	}

	/*
	 * Handle some ops separate.
	 */
	switch (p->n_op) {
	case UCALL:
	 	/* All registers must be free here. */
#ifdef old
		if (findfree(fregs, 0) < 0) /* XXX check BREGs */
			comperr("UCALL and not all regs free!");
#else
		{ int bs, rmsk = TAREGS|TBREGS;
			while ((bs = ffs(rmsk))) {
				bs--;
				if (regblk[bs] & 1)
					comperr("UCALL and reg %d used", bs);
				rmsk &= ~(1 << bs);
			}
		}
#endif
		if (cword & R_LREG) {
			regc = alloregs(p->n_left, NOPREF);
			freeregs(regc);
			/* Check that all regs are free? */
		}
		regc = getregs(RETREG, szty(p->n_type), nbreg);
		p->n_rall = RETREG;
		return regc;

	case UMUL:
		if ((p->n_su & LMASK) != LOREG)
			break;
		/* This op will be folded into OREG in code generation */
		regc = alloregs(p->n_left, wantreg);
		i = REGNUM(regc);
		freeregs(regc);
		regc = getregs(i, sreg, nbreg);
		p->n_rall = REGNUM(regc);
		return shave(regc, nareg+nbreg, q->rewrite);

	case ASSIGN:
		/*
		 * If left is in a register, and right is requested to
		 * be in a register, try to make it the same reg.
		 */
		if (p->n_left->n_op == REG && (p->n_su & RMASK) == RREG) {
			regc = alloregs(p->n_right, p->n_left->n_rval);
			if (REGNUM(regc) == p->n_left->n_rval) {
				p->n_su = DORIGHT; /* Forget this node */
				return regc;
			}  else
				freeregs(regc); /* do the normal way */
		}
		break;
	}

	switch (cword) {
	case 0: /* No registers, ignore */
		MKREGC(regc,0,0);
		break;

	case R_DOR:
		regc = alloregs(p->n_right, wantreg);
		break;

	case R_PREF:
		regc = getregs(wantreg, sreg, nbreg);
		p->n_rall = REGNUM(regc);
		rallset = 1;
		freeregs(regc);
		MKREGC(regc,0,0);
		break;

	case R_RRGHT: /* Reclaim, be careful about regs */
	case R_RLEFT:
		r = getlr(p, cword == R_RRGHT ? 'R' : 'L');
		if (r->n_op == REG) {
			MKREGC(regc, r->n_rval, szty(r->n_type));
			setused(r->n_rval, szty(r->n_type));
		} else
			MKREGC(regc,0,0);
		break;

	case R_LREG: /* Left in register */
		regc = alloregs(p->n_left, wantreg);
		break;

	case R_RREG: /* Typical for ASSIGN node */ 
		regc = alloregs(p->n_right, wantreg);
		freeregs(regc);
		MKREGC(regc,0,0);
		break;

	case R_RREG+R_LREG+R_PREF:
		regc = alloregs(p->n_left, wantreg);
		regc2 = alloregs(p->n_right, NOPREF);
		regc3 = getregs(wantreg, sreg, nbreg);
		freeregs(regc);
		freeregs(regc2);
		p->n_rall = REGNUM(regc3);
		freeregs(regc3);
		rallset = 1;
		MKREGC(regc,0,0);
		break;

	case R_RREG+R_PREF:
		regc = alloregs(p->n_right, wantreg);
		regc2 = getregs(wantreg, sreg, nbreg);
		p->n_rall = REGNUM(regc2);
		freeregs(regc2);
		freeregs(regc);
		rallset = 1;
		MKREGC(regc,0,0);
		break;

	case R_RESC: /* Reclaim allocated stuff */
		regc = getregs(wantreg, sreg, nbreg);
		break;

	case R_LREG+R_RRGHT: /* Left in register */
		regc = alloregs(p->n_left, wantreg);
		freeregs(regc);
		MKREGC(regc,0,0);
		break;

	case R_LREG+R_PREF+R_RESC:
		regc2 = alloregs(p->n_left, wantreg);
		regc = getregs(NOPREF, sreg, nbreg);
		freeregs(regc2);
		p->n_rall = REGNUM(regc);
		rallset = 1;
		regc = shave(regc, nareg+nbreg, q->rewrite);
		break;

	case R_LREG+R_PREF+R_RLEFT: /* Allocate regs, reclaim left */
		regc = alloregs(p->n_left, wantreg);
		regc2 = getregs(NOPREF, sreg, nbreg);
		p->n_rall = REGNUM(regc2);
		rallset = 1;
		p->n_left = checkreg(&regc, wantreg, p->n_left);
		freeregs(regc2);
		break;

	case R_LREG+R_PREF: /* Allocate regs, reclaim nothing */
		regc = alloregs(p->n_left, wantreg);
		regc2 = getregs(NOPREF, sreg, nbreg);
		p->n_rall = REGNUM(regc2);
		rallset = 1;
		freeregs(regc2);
		freeregs(regc);
		/* Nothing to reclaim */
		MKREGC(regc, 0, 0);
		break;

	case R_LREG+R_PREF+R_RRGHT: /* Allocate regs, reclaim right */
		regc = alloregs(p->n_left, wantreg);
		regc2 = getregs(NOPREF, sreg, nbreg);
		p->n_rall = REGNUM(regc2);
		rallset = 1;
		freeregs(regc2);
		freeregs(regc);
		/* Nothing to reclaim unless right is in a reg */
		MKREGC(regc, p->n_rval, szty(p->n_type));
		break;

	case R_LREG+R_NASL+R_PREF:
		/* left in a reg, alloc regs, no reclaim, may share left */
		regc2 = alloregs(p->n_left, wantreg);
		/* Check for sharing. XXX - fix common routine */
		i = REGNUM(regc2);
		freeregs(regc2);
		regc = getregs(i, sreg, nbreg);
		p->n_rall = REGNUM(regc);
		rallset = 1;
		freeregs(regc);
		MKREGC(regc, 0, 0); /* Nothing to reclaim */
		break;

	case R_LREG+R_NASL+R_RLEFT:
		/* left in a reg, alloc regs, reclaim regs, may share left */
		regc = alloregs(p->n_left, wantreg);
		break;

	case R_LREG+R_NASL+R_PREF+R_RESC:
		/* left in a reg, alloc regs, reclaim regs, may share left */
		regc2 = alloregs(p->n_left, wantreg);
		/* Check for sharing. XXX - fix common routine */
		i = REGNUM(regc2);
		freeregs(regc2);
		regc = getregs(i, sreg, nbreg);
		p->n_rall = REGNUM(regc);
		rallset = 1;
		regc = shave(regc, nareg+nbreg, q->rewrite);
		break;

	case R_DOR+R_RREG: /* Typical for ASSIGN node */
	case R_DOR+R_RLEFT+R_RREG: /* Typical for ASSIGN node */
	case R_DOR+R_RRGHT+R_RREG: /* Typical for ASSIGN node */
	case R_RREG+R_RRGHT: /* Typical for ASSIGN node */
	case R_RREG+R_RLEFT: /* Typical for ASSIGN node */
		regc = alloregs(p->n_right, wantreg);
		break;

	case R_DOR+R_RREG+R_LREG:
		regc = alloregs(p->n_right, NOPREF);
		regc2 = alloregs(p->n_left, NOPREF);
		freeregs(regc2);
		freeregs(regc);
		MKREGC(regc, 0, 0);
		break;

	case R_DOR+R_RREG+R_PREF:
		regc = alloregs(p->n_right, NOPREF);
		regc3 = getregs(NOPREF, sreg, nbreg);
		p->n_rall = REGNUM(regc3);
		rallset = 1;
		freeregs(regc3);
		freeregs(regc);
		MKREGC(regc, 0, 0);
		break;

	case R_DOR+R_RREG+R_LREG+R_PREF:
		regc = alloregs(p->n_right, NOPREF);
		regc2 = alloregs(p->n_left, NOPREF);
		regc3 = getregs(NOPREF, sreg, nbreg);
		p->n_rall = REGNUM(regc3);
		rallset = 1;
		freeregs(regc3);
		freeregs(regc2);
		freeregs(regc);
		MKREGC(regc, 0, 0);
		break;

	case R_RREG+R_LREG+R_PREF+R_RRGHT:
		regc2 = alloregs(p->n_left, NOPREF);
		regc = alloregs(p->n_right, wantreg);
		regc3 = getregs(NOPREF, sreg, nbreg);
		p->n_rall = REGNUM(regc3);
		rallset = 1;
		freeregs(regc3);
		freeregs(regc2);
		break;

	case R_DOR+R_RREG+R_PREF+R_RRGHT:
		regc = alloregs(p->n_right, wantreg);
		regc2 = getregs(NOPREF, sreg, nbreg);
		p->n_right = checkreg(&regc, wantreg, p->n_right);
		freeregs(regc2);
		break;

	case R_DOR+R_RREG+R_LREG+R_RRGHT:
		regc = alloregs(p->n_right, wantreg);
		regc2 = alloregs(p->n_left, NOPREF);
		p->n_right = checkreg(&regc, wantreg, p->n_right);
		freeregs(regc2);
		break;

	case R_DOR+R_RREG+R_NASL+R_PREF+R_RESC:
		regc3 = alloregs(p->n_right, NOPREF);
		regc2 = getregs(wantreg, sreg, nbreg);
		regc = shave(regc2, nareg+nbreg, q->rewrite);
		p->n_rall = REGNUM(regc2);
		rallset = 1;
		freeregs(regc3);
		break;

	/*
	 * Leaf nodes is where it all begin.
	 */
	case R_PREF+R_RESC: /* Leaf node that puts a value into a register */
	case R_NASR+R_PREF+R_RESC:
		regc = getregs(wantreg, sreg, nbreg);
		break;

	case R_NASL+R_PREF+R_RESC: /* alloc + reclaim regs, may share left */
		regc2 = getregs(wantreg, sreg, nbreg);
		regc = shave(regc2, nareg+nbreg, q->rewrite);
		p->n_rall = REGNUM(regc2);
		rallset = 1;
		break;

	case R_NASL+R_PREF: /* alloc, may share left */
		regc = getregs(wantreg, sreg, nbreg);
		p->n_rall = REGNUM(regc);
		rallset = 1;
		freeregs(regc);
		MKREGC(regc,0,0);
		break;

	case R_LREG+R_RLEFT: /* Operate on left leg */
		regc = alloregs(p->n_left, wantreg);
		p->n_left = checkreg(&regc, wantreg, p->n_left);
		break;

	case R_LREG+R_RREG: /* both legs in registers, no reclaim */
		/* Used for logical ops */
		regc = alloregs(p->n_left, wantreg);
		regc2 = alloregs(p->n_right, NOPREF);
		freeregs(regc2);
		freeregs(regc);
		MKREGC(regc,0,0);
		break;

	case R_LREG+R_RREG+R_RLEFT: /* binop, reclaim left */
		regc = alloregs(p->n_left, wantreg);
		regc2 = alloregs(p->n_right, NOPREF);

		p->n_left = checkreg(&regc, wantreg, p->n_left);
		freeregs(regc2);
		break;

	case R_LREG+R_RREG+R_RRGHT: /* binop, reclaim right */
		regc2 = alloregs(p->n_left, NOPREF);
		regc = alloregs(p->n_right, wantreg);
		p->n_right = checkreg(&regc, wantreg, p->n_right);
		freeregs(regc2);
		break;

	case R_RREG+R_LREG+R_NASL+R_PREF+R_RESC:
		/* l+r in reg, need regs, reclaim alloced regs, may share l */

		/* Traverse left first, it may be shared */
		regc = alloregs(p->n_left, NOPREF);
		freeregs(regc);
		regc = getregs(wantreg, sreg, nbreg);
		regc3 = alloregs(p->n_right, NOPREF);
		freeregs(regc3);
		p->n_rall = REGNUM(regc);
		rallset = 1;
		regc = shave(regc, nareg+nbreg, q->rewrite);

		break;

	case R_DOR+R_RREG+R_LREG+R_NASL+R_PREF+R_RESC:
		/* l+r in reg, need regs, reclaim alloced regs, may share l */

		/* Traverse right first, it may not be shared */
		regc3 = alloregs(p->n_right, NOPREF);
		if (findfree(sreg, 0) < 0) { /* XXX BREGs */
			/* No regs found, must move around */
			regc = getregs(NOPREF, REGSIZE(regc3), nbreg);
			p->n_right = movenode(p->n_right, REGNUM(regc));
			freeregs(regc3);
			regc3 = regc;
		}

		/* Check where to get our own regs. Try wantreg first */
		if (isfree(wantreg, sreg))
			i = wantreg;
		else if ((i = findfree(sreg, 0)) < 0) /* XXX BREGs */
			comperr("alloregs out of regs");

		/* Now allocate left, try to share it with our needs */
		regc = alloregs(p->n_left, i);

		/* So, at last finished. Cleanup */
		freeregs(regc);
		freeregs(regc3);

		regc = getregs(i, size, nbreg);
		p->n_rall = REGNUM(regc);
		rallset = 1;
		regc = shave(regc, nareg+nbreg, q->rewrite);
		break;

	case R_DOR+R_RREG+R_LREG+R_RLEFT:
		/* l+r in reg, reclaim left */
		regc2 = alloregs(p->n_right, NOPREF);
		regc = alloregs(p->n_left, wantreg);
		p->n_left = checkreg(&regc, wantreg, p->n_left);
		freeregs(regc2);
		break;

	case R_DOR+R_RREG+R_LREG+R_PREF+R_RRGHT:
		/* l+r in reg, reclaim right */
		regc = alloregs(p->n_right, wantreg);
		regc2 = alloregs(p->n_left, NOPREF);
		if ((p->n_rall = findfree(sreg, 0)) < 0) /* XXX BREGs */
			comperr("alloregs out of regs2");
		rallset = 1;
		freeregs(regc2);
		break;

	default:
#ifdef PCC_DEBUG
		fprintf(stderr, "%p) cword ", p);
		prtcword(cword);
		fputc('\n', stderr);
#endif
		comperr("alloregs");
	}
	if (rallset == 0)
		p->n_rall = REGNUM(regc);
	if (REGSIZE(regc) > szty(p->n_type) && !logop(p->n_op))
		comperr("too many regs returned for %p (%d)", p, REGSIZE(regc));
	return regc;
}

#define MAX(a,b) (((a) > (b)) ? (a) : (b))
 
/*
 * Count the number of registers needed to evaluate a tree.
 * This is the trivial implementation, for machines with symmetric
 * registers. Machines with difficult register assignment strategies
 * may need to define this function themselves.
 * Return value is the number of registers used so far.
 */
int
sucomp(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	int left, right;
	int nreg;

	if (p->n_su == -1)
		return sucomp(p->n_left);
   
	if (p->n_op == UCALL) {
		if ((p->n_su & LMASK) && sucomp(p->n_left) < 0)
			return -1;
		return fregs;
	}

	nreg = (q->needs & NACOUNT) * szty(p->n_type);

	switch (p->n_su & RMASK) {
	case RREG:
	case ROREG:
		if ((right = sucomp(p->n_right)) < 0)
			return right;
		break;
	case RTEMP: 
		cerror("sucomp RTEMP");
	default:
		right = 0;
	}
	switch (p->n_su & LMASK) {
	case LREG:
	case LOREG:
		if ((left = sucomp(p->n_left)) < 0)
			return left;
		break;	
	case LTEMP:
		cerror("sucomp LTEMP");
	default:
		left = 0; 
	}
//printf("sucomp: node %p right %d left %d\n", p, right, left);
	if ((p->n_su & RMASK) && (p->n_su & LMASK) &&
	    right + szty(p->n_left->n_type) > fregs &&
	    left + szty(p->n_right->n_type) > fregs) {
		int r = p->n_right->n_op;
		int l = p->n_left->n_op;
		/*
		 * Must store one subtree. Store the tree
		 * with highest SU, or left (unless it is an assign node).
		 * Be careful to not try to store an OREG.
		 */
		if (r == OREG && l == OREG)
			comperr("sucomp: cannot generate code, node %p", p);
		if ((right > left && r != OREG) || l == OREG) {
			p->n_right = store(p->n_right);
		} else {
			if (p->n_op == ASSIGN && l == UMUL)
				p->n_left->n_left = store(p->n_left->n_left);
			else
				p->n_left = store(p->n_left);
		}
		return -1;
	}
	if ((right+left) > fregs) {
		/* Almost out of regs, traverse the highest SU first */
		if (right > left)
			p->n_su |= DORIGHT;
	} else if (right && (q->needs & (NASL|NBSL)) && (q->rewrite & RLEFT)) {
		/* Make it easier to share regs */
		p->n_su |= DORIGHT;
	} else if (right > left) {
		p->n_su |= DORIGHT;
	}
	/* If both in regs and equal size, return l+r */
	if (left && left == right)
		left += right; /* returned below */

	if (right > nreg)
		nreg = right;
	if (left > nreg)
		nreg = left;
	return nreg;
}

/*
 * New-style register allocator using graph coloring.
 * The design is based on the George and Appel paper
 * "Iterated Register Coalescing", ACM Transactions, No 3, May 1996.
 */

#define	BITALLOC(ptr,all,sz) { \
	int __s = ((sz+NUMBITS-1)/NUMBITS) * sizeof(bittype); \
	ptr = all(__s); memset(ptr, 0, __s); }

#define	BIT2BYTE(bits) ((((bits)+NUMBITS-1)/NUMBITS)*(NUMBITS/8))

int tempmin, tempfe, tempmax;
/*
 * Count the number of registers needed to evaluate a tree.
 * This is only done to find the evaluation order of the tree.
 * While here, assign temp numbers to the registers that will
 * be needed when the tree is evaluated.
 *
 * While traversing the tree, assign temp numbers to the registers
 * used by all instructions:
 *	- n_rall is always set to the outgoing number. If the
 *	  instruction is 2-op (addl r0,r1) then an implicit move
 *	  is inserted just before the left (clobbered) operand.
 *	- if the instruction has needs then temporaries of size 
 *	  szty() are assumed above the n_rall number.
 */
int
nsucomp(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	int left, right;
	int nreg, need;

	if (p->n_su == -1)
		return nsucomp(p->n_left);
   
	nreg = (q->needs & NACOUNT) * szty(p->n_type); /* XXX BREGs */
	if (callop(p->n_op))
		nreg = MAX(fregs, nreg);

	switch (p->n_su & RMASK) {
	case RREG:
	case ROREG:
		right = nsucomp(p->n_right);
		break;
	case RTEMP: 
		cerror("sucomp RTEMP");
	default:
		right = 0;
	}
	switch (p->n_su & LMASK) {
	case LREG:
	case LOREG:
		left = nsucomp(p->n_left);
		break;	
	case LTEMP:
		cerror("sucomp LTEMP");
	default:
		left = 0; 
	}

	if ((p->n_su & RMASK) && (p->n_su & LMASK)) {
		/* Two children */
		if (right == left)
			need = left + MAX(nreg, 1);
		else
			need = MAX(right, left);
		/* XXX - should take care of overlapping needs */
		if (right > left) {
			p->n_su |= DORIGHT;
		} else if (right == left) {
			/* A favor to 2-operand architectures */
			if ((q->rewrite & RRIGHT) == 0)
				p->n_su |= DORIGHT;
		}
	} else if ((p->n_su & RMASK) || (p->n_su & LMASK)) {
		/* One child */
		need = MAX(right, left) + nreg;
	} else
		need = nreg;
	p->n_rall = tempmax++;
	if (!callop(p->n_op) && !(q->needs & NSPECIAL))
		tempmax += nreg;
	return nreg;
}

/*
 * Data structure overview for this implementation:
 *
 * Each temporary (called "node") is described by the type REGW.  
 * Space for all nodes is allocated initially as an array, so 
 * the nodes can be can be referenced both by the node number and
 * by pointer.
 * 
 * All moves are represented by the type REGM, allocated when needed. 
 *
 * The "live" set used during graph building is represented by a bitset.
 *
 * Interference edges are represented by struct AdjSet, hashed and linked
 * from index into the edgehash array.
 *
 * A mapping from each node to the moves it is assiciated with is 
 * maintained by an array moveList which for each node number has a linked
 * list of MOVL types, each pointing to a REGM.
 *
 * Adjacency list is maintained by the adjList array, indexed by the
 * node number. Each adjList entry points to an ADJL type, and is a
 * single-linked list for all adjacent nodes.
 *
 * degree, alias and color are integer arrays indexed by node number.
 */

/*
 * linked list of adjacent nodes.
 */
typedef struct regw3 {
	struct regw3 *r_next;
	int a_temp;
} ADJL;

/*
 * Structure describing a move.
 */
typedef struct regm {
	DLIST_ENTRY(regm) link;
	int src, dst;
	int queue;
} REGM;

typedef struct movlink {
	struct movlink *next;
	REGM *regm;
} MOVL;

/*
 * Structure describing a temporary.
 */
typedef struct regw {
	DLIST_ENTRY(regw) link;
	int r_alias;		/* number of aliased register */
	ADJL *r_adjList;	/* linked list of adjacent nodes */
	int r_degree;		/* degree of this node */
	int r_color;		/* final node color */
	struct regw *r_onlist;	/* which work list this node belongs to */
	MOVL *r_moveList;	/* moves associated with this node */
} REGW;

#define	RDEBUG(x)	if (rdebug) printf x

static int maxregs;	/* max usable regs for allocator */
static int allregs;	/* bitmask of usable regs */
static REGW *nodeblock;
#define	ALIAS(x)	nodeblock[x].r_alias
#define	ADJLIST(x)	nodeblock[x].r_adjList
#define	DEGREE(x)	nodeblock[x].r_degree
#define	COLOR(x)	nodeblock[x].r_color
#define	ONLIST(x)	nodeblock[x].r_onlist
#define	MOVELIST(x)	nodeblock[x].r_moveList
#define	R_TEMP(w)	(w - nodeblock)

static bittype *live;

#define	PUSHWLIST(w, l)	DLIST_INSERT_AFTER(&l, w, link); w->r_onlist = &l
#define	POPWLIST(l)	popwlist(&l);
#define	DELWLIST(w)	DLIST_REMOVE(w, link)
#define WLISTEMPTY(h)	DLIST_ISEMPTY(&h,link)
#define	PUSHMLIST(w, l, q)	DLIST_INSERT_AFTER(&l, w, link); w->queue = q
#define	POPMLIST(l)	popmlist(&l);

/*
 * Worklists, a node is always on exactly one of these lists.
 */
static REGW precolored, simplifyWorklist, freezeWorklist, spillWorklist,
	spilledNodes, coalescedNodes, coloredNodes, selectStack;

static inline REGW *
popwlist(REGW *l)
{
	REGW *w = DLIST_NEXT(l, link);

	DLIST_REMOVE(w, link);
	w->r_onlist = NULL;
	return w;
}

/*
 * Move lists, a move node is always on only one list.
 */
static REGM coalescedMoves, constrainedMoves, frozenMoves, 
	worklistMoves, activeMoves;
enum { COAL, CONSTR, FROZEN, WLIST, ACTIVE };

static inline REGM *
popmlist(REGM *l)
{
	REGM *w = DLIST_NEXT(l, link);

	DLIST_REMOVE(w, link);
	return w;
}

#define	REGUALL(r, n)	r = &nodeblock[n]
#define	GETRALL(p)	((p)->n_su == -1 ? getrall(p) : (p)->n_rall)

static int
getrall(NODE *p)
{
	while (p->n_su == -1)
		p = p->n_left;
	return p->n_rall;
}

#define LIVEADD(x) { RDEBUG(("Liveadd: %d\n", x)); BITSET(live, (x)); }
#define LIVEDEL(x) { RDEBUG(("Livedel: %d\n", x)); BITCLEAR(live, (x)); }

#define	MOVELISTADD(t, p) movelistadd(t, p)
#define WORKLISTMOVEADD(s,d) worklistmoveadd(s,d)

static void
movelistadd(int t, REGM *p)
{
	MOVL *w = tmpalloc(sizeof(MOVL));

	w->regm = p;
	w->next = MOVELIST(t);
	MOVELIST(t) = w;
}

static REGM *
worklistmoveadd(int src, int dst)
{
	REGM *w = tmpalloc(sizeof(REGM));

	DLIST_INSERT_AFTER(&worklistMoves, w, link);
	w->src = src;
	w->dst = dst;
	w->queue = WLIST;
	return w;
}

struct AdjSet {
	struct AdjSet *next;
	int u, v;
} *edgehash[256];

/* Check if a node pair is adjacent */
static int
adjSet(int u, int v)
{
	struct AdjSet *w;
	int t;

	if (u > v)
		t = v, v = u, u = t;
	w = edgehash[(u+v) & 255];
	for (; w; w = w->next) {
		if (u == w->u && v == w->v)
			return 1;
	}
	return 0;
}

/* Add a pair to adjset.  No check for dups */
static void
adjSetadd(int u, int v)
{
	struct AdjSet *w;
	int t;

	if (u > v)
		t = v, v = u, u = t;
	t = (u+v) & 255;
	w = tmpalloc(sizeof(struct AdjSet));
	w->u = u, w->v = v;
	w->next = edgehash[t];
	edgehash[t] = w;
}

/*
 * Add an interference edge between two nodes.
 */
static void
AddEdge(int u, int v)
{
	ADJL *x;

	RDEBUG(("AddEdge: u %d v %d\n", u, v));

	if (u == v)
		return;
	if (adjSet(u, v))
		return;

	adjSetadd(u, v);

	if (u >= tempmin) {
		x = tmpalloc(sizeof(ADJL));
		x->a_temp = v;
		x->r_next = ADJLIST(u);
		ADJLIST(u) = x;
		DEGREE(u)++;
	}
	if (v >= tempmin) {
		x = tmpalloc(sizeof(ADJL));
		x->a_temp = u;
		x->r_next = ADJLIST(v);
		ADJLIST(v) = x;
		DEGREE(v)++;
	}
}

static int
MoveRelated(int n)
{
	MOVL *l;
	REGM *w;

	for (l = MOVELIST(n); l; l = l->next) {
		w = l->regm;
		if (w->queue == ACTIVE || w->queue == WLIST)
			return 1;
	}
	return 0;
}

static void
MkWorklist(void)
{
	REGW *w;
	int n;

	DLIST_INIT(&precolored, link);
	DLIST_INIT(&simplifyWorklist, link);
	DLIST_INIT(&freezeWorklist, link);
	DLIST_INIT(&spillWorklist, link);
	DLIST_INIT(&spilledNodes, link);
	DLIST_INIT(&coalescedNodes, link);
	DLIST_INIT(&coloredNodes, link);
	DLIST_INIT(&selectStack, link);
	for (n = tempmin; n < tempmax; n++) {
		REGUALL(w, n);
		if (DEGREE(n) >= maxregs) {
			PUSHWLIST(w, spillWorklist);
		} else if (MoveRelated(n)) {
			PUSHWLIST(w, freezeWorklist);
		} else {
			PUSHWLIST(w, simplifyWorklist);
		}
	}
}

static void
addalledges(int e)
{
	int i, j, k;

	for (i = 0; i < tempmax; i += NUMBITS) {
		if ((k = live[i/NUMBITS]) == 0)
			continue;
		while (k) {
			j = ffs(k)-1;
			AddEdge(i+j, e);
			k &= ~(1 << j);
		}
	}
}

static void
moveadd(int def, int use)
{
	REGM *r;

	RDEBUG(("moveadd: def %d use %d\n", def, use));

	r = WORKLISTMOVEADD(use, def);
	MOVELISTADD(def, r);
	MOVELISTADD(use, r);
	addalledges(def);
}

/*
 * Do the actual liveness analysis inside a tree.
 * The tree is walked in backward-execution order to catch the 
 * long-term temporaries.
 * Moves to/from precolored registers are implicitly placed
 * inside the affected nodes (like return value from CALLs).
 */
static void
insnwalk(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	int def, nreg;
	int i, l, r;
	int left, right, rmask;

	RDEBUG(("insnwalk: %p\n", p));

	def = p->n_rall;
	addalledges(def);
	nreg = q->needs & NACOUNT;

	left = right = rmask = 0;
	if (q->needs & NSPECIAL) {
		int res;
		/* special instruction requirements */

		nspecial(q, &left, &right, &res, &rmask);

		/* if result ends up in a certain register, add move */
		if (res)
			moveadd(def, ffs(res)-1);
		
		/* Add edges for used registers */
		l = rmask;
		for (i = 0; l; i++) {
			if (l & 1) {
				LIVEADD(i);
				addalledges(i);
			}
			l >>= 1;
		}
		nreg = 0;
	}

	if (callop(p->n_op)) {
		/* first add all edges */
		for (i = 0; i < maxregs; i++)
			addalledges(i);
		/* implicit move after call */
		moveadd(def, RETREG);
		nreg = 0;
	}
	/*
	 * rall is the base of need allocation, RESCx tells which
	 * allocated register that should be reclaimed.
	 */
	for (i = 0; i < nreg; i++) {
		LIVEADD(def+i);
		addalledges(def+i);
	}
	/* If leg regs may not be shared, add edges */
	if ((p->n_su & LMASK) == LREG && !(q->needs & NASL))
		addalledges(GETRALL(p->n_left));
	if ((p->n_su & RMASK) == RREG && !(q->needs & NASR))
		addalledges(GETRALL(p->n_right));

	/* now remove the needs from the live set */
	for (i = 0; i < nreg; i++)
		LIVEDEL(def+i);
	for (l = rmask, i = 0; l; i++) {
		if (l & 1)
			LIVEDEL(i);
		l >>= 1;
	}

	/* walk down the legs and add interference edges */
	l = r = 0;
	if ((p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		r = GETRALL(p->n_right);
		LIVEADD(r);
		if (q->rewrite & RLEFT)
			moveadd(p->n_rall, GETRALL(p->n_left));
		if (q->needs & NSPECIAL && left)
			moveadd(ffs(left)-1, GETRALL(p->n_left));
		insnwalk(p->n_left);
		LIVEDEL(r);
	}
	if ((p->n_su & RMASK)) {
		if (r == 0 && (p->n_su & LMASK)) {
			l = GETRALL(p->n_left);
			LIVEADD(l);
		}
		if (q->rewrite & RRIGHT)
			moveadd(p->n_rall, GETRALL(p->n_right));
		if (q->needs & NSPECIAL && right)
			moveadd(ffs(right)-1, GETRALL(p->n_right));
		insnwalk(p->n_right);
		if (l)
			LIVEDEL(l);
	}
	if (!(p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		if (q->rewrite & RLEFT)
			moveadd(p->n_rall, GETRALL(p->n_left));
		if (q->needs & NSPECIAL && left)
			moveadd(ffs(left)-1, GETRALL(p->n_left));
		insnwalk(p->n_left);
	}

	/* Finished, clean up live set */
	if (r)
		LIVEDEL(r);
	if (l)
		LIVEDEL(l);
}

static bittype **gen, **kill, **in, **out;
static int unum;

static void
unionize(NODE *p)
{

	if (p->n_op != TEMP)
		return;
	BITSET(gen[unum], ((int)p->n_lval - tempmin));
}

/*
 * Do variable liveness analysis.  Only analyze the long-lived 
 * variables, and save the live-on-exit temporaries in a bit-field
 * at the end of each basic block. This bit-field is later used
 * when doing short-range liveness analysis in Build().
 */
static void
LivenessAnalysis(void)
{
	extern struct basicblock bblocks;
	struct basicblock *bb;
	struct interpass *ip;
	int i, b;

	/*
	 * generate the gen-kill sets for all basic blocks.
	 */
	DLIST_FOREACH(bb, &bblocks, bbelem) {
		i = bb->bbnum;
		for (ip = bb->last; ; ip = DLIST_PREV(ip, qelem)) {
			/* gen/kill is 'p', this node is 'n' */
			if (ip->type == IP_NODE) {
				NODE *p;

				if (ip->ip_node->n_op == ASSIGN &&
				    ip->ip_node->n_left->n_op == TEMP) {
					b = ip->ip_node->n_left->n_lval -
					    tempmin;
					BITCLEAR(gen[i], b);
					BITSET(kill[i], b);
					p = ip->ip_node->n_right;
				} else
					p = ip->ip_node;
				unum = i;
				walkf(p, unionize);
			}
			if (ip == bb->first)
				break;
		}
		memcpy(in[i], gen[i], BIT2BYTE(tempfe-tempmin));
#ifdef PCC_DEBUG
		if (rdebug) {
			printf("basic block %d\ngen: ", bb->bbnum);
			for (i = 0; i < tempfe-tempmin; i++)
				if (TESTBIT(gen[bb->bbnum], i))
					printf("%d ", i+tempmin);
			printf("\nkill: ");
			for (i = 0; i < tempfe-tempmin; i++)
				if (TESTBIT(kill[bb->bbnum], i))
					printf("%d ", i+tempmin);
			printf("\n");
		}
#endif
	}
}

#define	SETCOPY(t,f,i,n) for (i = 0; i < n; i += NUMBITS) t[i] = f[i]
#define	SETSET(t,f,i,n) for (i = 0; i < n; i += NUMBITS) t[i] |= f[i]
#define	SETCLEAR(t,f,i,n) for (i = 0; i < n; i += NUMBITS) t[i] &= ~f[i]
#define	SETCMP(v,t,f,i,n) for (i = v = 0; i < n; i += NUMBITS) \
	if (t[i] != f[i]) v = 1

/*
 * Build the set of interference edges and adjacency list.
 */
static void
Build(struct interpass *ip)
{
	extern struct basicblock bblocks;
	struct basicblock *bb;
	struct cfgnode *cn;
	extern int nbblocks;
	bittype *saved;
	int i, j, again, nbits;

	if (xsaveip && xssaflag) {
		/* Just fetch space for the temporaries from stack */

		nbits = tempfe - tempmin;
		gen = alloca(nbblocks*sizeof(bittype*));
		kill = alloca(nbblocks*sizeof(bittype*));
		in = alloca(nbblocks*sizeof(bittype*));
		out = alloca(nbblocks*sizeof(bittype*));
		for (i = 0; i < nbblocks; i++) {
			BITALLOC(gen[i],alloca,nbits);
			BITALLOC(kill[i],alloca,nbits);
			BITALLOC(in[i],alloca,nbits);
			BITALLOC(out[i],alloca,nbits);
		}
		BITALLOC(saved,alloca,nbits);
		LivenessAnalysis();

		/* do liveness analysis on basic block level */
		do {
			again = 0;
			/* XXX - loop should be in reversed execution-order */
			DLIST_FOREACH_REVERSE(bb, &bblocks, bbelem) {
				int i = bb->bbnum;
				SETCOPY(saved, out[i], j, nbits);
				SLIST_FOREACH(cn, &bb->children, cfgelem) {
					SETSET(out[i], in[cn->bblock->bbnum],
					    j, nbits);
				}
				SETCMP(again, saved, out[i], j, nbits);
				SETCOPY(saved, in[i], j, nbits);
				SETCOPY(in[i], out[i], j, nbits);
				SETCLEAR(in[i], kill[i], j, nbits);
				SETSET(in[i], gen[i], j, nbits);
				SETCMP(again, saved, in[i], j, nbits);
			}
		} while (again);

#ifdef PCC_DEBUG
		if (rdebug) {
			DLIST_FOREACH(bb, &bblocks, bbelem) {
				printf("basic block %d\nin: ", bb->bbnum);
				for (i = 0; i < tempfe-tempmin; i++)
					if (TESTBIT(in[bb->bbnum], i))
						printf("%d ", i+tempmin);
				printf("\nout: ");
				for (i = 0; i < tempfe-tempmin; i++)
					if (TESTBIT(out[bb->bbnum], i))
						printf("%d ", i+tempmin);
				printf("\n");
			}
		}
#endif

		DLIST_FOREACH(bb, &bblocks, bbelem) {
			SETCOPY(live, out[i], j, nbits);
			for (ip = bb->last; ; ip = DLIST_PREV(ip, qelem)) {
				if (ip->type == IP_NODE)
					insnwalk(ip->ip_node);
				if (ip == bb->first)
					break;
			}
		}
	} else 
		insnwalk(ip->ip_node);

	if (rdebug) {
		int i;
		struct AdjSet *w;

		printf("Interference edges\n");
		for (i = 0; i < 256; i++) {
			if ((w = edgehash[i]) == NULL)
				continue;
			for (; w; w = w->next)
				printf("%d <-> %d\n", w->u, w->v);
		}
	}

}

static void
EnableMoves(int n)
{
	MOVL *l;
	REGM *m;

	for (l = MOVELIST(n); l; l = l->next) {
		m = l->regm;
		if (m->queue != ACTIVE)
			continue;
		DLIST_REMOVE(m, link);
		PUSHMLIST(m, worklistMoves, WLIST);
	}
}

static void
EnableAdjMoves(int nodes)
{
	ADJL *w;
	int n;

	EnableMoves(nodes);
	for (w = ADJLIST(nodes); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
		EnableMoves(w->a_temp);
	}
}

static void
DecrementDegree(int m)
{
	REGW *w = &nodeblock[m];

	if (DEGREE(m)-- != maxregs)
		return;

	EnableAdjMoves(m);
	DELWLIST(w);
	ONLIST(m) = 0;
	if (MoveRelated(m)) {
		PUSHWLIST(w, freezeWorklist);
	} else {
		PUSHWLIST(w, simplifyWorklist);
	}
}

static void
Simplify(void)
{
	REGW *w;
	ADJL *l;

	RDEBUG(("Simplify\n"));
	w = POPWLIST(simplifyWorklist);
	PUSHWLIST(w, selectStack);

	l = w->r_adjList;
	for (; l; l = l->r_next) {
		if (ONLIST(l->a_temp) == &selectStack ||
		    ONLIST(l->a_temp) == &coalescedNodes)
			continue;
		DecrementDegree(l->a_temp);
	}
}

static int
GetAlias(int n)
{
	if (ONLIST(n) == &coalescedNodes)
		return GetAlias(ALIAS(n));
	return n;
}

static int
OK(int t, int r)
{
	if (DEGREE(t) < maxregs || t < maxregs || adjSet(t, r))
		return 1;
	return 0;
}

static int
adjok(int v, int u)
{
	ADJL *w;
	int t;

	for (w = ADJLIST(v); w; w = w->r_next) {
		t = w->a_temp;
		if (OK(t, u) == 0)
			return 0;
	}
	return 1;
}

static int
Conservative(int u, int v)
{
	ADJL *w;
	int k, n;

	k = 0;
	for (w = ADJLIST(u); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
		if (DEGREE(n) >= maxregs)
			k++;
	}
	for (w = ADJLIST(v); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
		if (DEGREE(n) >= maxregs)
			k++;
	}
	return k < maxregs;
}

static void
AddWorkList(int u)
{
	REGW *w = &nodeblock[u];

	if (u >= maxregs && !MoveRelated(u) && DEGREE(u) < maxregs) {
		DELWLIST(w);
		PUSHWLIST(w, simplifyWorklist);
	}
}

static void
Combine(int u, int v)
{
	REGW *w;
	MOVL *m;
	ADJL *l;
	int t;

	w = &nodeblock[v];
	if (ONLIST(v) == &freezeWorklist) {
		DELWLIST(w);
	} else {
		DELWLIST(w);
	}
	PUSHWLIST(w, coalescedNodes);
	ALIAS(v) = u;
	if ((m = MOVELIST(u))) {
		while (m->next)
			m = m->next;
		m->next = MOVELIST(v);
	} else
		MOVELIST(u) = MOVELIST(v);
	EnableMoves(v);
	for (l = ADJLIST(v); l; l = l->r_next) {
		t = l->a_temp;
		if (ONLIST(t) == &selectStack || ONLIST(t) == &coalescedNodes)
			continue;
		AddEdge(t, u);
		DecrementDegree(t);
	}
	if (DEGREE(u) >= maxregs && ONLIST(u) == &freezeWorklist) {
		w = &nodeblock[u];
		DELWLIST(w);
		PUSHWLIST(w, spillWorklist);
	}
}

static void
Coalesce(void)
{
	REGM *m;
	int x, y, u, v;

	RDEBUG(("Coalesce\n"));
	m = POPMLIST(worklistMoves);
	x = GetAlias(m->src);
	y = GetAlias(m->dst);
	if (y < maxregs)
		u = y, v = x;
	else
		u = x, v = y;
	if (u == v) {
		PUSHMLIST(m, coalescedMoves, COAL);
		AddWorkList(u);
	} else if (v < maxregs || adjSet(u, v)) {
		PUSHMLIST(m, constrainedMoves, CONSTR);
		AddWorkList(u);
		AddWorkList(v);
	} else if ((u < maxregs && adjok(v, u)) ||
	    (u >= maxregs && Conservative(u, v))) {
		PUSHMLIST(m, coalescedMoves, COAL);
		Combine(u, v);
		AddWorkList(u);
	} else {
		PUSHMLIST(m, activeMoves, ACTIVE);
	}
}

static void
FreezeMoves(int u)
{
	MOVL *w, *o;
	REGM *m;
	REGW *z;
	int x, y, v;

	for (w = MOVELIST(u); w; w = w->next) {
		m = w->regm;
		if (m->queue != WLIST && m->queue != ACTIVE)
			continue;
		x = m->src;
		y = m->dst;
		if (GetAlias(y) == GetAlias(u))
			v = GetAlias(x);
		else
			v = GetAlias(y);
		DLIST_REMOVE(m, link);
		PUSHMLIST(m, frozenMoves, FROZEN);
		if (ONLIST(v) != &freezeWorklist)
			continue;
		for (o = MOVELIST(v); o; o = o->next)
			if (o->regm->queue == WLIST || o->regm->queue == ACTIVE)
				break;
		if (o == NULL) {
			z = &nodeblock[v];
			DELWLIST(z);
			PUSHWLIST(z, simplifyWorklist);
		}
	}
}

static void
Freeze(void)
{
	REGW *u;

	RDEBUG(("Freeze\n"));
	u = POPWLIST(freezeWorklist);
	PUSHWLIST(u, simplifyWorklist);
	FreezeMoves(R_TEMP(u));
}

static void
SelectSpill(void)
{
	REGW *w;

	RDEBUG(("SelectSpill\n"));
	if (rdebug)
		DLIST_FOREACH(w, &spillWorklist, link)
			printf("SelectSpill: %d\n", R_TEMP(w));

	/* XXX - no heuristics, just fetch first element */
	w = POPWLIST(spillWorklist)
	PUSHWLIST(w, simplifyWorklist);
	RDEBUG(("Freezing node %d\n", R_TEMP(w)));
	FreezeMoves(R_TEMP(w));
}

static void
paint(NODE *p)
{
	if (p->n_su != -1 && p->n_rall != NOPREF)
		p->n_rall = COLOR(p->n_rall);
}

static void
AssignColors(struct interpass *ip)
{
	int okColors, o, c, n;
	REGW *w;
	ADJL *x;

	RDEBUG(("AssignColors\n"));
	while (!WLISTEMPTY(selectStack)) {
		w = POPWLIST(selectStack);
		n = R_TEMP(w);
		okColors = allregs;
		for (x = ADJLIST(n); x; x = x->r_next) {
			o = GetAlias(x->a_temp);
			RDEBUG(("Adj(%d): %d (%d)\n", n, o, x->a_temp));
			if (ONLIST(o) == &coloredNodes ||
			    ONLIST(o) == &precolored) {
				o = COLOR(o);
				okColors &= ~(1 << o);
			}
		}
		if (okColors == 0) {
			PUSHWLIST(w, spilledNodes);
			RDEBUG(("Spilling node %d\n", R_TEMP(w)));
		} else {
			PUSHWLIST(w, coloredNodes);
			c = ffs(okColors)-1;
			COLOR(n) = c;
		}
	}
	DLIST_FOREACH(w, &coalescedNodes, link)
		w->r_color = COLOR(GetAlias(R_TEMP(w)));

	if (rdebug)
		for (o = tempmin; o < tempmax; o++)
			printf("%d: %d\n", o, COLOR(o));
	if (DLIST_ISEMPTY(&spilledNodes, link)) {
		if (ip->type == IP_NODE)
			walkf(ip->ip_node, paint);
	}
}

static	struct interpass ipbase; /* to save nodes before calling emit */

static void
modifytree(NODE *p)
{
	extern struct interpass *storesave;
	REGW *w;
	NODE *q;

	/* Check for the node in the spilled list */
	DLIST_FOREACH(w, &spilledNodes, link) {
		if (R_TEMP(w) != p->n_rall)
			continue;
		/* Got the matching temp */
		RDEBUG(("modifytree: storing node %p\n", p));
		q = talloc();
		*q = *p;
		q = store(q);
		*p = *q;
		nfree(q);
		DLIST_INSERT_BEFORE(&ipbase, storesave, qelem);
		DELWLIST(w);
		return;
	}
}

/*
 * Store all spilled nodes in memory by fetching a temporary on the stack.
 * In the non-optimizing case recurse into emit() and let it handle the
 * stack space, otherwise generate stacklevel nodes and put them into 
 * the full function chain.
 * In the non-optimizing case be careful so that the coloring code won't
 * overwrite itself during recursion.
 */
static void
RewriteProgram(struct interpass *ip)
{

	DLIST_INIT(&ipbase, qelem);

	/* walk the tree bottom-up and isolate found nodes for spilling */
	walkf(ip->ip_node, modifytree);
	if (!DLIST_ISEMPTY(&spilledNodes, link))
		comperr("RewriteProgram");
	DLIST_FOREACH(ip, &ipbase, qelem) {
		emit(ip);
	}
}

/*
 * Do register allocation for trees by graph-coloring.
 */
int
ngenregs(struct interpass *ip)
{
	int i, sz = (tempmax+NUMBITS-1)/NUMBITS;

#define	ASZ(type) sizeof(type) * tempmax
#define ALLOC(type) tmpalloc(ASZ(type))

	nodeblock = ALLOC(REGW);
	memset(nodeblock, 0, ASZ(REGW));

	live = tmpalloc(sz * sizeof(bittype));
	memset(live, 0, sz * sizeof(bittype));
	memset(edgehash, 0, sizeof(edgehash));

	allregs = xsaveip ? AREGS : TAREGS;
	maxregs = 0;
	for (i = allregs; i ; i >>= 1)
		if (i & 1)
			maxregs++;

#ifdef PCC_DEBUG
	if (rdebug) {
		if (xsaveip) {
			struct interpass *ip2;
			DLIST_FOREACH(ip2, ip, qelem)
				if (ip2->type == IP_NODE)
					fwalk(ip2->ip_node, e2print, 0);
		} else
			fwalk(ip->ip_node, e2print, 0);
		printf("allregs: %x, maxregs %d\n", allregs, maxregs);
		printf("ngenregs: numtemps %d (%d, %d, %d)\n", tempmax-tempmin,
		    tempmin, tempfe, tempmax);
	}
#endif

	for (i = 0; i < maxregs; i++) {
		ONLIST(i) = &precolored;
		COLOR(i) = i;
	}
	DLIST_INIT(&coalescedMoves, link);
	DLIST_INIT(&constrainedMoves, link);
	DLIST_INIT(&frozenMoves, link);
	DLIST_INIT(&worklistMoves, link);
	DLIST_INIT(&activeMoves, link);

	Build(ip);
	RDEBUG(("Build done\n"));
	MkWorklist();
	RDEBUG(("MkWorklist done\n"));
	do {
		if (!WLISTEMPTY(simplifyWorklist))
			Simplify();
		else if (!WLISTEMPTY(worklistMoves))
			Coalesce();
		else if (!WLISTEMPTY(freezeWorklist))
			Freeze();
		else if (!WLISTEMPTY(spillWorklist))
			SelectSpill();
	} while (!WLISTEMPTY(simplifyWorklist) || !WLISTEMPTY(worklistMoves) ||
	    !WLISTEMPTY(freezeWorklist) || !WLISTEMPTY(spillWorklist));
	AssignColors(ip);
	if (rdebug)
		fwalk(ip->ip_node, e2print, 0);
	if (!WLISTEMPTY(spilledNodes)) {
		RewriteProgram(ip);
		return 1;
	} else
		return 0; /* Done! */
}
