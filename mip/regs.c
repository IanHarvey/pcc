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

int tempmin, tempmax;
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
		if (right > left)
			p->n_su |= DORIGHT;
	} else if ((p->n_su & RMASK) || (p->n_su & LMASK)) {
		/* One child */
		need = MAX(right, left) + nreg;
	} else
		need = nreg;
	p->n_rall = tempmax++;
	if (!callop(p->n_op))
		p->n_rall += nreg;
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
 * Structure describing a temporary.
 */
typedef struct regw {
	DLIST_ENTRY(regw) link;
	int r_temp; /* XXX - can be eliminated? */
} REGW;

typedef struct regw3 {
	struct regw3 *r_next;
	int r_temp;
} ADJL;

/*
 * Structure describing a move.
 */
typedef struct regm {
	DLIST_ENTRY(regm) link;
	struct regm *r_next;
	NODE *r_node;
	int queue;
} REGM;

typedef struct movlink {
	struct movlink *next;
	REGM *regm;
} MOVL;

#define	RDEBUG(x)	if (rdebug) printf x

#define	MAXREGS		5 /* XXX */
#define	ALLREGS		31 /* XXX */
static REGW *nodeblock;
static int *alias;

static bittype *live;
static ADJL **adjList;
static int *degree;
static int *color;

#define	PUSHWLIST(w, l)	DLIST_INSERT_AFTER(&l, w, link); onlist[w->r_temp] = &l
#define	POPWLIST(l)	popwlist(&l);
#define	DELWLIST(w)	DLIST_REMOVE(w, link)
#define WLISTEMPTY(h)	DLIST_ISEMPTY(&h,link)
//#define	PUSHWLIST(w, l)	w->r_next = l, l = w, onlist[w->r_temp] = &l
//#define	POPWLIST(l)	l; onlist[l->r_temp] = NULL; l = l->r_next
//#define	PUSHMLIST(w, l, q)	w->r_next = l, l = w, w->queue = q
//#define	POPMLIST(l)	l; l = l->r_next
#define	PUSHMLIST(w, l, q)	DLIST_INSERT_AFTER(&l, w, link); w->queue = q
#define	POPMLIST(l)	popmlist(&l);

/*
 * Worklists, a node is always on exactly one of these lists.
 */
static REGW precolored, simplifyWorklist, freezeWorklist, spillWorklist,
	spilledNodes, coalescedNodes, coloredNodes, selectStack;
static REGW **onlist;

static inline REGW *
popwlist(REGW *l)
{
	REGW *w = DLIST_NEXT(l, link);

	DLIST_REMOVE(w, link);
	onlist[w->r_temp] = NULL;
	return w;
}

/*
 * Move lists, a move node is always on only one list.
 */
static REGM coalescedMoves, constrainedMoves, frozenMoves, 
	worklistMoves, activeMoves;
static MOVL **moveList;
enum { COAL, CONSTR, FROZEN, WLIST, ACTIVE };

static inline REGM *
popmlist(REGM *l)
{
	REGM *w = DLIST_NEXT(l, link);

	DLIST_REMOVE(w, link);
	return w;
}

#define	REGUALL(r, n)	{ r = &nodeblock[n]; r->r_temp = n; }
#define	GETRALL(p)	((p)->n_su == -1 ? getrall(p) : (p)->n_rall)

static int
getrall(NODE *p)
{
	while (p->n_su == -1)
		p = p->n_left;
	return p->n_rall;
}

#define LIVEADD(x) BITSET(live, x)
#define LIVEDEL(x) BITCLEAR(live, x)

#define	MOVELISTADD(t, p) movelistadd(t, p)
#define WORKLISTMOVEADD(p) worklistmoveadd(p)

static void
movelistadd(int t, REGM *p)
{
	MOVL *w = tmpalloc(sizeof(MOVL));

	w->regm = p;
	w->next = moveList[t];
	moveList[t] = w;
}

static REGM *
worklistmoveadd(NODE *p)
{
	REGM *w = tmpalloc(sizeof(REGM));

	DLIST_INSERT_AFTER(&worklistMoves, w, link);
	w->r_node = p;
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
		x->r_temp = v;
		x->r_next = adjList[u];
		adjList[u] = x;
		degree[u]++;
	}
	if (v > tempmin) {
		x = tmpalloc(sizeof(ADJL));
		x->r_temp = u;
		x->r_next = adjList[v];
		adjList[v] = x;
		degree[v]++;
	}
}

static int
MoveRelated(int n)
{
	MOVL *l;
	REGM *w;

	for (l = moveList[n]; l; l = l->next) {
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
		if (degree[n] >= MAXREGS) {
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

/*
 * Walk a tree execution-wise backwards and add interference edges.
 * If a leg exists and has the result in the same register, add move.
 * Care is taken for all specialties the table may have for different
 * instructions.
 */
static void             
insnwalk(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	int t;

	RDEBUG(("insnwalk: %p\n", p));

	/* For call instructions there is an extra implicit
	 * move to temporary of return value just after the call */
	if (callop(p->n_op)) {
		REGM *r;

		t = 0; /* XXX return register */
		LIVEDEL(t);
		r = WORKLISTMOVEADD(p);
		MOVELISTADD(t, r);
		MOVELISTADD(p->n_rall, r);

		LIVEADD(p->n_rall); /* add this register to the live set */
		addalledges(p->n_rall);
		LIVEDEL(p->n_rall);
		LIVEADD(t);
	}
	/* begin basic */

	LIVEADD(p->n_rall); /* add this register to the live set */
	addalledges(p->n_rall);
	if (callop(p->n_op)) {
		addalledges(0);
		addalledges(1);
	}
	LIVEDEL(p->n_rall);

	if ((p->n_su & LMASK) == LREG)
		LIVEADD(GETRALL(p->n_left));
	if ((p->n_su & RMASK) == RREG)
		LIVEADD(GETRALL(p->n_right));
	/* end basic */

	t = 0;
	if ((p->n_su & LMASK) == LREG && (q->rewrite & RLEFT)) {
		/* add left move insn */
		t = GETRALL(p->n_left);
	}
	if ((p->n_su & RMASK) == RREG && (q->rewrite & RRIGHT)) {
		/* add right move insn */
		t = GETRALL(p->n_right);
	}
	if (t) {
		REGM *r;

		LIVEDEL(t);
		r = WORKLISTMOVEADD(p);
		MOVELISTADD(t, r);
		MOVELISTADD(p->n_rall, r);

		LIVEADD(p->n_rall); /* add this register to the live set */
		addalledges(p->n_rall);
		LIVEDEL(p->n_rall);
		LIVEADD(t);
	}

	if ((p->n_su & DORIGHT) && (p->n_su & LMASK))
		insnwalk(p->n_left);
	if ((p->n_su & RMASK))
		insnwalk(p->n_right);
	if (!(p->n_su & DORIGHT) && (p->n_su & LMASK))
		insnwalk(p->n_left);
}

static void
LivenessAnalysis(struct interpass *ip, struct interpass *ie)
{
}

/*
 * Build the set of interference edges and adjacency list.
 */
static void
Build(struct interpass *ip, struct interpass *ie)
{
	DLIST_INIT(&coalescedMoves, link);
	DLIST_INIT(&constrainedMoves, link);
	DLIST_INIT(&frozenMoves, link);
	DLIST_INIT(&worklistMoves, link);
	DLIST_INIT(&activeMoves, link);
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

#ifdef notyet
	struct interpass *w;
	NODE *t;

	forall b = basic blocks {
		live = liveout(b);
		forall t = trees(b) in reverse order {
			insnwalk(t);
		}
	}
#endif
}

static void
EnableMoves(int n)
{
	MOVL *l;
	REGM *m;

	for (l = moveList[n]; l; l = l->next) {
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
	for (w = adjList[nodes]; w; w = w->r_next) {
		n = w->r_temp;
		if (onlist[n] == &selectStack || onlist[n] == &coalescedNodes)
			continue;
		EnableMoves(w->r_temp);
	}
}

static void
DecrementDegree(int m)
{
	REGW *w = &nodeblock[m];

	if (degree[m]-- != MAXREGS)
		return;

	EnableAdjMoves(m);
	DELWLIST(w);
	onlist[m] = 0;
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

	l = adjList[w->r_temp];
	for (; l; l = l->r_next) {
		if (onlist[l->r_temp] == &selectStack ||
		    onlist[l->r_temp] == &coalescedNodes)
			continue;
		DecrementDegree(l->r_temp);
	}
}

static int
GetAlias(int n)
{
	if (onlist[n] == &coalescedNodes)
		return GetAlias(alias[n]);
	return n;
}

static int
OK(int t, int r)
{
	if (degree[t] < MAXREGS || t < MAXREGS || adjSet(t, r))
		return 1;
	return 0;
}

static int
adjok(int v, int u)
{
	ADJL *w;
	int t;

	for (w = adjList[v]; w; w = w->r_next) {
		t = w->r_temp;
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
	for (w = adjList[u]; w; w = w->r_next) {
		n = w->r_temp;
		if (onlist[n] == &selectStack || onlist[n] == &coalescedNodes)
			continue;
		if (degree[n] >= MAXREGS)
			k++;
	}
	for (w = adjList[v]; w; w = w->r_next) {
		n = w->r_temp;
		if (onlist[n] == &selectStack || onlist[n] == &coalescedNodes)
			continue;
		if (degree[n] >= MAXREGS)
			k++;
	}
	return k < MAXREGS;
}

static void
AddWorkList(int u)
{
	REGW *w = &nodeblock[u];

	if (u >= MAXREGS && !MoveRelated(u) && degree[u] < MAXREGS) {
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
	if (onlist[v] == &freezeWorklist) {
		DELWLIST(w);
	} else {
		DELWLIST(w);
	}
	PUSHWLIST(w, coalescedNodes);
	alias[v] = u;
	if ((m = moveList[u])) {
		while (m->next)
			m = m->next;
		m->next = moveList[v];
	} else
		moveList[u] = moveList[v];
	EnableMoves(v);
	for (l = adjList[v]; l; l = l->r_next) {
		t = l->r_temp;
		if (onlist[t] == &selectStack || onlist[t] == &coalescedNodes)
			continue;
		AddEdge(t, u);
		DecrementDegree(t);
	}
	if (degree[u] >= MAXREGS && onlist[u] == &freezeWorklist) {
		w = &nodeblock[u];
		DELWLIST(w);
		PUSHWLIST(w, spillWorklist);
	}
}

static void
Coalesce(void)
{
	struct optab *q;
	REGM *m;
	int x, y, u, v, z;

	RDEBUG(("Coalesce\n"));
	m = POPMLIST(worklistMoves);
	q = &table[TBLIDX(m->r_node->n_su)];
	x = GetAlias(m->r_node->n_rall);
	if (callop(q->op)) {
		z = 0; /* XXX return reg */
	} else {
		z = GETRALL(q->rewrite & RLEFT ?
		    m->r_node->n_left : m->r_node->n_right);
	}
	y = GetAlias(z);
	if (y < MAXREGS)
		u = y, v = x;
	else
		u = x, v = y;
	if (u == v) {
		PUSHMLIST(m, coalescedMoves, COAL);
		AddWorkList(u);
	} else if (v < MAXREGS || adjSet(u, v)) {
		PUSHMLIST(m, constrainedMoves, CONSTR);
		AddWorkList(u);
		AddWorkList(v);
	} else if ((u < MAXREGS && adjok(v, u)) ||
	    (u >= MAXREGS && Conservative(u, v))) {
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
	struct optab *q;
	MOVL *w, *o;
	REGM *m;
	REGW *z;
	int x, y, v;

	for (w = moveList[u]; w; w = w->next) {
		m = w->regm;
		if (m->queue != WLIST && m->queue != ACTIVE)
			continue;
		x = m->r_node->n_rall;
		q = &table[TBLIDX(m->r_node->n_su)];
		y = GETRALL(q->rewrite & RLEFT ?
		    m->r_node->n_left : m->r_node->n_right);
		if (GetAlias(y) == GetAlias(u))
			v = GetAlias(x);
		else
			v = GetAlias(y);
		DLIST_REMOVE(m, link);
		PUSHMLIST(m, frozenMoves, FROZEN);
		if (onlist[v] != &freezeWorklist)
			continue;
		for (o = moveList[v]; o; o = o->next)
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
	FreezeMoves(u->r_temp);
}

static void
SelectSpill(void)
{
	RDEBUG(("SelectSpill\n"));
	comperr("SelectSpill");
}

static void
paint(NODE *p)
{
	if (p->n_su != -1 && p->n_rall != NOPREF)
		p->n_rall = color[p->n_rall];
}

static void
AssignColors(struct interpass *ip, struct interpass *ie)
{
	int okColors, o, c, n;
	REGW *w;
	ADJL *x;

	RDEBUG(("AssignColors\n"));
	while (!WLISTEMPTY(selectStack)) {
		w = POPWLIST(selectStack);
		n = w->r_temp;
		okColors = ALLREGS;
		for (x = adjList[n]; x; x = x->r_next) {
			o = GetAlias(x->r_temp);
printf("Adj(%d): %d (%d)\n", n, o, x->r_temp);
			if (onlist[o] == &coloredNodes ||
			    onlist[o] == &precolored) {
				o = color[o];
				okColors &= ~(1 << o);
			}
		}
		if (okColors == 0) {
			PUSHWLIST(w, spilledNodes);
		} else {
			PUSHWLIST(w, coloredNodes);
			c = ffs(okColors)-1;
			color[n] = c;
		}
	}
	DLIST_FOREACH(w, &coalescedNodes, link)
		color[w->r_temp] = color[GetAlias(w->r_temp)];

	if (rdebug)
		for (o = tempmin; o < tempmax; o++)
			printf("%d: %d\n", o, color[o]);
	if (ip->type == IP_NODE)
		walkf(ip->ip_node, paint);
}

static void
RewriteProgram(void)
{
	comperr("RewriteProgram");
}

/*
 * Do register allocation for trees by graph-coloring.
 */
int
ngenregs(struct interpass *ip, struct interpass *ie)
{
	int i, sz = (tempmax+NUMBITS-1)/NUMBITS;

#define	ASZ(type) sizeof(type) * tempmax
#define ALLOC(type) tmpalloc(ASZ(type))
	nodeblock = ALLOC(REGW);
	alias = ALLOC(int);
	memset(alias, 0, ASZ(int));
	adjList = ALLOC(ADJL **); memset(adjList, 0, ASZ(ADJL **));
	degree = ALLOC(int); memset(degree, 0, ASZ(int));
	color = ALLOC(int); memset(color, 0, ASZ(int));
	onlist = ALLOC(REGW ***); memset(onlist, 0, ASZ(REGW ***));
	moveList = ALLOC(REGW **); memset(moveList, 0, ASZ(REGW **));

	live = tmpalloc(sz * sizeof(bittype));
	memset(live, 0, sz * sizeof(bittype));

#ifdef PCC_DEBUG
	if (rdebug) {
		if (xsaveip == 0)
			fwalk(ip->ip_node, e2print, 0);
		printf("ngenregs: numtemps %d (%d, %d)\n", tempmax-tempmin,
		    tempmin, tempmax);
	}
#endif

	if (xsaveip)
		LivenessAnalysis(ip, ie);
	for (i = 0; i < MAXREGS; i++)
		onlist[i] = &precolored;
	Build(ip, ie);
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
	AssignColors(ip, ie);
fwalk(ip->ip_node, e2print, 0);
	if (!WLISTEMPTY(spilledNodes)) {
		RewriteProgram();
		return 1;
	} else
		return 0; /* Done! */
}
