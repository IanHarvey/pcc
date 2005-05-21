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
 * The design is based on the George and Appel report, with
 * additions from Norman's paper to generalize the allocator.
 */

int tempbase = 8; /* XXX - should be max registers */
/*
 * Count the number of registers needed to evaluate a tree.
 * This is only done to find the evaluation order of the tree.
 * While here, assign temp numbers to the registers that will
 * be needed when the tree is evaluated.
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
	p->n_rall = tempbase++;
	return nreg;
}

/*
 * If r > l, then the interference graph contains move edges,
 * otherwise interference edges.
 */
int igraph[sizeof(int)*8];
struct nodinfo {
	struct nodinfo *next;
	int flags;
#define	INGRAPH	1
#define	HASMOVE	2
#define	COALESCED	4
	int numintf;
} nodinfo[32], *nodstk;

/*
 * Find next move node number.
 */
static inline int
nextmove(int n, int next)
{
	int i;

	for (i = next; i < n; i++) {
		if ((igraph[i] & (1 << n)) == 0)
			continue;
		return i;
	}
	for (; i < tempbase; i++) {
		if ((igraph[n] & (1 << i)) == 0)
			continue;
		return i;
	}
	return 0;
}
/*
 * Find next interfering node number.
 */
static inline int
nextint(int n, int next)
{
	int i;

	for (i = next; i < n; i++) {
		if ((igraph[n] & (1 << i)) == 0)
			continue;
		return i;
	}
	for (; i < tempbase; i++) {
		if ((igraph[i] & (1 << n)) == 0)
			continue;
		return i;
	}
	return 0;
}

static inline void
addint(int r, int l)
{
	int x;

	if (r < l)
		x = r, r = l, l = x;
	igraph[r] |= (1 << l);
	nodinfo[r].numintf++;
	nodinfo[l].numintf++;
}

static inline void
clrint(int r, int l)
{
	int x;

	if (r < l)
		x = r, r = l, l = x;
	igraph[r] |= (1 << l);
	nodinfo[r].numintf--;
	nodinfo[l].numintf--;
}

static inline void
addmove(int r, int l)
{
	int x;

	if (r > l)
		x = r, r = l, l = x;
	igraph[r] |= (1 << l);
	nodinfo[r].flags |= HASMOVE;
	nodinfo[l].flags |= HASMOVE;
}

static inline int
interferes(int r, int l)
{
	int x;

	if (r > l)
		x = r, r = l, l = x;
	return igraph[r] & (1 << l) ? 1 : 0;
}

struct intf {
	struct intf *next;
	int node;
};

static void
moreintf(int temp, struct intf *w)
{
	for (; w; w = w->next)
		addint(temp, w->node);
}

/*
 * Add interfering nodes to the interference graph.
 */
static int
interfere(NODE *p, struct intf *intp)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	struct intf intf;
	int right, left, rall;

	if (p->n_su == -1)
		return interfere(p->n_left, intp);

	right = left = 0;
	if (p->n_su & DORIGHT)
		right = interfere(p->n_right, intp);
	if (p->n_su & LMASK) {
		if (right) {
			intf.node = right;
			intf.next = intp;
			left = interfere(p->n_left, &intf);
		} else
			left = interfere(p->n_left, intp);
	}
	if (!(p->n_su & DORIGHT) && (p->n_su & RMASK)) {
		if (left) {
			intf.node = left;
			intf.next = intp;
			right = interfere(p->n_right, &intf);
		} else
			right = interfere(p->n_right, intp);
	}

	rall = p->n_rall;
	/*
	 * Add interference edges based on instruction needs.
	 */

	/* If both legs exists, they interfere */
	/* XXX - what if SPECIAL? */
	if (right && left)
		addint(right, left);

	/* If left leg exists and result is in left register, add move */
	if ((q->rewrite & RLEFT) && left)
		addmove(left, rall);

	/* If right leg exists and result is in right register, add move */
	if ((q->rewrite & RRIGHT) && right)
		addmove(right, rall);

	/* If we have needs, return needs, and they may not be shared 
	   with left leg, add interference */
	if (left && (q->needs & NACOUNT) && !(q->needs & NASL) &&
	    (q->rewrite & (RESC1|RESC2|RESC3)))
		addint(rall, left);

	/* If we have needs, return needs, and they may not be shared 
	   with right leg, add interference */
	if (right && (q->needs & NACOUNT) && !(q->needs & NASR) &&
	    (q->rewrite & (RESC1|RESC2|RESC3)))
		addint(rall, right);

	/*
	 * Add interference edges for this instruction to all
	 * temp nodes earlier allocated.
	 */
	moreintf(rall, intp);

	return rall;
}

/*
 * Remove non-move-related nodes from the graph.
 */
static void
simplify(NODE *p)
{
	int i, j, changed;

printf("simplify\n");
	do {
		changed = 0;
		for (i = 8; i < tempbase; i++) {
			if (nodinfo[i].flags & (HASMOVE|COALESCED))
				continue;
			if (nodinfo[i].numintf >= 6 || nodinfo[i].numintf == 0)
				continue; /* max regs */
			/* Find the other end of the edge and decrement it */
			changed = 1;
			j = 8;
printf("removing node %i\n", i);
			while ((j = nextint(i, j))) {
				nodinfo[i].numintf--;
				nodinfo[j].numintf--;
				j++;
			}
			nodinfo[i].next = nodstk;
			nodstk = &nodinfo[i];
		}
	} while (changed);
}

static void
pinterfere(NODE *p)
{
	int i, j; 

	printf("interferes:\n");
	for (i = 8; i < tempbase; i++) {
		for (j = i+1; j < tempbase; j++) {
			if (igraph[j] & (1 << i))
				printf("Node %d interferes with %d\n", i, j);
		}
	}

	printf("moves:\n");
	for (i = 8; i < tempbase; i++) {
		for (j = i+1; j < tempbase; j++) {
			if (igraph[i] & (1 << j))
				printf("Node %d moves to %d\n", i, j);
		}
	}

}

/*
 * Try to coalesce moves using the George and Appel strategy.
 */
static void
coalesce(NODE *p)
{
	int a, b, t, gotone;

printf("coalesce\n");
	do {
		gotone = 0;
		/* Loop over all nodes */
		for (a = 8; a < tempbase; a++) {
			if ((nodinfo[a].flags & HASMOVE) == 0 ||
			    (nodinfo[a].flags & COALESCED) != 0)
				continue;

			/* Check if it can be coalesced with the other end */
			for (b = nextmove(a, 8); b != 0; b = nextmove(a, b+1)) {
				/* b is the other end of the move */

				if (nodinfo[b].flags & COALESCED)
					continue; /* been here */

#if 0
				/*
				 * If b is coalesced already, point to the
				 * new node for checking.
				 */
				while (nodinfo[b].flags & COALESCED)
					b = nodinfo[b].next - nodinfo;
#endif

				/*
				 * If either all neighbors are of a degree < K
				 * or already interferes with (a,b).
				 */
				
				for (t = nextint(a, 8); t; t = nextint(a, t+1)){
					if (nodinfo[t].numintf >= 8 &&
					    !interferes(t, b))
						break;
				}
				if (t == 0) {
					/* an and b can be coalesced */
					/* coalesce to a and forget b */
printf("Coalescing node %d into %d\n", b, a);
					t = 8;
					while ((t = nextint(b, t+1)))
						addint(a, t);
					gotone++;
					nodinfo[b].flags |= COALESCED;
					nodinfo[b].next = &nodinfo[a];
				}
			}
		}
	} while (gotone);
}

/*
 * Do register allocation for trees by graph-coloring.
 */
int
ngenregs(struct interpass *ip, struct interpass *ie)
{
	NODE *p;

	for (;;ip = DLIST_NEXT(ip, qelem)) {
		if (ip->type != IP_NODE)
			continue;
		p = ip->ip_node;
		if (rdebug)
			fwalk(p, e2print, 0);
		interfere(p, NULL); /* Create interference graph */
		pinterfere(p);
		simplify(p);
		coalesce(p);
#if 0
		freeze(p);
		pspill(p);
		assign(p);
		aspill(p);
#endif
		if (ip == ie)
			return 0;
	}
}
