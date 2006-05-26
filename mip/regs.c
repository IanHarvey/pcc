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

#include "pass2.h"
#include <strings.h>
#include <stdlib.h>

#define MAX(a,b) (((a) > (b)) ? (a) : (b))
 
/*
 * New-style register allocator using graph coloring.
 * The design is based on the George and Appel paper
 * "Iterated Register Coalescing", ACM Transactions, No 3, May 1996.
 */

#define	BIT2BYTE(bits) ((((bits)+NUMBITS-1)/NUMBITS)*(NUMBITS/8))
#define	BITALLOC(ptr,all,sz) { \
	int __s = BIT2BYTE(sz); ptr = all(__s); memset(ptr, 0, __s); }

#undef COMPERR_PERM_MOVE
#define	RDEBUG(x)	if (rdebug) printf x
#define	RRDEBUG(x)	if (rdebug > 1) printf x
#define	RPRINTIP(x)	if (rdebug) printip(x)
#define	RDX(x)		x
#define UDEBUG(x)	if (udebug) printf x

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
	struct regw *a_temp;
} ADJL;

/*
 * Structure describing a move.
 */
typedef struct regm {
	DLIST_ENTRY(regm) link;
	struct regw *src, *dst;
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
	ADJL *r_adjList;	/* linked list of adjacent nodes */
	int r_class;		/* this nodes class */
	int r_nclass[NUMCLASS+1];	/* count of adjacent classes */
	struct regw *r_alias;		/* aliased temporary */
	int r_color;		/* final node color */
	struct regw *r_onlist;	/* which work list this node belongs to */
	MOVL *r_moveList;	/* moves associated with this node */
#ifdef PCC_DEBUG
	int nodnum;		/* Debug number */
#endif
} REGW;

/*
 * Worklists, a node is always on exactly one of these lists.
 */
static REGW precolored, simplifyWorklist, freezeWorklist, spillWorklist,
	spilledNodes, coalescedNodes, coloredNodes, selectStack;
static REGW initial, *nblock;
#ifdef PCC_DEBUG
int nodnum = 100;
#define	SETNUM(x)	(x)->nodnum = nodnum++
#define	ASGNUM(x)	(x)->nodnum
#else
#define SETNUM(x)
#define ASGNUM(x)
#endif

#define	ALLNEEDS (NACOUNT|NBCOUNT|NCCOUNT|NDCOUNT)

/* XXX */
REGW *ablock;

static int tempmin, tempmax, basetemp, xbits;
/*
 * nsavregs is an array that matches the permregs array.
 * Each entry in the array may have the values:
 * 0	: register coalesced, just ignore.
 * 1	: save register on stack
 * If the entry is 0 but the resulting color differs from the 
 * corresponding permregs index, add moves.
 * XXX - should be a bitfield!
 */
static int *nsavregs, *ndontregs;

/*
 * Return the REGW struct for a temporary.
 */
static REGW *
newblock(NODE *p)
{
	REGW *nb = &nblock[(int)p->n_lval];
	if (nb->link.q_forw == 0) {
		DLIST_INSERT_AFTER(&initial, nb, link);
		ASGNUM(nb) = p->n_lval;
		RDEBUG(("Adding longtime %d for tmp %d\n",
		    nb->nodnum, (int)p->n_lval));
	}
	if (nb->r_class == 0)
		nb->r_class = gclass(p->n_type);
	RDEBUG(("newblock: node %d class %d\n", nb->nodnum, nb->r_class));
	return nb;
}

#ifndef ragge
/*
 * Avoid unwanted edges for OREG registers.
 * XXX - should not be done like this.
 */
static int
chkoreg(NODE *p)
{
	int o;

//	if (p->n_op == UMUL)
//		p = p->n_left;
	if ((o = p->n_op) == TEMP) {
		p->n_regw = newblock(p);
		return 1;
	} else if (o == REG) {
		p->n_regw = &ablock[p->n_rval];
		return 1;
	}
	if ((o == PLUS || o == MINUS) && p->n_right->n_op == ICON &&
	    ((o = p->n_left->n_op) == TEMP || o == REG)) {
		p->n_left->n_regw = o == REG ?
		    &ablock[p->n_left->n_rval] : newblock(p->n_left);
		return 1;
	}
	return 0;
}

static void
setleaf(NODE *p)
{
	if (p->n_op != TEMP)
		return;

	p->n_regw = &nblock[(int)p->n_lval];
}
#endif

#ifdef notyet
/*
 * Count the number of registers needed to evaluate a tree.
 * This is only done to find the evaluation order of the tree.
 * It could be more intelligent for multiple register classes.
 */
int
sucomp(NODE *p)
{
	struct optab *q;
	int left, right;
	int nreg, need, i, nxreg;
	int nareg, nbreg, ncreg, ndreg;
	REGW *w;
	NODE *r;

	if (p->n_su == DOWNL)
		return sucomp(p->n_left);

	UDEBUG(("entering sucomp, node %p\n", p));

	if (p->n_op == CALL || p->n_op == STCALL || p->n_op == FORTCALL) {
		for (r = p->n_right; r->n_op == CM; r = r->n_left)
			sucomp(r->n_right);
		sucomp(r);
	}
   
	q = &table[TBLIDX(p->n_su)];
	nareg = (q->needs & NACOUNT);

	for (i = (q->needs & NBCOUNT), nbreg = 0; i; i -= NBREG)
		nbreg++;
	for (i = (q->needs & NCCOUNT), ncreg = 0; i; i -= NCREG)
		ncreg++;
	for (i = (q->needs & NDCOUNT), ndreg = 0; i; i -= NDREG)
		ndreg++;

	nxreg = nareg + nbreg + ncreg + ndreg;
	nreg = nxreg * szty(p->n_type);	/* XXX - sanitycheck this */
	if (callop(p->n_op))
		nreg = MAX(fregs, nreg);

	switch (p->n_su & RMASK) {
	case RREG:
		if (p->n_right->n_op == TEMP && (q->rewrite & RRIGHT) == 0) {
			right = 0;
			p->n_right->n_regw = &nblock[(int)p->n_right->n_lval];
		} else
			right = sucomp(p->n_right);
		break;

	case ROREG:
		if (oregok(p->n_right, 0))
			right = 0;
		else
			right = sucomp(p->n_right);
		break;

	case RTEMP: 
		cerror("sucomp RTEMP");
	default:
		if (optype(p->n_op) == BITYPE)
			walkf(p->n_right, setleaf);
		right = 0;
	}

	switch (p->n_su & LMASK) {
	case LREG:
		if (p->n_left->n_op == TEMP && (q->rewrite & RLEFT) == 0)
			left = 0;
			p->n_left->n_regw = &nblock[(int)p->n_left->n_lval];
		} else
			left = sucomp(p->n_left);
		break;

	case LOREG:
		if (oregok(p->n_left, 0))
			left = 0;
		else
			left = sucomp(p->n_left);
		break;	
	case LTEMP:
		cerror("sucomp LTEMP");
	default:
		if (optype(p->n_op) != LTYPE)
			walkf(p->n_left, setleaf);
		left = 0; 
	}

	UDEBUG(("node %p left %d right %d\n", p, left, right));

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

	UDEBUG(("node %p return regs %d\n", p, need));

	return need;
}
#endif

/*
 * Count the number of registers needed to evaluate a tree.
 * This is only done to find the evaluation order of the tree.
 * While here, assign temp numbers to the registers that will
 * be needed when the tree is evaluated.
 *
 * While traversing the tree, assign REGW nodes to the registers
 * used by all instructions:
 *	- n_regw[0] is always set to the outgoing node. If the
 *	  instruction is 2-op (addl r0,r1) then an implicit move
 *	  is inserted just before the left (clobbered) operand.
 *	- if the instruction has needs then REGW nodes are
 *	  allocated as n_regw[1] etc.
 */
int
nsucomp(NODE *p)
{
	struct optab *q;
	int left, right;
	int nreg, need, i, nxreg, o;
	int nareg, nbreg, ncreg, ndreg;
	REGW *w;

#ifdef ragge
	o = optype(p->n_op);

	if (p->n_su == 0) {
		int a = 0, b;
		if (o != LTYPE)
			a = nsucomp(p->n_left);
		if (o == BITYPE) {
			b = nsucomp(p->n_right);
			a = MAX(a, b);
			/* XXX - s�tt in DORIGHT */
		}
		return a;
	}
#else
	if (p->n_su == DOWNL)
		return nsucomp(p->n_left);
#endif

	UDEBUG(("entering nsucomp, node %p\n", p));

#ifndef ragge
	if (p->n_op == CALL || p->n_op == STCALL || p->n_op == FORTCALL) {
		for (r = p->n_right; r->n_op == CM; r = r->n_left)
			nsucomp(r->n_right);
		nsucomp(r);
	}
#endif
   
	q = &table[TBLIDX(p->n_su)];
	nareg = (q->needs & NACOUNT);

	for (i = (q->needs & NBCOUNT), nbreg = 0; i; i -= NBREG)
		nbreg++;
	for (i = (q->needs & NCCOUNT), ncreg = 0; i; i -= NCREG)
		ncreg++;
	for (i = (q->needs & NDCOUNT), ndreg = 0; i; i -= NDREG)
		ndreg++;

	nxreg = nareg + nbreg + ncreg + ndreg;
	nreg = nxreg * szty(p->n_type);	/* XXX - sanitycheck this */
	if (callop(p->n_op))
		nreg = MAX(fregs, nreg);

#ifdef ragge
	if (o == BITYPE) {
		right = nsucomp(p->n_right);
	} else
		right = 0;
#else
	switch (p->n_su & RMASK) {
	case RREG:
		if (p->n_right->n_op == TEMP && (q->rewrite & RRIGHT) == 0) {
			/* only read argument */
			p->n_right->n_regw = newblock(p->n_right);
			right = 0;
			break;
		}
		right = nsucomp(p->n_right);
		break;

	case ROREG:
		if (chkoreg(p->n_right))
			right = 0;
		else
			right = nsucomp(p->n_right);
		break;
	case RTEMP: 
		cerror("sucomp RTEMP");
	default:
		if (optype(p->n_op) == BITYPE)
			walkf(p->n_right, setleaf);
		right = 0;
	}
#endif

#ifdef ragge
	if (o != LTYPE)
		left = nsucomp(p->n_left);
	else
		left = 0;
#else
	switch (p->n_su & LMASK) {
	case LREG:
		if (p->n_left->n_op == TEMP && (q->rewrite & RLEFT) == 0) {
			/* only read argument */
			p->n_left->n_regw = newblock(p->n_left);
			left = 0;
			break;
		}
		left = nsucomp(p->n_left);
		break;

	case LOREG:
		if (p->n_left->n_op == UMUL && chkoreg(p->n_left->n_left))
			left = 0;
		else if (chkoreg(p->n_left))
			left = 0;
		else
			left = nsucomp(p->n_left);
		break;	
	case LTEMP:
		cerror("sucomp LTEMP");
	default:
		if (optype(p->n_op) != LTYPE)
			walkf(p->n_left, setleaf);
		left = 0; 
	}
#endif
	UDEBUG(("node %p left %d right %d\n", p, left, right));

#ifdef ragge
	if (o == BITYPE) {
#else
	if ((p->n_su & RMASK) && (p->n_su & LMASK)) {
#endif
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
#ifdef ragge
	} else if (o != LTYPE) {
#else
	} else if ((p->n_su & RMASK) || (p->n_su & LMASK)) {
#endif
		/* One child */
		need = MAX(right, left) + nreg;
	} else
		need = nreg;

	if (p->n_op == TEMP)
		(void)newblock(p);

	if (TCLASS(p->n_su) == 0 && nxreg == 0) {
		UDEBUG(("node %p no class\n", p));
		p->n_regw = NULL; /* may be set earlier */
		return need;
	}

#define	ADCL(n, cl)	\
	for (i = 0; i < n; i++, w++) {	w->r_class = cl; \
		DLIST_INSERT_BEFORE(&initial, w, link);  SETNUM(w); \
		UDEBUG(("Adding " #n " %d\n", w->nodnum)); \
	}

	UDEBUG(("node %p numregs %d\n", p, nxreg+1));
	w = p->n_regw = tmpalloc(sizeof(REGW) * (nxreg+1));
	memset(w, 0, sizeof(REGW) * (nxreg+1));

	w->r_class = TCLASS(p->n_su);
	if (w->r_class == 0)
		w->r_class = gclass(p->n_type);
	SETNUM(w);
	if (w->r_class)
		DLIST_INSERT_BEFORE(&initial, w, link);
	UDEBUG(("Adding short %d calss %d\n", w->nodnum, w->r_class));
	w++;
	ADCL(nareg, CLASSA);
	ADCL(nbreg, CLASSB);
	ADCL(ncreg, CLASSC);
	ADCL(ndreg, CLASSD);

	if (q->rewrite & RESC1) {
		w = p->n_regw + 1;
		w->r_class = -1;
		DLIST_REMOVE(w,link);
	} else if (q->rewrite & RESC2) {
		w = p->n_regw + 2;
		w->r_class = -1;
		DLIST_REMOVE(w,link);
	} else if (q->rewrite & RESC3) {
		w = p->n_regw + 3;
		w->r_class = -1;
		DLIST_REMOVE(w,link);
	}

	UDEBUG(("node %p return regs %d\n", p, need));

	return need;
}

#define	CLASS(x)	(x)->r_class
#define	NCLASS(x,c)	(x)->r_nclass[c]
#define	ADJLIST(x)	(x)->r_adjList
#define	ALIAS(x)	(x)->r_alias
#define	ONLIST(x)	(x)->r_onlist
#define	MOVELIST(x)	(x)->r_moveList
#define	COLOR(x)	(x)->r_color

static bittype *live;

#define	PUSHWLIST(w, l)	DLIST_INSERT_AFTER(&l, w, link); w->r_onlist = &l
#define	POPWLIST(l)	popwlist(&l);
#define	DELWLIST(w)	DLIST_REMOVE(w, link)
#define WLISTEMPTY(h)	DLIST_ISEMPTY(&h,link)
#define	PUSHMLIST(w, l, q)	DLIST_INSERT_AFTER(&l, w, link); w->queue = q
#define	POPMLIST(l)	popmlist(&l);

#define	trivially_colorable(x) \
	trivially_colorable_p((x)->r_class, (x)->r_nclass)
/*
 * Determine if a node is trivially colorable ("degree < K").
 * This implementation is a dumb one, without considering speed.
 */
static int
trivially_colorable_p(int c, int *n)
{
	int r[NUMCLASS+1];
	int i;

	for (i = 1; i < NUMCLASS+1; i++)
		r[i] = n[i] < regK[i] ? n[i] : regK[i];

#if 0
	/* add the exclusion nodes. */
	/* XXX can we do someything smart here? */
	/* worst-case for exclusion nodes are better than the `worst-case' */
	for (; excl; excl >>= 1)
		if (excl & 1)
			r[c]++;
#endif

	i = COLORMAP(c, r);
if (i < 0 || i > 1)
	comperr("trivially_colorable_p");
//printf("trivially_colorable_p: n[1] %d n[2] %d n[3] %d n[4] %d class %d, triv %d\n", n[1], n[2], n[3], n[4], c, i);
	return i;
}

static int
ncnt(int needs)
{
	int i = 0;

	while (needs & NACOUNT)
		needs -= NAREG, i++;
	while (needs & NBCOUNT)
		needs -= NBREG, i++;
	while (needs & NCCOUNT)
		needs -= NCREG, i++;
	while (needs & NDCOUNT)
		needs -= NDREG, i++;
	return i;
}

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
#define	GETP(p)		((p)->n_su == DOWNL ? getp(p) : p)
#define	GETRALL(p)	(GETP(p)->n_regw)

static NODE *
getp(NODE *p)
{
	while (p->n_su == DOWNL)
		p = p->n_left;
	return p;
}

/*
 * About data structures used in liveness analysis:
 *
 * The temporaries generated in pass1 are numbered between tempmin and
 * tempmax.  Temporaries generated in pass2 are numbered above tempmax,
 * so they are sequentially numbered.
 *
 * Bitfields are used for liveness.  Bit arrays are allocated on the
 * heap for the "live" variable and on the stack for the in, out, gen
 * and kill variables. Therefore, for a temp number, the bit number must
 * be biased with tempmin.
 *
 * There may be an idea to use a different data structure to store 
 * pass2 allocated temporaries, because they are very sparse.
 */

#ifdef PCC_DEBUG
static void
LIVEADD(int x)
{
	RDEBUG(("Liveadd: %d\n", x));
	if (x < tempmin || x >= tempmax)
		comperr("LIVEADD: out of range");
	BITSET(live, (x-tempmin));
}
static void
LIVEDEL(int x)
{
	RDEBUG(("Livedel: %d\n", x));
	if (x < tempmin || x >= tempmax)
		comperr("LIVEDEL: out of range");
	BITCLEAR(live, (x-tempmin));
}
#else
#define LIVEADD(x) BITSET(live, (x-tempmin))
#define LIVEDEL(x) BITCLEAR(live, (x-tempmin))
#endif

static struct lives {
	DLIST_ENTRY(lives) link;
	REGW *var;
} lused, lunused;

static void
LIVEADDR(REGW *x)
{
	struct lives *l;

#ifdef PCC_DEBUG
	RDEBUG(("LIVEADDR: %d\n", x->nodnum));
	DLIST_FOREACH(l, &lused, link)
		if (l->var == x)
			return;
//			comperr("LIVEADDR: multiple %d", ASGNUM(x));
#endif
	if (!DLIST_ISEMPTY(&lunused, link)) {
		l = DLIST_NEXT(&lunused, link);
		DLIST_REMOVE(l, link);
	} else
		l = tmpalloc(sizeof(struct lives));

	l->var = x;
	DLIST_INSERT_AFTER(&lused, l, link);
}

static void
LIVEDELR(REGW *x)
{
	struct lives *l;

	RDEBUG(("LIVEDELR: %d\n", x->nodnum));
	DLIST_FOREACH(l, &lused, link) {
		if (l->var != x)
			continue;
		DLIST_REMOVE(l, link);
		DLIST_INSERT_AFTER(&lunused, l, link);
		return;
	}
//	comperr("LIVEDELR: %p not found", x);
}

#define	MOVELISTADD(t, p) movelistadd(t, p)
#define WORKLISTMOVEADD(s,d) worklistmoveadd(s,d)

static void
movelistadd(REGW *t, REGM *p)
{
	MOVL *w = tmpalloc(sizeof(MOVL));

	w->regm = p;
	w->next = t->r_moveList;
	t->r_moveList = w;
}

static REGM *
worklistmoveadd(REGW *src, REGW *dst)
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
	REGW *u, *v;
} *edgehash[256];

/* Check if a node pair is adjacent */
static int
adjSet(REGW *u, REGW *v)
{
	struct AdjSet *w;
	REGW *t;

	if (ONLIST(u) == &precolored) {
		ADJL *a = ADJLIST(v);
		/*
		 * Check if any of the registers that have edges against v
		 * alias to u.
		 */
		for (; a; a = a->r_next) {
			if (ONLIST(a->a_temp) != &precolored)
				continue;
			t = a->a_temp;
			if (interferes(t - ablock, u - ablock))
				return 1;
		}
	}
	if (u > v)
		t = v, v = u, u = t;
	w = edgehash[((int)u+(int)v) & 255];
	for (; w; w = w->next) {
		if (u == w->u && v == w->v)
			return 1;
	}
	return 0;
}

/* Add a pair to adjset.  No check for dups */
static void
adjSetadd(REGW *u, REGW *v)
{
	struct AdjSet *w;
	int x;
	REGW *t;

	if (u > v)
		t = v, v = u, u = t;
	x = ((int)u+(int)v) & 255;
	w = tmpalloc(sizeof(struct AdjSet));
	w->u = u, w->v = v;
	w->next = edgehash[x];
	edgehash[x] = w;
}

/*
 * Add an interference edge between two nodes.
 */
static void
AddEdge(REGW *u, REGW *v)
{
	ADJL *x;

	RRDEBUG(("AddEdge: u %d v %d\n", ASGNUM(u), ASGNUM(v)));

#ifdef PCC_DEBUG
#if 0
	if (ASGNUM(u) == 0)
		comperr("AddEdge 0");
#endif
	if (CLASS(u) == 0 || CLASS(v) == 0)
		comperr("AddEdge class == 0 (%d=%d, %d=%d)",
		    CLASS(u), ASGNUM(u), CLASS(v), ASGNUM(v));
#endif

	if (u == v)
		return;
	if (adjSet(u, v))
		return;

	adjSetadd(u, v);

#if 0
	if (ONLIST(u) == &precolored || ONLIST(v) == &precolored)
		comperr("precolored node in AddEdge");
#endif

	if (ONLIST(u) != &precolored) {
		x = tmpalloc(sizeof(ADJL));
		x->a_temp = v;
		x->r_next = u->r_adjList;
		u->r_adjList = x;
		NCLASS(u, CLASS(v))++;
	}

	if (ONLIST(v) != &precolored) {
		x = tmpalloc(sizeof(ADJL));
		x->a_temp = u;
		x->r_next = v->r_adjList;
		v->r_adjList = x;
		NCLASS(v, CLASS(u))++;
	}

#if 0
	RDEBUG(("AddEdge: u %d(d %d) v %d(d %d)\n", u, DEGREE(u), v, DEGREE(v)));
#endif
}

#if 0 /* handle this in AssignColors instead */
/*
 * Trick for precolored nodes: if the node is not in the same
 * class as the other node, get the aliases registers and add
 * edges for them instead.
 */
static void
AddEdgepre(REGW *u, REGW *pre)
{
	int i, mask, rn;

	rn = GCLASS(pre - ablock);
	RDEBUG(("AddEdgepre: u %d pre %d rn %d\n", ASGNUM(u), ASGNUM(pre), rn));
	mask = aliasmap(CLASS(u), pre - ablock, rn);
	for (i = 0; mask; i++) {
		if ((mask & (1 << i)) == 0)
			continue;
		AddEdge(u, &ablock[MKREGNO(i, CLASS(u))]);
		mask &= ~(1 << i);
	}
}
#endif

static int
MoveRelated(REGW *n)
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

	RDX(int s=0);
	RDX(int f=0);
	RDX(int d=0);

	DLIST_INIT(&precolored, link);
	DLIST_INIT(&simplifyWorklist, link);
	DLIST_INIT(&freezeWorklist, link);
	DLIST_INIT(&spillWorklist, link);
	DLIST_INIT(&spilledNodes, link);
	DLIST_INIT(&coalescedNodes, link);
	DLIST_INIT(&coloredNodes, link);
	DLIST_INIT(&selectStack, link);

	/*
	 * Remove all nodes from the initial list and put them on 
	 * one of the worklists.
	 */
	while (!DLIST_ISEMPTY(&initial, link)) {
		w = DLIST_NEXT(&initial, link);
		DLIST_REMOVE(w, link);
		if (!trivially_colorable(w)) {
			PUSHWLIST(w, spillWorklist);
			RDX(s++);
		} else if (MoveRelated(w)) {
			PUSHWLIST(w, freezeWorklist);
			RDX(f++);
		} else {
			PUSHWLIST(w, simplifyWorklist);
			RDX(d++);
		}
	}
	RDEBUG(("MkWorklist: spill %d freeze %d simplify %d\n", s,f,d));
}

static void
addalledges(REGW *e)
{
	int i, j, k;
	int nbits = xbits;
	struct lives *l;

	RDEBUG(("addalledges for %d\n", e->nodnum));

#ifndef oldregw
	if (e->r_class == -1)
		return; /* unused */
#endif
	if (ONLIST(e) != &precolored) {
		for (i = 0; ndontregs[i] >= 0; i++)
			AddEdge(e, &ablock[ndontregs[i]]);
	}

	/* First add to long-lived temps */
	RDEBUG(("addalledges longlived "));
	for (i = 0; i < nbits; i += NUMBITS) {
		if ((k = live[i/NUMBITS]) == 0)
			continue;
		while (k) {
			j = ffs(k)-1;
			AddEdge(&nblock[i+j+tempmin], e);
			RRDEBUG(("%d ", i+j+tempmin));
			k &= ~(1 << j);
		}
	}
	RDEBUG(("done\n"));
	/* short-lived temps */
	RDEBUG(("addalledges shortlived "));
	DLIST_FOREACH(l, &lused, link) {
		RRDEBUG(("%d ", ASGNUM(l->var)));
		AddEdge(l->var, e);
	}
	RDEBUG(("done\n"));
}

static void
moveadd(REGW *def, REGW *use)
{
	REGM *r;

	if (def == use)
		return; /* no move to itself XXX - ``shouldn't happen'' */
	RDEBUG(("moveadd: def %d use %d\n", ASGNUM(def), ASGNUM(use)));

	r = WORKLISTMOVEADD(use, def);
	MOVELISTADD(def, r);
	MOVELISTADD(use, r);
	addalledges(def);
}

static void insnwalk(NODE *p);

/*
 * Traverse down both legs of an instruction.
 */
static void
insnbitype(NODE *p)
{
	struct rspecial *rc;
	struct optab *q = &table[TBLIDX(p->n_su)];
	REGW *l, *r, *n = 0, *rr;
	int nres;

	/* Step 1 */
	if (q->visit & INREGS)
		addalledges(p->n_regw);
	rr = p->n_regw;
	if ((q->needs & NSPECIAL) && (nres = rspecial(q, NRES)) >= 0) {
		rr = &ablock[nres];
		moveadd(p->n_regw, rr);
	}
#ifdef oldregw
	  else if (q->rewrite & RESC1) {
		rr = p->n_regw+1;
		moveadd(p->n_regw, rr);
	}
#endif

	l = GETRALL(p->n_left);
	r = GETRALL(p->n_right);

	/* Step 2 */
	if (q->needs & ALLNEEDS && p->n_regw[1].r_class != -1) {
		n = p->n_regw+1;
		addalledges(n);
//		int i;
//		for (i = 0; i < ncnt(q->needs); i++)
//			addalledges(&p->n_regw[1+i]);

	/* Step 3 */
	/* needs */
		if ((q->needs & NASL) == 0) /* XXX fix */
			AddEdge(l, n);
		if ((q->needs & NASR) == 0)
			AddEdge(r, n);
	}

	/* special needs */
	if (q->needs & NSPECIAL) {
		for (rc = nspecial(q); rc->op; rc++) {
			switch (rc->op) {
			case NLEFT: moveadd(l, &ablock[rc->num]); break;
			case NOLEFT: AddEdge(l, &ablock[rc->num]); break;
			case NRIGHT: moveadd(r, &ablock[rc->num]); break;
			case NORIGHT: AddEdge(r, &ablock[rc->num]); break;
			case NEVER: addalledges(&ablock[rc->num]); break;
			}
		}
	}

	/* leg rules */
	if (q->rewrite & RESC1) {
		AddEdge(l, r);
	} else if (q->rewrite & RLEFT) {
		AddEdge(r, rr);
		moveadd(l, rr);
	} else if (q->rewrite & RRIGHT) {
		AddEdge(l, rr);
		moveadd(r, rr);
	}

	/* step 4 */
	if ((p->n_su & DORIGHT) == 0) {
		LIVEADDR(l);
		insnwalk(p->n_right);
		LIVEDELR(l);
		insnwalk(p->n_left);
	} else {
		LIVEADDR(r);
		insnwalk(p->n_left);
		LIVEDELR(r);
		insnwalk(p->n_right);
	}
}

/*
 * only walk down one leg.
 */
static void
insntype(struct optab *q, NODE *p, int side)
{
	
	REGW *l, *n = 0, *rr, *regw;
	struct rspecial *rc;
	NODE *lr = (side == RRIGHT ? p->n_right : p->n_left);
	int nres;

	if (p->n_su == DOWNL)
		comperr("insntype == -1");

	regw = p->n_regw;

	/* Step 1 */
	if (q->visit & INREGS && regw)
		addalledges(regw);

	rr = regw;
	if ((q->needs & NSPECIAL) && (nres = rspecial(q, NRES)) >= 0) {
		rr = &ablock[nres];
		moveadd(regw, rr);
	}
#ifdef oldregw
	  else if (q->rewrite & RESC1) {
		rr = regw+1;
		moveadd(regw, rr);
	}
#endif
	l = GETRALL(lr);

	/* Step 2 */
	if ((q->needs & ALLNEEDS) && regw) {
		int cnt = ncnt(q->needs);
		int need = 0, i;

		for (i = 0; i < cnt; i++) {
			n = &regw[1+i];
			if (n->r_class == -1)
				continue;
			addalledges(n);
			if (side == RLEFT) {
				switch (CLASS(n)) {
				case CLASSA: need = NASL; break;
				case CLASSB: need = NBSL; break;
				case CLASSC: need = NCSL; break;
				case CLASSD: need = NDSL; break;
				}
			} else {
				switch (CLASS(n)) {
				case CLASSA: need = NASR; break;
				case CLASSB: need = NBSR; break;
				case CLASSC: need = NCSR; break;
				case CLASSD: need = NDSR; break;
				}
			}
			if ((q->needs & need) == 0)
				AddEdge(l, n);
		}
	}

	/* special needs */
	if (q->needs & NSPECIAL) {
		for (rc = nspecial(q); rc->op; rc++) {
			switch (rc->op) {
			case NRIGHT:
			case NLEFT: moveadd(l, &ablock[rc->num]); break;
			case NORIGHT:
			case NOLEFT: AddEdge(l, &ablock[rc->num]); break;
			case NEVER: addalledges(&ablock[rc->num]); break;
			}
		}
	}

	if (q->rewrite & side)
		moveadd(l, rr);

	/* step 4 */
	insnwalk(lr);
}

static void
templeaves(NODE *p)
{
	REGW *r;

	if (optype(p->n_op) != LTYPE)
		templeaves(p->n_left);
	if (optype(p->n_op) == BITYPE)
		templeaves(p->n_right);

	/* REGs? OREGs? */

	if (p->n_op != TEMP)
		return;
	r = newblock(p);
	addalledges(r);
	LIVEADD((int)p->n_lval);
}
/*
 * Handle leaf insn.
 */
static void 
insnleaf(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	REGW *rr, *r;
	struct rspecial *rc;
	int nres;

	/* Step 1 */
	if (q->visit & INREGS && p->n_regw)
		addalledges(p->n_regw);
	rr = p->n_regw;

	if (p->n_op == TEMP && rr == nblock + p->n_lval) {
		LIVEADD((int)p->n_lval);
		return;
	}

	if ((q->needs & NSPECIAL) && (nres = rspecial(q, NRES)) >= 0) {
		rr = &ablock[nres];
		moveadd(p->n_regw, rr);
	}
#ifdef oldregw
	 else if (q->rewrite & RESC1) {
		rr = p->n_regw+1;
		moveadd(p->n_regw, rr);
	}
#endif

	/* Step 2 */
	if (q->needs & ALLNEEDS && p->n_regw[1].r_class != -1)
		addalledges(p->n_regw+1);

	if (q->needs & NSPECIAL)
		for (rc = nspecial(q); rc->op; rc++)
			if (rc->op == NEVER)
				addalledges(&ablock[rc->num]);

	if (p->n_op == TEMP) {
		r = newblock(p);
		moveadd(r, rr);
		LIVEADD((int)p->n_lval);
	} else
		templeaves(p);
}

/*
 * Traverse arguments backwards.
 */
static void
argswalk(NODE *p)
{
	if (p->n_op == CM) {
		argswalk(p->n_left);
		insnwalk(p->n_right);
	} else
		insnwalk(p);
}

/*
 * Walk down a tree and do liveness analysis.
 */
static void
insnwalk(NODE *p)
{
	struct optab *q;
	int i, su;

	RDEBUG(("insnwalk: %p\n", p));

	if (p->n_su == DOWNL)
		return insnwalk(p->n_left);

	q = &table[TBLIDX(p->n_su)];
	su = p->n_su & (LMASK|RMASK);
	if (p->n_op == ASSIGN) {
		REGW *rr = q->visit & INREGS ? p->n_regw : NULL;

		if (p->n_left->n_op == TEMP) {
			REGW *nb = newblock(p->n_left);
			LIVEDEL((int)p->n_left->n_lval);
			if (rr)
				moveadd(nb, rr);
		}
		if (((su & RMASK) == RREG) || p->n_right->n_op == TEMP)
			if (rr)
				moveadd(GETRALL(p->n_right), rr);
	} else if (callop(p->n_op)) {
		/* first add all edges */
		for (i = 0; tempregs[i] >= 0; i++)
			addalledges(&ablock[i]);
		if (p->n_regw)
			moveadd(p->n_regw, &ablock[RETREG(p->n_type)]);
		/* XXX - here must all live arg registers be added 
		 * for archs with arguments in registers */
	}

	if ((su & LMASK) && (su & RMASK))
		insnbitype(p);
#if 0
	else if ((su & LMASK) == LOREG)
		insntype(q, NULL, p->n_left, RLEFT);
	else if (su & LMASK)
		insntype(q, p->n_regw, p->n_left, RLEFT);
	else if ((su & RMASK) == ROREG)
		insntype(q, NULL, p->n_right, RRIGHT);
	else if (su & RMASK)
		insntype(q, p->n_regw, p->n_right, RRIGHT);
#else
	else if (su & LMASK)
		insntype(q, p, RLEFT);
	else if (su & RMASK)
		insntype(q, p, RRIGHT);
#endif
	else if (su == 0)
		insnleaf(p);
	else
		comperr("insnwalk");

	/* Do liveness analysis on argument expressions (backwards) */
	if (p->n_op == CALL || p->n_op == STCALL || p->n_op == FORTCALL)
		argswalk(p->n_right);
}

static bittype **gen, **kill, **in, **out;

static void
unionize(NODE *p, int bb)
{
	int i, o, ty;

	if ((o = p->n_op) == TEMP) {
#ifdef notyet
		for (i = 0; i < szty(p->n_type); i++) {
			BITSET(gen[bb], ((int)p->n_lval - tempmin+i));
		}
#else
		i = 0;
		BITSET(gen[bb], ((int)p->n_lval - tempmin+i));
#endif
	}
	if (o == ASSIGN && p->n_left->n_op == TEMP) {
		int b = p->n_left->n_lval - tempmin;
#ifdef notyet
		for (i = 0; i < szty(p->n_type); i++) {
			BITCLEAR(gen[bb], (b+i));
			BITSET(kill[bb], (b+i));
		}
#else
		i = 0;
		BITCLEAR(gen[bb], (b+i));
		BITSET(kill[bb], (b+i));
#endif
		unionize(p->n_right, bb);
		return;
	}
	ty = optype(o);
	if (ty != LTYPE)
		unionize(p->n_left, bb);
	if (ty == BITYPE)
		unionize(p->n_right, bb);
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
	int i, bbnum;

	/*
	 * generate the gen-kill sets for all basic blocks.
	 */
	DLIST_FOREACH(bb, &bblocks, bbelem) {
		bbnum = bb->bbnum;
		for (ip = bb->last; ; ip = DLIST_PREV(ip, qelem)) {
			/* gen/kill is 'p', this node is 'n' */
			if (ip->type == IP_NODE)
				unionize(ip->ip_node, bbnum);
			if (ip == bb->first)
				break;
		}
		memcpy(in[bbnum], gen[bbnum], BIT2BYTE(tempmax-tempmin));
#ifdef PCC_DEBUG
		if (rdebug) {
			printf("basic block %d\ngen: ", bbnum);
			for (i = 0; i < tempmax-tempmin; i++)
				if (TESTBIT(gen[bbnum], i))
					printf("%d ", i+tempmin);
			printf("\nkill: ");
			for (i = 0; i < tempmax-tempmin; i++)
				if (TESTBIT(kill[bbnum], i))
					printf("%d ", i+tempmin);
			printf("\n");
		}
#endif
	}
}

#define	SETCOPY(t,f,i,n) for (i = 0; i < n/NUMBITS; i++) t[i] = f[i]
#define	SETSET(t,f,i,n) for (i = 0; i < n/NUMBITS; i++) t[i] |= f[i]
#define	SETCLEAR(t,f,i,n) for (i = 0; i < n/NUMBITS; i++) t[i] &= ~f[i]
#define	SETCMP(v,t,f,i,n) for (i = 0; i < n/NUMBITS; i++) \
	if (t[i] != f[i]) v = 1

/*
 * Build the set of interference edges and adjacency list.
 */
static void
Build(struct interpass *ipole)
{
	extern struct basicblock bblocks;
	struct basicblock bbfake;
	struct interpass *ip;
	struct basicblock *bb;
	struct cfgnode *cn;
	extern int nbblocks;
	bittype *saved;
	int i, j, again, nbits;

	if (xtemps == 0) {
		/*
		 * No basic block splitup is done if not optimizing,
		 * so fake one basic block to keep the liveness analysis 
		 * happy.
		 */
		nbblocks = 1;
		bbfake.bbnum = 0;
		bbfake.last = DLIST_PREV(ipole, qelem);
		bbfake.first = DLIST_NEXT(ipole, qelem);
		DLIST_INIT(&bblocks, bbelem);
		DLIST_INSERT_AFTER(&bblocks, &bbfake, bbelem);
		SLIST_INIT(&bbfake.children);
	}

	/* Just fetch space for the temporaries from stack */
	nbits = xbits+(NUMBITS-1);
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

	/* register variable temporaries are live */
	for (i = 0; i < NPERMREG-1; i++) {
		if (nsavregs[i])
			continue;
		BITSET(out[nbblocks-1], i);
		for (j = i+1; j < NPERMREG-1; j++) {
			if (nsavregs[j])
				continue;
			AddEdge(&nblock[i+tempmin], &nblock[j+tempmin]);
		}
	}

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
			for (i = 0; i < tempmax-tempmin; i++)
				if (TESTBIT(in[bb->bbnum], i))
					printf("%d ", i+tempmin);
			printf("\nout: ");
			for (i = 0; i < tempmax-tempmin; i++)
				if (TESTBIT(out[bb->bbnum], i))
					printf("%d ", i+tempmin);
			printf("\n");
		}
	}
#endif

	DLIST_FOREACH(bb, &bblocks, bbelem) {
		RDEBUG(("liveadd bb %d\n", bb->bbnum));
		i = bb->bbnum;
		for (j = 0; j < (tempmax-tempmin); j += NUMBITS)
			live[j/NUMBITS] = 0;
		SETCOPY(live, out[i], j, nbits);
		for (ip = bb->last; ; ip = DLIST_PREV(ip, qelem)) {
			if (ip->type == IP_NODE)
				insnwalk(ip->ip_node);
			if (ip == bb->first)
				break;
		}
	}

#ifdef PCC_DEBUG
	if (rdebug) {
		int i;
		struct AdjSet *w;
		ADJL *x;
		REGW *y;
		MOVL *m;

		printf("Interference edges\n");
		for (i = 0; i < 256; i++) {
			if ((w = edgehash[i]) == NULL)
				continue;
			for (; w; w = w->next)
				printf("%d <-> %d\n", ASGNUM(w->u), ASGNUM(w->v));
		}
		printf("Degrees\n");
		DLIST_FOREACH(y, &initial, link) {
			printf("%d (%c): trivial [%d] ", ASGNUM(y),
			    CLASS(y)+'@', trivially_colorable(y));
			for (x = ADJLIST(y); x; x = x->r_next) {
				if (ONLIST(x->a_temp) != &selectStack &&
				    ONLIST(x->a_temp) != &coalescedNodes)
					printf("%d ", ASGNUM(x->a_temp));
				else
					printf("(%d) ", ASGNUM(x->a_temp));
			}
			printf("\n");
		}
		printf("Move nodes\n");
		DLIST_FOREACH(y, &initial, link) {
			if (MOVELIST(y) == NULL)
				continue;
			printf("%d: ", ASGNUM(y));
			for (m = MOVELIST(y); m; m = m->next) {
				REGW *yy = m->regm->src == y ?
				    m->regm->dst : m->regm->src;
				printf("%d ", ASGNUM(yy));
			}
			printf("\n");
		}
	}
#endif

}

static void
EnableMoves(REGW *n)
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
EnableAdjMoves(REGW *nodes)
{
	ADJL *w;
	REGW *n;

	EnableMoves(nodes);
	for (w = ADJLIST(nodes); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
		EnableMoves(w->a_temp);
	}
}

/*
 * Decrement the degree of node w for class c.
 */
static void
DecrementDegree(REGW *w, int c)
{
	int wast;

	RRDEBUG(("DecrementDegree: w %d, c %d\n", ASGNUM(w), c));

	wast = trivially_colorable(w);
	NCLASS(w, c)--;
	if (wast == trivially_colorable(w))
		return;

	EnableAdjMoves(w);
	DELWLIST(w);
	ONLIST(w) = 0;
	if (MoveRelated(w)) {
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

	w = POPWLIST(simplifyWorklist);
	PUSHWLIST(w, selectStack);
	RDEBUG(("Simplify: node %d class %d\n", ASGNUM(w), w->r_class));

	l = w->r_adjList;
	for (; l; l = l->r_next) {
		if (ONLIST(l->a_temp) == &selectStack ||
		    ONLIST(l->a_temp) == &coalescedNodes)
			continue;
		DecrementDegree(l->a_temp, w->r_class);
	}
}

static REGW *
GetAlias(REGW *n)
{
	if (ONLIST(n) == &coalescedNodes)
		return GetAlias(ALIAS(n));
	return n;
}

static int
OK(REGW *t, REGW *r)
{
	RDEBUG(("OK: t %d CLASS(t) %d adjSet(%d,%d)=%d\n",
	    ASGNUM(t), CLASS(t), ASGNUM(t), ASGNUM(r), adjSet(t, r)));

#ifdef PCC_DEBUG
	if (rdebug > 1) {
		ADJL *w;
		int ndeg = 0;
		printf("OK degree: ");
		for (w = ADJLIST(t); w; w = w->r_next) {
			if (ONLIST(w->a_temp) != &selectStack &&
			    ONLIST(w->a_temp) != &coalescedNodes)
				printf("%c%d ", CLASS(w->a_temp)+'@',
				    ASGNUM(w->a_temp)), ndeg++;
			else
				printf("(%d) ", ASGNUM(w->a_temp));
		}
		printf("\n");
#if 0
		if (ndeg != DEGREE(t) && DEGREE(t) >= 0)
			printf("!!!ndeg %d != DEGREE(t) %d\n", ndeg, DEGREE(t));
#endif
	}
#endif

	if (trivially_colorable(t) || ONLIST(t) == &precolored || 
	    (adjSet(t, r) || !aliasmap(CLASS(t), COLOR(r))))/* XXX - check aliasmap */
		return 1;
	return 0;
}

static int
adjok(REGW *v, REGW *u)
{
	ADJL *w;
	REGW *t;

	RDEBUG(("adjok\n"));
	for (w = ADJLIST(v); w; w = w->r_next) {
		t = w->a_temp;
		if (ONLIST(t) == &selectStack || ONLIST(t) == &coalescedNodes)
			continue;
		if (OK(t, u) == 0)
			return 0;
	}
	RDEBUG(("adjok returns OK\n"));
	return 1;
}

/*
 * Do a conservative estimation of whether two temporaries can 
 * be coalesced.  This is "Briggs-style" check.
 * Neither u nor v is precolored when called.
 */
static int
Conservative(REGW *u, REGW *v)
{
	ADJL *w, *ww;
	REGW *n;
	int i, ncl[NUMCLASS+1];

	if (CLASS(u) != CLASS(v))
		comperr("Conservative: u(%d = %d), v(%d = %d)",
		    ASGNUM(u), CLASS(u), ASGNUM(v), CLASS(v));

	for (i = 0; i < NUMCLASS+1; i++)
		ncl[i] = 0;

	RDEBUG(("Conservative (%d,%d)\n", ASGNUM(u), ASGNUM(v)));

	for (w = ADJLIST(u); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
		for (ww = ADJLIST(v); ww; ww = ww->r_next)
			if (ww->a_temp == n)
				break;
		if (ww)
			continue;
		if (!trivially_colorable(n))
			ncl[CLASS(n)]++;
	}
	for (w = ADJLIST(v); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
		if (!trivially_colorable(n))
			ncl[CLASS(n)]++;
	}
	i = trivially_colorable_p(CLASS(u), ncl);
	RDEBUG(("Conservative i=%d\n", i));
	return i;
}

static void
AddWorkList(REGW *w)
{

	if (ONLIST(w) != &precolored && !MoveRelated(w) &&
	    trivially_colorable(w)) {
		DELWLIST(w);
		PUSHWLIST(w, simplifyWorklist);
	}
}

static void
Combine(REGW *u, REGW *v)
{
	MOVL *m;
	ADJL *l;
	REGW *t;

	RDEBUG(("Combine (%d,%d)\n", ASGNUM(u), ASGNUM(v)));

	if (ONLIST(v) == &freezeWorklist) {
		DELWLIST(v);
	} else {
		DELWLIST(v);
	}
	PUSHWLIST(v, coalescedNodes);
	ALIAS(v) = u;
	if (rdebug) { 
		printf("adjlist(%d): ", ASGNUM(v));
		for (l = ADJLIST(v); l; l = l->r_next)
			printf("%d ", l->a_temp->nodnum);
		printf("\n");
	}
#if 1
{
	MOVL *m0 = MOVELIST(v);

	for (m0 = MOVELIST(v); m0; m0 = m0->next) {
		for (m = MOVELIST(u); m; m = m->next)
			if (m->regm == m0->regm)
				break; /* Already on list */
		if (m)
			continue; /* already on list */
		MOVELISTADD(u, m0->regm);
	}
}
#else

	if ((m = MOVELIST(u))) {
		while (m->next)
			m = m->next;
		m->next = MOVELIST(v);
	} else
		MOVELIST(u) = MOVELIST(v);
#endif
	EnableMoves(v);
	for (l = ADJLIST(v); l; l = l->r_next) {
		t = l->a_temp;
		if (ONLIST(t) == &selectStack || ONLIST(t) == &coalescedNodes)
			continue;
		/* Do not add edge if u cannot affect the colorability of t */
		/* XXX - check aliasmap */
		if (ONLIST(u) != &precolored || aliasmap(CLASS(t), COLOR(u)))
			AddEdge(t, u);
		DecrementDegree(t, CLASS(v));
	}
	if (!trivially_colorable(u) && ONLIST(u) == &freezeWorklist) {
		DELWLIST(u);
		PUSHWLIST(u, spillWorklist);
	}
if (rdebug) {
	ADJL *w;
	printf("Combine %d class (%d): ", ASGNUM(u), CLASS(u));
	for (w = ADJLIST(u); w; w = w->r_next) {
		if (ONLIST(w->a_temp) != &selectStack &&
		    ONLIST(w->a_temp) != &coalescedNodes)
			printf("%d ", ASGNUM(w->a_temp));
		else
			printf("(%d) ", ASGNUM(w->a_temp));
	}
	printf("\n");
}
}

static void
Coalesce(void)
{
	REGM *m;
	REGW *x, *y, *u, *v;

	m = POPMLIST(worklistMoves);
	x = GetAlias(m->src);
	y = GetAlias(m->dst);

	if (ONLIST(y) == &precolored)
		u = y, v = x;
	else
		u = x, v = y;

	RDEBUG(("Coalesce: src %d dst %d u %d v %d x %d y %d\n",
	    ASGNUM(m->src), ASGNUM(m->dst), ASGNUM(u), ASGNUM(v),
	    ASGNUM(x), ASGNUM(y)));

	if (CLASS(m->src) != CLASS(m->dst))
		comperr("Coalesce: src class %d, dst class %d",
		    CLASS(m->src), CLASS(m->dst));

	if (u == v) {
		RDEBUG(("Coalesce: u == v\n"));
		PUSHMLIST(m, coalescedMoves, COAL);
		AddWorkList(u);
	} else if (ONLIST(v) == &precolored || adjSet(u, v)) {
		RDEBUG(("Coalesce: constrainedMoves\n"));
		PUSHMLIST(m, constrainedMoves, CONSTR);
		AddWorkList(u);
		AddWorkList(v);
	} else if ((ONLIST(u) == &precolored && adjok(v, u)) ||
	    (ONLIST(u) != &precolored && Conservative(u, v))) {
		RDEBUG(("Coalesce: Conservative\n"));
		PUSHMLIST(m, coalescedMoves, COAL);
		Combine(u, v);
		AddWorkList(u);
	} else {
		RDEBUG(("Coalesce: activeMoves\n"));
		PUSHMLIST(m, activeMoves, ACTIVE);
	}
}

static void
FreezeMoves(REGW *u)
{
	MOVL *w, *o;
	REGM *m;
	REGW *z;
	REGW *x, *y, *v;

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
		RDEBUG(("FreezeMoves: u %d (%d,%d) v %d\n",
		    ASGNUM(u),ASGNUM(x),ASGNUM(y),ASGNUM(v)));
		DLIST_REMOVE(m, link);
		PUSHMLIST(m, frozenMoves, FROZEN);
		if (ONLIST(v) != &freezeWorklist)
			continue;
		for (o = MOVELIST(v); o; o = o->next)
			if (o->regm->queue == WLIST || o->regm->queue == ACTIVE)
				break;
		if (o == NULL) {
			z = v;
			DELWLIST(z);
			PUSHWLIST(z, simplifyWorklist);
		}
	}
}

static void
Freeze(void)
{
	REGW *u;

	/* XXX
	 * Should check if the moves to freeze have exactly the same 
	 * interference edges.  If they do, coalesce them instead, it
	 * may free up other nodes that they interfere with.
	 */
	u = POPWLIST(freezeWorklist);
	PUSHWLIST(u, simplifyWorklist);
	RDEBUG(("Freeze %d\n", ASGNUM(u)));
	FreezeMoves(u);
}

static void
SelectSpill(void)
{
	REGW *w;

	RDEBUG(("SelectSpill\n"));
	if (rdebug)
		DLIST_FOREACH(w, &spillWorklist, link)
			printf("SelectSpill: %d\n", ASGNUM(w));

	/* First check if we can spill register variables */
	DLIST_FOREACH(w, &spillWorklist, link) {
		if (w >= &nblock[tempmin] && w < &nblock[basetemp])
			break;
	}

	if (w == &spillWorklist) {
		/* try to find another long-range variable */
		DLIST_FOREACH(w, &spillWorklist, link) {
			if (w >= &nblock[tempmin] && w < &nblock[tempmax])
				break;
		}
	}

	if (w == &spillWorklist) {
		/* no heuristics, just fetch first element */
		w = DLIST_NEXT(&spillWorklist, link);
	}
 
        DLIST_REMOVE(w, link);

	PUSHWLIST(w, simplifyWorklist);
	RDEBUG(("Freezing node %d\n", ASGNUM(w)));
	FreezeMoves(w);
}

int gregn(REGW *);

int
gregn(REGW *w)
{
	return w->nodnum;
}

#if 0
void setclass(int tmp, int class);
int getclass(int tmp);
void
setclass(int tmp, int class)
{
	if (tmp < tempmin || tmp >= tempmax)
		comperr("setclass");
	if (nblock[tmp].r_class)
		return;
	nblock[tmp].r_class = class;
}

int
getclass(int tmp)
{
	return nblock[tmp].r_class;
}
#endif

/*
 * Set class on long-lived temporaries based on its type.
 */
static void
traclass(NODE *p)
{
	REGW *nb;

	if (p->n_op != TEMP)
		return;

	nb = &nblock[(int)p->n_lval];
	if (CLASS(nb) == 0)
		CLASS(nb) = gclass(p->n_type);
}

static void
paint(NODE *p)
{
	struct optab *q;
	REGW *w, *ww;
	int i;

	if (p->n_su == DOWNL)
		return;

	if (p->n_regw != NULL) {
		/* Must color all allocated regs also */
		ww = w = p->n_regw;
		q = &table[TBLIDX(p->n_su)];
		p->n_reg = COLOR(w);
		w++;
		if (q->needs & ALLNEEDS)
			for (i = 0; i < ncnt(q->needs); i++) {
				if (w->r_class == -1)
					p->n_reg |= ENCRA(COLOR(ww), i);
				else
					p->n_reg |= ENCRA(COLOR(w), i);
				w++;
			}
	}
	if (p->n_op == TEMP) {
		REGW *nb = &nblock[(int)p->n_lval];
		p->n_rval = COLOR(nb);
		if (TCLASS(p->n_su) == 0)
			SCLASS(p->n_su, CLASS(nb));
		p->n_op = REG;
		p->n_lval = 0;
	}
}

static void
AssignColors(struct interpass *ip)
{
	int okColors, c;
	REGW *o, *w;
	ADJL *x;

	RDEBUG(("AssignColors\n"));
	while (!WLISTEMPTY(selectStack)) {
		w = POPWLIST(selectStack);
		okColors = classmask(CLASS(w));
		RDEBUG(("classmask av %d, class %d: %x\n",
		    w->nodnum, CLASS(w), okColors));

		for (x = ADJLIST(w); x; x = x->r_next) {
			o = GetAlias(x->a_temp);
			RRDEBUG(("Adj(%d): %d (%d)\n",
			    ASGNUM(w), ASGNUM(o), ASGNUM(x->a_temp)));

			if (ONLIST(o) == &coloredNodes ||
			    ONLIST(o) == &precolored) {
				c = aliasmap(CLASS(w), COLOR(o));
				RRDEBUG(("aliasmap in class %d by color %d: "
				    "%x, okColors %x\n",
				    CLASS(w), COLOR(o), c, okColors));

				okColors &= ~c;
			}
		}
		if (okColors == 0) {
			PUSHWLIST(w, spilledNodes);
			RDEBUG(("Spilling node %d\n", ASGNUM(w)));
		} else {
			PUSHWLIST(w, coloredNodes);
			c = ffs(okColors)-1;
			COLOR(w) = color2reg(c, CLASS(w));
			RDEBUG(("Coloring %d with %s\n",
			    ASGNUM(w), rnames[COLOR(w)]));
		}
	}
	DLIST_FOREACH(w, &coalescedNodes, link) {
		REGW *ww = GetAlias(w);
		COLOR(w) = COLOR(ww);
		if (ONLIST(ww) == &spilledNodes) {
			RDEBUG(("coalesced node %d spilled\n", w->nodnum));
			ww = DLIST_PREV(w, link);
			DLIST_REMOVE(w, link);
			PUSHWLIST(w, spilledNodes);
			w = ww;
		} else
			RDEBUG(("Giving coalesced node %d color %s\n",
			    w->nodnum, rnames[COLOR(w)]));
	}

	if (rdebug)
		DLIST_FOREACH(w, &coloredNodes, link)
			printf("%d: color %s\n", ASGNUM(w), rnames[COLOR(w)]);
	if (DLIST_ISEMPTY(&spilledNodes, link)) {
		struct interpass *ip2;
		DLIST_FOREACH(ip2, ip, qelem)
			if (ip2->type == IP_NODE)
				walkf(ip2->ip_node, paint);
	}
}

static REGW *spole;
/*
 * Store all spilled nodes in memory by fetching a temporary on the stack.
 * Will never end up here if not optimizing.
 */
static void
longtemp(NODE *p)
{
	REGW *w;

	if (p->n_op != TEMP)
		return;
	/* XXX - should have a bitmask to find temps to convert */
	DLIST_FOREACH(w, spole, link) {
		if (w != &nblock[(int)p->n_lval])
			continue;
		if (w->r_class == 0) {
			w->r_color = BITOOR(freetemp(szty(p->n_type)));
			w->r_class = 1;
		}
		p->n_op = OREG;
		p->n_lval = w->r_color;
		p->n_rval = FPREG;
		p->n_regw = NULL;
		break;
	}
}

static struct interpass *cip;
/*
 * Rewrite a tree by storing a variable in memory.
 * XXX - must check if basic block structure is destroyed!
 */
static void
shorttemp(NODE *p)
{
	struct interpass *nip;
	REGW *w;
	NODE *l, *r;
	int off;

	/* XXX - optimize this somewhat */
	DLIST_FOREACH(w, spole, link) {
		if (w != p->n_regw)
			continue;
		RDEBUG(("rewriting node %d\n", ASGNUM(w)));
		off = BITOOR(freetemp(szty(p->n_type)));
		l = mklnode(OREG, off, FPREG, p->n_type);
		r = talloc();
		*r = *p;
		nip = ipnode(mkbinode(ASSIGN, l, r, p->n_type));
		*p = *l;
		DLIST_INSERT_BEFORE(cip, nip, qelem);
		DLIST_REMOVE(w, link);
		break;
	}
}

/*
 * Change the TEMPs in the ipole list to stack variables.
 */
static void
treerewrite(struct interpass *ipole, REGW *rpole)
{
	struct interpass *ip;

	spole = rpole;

	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		cip = ip;
		walkf(ip->ip_node, shorttemp);	/* convert temps to oregs */
	}
	if (!DLIST_ISEMPTY(spole, link))
		comperr("treerewrite not empty");
}

/*
 * Change the TEMPs in the ipole list to stack variables.
 */
static void
leafrewrite(struct interpass *ipole, REGW *rpole)
{
	extern NODE *nodepole;
	extern int thisline;
	struct interpass *ip;

	spole = rpole;
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		nodepole = ip->ip_node;
		thisline = ip->lineno;
		walkf(ip->ip_node, longtemp);	/* convert temps to oregs */
	}
	nodepole = NIL;
}

#define	ONLYPERM 1
#define	LEAVES	 2
#define	SMALL	 3

/*
 * Scan the whole function and search for temporaries to be stored
 * on-stack.
 *
 * Be careful to not destroy the basic block structure in the first scan.
 */
static int
RewriteProgram(struct interpass *ip)
{
	REGW shortregs, longregs, saveregs, *q;
	REGW *w;
	int rwtyp;

	RDEBUG(("RewriteProgram\n"));
	DLIST_INIT(&shortregs, link);
	DLIST_INIT(&longregs, link);
	DLIST_INIT(&saveregs, link);

	/* sort the temporaries in three queues, short, long and perm */
	while (!DLIST_ISEMPTY(&spilledNodes, link)) {
		w = DLIST_NEXT(&spilledNodes, link);
		DLIST_REMOVE(w, link);

		if (w >= &nblock[tempmin] && w < &nblock[basetemp]) {
			q = &saveregs;
		} else if (w >= &nblock[basetemp] && w < &nblock[tempmax]) {
			q = &longregs;
		} else
			q = &shortregs;
		DLIST_INSERT_AFTER(q, w, link);
	}
#ifdef PCC_DEBUG
	if (rdebug) {
		printf("permanent: ");
		DLIST_FOREACH(w, &saveregs, link)
			printf("%d ", ASGNUM(w));
		printf("\nlong-lived: ");
		DLIST_FOREACH(w, &longregs, link)
			printf("%d ", ASGNUM(w));
		printf("\nshort-lived: ");
		DLIST_FOREACH(w, &shortregs, link)
			printf("%d ", ASGNUM(w));
		printf("\n");
	}
#endif
	rwtyp = 0;

	if (!DLIST_ISEMPTY(&saveregs, link)) {
		rwtyp = ONLYPERM;
		DLIST_FOREACH(w, &saveregs, link) {
			int num = w - nblock - tempmin;
			nsavregs[num] = 1;
		}
	}
	if (!DLIST_ISEMPTY(&longregs, link)) {
		rwtyp = LEAVES;
		DLIST_FOREACH(w, &longregs, link)
			w->r_class = 0; /* no stack space yet allocated */
	}

	if (rwtyp == LEAVES) {
		leafrewrite(ip, &longregs);
		rwtyp = ONLYPERM;
	}

	if (rwtyp == 0 && !DLIST_ISEMPTY(&shortregs, link)) {
		/* Must rewrite the trees */
		treerewrite(ip, &shortregs);
//		if (xtemps)
//			comperr("treerewrite");
		rwtyp = SMALL;
	}

	RDEBUG(("savregs %x rwtyp %d\n", 0, rwtyp));

	return rwtyp;
}

#ifdef notyet
/*
 * Assign instructions, calculate evaluation order and
 * set temporary register numbers.
 */
static void
insgen()
{
	geninsn(); /* instruction assignment */
	sucomp();  /* set evaluation order */
	slong();   /* set long temp types */
	sshort();  /* set short temp numbers */
}
#endif

/*
 * Do register allocation for trees by graph-coloring.
 */
void
ngenregs(struct interpass *ipole)
{
	extern NODE *nodepole;
	struct interpass_prolog *ipp, *epp;
	struct interpass *ip;
	int i, j, nbits = 0;
	int uu[NPERMREG] = { -1 };
	int xnsavregs[NPERMREG];
	int beenhere = 0;

	DLIST_INIT(&lunused, link);
	DLIST_INIT(&lused, link);

	/*
	 * Do some setup before doing the real thing.
	 */
	ipp = (struct interpass_prolog *)DLIST_NEXT(ipole, qelem);
	epp = (struct interpass_prolog *)DLIST_PREV(ipole, qelem);

	tempmin = ipp->ip_tmpnum;
	tempmax = epp->ip_tmpnum;

	/*
	 * Allocate space for the permanent registers in the
	 * same block as the long-lived temporaries.
	 * These temporaries will be handled the same way as 
	 * all other variables.
	 */
	basetemp = tempmin;
	nsavregs = xnsavregs;
	for (i = 0; i < NPERMREG; i++)
		xnsavregs[i] = 0;
	ndontregs = uu; /* currently never avoid any regs */

	tempmin -= (NPERMREG-1);
#ifdef notyet
	if (xavoidfp)
		dontregs |= REGBIT(FPREG);
#endif

#ifdef PCC_DEBUG
	nodnum = tempmax;
#endif
	nbits = xbits = tempmax - tempmin;
	if (nbits) {
		nblock = tmpalloc(nbits * sizeof(REGW));

		nblock -= tempmin;
		live = tmpalloc(BIT2BYTE(nbits));
		RDEBUG(("nblock %p num %d size %d\n",
		    nblock, nbits, nbits * sizeof(REGW)));
	}


	/* Block for precolored nodes */
	ablock = tmpalloc(sizeof(REGW)*MAXREGS);
	memset(ablock, 0, sizeof(REGW)*MAXREGS);
	for (i = 0; i < MAXREGS; i++) {
		ablock[i].r_onlist = &precolored;
		ablock[i].r_class = GCLASS(i); /* XXX */
		ablock[i].r_color = i;
#ifdef PCC_DEBUG
		ablock[i].nodnum = i;
#endif
	}
#ifdef notyet
	TMPMARK();
#endif


recalc:
onlyperm: /* XXX - should not have to redo all */

	if (nbits) {
		memset(nblock+tempmin, 0, nbits * sizeof(REGW));
		memset(live, 0, BIT2BYTE(nbits));
		memset(edgehash, 0, sizeof(edgehash));
#ifdef PCC_DEBUG
		for (i = tempmin; i < tempmax; i++)
			nblock[i].nodnum = i;
#endif
	}
	RPRINTIP(ipole);
	DLIST_INIT(&initial, link);
	DLIST_FOREACH(ip, ipole, qelem) {
		extern int thisline;
		if (ip->type != IP_NODE)
			continue;
		nodepole = ip->ip_node;
		thisline = ip->lineno;
		geninsn(ip->ip_node, FOREFF);
		nsucomp(ip->ip_node);
		walkf(ip->ip_node, traclass);
	}
	nodepole = NIL;
	RDEBUG(("nsucomp allocated %d temps (%d,%d)\n", 
	    tempmax-tempmin, tempmin, tempmax));

	RPRINTIP(ipole);
	RDEBUG(("ngenregs: numtemps %d (%d, %d)\n", tempmax-tempmin,
		    tempmin, tempmax));

	DLIST_INIT(&coalescedMoves, link);
	DLIST_INIT(&constrainedMoves, link);
	DLIST_INIT(&frozenMoves, link);
	DLIST_INIT(&worklistMoves, link);
	DLIST_INIT(&activeMoves, link);

	/* Set class and move-related for perm regs */
	for (i = 0; i < (NPERMREG-1); i++) {
		if (nsavregs[i])
			continue;
		nblock[i+tempmin].r_class = GCLASS(permregs[i]);
		DLIST_INSERT_AFTER(&initial, &nblock[i+tempmin], link);
		moveadd(&nblock[i+tempmin], &ablock[permregs[i]]);
	}

	Build(ipole);
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
	AssignColors(ipole);

	RDEBUG(("After AssignColors\n"));
	RPRINTIP(ipole);

	if (!WLISTEMPTY(spilledNodes)) {
		switch (RewriteProgram(ipole)) {
		case ONLYPERM:
			goto onlyperm;
		case SMALL:
			optimize(ipole);
			if (beenhere++)
				comperr("beenhere");
			goto recalc;
		}
	}

	/* fill in regs to save */
	ipp->ipp_regs = 0;
	for (i = 0; i < NPERMREG-1; i++) {
		if (nsavregs[i]) {
			ipp->ipp_regs |= (1 << permregs[i]);
			continue; /* Spilled */
		}
		if (nblock[i+tempmin].r_color == permregs[i])
			continue; /* Coalesced */
		/*
		 * If the original color of this permreg is used for
		 * coloring another register, swap them to avoid
		 * unneccessary moves.
		 */
		for (j = i+1; j < NPERMREG-1; j++) {
			if (nblock[j+tempmin].r_color != permregs[i])
				continue;
			nblock[j+tempmin].r_color = nblock[i+tempmin].r_color;
			break;
		}
		if (j != NPERMREG-1)
			continue;

		/* Generate reg-reg move nodes for save */
		ip = ipnode(mkbinode(ASSIGN, 
		    mklnode(REG, 0, nblock[i+tempmin].r_color, INT),
		    mklnode(REG, 0, permregs[i], INT), INT));
		geninsn(ip->ip_node, FOREFF);
		DLIST_INSERT_AFTER(ipole->qelem.q_forw, ip, qelem);
			/* XXX not int */
		ip = ipnode(mkbinode(ASSIGN, mklnode(REG, 0, permregs[i], INT),
		    mklnode(REG, 0, nblock[i+tempmin].r_color, INT), INT));
		geninsn(ip->ip_node, FOREFF);
		DLIST_INSERT_BEFORE(ipole->qelem.q_back, ip, qelem);
	}
	epp->ipp_regs = ipp->ipp_regs;
	/* Done! */
}
