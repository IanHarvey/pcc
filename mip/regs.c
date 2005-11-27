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

#define	RDEBUG(x)	if (rdebug) printf x
#define	RPRINTIP(x)	if (rdebug) printip(x)
#define	RDX(x)		x

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

/* XXX */
REGW *ablock;

int tempmin, tempmax, savregs;

/*
 * Return the REGW struct for a temporary.
 */
static REGW *
newblock(NODE *p, int class)
{
	REGW *nb = &nblock[(int)p->n_lval];
	if (nb->link.q_forw == 0) {
		DLIST_INSERT_AFTER(&initial, nb, link);
		ASGNUM(nb) = p->n_lval;
		RDEBUG(("Adding longtime %d for tmp %d, class %d\n",
		    nb->nodnum, (int)p->n_lval, class));
	}
	if (nb->r_class == 0)
		nb->r_class = class;
	return nb;
}


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
	struct optab *q;
	int left, right;
	int nreg, need, i, nxreg;
	int nareg, nbreg, ncreg, ndreg;

	if (p->n_su == -1)
		return nsucomp(p->n_left);
   
	q = &table[TBLIDX(p->n_su)];
	nareg = (q->needs & NACOUNT);

	for (i = (q->needs & NBCOUNT), nbreg = 0; i; i -= NBREG)
		nbreg++;
#ifdef notyet
	for (i = (q->needs & NCCOUNT), ncreg = 0; i; i -= NCREG)
		ncreg++;
	for (i = (q->needs & NDCOUNT), ndreg = 0; i; i -= NDREG)
		ndreg++;
#else
	ncreg = ndreg = 0;
#endif

	nxreg = nareg + nbreg + ncreg + ndreg;
	nreg = nxreg * szty(p->n_type);	/* XXX - sanitycheck this */
	if (callop(p->n_op))
		nreg = MAX(fregs, nreg);

	switch (p->n_su & RMASK) {
	case RREG:
		if (p->n_right->n_op == TEMP && (q->rewrite & RRIGHT) == 0) {
			/* only read argument */
			p->n_right->n_regw =
			    newblock(p->n_right, TCLASS(p->n_su));
			right = 0;
			break;
		}
		/* FALLTHROUGH */
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
		if (p->n_left->n_op == TEMP && (q->rewrite & RLEFT) == 0) {
			/* only read argument */
			p->n_left->n_regw = 
			    newblock(p->n_left, TCLASS(p->n_su));
			left = 0;
			break;
		}
		/* FALLTHROUGH */
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

	if (p->n_op == TEMP)
		(void)newblock(p, TCLASS(p->n_su));
	if ((TCLASS(p->n_su) && (q->rewrite & (RLEFT|RRIGHT))) || nxreg) {
		REGW *w = p->n_regw = tmpalloc(sizeof(REGW) * (nxreg+1));

		memset(w, 0, sizeof(REGW) * (nxreg+1));
		if (TCLASS(p->n_su) && (q->rewrite & (RLEFT|RRIGHT))) {
			w->r_class = TCLASS(p->n_su);
			SETNUM(w);
			DLIST_INSERT_BEFORE(&initial, w, link);
			RDEBUG(("Adding short %d\n", w->nodnum));
			w++;
		}
#define	ADCL(n, cl)	\
	for (i = 0; i < n; i++, w++) {	w->r_class = cl; \
		DLIST_INSERT_BEFORE(&initial, w, link);  SETNUM(w); \
		RDEBUG(("Adding " #n " %d\n", w->nodnum)); \
	}
		ADCL(nareg, CLASSA);
		ADCL(nbreg, CLASSB);
		ADCL(ncreg, CLASSC);
		ADCL(ndreg, CLASSD);
	}

	return nreg;
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
#define	GETP(p)		((p)->n_su == -1 ? getp(p) : p)
#ifdef MULTICLASS
#define	GETRALL(p)	(GETP(p)->n_regw)
#else
#define	GETRALL(p)	(GETP(p)->n_rall)
#endif

static NODE *
getp(NODE *p)
{
	while (p->n_su == -1)
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

#ifdef MULTICLASS
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
#else
#define	LIVEADDR LIVEADD
#define	LIVEDELR LIVEDEL
#endif

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
#ifdef MULTICLASS
adjSet(REGW *u, REGW *v)
#else
adjSet(int u, int v)
#endif
{
	struct AdjSet *w;
#ifdef MULTICLASS
	REGW *t;
#else
	int t;
#endif

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
#ifdef MULTICLASS
adjSetadd(REGW *u, REGW *v)
#else
adjSetadd(int u, int v)
#endif
{
	struct AdjSet *w;
	int x;
#ifdef MULTICLASS
	REGW *t;
#else
	int t;
#endif

	if (u > v)
		t = v, v = u, u = t;
	x = ((int)u+(int)v) & 255;
	w = tmpalloc(sizeof(struct AdjSet));
	w->u = u, w->v = v;
	w->next = edgehash[x];
	edgehash[x] = w;
}

int *alias2(int reg, int class);

/*
 * Add an interference edge between two nodes.
 */
static void
AddEdge(REGW *u, REGW *v)
{
	ADJL *x;

	RDEBUG(("AddEdge: u %d v %d\n", ASGNUM(u), ASGNUM(v)));

#ifdef PCC_DEBUG
#if 0
	if (ASGNUM(u) == 0)
		comperr("AddEdge 0");
#endif
	if (CLASS(u) == 0 || CLASS(v) == 0)
		comperr("AddEdge class == 0 (%d, %d)", CLASS(u), CLASS(v));
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

#ifndef MULTICLASS
	RDEBUG(("AddEdge: u %d(d %d) v %d(d %d)\n", u, DEGREE(u), v, DEGREE(v)));
#endif
}

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
	int nbits = tempmax - tempmin;
	struct lives *l;
	void (*fun)(REGW *, REGW *);

	RDEBUG(("addalledges for %d\n", e->nodnum));

	fun = ONLIST(e) == &precolored ? AddEdgepre : AddEdge;

	/* First add to long-lived temps */
	for (i = 0; i < nbits; i += NUMBITS) {
		if ((k = live[i/NUMBITS]) == 0)
			continue;
		while (k) {
			j = ffs(k)-1;
			(*fun)(&nblock[i+j+tempmin], e);
			k &= ~(1 << j);
		}
	}
	RDEBUG(("addalledges longlived done\n"));
	/* short-lived temps */
	DLIST_FOREACH(l, &lused, link)
		(*fun)(l->var, e);
	RDEBUG(("addalledges shortlived done\n"));
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

#if 0

static void
usetemps(NODE *p)
{
	REGW *nb;
	if (p->n_op != TEMP)
		return;

	nb = newblock(p, TCLASS(p->n_su));
	addalledges(nb);
	LIVEADD((int)p->n_lval);
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
	struct rspecial *rs, *rc;
	struct optab *q;
	int nreg;
	int f, i, t, size;
	int left, right;
	int class;
	REGW *def, *l, *r, *w;

	RDEBUG(("insnwalk: %p\n", p));

	if (p->n_su == -1)
		return insnwalk(p->n_left);

	q = &table[TBLIDX(p->n_su)];

	size = szty(p->n_type); /* outgoing count of regs used */
#define	SZLOOP(i) for (i = 0; i < size; i++)
#define	SZSLOOP(i,s) for (i = 0; i < szty(s); i++)
	if (p->n_op == ASSIGN) {
		int l, r;

		if (p->n_left->n_op == TEMP) {
			/* Remove from live set */

			REGW *nb = newblock(p->n_left, TCLASS(p->n_su));
			LIVEDEL((int)p->n_left->n_lval);
			/* always move via itself */
			if (p->n_regw)
				moveadd(nb, p->n_regw);
		}
		if (((l = p->n_left->n_op) == TEMP || l == REG) &&
		    ((r = p->n_right->n_op) == TEMP || r == REG)) {
			f = r == REG ? p->n_right->n_rval : p->n_right->n_lval;
			t = l == REG ? p->n_left->n_rval : p->n_left->n_lval;
			moveadd(&nblock[t], &nblock[f]);
		}
	}

	class = TCLASS(p->n_su);
//	if (class == 0)
//		comperr("insnwalk: node %p no class", p);
	def = p->n_regw;
	nreg = (q->needs & NACOUNT);

	if (class && def)
		addalledges(def);
#if 0
	for (i = 0; i < nreg; i++)
		MYADDEDGE(i+def, p->n_type); /* register constraints */
#endif

	rs = NULL;
	left = right = -1;

	if (q->needs & NSPECIAL) {
		/* special instruction requirements */

		for (rc = rs = nspecial(q); rc->op; rc++) {
			switch (rc->op) {
			case NLEFT: left = rc->num; break;
			case NRIGHT: right = rc->num; break;
			case NRES: moveadd(def, &ablock[rc->num]); break;
			case NEVER: addalledges(&ablock[rc->num]); break;
			case NOLEFT:
			case NORIGHT: break;
			default: comperr("insnwalk nspecial op %d", rc->op);
			}
		}
		nreg = 0;
	}

	if (callop(p->n_op)) {
		/* first add all edges */
		for (i = 0; i < NUMAREG; i++)
			if (TAREGS & (1 << i))
				addalledges(&ablock[i]);

		/* implicit move after call */
		if (class)
			moveadd(def, &ablock[RETREG(class)]);
		nreg = 0;
	}
	/*
	 * rall is the base of need allocation, RESCx tells which
	 * allocated register that should be reclaimed.
	 */
	w = p->n_regw;
	if (class && (q->rewrite & (RLEFT|RRIGHT)))
		w++;
	for (i = 0; i < nreg; i++, w++) {
		LIVEADDR(w);
		addalledges(w); /* XXX special regs? */
	}

	/* If leg regs may not be shared, add edges */
	/* XXX - addalledges -> AddEdge */
	if ((p->n_su & LMASK) == LREG) {
		NODE *lp = GETP(p->n_left);
		REGW *lr = lp->n_regw;

		if (!(q->needs & NASL))
			addalledges(lr);

		/* If a register will be clobbered, and this register */
		/* is not the leg register, add edge */
		if (rs) for (rc = rs; rc->op; rc++) {
			switch (rc->op) {
			case NEVER:
				if (left == rc->num)
					continue;
			case NOLEFT:
				AddEdge(lr, &ablock[rc->num]);
			}
		}
	}
	if ((p->n_su & RMASK) == RREG) {
		NODE *rp = GETP(p->n_right);
		REGW *rr = rp->n_regw;

		if (!(q->needs & NASR))
			addalledges(rr);

		if (rs) for (rc = rs; rc->op; rc++) {
			switch (rc->op) {
			case NEVER:
				if (right == rc->num)
					continue;
			case NOLEFT:
				AddEdge(rr, &ablock[rc->num]);
			}
		}
	}

	/* now remove the needs from the live set */
	w = p->n_regw;
	if (class && (q->rewrite & (RLEFT|RRIGHT)))
		w++;
	for (i = 0; i < nreg; i++, w++)
		LIVEDELR(w);

	/* walk down the legs and add interference edges */
	l = r = 0;
	if ((p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		NODE *rp = GETP(p->n_right);
		r = rp->n_regw;

		LIVEADDR(r);
		if (q->rewrite & RLEFT) {
			l = GETRALL(p->n_left);
			moveadd(p->n_regw, l);
		}
		if (q->needs & NSPECIAL && left) {
			NODE *lp = GETP(p->n_left);
			if (left >= 0)
				moveadd(lp->n_regw, &ablock[left]);
		}
		insnwalk(p->n_left);
		if (p->n_right->n_op != TEMP ||
		    p->n_right->n_regw != &nblock[p->n_right->n_lval]) {
			LIVEDELR(r);
		} else
			r = 0;
	}
	if ((p->n_su & RMASK)) {
		NODE *lp;
		if (r == 0 && (p->n_su & LMASK)) {
			lp = GETP(p->n_left);
			l = lp->n_regw;
			LIVEADDR(l);
		}
		if (q->rewrite & RRIGHT) {
			if (p->n_su & LMASK) {
				REGW *t = GETRALL(p->n_left);
				moveadd(p->n_regw, t);
			}
			moveadd(p->n_regw, GETRALL(p->n_right));
		}
		if (q->needs & NSPECIAL && right >= 0) {
			NODE *rp = GETP(p->n_right);
			if (right >= 0)
				moveadd(rp->n_regw, &ablock[right]);
		}
		insnwalk(p->n_right);
		if (p->n_su & LMASK) {
			if (p->n_left->n_op != TEMP ||
			    p->n_left->n_regw != &nblock[p->n_left->n_lval]) {
				if (l) {
					LIVEDELR(l);
				}
			} else
				l = 0;
		}
	}
	if (!(p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		NODE *lp = GETP(p->n_left);
		if (q->rewrite & RLEFT) {
			if (TCLASS(p->n_su) == TCLASS(lp->n_su))
				moveadd(p->n_regw, lp->n_regw);
		}

		if (left >= 0)
			moveadd(lp->n_regw, &ablock[left]);
		insnwalk(p->n_left);
	}
	if ((q->rewrite & RLEFT) && !(p->n_su & LMASK) &&
	    p->n_left->n_op == TEMP)
		moveadd(&nblock[p->n_left->n_lval], def);
	if ((q->rewrite & RRIGHT) && !(p->n_su & RMASK) &&
	    p->n_right->n_op == TEMP)
		moveadd(&nblock[p->n_right->n_lval], def);
	
	if (p->n_op == TEMP) {
		moveadd(&nblock[p->n_lval], def);
		addalledges(&nblock[(int)p->n_lval]);
		LIVEADD((int)p->n_lval);
	} /* XXX - fix artificial edges */
	  else if ((p->n_su & (RMASK|LMASK)) == 0) {
		if (p->n_op == ASSIGN && p->n_left->n_op == TEMP)
			walkf(p->n_right, usetemps);
		else
			walkf(p, usetemps);
	}

	/* Finished, clean up live set */
	if (r) {
		LIVEDELR(r);
	}
	if (l) {
		LIVEDELR(l);
	}
}

#else

static void insnwalk(NODE *p);

static void
insnbitype(NODE *p)
{
	
	struct optab *q = &table[TBLIDX(p->n_su)];
	REGW *l, *r, *t;
	REGW *nret = 0;

	if ((q->rewrite & (RLEFT|RRIGHT)) == 0)
		LIVEDELR(p->n_regw);
#if 0
	nreg = (q->needs & NACOUNT);

	if (q->needs & NSPECIAL) {
		/* bleh */
	} 

	needalloc();

	for (each need)
		addalledges(need);
#endif

	l = GETRALL(p->n_left);
	r = GETRALL(p->n_right);
	/* moves for 2-operand instructions */
	if (((p->n_su & LMASK) == LREG) && (q->rewrite & RLEFT)) {
		t = nret ? nret : p->n_regw;
		moveadd(l, t);
	}
	if (((p->n_su & RMASK) == RREG) && (q->rewrite & RRIGHT)) {
		t = nret ? nret : p->n_regw;
		moveadd(r, t);
	}
	if (p->n_su & DORIGHT) {
		if (q->rewrite & RRIGHT)
			LIVEDELR(p->n_regw);
		LIVEADDR(r);
		insnwalk(p->n_left);
	}
	if (q->rewrite & RLEFT)
		LIVEDELR(p->n_regw);
	LIVEADDR(l);
	insnwalk(p->n_right);
	if (!(p->n_su & DORIGHT)) {
		if (q->rewrite & RRIGHT)
			LIVEDELR(p->n_regw);
		LIVEADDR(r);
		insnwalk(p->n_left);
	}
}

/*
 * only walk down right leg.
 */
static void
insnrtype(NODE *p)
{
	
	struct optab *q = &table[TBLIDX(p->n_su)];
	REGW *r, *t;
	REGW *nret = 0;

	if ((q->rewrite & RRIGHT) == 0)
		LIVEDELR(p->n_regw);

	r = GETRALL(p->n_right);

	if (((p->n_su & RMASK) == RREG) && (q->rewrite & RRIGHT)) {
		t = nret ? nret : p->n_regw;
		moveadd(r, t);
	}
	if (q->rewrite & RRIGHT)
		LIVEDELR(p->n_regw);
	LIVEADDR(r);
	insnwalk(p->n_right);
	
}

/*
 * only walk down left leg.
 */
static void
insnltype(NODE *p)
{
	
	struct optab *q = &table[TBLIDX(p->n_su)];
	REGW *l, *t;
	REGW *nret = 0;

	if ((q->rewrite & RLEFT) == 0)
		LIVEDELR(p->n_regw);

	l = GETRALL(p->n_left);

	if (((p->n_su & LMASK) == LREG) && (q->rewrite & RLEFT)) {
		t = nret ? nret : p->n_regw;
		moveadd(l, t);
	}
	if (q->rewrite & RLEFT)
		LIVEDELR(p->n_regw);
	LIVEADDR(l);
	insnwalk(p->n_left);
	
}

/*
 * Handle leaf insn.
 */
static void 
insnleaf(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	REGW *l, *t;
	REGW *nret = 0;

	
}

#if 0
static void
godown(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];

	if (q->rewrite & (RLEFT|RRIGHT))
		LIVEDELR(p->n_regw);
	if (p->n_su & LMASK) {
		

}
#endif

static void
insnassign(NODE *p)
{
	struct optab *q;
	int ltemp = 0, downl, downr;
	REGW *r, *l;

	q = &table[TBLIDX(p->n_su)];

	downl = p->n_su & LMASK;
	downr = p->n_su & RMASK;
	if ((q->visit & INREGS) && (q->rewrite & (RLEFT|RRIGHT)) == 0)
		LIVEDELR(p->n_regw);
	if (p->n_left->n_op == TEMP) {
		REGW *nb = newblock(p->n_left, TCLASS(p->n_su));
		LIVEDEL((int)p->n_left->n_lval);
		if (q->visit & INREGS)
			moveadd(nb, p->n_regw);
		ltemp = 1;
	}
	l = downl ? GETRALL(p->n_left) : NULL;
	r = downr ? GETRALL(p->n_right) : NULL;
	if (p->n_su & DORIGHT && downr) {
		if (q->rewrite & RRIGHT)
			LIVEDELR(p->n_regw);
		LIVEADDR(r);
		if (!ltemp && downl)
			insnwalk(p->n_left);
	}
	if (q->rewrite & RLEFT)
		LIVEDELR(p->n_regw);
	if (downl)
		LIVEADDR(l);
	if (downr)
		insnwalk(p->n_right);
	if (!(p->n_su & DORIGHT) && downr) {
		if (q->rewrite & RRIGHT)
			LIVEDELR(p->n_regw);
		
		LIVEADDR(r);
		insnwalk(p->n_left);
	}
	godown(p);
	/* bleh */
}

static void
insnwalk(NODE *p)
{
	if (p->n_op == ASSIGN)
		insnassign(p);
	else if (callop(p->n_op))
		insncall(p);
	else if ((p->n_su & (LMASK|RMASK)) == (LMASK|RMASK))
		insnbitype(p);
	else if ((p->n_su & (LMASK|RMASK)) == LMASK)
		insnltype(p);
	else if ((p->n_su & (LMASK|RMASK)) == RMASK)
		insnrtype(p);
	else if ((p->n_su & (LMASK|RMASK)) == 0)
		insnleaf(p);
	else
		comperr("insnwalk");
}
#endif

static bittype **gen, **kill, **in, **out;

static void
unionize(NODE *p, int bb)
{
	int i, o, ty;

	if ((o = p->n_op) == TEMP) {
		for (i = 0; i < szty(p->n_type); i++) {
			BITSET(gen[bb], ((int)p->n_lval - tempmin+i));
		}
	}
	if (o == ASSIGN && p->n_left->n_op == TEMP) {
		int b = p->n_left->n_lval - tempmin;
		for (i = 0; i < szty(p->n_type); i++) {
			BITCLEAR(gen[bb], (b+i));
			BITSET(kill[bb], (b+i));
		}
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

#define	SETCOPY(t,f,i,n) for (i = 0; i < n; i += NUMBITS) t[i] = f[i]
#define	SETSET(t,f,i,n) for (i = 0; i < n; i += NUMBITS) t[i] |= f[i]
#define	SETCLEAR(t,f,i,n) for (i = 0; i < n; i += NUMBITS) t[i] &= ~f[i]
#define	SETCMP(v,t,f,i,n) for (i = v = 0; i < n; i += NUMBITS) \
	if (t[i] != f[i]) v = 1

/*
 * Build the set of interference edges and adjacency list.
 */
static void
Build(struct interpass *ipole)
{
	extern struct basicblock bblocks;
	struct interpass *ip;
	struct basicblock *bb;
	struct cfgnode *cn;
	extern int nbblocks;
	bittype *saved;
	int i, j, again, nbits;

	if (xtemps) {
		/* Just fetch space for the temporaries from stack */

		nbits = tempmax - tempmin;
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
		for (i = 0; i < NUMAREG; i++) {
			if ((savregs & (1 << i)) == 0)
				continue;
			BITSET(out[nbblocks-1], i);
			for (j = i+1; j < NUMAREG; j++) {
				if ((savregs & (1 << j)) == 0)
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
				live[j] = 0;
			SETCOPY(live, out[i], j, nbits);
			for (ip = bb->last; ; ip = DLIST_PREV(ip, qelem)) {
				if (ip->type == IP_NODE)
					insnwalk(ip->ip_node);
				if (ip == bb->first)
					break;
			}
		}
	} else {
		DLIST_FOREACH_REVERSE(ip, ipole, qelem) {
			if (ip->type != IP_NODE)
				continue;
			insnwalk(ip->ip_node);
		}
	}

#ifdef PCC_DEBUG
	if (rdebug) {
		int i;
		struct AdjSet *w;
		ADJL *x;
		REGW *y;

		printf("Interference edges\n");
		for (i = 0; i < 256; i++) {
			if ((w = edgehash[i]) == NULL)
				continue;
			for (; w; w = w->next)
				printf("%d <-> %d\n", ASGNUM(w->u), ASGNUM(w->v));
		}
		printf("Degrees\n");
		DLIST_FOREACH(y, &initial, link) {
			printf("%d: trivial [%d] ", ASGNUM(y), trivially_colorable(y));
			for (x = ADJLIST(y); x; x = x->r_next) {
				if (ONLIST(x->a_temp) != &selectStack &&
				    ONLIST(x->a_temp) != &coalescedNodes)
					printf("%d ", ASGNUM(x->a_temp));
				else
					printf("(%d) ", ASGNUM(x->a_temp));
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

	RDEBUG(("DecrementDegree: w %d, c %d\n", ASGNUM(w), c));

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
	    (adjSet(t, r) || !aliasmap(CLASS(t), GREGNO(COLOR(r)), CLASS(r))))
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
		comperr("Conservative");

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
	REGW *w;
	MOVL *m;
	ADJL *l;
	REGW *t;

	RDEBUG(("Combine (%d,%d)\n", ASGNUM(u), ASGNUM(v)));
	w = v;

	if (ONLIST(v) == &freezeWorklist) {
		DELWLIST(w);
	} else {
		DELWLIST(w);
	}
	PUSHWLIST(w, coalescedNodes);
	ALIAS(v) = u;
	if (rdebug) { 
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
		if (ONLIST(u) != &precolored || 
		    aliasmap(CLASS(t), GREGNO(COLOR(u)), CLASS(u)))
			AddEdge(t, u);
		DecrementDegree(t, CLASS(v));
	}
	if (!trivially_colorable(u) && ONLIST(u) == &freezeWorklist) {
		w = u;
		DELWLIST(w);
		PUSHWLIST(w, spillWorklist);
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
		if (w >= &nblock[tempmin] && w < &nblock[100])
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

static void
paint(NODE *p)
{
	struct optab *q;
	REGW *w;

	if (p->n_su == -1)
		return;

#ifdef MULTICLASS
	if (p->n_regw != NULL) {
		/* Must color all allocated regs also */
		w = p->n_regw;
		q = &table[TBLIDX(p->n_su)];
		p->n_reg = 0;
		if (q->rewrite & (RLEFT|RRIGHT)) {
			p->n_reg = COLOR(w);
			w++;
			if (q->needs & NAREG)
				p->n_reg |= ENCRA1(COLOR(w));
		} else if (q->rewrite & RESC1) {
			p->n_reg = COLOR(w);
		} else
			comperr("paint");
	}
#else
	if (p->n_rall != NOPREF)
		p->n_rall = COLOR(p->n_rall);
#endif
	if (p->n_op == TEMP) {
#ifdef MULTICLASS
		REGW *nb = &nblock[(int)p->n_lval];
		p->n_rval = COLOR(nb);
		if (TCLASS(p->n_su) == 0)
			SCLASS(p->n_su, CLASS(nb));
#else
		p->n_rval = COLOR((int)p->n_lval);
#endif
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
			RDEBUG(("Adj(%d): %d (%d)\n",
			    ASGNUM(w), ASGNUM(o), ASGNUM(x->a_temp)));

			if (ONLIST(o) == &coloredNodes ||
			    ONLIST(o) == &precolored) {

				int cl = GREGNO(COLOR(o));
				c = aliasmap(CLASS(w), cl, CLASS(o));
RDEBUG(("aliasmap in class %d by color %d in class %d: %x, okColors %x\n",
CLASS(w), cl, CLASS(o), c, okColors));

				okColors &= ~c;
			}
		}
		if (okColors == 0) {
			PUSHWLIST(w, spilledNodes);
			RDEBUG(("Spilling node %d\n", ASGNUM(w)));
		} else {
			PUSHWLIST(w, coloredNodes);
			c = ffs(okColors)-1;
			COLOR(w) = MKREGNO(c, CLASS(w));
			RDEBUG(("Coloring %d with %s\n",
			    ASGNUM(w), rnames[COLOR(w)]));
		}
	}
	DLIST_FOREACH(w, &coalescedNodes, link) {
		COLOR(w) = COLOR(GetAlias(w));
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

/*
 * Store all spilled nodes in memory by fetching a temporary on the stack.
 * In the non-optimizing case recurse into emit() and let it handle the
 * stack space, otherwise generate stacklevel nodes and put them into 
 * the full function chain.
 * In the non-optimizing case be careful so that the coloring code won't
 * overwrite itself during recursion.
 * XXX - check this comment.
 */

static REGW *spole;

static void
longtemp(NODE *p)
{
	REGW *w;

	if (p->n_op != TEMP)
		return;
	/* XXX - should have a bitmask to find temps to convert */
	DLIST_FOREACH(w, spole, link) {
#ifdef MULTICLASS
		if (w != &nblock[(int)p->n_lval])
#else
		if (R_TEMP(w) != p->n_lval)
#endif
			continue;
		if (w->r_class == 0) {
			w->r_color = BITOOR(freetemp(szty(p->n_type)));
			w->r_class = 1;
		}
		p->n_op = OREG;
		p->n_lval = w->r_color;
		p->n_rval = FPREG;
		break;
	}
}

/*
 * Change the TEMPs in the ipole list to stack variables.
 */
static void
leafrewrite(struct interpass *ipole, REGW *rpole)
{
	struct interpass *ip;

	spole = rpole;
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		walkf(ip->ip_node, longtemp);	/* convert temps to oregs */
		geninsn(ip->ip_node, FOREFF);	/* Do new insn assignment */
		nsucomp(ip->ip_node);		/* Redo sethi-ullman */
	}
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
	REGW shortregs, longregs, permregs, *q;
	REGW *w;
	int rwtyp;

	RDEBUG(("RewriteProgram\n"));
	DLIST_INIT(&shortregs, link);
	DLIST_INIT(&longregs, link);
	DLIST_INIT(&permregs, link);

	/* sort the temporaries in three queues, short, long and perm */
	while (!DLIST_ISEMPTY(&spilledNodes, link)) {
		w = DLIST_NEXT(&spilledNodes, link);
		DLIST_REMOVE(w, link);

		if (w >= &nblock[tempmin] && w < &nblock[100]) { /* XXX */
			q = &permregs;
		} else if (w >= &nblock[100] && w < &nblock[tempmax]) {
			q = &longregs;
		} else
			q = &shortregs;
		DLIST_INSERT_AFTER(q, w, link);
	}
#ifdef PCC_DEBUG
	if (rdebug) {
		printf("permanent: ");
		DLIST_FOREACH(w, &permregs, link)
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

	if (!DLIST_ISEMPTY(&permregs, link)) {
		rwtyp = ONLYPERM;
		DLIST_FOREACH(w, &permregs, link) {
			int num = w - nblock - tempmin;
			savregs &= ~(1 << num);
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

	if (!DLIST_ISEMPTY(&shortregs, link)) {
		/* Must rewrite the trees */
		rwtyp = SMALL;
		comperr("rwtyp == SMALL");
	}

	RDEBUG(("savregs %x rwtyp %d\n", savregs, rwtyp));

	return rwtyp;
}


/*
 * Do register allocation for trees by graph-coloring.
 */
void
ngenregs(struct interpass *ipole)
{
	struct interpass_prolog *ipp, *epp;
	struct interpass *ip;
	int i, nbits = 0;

	DLIST_INIT(&lunused, link);
	DLIST_INIT(&lused, link);

	/*
	 * Do some setup before doing the real thing.
	 */
	ipp = (struct interpass_prolog *)DLIST_NEXT(ipole, qelem);
	epp = (struct interpass_prolog *)DLIST_PREV(ipole, qelem);

	tempmin = ipp->ip_tmpnum;
	tempmax = epp->ip_tmpnum;
#ifdef PCC_DEBUG
	nodnum = tempmax;
#endif
	/*
	 * Allocate space for the permanent registers in the
	 * same block as the long-lived temporaries.
	 * These temporaries will be handled the same way as 
	 * all other variables.
	 */
	savregs = AREGS & ~TAREGS;
	tempmin -= NUMAREG;

	if ((nbits = tempmax - tempmin)) {
		nblock = tmpalloc(nbits * sizeof(REGW));

		nblock -= tempmin;
		live = tmpalloc(BIT2BYTE(nbits));
		RDEBUG(("nblock %p num %d size %d\n",
		    nblock, nbits, nbits * sizeof(REGW)));
	}


	/* Block for precolored nodes */
	ablock = tmpalloc(sizeof(REGW)*(NUMAREG+NUMBREG+NUMCREG+NUMDREG));
	for (i = 0; i < NUMAREG+NUMBREG+NUMCREG+NUMDREG; i++) {
		ablock[i].r_onlist = &precolored;
		ablock[i].r_class = GCLASS(i);
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
	DLIST_INIT(&initial, link);
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		geninsn(ip->ip_node, FOREFF);
		nsucomp(ip->ip_node);
	}
	RDEBUG(("nsucomp allocated %d temps (%d,%d)\n", 
	    tempmax-tempmin, tempmin, tempmax));

	RPRINTIP(ip);
	RDEBUG(("ngenregs: numtemps %d (%d, %d)\n", tempmax-tempmin,
		    tempmin, tempmax));

	DLIST_INIT(&coalescedMoves, link);
	DLIST_INIT(&constrainedMoves, link);
	DLIST_INIT(&frozenMoves, link);
	DLIST_INIT(&worklistMoves, link);
	DLIST_INIT(&activeMoves, link);

	/* Set class and move-related for perm regs */
	for (i = 0; i < NUMAREG; i++) {
		if ((savregs & (1 << i)) == 0)
			continue;
		nblock[i+tempmin].r_class = CLASSA;
		DLIST_INSERT_AFTER(&initial, &nblock[i+tempmin], link);
		moveadd(&nblock[i+tempmin], &ablock[i]);
	}

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

	RDEBUG(("After AssignColors\n"));
	RPRINTIP(ip);

	if (!WLISTEMPTY(spilledNodes)) {
		switch (RewriteProgram(ip)) {
		case ONLYPERM:
			goto onlyperm;
		case SMALL:
			goto recalc;
		}
	}
	/* fill in regs to save */
	ipp->ipp_regs = (savregs ^ -1) & (AREGS & ~TAREGS);
	epp->ipp_regs = ipp->ipp_regs;
	/* Done! */
}
