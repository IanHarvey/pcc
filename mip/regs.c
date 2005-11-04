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

#ifdef MULTICLASS
#define	NO_PRECOLORED
#endif

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
#ifdef MULTICLASS
	struct regw *a_temp;
#else
	int a_temp;
#endif
} ADJL;

/*
 * Structure describing a move.
 */
typedef struct regm {
	DLIST_ENTRY(regm) link;
#ifdef MULTICLASS
	struct regw *src, *dst;
#else
	int src, dst;
#endif
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
#ifdef MULTICLASS
	int r_class;		/* this nodes class */
	int r_nclass[NUMCLASS];	/* count of adjacent classes */
	struct regw *r_alias;		/* aliased temporary */
#else
	int r_alias;		/* number of aliased register */
	int r_degree;		/* degree of this node */
#endif
#ifdef NO_PRECOLORED
	int r_excl;		/* excluded registers for this set */
	int r_move;		/* moves are to these regs */
#endif
	int r_color;		/* final node color */
	struct regw *r_onlist;	/* which work list this node belongs to */
	MOVL *r_moveList;	/* moves associated with this node */
} REGW;

/*
 * Worklists, a node is always on exactly one of these lists.
 */
static REGW precolored, simplifyWorklist, freezeWorklist, spillWorklist,
	spilledNodes, coalescedNodes, coloredNodes, selectStack;
#ifdef MULTICLASS
static REGW initial, *nblock;
#endif

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
	struct optab *q;
	int left, right;
	int nreg, need;

	if (p->n_su == -1)
		return nsucomp(p->n_left);
   
	q = &table[TBLIDX(p->n_su)];
	nreg = (q->needs & NACOUNT) * szty(p->n_type); /* XXX BREGs */
	if (callop(p->n_op))
		nreg = MAX(fregs, nreg);

	switch (p->n_su & RMASK) {
	case RREG:
		if (p->n_right->n_op == TEMP && (q->rewrite & RRIGHT) == 0) {
			/* only read argument */
#ifdef MULTICLASS
			REGW *nb = &nblock[(int)p->n_right->n_lval];
			if (nb->link.q_forw == 0) {
				nb->r_class = TCLASS(p->n_su);
				DLIST_INSERT_AFTER(&initial, nb, link);
			}
			p->n_right->n_regw = nb;
#else
			p->n_right->n_rall = p->n_right->n_lval;
#endif
#ifdef NO_PRECOLORED
			p->n_right->n_qual = type2class(p->n_right->n_type);
#endif
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
#ifdef MULTICLASS
			REGW *nb = &nblock[(int)p->n_left->n_lval];
			if (nb->link.q_forw == 0) {
				nb->r_class = TCLASS(p->n_su);
				DLIST_INSERT_AFTER(&initial, nb, link);
			}
			p->n_left->n_regw = nb;
#else
			p->n_left->n_rall = p->n_left->n_lval;
#endif
#ifdef NO_PRECOLORED
			p->n_left->n_qual = type2class(p->n_left->n_type);
#endif
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
#ifdef MULTICLASS
	if (p->n_op == TEMP) {
		REGW *nb = &nblock[(int)p->n_lval];
		if (nb->link.q_forw == 0) {
			nb->r_class = TCLASS(p->n_su);
			DLIST_INSERT_AFTER(&initial, nb, link);
		}
	}
	p->n_regw = tmpalloc(sizeof(REGW));
	memset(p->n_regw, 0, sizeof(REGW));
	p->n_regw->r_class = TCLASS(p->n_su);
	DLIST_INSERT_BEFORE(&initial, p->n_regw, link);
#else
	p->n_rall = tempmax;
#endif
#ifdef NO_PRECOLORED
	p->n_qual = type2class(p->n_type);
#endif
#ifndef MULTICLASS
	tempmax += szty(p->n_type);
	if (!callop(p->n_op) && !(q->needs & NSPECIAL))
		tempmax += nreg;
#else
	/* !!! */
#endif
	return nreg;
}

#define	RDEBUG(x)	if (rdebug) printf x
#define	RDX(x)		x

#ifndef MULTICLASS
static int maxregs;	/* max usable regs for allocator */
static int allregs;	/* bitmask of usable regs */
#endif
static REGW *nodeblock;
#ifdef MULTICLASS
#define	CLASS(x)	(x)->r_class
#define	NCLASS(x,c)	(x)->r_nclass[c]
#define	ADJLIST(x)	(x)->r_adjList
#define	ALIAS(x)	(x)->r_alias
#else
#define	ALIAS(x)	nodeblock[x].r_alias
#define	ADJLIST(x)	nodeblock[x].r_adjList
#define	DEGREE(x)	nodeblock[x].r_degree
#endif
#ifdef NO_PRECOLORED
#define	EXCL(x)		nodeblock[x].r_excl
#endif
#ifdef MULTICLASS
#define	ONLIST(x)	(x)->r_onlist
#define	MOVELIST(x)	(x)->r_moveList
#define	COLOR(x)	(x)->r_color
#else
#define	ONLIST(x)	nodeblock[x].r_onlist
#define	MOVELIST(x)	nodeblock[x].r_moveList
#define	R_TEMP(w)	(w - nodeblock)
#define	COLOR(x)	nodeblock[x].r_color
#endif

static bittype *live;

#define	PUSHWLIST(w, l)	DLIST_INSERT_AFTER(&l, w, link); w->r_onlist = &l
#define	POPWLIST(l)	popwlist(&l);
#define	DELWLIST(w)	DLIST_REMOVE(w, link)
#define WLISTEMPTY(h)	DLIST_ISEMPTY(&h,link)
#define	PUSHMLIST(w, l, q)	DLIST_INSERT_AFTER(&l, w, link); w->queue = q
#define	POPMLIST(l)	popmlist(&l);

#ifdef MULTICLASS

#define	trivially_colorable(x) trivially_colorable_p((x)->r_nclass)
/*
 * Determine if a node is trivially colorable ("degree < K").
 * This implementation is a dumb one, without considering speed.
 */
static int
trivially_colorable_p(int *n)
{
	int r[NUMCLASS];
	int i;

	for (i = 0; i < NUMCLASS; i++)
		r[i] = n[i] < regK[i] ? n[i] : regK[i];

	return COLORMAP(i, r);
}
#endif

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
static void
LIVEADDR(REGW *x)
{
}

static void
LIVEDELR(REGW *x)
{
}
#else
#define	LIVEADDR LIVEADD
#define	LIVEDELR LIVEDEL
#endif

#define	MOVELISTADD(t, p) movelistadd(t, p)
#define WORKLISTMOVEADD(s,d) worklistmoveadd(s,d)

static void
#ifdef MULTICLASS
movelistadd(REGW *t, REGM *p)
#else
movelistadd(int t, REGM *p)
#endif
{
	MOVL *w = tmpalloc(sizeof(MOVL));

	w->regm = p;
#ifdef MULTICLASS
	w->next = t->r_moveList;
	t->r_moveList = w;
#else
	w->next = MOVELIST(t);
	MOVELIST(t) = w;
#endif
}

static REGM *
#ifdef MULTICLASS
worklistmoveadd(REGW *src, REGW *dst)
#else
worklistmoveadd(int src, int dst)
#endif
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
#ifdef MULTICLASS
	REGW *u, *v;
#else
	int u, v;
#endif
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

/*
 * Add an interference edge between two nodes.
 */
static void
#ifdef MULTICLASS
AddEdge(REGW *u, REGW *v)
#else
AddEdge(int u, int v)
#endif
{
	ADJL *x;

#ifdef MULTICLASS
	RDEBUG(("AddEdge: u %p v %p\n", u, v));
#else
	RDEBUG(("AddEdge: u %d v %d\n", u, v));
#endif

	if (u == v)
		return;
	if (adjSet(u, v))
		return;

	adjSetadd(u, v);

#if defined(NO_PRECOLORED) && !defined(MULTICLASS)
	if (u < tempmin || v < tempmin)
		comperr("precolored node in AddEdge");
#endif

#ifndef MULTICLASS
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
#else
	x = tmpalloc(sizeof(ADJL));
	x->a_temp = v;
	x->r_next = u->r_adjList;
	u->r_adjList = x;
	u->r_nclass[v->r_class]++;

	x = tmpalloc(sizeof(ADJL));
	x->a_temp = u;
	x->r_next = v->r_adjList;
	v->r_adjList = x;
	v->r_nclass[u->r_class]++;

#endif
#ifndef MULTICLASS
	RDEBUG(("AddEdge: u %d(d %d) v %d(d %d)\n", u, DEGREE(u), v, DEGREE(v)));
#endif
}

static int
#ifdef MULTICLASS
MoveRelated(REGW *n)
#else
MoveRelated(int n)
#endif
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
#ifndef MULTICLASS
	int n;
#endif
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
#ifdef MULTICLASS
	while (!DLIST_ISEMPTY(&initial, link)) {
		w = DLIST_NEXT(&initial, link);
		DLIST_REMOVE(w, link);
		if (!trivially_colorable(w)) {
#else
	for (n = tempmin; n < tempmax; n++) {
		REGUALL(w, n);
		if (DEGREE(n) >= maxregs) {
#endif
			PUSHWLIST(w, spillWorklist);
			RDX(s++);
#ifdef MULTICLASS
		} else if (MoveRelated(w)) {
#else
		} else if (MoveRelated(n)) {
#endif
			PUSHWLIST(w, freezeWorklist);
			RDX(f++);
		} else {
			PUSHWLIST(w, simplifyWorklist);
			RDX(d++);
		}
	}
	RDEBUG(("MkWorklist: spill %d freeze %d simplify %d\n", s,f,d));
}

#ifdef MULTICLASS
static struct slt {
	struct slt *next;
	REGW *regw;
} *sltemps;
#endif

static void
#ifdef MULTICLASS
addalledges(REGW *e)
#else
addalledges(int e)
#endif
{
	int i, j, k;
	int nbits = tempmax - tempmin;
	struct slt *w = sltemps;

	/* First add to long-lived temps */
	for (i = 0; i < nbits; i += NUMBITS) {
		if ((k = live[i/NUMBITS]) == 0)
			continue;
		while (k) {
			j = ffs(k)-1;
			AddEdge(&nblock[i+j+tempmin], e);
			k &= ~(1 << j);
		}
	}
	while (w != NULL) {
		AddEdge(w->regw, e);
		w = w->next;
	}
}

#if MULTICLASS && 0
/*
 * Add exclusion edges to all currently live vars.
 * Exclusion edges are against a precolored node.
 * XXX - look at adding an exclusion field instead (SRH).
 */
static void
addalltmpedges(void)
{
	int i, j, k, e;
	int nbits = tempmax - tempmin;
#ifndef NO_PRECOLORED
	int l;
#endif

	for (i = 0; i < nbits; i += NUMBITS) {
		if ((k = live[i/NUMBITS]) == 0)
			continue;
		while (k) {
			j = ffs(k)-1;
			e = tclassmask(CLASS(i+j+tempmin));
#ifdef NO_PRECOLORED
			EXCL(i+j+tempmin) |= e;
#else
			while (e) {
				l = ffs(e)-1;
				AddEdge(i+j+tempmin, l);
				e &= ~(1 << l);
			}
#endif
			k &= ~(1 << j);
		}
	}
}
#endif

static void
#ifdef MULTICLASS
moveadd(REGW *def, REGW *use)
#else
moveadd(int def, int use)
#endif
{
	REGM *r;

	if (def == use)
		return; /* no move to itself XXX - ``shouldn't happen'' */
#ifdef MULTICLASS
	RDEBUG(("moveadd: def %p use %p\n", def, use));
#else
	RDEBUG(("moveadd: def %d use %d\n", def, use));
#endif

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
	struct optab *q;
	int nreg;
	int f, t, size;
	int *left, *right, *rmask;
#ifdef MULTICLASS
	int class;
	REGW *def, *l, *r;
#else
	int def, i, l, r;
#endif

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
#ifdef MULTICLASS
			LIVEDEL((int)p->n_left->n_lval);
			/* always move via itself */
			moveadd(&nblock[(int)p->n_left->n_lval], p->n_regw);
#else
			LIVEDEL((int)p->n_left->n_lval);
			/* always move via itself */
			moveadd((int)p->n_left->n_lval, p->n_rall);
#endif
				
		}
		if (((l = p->n_left->n_op) == TEMP || l == REG) &&
		    ((r = p->n_right->n_op) == TEMP || r == REG)) {
			f = r == REG ? p->n_right->n_rval : p->n_right->n_lval;
			t = l == REG ? p->n_left->n_rval : p->n_left->n_lval;
			moveadd(&nblock[t], &nblock[f]);
		}
	}
#ifdef MULTICLASS
	class = TCLASS(p->n_su);
	if (class == 0)
		comperr("insnwalk: node %p no class", p);
	def = p->n_regw;
#else
	def = p->n_rall;
#endif
	addalledges(def);
	nreg = (q->needs & NACOUNT) * size;
#if 0
	for (i = 0; i < nreg; i++)
		MYADDEDGE(i+def, p->n_type); /* register constraints */
#endif

	left = right = 0;
	rmask = 0;
#if MULTICLASS && 0
	if (q->needs & NSPECIAL) {
		struct rspecial *rs = nspecial(q);
		/* special instruction requirements */


		/* if result ends up in a certain register, add move */
		if (rs->res)
			moveadd(def, rs->res[0]);

		rmask = rs->rmask;
		left = rs->left;
		right = rs->right;
		/* Add edges for used registers */
		for (i = 0; rmask && rmask[i] >= 0; i++)
			addalledges(rmask[i]);
		nreg = 0;
	}
#endif

	if (callop(p->n_op)) {
		/* first add all edges */
#ifdef MULTICLASS
/*		addalltmpedges(); */
		/* XXX - retreg */
#else
		for (i = 0; i < maxregs; i++)
			if (TAREGS & (1 << i))
				addalledges(i);
#endif
		/* implicit move after call */
		moveadd(def, RETREG);
		nreg = 0;
	}
	/*
	 * rall is the base of need allocation, RESCx tells which
	 * allocated register that should be reclaimed.
	 */
#if 0 /* XXX */
	for (i = 0; i < nreg; i++) {
		LIVEADD(def+i);
		addalledges(def+i); /* XXX special regs? */
	}
#endif
	/* If leg regs may not be shared, add edges */
	/* XXX - addalledges -> AddEdge */
	if ((p->n_su & LMASK) == LREG) {
		NODE *lp = GETP(p->n_left);
#ifdef MULTICLASS
		REGW *lr = lp->n_regw;
#else
		int lr = lp->n_rall;
#endif

		if (!(q->needs & NASL))
			addalledges(lr);

		/* If a register will be clobbered, and this register */
		/* is not the leg register, add edge */
#if 0 /* XXX */
		for (i = 0; rmask && rmask[i] >= 0; i++) {
			if (left && rmask[i] == left[0])
				continue;
			AddEdge(lr, rmask[i]);
		}
#endif
	}
	if ((p->n_su & RMASK) == RREG) {
		NODE *rp = GETP(p->n_right);
#ifdef MULTICLASS
		REGW *rr = rp->n_regw;
#else
		int rr = rp->n_rall;
#endif

		if (!(q->needs & NASR))
			addalledges(rr);

#if 0 /* XXX */
		for (i = 0; rmask && rmask[i] >= 0; i++) {
			if (right && rmask[i] == right[0])
				continue;
			AddEdge(rr, rmask[i]);
		}
#endif
	}

	/* now remove the needs from the live set */
#if 0 /* XXX */
	for (i = 0; i < nreg; i++)
		LIVEDEL(def+i);
#endif

	/* walk down the legs and add interference edges */
	l = r = 0;
	if ((p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		NODE *rp = GETP(p->n_right);
#ifdef MULTICLASS
		r = rp->n_regw;
#else
		r = rp->n_rall;
#endif

		LIVEADDR(r);
		if (q->rewrite & RLEFT) {
			l = GETRALL(p->n_left);
#ifdef MULTICLASS
			moveadd(p->n_regw, l);
#else
			moveadd(p->n_rall, l);
#endif
		}
		if (q->needs & NSPECIAL && left) {
#if MULTICLASS && 0
			NODE *lp = GETP(p->n_left);
			if (left)
				moveadd(lp->n_rall, left[0]);
#endif
		}
		insnwalk(p->n_left);
		if (p->n_right->n_op != TEMP ||
#ifdef MULTICLASS
		    p->n_right->n_regw != &nblock[p->n_right->n_lval]) {
			LIVEDELR(r);
#else
		    p->n_right->n_rall != p->n_right->n_lval) {
			LIVEDEL(r);
#endif
		} else
			r = 0;
	}
	if ((p->n_su & RMASK)) {
		NODE *lp;
		if (r == 0 && (p->n_su & LMASK)) {
			lp = GETP(p->n_left);
#ifdef MULTICLASS
			l = lp->n_regw;
			LIVEADDR(l);
#else
			l = lp->n_rall;
			LIVEADD(l);
#endif
		}
		if (q->rewrite & RRIGHT) {
			if (p->n_su & LMASK) {
				REGW *t = GETRALL(p->n_left);
#ifdef MULTICLASS
				moveadd(p->n_regw, t);
#else
				moveadd(p->n_rall, t);
#endif
			}
#ifdef MULTICLASS
			moveadd(p->n_regw, GETRALL(p->n_right));
#else
			moveadd(p->n_rall, GETRALL(p->n_right));
#endif
		}
		if (q->needs & NSPECIAL && right) {
#if MULTICLASS && 0
			NODE *rp = GETP(p->n_right);
			if (right)
				moveadd(rp->n_rall, right[0]);
#endif
		}
		insnwalk(p->n_right);
		if (p->n_su & LMASK) {
			if (p->n_left->n_op != TEMP ||
#ifdef MULTICLASS
			    p->n_left->n_regw != &nblock[p->n_left->n_lval]) {
#else
			    p->n_left->n_rall != p->n_left->n_lval) {
#endif
				if (l) {
					LIVEDELR(l);
				}
			} else
				l = 0;
		}
	}
	if (!(p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		if (q->rewrite & RLEFT)
#ifdef MULTICLASS
			moveadd(p->n_regw, GETRALL(p->n_left));
#else
			moveadd(p->n_rall, GETRALL(p->n_left));
#endif

		if (q->needs & NSPECIAL && left) {
#if MULTICLASS && 0
			NODE *lp = GETP(p->n_left);
			if (left)
				moveadd(lp->n_rall, left[0]);
#endif
		}
		insnwalk(p->n_left);
	}
	if (p->n_op == TEMP) {
#ifdef MULTICLASS
		moveadd(&nblock[p->n_lval], def);
#else
		moveadd(p->n_lval, def);
#endif
		LIVEADD((int)p->n_lval);
	} /* XXX - fix artificial edges */

	/* Finished, clean up live set */
	if (r) {
		LIVEDELR(r);
	}
	if (l) {
		LIVEDELR(l);
	}
}

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
		memcpy(in[bbnum], gen[bbnum], BIT2BYTE(tempfe-tempmin));
#ifdef PCC_DEBUG
		if (rdebug) {
			printf("basic block %d\ngen: ", bbnum);
			for (i = 0; i < tempfe-tempmin; i++)
				if (TESTBIT(gen[bbnum], i))
					printf("%d ", i+tempmin);
			printf("\nkill: ");
			for (i = 0; i < tempfe-tempmin; i++)
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

static int savregs;

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

#ifndef MULTICLASS
		/* register variable temporaries are live */
		for (i = 0; i < NREGREG; i++) {
			if ((savregs & (1 << i)) == 0)
				continue; /* spilled */
			BITSET(out[nbblocks-1], i);
			moveadd(i+MINRVAR, i+tempmin);
			for (j = i; j < NREGREG; j++)
				AddEdge(i+tempmin, j+tempmin);
		}
#endif

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
#ifdef MULTICLASS
		REGW *y;
#endif

		printf("Interference edges\n");
		for (i = 0; i < 256; i++) {
			if ((w = edgehash[i]) == NULL)
				continue;
			for (; w; w = w->next)
#ifdef MULTICLASS
				printf("%p <-> %p\n", w->u, w->v);
#else
				printf("%d <-> %d\n", w->u, w->v);
#endif
		}
		printf("Degrees\n");
#ifdef MULTICLASS
		DLIST_FOREACH(y, &initial, link) {
			printf("%p: trivial %d ", y, trivially_colorable(y));
			for (x = ADJLIST(y); x; x = x->r_next) {
				if (ONLIST(x->a_temp) != &selectStack &&
				    ONLIST(x->a_temp) != &coalescedNodes)
					printf("%p ", x->a_temp);
				else
					printf("(%p) ", x->a_temp);
			}
			printf("\n");
		}
#else
		for (i = tempmin; i < tempmax; i++) {
			printf("%d: degree(%d), ", i, DEGREE(i));
			for (x = ADJLIST(i); x; x = x->r_next) {
				if (ONLIST(x->a_temp) != &selectStack &&
				    ONLIST(x->a_temp) != &coalescedNodes)
					printf("%d ", x->a_temp);
				else
					printf("(%d) ", x->a_temp);
			}
			printf("\n");
		}
#endif
	}
#endif

}

static void
#ifdef MULTICLASS
EnableMoves(REGW *n)
#else
EnableMoves(int n)
#endif
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
#ifdef MULTICLASS
EnableAdjMoves(REGW *nodes)
#else
EnableAdjMoves(int nodes)
#endif
{
	ADJL *w;
#ifdef MULTICLASS
	REGW *n;
#else
	int n;
#endif

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
#ifdef MULTICLASS
DecrementDegree(REGW *w, int c)
#else
DecrementDegree(int m)
#endif
{
#ifndef MULTICLASS
	REGW *w = &nodeblock[m];
#endif

#ifdef MULTICLASS
	RDEBUG(("DecrementDegree: w %p, c %d\n", w, c));
#else
	RDEBUG(("DecrementDegree: m %d, degree %d\n", m, DEGREE(m)));
#endif

#ifdef MULTICLASS
	NCLASS(w, c)--;
	if (!trivially_colorable(w))
		return;
#else
	if (DEGREE(m)-- != maxregs)
		return;
#endif

#ifdef MULTICLASS
	EnableAdjMoves(w);
	DELWLIST(w);
	ONLIST(w) = 0;
	if (MoveRelated(w)) {
#else
	EnableAdjMoves(m);
	DELWLIST(w);
	ONLIST(m) = 0;
	if (MoveRelated(m)) {
#endif
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
#ifdef MULTICLASS
	RDEBUG(("Simplify: node %p class %d\n", w, w->r_class));
#else
	RDEBUG(("Simplify: node %d degree %d\n", R_TEMP(w), w->r_degree));
#endif

	l = w->r_adjList;
	for (; l; l = l->r_next) {
		if (ONLIST(l->a_temp) == &selectStack ||
		    ONLIST(l->a_temp) == &coalescedNodes)
			continue;
#ifdef MULTICLASS
		DecrementDegree(l->a_temp, w->r_class);
#else
		DecrementDegree(l->a_temp);
#endif
	}
}

#ifdef MULTICLASS
static REGW *
GetAlias(REGW *n)
#else
static int
GetAlias(int n)
#endif
{
	if (ONLIST(n) == &coalescedNodes)
		return GetAlias(ALIAS(n));
	return n;
}

static int
#ifdef MULTICLASS
OK(REGW *t, REGW *r)
#else
OK(int t, int r)
#endif
{
#ifdef MULTICLASS
	RDEBUG(("OK: t %p degree(t) %d adjSet(%p,%p)=%d\n",
	    t, CLASS(t), t, r, adjSet(t, r)));
#else
	RDEBUG(("OK: t %d degree(t) %d adjSet(%d,%d)=%d\n",
	    t, DEGREE(t), t, r, adjSet(t, r)));
#endif

#ifdef PCC_DEBUG
	if (rdebug > 1) {
		ADJL *w;
		int ndeg = 0;
		printf("OK degree: ");
		for (w = ADJLIST(t); w; w = w->r_next) {
			if (ONLIST(w->a_temp) != &selectStack &&
			    ONLIST(w->a_temp) != &coalescedNodes)
#ifdef MULTICLASS
				printf("%p ", w->a_temp), ndeg++;
			else
				printf("(%p) ", w->a_temp);
#else
				printf("%d ", w->a_temp), ndeg++;
			else
				printf("(%d) ", w->a_temp);
#endif
		}
		printf("\n");
#ifndef MULTICLASS
		if (ndeg != DEGREE(t) && DEGREE(t) >= 0)
			printf("!!!ndeg %d != DEGREE(t) %d\n", ndeg, DEGREE(t));
#endif
	}
#endif

#ifdef MULTICLASS
	if (trivially_colorable(t) /* || t < MAXREGNUM */ || adjSet(t, r))
		return 1;
#else
	if (DEGREE(t) < maxregs || t < maxregs || adjSet(t, r))
		return 1;
#endif
	return 0;
}

static int
#ifdef MULTICLASS
adjok(REGW *v, REGW *u)
#else
adjok(int v, int u)
#endif
{
	ADJL *w;
#ifdef MULTICLASS
	REGW *t;
#else
	int t;
#endif

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
#ifdef MULTICLASS
Conservative(REGW *u, REGW *v)
#else
Conservative(int u, int v)
#endif
{
	ADJL *w, *ww;
#ifdef MULTICLASS
	REGW *n;
	int i, ncl[NUMCLASS];

	for (i = 0; i < NUMCLASS; i++)
		ncl[i] = 0;
#else
	int k = 0, n;
#endif

#ifdef MULTICLASS
	RDEBUG(("Conservative (%p,%p)\n", u, v));
#else
	RDEBUG(("Conservative (%d,%d)\n", u, v));
#endif
	for (w = ADJLIST(u); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
		for (ww = ADJLIST(v); ww; ww = ww->r_next)
			if (ww->a_temp == n)
				break;
		if (ww)
			continue;
#ifdef MULTICLASS
		if (!trivially_colorable(n))
			ncl[CLASS(n)]++;
#else
		if (DEGREE(n) >= maxregs)
			k++;
#endif
	}
	for (w = ADJLIST(v); w; w = w->r_next) {
		n = w->a_temp;
		if (ONLIST(n) == &selectStack || ONLIST(n) == &coalescedNodes)
			continue;
#ifdef MULTICLASS
		if (!trivially_colorable(n))
			ncl[CLASS(n)]++;
#else
		if (DEGREE(n) >= maxregs)
			k++;
#endif
	}
#ifdef MULTICLASS
	i = trivially_colorable_p(ncl);
	RDEBUG(("Conservative i=%d\n", i));
	return i;
#else
	RDEBUG(("Conservative k=%d\n", k));
	return k < maxregs;
#endif
}

static void
#ifdef MULTICLASS
AddWorkList(REGW *w)
#else
AddWorkList(int u)
#endif
{
#ifndef MULTICLASS
	REGW *w = &nodeblock[u];
#endif

#ifdef MULTICLASS
	if (!MoveRelated(w) && trivially_colorable(w)) {
#else
	if (u >= maxregs && !MoveRelated(u) && DEGREE(u) < maxregs) {
#endif
		DELWLIST(w);
		PUSHWLIST(w, simplifyWorklist);
	}
}

static void
#ifdef MULTICLASS
Combine(REGW *u, REGW *v)
#else
Combine(int u, int v)
#endif
{
	REGW *w;
	MOVL *m;
	ADJL *l;
#ifdef MULTICLASS
	REGW *t;
#else
	int t;
#endif

#ifdef MULTICLASS
	RDEBUG(("Combine (%p,%p)\n", u, v));
#else
	RDEBUG(("Combine (%d,%d)\n", u, v));
#endif
#ifdef MULTICLASS
	w = v;
#else
	w = &nodeblock[v];
#endif
	if (ONLIST(v) == &freezeWorklist) {
		DELWLIST(w);
	} else {
		DELWLIST(w);
	}
	PUSHWLIST(w, coalescedNodes);
	ALIAS(v) = u;
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
		AddEdge(t, u);
#ifdef MULTICLASS
		DecrementDegree(t, CLASS(v));
#else
		DecrementDegree(t);
#endif
	}
#ifdef MULTICLASS
	if (!trivially_colorable(u) && ONLIST(u) == &freezeWorklist) {
#else
	if (DEGREE(u) >= maxregs && ONLIST(u) == &freezeWorklist) {
#endif
		w = u;
		DELWLIST(w);
		PUSHWLIST(w, spillWorklist);
	}
if (rdebug) {
	ADJL *w;
#ifdef MULTICLASS
	printf("Combine %p class (%d): ", u, CLASS(u));
#else
	printf("Combine %d degree (%d): ", u, DEGREE(u));
#endif
	for (w = ADJLIST(u); w; w = w->r_next) {
		if (ONLIST(w->a_temp) != &selectStack &&
		    ONLIST(w->a_temp) != &coalescedNodes)
#ifdef MULTICLASS
			printf("%p ", w->a_temp);
		else
			printf("(%p) ", w->a_temp);
#else
			printf("%d ", w->a_temp);
		else
			printf("(%d) ", w->a_temp);
#endif
	}
	printf("\n");
}
}

static void
Coalesce(void)
{
	REGM *m;
#ifdef MULTICLASS
	REGW *x, *y, *u, *v;
#else
	int x, y, u, v;
#endif

	m = POPMLIST(worklistMoves);
	x = GetAlias(m->src);
	y = GetAlias(m->dst);
#ifdef MULTICLASS
	if (/*y < MAXREGNUM */ 0 )
#else
	if (y < maxregs)
#endif
		u = y, v = x;
	else
		u = x, v = y;
#ifdef MULTICLASS
	RDEBUG(("Coalesce: src %p dst %p u %p v %p x %p y %p\n",
	    m->src, m->dst, u, v, x, y));
#else
	RDEBUG(("Coalesce: src %d dst %d u %d v %d x %d y %d\n",
	    m->src, m->dst, u, v, x, y));
#endif
	if (u == v) {
		RDEBUG(("Coalesce: u == v\n"));
		PUSHMLIST(m, coalescedMoves, COAL);
		AddWorkList(u);
#ifdef MULTICLASS
	} else if (/*v < MAXREGNUM */ 0 || adjSet(u, v)) {
#else
	} else if (v < maxregs || adjSet(u, v)) {
#endif
		RDEBUG(("Coalesce: constrainedMoves\n"));
		PUSHMLIST(m, constrainedMoves, CONSTR);
		AddWorkList(u);
		AddWorkList(v);
#ifdef MULTICLASS
	} else if ((/* u < MAXREGNUM */ 0 && adjok(v, u)) ||
	    (/* u >= MAXREGNUM */ 1 && Conservative(u, v))) {
#else
	} else if ((u < maxregs && adjok(v, u)) ||
	    (u >= maxregs && Conservative(u, v))) {
#endif
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
#ifdef MULTICLASS
FreezeMoves(REGW *u)
#else
FreezeMoves(int u)
#endif
{
	MOVL *w, *o;
	REGM *m;
	REGW *z;
#ifdef MULTICLASS
	REGW *x, *y, *v;
#else
	int x, y, v;
#endif

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
#ifdef MULTICLASS
		RDEBUG(("FreezeMoves: u %p (%p,%p) v %p\n", u,x,y,v));
#else
		RDEBUG(("FreezeMoves: u %d (%d,%d) v %d\n", u,x,y,v));
#endif
		DLIST_REMOVE(m, link);
		PUSHMLIST(m, frozenMoves, FROZEN);
		if (ONLIST(v) != &freezeWorklist)
			continue;
		for (o = MOVELIST(v); o; o = o->next)
			if (o->regm->queue == WLIST || o->regm->queue == ACTIVE)
				break;
		if (o == NULL) {
#ifdef MULTICLASS
			z = v;
#else
			z = &nodeblock[v];
#endif
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
#ifdef MULTICLASS
	RDEBUG(("Freeze %p\n", u));
	FreezeMoves(u);
#else
	RDEBUG(("Freeze %d\n", R_TEMP(u)));
	FreezeMoves(R_TEMP(u));
#endif
}

static void
SelectSpill(void)
{
	REGW *w;

	RDEBUG(("SelectSpill\n"));
	if (rdebug)
#ifdef MULTICLASS
		DLIST_FOREACH(w, &spillWorklist, link)
			printf("SelectSpill: %p\n", w);
#else
		DLIST_FOREACH(w, &spillWorklist, link)
			printf("SelectSpill: %d\n", R_TEMP(w));
#endif

	/* First check if we can spill register variables */
#ifndef MULTICLASS
	DLIST_FOREACH(w, &spillWorklist, link) {
		if (R_TEMP(w) < (tempmin+NREGREG))
			break;
	}

	if (w == &spillWorklist) {
		/* try to find another long-range variable */
		DLIST_FOREACH(w, &spillWorklist, link) {
			if (R_TEMP(w) < tempfe)
				break;
		}
	}
#else
	w = &spillWorklist;
#endif

	if (w == &spillWorklist) {
		/* no heuristics, just fetch first element */
		w = DLIST_NEXT(&spillWorklist, link);
	}
 
        DLIST_REMOVE(w, link);

	PUSHWLIST(w, simplifyWorklist);
#ifdef MULTICLASS
	RDEBUG(("Freezing node %p\n", w));
	FreezeMoves(w);
#else
	RDEBUG(("Freezing node %d\n", R_TEMP(w)));
	FreezeMoves(R_TEMP(w));
#endif
}

static void
paint(NODE *p)
{
	if (p->n_su == -1)
		return;

#ifdef MULTICLASS
	if (p->n_regw != NULL)
		p->n_reg = COLOR(p->n_regw);
#else
	if (p->n_rall != NOPREF)
		p->n_rall = COLOR(p->n_rall);
#endif
	if (p->n_op == TEMP) {
		p->n_op = REG;
#ifdef MULTICLASS
		p->n_rval = COLOR(&nblock[(int)p->n_lval]);
#else
		p->n_rval = COLOR((int)p->n_lval);
#endif
		p->n_lval = 0;
	}
}

static void
AssignColors(struct interpass *ip)
{
#ifdef MULTICLASS
	int okColors, c;
	REGW *o;
#else
	int okColors, o, c, n;
#endif
	REGW *w;
	ADJL *x;

	RDEBUG(("AssignColors\n"));
	while (!WLISTEMPTY(selectStack)) {
		w = POPWLIST(selectStack);
#ifdef MULTICLASS
		okColors = classmask(CLASS(w));
#else
		n = R_TEMP(w);
		okColors = allregs;
#endif
#ifdef MULTICLASS
		for (x = ADJLIST(w); x; x = x->r_next) {
#else
		for (x = ADJLIST(n); x; x = x->r_next) {
#endif
			o = GetAlias(x->a_temp);
#ifdef MULTICLASS
			RDEBUG(("Adj(%p): %p (%p)\n", w, o, x->a_temp));
#else
			RDEBUG(("Adj(%d): %d (%d)\n", n, o, x->a_temp));
#endif
			if (ONLIST(o) == &coloredNodes ||
			    ONLIST(o) == &precolored) {
#ifdef MULTICLASS
				c = aliasmap(CLASS(w), COLOR(o), CLASS(o));
				okColors &= ~c;
#else
				o = COLOR(o);
				okColors &= ~(1 << o);
#endif
			}
		}
		if (okColors == 0) {
			PUSHWLIST(w, spilledNodes);
#ifdef MULTICLASS
			RDEBUG(("Spilling node %p\n", w));
#else
			RDEBUG(("Spilling node %d\n", R_TEMP(w)));
#endif
		} else {
			PUSHWLIST(w, coloredNodes);
			c = ffs(okColors)-1;
#ifdef MULTICLASS
			COLOR(w) = c;
#else
			COLOR(n) = c;
#endif
		}
	}
	DLIST_FOREACH(w, &coalescedNodes, link)
#ifdef MULTICLASS
		COLOR(w) = COLOR(GetAlias(w));
#else
		w->r_color = COLOR(GetAlias(R_TEMP(w)));
#endif

#ifndef MULTICLASS
	if (rdebug)
		for (o = tempmin; o < tempmax; o++)
			printf("%d: %d\n", o, COLOR(o));
#endif
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
	REGW lownum, highnum;
	REGW *w, *ww;
	int rwtyp;

	RDEBUG(("RewriteProgram\n"));
	DLIST_INIT(&lownum, link);
	DLIST_INIT(&highnum, link);
	/* sort the temporaries in two queues, short and long live range */
	while (!DLIST_ISEMPTY(&spilledNodes, link)) {
		w = DLIST_NEXT(&spilledNodes, link);
		DLIST_REMOVE(w, link);
#ifdef MULTICLASS
		if (0) { /* XXX */
#else
		if (R_TEMP(w) < tempfe) {
#endif
			/* No special order */
			DLIST_INSERT_AFTER(&lownum, w, link);
		} else {
			/* Sort numeric */
			ww = DLIST_NEXT(&highnum, link);
			while (ww != &highnum) {
				if (R_TEMP(ww) > R_TEMP(w))
					break;
				ww = DLIST_NEXT(ww, link);
			}
			DLIST_INSERT_BEFORE(ww, w, link);
		}
	}
#ifdef PCC_DEBUG
	if (rdebug) {
		printf("long-lived: ");
		DLIST_FOREACH(w, &lownum, link)
			printf("%d ", R_TEMP(w));
		printf("\nshort-lived: ");
		DLIST_FOREACH(w, &highnum, link)
			printf("%d ", R_TEMP(w));
		printf("\n");
	}
#endif
	rwtyp = 0;
	DLIST_FOREACH(w, &lownum, link) {
		/* No need to rewrite the trees */
		if (R_TEMP(w) < tempmin+NREGREG) {
			savregs &= ~(1 << (R_TEMP(w)-tempmin));
			if (rwtyp < ONLYPERM)
				rwtyp = ONLYPERM;
		} else {
			rwtyp = LEAVES;
			w->r_class = 0; /* no stack space yet allocated */
		}
	}
	if (rwtyp == LEAVES) {
		leafrewrite(ip, &lownum);
		rwtyp = ONLYPERM;
	}

	if (!DLIST_ISEMPTY(&highnum, link)) {
		/* Must rewrite the trees */
		rwtyp = SMALL;
		comperr("rwtyp == SMALL");
	}

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
#ifndef MULTICLASS
	first = 0;
#endif

#ifdef MULTICLASS
	cmapinit();
#endif

	/*
	 * Do some setup before doing the real thing.
	 */
	ipp = (struct interpass_prolog *)DLIST_NEXT(ipole, qelem);
	epp = (struct interpass_prolog *)DLIST_PREV(ipole, qelem);

#ifdef MULTICLASS
	tempmin = ipp->ip_tmpnum;
	tempmax = epp->ip_tmpnum;
	nbits = tempmax - tempmin;
	nblock = tmpalloc(nbits * sizeof(REGW));
	memset(nblock, 0, nbits * sizeof(REGW));
	nblock -= tempmin;
	live = tmpalloc(BIT2BYTE(nbits));
#else
	tempmin = ipp->ip_tmpnum - NREGREG;
	tempfe = tempmax = epp->ip_tmpnum;
#endif

#ifndef MULTICLASS
	allregs = xtemps ? AREGS : TAREGS;
	maxregs = 0;
	
	/* Get total number of registers */
	for (i = allregs; i ; i >>= 1)
		if (i & 1)
			maxregs++;
#endif


recalc:
	DLIST_INIT(&initial, link);
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		geninsn(ip->ip_node, FOREFF);
		nsucomp(ip->ip_node);
	}
	RDEBUG(("nsucomp allocated %d temps (%d,%d,%d)\n", 
	    tempmax-tempfe, tempmin, tempfe, tempmax));

#ifndef MULTICLASS
	if (first == 0) {
		/* XXX - giant offset error */
//		nodeblock = tmpalloc(tempmax * sizeof(REGW));
		nodeblock = malloc(tempmax * sizeof(REGW));
		nbits = tempmax - tempmin;
		live = tmpalloc(BIT2BYTE(nbits));

		savregs = 7; /* XXX */
		first = 1;
	}
#endif
onlyperm:
	memset(nodeblock, 0, tempmax * sizeof(REGW));
	memset(live, 0, BIT2BYTE(nbits));
	memset(edgehash, 0, sizeof(edgehash));

#ifdef PCC_DEBUG
	if (rdebug) {
		printip(ip);
#ifndef MULTICLASS
		printf("allregs: %x, maxregs %d\n", allregs, maxregs);
#endif
		printf("ngenregs: numtemps %d (%d, %d, %d)\n", tempmax-tempmin,
		    tempmin, tempfe, tempmax);
	}
#endif

#ifndef MULTICLASS
	for (i = 0; i < maxregs; i++) {
		ONLIST(i) = &precolored;
		COLOR(i) = i;
	}
#endif
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
#ifdef PCC_DEBUG
	if (rdebug) {
		printf("After AssignColors\n");
		printip(ip);
	}
#endif
	if (!WLISTEMPTY(spilledNodes)) {
		switch (RewriteProgram(ip)) {
		case ONLYPERM:
			goto onlyperm;
		case SMALL:
			goto recalc;
		}
	}
	/* fill in regs to save */
	ipp->ipp_regs = 0;
	for (i = 0; i < NREGREG; i++)
		if ((savregs & (1 << i)) == 0)
			ipp->ipp_regs |= (1 << (i+MINRVAR));
	epp->ipp_regs = ipp->ipp_regs;
	/* Done! */
}
