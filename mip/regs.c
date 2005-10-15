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
			p->n_right->n_rall = p->n_right->n_lval;
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
			p->n_left->n_rall = p->n_left->n_lval;
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
	p->n_rall = tempmax;
	tempmax += szty(p->n_type);
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
#ifdef MULTICLASS
	int r_class;		/* this nodes class */
	int r_nclass[NUMCLASS];	/* count of adjacent classes */
#else
	int r_degree;		/* degree of this node */
#endif
	int r_color;		/* final node color */
	struct regw *r_onlist;	/* which work list this node belongs to */
	MOVL *r_moveList;	/* moves associated with this node */
} REGW;

#define	RDEBUG(x)	if (rdebug) printf x
#define	RDX(x)		x

static int maxregs;	/* max usable regs for allocator */
static int allregs;	/* bitmask of usable regs */
static REGW *nodeblock;
#define	ALIAS(x)	nodeblock[x].r_alias
#define	ADJLIST(x)	nodeblock[x].r_adjList
#ifdef MULTICLASS
#define	CLASS(x)	nodeblock[x].r_class
#define	NCLASS(x,c)	nodeblock[x].r_nclass[c]
#else
#define	DEGREE(x)	nodeblock[x].r_degree
#endif
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

#ifdef MULTICLASS
/*
 * Determine if a node is trivially colorable ("degree < K").
 */
static int
trivially_colorable(int n)
{
	return 0;
}
#endif

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
#define	GETP(p)		((p)->n_su == -1 ? getp(p) : p)
#define	GETRALL(p)	(GETP(p)->n_rall)

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
#ifdef MULTICLASS
		NCLASS(u,CLASS(v))++;
#else
		DEGREE(u)++;
#endif
	}
	if (v >= tempmin) {
		x = tmpalloc(sizeof(ADJL));
		x->a_temp = u;
		x->r_next = ADJLIST(v);
		ADJLIST(v) = x;
#ifdef MULTICLASS
		NCLASS(v,CLASS(u))++;
#else
		DEGREE(v)++;
#endif
	}
#ifndef MULTICLASS
	RDEBUG(("AddEdge: u %d(d %d) v %d(d %d)\n", u, DEGREE(u), v, DEGREE(v)));
#endif
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
	for (n = tempmin; n < tempmax; n++) {
		REGUALL(w, n);
#ifdef MULTICLASS
		if (!trivially_colorable(n)) {
#else
		if (DEGREE(n) >= maxregs) {
#endif
			PUSHWLIST(w, spillWorklist);
			RDX(s++);
		} else if (MoveRelated(n)) {
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
addalledges(int e)
{
	int i, j, k;
	int nbits = tempmax - tempmin;

	for (i = 0; i < nbits; i += NUMBITS) {
		if ((k = live[i/NUMBITS]) == 0)
			continue;
		while (k) {
			j = ffs(k)-1;
			AddEdge(i+j+tempmin, e);
			k &= ~(1 << j);
		}
	}
}

static void
moveadd(int def, int use)
{
	REGM *r;

	if (def == use)
		return; /* no move to itself XXX - ``shouldn't happen'' */
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
	struct optab *q;
	int def, nreg;
	int i, l, r, f, t, size;
	int *left, *right, *rmask;

	RDEBUG(("insnwalk: %p\n", p));

	if (p->n_su == -1)
		return insnwalk(p->n_left);

	q = &table[TBLIDX(p->n_su)];

	size = szty(p->n_type); /* outgoing count of regs used */
#define	SZLOOP(i) for (i = 0; i < size; i++)
#define	SZSLOOP(i,s) for (i = 0; i < szty(s); i++)
	if (p->n_op == ASSIGN) {
		if (p->n_left->n_op == TEMP) {
			/* Remove from live set */
			LIVEDEL((int)p->n_left->n_lval);
			/* always move via itself */
			moveadd((int)p->n_left->n_lval, p->n_rall);
				
		}
		if (((l = p->n_left->n_op) == TEMP || l == REG) &&
		    ((r = p->n_right->n_op) == TEMP || r == REG)) {
			f = r == REG ? p->n_right->n_rval : p->n_right->n_lval;
			t = l == REG ? p->n_left->n_rval : p->n_left->n_lval;
			moveadd(t, f);
		}
	}
	def = p->n_rall;
	addalledges(def);
	nreg = (q->needs & NACOUNT) * size;
	for (i = 0; i < nreg; i++)
		MYADDEDGE(i+def, p->n_type); /* register constraints */

	left = right = 0;
	rmask = 0;
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

	if (callop(p->n_op)) {
		/* first add all edges */
		for (i = 0; i < maxregs; i++)
			if (TAREGS & (1 << i))
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
		addalledges(def+i); /* XXX special regs? */
	}
	/* If leg regs may not be shared, add edges */
	/* XXX - addalledges -> AddEdge */
	if ((p->n_su & LMASK) == LREG) {
		NODE *lp = GETP(p->n_left);
		int lr = lp->n_rall;

		if (!(q->needs & NASL))
			addalledges(lr);

		/* If a register will be clobbered, and this register */
		/* is not the leg register, add edge */
		for (i = 0; rmask && rmask[i] >= 0; i++) {
			if (left && rmask[i] == left[0])
				continue;
			AddEdge(lr, rmask[i]);
		}
	}
	if ((p->n_su & RMASK) == RREG) {
		NODE *rp = GETP(p->n_right);
		int rr = rp->n_rall;
		if (!(q->needs & NASR))
			addalledges(rr);

		for (i = 0; rmask && rmask[i] >= 0; i++) {
			if (right && rmask[i] == right[0])
				continue;
			AddEdge(rr, rmask[i]);
		}
	}

	/* now remove the needs from the live set */
	for (i = 0; i < nreg; i++)
		LIVEDEL(def+i);

	/* walk down the legs and add interference edges */
	l = r = 0;
	if ((p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		NODE *rp = GETP(p->n_right);
		r = rp->n_rall;
		LIVEADD(r);
		if (q->rewrite & RLEFT) {
			l = GETRALL(p->n_left);
			moveadd(p->n_rall, l);
		}
		if (q->needs & NSPECIAL && left) {
			NODE *lp = GETP(p->n_left);
			if (left)
				moveadd(lp->n_rall, left[0]);
		}
		insnwalk(p->n_left);
		if (p->n_right->n_op != TEMP ||
		    p->n_right->n_rall != p->n_right->n_lval) {
			LIVEDEL(r);
		} else
			r = 0;
	}
	if ((p->n_su & RMASK)) {
		NODE *lp;
		if (r == 0 && (p->n_su & LMASK)) {
			lp = GETP(p->n_left);
			l = lp->n_rall;
			LIVEADD(l);
		}
		if (q->rewrite & RRIGHT) {
			if (p->n_su & LMASK) {
				t = GETRALL(p->n_left);
				moveadd(p->n_rall, t);
			}
			moveadd(p->n_rall, GETRALL(p->n_right));
		}
		if (q->needs & NSPECIAL && right) {
			NODE *rp = GETP(p->n_right);
			if (right)
				moveadd(rp->n_rall, right[0]);
		}
		insnwalk(p->n_right);
		if (p->n_su & LMASK) {
			if (p->n_left->n_op != TEMP ||
			    p->n_left->n_rall != p->n_left->n_lval) {
				if (l) {
					LIVEDEL(l);
				}
			} else
				l = 0;
		}
	}
	if (!(p->n_su & DORIGHT) && (p->n_su & LMASK)) {
		if (q->rewrite & RLEFT)
			moveadd(p->n_rall, GETRALL(p->n_left));

		if (q->needs & NSPECIAL && left) {
			NODE *lp = GETP(p->n_left);
			if (left)
				moveadd(lp->n_rall, left[0]);
		}
		insnwalk(p->n_left);
	}
	if (p->n_op == TEMP) {
		moveadd(p->n_lval, def);
		LIVEADD((int)p->n_lval);
	} /* XXX - fix artificial edges */

	/* Finished, clean up live set */
	if (r) {
		LIVEDEL(r);
	}
	if (l) {
		LIVEDEL(l);
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

		/* register variable temporaries are live */
		for (i = 0; i < NREGREG; i++) {
			if ((savregs & (1 << i)) == 0)
				continue; /* spilled */
			BITSET(out[nbblocks-1], i);
			moveadd(i+MINRVAR, i+tempmin);
			for (j = i; j < NREGREG; j++)
				AddEdge(i+tempmin, j+tempmin);
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

		printf("Interference edges\n");
		for (i = 0; i < 256; i++) {
			if ((w = edgehash[i]) == NULL)
				continue;
			for (; w; w = w->next)
				printf("%d <-> %d\n", w->u, w->v);
		}
		printf("Degrees\n");
		for (i = tempmin; i < tempmax; i++) {
#ifndef MULTICLASS
			printf("%d: degree(%d), ", i, DEGREE(i));
#endif
			for (x = ADJLIST(i); x; x = x->r_next) {
				if (ONLIST(x->a_temp) != &selectStack &&
				    ONLIST(x->a_temp) != &coalescedNodes)
					printf("%d ", x->a_temp);
				else
					printf("(%d) ", x->a_temp);
			}
			printf("\n");
		}
	}
#endif

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
#ifdef MULTICLASS
DecrementDegree(int m, int c)
#else
DecrementDegree(int m)
#endif
{
	REGW *w = &nodeblock[m];

#ifdef MULTICLASS
	RDEBUG(("DecrementDegree: m %d, c %d\n", m, CLASS(c)));
#else
	RDEBUG(("DecrementDegree: m %d, degree %d\n", m, DEGREE(m)));
#endif

#ifdef MULTICLASS
	NCLASS(m, CLASS(c))--;
	if (!trivially_colorable(m))
		return;
#else
	if (DEGREE(m)-- != maxregs)
		return;
#endif

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

	w = POPWLIST(simplifyWorklist);
	PUSHWLIST(w, selectStack);
#ifdef MULTICLASS
	RDEBUG(("Simplify: node %d class %d\n", R_TEMP(w), w->r_class));
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
#ifdef MULTICLASS
	RDEBUG(("OK: t %d degree(t) %d adjSet(%d,%d)=%d\n",
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
				printf("%d ", w->a_temp), ndeg++;
			else
				printf("(%d) ", w->a_temp);
		}
		printf("\n");
#ifndef MULTICLASS
		if (ndeg != DEGREE(t) && DEGREE(t) >= 0)
			printf("!!!ndeg %d != DEGREE(t) %d\n", ndeg, DEGREE(t));
#endif
	}
#endif

#ifdef MULTICLASS
	if (trivially_colorable(t) || t < maxregs || adjSet(t, r))
		return 1;
#else
	if (DEGREE(t) < maxregs || t < maxregs || adjSet(t, r))
		return 1;
#endif
	return 0;
}

static int
adjok(int v, int u)
{
	ADJL *w;
	int t;

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
 */
static int
Conservative(int u, int v)
{
	ADJL *w, *ww;
	int k, n;

	RDEBUG(("Conservative (%d,%d)\n", u, v));
	k = 0;
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
			k++;
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
			k++;
#else
		if (DEGREE(n) >= maxregs)
			k++;
#endif
	}
	RDEBUG(("Conservative k=%d\n", k));
	return k < maxregs;
}

static void
AddWorkList(int u)
{
	REGW *w = &nodeblock[u];

#ifdef MULTICLASS
	if (u >= maxregs && !MoveRelated(u) && trivially_colorable(u)) {
#else
	if (u >= maxregs && !MoveRelated(u) && DEGREE(u) < maxregs) {
#endif
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

	RDEBUG(("Combine (%d,%d)\n", u, v));
	w = &nodeblock[v];
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
		DecrementDegree(t, v);
#else
		DecrementDegree(t);
#endif
	}
#ifdef MULTICLASS
	if (!trivially_colorable(u) && ONLIST(u) == &freezeWorklist) {
#else
	if (DEGREE(u) >= maxregs && ONLIST(u) == &freezeWorklist) {
#endif
		w = &nodeblock[u];
		DELWLIST(w);
		PUSHWLIST(w, spillWorklist);
	}
if (rdebug) {
	ADJL *w;
#ifdef MULTICLASS
	printf("Combine %d class (%d): ", u, CLASS(u));
#else
	printf("Combine %d degree (%d): ", u, DEGREE(u));
#endif
	for (w = ADJLIST(u); w; w = w->r_next) {
		if (ONLIST(w->a_temp) != &selectStack &&
		    ONLIST(w->a_temp) != &coalescedNodes)
			printf("%d ", w->a_temp);
		else
			printf("(%d) ", w->a_temp);
	}
	printf("\n");
}
}

static void
Coalesce(void)
{
	REGM *m;
	int x, y, u, v;

	m = POPMLIST(worklistMoves);
	x = GetAlias(m->src);
	y = GetAlias(m->dst);
	if (y < maxregs)
		u = y, v = x;
	else
		u = x, v = y;
	RDEBUG(("Coalesce: src %d dst %d u %d v %d x %d y %d\n",
	    m->src, m->dst, u, v, x, y));
	if (u == v) {
		RDEBUG(("Coalesce: u == v\n"));
		PUSHMLIST(m, coalescedMoves, COAL);
		AddWorkList(u);
	} else if (v < maxregs || adjSet(u, v)) {
		RDEBUG(("Coalesce: constrainedMoves\n"));
		PUSHMLIST(m, constrainedMoves, CONSTR);
		AddWorkList(u);
		AddWorkList(v);
	} else if ((u < maxregs && adjok(v, u)) ||
	    (u >= maxregs && Conservative(u, v))) {
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
		RDEBUG(("FreezeMoves: u %d (%d,%d) v %d\n", u,x,y,v));
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

	u = POPWLIST(freezeWorklist);
	PUSHWLIST(u, simplifyWorklist);
	RDEBUG(("Freeze %d\n", R_TEMP(u)));
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

	/* First check if we can spill register variables */
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

	if (w == &spillWorklist) {
		/* no heuristics, just fetch first element */
		w = DLIST_NEXT(&spillWorklist, link);
	}
 
        DLIST_REMOVE(w, link);

	PUSHWLIST(w, simplifyWorklist);
	RDEBUG(("Freezing node %d\n", R_TEMP(w)));
	FreezeMoves(R_TEMP(w));
}

static void
paint(NODE *p)
{
	if (p->n_su == -1)
		return;

	if (p->n_rall != NOPREF)
		p->n_rall = COLOR(p->n_rall);
	if (p->n_op == TEMP) {
		p->n_op = REG;
		p->n_rval = COLOR((int)p->n_lval);
		p->n_lval = 0;
	}
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
#ifdef MULTICLASS
		okColors = classmask[CLASS(n)];
#else
		okColors = allregs;
#endif
		for (x = ADJLIST(n); x; x = x->r_next) {
			o = GetAlias(x->a_temp);
			RDEBUG(("Adj(%d): %d (%d)\n", n, o, x->a_temp));
			if (ONLIST(o) == &coloredNodes ||
			    ONLIST(o) == &precolored) {
#ifdef MULTICLASS
				o = aliasmap(CLASS(n), o, CLASS(o));
				okColors &= ~o;
#else
				o = COLOR(o);
				okColors &= ~(1 << o);
#endif
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
		if (R_TEMP(w) != p->n_lval)
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
		if (R_TEMP(w) < tempfe) {
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
	int i, nbits = 0, first = 0;

	/*
	 * Do some setup before doing the real thing.
	 */
	ipp = (struct interpass_prolog *)DLIST_NEXT(ipole, qelem);
	epp = (struct interpass_prolog *)DLIST_PREV(ipole, qelem);

	tempmin = ipp->ip_tmpnum - NREGREG;
	tempfe = tempmax = epp->ip_tmpnum;

	allregs = xtemps ? AREGS : TAREGS;
	maxregs = 0;
	
	/* Get total number of registers */
	for (i = allregs; i ; i >>= 1)
		if (i & 1)
			maxregs++;


recalc:
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		geninsn(ip->ip_node, FOREFF);
		nsucomp(ip->ip_node);
	}
	RDEBUG(("nsucomp allocated %d temps (%d,%d,%d)\n", 
	    tempmax-tempfe, tempmin, tempfe, tempmax));

	if (first == 0) {
		/* XXX - giant offset error */
//		nodeblock = tmpalloc(tempmax * sizeof(REGW));
		nodeblock = malloc(tempmax * sizeof(REGW));
		nbits = tempmax - tempmin;
		live = tmpalloc(BIT2BYTE(nbits));

		savregs = 7; /* XXX */
		first = 1;
	}

onlyperm:
	memset(nodeblock, 0, tempmax * sizeof(REGW));
	memset(live, 0, BIT2BYTE(nbits));
	memset(edgehash, 0, sizeof(edgehash));

#ifdef PCC_DEBUG
	if (rdebug) {
		printip(ip);
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
