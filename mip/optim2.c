/*	$Id$	*/
/*
 * Copyright (c) 2004 Anders Magnusson (ragge@ludd.luth.se).
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

#include <string.h>
#include <stdlib.h>

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif

#define	BDEBUG(x)	if (b2debug) printf x

#define	mktemp(n, t)	mklnode(TEMP, 0, n, t)

static int dfsnum;

void saveip(struct interpass *ip);
void deljumps(struct p2env *);
void optdump(struct interpass *ip);
void printip(struct interpass *pole);

static struct varinfo defsites;
struct interpass *storesave;

void bblocks_build(struct p2env *, struct labelinfo *, struct bblockinfo *);
void cfg_build(struct p2env *, struct labelinfo *labinfo);
void cfg_dfs(struct basicblock *bb, unsigned int parent, 
	     struct bblockinfo *bbinfo);
void dominators(struct p2env *, struct bblockinfo *bbinfo);
struct basicblock *
ancestorwithlowestsemi(struct basicblock *bblock, struct bblockinfo *bbinfo);
void link(struct basicblock *parent, struct basicblock *child);
void computeDF(struct basicblock *bblock, struct bblockinfo *bbinfo);
void printDF(struct p2env *p2e, struct bblockinfo *bbinfo);
void findTemps(struct interpass *ip);
void placePhiFunctions(struct p2env *, struct bblockinfo *bbinfo);
void renamevar(struct p2env *p2e,struct basicblock *bblock, struct bblockinfo *bbinfo);
void removephi(struct p2env *p2e, struct labelinfo *,struct bblockinfo *bbinfo);
void remunreach(struct p2env *);

void
optimize(struct p2env *p2e)
{
	struct interpass *ipole = &p2e->ipole;
	struct labelinfo labinfo;
	struct bblockinfo bbinfo;

	if (b2debug) {
		printf("initial links\n");
		printip(ipole);
	}

	if (xdeljumps)
		deljumps(p2e); /* Delete redundant jumps and dead code */

#ifdef PCC_DEBUG
	if (b2debug) {
		printf("links after deljumps\n");
		printip(ipole);
	}
#endif
	if (xssaflag || xtemps) {
		DLIST_INIT(&p2e->bblocks, bbelem);
		bblocks_build(p2e, &labinfo, &bbinfo);
		BDEBUG(("Calling cfg_build\n"));
		cfg_build(p2e, &labinfo);
	}
	if (xssaflag) {
		BDEBUG(("Calling dominators\n"));
		dominators(p2e, &bbinfo);
		BDEBUG(("Calling computeDF\n"));
		computeDF(DLIST_NEXT(&p2e->bblocks, bbelem), &bbinfo);

		if (b2debug) {
			printDF(p2e,&bbinfo);
		}

		BDEBUG(("Calling placePhiFunctions\n"));

		placePhiFunctions(p2e, &bbinfo);

		BDEBUG(("Calling renamevar\n"));

		renamevar(p2e,DLIST_NEXT(&p2e->bblocks, bbelem), &bbinfo);

		BDEBUG(("Calling removephi\n"));

		removephi(p2e,&labinfo,&bbinfo);

		BDEBUG(("Calling remunreach\n"));
		remunreach(p2e);
		
		/*
		 Recalculate basic blocks and cfg that was destroyed
		 by removephi
		 */

		DLIST_INIT(&p2e->bblocks, bbelem);
		bblocks_build(p2e, &labinfo, &bbinfo);
		BDEBUG(("Calling cfg_build\n"));
		cfg_build(p2e, &labinfo);

#ifdef PCC_DEBUG
		if (b2debug) {
			printf("new tree\n");
			printip(ipole);
		}
#endif
	}

#ifdef PCC_DEBUG
	{
		int i;
		for (i = NIPPREGS; i--; )
			if (p2e->epp->ipp_regs[i] != 0)
				comperr("register error");
	}
#endif

	myoptim(ipole);
}

/*
 * Delete unused labels, excess of labels, gotos to gotos.
 * This routine can be made much more efficient.
 */
void
deljumps(struct p2env *p2e)
{
	struct interpass *ipole = &p2e->ipole;
	struct interpass *ip, *n, *ip2, *start;
	int gotone,low, high;
	int *lblary, *jmpary, sz, o, i, j, lab1, lab2;
	int del;
	extern int negrel[];
	extern size_t negrelsize;

	low = p2e->ipp->ip_lblnum;
	high = p2e->epp->ip_lblnum;

#ifdef notyet
	mark = tmpmark(); /* temporary used memory */
#endif

	sz = (high-low) * sizeof(int);
	lblary = tmpalloc(sz);
	jmpary = tmpalloc(sz);

	/*
	 * XXX: Find the first two labels. They may not be deleted,
	 * because the register allocator expects them to be there.
	 * These will not be coalesced with any other label.
	 */
	lab1 = lab2 = -1;
	start = NULL;
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_DEFLAB)
			continue;
		if (lab1 < 0)
			lab1 = ip->ip_lbl;
		else if (lab2 < 0) {
			lab2 = ip->ip_lbl;
			start = ip;
		} else	/* lab1 >= 0 && lab2 >= 0, we're done. */
			break;
	}
	if (lab1 < 0 || lab2 < 0)
		comperr("deljumps");

again:	gotone = 0;
	memset(lblary, 0, sz);
	lblary[lab1 - low] = lblary[lab2 - low] = 1;
	memset(jmpary, 0, sz);

	/* refcount and coalesce all labels */
	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type == IP_DEFLAB && ip->ip_lbl != lab1 &&
		    ip->ip_lbl != lab2) {
			n = DLIST_NEXT(ip, qelem);

			/*
			 * Find unconditional jumps directly following a
			 * label. Jumps jumping to themselves are not
			 * taken into account.
			 */
			if (n->type == IP_NODE && n->ip_node->n_op == GOTO) {
				i = n->ip_node->n_left->n_lval;
				if (i != ip->ip_lbl)
					jmpary[ip->ip_lbl - low] = i;
			}

			while (n->type == IP_DEFLAB) {
				if (n->ip_lbl != lab1 && n->ip_lbl != lab2 &&
				    lblary[n->ip_lbl-low] >= 0) {
					/*
					 * If the label is used, mark the
					 * label to be coalesced with as
					 * used, too.
					 */
					if (lblary[n->ip_lbl - low] > 0 &&
					    lblary[ip->ip_lbl - low] == 0)
						lblary[ip->ip_lbl - low] = 1;
					lblary[n->ip_lbl - low] = -ip->ip_lbl;
				}
				n = DLIST_NEXT(n, qelem);
			}
		}
		if (ip->type != IP_NODE)
			continue;
		o = ip->ip_node->n_op;
		if (o == GOTO)
			i = ip->ip_node->n_left->n_lval;
		else if (o == CBRANCH)
			i = ip->ip_node->n_right->n_lval;
		else
			continue;

		/*
		 * Mark destination label i as used, if it is not already.
		 * If i is to be merged with label j, mark j as used, too.
		 */
		if (lblary[i - low] == 0)
			lblary[i-low] = 1;
		else if ((j = lblary[i - low]) < 0 && lblary[-j - low] == 0)
			lblary[-j - low] = 1;
	}

	/* delete coalesced/unused labels and rename gotos */
	DLIST_FOREACH(ip, ipole, qelem) {
		n = DLIST_NEXT(ip, qelem);
		if (n->type == IP_DEFLAB) {
			if (lblary[n->ip_lbl-low] <= 0) {
				DLIST_REMOVE(n, qelem);
				gotone = 1;
			}
		}
		if (ip->type != IP_NODE)
			continue;
		o = ip->ip_node->n_op;
		if (o == GOTO)
			i = ip->ip_node->n_left->n_lval;
		else if (o == CBRANCH)
			i = ip->ip_node->n_right->n_lval;
		else
			continue;

		/* Simplify (un-)conditional jumps to unconditional jumps. */
		if (jmpary[i - low] > 0) {
			gotone = 1;
			i = jmpary[i - low];
			if (o == GOTO)
				ip->ip_node->n_left->n_lval = i;
			else
				ip->ip_node->n_right->n_lval = i;
		}

		/* Fixup for coalesced labels. */
		if (lblary[i-low] < 0) {
			if (o == GOTO)
				ip->ip_node->n_left->n_lval = -lblary[i-low];
			else
				ip->ip_node->n_right->n_lval = -lblary[i-low];
		}
	}

	/* Delete gotos to the next statement */
	DLIST_FOREACH(ip, ipole, qelem) {
		n = DLIST_NEXT(ip, qelem);
		if (n->type != IP_NODE)
			continue;
		o = n->ip_node->n_op;
		if (o == GOTO)
			i = n->ip_node->n_left->n_lval;
		else if (o == CBRANCH)
			i = n->ip_node->n_right->n_lval;
		else
			continue;

		ip2 = n;
		ip2 = DLIST_NEXT(ip2, qelem);

		if (ip2->type != IP_DEFLAB)
			continue;
		if (ip2->ip_lbl == i && i != lab1 && i != lab2) {
			tfree(n->ip_node);
			DLIST_REMOVE(n, qelem);
			gotone = 1;
		}
	}

	/*
	 * Transform cbranch cond, 1; goto 2; 1: ... into
	 * cbranch !cond, 2; 1: ...
	 */
	DLIST_FOREACH(ip, ipole, qelem) {
		n = DLIST_NEXT(ip, qelem);
		ip2 = DLIST_NEXT(n, qelem);
		if (ip->type != IP_NODE || ip->ip_node->n_op != CBRANCH)
			continue;
		if (n->type != IP_NODE || n->ip_node->n_op != GOTO)
			continue;
		if (ip2->type != IP_DEFLAB)
			continue;
		i = ip->ip_node->n_right->n_lval;
		j = n->ip_node->n_left->n_lval;
		if (j == lab1 || j == lab2)
			continue;
		if (i != ip2->ip_lbl || i == lab1 || i == lab2)
			continue;
		ip->ip_node->n_right->n_lval = j;
		i = ip->ip_node->n_left->n_op;
		if (i < EQ || i - EQ >= (int)negrelsize)
			comperr("deljumps: unexpected op");
		ip->ip_node->n_left->n_op = negrel[i - EQ];
		tfree(n->ip_node);
		DLIST_REMOVE(n, qelem);
		gotone = 1;
	}

	/* Delete everything after a goto up to the next label. */
	for (ip = start, del = 0; ip != DLIST_ENDMARK(ipole);
	     ip = DLIST_NEXT(ip, qelem)) {
loop:
		if ((n = DLIST_NEXT(ip, qelem)) == DLIST_ENDMARK(ipole))
			break;
		if (n->type != IP_NODE) {
			del = 0;
			continue;
		}
		if (del) {
			tfree(n->ip_node);
			DLIST_REMOVE(n, qelem);
			gotone = 1;
			goto loop;
		} else if (n->ip_node->n_op == GOTO)
			del = 1;
	}

	if (gotone)
		goto again;

#ifdef notyet
	tmpfree(mark);
#endif
}

void
optdump(struct interpass *ip)
{
	static char *nm[] = { "node", "prolog", "newblk", "epilog", "locctr",
		"deflab", "defnam", "asm" };
	printf("type %s\n", nm[ip->type-1]);
	switch (ip->type) {
	case IP_NODE:
#ifdef PCC_DEBUG
		fwalk(ip->ip_node, e2print, 0);
#endif
		break;
	case IP_DEFLAB:
		printf("label " LABFMT "\n", ip->ip_lbl);
		break;
	case IP_ASM:
		printf(": %s\n", ip->ip_asm);
		break;
	}
}

/*
 * Build the basic blocks, algorithm 9.1, pp 529 in Compilers.
 *
 * Also fills the labelinfo struct with information about which bblocks
 * that contain which label.
 */

void
bblocks_build(struct p2env *p2e, struct labelinfo *labinfo,
    struct bblockinfo *bbinfo)
{
	struct interpass *ipole = &p2e->ipole;
	struct interpass *ip;
	struct basicblock *bb = NULL;
	int low, high;
	int count = 0;
	int i;

	BDEBUG(("bblocks_build (%p, %p)\n", labinfo, bbinfo));
	low = p2e->ipp->ip_lblnum;
	high = p2e->epp->ip_lblnum;

	/* 
	 * First statement is a leader.
	 * Any statement that is target of a jump is a leader.
	 * Any statement that immediately follows a jump is a leader.
	 */
	DLIST_FOREACH(ip, ipole, qelem) {
		if (bb == NULL || (ip->type == IP_EPILOG) ||
		    (ip->type == IP_DEFLAB) || (ip->type == IP_DEFNAM)) {
			bb = tmpalloc(sizeof(struct basicblock));
			bb->first = ip;
			SLIST_INIT(&bb->children);
			SLIST_INIT(&bb->parents);
			bb->dfnum = 0;
			bb->dfparent = 0;
			bb->semi = 0;
			bb->ancestor = 0;
			bb->idom = 0;
			bb->samedom = 0;
			bb->bucket = NULL;
			bb->df = NULL;
			bb->dfchildren = NULL;
			bb->Aorig = NULL;
			bb->Aphi = NULL;
			SLIST_INIT(&bb->phi);
			bb->bbnum = count;
			DLIST_INSERT_BEFORE(&p2e->bblocks, bb, bbelem);
			count++;
		}
		bb->last = ip;
		if ((ip->type == IP_NODE) && (ip->ip_node->n_op == GOTO || 
		    ip->ip_node->n_op == CBRANCH))
			bb = NULL;
		if (ip->type == IP_PROLOG)
			bb = NULL;
	}
	p2e->nbblocks = count;

	if (b2debug) {
		printf("Basic blocks in func: %d, low %d, high %d\n",
		    count, low, high);
		DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {
			printf("bb %p: first %p last %p\n", bb,
			    bb->first, bb->last);
		}
	}

	labinfo->low = low;
	labinfo->size = high - low + 1;
	labinfo->arr = tmpalloc(labinfo->size * sizeof(struct basicblock *));
	for (i = 0; i < labinfo->size; i++) {
		labinfo->arr[i] = NULL;
	}
	
	bbinfo->size = count + 1;
	bbinfo->arr = tmpalloc(bbinfo->size * sizeof(struct basicblock *));
	for (i = 0; i < bbinfo->size; i++) {
		bbinfo->arr[i] = NULL;
	}

	/* Build the label table */
	DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {
		if (bb->first->type == IP_DEFLAB)
			labinfo->arr[bb->first->ip_lbl - low] = bb;
	}

	if (b2debug) {
		printf("Label table:\n");
		for (i = 0; i < labinfo->size; i++)
			if (labinfo->arr[i])
				printf("Label %d bblock %p\n", i+low,
				    labinfo->arr[i]);
	}
}

/*
 * Build the control flow graph.
 */

void
cfg_build(struct p2env *p2e, struct labelinfo *labinfo)
{
	/* Child and parent nodes */
	struct cfgnode *cnode; 
	struct cfgnode *pnode;
	struct basicblock *bb;
	
	DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {

		if (bb->first->type == IP_EPILOG) {
			break;
		}

		cnode = tmpalloc(sizeof(struct cfgnode));
		pnode = tmpalloc(sizeof(struct cfgnode));
		pnode->bblock = bb;

		if ((bb->last->type == IP_NODE) && 
		    (bb->last->ip_node->n_op == GOTO) &&
		    (bb->last->ip_node->n_left->n_op == ICON))  {
			if (bb->last->ip_node->n_left->n_lval - labinfo->low > 
			    labinfo->size) {
				comperr("Label out of range: %d, base %d", 
					bb->last->ip_node->n_left->n_lval, 
					labinfo->low);
			}
			cnode->bblock = labinfo->arr[bb->last->ip_node->n_left->n_lval - labinfo->low];
			SLIST_INSERT_LAST(&cnode->bblock->parents, pnode, cfgelem);
			SLIST_INSERT_LAST(&bb->children, cnode, cfgelem);
			continue;
		}
		if ((bb->last->type == IP_NODE) && 
		    (bb->last->ip_node->n_op == CBRANCH)) {
			if (bb->last->ip_node->n_right->n_lval - labinfo->low > 
			    labinfo->size) 
				comperr("Label out of range: %d", 
					bb->last->ip_node->n_left->n_lval);
			
			cnode->bblock = labinfo->arr[bb->last->ip_node->n_right->n_lval - labinfo->low];
			SLIST_INSERT_LAST(&cnode->bblock->parents, pnode, cfgelem);
			SLIST_INSERT_LAST(&bb->children, cnode, cfgelem);
			cnode = tmpalloc(sizeof(struct cfgnode));
			pnode = tmpalloc(sizeof(struct cfgnode));
			pnode->bblock = bb;
		}

		cnode->bblock = DLIST_NEXT(bb, bbelem);
		SLIST_INSERT_LAST(&cnode->bblock->parents, pnode, cfgelem);
		SLIST_INSERT_LAST(&bb->children, cnode, cfgelem);
	}
}

void
cfg_dfs(struct basicblock *bb, unsigned int parent, struct bblockinfo *bbinfo)
{
	struct cfgnode *cnode;
	
	if (bb->dfnum != 0)
		return;

	bb->dfnum = ++dfsnum;
	bb->dfparent = parent;
	bbinfo->arr[bb->dfnum] = bb;
	SLIST_FOREACH(cnode, &bb->children, cfgelem) {
		cfg_dfs(cnode->bblock, bb->dfnum, bbinfo);
	}
	/* Don't bring in unreachable nodes in the future */
	bbinfo->size = dfsnum + 1;
}

static bittype *
setalloc(int nelem)
{
	bittype *b;
	int sz = (nelem+NUMBITS-1)/NUMBITS;

	b = tmpalloc(sz * sizeof(bittype));
	memset(b, 0, sz * sizeof(bittype));
	return b;
}

/*
 * Algorithm 19.9, pp 414 from Appel.
 */

void
dominators(struct p2env *p2e, struct bblockinfo *bbinfo)
{
	struct cfgnode *cnode;
	struct basicblock *bb, *y, *v;
	struct basicblock *s, *sprime, *p;
	int h, i;

	DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {
		bb->bucket = setalloc(bbinfo->size);
		bb->df = setalloc(bbinfo->size);
		bb->dfchildren = setalloc(bbinfo->size);
	}

	dfsnum = 0;
	cfg_dfs(DLIST_NEXT(&p2e->bblocks, bbelem), 0, bbinfo);

	if (b2debug) {
		struct basicblock *bbb;
		struct cfgnode *ccnode;

		DLIST_FOREACH(bbb, &p2e->bblocks, bbelem) {
			printf("Basic block %d, parents: ", bbb->dfnum);
			SLIST_FOREACH(ccnode, &bbb->parents, cfgelem) {
				printf("%d, ", ccnode->bblock->dfnum);
			}
			printf("\nChildren: ");
			SLIST_FOREACH(ccnode, &bbb->children, cfgelem) {
				printf("%d, ", ccnode->bblock->dfnum);
			}
			printf("\n");
		}
	}

	for(h = bbinfo->size - 1; h > 1; h--) {
		bb = bbinfo->arr[h];
		p = s = bbinfo->arr[bb->dfparent];
		SLIST_FOREACH(cnode, &bb->parents, cfgelem) {
			if (cnode->bblock->dfnum ==0)
				continue; /* Ignore unreachable code */

			if (cnode->bblock->dfnum <= bb->dfnum) 
				sprime = cnode->bblock;
			else 
				sprime = bbinfo->arr[ancestorwithlowestsemi
					      (cnode->bblock, bbinfo)->semi];
			if (sprime->dfnum < s->dfnum)
				s = sprime;
		}
		bb->semi = s->dfnum;
		BITSET(s->bucket, bb->dfnum);
		link(p, bb);
		for (i = 1; i < bbinfo->size; i++) {
			if(TESTBIT(p->bucket, i)) {
				v = bbinfo->arr[i];
				y = ancestorwithlowestsemi(v, bbinfo);
				if (y->semi == v->semi) 
					v->idom = p->dfnum;
				else
					v->samedom = y->dfnum;
			}
		}
		memset(p->bucket, 0, (bbinfo->size + 7)/8);
	}

	if (b2debug) {
		printf("Num\tSemi\tAncest\tidom\n");
		DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {
			printf("%d\t%d\t%d\t%d\n", bb->dfnum, bb->semi,
			    bb->ancestor, bb->idom);
		}
	}

	for(h = 2; h < bbinfo->size; h++) {
		bb = bbinfo->arr[h];
		if (bb->samedom != 0) {
			bb->idom = bbinfo->arr[bb->samedom]->idom;
		}
	}
	DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {
		if (bb->idom != 0 && bb->idom != bb->dfnum) {
			BDEBUG(("Setting child %d of %d\n",
			    bb->dfnum, bbinfo->arr[bb->idom]->dfnum));
			BITSET(bbinfo->arr[bb->idom]->dfchildren, bb->dfnum);
		}
	}
}


struct basicblock *
ancestorwithlowestsemi(struct basicblock *bblock, struct bblockinfo *bbinfo)
{
	struct basicblock *u = bblock;
	struct basicblock *v = bblock;

	while (v->ancestor != 0) {
		if (bbinfo->arr[v->semi]->dfnum < 
		    bbinfo->arr[u->semi]->dfnum) 
			u = v;
		v = bbinfo->arr[v->ancestor];
	}
	return u;
}

void
link(struct basicblock *parent, struct basicblock *child)
{
	child->ancestor = parent->dfnum;
}

void
computeDF(struct basicblock *bblock, struct bblockinfo *bbinfo)
{
	struct cfgnode *cnode;
	int h, i;
	
	SLIST_FOREACH(cnode, &bblock->children, cfgelem) {
		if (cnode->bblock->idom != bblock->dfnum)
			BITSET(bblock->df, cnode->bblock->dfnum);
	}
	for (h = 1; h < bbinfo->size; h++) {
		if (!TESTBIT(bblock->dfchildren, h))
			continue;
		computeDF(bbinfo->arr[h], bbinfo);
		for (i = 1; i < bbinfo->size; i++) {
			if (TESTBIT(bbinfo->arr[h]->df, i) && 
			    (bbinfo->arr[i] == bblock ||
			     (bblock->dfnum != bbinfo->arr[i]->idom))) 
			    BITSET(bblock->df, i);
		}
	}
}

void printDF(struct p2env *p2e, struct bblockinfo *bbinfo)
{
	struct basicblock *bb;
	int i;

	printf("Dominance frontiers:\n");
    
	DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {
		printf("bb %d : ", bb->dfnum);
	
		for (i=1; i < bbinfo->size;i++) {
			if (TESTBIT(bb->df,i)) {
				printf("%d ",i);
			}
		}
	    
		printf("\n");
	}
    
}



static struct basicblock *currbb;
static struct interpass *currip;

/* Helper function for findTemps, Find assignment nodes. */
static void
searchasg(NODE *p, void *arg)
{
	struct pvarinfo *pv;
	int tempnr;
	struct varstack *stacke;
    
	if (p->n_op != ASSIGN)
		return;

	if (p->n_left->n_op != TEMP)
		return;

	tempnr=regno(p->n_left)-defsites.low;
    
	BITSET(currbb->Aorig, tempnr);
	
	pv = tmpcalloc(sizeof(struct pvarinfo));
	pv->next = defsites.arr[tempnr];
	pv->bb = currbb;
	pv->n_type = p->n_left->n_type;
	
	defsites.arr[tempnr] = pv;
	
	
	if (SLIST_FIRST(&defsites.stack[tempnr])==NULL) {
		stacke=tmpcalloc(sizeof (struct varstack));
		stacke->tmpregno=0;
		SLIST_INSERT_FIRST(&defsites.stack[tempnr],stacke,varstackelem);
	}
}

/* Walk the interpass looking for assignment nodes. */
void findTemps(struct interpass *ip)
{
	if (ip->type != IP_NODE)
		return;

	currip = ip;

	walkf(ip->ip_node, searchasg, 0);
}

/*
 * Algorithm 19.6 from Appel.
 */

void
placePhiFunctions(struct p2env *p2e, struct bblockinfo *bbinfo)
{
	struct basicblock *bb;
	struct basicblock *y;
	struct interpass *ip;
	int maxtmp, i, j, k;
	struct pvarinfo *n;
	struct cfgnode *cnode;
	TWORD ntype;
	struct pvarinfo *pv;
	struct phiinfo *phi;
	int phifound;

	bb = DLIST_NEXT(&p2e->bblocks, bbelem);
	defsites.low = ((struct interpass_prolog *)bb->first)->ip_tmpnum;
	bb = DLIST_PREV(&p2e->bblocks, bbelem);
	maxtmp = ((struct interpass_prolog *)bb->first)->ip_tmpnum;
	defsites.size = maxtmp - defsites.low + 1;
	defsites.arr = tmpcalloc(defsites.size*sizeof(struct pvarinfo *));
	defsites.stack = tmpcalloc(defsites.size*sizeof(SLIST_HEAD(, varstack)));
	
	for (i=0;i<defsites.size;i++)
		SLIST_INIT(&defsites.stack[i]);	
	
	/* Find all defsites */
	DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {
		currbb = bb;
		ip = bb->first;
		bb->Aorig = setalloc(defsites.size);
		bb->Aphi = setalloc(defsites.size);
		
		while (ip != bb->last) {
			findTemps(ip);
			ip = DLIST_NEXT(ip, qelem);
		}
		/* Make sure we get the last statement in the bblock */
		findTemps(ip);
	}
    
	/* For each variable */
	for (i = 0; i < defsites.size; i++) {
		/* While W not empty */
		while (defsites.arr[i] != NULL) {
			/* Remove some node n from W */
			n = defsites.arr[i];
			defsites.arr[i] = n->next;
			/* For each y in n->bb->df */
			for (j = 0; j < bbinfo->size; j++) {
				if (!TESTBIT(n->bb->df, j))
					continue;
				
				if (TESTBIT(bbinfo->arr[j]->Aphi, i))
					continue;

				y=bbinfo->arr[j];
				ntype = n->n_type;
				k = 0;
				/* Amount of predecessors for y */
				SLIST_FOREACH(cnode, &y->parents, cfgelem) 
					k++;
				/* Construct phi(...) 
				*/
			    
				phifound=0;
			    
				SLIST_FOREACH(phi, &y->phi, phielem) {
				    if (phi->tmpregno==i+defsites.low)
					phifound++;
				}
			    
				if (phifound==0) {
					if (b2debug)
					    printf("Phi in %d (%p) for %d\n",y->dfnum,y,i+defsites.low);

					phi = tmpcalloc(sizeof(struct phiinfo));
			    
					phi->tmpregno=i+defsites.low;
					phi->size=k;
					phi->n_type=ntype;
					phi->intmpregno=tmpcalloc(k*sizeof(int));
			    
					SLIST_INSERT_LAST(&y->phi,phi,phielem);
				} else {
				    if (b2debug)
					printf("Phi already in %d for %d\n",y->dfnum,i+defsites.low);
				}

				BITSET(bbinfo->arr[j]->Aphi, i);
				if (!TESTBIT(bbinfo->arr[j]->Aorig, i)) {
					pv = tmpalloc(sizeof(struct pvarinfo));
					pv->bb = y;
				        pv->n_type=ntype;
					pv->next = defsites.arr[i];
					defsites.arr[i] = pv;
				}
					

			}
		}
	}
}

/* Helper function for renamevar. */
static void
renamevarhelper(struct p2env *p2e,NODE *t,void *poplistarg)
{	
	SLIST_HEAD(, varstack) *poplist=poplistarg;
	int opty;
	int tempnr;
	int newtempnr;
	int x;
	struct varstack *stacke;
	
	if (t->n_op == ASSIGN && t->n_left->n_op == TEMP) {
		renamevarhelper(p2e,t->n_right,poplist);
				
		tempnr=regno(t->n_left)-defsites.low;
		
		newtempnr=p2e->epp->ip_tmpnum++;
		regno(t->n_left)=newtempnr;
		
		stacke=tmpcalloc(sizeof (struct varstack));
		stacke->tmpregno=newtempnr;
		SLIST_INSERT_FIRST(&defsites.stack[tempnr],stacke,varstackelem);
		
		stacke=tmpcalloc(sizeof (struct varstack));
		stacke->tmpregno=tempnr;
		SLIST_INSERT_FIRST(poplist,stacke,varstackelem);
	} else {
		if (t->n_op == TEMP) {
			tempnr=regno(t)-defsites.low;
			
			x=SLIST_FIRST(&defsites.stack[tempnr])->tmpregno;
			regno(t)=x;
		}
		
		opty = optype(t->n_op);
		
		if (opty != LTYPE)
			renamevarhelper(p2e, t->n_left,poplist);
		if (opty == BITYPE)
			renamevarhelper(p2e, t->n_right,poplist);
	}
}


void
renamevar(struct p2env *p2e,struct basicblock *bb, struct bblockinfo *bbinfo)
{
    	struct interpass *ip;
	int h,j;
	SLIST_HEAD(, varstack) poplist;
	struct varstack *stacke;
	struct cfgnode *cfgn;
	struct cfgnode *cfgn2;
	int tmpregno,newtmpregno;
	struct phiinfo *phi;
	
	SLIST_INIT(&poplist);
	
	SLIST_FOREACH(phi,&bb->phi,phielem) {
		tmpregno=phi->tmpregno-defsites.low;
		
		newtmpregno=p2e->epp->ip_tmpnum++;
		phi->newtmpregno=newtmpregno;
		
		stacke=tmpcalloc(sizeof (struct varstack));
		stacke->tmpregno=newtmpregno;
		SLIST_INSERT_FIRST(&defsites.stack[tmpregno],stacke,varstackelem);
		
		stacke=tmpcalloc(sizeof (struct varstack));
		stacke->tmpregno=tmpregno;
		SLIST_INSERT_FIRST(&poplist,stacke,varstackelem);		
	}
	
	
	ip=bb->first;
	
	while (1) {		
		if ( ip->type == IP_NODE) {
			renamevarhelper(p2e,ip->ip_node,&poplist);
		}
		
		if (ip==bb->last)
			break;
		
		ip = DLIST_NEXT(ip, qelem);
	}
	
	SLIST_FOREACH(cfgn,&bb->children,cfgelem) {
		j=0;
		
		SLIST_FOREACH(cfgn2, &cfgn->bblock->parents, cfgelem) { 
			if (cfgn2->bblock->dfnum==bb->dfnum)
				break;
			
			j++;
		}

		SLIST_FOREACH(phi,&cfgn->bblock->phi,phielem) {
			phi->intmpregno[j]=SLIST_FIRST(&defsites.stack[phi->tmpregno-defsites.low])->tmpregno;
		}
		
	}
	
	for (h = 1; h < bbinfo->size; h++) {
		if (!TESTBIT(bb->dfchildren, h))
			continue;
		
		renamevar(p2e,bbinfo->arr[h], bbinfo);
	}
	
	SLIST_FOREACH(stacke,&poplist,varstackelem) {
		tmpregno=stacke->tmpregno;
		
		defsites.stack[tmpregno].q_forw=defsites.stack[tmpregno].q_forw->varstackelem.q_forw;
	}
}

void
removephi(struct p2env *p2e, struct labelinfo *labinfo,struct bblockinfo *bbinfo)
{
	struct basicblock *bb,*bbparent;
	struct cfgnode *cfgn;
	struct phiinfo *phi;
	int i;
	struct interpass *ip;
	struct interpass *pip;
	TWORD n_type;
	int complex;
	int label=0;
	int newlabel;
	
	DLIST_FOREACH(bb, &p2e->bblocks, bbelem) {		
		SLIST_FOREACH(phi,&bb->phi,phielem) { // Look at only one, notice break at end
			i=0;
			
			SLIST_FOREACH(cfgn, &bb->parents, cfgelem) { 
				bbparent=cfgn->bblock;
				
				pip=bbparent->last;
				
				complex = 0;
				
				BDEBUG(("removephi: %p in %d",pip,bb->dfnum));
				if (pip->type == IP_NODE && pip->ip_node->n_op == GOTO) {
					BDEBUG((" GOTO "));
					label=pip->ip_node->n_left->n_lval;
					complex=1;
				} else if (pip->type == IP_NODE && pip->ip_node->n_op == CBRANCH) {
					BDEBUG((" CBRANCH "));
					label=pip->ip_node->n_right->n_lval;
					
					if (bb==labinfo->arr[label - p2e->ipp->ip_lblnum])
						complex=2;
				}	
       
				BDEBUG((" Complex: %d\n",complex));

				if (complex > 0) {
					/*
					 This destroys basic block calculations.
					 Maybe it shoud not
					*/
					ip = ipnode(mkunode(GOTO, mklnode(ICON, label, 0, INT), 0, INT));
					DLIST_INSERT_BEFORE((bb->first), ip, qelem);
					
					newlabel=getlab2();
					
					ip = tmpalloc(sizeof(struct interpass));
					ip->type = IP_DEFLAB;
					// Line number?? ip->lineno;
					ip->ip_lbl = newlabel;
					DLIST_INSERT_BEFORE((bb->first), ip, qelem);
					
					SLIST_FOREACH(phi,&bb->phi,phielem) {
						if (phi->intmpregno[i]>0) {
							n_type=phi->n_type;
							ip = ipnode(mkbinode(ASSIGN,
								     mktemp(phi->newtmpregno, n_type),
								     mktemp(phi->intmpregno[i],n_type),
								     n_type));
					
							DLIST_INSERT_BEFORE((bb->first), ip, qelem);
						}
					}
					
					if (complex==1)
						pip->ip_node->n_left->n_lval=newlabel;
					
					if (complex==2)
						pip->ip_node->n_right->n_lval=newlabel;
					
				} else {
					/* Construct move */
					SLIST_FOREACH(phi,&bb->phi,phielem) {
						if (phi->intmpregno[i]>0) {
							n_type=phi->n_type;
							ip = ipnode(mkbinode(ASSIGN,
							     mktemp(phi->newtmpregno, n_type),
							     mktemp(phi->intmpregno[i],n_type),
							     n_type));
				
							/* Insert move at bottom of parent basic block */
							DLIST_INSERT_AFTER((bbparent->last), ip, qelem);
						}
					}
				}
				i++;
			}
			break;
		}
	}
}

    
/*
 * Remove unreachable nodes in the CFG.
 */ 

void
remunreach(struct p2env *p2e)
{
	struct basicblock *bb, *nbb;
	struct interpass *next, *ctree;

	bb = DLIST_NEXT(&p2e->bblocks, bbelem);
	while (bb != &p2e->bblocks) {
		nbb = DLIST_NEXT(bb, bbelem);

		/* Code with dfnum 0 is unreachable */
		if (bb->dfnum != 0) {
			bb = nbb;
			continue;
		}

		/* Need the epilogue node for other parts of the
		   compiler, set its label to 0 and backend will
		   handle it. */ 
		if (bb->first->type == IP_EPILOG) {
			bb->first->ip_lbl = 0;
			bb = nbb;
			continue;
		}

		next = bb->first;
		do {
			ctree = next;
			next = DLIST_NEXT(ctree, qelem);
			
			if (ctree->type == IP_NODE)
				tfree(ctree->ip_node);
			DLIST_REMOVE(ctree, qelem);
		} while (ctree != bb->last);
			
		DLIST_REMOVE(bb, bbelem);
		bb = nbb;
	}
}

void
printip(struct interpass *pole)
{
	static char *foo[] = {
	   0, "NODE", "PROLOG", "STKOFF", "EPILOG", "DEFLAB", "DEFNAM", "ASM" };
	struct interpass *ip;
	struct interpass_prolog *ipplg, *epplg;
	unsigned i;

	DLIST_FOREACH(ip, pole, qelem) {
		if (ip->type > MAXIP)
			printf("IP(%d) (%p): ", ip->type, ip);
		else
			printf("%s (%p): ", foo[ip->type], ip);
		switch (ip->type) {
		case IP_NODE: printf("\n");
#ifdef PCC_DEBUG
			fwalk(ip->ip_node, e2print, 0); break;
#endif
		case IP_PROLOG:
			ipplg = (struct interpass_prolog *)ip;
			printf("%s %s regs",
			    ipplg->ipp_name, ipplg->ipp_vis ? "(local)" : "");
			for (i = 0; i < NIPPREGS; i++)
				printf("%s0x%x", i? ":" : " ",
				    ipplg->ipp_regs[i]);
			printf(" autos %d mintemp %d minlbl %d\n",
			    ipplg->ipp_autos, ipplg->ip_tmpnum, ipplg->ip_lblnum);
			break;
		case IP_EPILOG:
			epplg = (struct interpass_prolog *)ip;
			printf("%s %s regs",
			    epplg->ipp_name, epplg->ipp_vis ? "(local)" : "");
			for (i = 0; i < NIPPREGS; i++)
				printf("%s0x%x", i? ":" : " ",
				    epplg->ipp_regs[i]);
			printf(" autos %d mintemp %d minlbl %d\n",
			    epplg->ipp_autos, epplg->ip_tmpnum, epplg->ip_lblnum);
			break;
		case IP_DEFLAB: printf(LABFMT "\n", ip->ip_lbl); break;
		case IP_DEFNAM: printf("\n"); break;
		case IP_ASM: printf("%s\n", ip->ip_asm); break;
		default:
			break;
		}
	}
}
