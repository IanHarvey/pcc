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
#include "external.h"

#include <string.h>
#include <stdlib.h>
#include <machine/limits.h>

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif

extern int saving;

void saveip(struct interpass *ip);
void deljumps(void);
void deltemp(NODE *p);
void optdump(struct interpass *ip);

static SIMPLEQ_HEAD(, interpass) ipole = SIMPLEQ_HEAD_INITIALIZER(ipole);

static struct rsv {
	struct rsv *next;
	int fpoff;
	TWORD type;
	int use;
} *rsv;

struct basicblock {
	CIRCLEQ_ENTRY(basicblock) bbelem;
	SIMPLEQ_HEAD(, cfgnode) children;
	SIMPLEQ_HEAD(, cfgnode) parents;
	struct interpass *first; /* first element of basic block */
	struct interpass *last;  /* last element of basic block */
};

struct labelinfo {
	struct basicblock **arr;
	unsigned int size;
	unsigned int low;
};

struct cfgnode {
	SIMPLEQ_ENTRY(cfgnode) cfgelem;
	struct basicblock *bblock;
};

int bblocks_build(struct labelinfo *labinfo);
void cfg_build(struct labelinfo *labinfo);


static CIRCLEQ_HEAD(, basicblock) bblocks = CIRCLEQ_HEAD_INITIALIZER(bblocks);

static void
addcand(TWORD type, int off, int avoid)
{
	struct rsv *w = rsv;

	while (w != NULL) {
		if (w->type == type && w->fpoff == off) {
			if (avoid)
				w->use = -1;
			else if (w->use > 0)
				w->use++;
			return;
		}
		w = w->next;
	}
	w = tmpalloc(sizeof(*w));
	w->type = type;
	w->fpoff = off;
	w->use = avoid ? -1 : 1;
	w->next = rsv;
	rsv = w;
}

/*
 * walk through the tree and count the number of (possible)
 * temporary nodes.
 */
static void
cntuse(NODE *p)
{
	NODE *l = p->n_left;
	NODE *r = p->n_right;

	if (p->n_op == UMUL && l->n_op == REG && l->n_rval == FPREG) {
		/* found a candidate for register */
		addcand(p->n_type, 0, ISVOL(p->n_qual << TSHIFT));
	} else if (p->n_op == UMUL && l->n_op == PLUS &&
	    l->n_right->n_op == ICON && 
	     (l->n_left->n_op == REG && l->n_left->n_rval == FPREG)) {
		/* The same as above */
		addcand(p->n_type, l->n_right->n_lval,
		    ISVOL(p->n_qual << TSHIFT));
	} else if (p->n_op == PLUS && l->n_op == REG && l->n_rval == FPREG &&
	    p->n_right->n_op == ICON) {
		/* Address taken of temporary, avoid register */
		addcand(DECREF(p->n_type), r->n_lval, 1);
	} else {
		if (optype(p->n_op) == BITYPE)
			cntuse(r);
		if (optype(p->n_op) != LTYPE)
			cntuse(l);
	}
}

/*
 * Insert a node into the register stack.
 */
static void
insert(struct rsv *w, struct rsv **saved, int maxregs)
{
	int i, j, size;

	size = szty(w->type);

	/* Find reg move position */
	for (i = 0; i < maxregs; i++) {
		if (saved[i] == NULL)
			continue;
		if (saved[i]->use > w->use)
			break;
	}
	/* Move down other regs */
	for (j = size; j < i; j++)
		saved[j-size] = saved[j];

	/* Insert new reg pointer */
	if (i-size >= 0) {
		saved[i-size] = w;
		for (j = i-size+1; j < i; j++)
			saved[j] = NULL;
	}
}

/* Help routine to rconvert() */
static int
matches(TWORD type, int off, struct rsv **rsv, int maxregs)
{
	int i;

	for (i = 0; i < maxregs; i++)
		if (rsv[i] && rsv[i]->type == type && rsv[i]->fpoff == off)
			return i;
	return -1;
}

/* Help routine to rconvert() */
static void
modify(NODE *p, int reg)
{
	tfree(p->n_left);
	p->n_op = REG;
	p->n_rval = p->n_rall = reg + MINRVAR;
	p->n_lval = 0;
}

/*
 * walk through the tree and convert nodes to registers
 */
static void
rconvert(NODE *p, struct rsv **rsv, int maxregs)
{
	NODE *l = p->n_left;
	NODE *r = p->n_right;
	int i;

	if (p->n_op == UMUL && l->n_op == REG && l->n_rval == FPREG) {
		/* found a candidate for register */
		if ((i = matches(p->n_type, 0, rsv, maxregs)) >= 0)
			modify(p, i);
	} else if (p->n_op == UMUL && l->n_op == PLUS &&
	    l->n_right->n_op == ICON && 
	     (l->n_left->n_op == REG && l->n_left->n_rval == FPREG)) {
		/* The same as above */
		if ((i = matches(p->n_type,
		    l->n_right->n_lval, rsv, maxregs)) >= 0)
			modify(p, i);
#if 0
	} else if (p->n_op == PLUS && l->n_op == REG && l->n_rval == FPREG &&
	    p->n_right->n_op == ICON) {
		/* Address taken of temporary, avoid register */
		addcand(DECREF(p->n_type), r->n_lval, 1);
#endif
	} else {
		if (optype(p->n_op) == BITYPE)
			rconvert(r, rsv, maxregs);
		if (optype(p->n_op) != LTYPE)
			rconvert(l, rsv, maxregs);
	}
}

/*
 * Assign non-temporary registers to variables.
 * Cannot do it if:
 * - address is taken of the temporary
 * - variable is declared "volatile".
 */
int asgregs(void);
int
asgregs(void)
{
	struct interpass *ip;
	struct rsv *w, **saved;
	int i, maxregs = MAXRVAR - MINRVAR + 1;

	if (maxregs == 0)
		return MAXRVAR; /* No register usage */
	rsv = NULL;

	/* Loop over the function to do a usage count */
	SIMPLEQ_FOREACH(ip, &ipole, sqelem) {
		if (ip->type != IP_NODE)
			continue;
		cntuse(ip->ip_node);
	}
	/* Check which nodes that shall be converted to registers */
	saved = tmpalloc(sizeof(struct rsv *) * maxregs);
	memset(saved, 0, sizeof(struct rsv *) * maxregs);
	w = rsv;
	for (w = rsv; w; w = w->next) {
		if (w->use < 0)
			continue; /* Not allowed to be in register */

		/* XXX check here if type is allowed to be in register */

		insert(w, saved, maxregs);
	}

	/* Convert found nodes to registers */
	SIMPLEQ_FOREACH(ip, &ipole, sqelem) {
		if (ip->type != IP_NODE)
			continue;
		rconvert(ip->ip_node, saved, maxregs);
	}
	for (i = 0; i < maxregs; i++)
		if (saved[i] != NULL)
			break;
	return MINRVAR+i-1;
}

void
saveip(struct interpass *ip)
{
	struct interpass_prolog *ipp, *epp;

#if 0
	int regs;
#endif
	struct labelinfo labinfo;

	SIMPLEQ_INSERT_TAIL(&ipole, ip, sqelem);

	if (ip->type != IP_EPILOG)
		return;
	epp = (struct interpass_prolog *)ip;
	saving = -1;

	//		deljumps();	/* Delete redundant jumps and dead code */
	if (xssaflag) {
		CIRCLEQ_INIT(&bblocks);
		if (bblocks_build(&labinfo)) {
			cfg_build(&labinfo);
#if 0
			dfg = dfg_build(cfg);
			ssa = ssa_build(cfg, dfg);
#endif
		}
 
	}
#if 0
	regs = asgregs();	/* Assign non-temporary registers */
#endif

#ifdef PCC_DEBUG
	if (epp->ipp_regs != MAXRVAR)
		comperr("register error");
#endif

	ipp = (struct interpass_prolog *)SIMPLEQ_FIRST(&ipole);
	ipp->ipp_autos = epp->ipp_autos;
	ipp->ipp_regs = epp->ipp_regs; // = regs;

#ifdef MYOPTIM
	myoptim(prol);
#endif

	while ((ip = SIMPLEQ_FIRST(&ipole))) {
		SIMPLEQ_REMOVE_HEAD(&ipole, sqelem);
		pass2_compile(ip);
	}
}

void
deljumps()
{
	struct interpass *ip, *n;
	int gotone;

again:	gotone = 0;

	SIMPLEQ_FOREACH(ip, &ipole, sqelem) {
		if (ip->type == IP_EPILOG)
			return;
		if (ip->type != IP_NODE)
			continue;
		n = ip->sqelem.sqe_next;
		/* Check for nodes without side effects */
		if (ip->ip_node->n_op != GOTO)
			continue;
		switch (n->type) {
		case IP_NODE:
			tfree(n->ip_node);
			ip->sqelem.sqe_next = n->sqelem.sqe_next;
			break;
		case IP_DEFLAB:
			if (ip->ip_node->n_left->n_lval != n->ip_lbl)
				continue;
			tfree(ip->ip_node);
			*ip = *n;
			break;
		default:
			continue;
		}
		gotone = 1;
	}
	if (gotone)
		goto again;
}

void
optdump(struct interpass *ip)
{
	static char *nm[] = { "node", "prolog", "newblk", "epilog", "locctr",
		"deflab", "defnam", "asm" };
	printf("type %s\n", nm[ip->type-1]);
	switch (ip->type) {
	case IP_NODE:
		fwalk(ip->ip_node, e2print, 0);
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

int
bblocks_build(struct labelinfo *labinfo)
{
	struct interpass *ip;
	struct basicblock *bb = NULL;
	int leader = 1;
	unsigned int low = UINT_MAX, high = 0;
	int XXXcount = 0;
	int i;

	/* 
	 * First statement is a leader.
	 * Any statement that is target of a jump is a leader.
	 * Any statement that immediately follows a jump is a leader.
	 */

	SIMPLEQ_FOREACH(ip, &ipole, sqelem) {
		/* Garbage, skip it */
		if ((ip->type == IP_LOCCTR) || (ip->type == IP_NEWBLK))
			continue;

		if (leader) {
			bb = tmpalloc(sizeof(struct basicblock));
			bb->first = bb->last = ip;
			SIMPLEQ_INIT(&bb->children);
			SIMPLEQ_INIT(&bb->parents);
			CIRCLEQ_INSERT_TAIL(&bblocks, bb, bbelem);
			leader = 0;
			XXXcount++;
		} 
		
		/* Prologue and epilogue in their own bblock */
		if ((ip->type == IP_PROLOG) || (ip->type == IP_EPILOG)) {
			bb->last = ip;
			if (ip->type == IP_EPILOG)
				high = MAX(ip->ip_lbl, high);
			leader = 1;
			continue;
		}
		
		/* Keep track of highest and lowest label number */
		if (ip->type == IP_DEFLAB) {
			low = MIN(ip->ip_lbl, low);
			high = MAX(ip->ip_lbl, high);
		}

		/* Make sure each label is in a unique bblock */
		if (((ip->type == IP_DEFLAB) || (ip->type == IP_DEFNAM)) && 
		    bb->first != ip) {
			bb = tmpalloc(sizeof(struct basicblock));
			bb->first = bb->last = ip;
			SIMPLEQ_INIT(&bb->children);
			SIMPLEQ_INIT(&bb->parents);
			CIRCLEQ_INSERT_TAIL(&bblocks, bb, bbelem);
			XXXcount++;
			continue;
		}

		if (ip->type == IP_ASM)
			return 0;

		if (ip->type == IP_NODE) {
			switch(ip->ip_node->n_op) {
			case CBRANCH:
			case CALL:
			case UCALL:
			case FORTCALL:
			case UFORTCALL:
			case STCALL:
			case USTCALL:
			case GOTO:
			case RETURN:
				/* Jumps, last in bblock. */
				leader = 1;
				break;

			case NAME:
			case ICON:
			case FCON:
			case REG:
			case OREG:
			case MOVE:
			case PLUS:
			case MINUS:
			case DIV:
			case MOD:
			case MUL:
			case AND:
			case OR:
			case ER:
			case LS:
			case COMPL:
			case INCR:
			case DECR:
			case UMUL:
			case UMINUS:
			case EQ:
			case NE:
			case LE:
			case GE:
			case GT:
			case ULE:
			case ULT:
			case UGE:
			case UGT:
			case ASSIGN:
			case FORCE:
			case FUNARG:
				/* Not jumps, continue with bblock. */
				break;

			default:
				comperr("optim2:bblocks_build() %d",ip->ip_node->n_op ); 
				break;
			}
		}

		bb->last = ip;
	}
#ifdef PCC_DEBUG
	printf("Basic blocks in func: %d\n", XXXcount);
	printf("Label range in func: %d\n %d", high - low + 1, low);
#endif

	labinfo->low = low;
	labinfo->size = high - low + 1;
	labinfo->arr = tmpalloc(labinfo->size * sizeof(struct basicblock *));
	for(i = 0; i <= labinfo->size; i++) {
		labinfo->arr[i] = NULL;
	}

	/* Build the label table */
	CIRCLEQ_FOREACH(bb, &bblocks, bbelem) {
		if (bb->first->type == IP_DEFLAB) {
			labinfo->arr[bb->first->ip_lbl - low] = bb;
		}
		if (bb->first->type == IP_EPILOG) {
			labinfo->arr[bb->first->ip_lbl - low] = bb;
		}
	}

	return 1;
}

/*
 * Build the control flow graph.
 */

void
cfg_build(struct labelinfo *labinfo)
{
	/* Child and parent nodes */
	struct cfgnode *cnode; 
	struct cfgnode *pnode;
	struct basicblock *bb;
	
	CIRCLEQ_FOREACH(bb, &bblocks, bbelem) {

		if (bb->first->type == IP_EPILOG) {
			break;
		}

		cnode = tmpalloc(sizeof(struct cfgnode));
		pnode = tmpalloc(sizeof(struct cfgnode));
		pnode->bblock = bb;

		if ((bb->last->type == IP_NODE) && 
		    (bb->last->ip_node->n_op == GOTO)) {
			if (bb->last->ip_node->n_left->n_lval - labinfo->low > 
			    labinfo->size) {
				comperr("Label out of range: %d, base %d", 
					bb->last->ip_node->n_left->n_lval, 
					labinfo->low);
			}
			cnode->bblock = labinfo->arr[bb->last->ip_node->n_left->n_lval - labinfo->low];
			SIMPLEQ_INSERT_TAIL(&cnode->bblock->parents, pnode, cfgelem);
			SIMPLEQ_INSERT_TAIL(&bb->children, cnode, cfgelem);
			continue;
		}
		if ((bb->last->type == IP_NODE) && 
		    (bb->last->ip_node->n_op == CBRANCH)) {
			if (bb->last->ip_node->n_right->n_lval - labinfo->low > 
			    labinfo->size) 
				comperr("Label out of range: %d", 
					bb->last->ip_node->n_left->n_lval);
			
			cnode->bblock = labinfo->arr[bb->last->ip_node->n_right->n_lval - labinfo->low];
			SIMPLEQ_INSERT_TAIL(&cnode->bblock->parents, pnode, cfgelem);
			SIMPLEQ_INSERT_TAIL(&bb->children, cnode, cfgelem);
			cnode = tmpalloc(sizeof(struct cfgnode));
			pnode = tmpalloc(sizeof(struct cfgnode));
			pnode->bblock = bb;
		}

		cnode->bblock = CIRCLEQ_NEXT(bb, bbelem);
		SIMPLEQ_INSERT_TAIL(&cnode->bblock->parents, pnode, cfgelem);
		SIMPLEQ_INSERT_TAIL(&bb->children, cnode, cfgelem);
	}
}
