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
 * Assign non-temporary registers to variables.
 * Cannot do it if:
 * - address is taken of the temporary
 * - variable is declared "volatile".
 */
static void
asgregs(void)
{
	struct interpass *ip;
	struct rsv *w;
	

	rsv = NULL;

	/* Loop over the function to do a usage count */
	SIMPLEQ_FOREACH(ip, &ipole, sqelem) {
		if (ip->type != IP_NODE)
			continue;
		cntuse(ip->ip_node);
	}

	/* Check which nodes that shall be converted to registers */
	w = rsv;
	for (w = rsv; w; w = w->next) {
		if (w->use < 0)
			continue;
		printf("type %x off %d use %d\n", w->type, w->fpoff, w->use);
	}

	/* Convert found nodes to registers */
}

void
saveip(struct interpass *ip)
{
	struct interpass *prol;

	SIMPLEQ_INSERT_TAIL(&ipole, ip, sqelem);

	if (ip->type != IP_EPILOG)
		return;
	saving = -1;

	deljumps();	/* Delete redundant jumps and dead code */
	asgregs();	/* Assign non-temporary registers */

	prol = SIMPLEQ_FIRST(&ipole);
	prol->ip_auto = ip->ip_auto;
	prol->ip_regs = ip->ip_regs;

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
