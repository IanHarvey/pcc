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
 * Copyright(C) Caldera International Inc. 2001-2002. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code and documentation must retain the above
 * copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditionsand the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 * 	This product includes software developed or owned by Caldera
 *	International, Inc.
 * Neither the name of Caldera International, Inc. nor the names of other
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * USE OF THE SOFTWARE PROVIDED FOR UNDER THIS LICENSE BY CALDERA
 * INTERNATIONAL, INC. AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL CALDERA INTERNATIONAL, INC. BE LIABLE
 * FOR ANY DIRECT, INDIRECT INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OFLIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 */

# include "pass2.h"
#include "external.h"

#include <string.h>
#include <stdarg.h>

/*	some storage declarations */
int nrecur;
int lflag;
int x2debug;
int udebug = 0;
int ftnno;
static int thisline;
int fregs;

NODE *nodepole;
int saving;

int e2print(NODE *p, int down, int *a, int *b);
void saveip(struct interpass *ip);
void deljumps(void);
void deltemp(NODE *p);
void optdump(struct interpass *ip);
void cvtemps(struct interpass *epil);
int findops(NODE *p, int);
int findasg(NODE *p, int);
int finduni(NODE *p, int);
int findleaf(NODE *p, int);
int relops(NODE *p);
int asgops(NODE *p, int);
NODE *store(NODE *);

static void gencode(NODE *p, int cookie);

static char *ltyp[] = { "", "LREG", "LOREG", "LTEMP" };
static char *rtyp[] = { "", "RREG", "ROREG", "RTEMP" };

#define	DELAYS 20
NODE *deltrees[DELAYS];
int deli;

#ifdef PCC_DEBUG
static void
cktree(NODE *p)
{
	if (p->n_op > MAXOP)
		cerror("op %d slipped through", p->n_op);
	if (p->n_op == CBRANCH && !logop(p->n_left->n_op))
		cerror("not logop branch");
	if ((dope[p->n_op] & ASGOPFLG) && p->n_op != RETURN)
		cerror("asgop %d slipped through", p->n_op);
	if (p->n_op ==CALL || p->n_op == ADDROF)
		cerror("non-UCALL node");
}
#endif

static void
p2compile(NODE *p)
{
	int i;

#if !defined(MULTIPASS)
	extern char *ftitle;
#endif

	if (lflag)
		lineid(lineno, ftitle);

	/* generate code for the tree p */
#ifdef PCC_DEBUG
	walkf(p, cktree);
	if (e2debug) {
		fprintf(stderr, "Entering pass2\n");
		fwalk(p, e2print, 0);
	}
#endif

# ifdef MYREADER
	MYREADER(p);  /* do your own laundering of the input */
# endif
	nrecur = 0;
	deli = 0;
	delay(p);
	codgen(p, FOREFF);
	for (i = 0; i < deli; ++i)
		codgen(deltrees[i], FOREFF);  /* do the rest */
	tfree(p);
}

/* look for delayable ++ and -- operators */
void
delay(NODE *p)
{
	int ty = optype(p->n_op);

	switch (p->n_op) {
	case UCALL:
	case STCALL:
	case USTCALL:
	case FORTCALL:
	case UFORTCALL:
	case CBRANCH:
		/* for the moment, don7t delay past a conditional context, or
		 * inside of a call */
		return;

	case UMUL:
		/* if *p++, do not rewrite */
		if( autoincr( p ) ) return;
		break;

	case INCR:
	case DECR:
		break;
		if( deltest( p ) ){
			if( deli < DELAYS ){
				register NODE *q;
				deltrees[deli++] = tcopy(p);
				q = p->n_left;
				nfree(p->n_right); /* zap constant */
				*p = *q;
				nfree(q);
				return;
			}
		}

	}

	if (ty == BITYPE)
		delay(p->n_right);
	if (ty != LTYPE)
		delay(p->n_left);
}

/*
 * Check if a node has side effects.
 */
static int
isuseless(NODE *n)
{
	switch (n->n_op) {
	case FUNARG:
	case UCALL:
	case UFORTCALL:
	case FORCE:
	case INIT:
	case ASSIGN:
	case INCR:
	case DECR:
	case CALL:
	case FORTCALL:
	case CBRANCH:
	case RETURN:
	case GOTO:
	case STCALL:
	case USTCALL:
	case STASG:
	case STARG:
		return 0;
	default:
		return 1;
	}
}

static NODE *
deluseless(NODE *p)
{
	struct interpass *ip;
	NODE *l, *r;

	if (optype(p->n_op) == LTYPE) {
		nfree(p);
		return NULL;
	}
	if (isuseless(p) == 0)
		return p;

	if (optype(p->n_op) == UTYPE) {
		l = p->n_left;
		nfree(p);
		return deluseless(l);
	}

	/* Be sure that both leaves may be valid */
	l = deluseless(p->n_left);
	r = deluseless(p->n_right);
	nfree(p);
	if (l && r) {
		/* Put left on queue first */
		ip = tmpalloc(sizeof(*ip));
		ip->type = IP_NODE;
		ip->lineno = 0; /* XXX */
		ip->ip_node = l;
		pass2_compile(ip);
		return r;
	} else if (l)
		return l;
	else if (r)
		return r;
	return NULL;
}

static void newblock(int myreg, int aoff);
static void epilogue(int regs, int autos, int retlab);

void
pass2_compile(struct interpass *ip)
{
	if (ip->type == IP_NODE) {
#ifdef PCC_DEBUG
		if (e2debug) {
			printf("pass2 called on:\n");
			fwalk(ip->ip_node, e2print, 0);
		}
#endif
		ip->ip_node = deluseless(ip->ip_node);
		if (ip->ip_node == NULL)
			return;
	}
	if (Oflag) {
		if (ip->type == IP_PROLOG)
			saving++;
		if (saving)
			return saveip(ip);
	}
	switch (ip->type) {
	case IP_NODE:
		thisline = ip->lineno;
		p2compile(ip->ip_node);
		tfree(ip->ip_node);
		break;
	case IP_PROLOG:
		prologue(ip->ip_regs, ip->ip_auto);
		break;
	case IP_NEWBLK:
		newblock(ip->ip_regs, ip->ip_auto);
		break;
	case IP_EPILOG:
		epilogue(ip->ip_regs, ip->ip_auto, ip->ip_retl);
		break;
	case IP_LOCCTR:
		setlocc(ip->ip_locc);
		break;
	case IP_DEFLAB:
		deflab(ip->ip_lbl);
		break;
	case IP_DEFNAM:
		defname(ip->ip_name, ip->ip_vis);
		break;
	case IP_ASM:
		printf("%s\n", ip->ip_asm);
		break;
	default:
		cerror("pass2_compile %d", ip->type);
	}
}

static void
newblock(int myreg, int aoff)
{
	setregs();
}

static void
epilogue(int regs, int autos, int retlab)
{
	eoftn(regs, autos, retlab);
}

/*
 * generate the code for p;
 * store may call codgen recursively
 * cookie is used to describe the context
 */
void
codgen(NODE *p, int cookie)
{
	int o;

	nodepole = p;
	canon(p);  /* creats OREG from * if possible and does sucomp */
#ifdef PCC_DEBUG
	if (e2debug) {
		printf("geninsn called on:\n");
		fwalk(p, e2print, 0);
	}
#endif
	do {
		geninsn(p, cookie); /* Assign instructions for tree */
#ifdef PCC_DEBUG
		if (udebug) {
			printf("sucomp called on:\n");
			fwalk(p, e2print, 0);
		}
#endif
	} while (sucomp(p) < 0);  /* Calculate sub-tree evaluation order */
#ifdef PCC_DEBUG
	if (udebug) {
		printf("genregs called on:\n");
		fwalk(p, e2print, 0);
	}
#endif
	/*
	 * When here it is known that the tree can be evaluated.
	 * Assign registers for all instructions.
	 */
	genregs(p); /* allocate registers for instructions */
#ifdef PCC_DEBUG
	if (udebug) {
		printf("gencode called on:\n");
		fwalk(p, e2print, 0);
	}
#endif
	switch (p->n_op) {
	case CBRANCH:
		o = p->n_left->n_op;
		gencode(p, FORCC);
		cbgen(o, p->n_right->n_lval);
		break;
	case FORCE:
		gencode(p->n_left, INTAREG|INTBREG);
		break;
	default:
		if (p->n_op != REG || p->n_type != VOID) /* XXX */
			gencode(p, FOREFF); /* Emit instructions */
	}
}

#ifdef PCC_DEBUG
char *cnames[] = {
	"SANY",
	"SAREG",
	"STAREG",
	"SBREG",
	"STBREG",
	"SCC",
	"SNAME",
	"SCON",
	"SFLD",
	"SOREG",
	"STARNM",
	"STARREG",
	"INTEMP",
	"FORARG",
	"SWADD",
	0,
};

/*
 * print a nice-looking description of cookie
 */
char *
prcook(int cookie)
{
	static char buf[50];
	int i, flag;

	if (cookie & SPECIAL) {
		switch (cookie) {
		case SZERO:
			return "SZERO";
		case SONE:
			return "SONE";
		case SMONE:
			return "SMONE";
		default:
			sprintf(buf, "SPECIAL+%d", cookie & ~SPECIAL);
			return buf;
		}
	}

	flag = 0;
	buf[0] = 0;
	for (i = 0; cnames[i]; ++i) {
		if (cookie & (1<<i)) {
			if (flag)
				strcat(buf, "|");
			++flag;
			strcat(buf, cnames[i]);
		}
	}
	return buf;
}

#endif

int odebug = 0;

void
geninsn(NODE *p, int cookie)
{
	NODE *p1, *p2;
	int o, rv;

#ifdef PCC_DEBUG
	if (odebug) {
		printf("geninsn(%p, %s)\n", p, prcook(cookie));
		fwalk(p, e2print, 0);
	}
#endif

again:	switch (o = p->n_op) {
	case EQ:
	case NE:
	case LE:
	case LT:
	case GE:
	case GT:
	case ULE:
	case ULT:
	case UGE:
	case UGT:
		if ((rv = relops(p)) < 0) {
			if (setbin(p))
				goto again;
			goto failed;
		}
		goto sw;

	case PLUS:
	case MINUS:
	case MUL:
	case DIV:
	case MOD:
	case AND:
	case OR:
	case ER:
	case LS:
	case RS:
		if ((rv = findops(p, cookie)) < 0) {
			if (setbin(p))
				goto again;
			goto failed;
		}
		goto sw;

	case INCR:
	case DECR:
		if ((rv = findops(p, cookie)) < 0) {
			if (setbin(p))
				goto again;
		} else
			goto sw;

		/*
		 * Rewrite x++ to (x = x + 1) -1;
		 */
		p1 = p->n_left;
		p->n_op = o == INCR ? MINUS : PLUS;
		/* Assign node */
		p2 = talloc();
		p2->n_type = p->n_type;
		p2->n_name = "";
		p2->n_op = ASSIGN;
		p->n_left = p2;
		p->n_left->n_left = p1;
		/* incr/decr node */
		p2 = talloc();
		p2->n_type = p->n_type;
		p2->n_name = "";
		p2->n_op = o == INCR ? PLUS : MINUS;
		p->n_left->n_right = p2;
		/* const one node */
		p->n_left->n_right->n_right = tcopy(p->n_right);
		/* input tree */
		p1 = tcopy(p1);
		/* idstrip(p1); */
		p->n_left->n_right->n_left = p1;
		goto again;

	case ASSIGN:
		if ((rv = findasg(p, cookie)) < 0) {
			if (setasg(p, cookie))
				goto again;
			goto failed;
		}
		/*
		 * Do subnodes conversions (if needed).
		 */
sw:		switch (rv & LMASK) {
		case LREG:
			geninsn(p->n_left, INTAREG|INTBREG);
			break;
		case LOREG:
			offstar(p->n_left->n_left);
			p->n_left->n_su = -1;
			break;
		case LTEMP:
			geninsn(p->n_left, INTEMP);
			break;
		}

		switch (rv & RMASK) {
		case RREG:
			geninsn(p->n_right, INTAREG|INTBREG);
			break;
		case ROREG:
			offstar(p->n_right->n_left);
			p->n_right->n_su = -1;
			break;
		case RTEMP:
			geninsn(p->n_right, INTEMP);
			break;
		}
		p->n_su = rv;
		break;

	case REG:
		if (istnode(p))
			comperr("geninsn REG");
		/* FALLTHROUGH */
	case NAME:
	case ICON:
	case OREG:
#if 0
		if ((cookie & (INTAREG|INTBREG)) == 0)
			comperr("geninsn OREG, node %p", p);
#endif
		if ((rv = findleaf(p, cookie)) < 0) {
			if (setasg(p, cookie))
				goto again;
			goto failed;
		}
		p->n_su = rv;
		break;

	case UMUL:
		/*
		 * If we end up here with an UMUL, try to fold it into
		 * an OREG anyway.
		 */
		if (p->n_type == STRTY) {
			/* XXX - what to do here? */
			geninsn(p->n_left, cookie);
			p->n_su = -1;
			break;
		}
#if 0
		if ((cookie & INTAREG) == 0)
			comperr("bad umul!");
#endif
		if (offstar(p->n_left)) {
			p->n_op = OREG;
			if ((rv = findleaf(p, cookie)) < 0)
				comperr("bad findleaf"); /* XXX */
			p->n_op = UMUL;
			p->n_su = rv | LOREG;
			break;
		}

	case COMPL:
	case UMINUS:
	case PCONV:
	case SCONV:
	case INIT:
	case GOTO:
	case FUNARG:
	case UCALL:
		if ((rv = finduni(p, cookie)) < 0) {
			if (setuni(p, cookie))
				goto again;
			goto failed;
		}
		switch (rv & LMASK) {
		case LREG:
			geninsn(p->n_left, INTAREG|INTBREG);
			break;
		case LOREG:
			offstar(p->n_left->n_left);
			p->n_left->n_su = -1;
			break;
		case LTEMP:
			geninsn(p->n_left, INTEMP);
			break;
		}
		p->n_su = rv;
		break;

	case CBRANCH:
		p1 = p->n_left;
		p2 = p->n_right;
		p1->n_label = p2->n_lval;
		o = p1->n_op;
		geninsn(p1, FORCC);
		p->n_su = -1; /* su calculations traverse left */
		break;

	case FORCE:
		geninsn(p->n_left, INTAREG|INTBREG);
		p->n_su = -1; /* su calculations traverse left */
		break;

	default:
		comperr("geninsn: bad op %d, node %p", o, p);
	}
	return;

failed:
	comperr("Cannot generate code, node %p op %s", p, opst[p->n_op]);

}

/*
 * Store a given subtree in a temporary location.
 * Return an OREG node where it is located.
 */
NODE *
store(NODE *p)
{
	NODE *q, *r, *s;

	q = talloc();
	r = talloc();
	s = talloc();
	q->n_op = OREG;
	q->n_type = p->n_type;
	q->n_name = "";
	q->n_rval = FPREG;
	q->n_lval = BITOOR(freetemp(szty(p->n_type)));
	*r = *q;
	s->n_op = ASSIGN;
	s->n_type = p->n_type;
	s->n_name = "";
	s->n_left = q;
	s->n_right = p;
	codgen(s, FOREFF);
	tfree(s);
	return r;
}

/*
 * Count the number of registers needed to evaluate a tree.
 * This is the trivial implementation, for machines with symmetric
 * registers. Machines with difficult register assignment strategies
 * will need to define this function themselves.
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
		/*
		 * Must store one subtree. Store the tree
		 * with highest SU, or left.
		 */
		if (right > left)
			p->n_right = store(p->n_right);
		else
			p->n_left = store(p->n_left);
		return -1;
	}
	if ((right+left) > fregs) {
		/* Almost out of regs, traverse the highest SU first */
		if (right > left)
			p->n_su |= DORIGHT;
	} else if (right && (q->needs & NASL) && (q->rewrite & RLEFT)) {
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
 * Rewrite node after instruction emit.
 */
static void
rewrite(NODE *p, int rewrite)
{
	NODE *l, *r;
	int o;

	if (p->n_su == -1)
		comperr("rewrite");

	l = getlr(p, 'L');
	r = getlr(p, 'R');
	o = p->n_op;
	p->n_op = REG;
	p->n_lval = 0;
	if (rewrite & RLEFT) {
#ifdef PCC_DEBUG
		if (l->n_op != REG)
			comperr("rewrite left");
#endif
		p->n_rval = l->n_rval;
	} else if (rewrite & RRIGHT) {
#ifdef PCC_DEBUG
		if (r->n_op != REG)
			comperr("rewrite right");
#endif
		p->n_rval = r->n_rval;
	} else if (rewrite & RESC1)
		p->n_rval = p->n_rall;
	else if (rewrite & RESC2)
		p->n_rval = p->n_rall + szty(p->n_type);
	else if (rewrite & RESC3)
		p->n_rval = p->n_rall + 2*szty(p->n_type);
	if (optype(o) != LTYPE)
		tfree(l);
	if (optype(o) == BITYPE)
		tfree(r);
}

void
gencode(NODE *p, int cookie)
{
	struct optab *q = &table[TBLIDX(p->n_su)];

	if (p->n_su == -1) /* For OREGs and similar */
		return gencode(p->n_left, cookie);

	if (p->n_su & DORIGHT) {
		gencode(p->n_right, INTAREG|INTBREG);
		if ((p->n_su & RMASK) == ROREG)
			canon(p);
	}
	if (p->n_su & LMASK) {
		gencode(p->n_left, INTAREG|INTBREG);
		if ((p->n_su & LMASK) == LOREG)
			canon(p);
	}
	if ((p->n_su & RMASK) && !(p->n_su & DORIGHT)) {
		gencode(p->n_right, INTAREG|INTBREG);
		if ((p->n_su & RMASK) == ROREG)
			canon(p);
	}
	expand(p, cookie, q->cstring);
	rewrite(p, q->rewrite);
}

int negrel[] = { NE, EQ, GT, GE, LT, LE, UGT, UGE, ULT, ULE } ;  /* negatives of relationals */

void
rcount()
{ /* count recursions */
	if( ++nrecur > NRECUR ){
		cerror( "expression causes compiler loop: try simplifying" );
	}
}

#ifdef PCC_DEBUG
#undef	PRTABLE
int
e2print(NODE *p, int down, int *a, int *b)
{
#ifdef PRTABLE
	extern int tablesize;
#endif

	*a = *b = down+1;
	while( down >= 2 ){
		fprintf(stderr, "\t");
		down -= 2;
		}
	if( down-- ) fprintf(stderr, "    " );


	fprintf(stderr, "%p) %s", p, opst[p->n_op] );
	switch( p->n_op ) { /* special cases */

	case REG:
		fprintf(stderr, " %s", rnames[p->n_rval] );
		break;

	case ICON:
	case NAME:
	case OREG:
		fprintf(stderr, " " );
		adrput(stderr, p );
		break;

	case STCALL:
	case USTCALL:
	case STARG:
	case STASG:
		fprintf(stderr, " size=%d", p->n_stsize );
		fprintf(stderr, " align=%d", p->n_stalign );
		break;
		}

	fprintf(stderr, ", " );
	tprint(stderr, p->n_type, p->n_qual);
	fprintf(stderr, ", " );
	if( p->n_rall == NOPREF ) fprintf(stderr, "NOPREF" );
	else {
		if( p->n_rall & MUSTDO ) fprintf(stderr, "MUSTDO " );
		else fprintf(stderr, "PREF " );
		fprintf(stderr, "%s", rnames[p->n_rall&~MUSTDO]);
		}
	fprintf(stderr, ", SU= %d(%s,%s,%s,%s)\n",
	    TBLIDX(p->n_su), 
#ifdef PRTABLE
	    TBLIDX(p->n_su) >= 0 && TBLIDX(p->n_su) <= tablesize ?
	    table[TBLIDX(p->n_su)].cstring : "",
#else
	    "",
#endif
	    ltyp[LMASK&p->n_su],
	    rtyp[(p->n_su&RMASK) >> 2], p->n_su & DORIGHT ? "DORIGHT" : "");
	return 0;
}
#endif

#ifndef FIELDOPS
/*
 * do this if there is no special hardware support for fields
 */
static int
ffld(NODE *p, int down, int *down1, int *down2 )
{
	/*
	 * look for fields that are not in an lvalue context,
	 * and rewrite them...
	 */
	NODE *shp;
	int s, o, v, ty;

	*down1 =  asgop( p->n_op );
	*down2 = 0;

	if( !down && p->n_op == FLD ){ /* rewrite the node */

		if( !rewfld(p) ) return 0;

		ty = (szty(p->n_type) == 2)? LONG: INT; /* XXXX */
		v = p->n_rval;
		s = UPKFSZ(v);
# ifdef RTOLBYTES
		o = UPKFOFF(v);  /* amount to shift */
# else
		o = szty(p->n_type)*SZINT - s - UPKFOFF(v);  /* amount to shift */
#endif

		/* make & mask part */

		p->n_left->n_type = ty;

		p->n_op = AND;
		p->n_right = talloc();
		p->n_right->n_op = ICON;
		p->n_right->n_rall = NOPREF;
		p->n_right->n_type = ty;
		p->n_right->n_lval = 1;
		p->n_right->n_rval = 0;
		p->n_right->n_name = "";
		p->n_right->n_lval <<= s;
		p->n_right->n_lval--;

		/* now, if a shift is needed, do it */

		if( o != 0 ){
			shp = talloc();
			shp->n_op = RS;
			shp->n_rall = NOPREF;
			shp->n_type = ty;
			shp->n_left = p->n_left;
			shp->n_right = talloc();
			shp->n_right->n_op = ICON;
			shp->n_right->n_rall = NOPREF;
			shp->n_right->n_type = ty;
			shp->n_right->n_rval = 0;
			shp->n_right->n_lval = o;  /* amount to shift */
			shp->n_right->n_name = "";
			p->n_left = shp;
			/* whew! */
		}
	}
	return 0;
}
#endif

/*
 * change left TEMPs into OREGs
 */
void
deltemp(NODE *p)
{
	NODE *l, *r;

	if (p->n_op == ADDROF) {
		/* TEMPs are already converted to OREGs */
		if ((l = p->n_left)->n_op != OREG)
			comperr("bad U&");
		p->n_op = PLUS;
		l->n_op = REG;
		l->n_type = INCREF(l->n_type);
		r = p->n_right = talloc();
		r->n_op = ICON;
		r->n_name = "";
		r->n_lval = l->n_lval;
		r->n_type = INT;
	}
}

/*
 * for pointer/integer arithmetic, set pointer at left node
 */
static void
setleft(NODE *p)          
{        
	NODE *q;

	/* only additions for now */
	if (p->n_op != PLUS)
		return;
	if (ISPTR(p->n_right->n_type) && !ISPTR(p->n_left->n_type)) {
		q = p->n_right;
		p->n_right = p->n_left;
		p->n_left = q;
	}
}

/*
 * look for situations where we can turn * into OREG
 */
void
oreg2(NODE *p)
{

	NODE *q;
	int r;
	char *cp;
	NODE *ql, *qr;
	CONSZ temp;

	if (Oflag == 0)
		deltemp(p);

	if (p->n_op == UMUL) {
		q = p->n_left;
		if (q->n_op == REG) {
			temp = q->n_lval;
			r = q->n_rval;
			cp = q->n_name;
			goto ormake;
		}

		if (q->n_op != PLUS && q->n_op != MINUS)
			return;
		ql = q->n_left;
		qr = q->n_right;

#ifdef R2REGS

		/* look for doubly indexed expressions */

		if( q->n_op == PLUS) {
			int i;
			if( (r=base(ql))>=0 && (i=offset(qr, tlen(p)))>=0) {
				makeor2(p, ql, r, i);
				return;
			} else if((r=base(qr))>=0 && (i=offset(ql, tlen(p)))>=0) {
				makeor2(p, qr, r, i);
				return;
			}
		}


#endif

		if( (q->n_op==PLUS || q->n_op==MINUS) && qr->n_op == ICON &&
				ql->n_op==REG && szty(qr->n_type)==1) {
			temp = qr->n_lval;
			if( q->n_op == MINUS ) temp = -temp;
			r = ql->n_rval;
			temp += ql->n_lval;
			cp = qr->n_name;
			if( *cp && ( q->n_op == MINUS || *ql->n_name ) ) return;
			if( !*cp ) cp = ql->n_name;

			ormake:
			if( notoff( p->n_type, r, temp, cp ) ) return;
			p->n_op = OREG;
			p->n_rval = r;
			p->n_lval = temp;
			p->n_name = cp;
			tfree(q);
			return;
		}
	}
}

void
canon(p) NODE *p; {
	/* put p in canonical form */

#ifndef FIELDOPS
	fwalk(p, ffld, 0);	/* look for field operators */
# endif
	walkf(p, setleft);	/* ptrs at left node for arithmetic */
	walkf(p, oreg2);	/* look for and create OREG nodes */
#ifdef MYCANON
	MYCANON(p);		/* your own canonicalization routine(s) */
#endif

}

/*
 * Find the best ops for a given tree. 
 * Different instruction sequences are graded as:
  	add2 reg,reg	 = 0
	add2 mem,reg	 = 1
	add3 mem,reg,reg = 2
	add3 reg,mem,reg = 2
	add3 mem,mem,reg = 3
	move mem,reg ; add2 mem,reg 	= 4
	move mem,reg ; add3 mem,reg,reg = 5
	move mem,reg ; move mem,reg ; add2 reg,reg = 6
	move mem,reg ; move mem,reg ; add3 reg,reg,reg = 7
 * The instruction with the lowest grading is emitted.
 */
int
findops(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl, shr, tl, tr, is3, rsr, rsl, osr, osl;
	NODE *l, *r;
	int *ixp;
	int rv = -1, mtchno = 10;

if (f2debug) printf("findops tree:\n");
if (f2debug) fwalk(p, e2print, 0);

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("findop: ixp %d\n", ixp[i]);
		l = getlr(p, 'L');
		r = getlr(p, 'R');
		if (ttype(l->n_type, q->ltype) == 0 ||
		    ttype(r->n_type, q->rtype) == 0)
			continue; /* Types must be correct */

if (f2debug) printf("findop got types\n");
		shl = tshape(l, q->lshape);
		if ((q->lshape & SPECIAL) == 0) {
			rsl = (q->lshape & (SAREG|STAREG)) != 0;
			osl = (q->lshape & SOREG) && l->n_op == UMUL;
		} else
			rsl = osl = 0;
		
		if (shl == 0 && rsl == 0)
			continue; /* useless */
if (f2debug) printf("findop lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);
		shr = tshape(r, q->rshape);
		if ((q->rshape & SPECIAL) == 0) {
			rsr = (q->rshape & (SAREG|STAREG)) != 0;
			osr = (q->rshape & SOREG) && r->n_op == UMUL;
		} else
			rsl = osl = 0;
		if (shr == 0 && rsr == 0)
			continue; /* useless */
if (f2debug) printf("findop rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		tl = istnode(p->n_left);
		tr = istnode(p->n_right);
		is3 = ((q->needs & (NDLEFT|NDRIGHT)) == 0);

		if (shl && shr) {
			int got = 10;
			/*
			 * Both shapes matches. If one of them is in a
			 * temp register and there is a corresponding
			 * 2-op instruction, be very happy. If not, but
			 * there is a 3-op instruction that ends in a reg,
			 * be quite happy. If neither, cannot do anything.
			 */
			if (tl && (q->needs & NDLEFT)) {
				got = 1;
			} else if (tr && (q->needs & NDRIGHT)) {
				got = 1;
			} else if ((q->needs & (NDLEFT|NDRIGHT)) == 0) {
				got = 3;
			}
			if (got < mtchno) {
				mtchno = got;
				rv = MKIDX(ixp[i], 0);
			}
			if (got != 10)
				continue;
		}
if (f2debug) printf("second\n");
		if (shr) {
			/*
			 * Right shape matched. If left node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if ((q->needs & NDRIGHT) && istnode(r)) {
				/* put left in temp, add to right */
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], LREG);
				}
			} else if (q->needs & NDLEFT) {
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], LREG);
				}
				continue; /* Can't do anything else */
			} else if (is3) {
				if (5 < mtchno) {
					mtchno = 5;
					rv = MKIDX(ixp[i], LREG);
				}
				continue; /* Can't do anything else */
			}
		}
if (f2debug) printf("third\n");
		if (shl) {
			/*
			 * Left shape matched. If right node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if ((q->needs & NDLEFT) && istnode(l)) {
				/* put right in temp, add to left */
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], RREG);
				}
			} else if (q->needs & NDRIGHT) {
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], RREG);
				}
				continue; /* Can't do anything */
			} else if (is3) {
				if (5 < mtchno) {
					mtchno = 5;
					rv = MKIDX(ixp[i], RREG);
				}
				continue; /* Can't do anything */
			}
		}
		/*
		 * Neither of the shapes matched. Put both args in 
		 * regs and be done with it.
		 */
		if (rsr && rsl) { /* both can be in reg */
			if (is3) {
				if (7 < mtchno) {
					mtchno = 7;
					rv = MKIDX(ixp[i], RREG|LREG);
				}
			} else {
				if (6 < mtchno) {
					mtchno = 6;
					rv = MKIDX(ixp[i], RREG|LREG);
				}
			}
		}
	}
#ifdef PCC_DEBUG
	if (f2debug) {
		if (rv == -1)
			printf("findops failed\n");
		else
			printf("findops entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}

/*
 * Find the best relation op for matching the two trees it has.
 * This is a sub-version of the function findops() above.
 * The instruction with the lowest grading is emitted.
 */
int
relops(NODE *p)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl, shr, rsr, rsl;
	NODE *l, *r;
	int *ixp;
	int rv = -1, mtchno = 10;

if (f2debug) printf("relops tree:\n");
if (f2debug) fwalk(p, e2print, 0);

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("relops: ixp %d\n", ixp[i]);
		l = getlr(p, 'L');
		r = getlr(p, 'R');
		if (ttype(l->n_type, q->ltype) == 0 ||
		    ttype(r->n_type, q->rtype) == 0)
			continue; /* Types must be correct */

if (f2debug) printf("relops got types\n");
		shl = tshape(l, q->lshape);
		rsl = (q->lshape & (SAREG|STAREG)) != 0 &&
		    (q->lshape & SPECIAL) == 0;
		if (shl == 0 && rsl == 0)
			continue; /* useless */
if (f2debug) printf("relops lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);
		shr = tshape(r, q->rshape);
		rsr = (q->rshape & (SAREG|STAREG)) != 0 &&
		    (q->rshape & SPECIAL) == 0;
		if (shr == 0 && rsr == 0)
			continue; /* useless */
if (f2debug) printf("relops rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (shl && shr) {
			/*
			 * Both shapes matches directly. For relops this
			 * is the best match; just return.
			 */
			rv = MKIDX(ixp[i], 0);
			break;
		}
if (f2debug) printf("second\n");
		if (shr) {
			/*
			 * Right shape matched. If left node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if (4 < mtchno) {
				mtchno = 4;
				rv = MKIDX(ixp[i], LREG);
			}
			continue; /* nothing more to do */
		}
if (f2debug) printf("third\n");
		if (shl) {
			/*
			 * Left shape matched. If right node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if (4 < mtchno) {
				mtchno = 4;
				rv = MKIDX(ixp[i], RREG);
			}
			continue; /* nothing more to do */
		}
		if (6 < mtchno) {
			mtchno = 6;
			rv = MKIDX(ixp[i], RREG|LREG);
		}
	}
#ifdef PCC_DEBUG
	if (f2debug) {
		if (rv == -1)
			printf("relops failed\n");
		else
			printf("relops entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}

/*
 * Find a matching assign op.
 *
 * Level assignment for priority:
 * 	left	right	prio
 *	-	-	-
 *	direct	direct	1
 *	direct	REG	2
 *	direct	OREG	3
 *	OREG	direct	4
 *	OREG	REG	5
 *	OREG	OREG	6
 */
int
findasg(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl, shr, rsr, lvl = 10;
	NODE *l, *r;
	int *ixp;
	int rv = -1;

#ifdef PCC_DEBUG
	if (f2debug) {
		printf("findasg tree: %s\n", prcook(cookie));
		fwalk(p, e2print, 0);
	}
#endif

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("asgop: ixp %d\n", ixp[i]);
		l = getlr(p, 'L');
		r = getlr(p, 'R');
		if (ttype(l->n_type, q->ltype) == 0 ||
		    ttype(r->n_type, q->rtype) == 0)
			continue; /* Types must be correct */

		if ((cookie & (INTAREG|INTBREG)) &&
		    (q->rewrite & (RLEFT|RRIGHT)) == 0)
			continue; /* must get a result somehere */

if (f2debug) printf("asgop got types\n");
		shl = tshape(l, q->lshape);
		if (shl == 0) {
			/* See if this can end up as an OREG */
			if (p->n_left->n_op != UMUL)
				continue;
			if ((q->lshape & SOREG) == 0)
				continue;
		}

if (f2debug) printf("asgop lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);

		shr = tshape(r, q->rshape);
		rsr = (q->rshape & (SAREG|STAREG)) != 0 &&
		    (q->rshape & SPECIAL) == 0;
		if (shr == 0 && rsr == 0)
			continue; /* useless */
if (f2debug) printf("asgop rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (shl && shr) {
			/*
			 * Both shapes matches.
			 * Ideal situation, encode and be done with it.
			 */
			rv = MKIDX(ixp[i], 0);
			break;
		}
if (f2debug) printf("second\n");
		if (shl) {
			/*
			 * Left shape matched. Right node must be put into
			 * a temporary register.
			 */
			if (lvl < 3)
				continue;
			lvl = 3;
			rv = MKIDX(ixp[i], RREG);
			continue;
		}
		if (shr) {
			if (lvl < 4)
				continue;
			lvl = 4;
			rv = MKIDX(ixp[i], LOREG);
			continue;
		}
		if (lvl < 6)
			continue;
		lvl = 6;
		rv = MKIDX(ixp[i], LOREG|RREG);
	}
#ifdef PCC_DEBUG
	if (f2debug) {
		if (rv == -1)
			printf("findasg failed\n");
		else
			printf("findasg entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}

/*
 * Find an ASSIGN node that puts the value into a register.
 */
int
findleaf(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl;
	int *ixp;
	int rv = -1;

#ifdef PCC_DEBUG
	if (f2debug) {
		printf("findleaf tree: %s\n", prcook(cookie));
		fwalk(p, e2print, 0);
	}
#endif

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("findleaf: ixp %d\n", ixp[i]);
		if (ttype(p->n_type, q->rtype) == 0)
			continue; /* Type must be correct */

if (f2debug) printf("findleaf got types\n");
		shl = tshape(p, q->rshape);
		if (shl == 0)
			continue; /* shape must match */

if (f2debug) printf("findleaf got shapes %d\n", shl);

		if (q->needs & REWRITE)
			break;	/* Done here */

		rv = MKIDX(ixp[i], 0);
		break;
	}
	if (f2debug) { 
		if (rv == -1)
			printf("findleaf failed\n");
		else
			printf("findleaf entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
	return rv;
}

/*
 * Find a UNARY op that satisfy the needs.
 * For now, the destination is always a register.
 * Both source and dest types must match, but only source (left)
 * shape is of interest.
 */
int
finduni(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	NODE *l, *r;
	int i, shl, rsl;
	int *ixp;
	int rv = -1;

#ifdef PCC_DEBUG
	if (f2debug) {
		printf("finduni tree: %s\n", prcook(cookie));
		fwalk(p, e2print, 0);
	}
#endif

	l = getlr(p, 'L');
	r = getlr(p, 'R');
	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("finduni: ixp %d\n", ixp[i]);
		if (ttype(l->n_type, q->ltype) == 0)
			continue; /* Type must be correct */

if (f2debug) printf("finduni got left type\n");
		if (ttype(r->n_type, q->rtype) == 0)
			continue; /* Type must be correct */

if (f2debug) printf("finduni got types\n");
		shl = tshape(l, q->lshape);
		rsl = (q->lshape & (SAREG|STAREG)) != 0 &&
		    (q->lshape & SPECIAL) == 0;
		if (shl == 0 && rsl == 0)
			continue; /* shape or regs must match */

if (f2debug) printf("finduni got shapes %d\n", shl);
		if (q->needs & REWRITE)
			break;	/* Done here */

		rv = MKIDX(ixp[i], shl ? 0 : LREG);
		if (shl)
			break;
	}
#ifdef PCC_DEBUG
	if (f2debug) { 
		if (rv == -1)
			printf("finduni failed\n");
		else
			printf("finduni entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}

void
comperr(char *str, ...)
{
	extern char *ftitle;
	va_list ap;

	va_start(ap, str);
	fprintf(stderr, "%s, line %d: compiler error: ", ftitle, thisline);
	vfprintf(stderr, str, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	fwalk(nodepole, e2print, 0);
	exit(1);
}

/*
 * allocate k integers worth of temp space
 * we also make the convention that, if the number of words is
 * more than 1, it must be aligned for storing doubles...
 * Returns bits offset from base register.
 * XXX - redo this.
 */
int
freetemp(int k)
{
#ifndef BACKTEMP
	int t;

	if (k > 1)
		SETOFF(autooff, ALDOUBLE);

	t = autooff;
	autooff += k*SZINT;
	if (autooff > maxautooff)
		maxautooff = autooff;
	return (t);

#else
	autooff += k*SZINT;
	if (k > 1)
		SETOFF(autooff, ALDOUBLE);

	if (autooff > maxautooff)
		maxautooff = autooff;
	return( -autooff );
#endif
	}

