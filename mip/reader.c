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

/*
 * processing order for nodes:
 * - myreader()
 * - mkhardops()
 * - gencall()
 * - delay()
 * - canon()
 * - deluseless()
 * - saves trees here if optimizing
 */

# include "pass2.h"

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

/*	some storage declarations */
int nrecur;
int lflag;
int x2debug;
int udebug = 0;
int ftnno;
int thisline;
int fregs;
int p2autooff, p2maxautooff;

NODE *nodepole;
int saving;

int e2print(NODE *p, int down, int *a, int *b);
void saveip(struct interpass *ip);
void deljumps(void);
void deltemp(NODE *p);
void mkhardops(NODE *p);
void optdump(struct interpass *ip);
void cvtemps(struct interpass *epil);
int findops(NODE *p, int);
int findasg(NODE *p, int);
int finduni(NODE *p, int);
int findleaf(NODE *p, int);
int relops(NODE *p);
int asgops(NODE *p, int);
NODE *store(NODE *);
void rcount(void);
void compile2(struct interpass *ip);
void compile3(struct interpass *ip);
void compile4(struct interpass *ip);
struct interpass delayq;

static void gencode(NODE *p, int cookie);

static char *ltyp[] = { "", "LREG", "LOREG", "LTEMP" };
static char *rtyp[] = { "", "RREG", "ROREG", "RTEMP" };

/* used when removing nodes */
struct tmpsave {
	struct tmpsave *next;
	CONSZ tempaddr;
	int tempno;
} *tmpsave;

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
	if (p->n_op ==CALL)
		cerror("non-UCALL node");
}
#endif

/*
 * See if post-decrement and post-increment operators can be delayed
 * past this statement.  This is only possible if it do not end up
 * after a function call.
 * There may be instructions that will do post-in/decrement, therefore
 * call special routines to check if we can do this.
 */
void
delay(NODE *p)
{
	struct interpass *ip;
	NODE *q;

	int ty = optype(p->n_op);

	switch (p->n_op) {
	case UCALL:
	case STCALL:
	case USTCALL:
	case FORTCALL:
	case UFORTCALL:
	case CBRANCH:
		/* for the moment, don't delay past a conditional context, or
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
			ip = ipnode(tcopy(p));
			DLIST_INSERT_BEFORE(&delayq, ip, qelem);
			q = p->n_left;
			nfree(p->n_right); /* zap constant */
			*p = *q;
			nfree(q);
			return;
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

/*
 * Delete statements with no meaning (like a+b; or 513.4;)
 */
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

/*
 * Receives interpass structs from pass1.
 */
void
pass2_compile(struct interpass *ip)
{

	if (ip->type == IP_NODE) {
		myreader(ip->ip_node); /* local massage of input */
		mkhardops(ip->ip_node);
		gencall(ip->ip_node, NIL);
		compile2(ip);
	} else
		compile4(ip);
}

void
compile2(struct interpass *ip)
{
	DLIST_INIT(&delayq, qelem);
	delay(ip->ip_node);
	compile3(ip);
	while (DLIST_NEXT(&delayq, qelem) != &delayq) {
		ip = DLIST_NEXT(&delayq, qelem);
		DLIST_REMOVE(ip, qelem);
		compile3(ip);
	}
}

void
compile3(struct interpass *ip)
{
	NODE *p = ip->ip_node;

	canon(p);
	if ((p = deluseless(p)) == NULL)
		return; /* nothing to do */
#ifdef PCC_DEBUG
	walkf(p, cktree);
	if (e2debug) {
		fprintf(stderr, "Entering pass2\n");
		fwalk(p, e2print, 0);
	}
#endif
	ip->ip_node = p;
	compile4(ip);
}

void
compile4(struct interpass *ip)
{
	if (xsaveip) 
		return saveip(ip);

	emit(ip);
}

void
emit(struct interpass *ip)
{
	NODE *p;
	int o, savautooff;

	switch (ip->type) {
	case IP_NODE:
		p = ip->ip_node;

		if (xsaveip == 0) {
			savautooff = p2autooff;
if (xnewreg == 0) {
			do {
				geninsn(p, FOREFF);
			} while (sucomp(p) < 0);

			genregs(p); /* allocate registers for instructions */
			mygenregs(p);
} else {
			do {
				extern int tempmin, tempmax;

				geninsn(ip->ip_node, FOREFF);
				tempmin = tempmax = 10; /* XXX maxreg */
				nsucomp(ip->ip_node);
			} while (ngenregs(ip, ip));
}
			p2autooff = savautooff;
		}

		switch (p->n_op) {
		case CBRANCH:
			/* Only emit branch insn if RESCC */
			if (table[TBLIDX(p->n_left->n_su)].rewrite & RESCC) {
				o = p->n_left->n_op;
				gencode(p, FORCC);
				cbgen(o, p->n_right->n_lval);
			} else
				gencode(p, FORCC);
			break;
		case FORCE:
			gencode(p->n_left, INTAREG|INTBREG);
			break;
		default:
			if (p->n_op != REG || p->n_type != VOID) /* XXX */
				gencode(p, FOREFF); /* Emit instructions */
		}

		tfree(p);
		break;
	case IP_PROLOG:
		prologue((struct interpass_prolog *)ip);
		break;
	case IP_EPILOG:
		eoftn((struct interpass_prolog *)ip);
		tmpsave = NULL;	/* Always forget old nodes */
		p2maxautooff = p2autooff = AUTOINIT;
		break;
	case IP_STKOFF:
		if (xsaveip == 0) {
			p2autooff = ip->ip_off;
			if (p2autooff > p2maxautooff)
				p2maxautooff = p2autooff;
		}
		break;
	case IP_DEFLAB:
		deflab(ip->ip_lbl);
		break;
	case IP_ASM:
		printf("\t%s\n", ip->ip_asm);
		break;
	case IPSTK:
		break;
	default:
		cerror("compile4 %d", ip->type);
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
		} else {
			/* Do tree rewriting to ensure correct incr */
			if ((rv & LMASK) != LREG)
				goto sw;
		}
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
		canon(p); /* if fields involved */
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
			if (p->n_left->n_op == FLD) {
				offstar(p->n_left->n_left->n_left);
				p->n_left->n_left->n_su = -1;
			} else
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
		/* FALLTHROUGH */

	case COMPL:
	case UMINUS:
	case PCONV:
	case SCONV:
	case INIT:
	case GOTO:
	case FUNARG:
	case UCALL:
	case USTCALL:
		if ((rv = finduni(p, cookie)) < 0) {
			if (setuni(p, cookie))
				goto again;
			goto failed;
		}
		switch (rv & LMASK) {
		case LREG:
			cookie = INTAREG|INTBREG;
			if (rv & LBREG) /* XXX - make prettier */
				cookie = INTBREG;
			geninsn(p->n_left, cookie);
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
	extern struct interpass *storesave;
	struct interpass *ip;
	NODE *q, *r;
	int s;

	s = BITOOR(freetemp(szty(p->n_type)));
	q = mklnode(OREG, s, FPREG, p->n_type);
	r = mklnode(OREG, s, FPREG, p->n_type);
	ip = ipnode(mkbinode(ASSIGN, q, p, p->n_type));

	if (xsaveip)
		storesave = ip;
	else
		emit(ip);
	return r;
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
	p->n_name = "";
	if (rewrite & RLEFT) {
#ifdef PCC_DEBUG
		if (l->n_op != REG)
			comperr("rewrite left");
#endif
		p->n_rall = p->n_rval = l->n_rval;
	} else if (rewrite & RRIGHT) {
#ifdef PCC_DEBUG
		if (r->n_op != REG)
			comperr("rewrite right");
#endif
		p->n_rall = p->n_rval = r->n_rval;
	} else if (rewrite & RESC1)
		p->n_rval = p->n_rall;
	else if (rewrite & RESC2)
		p->n_rval = p->n_rall + szty(p->n_type);
	else if (rewrite & RESC3)
		p->n_rval = p->n_rall + 2*szty(p->n_type);
	else if (p->n_su == DORIGHT)
		p->n_rall = p->n_rval = l->n_rval; /* XXX special */
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
		else if (xnewreg && (p->n_su & RMASK) == RREG &&
		    (q->rewrite & RRIGHT) && p->n_right->n_rall != p->n_rall) {
			rmove(p->n_right->n_rall, p->n_rall, p->n_type);
			p->n_right->n_rall = p->n_rall;
			p->n_right->n_rval = p->n_rall;
		}
	}
	if (p->n_su & LMASK) {
		gencode(p->n_left, INTAREG|INTBREG);
		if ((p->n_su & LMASK) == LOREG) {
			canon(p);
		} else if (xnewreg && (p->n_su & LMASK) == LREG &&
		    (q->rewrite & RLEFT) && p->n_left->n_rall != p->n_rall) {
			rmove(p->n_left->n_rall, p->n_rall, p->n_type);
			p->n_left->n_rall = p->n_rall;
			p->n_left->n_rval = p->n_rall;
		}
	}
	if ((p->n_su & RMASK) && !(p->n_su & DORIGHT)) {
		gencode(p->n_right, INTAREG|INTBREG);
		if ((p->n_su & RMASK) == ROREG)
			canon(p);
		else if (xnewreg && (p->n_su & RMASK) == RREG &&
		    (q->rewrite & RRIGHT) && p->n_right->n_rall != p->n_rall) {
			rmove(p->n_right->n_rall, p->n_rall, p->n_type);
			p->n_right->n_rall = p->n_rall;
			p->n_right->n_rval = p->n_rall;
		}
	}
	expand(p, cookie, q->cstring);
	if (xnewreg && callop(p->n_op) && p->n_rall != RETREG)
		rmove(RETREG, p->n_rall, p->n_type);

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

	case TEMP:
		fprintf(stderr, " " CONFMT, p->n_lval);
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
		if ((p->n_rall&~MUSTDO) > 8) /* XXX */
		fprintf(stderr, "(%d)", (p->n_rall&~MUSTDO));
		else fprintf(stderr, "%s", rnames[p->n_rall&~MUSTDO]);
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
		p->n_right = mklnode(ICON, (1 << s)-1, 0, ty);

		/* now, if a shift is needed, do it */

		if( o != 0 ){
			shp = mkbinode(RS, p->n_left,
			    mklnode(ICON, o, 0, ty), ty);
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
	struct tmpsave *w;
	NODE *l;

	if (p->n_op == TEMP) {
		/* Check if already existing */
		for (w = tmpsave; w; w = w->next)
			if (w->tempno == p->n_lval)
				break;
		if (w == NULL) {
			/* new on stack */
			w = tmpalloc(sizeof(struct tmpsave));
			w->tempno = p->n_lval;
			w->tempaddr = BITOOR(freetemp(szty(p->n_type)));
			w->next = tmpsave;
			tmpsave = w;
		}
		p->n_op = OREG;
		p->n_rval = FPREG;
		p->n_lval = w->tempaddr;
	} else if (p->n_op == ADDROF) {
		/* TEMPs are already converted to OREGs */
		if ((l = p->n_left)->n_op != OREG)
			comperr("bad U&");
		p->n_op = PLUS;
		l->n_op = REG;
		l->n_type = INCREF(l->n_type);
		p->n_right = mklnode(ICON, l->n_lval, 0, INT);
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

	if (xsaveip == 0)
		walkf(p, deltemp);
	walkf(p, setleft);	/* ptrs at left node for arithmetic */
	walkf(p, oreg2);	/* look for and create OREG nodes */
#ifndef FIELDOPS
	fwalk(p, ffld, 0);	/* look for field operators */
# endif
#ifdef MYCANON
	MYCANON(p);		/* your own canonicalization routine(s) */
#endif

}

static int shltab[] = { 0, 0, LOREG, LREG };
static int shrtab[] = { 0, 0, ROREG, RREG };

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
	int i, shl, shr, tl, tr, is3;
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
		if ((shl = tshape(l, q->lshape)) == SRNOPE)
			continue; /* useless */
if (f2debug) printf("findop lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);
		if ((shr = tshape(r, q->rshape)) == SRNOPE)
			continue; /* useless */
if (f2debug) printf("findop rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		tl = istnode(p->n_left);
		tr = istnode(p->n_right);
		is3 = ((q->rewrite & (RLEFT|RRIGHT)) == 0);

		if (shl == SRDIR && shr== SRDIR ) {
			int got = 10;
			/*
			 * Both shapes matches direct. If one of them is
			 * in a temp register and there is a corresponding
			 * 2-op instruction, be very happy. If not, but
			 * there is a 3-op instruction that ends in a reg,
			 * be quite happy. If neither, cannot do anything.
			 */
			if (tl && (q->rewrite & RLEFT)) {
				got = 1;
			} else if (tr && (q->rewrite & RRIGHT)) {
				got = 1;
			} else if ((q->rewrite & (RLEFT|RRIGHT)) == 0) {
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
		if (shr == SRDIR) {
			/*
			 * Right shape matched. If left node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if ((q->rewrite & RRIGHT) && istnode(r)) {
				/* put left in temp, add to right */
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], shltab[shl]);
				}
			} else if (q->rewrite & RLEFT) {
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], LREG);
				}
				continue; /* Can't do anything else */
			} else if (is3) {
				if (5 < mtchno) {
					mtchno = 5;
					rv = MKIDX(ixp[i], shltab[shl]);
				}
				continue; /* Can't do anything else */
			}
		}
if (f2debug) printf("third\n");
		if (shl == SRDIR) {
			/*
			 * Left shape matched. If right node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if ((q->rewrite & RLEFT) && istnode(l)) {
				/* put right in temp, add to left */
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], shrtab[shr]);
				}
			} else if (q->rewrite & RRIGHT) {
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], RREG);
				}
				continue; /* Can't do anything */
			} else if (is3) {
				if (5 < mtchno) {
					mtchno = 5;
					rv = MKIDX(ixp[i], shrtab[shr]);
				}
				continue; /* Can't do anything */
			}
		}
		/*
		 * Neither of the shapes matched. Put both args in 
		 * regs and be done with it.
		 */
		if (is3) {
			if (7 < mtchno) {
				mtchno = 7;
				rv = MKIDX(ixp[i], shltab[shl]|shrtab[shr]);
			}
		} else {
			if (6 < mtchno) {
				mtchno = 6;
				if (q->rewrite & RLEFT)
					rv = MKIDX(ixp[i], shrtab[shr]|LREG);
				else
					rv = MKIDX(ixp[i], shltab[shl]|RREG);
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
 *
 * Level assignment for priority:
 *	left	right	prio
 *	-	-	-
 *	direct	direct	1
 *	direct	OREG	2	# make oreg
 *	OREG	direct	2	# make oreg
 *	OREG	OREG	2	# make both oreg
 *	direct	REG	3	# put in reg
 *	OREG	REG	3	# put in reg, make oreg
 *	REG	direct	3	# put in reg
 *	REG	OREG	3	# put in reg, make oreg
 *	REG	REG	4	# put both in reg
 */
int
relops(NODE *p)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl, shr;
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
		if (shl == 0)
			continue; /* useless */
if (f2debug) printf("relops lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);
		shr = tshape(r, q->rshape);
		if (shr == 0)
			continue; /* useless */
if (f2debug) printf("relops rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (shl+shr < mtchno) {
			mtchno = shl+shr;
			rv = MKIDX(ixp[i], shltab[shl]|shrtab[shr]);
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
	int i, shl, shr, lvl = 10;
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
		if ((shl = tshape(l, q->lshape)) == SRNOPE || shl == SRREG)
			continue;
if (f2debug) printf("asgop lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);

		if ((shr = tshape(r, q->rshape)) == SRNOPE)
			continue; /* useless */

if (f2debug) printf("asgop rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (lvl <= (shl + shr))
			continue;
		lvl = shl + shr;
		
		rv = MKIDX(ixp[i], shltab[shl]|shrtab[shr]);
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
		if ((shl = tshape(p, q->rshape)) != SRDIR)
			continue; /* shape must match */

		if ((q->visit & cookie) == 0)
			continue; /* wrong registers */

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
	int i, shl, num = 4;
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
		if ((shl = tshape(l, q->lshape)) == SRNOPE)
			continue; /* shape must match */

if (f2debug) printf("finduni got shapes %d\n", shl);
		if (q->rewrite & RLEFT) {
			/* left node must be in a temp register */
			if (l->n_op == REG && !istreg(l->n_rval))
				shl = SRREG;
		}

		if ((cookie & q->visit) == 0)	/* check correct return value */
			continue;		/* XXX - should check needs */

if (f2debug) printf("finduni got cookie\n");
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (shl >= num)
			continue;
		num = shl;
		rv = MKIDX(ixp[i], shltab[shl]);
		if ((q->lshape & (SBREG|STBREG)) &&
		    !(q->lshape & (SAREG|STAREG)))
			rv |= LBREG;
		if (shl == SRDIR)
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

	if (nodepole && nodepole->n_op != FREE)
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
		SETOFF(p2autooff, ALDOUBLE);

	t = p2autooff;
	p2autooff += k*SZINT;
	if (p2autooff > p2maxautooff)
		p2maxautooff = p2autooff;
	return (t);

#else
	p2autooff += k*SZINT;
	if (k > 1)
		SETOFF(p2autooff, ALDOUBLE);

	if (p2autooff > p2maxautooff)
		p2maxautooff = p2autooff;
	return( -p2autooff );
#endif
	}

/*
 * Convert unimplemented ops to function calls.
 */
void
mkhardops(NODE *p)
{
	NODE *r, *l, *q;
	struct hardops *hop;
	int ty = optype(p->n_op);

	if (ty == UTYPE)
		return mkhardops(p->n_left);
	if (ty != BITYPE)
		return;
	for (hop = hardops; hop->op != 0; hop++)
		if (hop->op == p->n_op && hop->type == p->n_type)
			break;

	/* Traverse down only if find hardop failed */
	/* otherwise traversal down will be done in pass2_compile() */
	if (hop->op == 0) {
		mkhardops(p->n_left);
		mkhardops(p->n_right);
		return;
	}

	l = p->n_left;
	if (p->n_op == STASG) {
		if (l->n_op == UMUL) {
			/* make it a pointer reference */
			r = l;
			l = l->n_left;
			nfree(r);
		} else if (l->n_op == NAME) {
			l->n_op = ICON; /* Constant reference */
			l->n_type = INCREF(l->n_type);
		} else
			comperr("STASG mot UMUL");
	}
	r = p->n_right;

	/*
	 * node p must be converted to a call to fun.
	 * arguments first.
	 */
	q = mkbinode(CM, l, r, 0);

	if (p->n_op == STASG) {
		/* Must push the size */
		
		l = mklnode(ICON, p->n_stsize, 0, INT);
		r = mkbinode(CM, q, l, 0);
		q = r;
	}
	p->n_op = CALL;
	p->n_right = q;

	/* Make function name node */
	p->n_left = mklnode(ICON, 0, 0, 0);
	p->n_left->n_name = hop->fun;
	/* Done! */
}

NODE *
mklnode(int op, CONSZ lval, int rval, TWORD type)
{
	NODE *p = talloc();

	p->n_name = "";
	p->n_qual = 0;
	p->n_op = op;
	p->n_lval = lval;
	p->n_rval = rval;
	p->n_type = type;
	return p;
}

NODE *
mkbinode(int op, NODE *left, NODE *right, TWORD type)
{
	NODE *p = talloc();

	p->n_name = "";
	p->n_qual = 0;
	p->n_op = op;
	p->n_left = left;
	p->n_right = right;
	p->n_type = type;
	return p;
}

NODE *
mkunode(int op, NODE *left, int rval, TWORD type)
{
	NODE *p = talloc();

	p->n_name = "";
	p->n_qual = 0;
	p->n_op = op;
	p->n_left = left;
	p->n_rval = rval;
	p->n_type = type;
	return p;
}

struct interpass *
ipnode(NODE *p)
{
	struct interpass *ip = tmpalloc(sizeof(struct interpass));

	ip->ip_node = p;
	ip->type = IP_NODE;
	ip->lineno = thisline;
	return ip;
}
