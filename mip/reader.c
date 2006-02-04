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
int thisline;
int fregs;
int p2autooff, p2maxautooff;

NODE *nodepole;
FILE *prfil = stdout;

int e2print(NODE *p, int down, int *a, int *b);
void saveip(struct interpass *ip);
void deljumps(void);
void deltemp(NODE *p);
void mkhardops(NODE *p);
void optdump(struct interpass *ip);
void cvtemps(struct interpass *epil);
NODE *store(NODE *);
void rcount(void);
void compile2(struct interpass *ip);
void compile3(struct interpass *ip);
void compile4(struct interpass *ip);
struct interpass delayq;

static void gencode(NODE *p, int cookie);

char *ltyp[] = { "", "LREG", "LOREG", "LTEMP" };
char *rtyp[] = { "", "RREG", "ROREG", "RTEMP" };

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

static struct interpass ipole;
struct interpass_prolog *epp;

#if 0
static NODE *
fixargs(NODE *p, struct interpass *ip)
{
	struct interpass *ip2;
	int num;

	if (canaddr(p))
		return p;
	num = epp->ip_tmpnum++;
	ip2 = ipnode(mkbinode(ASSIGN, 
	    mklnode(TEMP, num, 0, p->n_type), p, p->n_type));
	DLIST_INSERT_BEFORE(ip, ip2, qelem);
	return mklnode(TEMP, num, 0, p->n_type);
}

/*
 * Gencall() is the first function in pass2.  It cuts out the 
 * call arguments and replaces them with temp variables.
 */
static void
xgencall(NODE *p, struct interpass *ip)
{
	int o = p->n_op;
	int ty = optype(o);

	if (ty == LTYPE)
		return;

	if (ty != UTYPE)
		xgencall(p->n_right, ip);
	xgencall(p->n_left, ip);

	if (o != CALL)
		return;

	if (p->n_right->n_op == CM) {
		p = p->n_right;
		while (p->n_left->n_op == CM) {
			p->n_right = fixargs(p->n_right, ip);
			p = p->n_left;
		}
		p->n_right = fixargs(p->n_right, ip);
		p->n_left = fixargs(p->n_left, ip);
	} else
		p->n_right = fixargs(p->n_right, ip);
}
#endif

/*
 * Receives interpass structs from pass1.
 */
void
pass2_compile(struct interpass *ip)
{
	if (ip->type == IP_PROLOG) {
		tmpsave = NULL;
		DLIST_INIT(&ipole, qelem);
	}
	DLIST_INSERT_BEFORE(&ipole, ip, qelem);
	if (ip->type != IP_EPILOG)
		return;

#ifdef PCC_DEBUG
	if (e2debug) {
		printf("Entering pass2\n");
		printip(&ipole);
	}
#endif
	epp = (struct interpass_prolog *)DLIST_PREV(&ipole, qelem);
	p2maxautooff = p2autooff = epp->ipp_autos;
	DLIST_FOREACH(ip, &ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		myreader(ip->ip_node); /* local massage of input */
		if (xtemps == 0)
			walkf(ip->ip_node, deltemp);
		DLIST_INIT(&delayq, qelem);
		delay(ip->ip_node);
		while (DLIST_NEXT(&delayq, qelem) != &delayq) {
			struct interpass *ip2;
			ip2 = DLIST_NEXT(&delayq, qelem);
			DLIST_REMOVE(ip2, qelem);
			DLIST_INSERT_AFTER(ip, ip2, qelem);
			ip = ip2;
		}

	}
	DLIST_FOREACH(ip, &ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		canon(ip->ip_node);
		walkf(ip->ip_node, cktree);
		if ((ip->ip_node = deluseless(ip->ip_node)) == NULL)
			DLIST_REMOVE(ip, qelem);
	}

	optimize(&ipole);
	ngenregs(&ipole);

	DLIST_FOREACH(ip, &ipole, qelem)
		emit(ip);
}

#if 0
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
		if (xtemps == 0)
			walkf(ip->ip_node, deltemp);
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
		printf("Entering pass2\n");
		fwalk(p, e2print, 0);
	}
#endif
	ip->ip_node = p;
	compile4(ip);
}

/*
 * Save a complete function before doing anything with it in both the
 * optimized and unoptimized case.
 */
void
compile4(struct interpass *ip)
{
	struct interpass_prolog *epp;

	if (ip->type == IP_PROLOG) {
		tmpsave = NULL;
		DLIST_INIT(&ipole, qelem);
	}

	DLIST_INSERT_BEFORE(&ipole, ip, qelem);

	if (ip->type != IP_EPILOG)
		return;

#ifdef PCC_DEBUG
	if (e2debug)
		printip(&ipole);
#endif
	epp = (struct interpass_prolog *)DLIST_PREV(&ipole, qelem);
	p2maxautooff = p2autooff = epp->ipp_autos;

	optimize(&ipole);
	ngenregs(&ipole);

	DLIST_FOREACH(ip, &ipole, qelem)
		emit(ip);
}
#endif

void
emit(struct interpass *ip)
{
	NODE *p;
	int o;

	switch (ip->type) {
	case IP_NODE:
		p = ip->ip_node;

		nodepole = p;
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
			gencode(p->n_left, INREGS);
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
	case IP_DEFLAB:
		deflab(ip->ip_lbl);
		break;
	case IP_ASM:
		printf("\t%s\n", ip->ip_asm);
		break;
	default:
		cerror("compile4 %d", ip->type);
	}
}

#ifdef PCC_DEBUG
char *cnames[] = {
	"SANY",
	"SAREG",
	"SBREG",
	"SCREG",
	"SDREG",
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

int
geninsn(NODE *p, int cookie)
{
	NODE *p1, *p2;
	int o, rv = 0;

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
		rv = relops(p);
		break;

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
		rv = findops(p, cookie);
		break;

	case INCR:
	case DECR:
		rv = findops(p, cookie);
		if (rv != FFAIL)
			break;

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
		rv = findasg(p, cookie);
		break;

	case REG:
	case TEMP:
	case NAME:
	case ICON:
	case OREG:
		rv = findleaf(p, cookie);
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
		if (offstar(p->n_left, 0)) {
			p->n_op = OREG;
			if ((rv = findleaf(p, cookie)) < 0)
				comperr("bad findleaf"); /* XXX */
			p->n_su |= LOREG;
			p->n_op = UMUL;
			break;
		}
		/* FALLTHROUGH */
#else
		/* create oreg anyway */
		(void)offstar(p->n_left, 0);
		p->n_op = OREG;
		if ((rv = findleaf(p, cookie)) < 0)
			comperr("bad findleaf"); /* XXX */
		p->n_su |= LOREG;
		p->n_op = UMUL;
		break;
#endif

	case STCALL:
	case CALL:
		/* CALL arguments are handled special */
		for (p1 = p->n_right; p1->n_op == CM; p1 = p1->n_left)
			geninsn(p1->n_right, FOREFF);
		geninsn(p1, FOREFF);
		/* FALLTHROUGH */
	case COMPL:
	case UMINUS:
	case PCONV:
	case SCONV:
	case INIT:
	case GOTO:
	case FUNARG:
	case STARG:
	case UCALL:
	case USTCALL:
		rv = finduni(p, cookie);
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
		geninsn(p->n_left, INREGS);
		p->n_su = -1; /* su calculations traverse left */
		break;

	default:
		comperr("geninsn: bad op %d, node %p", o, p);
	}
	if (rv == FFAIL)
		comperr("Cannot generate code, node %p op %s", p,opst[p->n_op]);
	if (rv == FRETRY)
		goto again;
	return rv;
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

	storesave = ip;
	return r;
}

#ifdef PCC_DEBUG
#define	CDEBUG(x) if (c2debug) printf x
#else
#define	CDEBUG(x)
#endif

/*
 * Rewrite node after instruction emit.
 */
static void
rewrite(NODE *p, int rewrite, int cookie)
{
//	struct optab *q = &table[TBLIDX(p->n_su)];
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

	if (cookie != FOREFF) {
	if (p->n_su == DORIGHT)
		comperr("p->n_su == DORIGHT");
	p->n_rval = DECRA(p->n_reg, 0);
#if 0
	if (rewrite & RLEFT) {
#ifdef PCC_DEBUG
		if (l->n_op != REG)
			comperr("rewrite left");
#endif
		p->n_rval = DECRA(p->n_reg, 0);
	} else if (rewrite & RRIGHT) {
#ifdef PCC_DEBUG
		if (r->n_op != REG)
			comperr("rewrite right");
#endif
		p->n_rval = DECRA(p->n_reg, 0);
	} else if (rewrite & RESC1) {
		p->n_rval = p->n_reg;
	} else if (rewrite & RESC2)
		p->n_reg = p->n_rval = p->n_reg;
	else if (rewrite & RESC3)
		p->n_rval = 0; /* XXX */
	else if (p->n_su == DORIGHT)
		p->n_reg = p->n_rval = l->n_rval; /* XXX special */
#endif
	}
	if (optype(o) != LTYPE)
		tfree(l);
	if (optype(o) == BITYPE)
		tfree(r);
}

void
gencode(NODE *p, int cookie)
{
	struct optab *q = &table[TBLIDX(p->n_su)];
	NODE *p1;

	if (p->n_su == -1) /* For OREGs and similar */
		return gencode(p->n_left, cookie);

	CDEBUG(("gencode: node %p\n", p));

	if (p->n_op == REG && DECRA(p->n_reg, 0) == p->n_rval)
		return; /* meaningless move to itself */
	if (p->n_su & DORIGHT) {
		gencode(p->n_right, INREGS);
		if ((p->n_su & RMASK) == ROREG)
			canon(p);
	}
	if (p->n_su & LMASK) {
		gencode(p->n_left, INREGS);
		if ((p->n_su & LMASK) == LOREG)
			canon(p);
	}
	if ((p->n_su & RMASK) && !(p->n_su & DORIGHT)) {
		gencode(p->n_right, INREGS);
		if ((p->n_su & RMASK) == ROREG)
			canon(p);
	}

	if ((p->n_su & RMASK) == RREG) {
		if (q->needs & NSPECIAL) {
			int rr = rspecial(q, NRIGHT);

			if (rr >= 0 && rr != p->n_right->n_rval) {
				CDEBUG(("gencode(%p) right nspec move\n", p));
				rmove(p->n_right->n_rval,
				    rr, p->n_right->n_type);
				p->n_right->n_reg = rr;
				p->n_right->n_rval = rr;
			}
		} else if ((q->rewrite & RRIGHT) &&
		    DECRA(p->n_right->n_reg, 0) != DECRA(p->n_reg, 0)) {
#ifdef notyet
			if (p->n_op == ASSIGN)
				comperr("ASSIGN error");
#endif
			CDEBUG(("gencode(%p) right move\n", p));
			rmove(p->n_right->n_reg, p->n_reg, p->n_type);
			p->n_right->n_reg = p->n_reg;
			p->n_right->n_rval = p->n_reg;
		}
	}
	if ((p->n_su & LMASK) == LREG) {
		if (q->needs & NSPECIAL) {
			int rr = rspecial(q, NLEFT);

			if (rr >= 0 && rr != DECRA(p->n_left->n_reg, 0)) {
				CDEBUG(("gencode(%p) left nspec move\n", p));
				rmove(DECRA(p->n_left->n_reg, 0), rr,
				    p->n_left->n_type);
				p->n_left->n_reg = rr;
				p->n_left->n_rval = rr;
			}
		} else if ((q->rewrite & RLEFT) &&
		    DECRA(p->n_left->n_reg, 0) != DECRA(p->n_reg, 0)) {
#ifdef notyet
			if (p->n_op == ASSIGN)
				comperr("ASSIGN error");
#endif
			CDEBUG(("gencode(%p) left move\n", p));
			rmove(p->n_left->n_reg, p->n_reg, p->n_type);
			p->n_left->n_reg = p->n_reg;
			p->n_left->n_rval = p->n_reg;
		}
	}

	if (p->n_op == ASSIGN &&
	    p->n_left->n_op == REG && p->n_right->n_op == REG &&
	    p->n_left->n_rval == p->n_right->n_rval){
		/* do not emit anything */
		CDEBUG(("gencode(%p) assign nothing\n", p));
		rewrite(p, q->rewrite, cookie);
		return;
	}

	CDEBUG(("emitting node %p\n", p));

	if (p->n_op == CALL || p->n_op == FORTCALL || p->n_op == STCALL) {
		/* Print out arguments first */
		lastcall(p); /* last chance before printing out insn */
		for (p1 = p->n_right; p1->n_op == CM; p1 = p1->n_left)
			gencode(p1->n_right, FOREFF);
		gencode(p1, FOREFF);
	}

	expand(p, cookie, q->cstring);
	if (callop(p->n_op) && cookie != FOREFF &&
	    DECRA(p->n_reg, 0) != RETREG(p->n_type)) {
		CDEBUG(("gencode(%p) retreg\n", p));
		rmove(RETREG(p->n_type), DECRA(p->n_reg, 0), p->n_type);
	} else if (q->needs & NSPECIAL) {
		int rr = rspecial(q, NRES);

		if (rr >= 0 && p->n_reg != rr) {
			CDEBUG(("gencode(%p) nspec retreg\n", p));
			rmove(rr, DECRA(p->n_reg, 0), p->n_type);
		}
	} else if ((q->rewrite & RESC1) &&
	    (DECRA(p->n_reg, 1) != DECRA(p->n_reg, 0))) {
		CDEBUG(("gencode(%p) RESC1 retreg\n", p));
		rmove(DECRA(p->n_reg, 1), DECRA(p->n_reg, 0), p->n_type);
	}
	rewrite(p, q->rewrite, cookie);
}

int negrel[] = { NE, EQ, GT, GE, LT, LE, UGT, UGE, ULT, ULE } ;  /* negatives of relationals */

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
		fprintf(prfil, "\t");
		down -= 2;
		}
	if( down-- ) fprintf(prfil, "    " );


	fprintf(prfil, "%p) %s", p, opst[p->n_op] );
	switch( p->n_op ) { /* special cases */

	case REG:
		fprintf(prfil, " %s", rnames[p->n_rval] );
		break;

	case TEMP:
		fprintf(prfil, " " CONFMT, p->n_lval);
		break;

	case ICON:
	case NAME:
	case OREG:
		fprintf(prfil, " " );
		adrput(prfil, p );
		break;

	case STCALL:
	case USTCALL:
	case STARG:
	case STASG:
		fprintf(prfil, " size=%d", p->n_stsize );
		fprintf(prfil, " align=%d", p->n_stalign );
		break;
		}

	fprintf(prfil, ", " );
	tprint(prfil, p->n_type, p->n_qual);
	fprintf(prfil, ", " );
	{
		int gregn(struct regw *);
		if (p->n_reg < 100000) /* XXX */
			fprintf(prfil, "REG %s", rnames[DECRA(p->n_reg, 0)]);
		else
			fprintf(prfil, "TEMP %d", gregn(p->n_regw));
		}
	fprintf(prfil, ", SU= %d(%cREG,%s,%s,%s,%s)\n",
	    TBLIDX(p->n_su), 
	    TCLASS(p->n_su)+'@',
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
		if (q->n_op == REG && q->n_rval == DECRA(q->n_reg, 0)) {
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
				ql->n_op==REG && szty(qr->n_type)==1 &&
				(ql->n_rval == DECRA(ql->n_reg, 0) ||
				/* XXX */
				 ql->n_rval == FPREG || ql->n_rval == STKREG)) {
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
			/* stop gencode traversal */
			if (p->n_su == -1)
				p->n_su = 0;
			else
				p->n_su &= ~(LMASK|RMASK|DORIGHT);
			tfree(q);
			return;
		}
	}
}

void
canon(p) NODE *p; {
	/* put p in canonical form */

	walkf(p, setleft);	/* ptrs at left node for arithmetic */
	walkf(p, oreg2);	/* look for and create OREG nodes */
#ifndef FIELDOPS
	fwalk(p, ffld, 0);	/* look for field operators */
# endif
#ifdef MYCANON
	MYCANON(p);		/* your own canonicalization routine(s) */
#endif

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
	prfil = stderr;

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
	p->n_regw = NULL;
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
	p->n_regw = NULL;
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
	p->n_regw = NULL;
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

int
rspecial(struct optab *q, int what)
{
	struct rspecial *r = nspecial(q);
	while (r->op) {
		if (r->op == what)
			return r->num;
		r++;
	}
	return -1;
}
