/*	$Id$	*/
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

/*	some storage declarations */
int nrecur;
int lflag;
int x2debug;
int udebug = 0;
int ftnno;

NODE *stotree;
int stocook;
static int saving;

static struct templst {
	struct templst *next;
	int tempnr;
	int tempoff;
} *templst;

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

static void genregs(NODE *p);
static void gencode(NODE *p);

/*
 * Layout of findops() return value:
 *	bit 0-1 where to store left node.
 *	bit 2-3 where to store right node.
 *	bit 4	set if right leg should be evaluated first
 *	bit 5-	table index
 */

#define	LREG		001
#define	LOREG		002
#define	LTEMP		003
#define	LMASK		003
#define	RREG		004
#define	ROREG		010
#define	RTEMP		014
#define	RMASK		014
#define	DORIGHT		020
#define	TBSH		5
#define	TBLIDX(idx)	((idx) >> TBSH)
#define	MKIDX(tbl,mod)	(((tbl) << TBSH) | (mod))

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
	if (e2debug)
		fwalk(p, e2print, 0);
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
	reclaim( p, RNULL, 0 );
	allchk();
}

/* look for delayable ++ and -- operators */
void
delay(NODE *p)
{
	int ty = optype(p->n_op);

	switch (p->n_op) {
	case CALL:
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

static void newblock(int myreg, int aoff);
static void epilogue(int regs, int autos, int retlab);

void
pass2_compile(struct interpass *ip)
{
	if (Oflag) {
		if (ip->type == IP_PROLOG)
			saving++;
		if (saving)
			return saveip(ip);
	}
	switch (ip->type) {
	case IP_NODE:
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
	templst = NULL;
	eoftn(regs, autos, retlab);
}

/*
 * generate the code for p;
 * order may call codgen recursively
 * cookie is used to describe the context
 */
void
codgen(NODE *p, int cookie)
{
	canon(p);  /* creats OREG from * if possible and does sucomp */
#ifdef PCC_DEBUG
	if (e2debug) {
		printf("geninsn called on:\n");
		fwalk(p, e2print, 0);
	}
#endif

	geninsn(p, cookie); /* Assign instructions for tree */
	sucomp(p);  /* Calculate sub-tree evaluation order */
#ifdef PCC_DEBUG
	if (udebug) {
		printf("genregs called on:\n");
		fwalk(p, e2print, 0);
	}
#endif
	genregs(p); /* allocate registers for instructions */
	gencode(p); /* Emit instructions */
#if 0
	for (;;) {
		canon(p);  /* creats OREG from * if possible and does sucomp */
		stotree = NIL;
#ifdef PCC_DEBUG
		if (e2debug) {
			printf("store called on:\n");
			fwalk(p, e2print, 0);
		}
#endif
		store(p);
		if( stotree==NIL ) break;

		/* because it's minimal, can do w.o. stores */

		order( stotree, stocook );
	}
	order( p, cookie );
#endif
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
void
prcook(int cookie)
{
	int i, flag;

	if( cookie & SPECIAL ){
		if( cookie == SZERO ) printf( "SZERO" );
		else if( cookie == SONE ) printf( "SONE" );
		else if( cookie == SMONE ) printf( "SMONE" );
		else printf( "SPECIAL+%d", cookie & ~SPECIAL );
		return;
		}

	flag = 0;
	for( i=0; cnames[i]; ++i ){
		if( cookie & (1<<i) ){
			if( flag ) printf( "|" );
			++flag;
			printf( cnames[i] );
			}
		}

}
#endif

int odebug = 0;

void
geninsn(NODE *p, int cookie)
{
	int o, rv;

#ifdef PCC_DEBUG
	if (odebug) {
		printf("geninsn(%p, ", p);
		prcook(cookie);
		printf(")\n");
		fwalk(p, e2print, 0);
	}
#endif

again:	switch (o = p->n_op) {
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
	case ASSIGN:
		if (o == ASSIGN)
		    if ((rv = findasg(p, cookie)) < 0) {
			if (setasg(p, cookie))
				goto again;
			goto failed;
		}
		/*
		 * Do subnodes conversions (if needed).
		 */
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
			cerror("geninsn REG");
		/* FALLTHROUGH */
	case OREG:
		if ((cookie & (INTAREG|INTBREG)) == 0)
			cerror("geninsn OREG");
		if ((rv = findleaf(p, cookie)) < 0) {
			if (setasg(p, cookie))
				goto again;
			goto failed;
		}
		p->n_su = rv;
		break;

	case UMUL:
		if ((rv = finduni(p, cookie)) < 0) {
			if (setuni(p, cookie))
				goto again;
			goto failed;
		}
		switch (rv & LMASK) {
		case LREG:
			geninsn(p->n_left, INTAREG|INTBREG);
			break;
		}
		p->n_su = rv;
		break;

	default:
		cerror("geninsn: bad op %d", o);
	}
	return;

failed:
#ifdef PCC_DEBUG
	fwalk(p, e2print, 0);
#endif
	cerror("Cannot generate code for op %d\n", o);
}

void
order(NODE *p, int cook)
{
//	struct optab *q;
	int o, ty, m, rv;
	int cookie;
	NODE *p1, *p2;

	/*
	 * by this time, p should be able to be generated without stores;
	 * the only question is how
	 */
	again:

	cookie = cook;
	rcount();
	canon(p);
	rallo(p, p->n_rall);

#ifdef PCC_DEBUG
	if (odebug) {
		printf("order(%p, ", p);
		prcook(cookie);
		printf(")\n");
		fwalk(p, e2print, 0);
	}
#endif

	o = p->n_op;
	ty = optype(o);

	/* first of all, for most ops, see if it is in the table */

	/* look for ops */

	switch (m = p->n_op) {

#if 0
	case ASSIGN:
		/*
		 * For ASSIGN the left node must be directly addressable,
		 * the right can be put into a register.
		 * XXX - Will not try to match any smart instructions yet.
		 */
//printf("foo\n");
//fwalk(p, e2print, 0);
		if (!canaddr(p->n_left)) {
			if (p->n_left->n_op == UMUL) {
				offstar(p->n_left->n_left);
				goto again;
			}
			cerror("bad assign lvalue");
		}
//printf("foo1\n");
//fwalk(p, e2print, 0);
		if (!canaddr(p->n_right)) {
			if (p->n_right->n_op == UMUL) {
				offstar(p->n_right->n_left);
				goto again;
			}
			order(p->n_right, INTAREG|INTBREG);
		}
//printf("foo2\n");
//fwalk(p, e2print, 0);
		rv = asgops(p, cook);
//printf("foo6 : %x\n", rv);
		if (rv < 0)
			goto nomat;
		if (rv & RREG)
			order(p->n_right, INTAREG|INTBREG);
		q = &table[rv >> 2];
//printf("foo7\n");
		if (!allo(p, q))
			cerror("assign allo failed");
//printf("foo3\n");
		expand(p, cook, q->cstring);
		reclaim(p, q->rewrite, cook);
//printf("foo4\n");
//fwalk(p, e2print, 0);
		goto cleanup;
#endif

	case PLUS:
	case MINUS:
	case AND:
	case OR:
	case ER:
	case DIV:
	case MOD:
	case MUL:
	case LS:
	case RS:

		/*
		 * Get a suitable op.
		 */
		if ((rv = findops(p, cook)) < 0) {
			if (setbin(p))
				goto again;
			goto nomat;
		}

		/*
		 * Do subnodes conversions (if needed).
		 */
		switch (rv & LMASK) {
		case LREG:
			order(p->n_left, INTAREG|INTBREG);
			break;
		case LOREG:
			offstar(p->n_left->n_left);
			canon(p->n_left);
			break;
		case LTEMP:
			order(p->n_left, INTEMP);
			break;
		}

		switch (rv & RMASK) {
		case RREG:
			order(p->n_right, INTAREG|INTBREG);
			break;
		case ROREG:
			offstar(p->n_right->n_left);
			canon(p->n_right);
			break;
		case RTEMP:
			order(p->n_right, INTEMP);
			break;
		}
		p->n_su = rv;
		return;
#if 0
		/*
		 * Be sure that both sides are addressable.
		 */
//printf("newstyle node %p\n", p);
		if (!canaddr(p->n_left)) {
			if (p->n_left->n_op == UMUL) {
				offstar(p->n_left->n_left);
				goto again;
			}
			order(p->n_left, INTAREG|INTBREG);
		}
//printf("newstyle addrl %p\n", p);
		if (!canaddr(p->n_right)) {
			if (p->n_right->n_op == UMUL) {
				offstar(p->n_right->n_left);
				goto again;
			}
			order(p->n_right, INTAREG|INTBREG);
		}
//printf("newstyle addrr %p\n", p);

		/*
		 *
		 */
		m = INTAREG|INTBREG;
		rv = findops(p);
foo:		if (rv < 0) {
			if (setbin(p))
				goto again;
			goto nomat;
		}
		if (rv & LREG) {
			if (p->n_left->n_op == UMUL) {
				offstar(p->n_left->n_left);
				goto again;
			}
			order(p->n_left, INTAREG|INTBREG);
		}
//printf("newstyle ltmp %p\n", p);
		if (rv & RREG) {
			if (p->n_right->n_op == UMUL) {
				offstar(p->n_right->n_left);
				goto again;
			}
			order(p->n_right, INTAREG|INTBREG);
		}
//printf("newstyle rtmp %p\n", p);
		

		q = &table[rv >> 2];
		if (!allo(p, q)) {
			/*
			 * Ran out of suitable temp regs.
			 * Force everything onto stack.
			 * Be careful to avoid loops.
			 * XXX - this is bad code!
			 */
			if ((rv & LREG) == 0 && istnode(p->n_left)) {
				order(p->n_left, INTEMP);
				goto again;
			} else if (!(rv & RREG) &&istnode(p->n_right)) {
				order(p->n_right, INTEMP);
				goto again;
			}
			cerror("allo failed");
		}
		expand(p, m, q->cstring);
		reclaim(p, q->rewrite, m);
//printf("newstyle ute %p\n", p);
		goto cleanup;

#endif

		/*
		 * For now just be sure that the trees on each side
		 * are adressable.
		 */
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

		if (!canaddr(p->n_left)) {
			if (p->n_left->n_op == UMUL) {
				offstar(p->n_left->n_left);
				goto again;
			}
			order(p->n_left, INTAREG|INTBREG|INAREG|INBREG);
		}
		if (!canaddr(p->n_right)) {
			if (p->n_right->n_op == UMUL) {
				offstar(p->n_right->n_left);
				goto again;
			}
			order(p->n_right, INTAREG|INTBREG|INAREG|INBREG);
		}
		rv = relops(p);
		m = FORCC;
		break;

	default:
		/* look for op in table */
		for (;;) {
			if ((m = match(p, cookie)) == MDONE)
				goto cleanup;
			else if (m == MNOPE) {
				if (!(cookie = nextcook(p, cookie)))
					goto nomat;
				continue;
			} else
				break;
		}
		break;

	case FORCE:
	case CBRANCH:
	case UCALL:
	case CALL:
	case USTCALL:
	case STCALL:
	case UFORTCALL:
	case FORTCALL:
		/* don't even go near the table... */
		;

	}
	/*
	 * get here to do rewriting if no match or
	 * fall through from above for hard ops
	 */

	p1 = p->n_left;
	if (ty == BITYPE)
		p2 = p->n_right;
	else
		p2 = NIL;
	
#ifdef PCC_DEBUG
	if (odebug) {
		printf("order(%p, ", p);
		prcook(cook);
		printf("), cookie ");
		prcook(cookie);
		printf(", rewrite %s\n", opst[m]);
	}
#endif
	switch (m) {
	default:
		nomat:
		cerror( "no table entry for op %s", opst[p->n_op] );

	case FORCE:
		cook = INTAREG|INTBREG;
		order(p->n_left, cook);
		reclaim(p, RLEFT, cook);
		return;

	case CBRANCH:
		p1->n_label = p2->n_lval;
		o = p1->n_op;
		codgen(p1, FORCC);
		cbgen(o, p2->n_lval);
		reclaim(p1, RNULL, 0);
		nfree(p2);
		nfree(p);
		return;

	case FLD:	/* fields of funny type */
		if ( p1->n_op == UMUL ){
			offstar( p1->n_left );
			goto again;
			}

	case UMINUS:
		order( p1, INBREG|INAREG);
		goto again;

	case NAME:
		/* all leaves end up here ... */
		if( o == REG ) goto nomat;
		order( p, INTAREG|INTBREG );
		goto again;

	case INIT:
		uerror("init: illegal initialization");
		return;

	case UFORTCALL:
		p->n_right = NIL;
	case FORTCALL:
		o = p->n_op = UFORTCALL;
		if( genfcall( p, cookie ) ) goto nomat;
		goto cleanup;

	case UCALL:
		p->n_right = NIL;
	case CALL:
		o = p->n_op = UCALL;
		if( gencall( p, cookie ) ) goto nomat;
		goto cleanup;

	case USTCALL:
		p->n_right = NIL;
	case STCALL:
		o = p->n_op = USTCALL;
		if( genscall( p, cookie ) ) goto nomat;
		goto cleanup;

		/* if arguments are passed in register, care must be taken that reclaim
		 * not throw away the register which now has the result... */

	case UMUL:
		if( cook == FOREFF ){
			/* do nothing */
			order( p->n_left, FOREFF );
			nfree(p);
			return;
		}
		offstar( p->n_left );
#if 0
		canon(p);
		if( canaddr(p) && cook != INTEMP )
			goto cleanup;
#endif
		goto again;

	case INCR:  /* INCR and DECR */
		if( setincr(p) ) goto again;

		/* x++ becomes (x = x + 1) -1; */

		p1 = tcopy(p);
		if (cook & FOREFF) {
			nfree(p->n_right);
			p->n_right = p1;
			p1->n_op = (p->n_op == INCR) ? PLUS: MINUS;
			p->n_op = ASSIGN;
		} else {
			p2 = talloc();
			p2->n_rall = NOPREF;
			p2->n_name = "";
			p2->n_op = ASSIGN;
			p2->n_type = p->n_type;
			p2->n_left = p->n_left;
			p2->n_right = p1;
			p1->n_op = (p->n_op == INCR) ? PLUS: MINUS;
			p->n_op = (p->n_op == INCR) ? MINUS : PLUS;
			p->n_left = p2;
		}
		goto again;

	case STASG:
		if( setstr( p ) ) goto again;
		goto nomat;

	case ASSIGN:
		if (setasg(p, cook))
			goto again;
		goto nomat;

	case BITYPE:
		if( setbin( p ) ) goto again;
		goto nomat;

		}

	cleanup:

	/* if it is not yet in the right state, put it there */

	if( cook & FOREFF ){
		reclaim( p, RNULL, 0 );
		return;
		}

	if( p->n_op==FREE ) return;

	if( tshape( p, cook ) ) return;

	if( (m=match(p,cook) ) == MDONE ) return;

	/* we are in bad shape, try one last chance */
	if (lastchance(p, cook))
		goto again;

	goto nomat;
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

	nreg = (q->needs & NACOUNT) * szty(p->n_type);

	switch (p->n_su & RMASK) {
	case RREG:
	case ROREG:
		right = sucomp(p->n_right);
		break;
	case RTEMP:
		cerror("sucomp RTEMP");
	default:
		right = 0;
	}
	switch (p->n_su & LMASK) {
	case LREG:
	case LOREG:
		left = sucomp(p->n_left);
		break;
	case LTEMP:
		cerror("sucomp LTEMP");
	default:
		left = 0;
	}
//printf("sucomp: right %d left %d\n", right, left);
	if (right > left)
		p->n_su |= DORIGHT;
	if (right > nreg)
		nreg = right;
	if (left > nreg)
		nreg = right;
	return nreg;
}

void
genregs(NODE *p)
{
	rallo(p, NOPREF);
}

void
gencode(NODE *p)
{
	struct optab *q = &table[TBLIDX(p->n_su)];

	if (p->n_su == -1) /* For OREGs and similar */
		return gencode(p->n_left);

	if (p->n_su & DORIGHT) {
		gencode(p->n_right);
		if ((p->n_su & RMASK) == ROREG)
			canon(p);
	}
	if (p->n_su & LMASK) {
		gencode(p->n_left);
		if ((p->n_su & LMASK) == LOREG)
			canon(p);
	}
	if ((p->n_su & RMASK) && !(p->n_su & DORIGHT)) {
		gencode(p->n_right);
		if ((p->n_su & RMASK) == ROREG)
			canon(p);
	}
	if (!allo(p, q))
		cerror("failed register allocation");
	expand(p, FOREFF, q->cstring);
	reclaim(p, q->rewrite, INTAREG);
}


int callflag;
int fregs;

void
store( p ) NODE *p; {

	/* find a subtree of p which should be stored */

	int o, ty;

	o = p->n_op;
	ty = optype(o);

	if( ty == LTYPE ) return;

	switch( o ){

	case UCALL:
	case UFORTCALL:
	case USTCALL:
		++callflag;
		break;

	case UMUL:
		if (asgop(p->n_left->n_op))
			stoasg(p->n_left, UMUL);
		break;

	case CALL:
	case FORTCALL:
	case STCALL:
		store( p->n_left );
		stoarg( p->n_right, o );
		++callflag;
		return;

		}

	if (ty == UTYPE) {
		store(p->n_left);
		return;
	}

	if (asgop(p->n_right->n_op))
		stoasg(p->n_right, o);

	if( p->n_su>fregs ){ /* must store */
		mkadrs( p );  /* set up stotree and stocook to subtree
				 that must be stored */
		}

	store( p->n_right );
	store( p->n_left );
	}

/* mark off calls below the current node */
void
markcall(NODE *p)
{

	again:
	switch( p->n_op ){

	case UCALL:
	case USTCALL:
	case UFORTCALL:
	case CALL:
	case STCALL:
	case FORTCALL:
		++callflag;
		return;

		}

	switch( optype( p->n_op ) ){

	case BITYPE:
		markcall( p->n_right );
	case UTYPE:
		p = p->n_left;
		/* eliminate recursion (aren't I clever...) */
		goto again;
	case LTYPE:
		return;
		}

}

void
stoarg(NODE *p, int calltype)
{
	/* arrange to store the args */
	if( p->n_op == CM ){
		stoarg( p->n_left, calltype );
		p = p->n_right ;
		}
	if( calltype == CALL ){
		STOARG(p);
		}
	else if( calltype == STCALL ){
		STOSTARG(p);
		}
	else {
		STOFARG(p);
		}
	callflag = 0;
	store(p);
#ifdef NO_NESTCALLS
	if( callflag ){ /* prevent two calls from being active at once  */
		SETSTO(p,INTEMP);
		store(p); /* do again to preserve bottom up nature....  */
	}
#endif
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
int
e2print(NODE *p, int down, int *a, int *b)
{

	*a = *b = down+1;
	while( down >= 2 ){
		printf( "\t" );
		down -= 2;
		}
	if( down-- ) printf( "    " );


	printf( "%p) %s", p, opst[p->n_op] );
	switch( p->n_op ) { /* special cases */

	case REG:
		printf( " %s", rnames[p->n_rval] );
		break;

	case TEMP:
		printf(" %d", (int)p->n_lval);
		break;

	case ICON:
	case NAME:
	case OREG:
		printf( " " );
		adrput( p );
		break;

	case STCALL:
	case USTCALL:
	case STARG:
	case STASG:
		printf( " size=%d", p->n_stsize );
		printf( " align=%d", p->n_stalign );
		break;
		}

	printf( ", " );
	tprint( p->n_type, p->n_qual);
	printf( ", " );
	if( p->n_rall == NOPREF ) printf( "NOPREF" );
	else {
		if( p->n_rall & MUSTDO ) printf( "MUSTDO " );
		else printf( "PREF " );
		printf( "%s", rnames[p->n_rall&~MUSTDO]);
		}
	printf( ", SU= %d\n", p->n_su );
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
	struct templst *w = templst;

	if (p->n_op != TEMP)
		return;
	/*
	 * the size of a TEMP is in multiples of the reg size.
	 */
	p->n_op = OREG;
	p->n_rval = FPREG;
	while (w != NULL) {
		if (w->tempnr == p->n_lval)
			break;
		w = w->next;
	}
	if (w == NULL) {
		w = tmpalloc(sizeof(struct templst));
		w->tempnr = p->n_lval;
		w->tempoff = BITOOR(freetemp(szty(p->n_type)));
		w->next = templst;
		templst = w;
	}
	p->n_lval = w->tempoff;
	p->n_name = "";
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
#if 0
	walkf(p, sucomp);	/* do the Sethi-Ullman computation */
#endif

}

static SIMPLEQ_HEAD(, interpass) ipole = SIMPLEQ_HEAD_INITIALIZER(ipole);

void
saveip(struct interpass *ip)
{
	struct interpass *prol;

	SIMPLEQ_INSERT_TAIL(&ipole, ip, sqelem);

	if (ip->type != IP_EPILOG)
		return;
	saving = -1;

	cvtemps(ip);	/* Convert TEMPs to OREGs */
	deljumps();	/* Delete redundant jumps and dead code */

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

/*
 * Convert TEMPs to OREGs.
 * XXX - ugly.
 */
void
cvtemps(struct interpass *epil)
{
	struct interpass *ip;

	SETOFF(epil->ip_auto, ALINT);
	maxautooff = autooff = epil->ip_auto;
	SIMPLEQ_FOREACH(ip, &ipole, sqelem) {
		if (ip->type != IP_NODE)
			continue;
		walkf(ip->ip_node, deltemp);
	}
	epil->ip_auto = maxautooff;
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
		if (ip->ip_node->n_op != GOTO)
			continue;
		n = ip->sqelem.sqe_next;
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
			return ixp[i] << 2;
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
				rv = (ixp[i] << 2) | LREG;
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
				rv = (ixp[i] << 2) | RREG;
			}
			continue; /* nothing more to do */
		}
		if (6 < mtchno) {
			mtchno = 6;
			rv = (ixp[i] << 2) | RREG|LREG;
		}
	}
if (f2debug) { if (rv == -1) printf("relops failed\n"); else printf("relops entry %d, %s %s\n", rv >> 2, rv & RREG ? "RREG" : "", rv & LREG ? "LREG" : ""); } 
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
		printf("findasg tree: ");
		prcook(cookie);
		printf("\n");
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
		printf("findleaf tree: ");
		prcook(cookie);
		printf("\n");
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
 */
int
finduni(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	NODE *l;
	int i, shl, rsl;
	int *ixp;
	int rv = -1;

#ifdef PCC_DEBUG
	if (f2debug) {
		printf("finduni tree: ");
		prcook(cookie);
		printf("\n");
		fwalk(p, e2print, 0);
	}
#endif

	l = getlr(p, 'L');
	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("finduni: ixp %d\n", ixp[i]);
		if (ttype(l->n_type, q->ltype) == 0)
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
