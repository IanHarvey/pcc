#if 0
static char *sccsid ="@(#)reader.c	4.8 (Berkeley) 12/10/87";
#endif

# include "pass2.h"

/*	some storage declarations */
/*
 * TODO: re-invent autoincr()/deltest()
 */

int nrecur;
int lflag;
extern int Wflag;
int x2debug;
int udebug = 0;
int vdebug = 0;

OFFSZ tmpoff;  /* offset for first temporary, in bits for current block */
OFFSZ maxoff;  /* maximum temporary offset over all blocks in current ftn, in bits */
int maxtreg;

NODE *stotree;
int stocook;

OFFSZ baseoff = 0;
OFFSZ maxtemp = 0;

static struct templst {
	struct templst *next;
	int tempnr;
	int tempoff;
} *templst;

int e2print(NODE *p, int down, int *a, int *b);
void cbranch(NODE *p, int false);

#ifdef PCC_DEBUG
static void
cktree(NODE *p)
{
	if (p->n_op > MAXOP)
		cerror("op %d slipped through", p->n_op);
}
#endif

static void
p2compile(NODE *p)
{
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
	codgen(p, FOREFF);
	reclaim( p, RNULL, 0 );
	allchk();
	/* can't do tcheck here; some stuff (e.g., attributes) may be around from first pass */
	/* first pass will do it... */
}

static void newblock(int myreg, int aoff);
static void epilogue(int regs, int autos, int retlab);

void
pass2_compile(struct interpass *ip)
{
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
	default:
		cerror("pass2_compile %d", ip->type);
	}
}

static void
newblock(int myreg, int aoff)
{
	static int myftn = -1;

	tmpoff = baseoff = (unsigned int) aoff;
	maxtreg = myreg;
	if( myftn != ftnno ){ /* beginning of function */
		maxoff = baseoff;
		myftn = ftnno;
		maxtemp = 0;
		}
	else {
		if( baseoff > maxoff ) maxoff = baseoff;
		/* maxoff at end of ftn is max of autos and temps over all blocks */
		}
	setregs();
}

static void
epilogue(int regs, int autos, int retlab)
{
	SETOFF(maxoff, ALSTACK);
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

	for (;;) {
		canon(p);  /* creats OREG from * if possible and does sucomp */
		stotree = NIL;
# ifndef BUG4
		if (e2debug) {
			printf("store called on:\n");
			fwalk(p, e2print, 0);
		}
# endif
		store(p);
		if( stotree==NIL ) break;

		/* because it's minimal, can do w.o. stores */

		order( stotree, stocook );
	}
	order( p, cookie );
}

# ifndef BUG4
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
# ifdef WCARD1
	"WCARD1",
# else
	"STARNM",
# endif
# ifdef WCARD2
	"WCARD2",
# else
	"STARREG",
# endif
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
		else if( cookie == SCCON ) printf( "SCCON" );
		else if( cookie == SSCON ) printf( "SSCON" );
		else if( cookie == SSOREG ) printf( "SSOREG" );
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
# endif

int odebug = 0;

void
order(NODE *p, int cook)
{
	int o, ty, m;
	int cookie;
	NODE *p1, *p2;

	cookie = cook;
	rcount();
	canon(p);
	rallo(p, p->n_rall);
	goto first;


	/*
	 * by this time, p should be able to be generated without stores;
	 * the only question is how
	 */
	again:

	if (p->n_op == FREE)
		cerror("order");	/* whole tree was done */
	cookie = cook;
	rcount();
	canon(p);
	rallo(p, p->n_rall);
	/*
	 * if any rewriting and canonicalization has put
	 * the tree (p) into a shape that cook is happy
	 * with (exclusive of FOREFF, FORREW, and INTEMP)
	 * then we are done.
	 * this allows us to call order with shapes in
	 * addition to cookies and stop short if possible.
	 */
	if (tshape(p, cook &(~(FOREFF|FORREW|INTEMP))))
		return;

	first:
# ifndef BUG4
	if (odebug) {
		printf("order(%p, ", p);
		prcook(cookie);
		printf(")\n");
		fwalk(p, e2print, 0);
	}
# endif

	o = p->n_op;
	ty = optype(o);

	/* first of all, for most ops, see if it is in the table */

	/* look for ops */

	switch (m = p->n_op) {

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
	case UNARY CALL:
	case CALL:
	case UNARY STCALL:
	case STCALL:
	case UNARY FORTCALL:
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
	
# ifndef BUG4
	if (odebug) {
		printf("order(%p, ", p);
		prcook(cook);
		printf("), cookie ");
		prcook(cookie);
		printf(", rewrite %s\n", opst[m]);
	}
# endif
	switch (m) {
	default:
		nomat:
		cerror( "no table entry for op %s", opst[p->n_op] );

	case FORCE:
		/* recurse, letting the work be done by rallo */
		cook = INTAREG|INTBREG;
		order(p->n_left, cook);
		reclaim(p, RLEFT, cook);
		goto again;

	case CBRANCH:
		o = p2->n_lval;
		cbranch( p1, o );
		nfree(p2);
		nfree(p);
		return;

	case FLD:	/* fields of funny type */
		if ( p1->n_op == UNARY MUL ){
			offstar( p1->n_left );
			goto again;
			}

	case UNARY MINUS:
		order( p1, INBREG|INAREG|SOREG );
		goto again;

	case NAME:
		/* all leaves end up here ... */
		if( o == REG ) goto nomat;
		order( p, INTAREG|INTBREG );
		goto again;

	case INIT:
		uerror("init: illegal initialization");
		return;

	case UNARY FORTCALL:
		p->n_right = NIL;
	case FORTCALL:
		o = p->n_op = UNARY FORTCALL;
		if( genfcall( p, cookie ) ) goto nomat;
		goto cleanup;

	case UNARY CALL:
		p->n_right = NIL;
	case CALL:
		o = p->n_op = UNARY CALL;
		if( gencall( p, cookie ) ) goto nomat;
		goto cleanup;

	case UNARY STCALL:
		p->n_right = NIL;
	case STCALL:
		o = p->n_op = UNARY STCALL;
		if( genscall( p, cookie ) ) goto nomat;
		goto cleanup;

		/* if arguments are passed in register, care must be taken that reclaim
		 * not throw away the register which now has the result... */

	case UNARY MUL:
		if( cook == FOREFF ){
			/* do nothing */
			order( p->n_left, FOREFF );
			nfree(p);
			return;
		}
#ifdef R2REGS
		/* try to coax a tree into a doubly indexed OREG */
		p1 = p->n_left;
		if( p1->n_op == PLUS ) {
			if( ISPTR(p1->n_left->n_type) &&
			    offset(p1->n_right, tlen(p)) >= 0 ) {
				order( p1->n_left, INAREG|INTAREG );
				goto again;
				}
			if( ISPTR(p1->n_right->n_type) &&
			    offset(p1->n_left, tlen(p)) >= 0 ) {
				order( p1->n_right, INAREG|INTAREG );
				goto again;
				}
			}
#endif
		offstar( p->n_left );
		goto again;

	case INCR:  /* INCR and DECR */
		if( setincr(p) ) goto again;

		/* x++ becomes (x += 1) -1; */

		if( cook & FOREFF ){  /* result not needed so inc or dec and be done with it */
			/* x++ => x += 1 */
			p->n_op = (p->n_op==INCR)?ASG PLUS:ASG MINUS;
			goto again;
			}

		p1 = tcopy(p);
		reclaim( p->n_left, RNULL, 0 );
		p->n_left = p1;
		p1->n_op = (p->n_op==INCR)?ASG PLUS:ASG MINUS;
		p->n_op = (p->n_op==INCR)?MINUS:PLUS;
		goto again;

	case STASG:
		if( setstr( p ) ) goto again;
		goto nomat;

	case ASG PLUS:  /* and other assignment ops */
		if( setasop(p) ) goto again;

		/* there are assumed to be no side effects in LHS */

		p2 = tcopy(p);
		p->n_op = ASSIGN;
		reclaim( p->n_right, RNULL, 0 );
		p->n_right = p2;
		canon(p);
		rallo( p, p->n_rall );

# ifndef BUG4
		if( odebug ) fwalk( p, e2print, 0 );
# endif

		order( p2->n_left, INTBREG|INTAREG );
		order( p2, INTBREG|INTAREG );
		goto again;

	case ASSIGN:
		if (setasg(p))
			goto again;
		goto nomat;


	case BITYPE:
		if( setbin( p ) ) goto again;
		/* try to replace binary ops by =ops */
		switch(o){

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
			p->n_op = ASG o;
			goto again;
			}
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

	case UNARY CALL:
	case UNARY FORTCALL:
	case UNARY STCALL:
		++callflag;
		break;

	case UNARY MUL:
		if (asgop(p->n_left->n_op))
			stoasg(p->n_left, UNARY MUL);
		break;

	case CALL:
	case FORTCALL:
	case STCALL:
		store( p->n_left );
		stoarg( p->n_right, o );
		++callflag;
		return;

	case CBRANCH:   /* to prevent complicated expressions on the LHS from being stored */
		constore( p->n_left );
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

/*
 * store conditional expressions
 * the point is, avoid storing expressions in conditional
 * conditional context, since the evaluation order is predetermined
 */
void
constore(NODE *p)
{
	store( p );
}

/* mark off calls below the current node */
void
markcall(NODE *p)
{

	again:
	switch( p->n_op ){

	case UNARY CALL:
	case UNARY STCALL:
	case UNARY FORTCALL:
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
# ifndef NESTCALLS
	if( callflag ){ /* prevent two calls from being active at once  */
		SETSTO(p,INTEMP);
		store(p); /* do again to preserve bottom up nature....  */
	}
#endif
}

int negrel[] = { NE, EQ, GT, GE, LT, LE, UGT, UGE, ULT, ULE } ;  /* negatives of relationals */

/*
 * evaluate p for truth value, and branch to true or false
 * accordingly: label <0 means fall through
 */

void
cbranch(NODE *p, int false)
{
	switch (p->n_op) {

	case ULE:
	case ULT:
	case UGE:
	case UGT:
	case EQ:
	case NE:
	case LE:
	case LT:
	case GE:
	case GT:
		p->n_op = negrel[p->n_op - EQ];
		p->n_label = false;
		codgen(p, FORCC);
		reclaim(p, RNULL, 0);
		return;

	case ICON:
		if (p->n_type != FLOAT && p->n_type != DOUBLE) {
			if ((p->n_lval == 0) && (p->n_name[0] == 0))
				cbgen(0, false, 'I');
			nfree(p);
			return;
		}
		/* fall through to default with other strange constants */

	default:
		/* get condition codes */
		codgen(p, FORCC);
		cbgen(EQ, false, 'I');
		reclaim(p, RNULL, 0);
		return;

	}
}

void
rcount()
{ /* count recursions */
	if( ++nrecur > NRECUR ){
		cerror( "expression causes compiler loop: try simplifying" );
	}
}

# ifndef BUG4
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
	case UNARY STCALL:
	case STARG:
	case STASG:
		printf( " size=%d", p->n_stsize );
		printf( " align=%d", p->n_stalign );
		break;
		}

	printf( ", " );
	tprint( p->n_type );
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
# endif

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

		ty = (szty(p->n_type) == 2)? LONG: INT;
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

	/*
	 * the size of a TEMP is on multiples of the reg size.
	 */
	if (p->n_op == TEMP) {
		struct templst *w = templst;
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
		return;
	}

	if (p->n_op == UNARY MUL) {
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
	fwalk( p, ffld, 0 ); /* look for field operators */
# endif
	walkf( p, oreg2 );  /* look for and create OREG nodes */
#ifdef MYCANON
	MYCANON(p);  /* your own canonicalization routine(s) */
#endif
	walkf( p, sucomp );  /* do the Sethi-Ullman computation */

}

