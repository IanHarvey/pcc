# include "pass1.h"
# include "pass2.h"	/* for NOPREF */

# include <setjmp.h>
# include <stdarg.h>
# include <string.h>

int adebug = 0;	/* XXX 4.4 */
extern int ddebug;	/* XXX 4.4 */

void chkpun(NODE *p);
int opact(NODE *p);
int moditype(TWORD);
static void to_pass2(NODE *p);

/* corrections when in violation of lint */

/*	some special actions, used in finding the type of nodes */
# define NCVT 01
# define PUN 02
# define TYPL 04
# define TYPR 010
# define TYMATCH 040
# define LVAL 0100
# define CVTO 0200
# define CVTL 0400
# define CVTR 01000
# define PTMATCH 02000
# define OTHER 04000
# define NCVTR 010000

#ifdef PCC_DEBUG
static void
printact(NODE *t, int acts)	/* XXX 4.4  hela printact */
{
	static struct actions {
		int	a_bit;
		char	*a_name;
	} actions[] = {
		{ PUN,		"PUN" },
		{ CVTL,		"CVTL" },
		{ CVTR,		"CVTR" },
		{ TYPL,		"TYPL" },
		{ TYPR,		"TYPR" },
		{ TYMATCH,	"TYMATCH" },
		{ PTMATCH,	"PTMATCH" },
		{ LVAL,		"LVAL" },
		{ CVTO,		"CVTO" },
		{ NCVT,		"NCVT" },
		{ OTHER,	"OTHER" },
		{ NCVTR,	"NCVTR" },
		{ 0 }
	};
	register struct actions *p;
	char *sep = " ";

	printf("actions");
	for (p = actions; p->a_name; p++)
		if (p->a_bit & acts) {
			printf("%s%s", sep, p->a_name);
			sep = "|";
		}
	if (!bdebug) {
		printf(" for:\n");
		fwalk(t, eprint, 0);
	} else
		putchar('\n');
}
#endif

/* node conventions:

	NAME:	rval>0 is stab index for external
		rval<0 is -inlabel number
		lval is offset in bits
	ICON:	lval has the value
		rval has the STAB index, or - label number,
			if a name whose address is in the constant
		rval = NONAME means no name
	REG:	rval is reg. identification cookie

	*/

int bdebug = 0;

NODE *
buildtree(int o, NODE *l, NODE *r)
{
	NODE *p, *q;
	int actions;
	int opty;
	struct symtab *sp;
	NODE *lr, *ll;
	char *name;
	struct symtab **elem;

# ifndef BUG1
	if (bdebug)
		printf("buildtree(%s, %p, %p)\n", opst[o], l, r);
# endif
	opty = optype(o);

	/* check for constants */

	if( opty == UTYPE && l->n_op == ICON ){

		switch( o ){

		case NOT:
		case UNARY MINUS:
		case COMPL:
			if( conval( l, o, l ) ) return(l);
			break;

			}
		}

	/* XXX 4.4  följande sats */
	else if( opty == UTYPE && (l->n_op == FCON || l->n_op == DCON) ){

		switch( o ){

		case NOT:
			if( l->n_op == FCON )
				l->n_lval = l->n_fcon == 0.0;
			else
				l->n_lval = l->n_dcon == 0.0;
			l->n_sp = NULL;
			l->n_op = ICON;
			l->n_type = INT;
			l->n_sue = MKSUE(INT);
			l->n_df = NULL;
			return(l);
		case UNARY MINUS:
			if( l->n_op == FCON )
				l->n_fcon = -l->n_fcon;
			else
				l->n_dcon = -l->n_dcon;
			return(l);
			}
		}

	else if( o==QUEST && l->n_op==ICON ) {
		l->n_op = FREE;
		r->n_op = FREE;
		if( l->n_lval ){
			tfree( r->n_right );
			return( r->n_left );
			}
		else {
			tfree( r->n_left );
			return( r->n_right );
			}
		}

	else if( (o==ANDAND || o==OROR) && (l->n_op==ICON||r->n_op==ICON) ) goto ccwarn;

	else if( opty == BITYPE && l->n_op == ICON && r->n_op == ICON ){

		switch( o ){

		case ULT:
		case UGT:
		case ULE:
		case UGE:
		case LT:
		case GT:
		case LE:
		case GE:
		case EQ:
		case NE:	/* XXX 4.4 nedanstanende sats */
			if( l->n_type == ENUMTY && r->n_type == ENUMTY ){
				p = block(o, l, r, INT, 0, MKSUE(INT));
				chkpun( p );
				p->n_op = FREE;
				}

		case ANDAND:
		case OROR:
		case CBRANCH:

		ccwarn:

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
			if( conval( l, o, r ) ) {
				r->n_op = FREE;
				return(l);
				}
			break;
			}
		}
	else if (opty == BITYPE &&	/* XXX 4.4 DCON-tillägget */
		(l->n_op == FCON || l->n_op == DCON || l->n_op == ICON) &&
		(r->n_op == FCON || r->n_op == DCON || r->n_op == ICON)) {
			if (o == PLUS || o == MINUS || o == MUL || o == DIV) {
				extern int fpe_count;
				extern jmp_buf gotfpe;

				fpe_count = 0;
				if (setjmp(gotfpe))
					goto treatfpe;
				if (l->n_op == ICON)
					l->n_dcon = l->n_lval;
				else if (l->n_op == FCON)
					l->n_dcon = l->n_fcon;
				if (r->n_op == ICON)
					r->n_dcon = r->n_lval;
				else if (r->n_op == FCON)
					r->n_dcon = r->n_fcon;
				switch (o) {

				case PLUS:
					l->n_dcon += r->n_dcon;
					break;

				case MINUS:
					l->n_dcon -= r->n_dcon;
					break;

				case MUL:
					l->n_dcon *= r->n_dcon;
					break;

				case DIV:
					if (r->n_dcon == 0)
						uerror("division by 0.");
					else
						l->n_dcon /= r->n_dcon;
					break;
					}
			treatfpe:
				if (fpe_count > 0) {
					uerror("floating point exception in constant expression");
					l->n_dcon = 1.0; /* Fairly harmless */
					}
				fpe_count = -1;
				l->n_op = DCON;
				l->n_type = DOUBLE;
				l->n_sue = MKSUE(DOUBLE);
				r->n_op = FREE;
				return (l);
			}
		}

	/* its real; we must make a new node */

	p = block(o, l, r, INT, 0, MKSUE(INT));

	actions = opact(p);
#ifdef PCC_DEBUG
	if (adebug)	/* XXX 4.4 */
		printact(p, actions);
#endif

	if( actions&LVAL ){ /* check left descendent */
		if( notlval(p->n_left) ) {
			uerror( "lvalue required" );
			}
		}

	if( actions & NCVTR ){
		p->n_left = pconvert( p->n_left );
		}
	else if( !(actions & NCVT ) ){
		switch( opty ){

		case BITYPE:
			p->n_right = pconvert( p->n_right );
		case UTYPE:
			p->n_left = pconvert( p->n_left );

			}
		}

	if ((actions&PUN) && (o!=CAST||cflag))
		chkpun(p);

	if( actions & (TYPL|TYPR) ){

		q = (actions&TYPL) ? p->n_left : p->n_right;

		p->n_type = q->n_type;
		p->n_df = q->n_df;
		p->n_sue = q->n_sue;
		}

	if( actions & CVTL ) p = convert( p, CVTL );
	if( actions & CVTR ) p = convert( p, CVTR );
	if( actions & TYMATCH ) p = tymatch(p);
	if( actions & PTMATCH ) p = ptmatch(p);

	if( actions & OTHER ){
		l = p->n_left;
		r = p->n_right;

		switch(o){

		case NAME:
			sp = spname;
			if (sp->stype == UNDEF) {
				uerror("%s undefined", sp->sname);
				/* make p look reasonable */
				p->n_type = INT;
				p->n_sue = MKSUE(INT);
				p->n_df = NULL;
				p->n_sp = sp;
				p->n_lval = 0;
				defid(p, SNULL);
				break;
			}
			p->n_type = sp->stype;
			p->n_df = sp->sdf;
			p->n_sue = sp->ssue;
			p->n_lval = 0;
			p->n_sp = sp;
			/* special case: MOETY is really an ICON... */
			if (p->n_type == MOETY) {
				p->n_sp = NULL;
				p->n_lval = sp->soffset;
				p->n_df = NULL;
				p->n_type = ENUMTY;
				p->n_op = ICON;
			}
			break;

		case ICON:
			p->n_type = INT;
			p->n_df = NULL;
			p->n_sue = MKSUE(INT);
			break;

		case STRING:
			p->n_op = NAME;
#ifdef CHAR_UNSIGNED
			p->n_type = UCHAR+ARY;
			p->n_sue = MKSUE(UCHAR);
#else
			p->n_type = CHAR+ARY;
			p->n_sue = MKSUE(CHAR);
#endif
			p->n_lval = 0;
			p->n_sp = NULL;
			break;

		case FCON:
			p->n_lval = 0;
			p->n_rval = 0;
			p->n_type = FLOAT;
			p->n_df = NULL;
			p->n_sue = MKSUE(FLOAT);
			break;

		case DCON:	/* XXX 4.4 */
			p->n_lval = 0;
			p->n_rval = 0;
			p->n_type = DOUBLE;
			p->n_df = NULL;
			p->n_sue = MKSUE(DOUBLE);
			break;

		case STREF:
			/* p->x turned into *(p+offset) */
			/* rhs must be a name; check correctness */

			/* Find member symbol struct */
			if (l->n_type != PTR+STRTY && l->n_type != PTR+UNIONTY){
				uerror("struct or union required");
				break;
			}

			if ((elem = l->n_sue->suelem) == NULL)
				uerror("undefined struct or union");

			name = r->n_name;
			for (; *elem != NULL; elem++) {
				sp = *elem;
				if (sp->sname == name)
					break;
			}
			if (*elem == NULL)
				uerror("member '%s' not declared", name);

			r->n_sp = sp;
			p = stref(p);
			break;

		case UNARY MUL:
			if( l->n_op == UNARY AND ){
				p->n_op = l->n_op = FREE;
				p = l->n_left;
				}
			if( !ISPTR(l->n_type))uerror("illegal indirection");
			p->n_type = DECREF(l->n_type);
			p->n_df = l->n_df;
			p->n_sue = l->n_sue;
			break;

		case UNARY AND:
			switch( l->n_op ){

			case UNARY MUL:
				p->n_op = l->n_op = FREE;
				p = l->n_left;
			case NAME:
				p->n_type = INCREF( l->n_type );
				p->n_df = l->n_df;
				p->n_sue = l->n_sue;
				break;

			case COMOP:
				lr = buildtree( UNARY AND, l->n_right, NIL );
				p->n_op = l->n_op = FREE;
				p = buildtree( COMOP, l->n_left, lr );
				break;

			case QUEST:
				lr = buildtree( UNARY AND, l->n_right->n_right, NIL );
				ll = buildtree( UNARY AND, l->n_right->n_left, NIL );
				p->n_op = l->n_op = l->n_right->n_op = FREE;
				p = buildtree( QUEST, l->n_left, buildtree( COLON, ll, lr ) );
				break;

# ifdef ADDROREG	/* XXX 4.4  addroreg */
			case OREG:
				/* OREG was built in clocal()
				 * for an auto or formal parameter
				 * now its address is being taken
				 * local code must unwind it
				 * back to PLUS/MINUS REG ICON
				 * according to local conventions
				 */
				p->n_op = FREE;
				p = addroreg( l );
				break;

# endif
			default:
				uerror("unacceptable operand of &: %d", l->n_op );
				break;
				}
			break;

		case LS:
		case RS:	/* XXX 4.4 satsen */
			if( l->n_type == CHAR || l->n_type == SHORT )
				p->n_type = INT;
			else if( l->n_type == UCHAR || l->n_type == USHORT )
				p->n_type = UNSIGNED;
			else
				p->n_type = l->n_type;
		case ASG LS:
		case ASG RS:
			if( r->n_type != INT )
				p->n_right = r = makety(r, INT, 0, MKSUE(INT));
			break;

		case RETURN:
		case ASSIGN:
		case CAST:
			/* structure assignment */
			/* take the addresses of the two sides; then make an
			 * operator using STASG and
			 * the addresses of left and right */

			{
				struct suedef *sue;
				TWORD t;
				union dimfun *d;

				if (l->n_sue != r->n_sue)
					uerror("assignment of different structures");

				r = buildtree(UNARY AND, r, NIL);
				t = r->n_type;
				d = r->n_df;
				sue = r->n_sue;

				l = block(STASG, l, r, t, d, sue);

				if( o == RETURN ){
					p->n_op = FREE;
					p = l;
					break;
				}

				p->n_op = UNARY MUL;
				p->n_left = l;
				p->n_right = NIL;
				break;
				}
		case COLON:
			/* structure colon */

			if (l->n_sue != r->n_sue)
				uerror( "type clash in conditional" );
			break;

		case CALL:
			p->n_right = r = fixargs(p->n_right);
		case UNARY CALL:
			if (!ISPTR(l->n_type))
				uerror("illegal function");
			p->n_type = DECREF(l->n_type);
			if (!ISFTN(p->n_type))
				uerror("illegal function");
			p->n_type = DECREF(p->n_type);
			p->n_df = l->n_df;
			p->n_sue = l->n_sue;
			if (l->n_op == UNARY AND && l->n_left->n_op == NAME &&
			    l->n_left->n_sp != NULL && l->n_left->n_sp != NULL &&
			    (l->n_left->n_sp->sclass == FORTRAN ||
			    l->n_left->n_sp->sclass == UFORTRAN)) {
				p->n_op += (FORTCALL-CALL);
			}
			if (p->n_type == STRTY || p->n_type == UNIONTY) {
				/* function returning structure */
				/*  make function really return ptr to str., with * */

				p->n_op += STCALL-CALL;
				p->n_type = INCREF(p->n_type);
				p = buildtree(UNARY MUL, p, NIL);

				}
			break;

		default:
			cerror( "other code %d", o );
			}

		}

	/*
	 * Allow (void)0 casts.
	 * XXX - anything on the right side must be possible to cast.
	 * XXX - remove void types further on.
	 */
	if (p->n_op == CAST && p->n_type == UNDEF &&
	    p->n_right->n_op == ICON)
		p->n_right->n_type = UNDEF;

	if( actions & CVTO ) p = oconvert(p);
	p = clocal(p);

#ifdef PCC_DEBUG
	if( bdebug ) fwalk( p, eprint, 0 );
#endif

	return(p);

	}

int fpe_count = -1;
jmp_buf gotfpe;

void fpe(int);
void
fpe(int a)	/* XXX 4.4 */
{
	if (fpe_count < 0)
		cerror("floating point exception");
	++fpe_count;
	longjmp(gotfpe, 1);
}

/*	* XXX 4.4 comments *
 * Rewrite arguments in a function call.
 * Structure arguments are massaged, single
 * precision floating point constants are
 * cast to double (to eliminate convert code).
 */
NODE *
fixargs( p ) register NODE *p;  {
	int o = p->n_op;

	if( o == CM ){
		p->n_left = fixargs( p->n_left );
		p->n_right = fixargs( p->n_right );
		return( p );
		}

	if( p->n_type == STRTY || p->n_type == UNIONTY ){
		p = block(STARG, p, NIL, p->n_type, p->n_df, p->n_sue);
		p->n_left = buildtree( UNARY AND, p->n_left, NIL );
		p = clocal(p);
		}
	else if( o == FCON )	/* XXX 4.4 */
		p = makety(p, DOUBLE, 0, 0);
	return( p );
}

/*
 * apply the op o to the lval part of p; if binary, rhs is val
 */
int
conval(NODE *p, int o, NODE *q)
{
	int i, u;
	CONSZ val;
	TWORD utype;	/* XXX 4.4 */

	val = q->n_lval;
	u = ISUNSIGNED(p->n_type) || ISUNSIGNED(q->n_type);
	if( u && (o==LE||o==LT||o==GE||o==GT)) o += (UGE-GE);

	if (p->n_sp != NULL && q->n_sp != NULL)
		return(0);
	if (q->n_sp != NULL && o != PLUS)
		return(0);
	if (p->n_sp != NULL && o != PLUS && o != MINUS)
		return(0);

		/* XXX 4.4 följande stycke */
	/* usual type conversions -- handle casts of constants */
#define	ISLONG(t)	((t) == LONG || (t) == ULONG)
#define	ISLONGLONG(t)	((t) == LONGLONG || (t) == ULONGLONG)
	if (ISLONG(p->n_type) || ISLONG(q->n_type))
		utype = u ? ULONG : LONG;
	else if (ISLONGLONG(p->n_type) || ISLONGLONG(q->n_type))
		utype = u ? ULONGLONG : LONGLONG;
	else
		utype = u ? UNSIGNED : INT;
	if( !ISPTR(p->n_type) && p->n_type != utype )
		p = makety(p, utype, 0, MKSUE(utype));
	if( q->n_type != utype )
		q = makety(q, utype, 0, MKSUE(utype));

	switch( o ){

	case PLUS:
		p->n_lval += val;
		if (p->n_sp == NULL) {
			p->n_rval = q->n_rval;
			p->n_type = q->n_type;
		}
		break;
	case MINUS:
		p->n_lval -= val;
		break;
	case MUL:
		p->n_lval *= val;
		break;
	case DIV:
		if( val == 0 ) uerror( "division by 0" );
		else if ( u ) p->n_lval = (unsigned) p->n_lval / val;	/* XXX 4.4 */
		else p->n_lval /= val;
		break;
	case MOD:
		if( val == 0 ) uerror( "division by 0" );
		else if ( u ) p->n_lval = (unsigned) p->n_lval % val;	/* XXX 4.4 */
		else p->n_lval %= val;
		break;
	case AND:
		p->n_lval &= val;
		break;
	case OR:
		p->n_lval |= val;
		break;
	case ER:
		p->n_lval ^= val;
		break;
	case LS:
		i = val;
		p->n_lval = p->n_lval << i;
		break;
	case RS:
		i = val;
		if ( u ) p->n_lval = (unsigned) p->n_lval >> i;	/* XXX 4.4 */
		else p->n_lval = p->n_lval >> i;
		break;

	case UNARY MINUS:
		p->n_lval = - p->n_lval;
		break;
	case COMPL:
		p->n_lval = ~p->n_lval;
		break;
	case NOT:
		p->n_lval = !p->n_lval;
		break;
	case LT:
		p->n_lval = p->n_lval < val;
		break;
	case LE:
		p->n_lval = p->n_lval <= val;
		break;
	case GT:
		p->n_lval = p->n_lval > val;
		break;
	case GE:
		p->n_lval = p->n_lval >= val;
		break;
	case ULT:
		p->n_lval = p->n_lval < (unsigned) val;
		break;
	case ULE:
		p->n_lval = p->n_lval <= (unsigned) val;
		break;
	case UGT:
		p->n_lval = p->n_lval > (unsigned) val;
		break;
	case UGE:
		p->n_lval = p->n_lval >= (unsigned) val;
		break;
	case EQ:
		p->n_lval = p->n_lval == val;
		break;
	case NE:
		p->n_lval = p->n_lval != val;
		break;
	default:
		return(0);
		}
	return(1);
	}

/*
 * Checks p for the existance of a pun.  This is called when the op of p
 * is ASSIGN, RETURN, CAST, COLON, or relational.
 * One case is when enumerations are used: this applies only to lint.
 * In the other case, one operand is a pointer, the other integer type
 * we check that this integer is in fact a constant zero...
 * in the case of ASSIGN, any assignment of pointer to integer is illegal
 * this falls out, because the LHS is never 0.
 */
void
chkpun(NODE *p)
{
	union dimfun *d1, *d2;
	NODE *q;
	int t1, t2, ref1, ref2;

	t1 = p->n_left->n_type;
	t2 = p->n_right->n_type;

	/* check for enumerations */
	if (t1==ENUMTY || t2==ENUMTY) {
		/* rob pike says this is obnoxious...
		if( logop( p->n_op ) && p->n_op != EQ && p->n_op != NE )
			werror( "comparison of enums" ); */	/* XXX 4.4 */
		if (t1==ENUMTY && t2==ENUMTY) {
			if (p->n_left->n_sue!=p->n_right->n_sue)
				werror("enumeration type clash, "
				    "operator %s", opst[p->n_op]);	/* XXX 4.4 */
			return;
		}
		if (t1 == ENUMTY)
			t1 = INT;
		if (t2 == ENUMTY)
			t2 = INT;
	}

	ref1 = ISPTR(t1) || ISARY(t1);
	ref2 = ISPTR(t2) || ISARY(t2);

	if (ref1 ^ ref2) {	/* XXX 4.4 */
		if (ref1)
			q = p->n_right;
		else
			q = p->n_left;
		if (q->n_op != ICON || q->n_lval != 0)
			werror("illegal combination of pointer "
			    "and integer, op %s", opst[p->n_op]);
	} else if (ref1) {	/* XXX 4.4 */
		if (t1 == t2) {
			if (p->n_left->n_sue != p->n_right->n_sue) {
				werror("illegal structure pointer combination");
				return;
			}
			d1 = p->n_left->n_df;
			d2 = p->n_right->n_df;
			for (;;) {
				if (ISARY(t1)) {
					if (d1->ddim != d2->ddim) {
						werror("illegal array "
						    "size combination");
						return;
					}
					++d1;
					++d2;
				} else if (ISFTN(t1)) {
					if (chkftn(d1->dfun, d2->dfun)) {
						werror("illegal function "
						    "pointer combination");
						return;
					}
					++d1;
					++d2;
				} else
					if (!ISPTR(t1))
						break;
				t1 = DECREF(t1);
			}
		} else if (t1 != INCREF(UNDEF) && t2 != INCREF(UNDEF))	/* XXX 4.4 */
			werror("illegal pointer combination");
	}
}

NODE *
stref(NODE *p)
{
	struct suedef *sue;
	union dimfun *d;
	TWORD t;
	int dsc, align;	/* XXX 4.4 */
	OFFSZ off;
	register struct symtab *q;

	/* make p->x */
	/* this is also used to reference automatic variables */

	q = p->n_right->n_sp;
	p->n_right->n_op = FREE;
	p->n_op = FREE;
	p = pconvert(p->n_left);

	/* make p look like ptr to x */

	if (!ISPTR(p->n_type))
		p->n_type = PTR+UNIONTY;

	t = INCREF(q->stype);
	d = q->sdf;
	sue = q->ssue;

	p = makety(p, t, d, sue);

	/* compute the offset to be added */

	off = q->soffset;
	dsc = q->sclass;

	if (dsc & FIELD) {  /* normalize offset */
		align = ALINT;	/* XXX 4.4 */
		sue = MKSUE(INT);
		off = (off/align)*align;	/* XXX 4.4 */
	}
	if (off != 0)
		p = clocal(block(PLUS, p, offcon(off, t, d, sue), t, d, sue));

	p = buildtree(UNARY MUL, p, NIL);

	/* if field, build field info */

	if (dsc & FIELD) {
		p = block(FLD, p, NIL, q->stype, 0, q->ssue);
		p->n_rval = PKFIELD(dsc&FLDSIZ, q->soffset%align);
	}

	p = clocal(p);
	return p;
}

int
notlval(p) register NODE *p; {

	/* return 0 if p an lvalue, 1 otherwise */

	again:

	switch( p->n_op ){

	case FLD:
		p = p->n_left;
		goto again;

	case UNARY MUL:	/* XXX 4.4 */
		/* fix the &(a=b) bug, given that a and b are structures */
		if( p->n_left->n_op == STASG ) return( 1 );
		/* and the f().a bug, given that f returns a structure */
		if( p->n_left->n_op == UNARY STCALL ||
		    p->n_left->n_op == STCALL ) return( 1 );
	case NAME:
	case OREG:
		if( ISARY(p->n_type) || ISFTN(p->n_type) ) return(1);
	case REG:
		return(0);

	default:
		return(1);

		}

	}
/* make a constant node with value i */
NODE *
bcon(int i)
{
	register NODE *p;

	p = block(ICON, NIL, NIL, INT, 0, MKSUE(INT));
	p->n_lval = i;
	p->n_sp = NULL;
	return(clocal(p));
}

NODE *
bpsize(NODE *p)
{
	return(offcon(psize(p), p->n_type, p->n_df, p->n_sue));
}

/*
 * p is a node of type pointer; psize returns the
 * size of the thing pointed to
 */
OFFSZ
psize(NODE *p)
{

	if (!ISPTR(p->n_type)) {
		uerror("pointer required");
		return(SZINT);
	}
	/* note: no pointers to fields */
	return(tsize(DECREF(p->n_type), p->n_df, p->n_sue));
}

/*
 * convert an operand of p
 * f is either CVTL or CVTR
 * operand has type int, and is converted by the size of the other side
 */
NODE *
convert(NODE *p, int f)
{
	NODE *q, *r, *s;

	q = (f == CVTL) ? p->n_left : p->n_right;

	s = bpsize(f == CVTL ? p->n_right : p->n_left);
	r = block(PMCONV, q, s, INT, 0, MKSUE(INT));
	r = clocal(r);
	/*
	 * Indexing is only allowed with integer arguments, so insert
	 * SCONV here if arg is not an integer.
	 * XXX - complain?
	 */
	if (r->n_type != INT)
		r = clocal(block(SCONV, r, NIL, INT, 0, MKSUE(INT)));
	if (f == CVTL)
		p->n_left = r;
	else
		p->n_right = r;
	return(p);
}

#ifndef econvert	/* XXX 4.4 */
void
econvert( p ) register NODE *p; {

	/* change enums to ints, or appropriate types */

	register TWORD ty;

	if( (ty=BTYPE(p->n_type)) == ENUMTY || ty == MOETY ) {
		if (p->n_sue->suesize == SZCHAR)
			ty = CHAR;
		else if (p->n_sue->suesize == SZINT)
			ty = INT;
		else if (p->n_sue->suesize == SZSHORT)
			ty = SHORT;
		else if (p->n_sue->suesize == SZLONGLONG)
			ty = LONGLONG;
		else
			ty = LONG;
		ty = ctype(ty);
		p->n_sue = MKSUE(ty);
		MODTYPE(p->n_type,ty);
		if (p->n_op == ICON && ty != LONG && ty != LONGLONG)
			p->n_type = INT, p->n_sue = MKSUE(INT);
	}
}
#endif

NODE *
pconvert( p ) register NODE *p; {

	/* if p should be changed into a pointer, do so */

	if( ISARY( p->n_type) ){
		p->n_type = DECREF( p->n_type );
		++p->n_df;
		return( buildtree( UNARY AND, p, NIL ) );
	}
	if( ISFTN( p->n_type) )
		return( buildtree( UNARY AND, p, NIL ) );

	return( p );
	}

NODE *
oconvert(p) register NODE *p; {
	/* convert the result itself: used for pointer and unsigned */

	switch(p->n_op) {

	case LE:
	case LT:
	case GE:
	case GT:
		if( ISUNSIGNED(p->n_left->n_type) || ISUNSIGNED(p->n_right->n_type) )  p->n_op += (ULE-LE);
	case EQ:
	case NE:
		return( p );

	case MINUS:
		return(  clocal( block( PVCONV,
			p, bpsize(p->n_left), INT, 0, MKSUE(INT))));
		}

	cerror( "illegal oconvert: %d", p->n_op );

	return(p);
	}

NODE *
ptmatch(p)  register NODE *p; {

	/* makes the operands of p agree; they are
	   either pointers or integers, by this time */
	/* with MINUS, the sizes must be the same */
	/* with COLON, the types must be the same */

	struct suedef *sue, *sue2;
	union dimfun *d, *d2;
	TWORD t1, t2, t;
	int o;

	o = p->n_op;
	t = t1 = p->n_left->n_type;
	t2 = p->n_right->n_type;
	d = p->n_left->n_df;
	d2 = p->n_right->n_df;
	sue = p->n_left->n_sue;
	sue2 = p->n_right->n_sue;

	switch( o ){

	case ASSIGN:
	case RETURN:
	case CAST:
		{  break; }

	case MINUS:
		{  if( psize(p->n_left) != psize(p->n_right) ){
			uerror( "illegal pointer subtraction");
			}
		   break;
		   }
	case COLON:
		if (t1 != t2) {
			/*
			 * Check for void pointer types. They are allowed
			 * to cast to/from any pointers.
			 */
			if (ISPTR(t1) && ISPTR(t2) &&
			    (BTYPE(t1) == UNDEF || BTYPE(t2) == UNDEF))
				break;
			uerror("illegal types in :");
		}
		break;

	default:  /* must work harder: relationals or comparisons */

		if( !ISPTR(t1) ){
			t = t2;
			d = d2;
			sue = sue2;
			break;
			}
		if( !ISPTR(t2) ){
			break;
			}

		/* both are pointers */
		if( talign(t2,sue2) < talign(t,sue) ){
			t = t2;
			sue = sue2;
			}
		break;
		}

	p->n_left = makety( p->n_left, t, d, 0 );
	p->n_left->n_sue = sue;
	p->n_right = makety( p->n_right, t, d, 0 );
	p->n_right->n_sue = sue;
	if( o!=MINUS && !logop(o) ){

		p->n_type = t;
		p->n_df = d;
		p->n_sue = sue;
		}

	return(clocal(p));
	}

int tdebug = 0;

NODE *
tymatch(p)  register NODE *p; {

	/* satisfy the types of various arithmetic binary ops */

	/* rules are:
		if assignment, type of LHS
		if any float or doubles, make double
		if any longs, make long
		otherwise, make int
		if either operand is unsigned, the result is...
	*/

	TWORD t1, t2, t, tu;
	int o, u;

	o = p->n_op;

	t1 = p->n_left->n_type;
	t2 = p->n_right->n_type;
	if( (t1==UNDEF || t2==UNDEF) && o!=CAST )	/* XXX 4.4 */
		uerror("void type illegal in expression");

	u = 0;
	if( ISUNSIGNED(t1) ){
		u = 1;
		t1 = DEUNSIGN(t1);
		}
	if( ISUNSIGNED(t2) ){
		u = 1;
		t2 = DEUNSIGN(t2);
		}

	if( ( t1 == CHAR || t1 == SHORT ) && o!= RETURN ) t1 = INT;
	if( t2 == CHAR || t2 == SHORT ) t2 = INT;

#ifdef SPRECC	/* XXX 4.4 */
	if( t1 == DOUBLE || t2 == DOUBLE )
		t = DOUBLE;
	else if( t1 == FLOAT || t2 == FLOAT )
		t = FLOAT;
#else
	if (t1 == DOUBLE || t1 == FLOAT || t2 == DOUBLE || t2 == FLOAT)
		t = DOUBLE;
#endif
	else if (t1==LONG || t2==LONG)
		t = LONG;
	else if (t1==LONGLONG || t2 == LONGLONG)
		t = LONGLONG;
	else
		t = INT;

	if( o == ASSIGN || o == CAST || o == RETURN ) {	/* XXX 4.4 */
		tu = p->n_left->n_type;
		t = t1;
	} else {
		tu = (u && UNSIGNABLE(t))?ENUNSIGN(t):t;
	}

	/* because expressions have values that are at least as wide
	   as INT or UNSIGNED, the only conversions needed
	   are those involving FLOAT/DOUBLE, and those
	   from LONG to INT and ULONG to UNSIGNED */

	if( (t != t1 || (u && !ISUNSIGNED(p->n_left->n_type))) && ! asgop(o) )	/* XXX 4.4 */
		p->n_left = makety( p->n_left, tu, 0, MKSUE(tu));	/* XXX 4.4 */

	if( t != t2 || (u && !ISUNSIGNED(p->n_right->n_type)) || o==CAST) {	/* XXX 4.4 */
		if ( tu == ENUMTY ) {/* always asgop */	/* XXX 4.4 */
			p->n_right = makety( p->n_right, INT, 0, MKSUE(INT));	/* XXX 4.4 */
			p->n_right->n_type = tu;	/* XXX 4.4 */
			p->n_right->n_df = p->n_left->n_df;	/* XXX 4.4 */
			p->n_right->n_sue = p->n_left->n_sue;	/* XXX 4.4 */
			}	/* XXX 4.4 */
		else	/* XXX 4.4 */
			p->n_right = makety( p->n_right, tu, 0, MKSUE(tu));	/* XXX 4.4 */
	}

	if( asgop(o) ){
		p->n_type = p->n_left->n_type;
		p->n_df = p->n_left->n_df;
		p->n_sue = p->n_left->n_sue;
		}
	else if( !logop(o) ){
		p->n_type = tu;
		p->n_df = NULL;
		p->n_sue = MKSUE(t);
		}

# ifndef BUG1
	if (tdebug)
		printf("tymatch(%p): %o %s %o => %o\n",p,t1,opst[o],t2,tu );
# endif

	return(p);
	}

NODE *
makety(NODE *p, TWORD t, union dimfun *d, struct suedef *sue)
{
	/* make p into type t by inserting a conversion */

	if( p->n_type == ENUMTY && p->n_op == ICON ) econvert(p);
	if( t == p->n_type ){
		p->n_df = d;
		p->n_sue = sue;
		return(p);
	}

	if( t & TMASK ){
		/* non-simple type */
		return( block( PCONV, p, NIL, t, d, sue));
		}

	if( p->n_op == ICON ){
		if (t == DOUBLE) {	/* XXX 4.4 */
			p->n_op = DCON;
			if (ISUNSIGNED(p->n_type))
				p->n_dcon = (U_CONSZ) p->n_lval;
			else
				p->n_dcon = p->n_lval;
			p->n_type = t;
			p->n_sue = MKSUE(t);
			return (clocal(p));
		}
		if (t == FLOAT) {	/* XXX 4.4 */
			p->n_op = FCON;
			if( ISUNSIGNED(p->n_type) ){
				p->n_fcon = (U_CONSZ) p->n_lval;
				}
			else {
				p->n_fcon = p->n_lval;
				}

			p->n_type = t;
			p->n_sue = MKSUE(t);
			return( clocal(p) );
			}
		}
	else if (p->n_op == FCON && t == DOUBLE) {	/* XXX 4.4 */
		double db;

		p->n_op = DCON;	/* XXX 4.4 */
		db = p->n_fcon;	/* XXX 4.4 */
		p->n_dcon = db;	/* XXX 4.4 */
		p->n_type = t;	/* XXX 4.4 */
		p->n_sue = MKSUE(t);	/* XXX 4.4 */
		return (clocal(p));	/* XXX 4.4 */
	} else if (p->n_op == DCON && t == FLOAT) {	/* XXX 4.4 */
		float fl;	/* XXX 4.4 */
	/* XXX 4.4 */
		p->n_op = FCON;	/* XXX 4.4 */
		fl = p->n_dcon;	/* XXX 4.4 */
#ifdef notdef
		if (fl != p->n_dcon)
			werror("float conversion loses precision");
#endif
		p->n_fcon = fl;
		p->n_type = t;
		p->n_sue = MKSUE(t);
		return (clocal(p));
	}

	return( clocal( block( SCONV, p, NIL, t, d, sue)));	/* XXX 4.4 */

	}

NODE *
block(int o, NODE *l, NODE *r, TWORD t, union dimfun *d, struct suedef *sue)
{
	register NODE *p;

	p = talloc();
	p->n_rval = 0;
	p->n_op = o;
	p->n_lval = 0; /* Protect against large lval */
	p->n_left = l;
	p->n_right = r;
	p->n_type = t;
	p->n_df = d;
	p->n_sue = sue;
	return(p);
	}

int
icons(p) register NODE *p; {
	/* if p is an integer constant, return its value */
	int val;

	if( p->n_op != ICON ){
		uerror( "constant expected");
		val = 1;
		}
	else {
		val = p->n_lval;
		if( val != p->n_lval ) uerror( "constant too big for cross-compiler" );
		}
	tfree( p );
	return(val);
}

/* 
 * the intent of this table is to examine the
 * operators, and to check them for
 * correctness.
 * 
 * The table is searched for the op and the
 * modified type (where this is one of the
 * types INT (includes char and short), LONG,
 * DOUBLE (includes FLOAT), and POINTER
 * 
 * The default action is to make the node type integer
 * 
 * The actions taken include:
 * 	PUN	  check for puns
 * 	CVTL	  convert the left operand
 * 	CVTR	  convert the right operand
 * 	TYPL	  the type is determined by the left operand
 * 	TYPR	  the type is determined by the right operand
 * 	TYMATCH	  force type of left and right to match,by inserting conversions
 * 	PTMATCH	  like TYMATCH, but for pointers
 * 	LVAL	  left operand must be lval
 * 	CVTO	  convert the op
 * 	NCVT	  do not convert the operands
 * 	OTHER	  handled by code
 * 	NCVTR	  convert the left operand, not the right...
 * 
 */

# define MINT 01	/* integer */
# define MDBI 02	/* integer or double */
# define MSTR 04	/* structure */
# define MPTR 010	/* pointer */
# define MPTI 020	/* pointer or integer */
# define MENU 040	/* enumeration variable or member */
# define MVOID 0100000	/* void type */	/* XXX 4.4 */

int
opact(NODE *p)	/* XXX 4.4 hela opact mixtrad med */
{
	int mt12, mt1, mt2, o;

	mt1 = mt2 = mt12 = 0;

	switch (optype(o = p->n_op)) {
	case BITYPE:
		mt2 = moditype(p->n_right->n_type);
	case UTYPE:
		mt1 = moditype(p->n_left->n_type);
		break;
	}

	if( ((mt1 | mt2) & MVOID) &&
	    o != COMOP &&
	    o != COLON &&
	    !(o == QUEST && (mt1 & MVOID) == 0) &&
	    !(o == CAST && (mt1 & MVOID)) ){
		/* if lhs of RETURN is void, grammar will complain */
		if (o != RETURN)
			uerror("value of void expression used");
		return( NCVT );
	}
	mt12 = mt1 & mt2;

	switch( o ){

	case NAME :
	case STRING :
	case ICON :
	case FCON :
	case DCON :
	case CALL :
	case UNARY CALL:
	case UNARY MUL:
		{  return( OTHER ); }
	case UNARY MINUS:
		if( mt1 & MENU ) return( 0 );
		if( mt1 & MDBI ) return( TYPL );
		break;

	case COMPL:
		if( mt1 & MENU ) return( 0 );
		if( mt1 & MINT ) return( TYPL );
		break;

	case UNARY AND:
		return( NCVT+OTHER );
	case INIT:
	case CM:
		return( 0 );

	case NOT:
	case CBRANCH:
		if( mt1 & MSTR ) break;
		return( 0 );

	case ANDAND:
	case OROR:
		if( (mt1 & MSTR) || (mt2 & MSTR) ) break;
		return( 0 );

	case MUL:
	case DIV:
		if( mt12 & MDBI ) return( TYMATCH );
		break;

	case MOD:
	case AND:
	case OR:
	case ER:
		if( mt12 & MINT ) return( TYMATCH );
		break;

	case LS:
	case RS:
		if( mt12 & MINT ) return( OTHER );
		break;

	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
		if( mt12 & MENU ) return( TYMATCH+NCVT+PUN );
		if( mt12 & MDBI ) return( TYMATCH+NCVT+CVTO );
		else if( mt12 & MPTR ) return( PTMATCH+PUN );
		else if( mt12 & MPTI ) return( PTMATCH+PUN );
		else break;

	case QUEST:
	case COMOP:
		if( mt2&MENU ) return( TYPR+NCVTR );
		return( TYPR );

	case STREF:
		return( NCVTR+OTHER );

	case FORCE:
		return( TYPL );

	case COLON:
		if( mt12 & MENU ) return( NCVT+PUN+TYMATCH );
		else if( mt12 & MDBI ) return( NCVT+TYMATCH );
		else if( mt12 & MPTR ) return( TYPL+PTMATCH+PUN );
		else if( (mt1&MINT) && (mt2&MPTR) ) return( TYPR+PUN );
		else if( (mt1&MPTR) && (mt2&MINT) ) return( TYPL+PUN );
		else if( mt12 & MSTR ) return( NCVT+TYPL+OTHER );
		else if( mt12 == MVOID ) return( NCVT+TYPL );
		break;

	case ASSIGN:
	case RETURN:
		if( mt12 & MSTR ) return( LVAL+NCVT+TYPL+OTHER );
		else if( mt12 & MENU ) return( LVAL+NCVT+TYPL+TYMATCH+PUN );
	case CAST:
		if(o==CAST && mt1==MVOID)return(TYPL+TYMATCH);
		else if( mt12 & MDBI ) return( TYPL+LVAL+NCVT+TYMATCH );
		else if( mt2 == MVOID &&
		        ( p->n_right->n_op == CALL ||
			  p->n_right->n_op == UNARY CALL)) break;
		else if( (mt1 & MPTR) && (mt2 & MPTI) )
			return( LVAL+PTMATCH+PUN );
		else if( mt12 & MPTI ) return( TYPL+LVAL+TYMATCH+PUN );
		break;

	case ASG LS:
	case ASG RS:
		if( mt12 & MINT ) return( TYPL+LVAL+OTHER );
		break;

	case ASG MUL:
	case ASG DIV:
		if( mt12 & MDBI ) return( LVAL+TYMATCH );
		break;

	case ASG MOD:
	case ASG AND:
	case ASG OR:
	case ASG ER:
		if (mt12 & MINT)
			return(LVAL+TYMATCH);
		break;

	case ASG PLUS:
	case ASG MINUS:
	case INCR:
	case DECR:
		if (mt12 & MDBI)
			return(TYMATCH+LVAL);
		else if ((mt1&MPTR) && (mt2&MINT))
			return(TYPL+LVAL+CVTR);
		break;

	case MINUS:
		if (mt12 & MPTR)
			return(CVTO+PTMATCH+PUN);
		if (mt2 & MPTR)
			break;
		/* FALLTHROUGH */
	case PLUS:
		if (mt12 & MDBI)
			return(TYMATCH);
		else if ((mt1&MPTR) && (mt2&MINT))
			return(TYPL+CVTR);
		else if ((mt1&MINT) && (mt2&MPTR))
			return(TYPR+CVTL);

	}
	if (mt12 == MSTR)
		uerror("%s is not a permitted struct/union operation", opst[o]);
	else
		uerror("operands of %s have incompatible types", opst[o]);
	return(NCVT);
}

int
moditype(TWORD ty)
{
	switch (ty) {

	case UNDEF:
		return( MVOID );
	case ENUMTY:
	case MOETY:
		return( MENU|MINT|MDBI|MPTI );  /* enums are ints */

	case STRTY:
	case UNIONTY:
		return( MSTR );

	case CHAR:
	case SHORT:
	case UCHAR:
	case USHORT:
		return( MINT|MPTI|MDBI );
	case UNSIGNED:
	case ULONG:
	case ULONGLONG:
	case INT:
	case LONG:
	case LONGLONG:
		return( MINT|MDBI|MPTI );
	case FLOAT:
	case DOUBLE:
		return( MDBI );
	default:
		return( MPTR|MPTI );

	}
}

/*
 * Do sizeof on p.
 * XXX - add runtime evaluation sizeof.
 */
NODE *
doszof(NODE *p)
{
	int i;

	i = tsize( p->n_type, p->n_df, p->n_sue )/SZCHAR;

	tfree(p);
	if (i <= 0)
		werror( "sizeof returns 0" );
	return (bcon(i));
}

#ifdef PCC_DEBUG
int
eprint(NODE *p, int down, int *a, int *b)
{
	int ty;

	*a = *b = down+1;
	while( down > 1 ){
		printf( "\t" );
		down -= 2;
		}
	if( down ) printf( "    " );

	ty = optype( p->n_op );

	printf("%p) %s, ", p, opst[p->n_op] );
	if (ty == LTYPE) {
		printf(CONFMT, p->n_lval);
		printf(", %d, ", p->n_rval);
	}
	tprint( p->n_type );
	printf( ", %p, %p\n", p->n_df, p->n_sue );
	return 0;
}
# endif

#ifndef PRTDCON
void
prtdcon(NODE *p)
{
	int o = p->n_op, i;

	if( o == DCON || o == FCON ){
		(void) locctr( DATA );
		defalign( o == DCON ? ALDOUBLE : ALFLOAT );
		deflab(i = getlab());
		if( o == FCON )
			fincode( p->n_fcon, SZFLOAT );
		else
			fincode( p->n_dcon, SZDOUBLE );
		p->n_op = NAME;
		p->n_type = (o == DCON ? DOUBLE : FLOAT);
		p->n_lval = 0;
		p->n_sp = tmpalloc(sizeof(struct symtab_hdr));
		p->n_sp->sclass = ILABEL;
		p->n_sp->soffset = i;
	}
}
#endif PRTDCON


int edebug = 0;
void
ecomp( p ) register NODE *p; {
#ifdef PCC_DEBUG
	if( edebug ) fwalk( p, eprint, 0 );
#endif
	if( !reached ){
		werror( "statement not reached" );
		reached = 1;
		}
	p = optim(p);
	walkf( p, prtdcon );
	(void) locctr( PROG );
	ecode( p );
	tfree(p);
	}

# ifdef STDPRTREE

void
p2tree(NODE *p)
{
	int ty;

# ifdef MYP2TREE
	MYP2TREE(p);  /* local action can be taken here; then return... */
# endif

	ty = optype(p->n_op);

	switch( p->n_op ){

	case NAME:
	case ICON:
		if (p->n_sp != NULL) {
			if (p->n_sp->sflags & SLABEL ||
			    p->n_sp->sclass == ILABEL) {
				char *cp = (isinlining ?
				    permalloc(32) : tmpalloc(32));
				sprintf(cp, LABFMT, p->n_sp->soffset);
				p->n_name = cp;
			} else {
				p->n_name = exname(p->n_sp->sname);
			}
		} else
			p->n_name = "";
		break;

	case STARG:
	case STASG:
	case STCALL:
	case UNARY STCALL:
		/* set up size parameters */
		p->n_stsize = (tsize(STRTY,p->n_left->n_df,p->n_left->n_sue)+SZCHAR-1)/SZCHAR;
		p->n_stalign = talign(STRTY,p->n_left->n_sue)/SZCHAR;
		break;

	case REG:
		rbusy( p->n_rval, p->n_type );
	default:
		p->n_name = "";
		}

	p->n_rall = NOPREF;

	if( ty != LTYPE ) p2tree( p->n_left );
	if( ty == BITYPE ) p2tree( p->n_right );
	}

# endif

void
to_pass2(NODE *p)
{
	if (isinlining)
		inline_savenode(p);
	else
		p2compile(p);
}

void
ecode(NODE *p)  
{
	/* walk the tree and write out the nodes.. */

	if (nerrors)    
		return;
#ifdef PCC_DEBUG
	if (xdebug) {
		printf("Fulltree:\n"); 
		fwalk(p, eprint, 0); 
	}
#endif
	p2tree(p);      
	to_pass2(p);
}

/*
 * Printout something in pass1.
 * This is spooled up if inline is defined.
 * XXX - should be done in some other way.
 */
void
p1print(char *fmt, ...)
{
	va_list ap;
	char *buf;

	va_start(ap, fmt);
	if (isinlining) {
		vasprintf(&buf, fmt, ap);
		inline_savestring(buf);
	} else
		vprintf(fmt, ap);
	va_end(ap);
}
