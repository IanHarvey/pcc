# include "pass1.h"
# include "pass2.h"	/* for NOPREF */

# include <setjmp.h>
# include <stdarg.h>
# include <string.h>

void chkpun(NODE *p);
int opact(NODE *p);
int moditype(TWORD);
static NODE *strargs(NODE *);

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
	} else if( o == UNARY MINUS && l->n_op == FCON ){
			if( l->n_type == FLOAT )
				l->n_fcon = -l->n_fcon;
			else
				l->n_dcon = -l->n_dcon;
			return(l);

	} else if( o==QUEST && l->n_op==ICON ) {
		CONSZ c = l->n_lval;
		nfree(l);
		if (c) {
			tfree(r->n_right);
			l = r->n_left;
			nfree(r);
			return(l);
		} else {
			tfree(r->n_left);
			l = r->n_right;
			nfree(r);
			return(l);
		}
	} else if( (o==ANDAND || o==OROR) && (l->n_op==ICON||r->n_op==ICON) )
		goto ccwarn;

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
		case NE:
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
				nfree(r);
				return(l);
			}
			break;
		}
	} else if (opty == BITYPE && (l->n_op == FCON || l->n_op == ICON) &&
	    (r->n_op == FCON || r->n_op == ICON) && (o == PLUS || o == MINUS ||
	    o == MUL || o == DIV)) {
		switch(o){
		case PLUS:
		case MINUS:
		case MUL:
		case DIV:
			if (l->n_op == ICON)
				l->n_dcon = l->n_lval;
			else if (l->n_type == FLOAT)
				l->n_dcon = l->n_fcon;
			if (r->n_op == ICON)
				r->n_dcon = r->n_lval;
			else if (r->n_type == FLOAT)
				r->n_dcon = r->n_fcon;
			switch (o) {
			case PLUS:
				l->n_dcon += r->n_dcon; break;
			case MINUS:
				l->n_dcon -= r->n_dcon; break;
			case MUL:
				l->n_dcon *= r->n_dcon; break;
			case DIV:
				if (r->n_dcon == 0)
					uerror("division by 0.");
				else
					l->n_dcon /= r->n_dcon;
			}
			l->n_op = FCON;
			l->n_type = DOUBLE;
			l->n_sue = MKSUE(DOUBLE);
			nfree(r);
			return(l);
		}
	}

	/* its real; we must make a new node */

	p = block(o, l, r, INT, 0, MKSUE(INT));

	actions = opact(p);

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
			if (l->n_op == UNARY AND) {
				nfree(p);
				p = l->n_left;
				nfree(l);
			}
			if( !ISPTR(l->n_type))uerror("illegal indirection");
			p->n_type = DECREF(l->n_type);
			p->n_df = l->n_df;
			p->n_sue = l->n_sue;
			break;

		case UNARY AND:
			switch( l->n_op ){

			case UNARY MUL:
				nfree(p);
				p = l->n_left;
				nfree(l);
			case NAME:
				p->n_type = INCREF( l->n_type );
				p->n_df = l->n_df;
				p->n_sue = l->n_sue;
				break;

			case COMOP:
				nfree(p);
				lr = buildtree( UNARY AND, l->n_right, NIL );
				p = buildtree( COMOP, l->n_left, lr );
				nfree(l);
				break;

			case QUEST:
				lr = buildtree( UNARY AND, l->n_right->n_right, NIL );
				ll = buildtree( UNARY AND, l->n_right->n_left, NIL );
				nfree(p); nfree(l->n_right);
				p = buildtree( QUEST, l->n_left, buildtree( COLON, ll, lr ) );
				nfree(l);
				break;

			default:
				uerror("unacceptable operand of &: %d", l->n_op );
				break;
				}
			break;

		case LS:
		case RS:
		case ASG LS:
		case ASG RS:
			if(tsize(p->n_right->n_type, p->n_right->n_df, p->n_right->n_sue) > SZINT)
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
					nfree(p);
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
			p->n_right = r = strargs(p->n_right);
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
	if (p->n_op == CAST && p->n_type == VOID &&
	    p->n_right->n_op == ICON)
		p->n_right->n_type = VOID;

	if( actions & CVTO ) p = oconvert(p);
	p = clocal(p);

#ifdef PCC_DEBUG
	if( bdebug ) fwalk( p, eprint, 0 );
#endif

	return(p);

	}

NODE *
strargs( p ) register NODE *p;  { /* rewrite structure flavored arguments */

	if( p->n_op == CM ){
		p->n_left = strargs( p->n_left );
		p->n_right = strargs( p->n_right );
		return( p );
		}

	if( p->n_type == STRTY || p->n_type == UNIONTY ){
		p = block(STARG, p, NIL, p->n_type, p->n_df, p->n_sue);
		p->n_left = buildtree( UNARY AND, p->n_left, NIL );
		p = clocal(p);
		}
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

	val = q->n_lval;
	u = ISUNSIGNED(p->n_type) || ISUNSIGNED(q->n_type);
	if( u && (o==LE||o==LT||o==GE||o==GT)) o += (UGE-GE);

	if (p->n_sp != NULL && q->n_sp != NULL)
		return(0);
	if (q->n_sp != NULL && o != PLUS)
		return(0);
	if (p->n_sp != NULL && o != PLUS && o != MINUS)
		return(0);

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
		if (val == 0)
			uerror("division by 0");
		else 
			p->n_lval /= val;
		break;
	case MOD:
		if (val == 0)
			uerror("division by 0");
		else 
			p->n_lval %= val;
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
		p->n_lval = p->n_lval >> i;
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
		p->n_lval = (p->n_lval-val)<0;
		break;
	case ULE:
		p->n_lval = (p->n_lval-val)<=0;
		break;
	case UGT:
		p->n_lval = (p->n_lval-val)>=0;
		break;
	case UGE:
		p->n_lval = (p->n_lval-val)>0;
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
	int t1, t2;

	t1 = p->n_left->n_type;
	t2 = p->n_right->n_type;

	/* check for enumerations */
	if (t1==ENUMTY || t2==ENUMTY) {
		if( logop( p->n_op ) && p->n_op != EQ && p->n_op != NE ) {
			uerror( "comparison of enums" );
			return;
			}
		if (t1==ENUMTY && t2==ENUMTY) {
			if (p->n_left->n_sue!=p->n_right->n_sue)
				werror("enumeration type clash, "
				    "operator %s", opst[p->n_op]);
			return;
		}
	}

	if( ISPTR(t1) || ISARY(t1) ) q = p->n_right;
	else q = p->n_left;

	if ( !ISPTR(q->n_type) && !ISARY(q->n_type) ){
		if (q->n_op != ICON || q->n_lval != 0)
			werror("illegal combination of pointer and integer");
	} else {
		d1 = p->n_left->n_df;
		d2 = p->n_right->n_df;
		for (;;) {
			if( t1 == t2 ) {;
				if (p->n_left->n_sue != p->n_right->n_sue) {
					werror("illegal structure pointer combination");
				}
				return;
			}
			if (ISARY(t1) || ISPTR(t1) ) {
				if( !ISARY(t2) && !ISPTR(t2) ) break;
				if (ISARY(t1) && ISARY(t2) && d1->ddim != d2->ddim) {
					werror("illegal array size combination");
					return;
				}
				if( ISARY(t1) ) ++d1;
				if( ISARY(t2) ) ++d2; ++d2;
			} else if (ISFTN(t1)) {
				if (chkftn(d1->dfun, d2->dfun)) {
					werror("illegal function "
					    "pointer combination");
					return;
				}
				++d1;
				++d2;
			} else break;
			t1 = DECREF(t1);
			t2 = DECREF(t2);
		}
		werror("illegal pointer combination");
	}
}

NODE *
stref(NODE *p)
{
	NODE *r;
	struct suedef *sue;
	union dimfun *d;
	TWORD t;
	int dsc;
	OFFSZ off;
	register struct symtab *q;

	/* make p->x */
	/* this is also used to reference automatic variables */

	q = p->n_right->n_sp;
	nfree(p->n_right);
	r = p->n_left;
	nfree(p);
	p = pconvert(r);

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

	if (dsc & FIELD) {  /* make fields look like ints */
		off = (off/ALINT)*ALINT;
		sue = MKSUE(INT);
	}
	if (off != 0)
		p = clocal(block(PLUS, p, offcon(off, t, d, sue), t, d, sue));

	p = buildtree(UNARY MUL, p, NIL);

	/* if field, build field info */

	if (dsc & FIELD) {
		p = block(FLD, p, NIL, q->stype, 0, q->ssue);
		p->n_rval = PKFIELD(dsc&FLDSIZ, q->soffset%ALINT);
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

	case NAME:
	case OREG:
	case UNARY MUL:
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
			    (BTYPE(t1) == VOID || BTYPE(t2) == VOID))
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

	p->n_left = makety( p->n_left, t, d, sue );
	p->n_right = makety( p->n_right, t, d, sue );
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

	if (t1 == DOUBLE || t1 == FLOAT || t2 == DOUBLE || t2 == FLOAT)
		t = DOUBLE;
	else if (t1==LONG || t2==LONG)
		t = LONG;
	else if (t1==LONGLONG || t2 == LONGLONG)
		t = LONGLONG;
	else
		t = INT;

	if( asgop(o) ){
		tu = p->n_left->n_type;
		t = t1;
	} else {
		tu = (u && UNSIGNABLE(t))?ENUNSIGN(t):t;
	}

	/* because expressions have values that are at least as wide
	   as INT or UNSIGNED, the only conversions needed
	   are those involving FLOAT/DOUBLE, and those
	   from LONG to INT and ULONG to UNSIGNED */


	if( t != t1 ) p->n_left = makety( p->n_left, tu, 0, MKSUE(tu));

	if( t != t2 || o==CAST) p->n_right = makety( p->n_right, tu, 0, MKSUE(tu));

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

#ifdef PCC_DEBUG
	if (tdebug)
		printf("tymatch(%p): %o %s %o => %o\n",p,t1,opst[o],t2,tu );
#endif

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
		if (t == DOUBLE || t == FLOAT) {
			p->n_op = FCON;
			if (ISUNSIGNED(p->n_type))
				p->n_dcon = (U_CONSZ) p->n_lval;
			else
				p->n_dcon = p->n_lval;
			p->n_type = t;
			p->n_sue = MKSUE(t);
			return (clocal(p));
		}
	}
	return( clocal( block( SCONV, p, NIL, t, d, sue)));

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

int
opact(NODE *p)
{
	int mt12, mt1, mt2, o;

	mt12 = 0;

	switch (optype(o = p->n_op)) {
	case BITYPE:
		mt12=mt2 = moditype(p->n_right->n_type);
	case UTYPE:
		mt12 &= (mt1 = moditype(p->n_left->n_type));
		break;
	}

	switch( o ){

	case NAME :
	case STRING :
	case ICON :
	case FCON :
	case CALL :
	case UNARY CALL:
	case UNARY MUL:
		{  return( OTHER ); }
	case UNARY MINUS:
		if( mt1 & MDBI ) return( TYPL );
		break;

	case COMPL:
		if( mt1 & MINT ) return( TYPL );
		break;

	case UNARY AND:
		return( NCVT+OTHER );
	case INIT:
	case CM:
	case NOT:
	case CBRANCH:
	case ANDAND:
	case OROR:
		return( 0 );

	case MUL:
	case DIV:
		if ((mt1&MDBI) && (mt2&MENU)) return( TYMATCH );
		if ((mt2&MDBI) && (mt1&MENU)) return( TYMATCH );
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
		if( mt12 & MINT ) return( TYPL+OTHER );
		break;

	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
		if( (mt1&MENU)||(mt2&MENU) ) return( PTMATCH+PUN+NCVT );
		if( mt12 & MDBI ) return( TYMATCH+CVTO );
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
		if( mt12 & MENU ) return( NCVT+PUN+PTMATCH );
		else if( mt12 & MDBI ) return( TYMATCH );
		else if( mt12 & MPTR ) return( TYPL+PTMATCH+PUN );
		else if( (mt1&MINT) && (mt2&MPTR) ) return( TYPR+PUN );
		else if( (mt1&MPTR) && (mt2&MINT) ) return( TYPL+PUN );
		else if( mt12 & MSTR ) return( NCVT+TYPL+OTHER );
		break;

	case ASSIGN:
	case RETURN:
		if( mt12 & MSTR ) return( LVAL+NCVT+TYPL+OTHER );
	case CAST:
		if( mt12 & MDBI ) return( TYPL+LVAL+TYMATCH );
		else if( (mt1&MENU)||(mt2&MENU) )
			return( LVAL+NCVT+TYPL+PTMATCH+PUN );
		else if( mt1 & MPTR) return( LVAL+PTMATCH+PUN );
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
		else if ((mt1&MPTR) && (mt2&MINT || mt2&MENU))
			return(TYPL+CVTR);
		else if ((mt1&MINT || mt1&MENU) && (mt2&MPTR))
			return(TYPR+CVTL);

	}
	uerror("operands of %s have incompatible types", opst[o]);
	return(NCVT);
}

int
moditype(TWORD ty)
{
	switch (ty) {

	case ENUMTY:
	case MOETY:
		return( MENU );

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

void
prtdcon(NODE *p)
{
	int o = p->n_op, i;

	if( o == FCON ){
		send_passt(IP_LOCCTR, DATA);
		defalign( p->n_type == DOUBLE ? ALDOUBLE : ALFLOAT );
		deflab(i = getlab());
		fincode( p, p->n_type == DOUBLE ? SZDOUBLE : SZFLOAT );
		p->n_op = NAME;
		p->n_lval = 0;
		p->n_sp = tmpalloc(sizeof(struct symtab_hdr));
		p->n_sp->sclass = ILABEL;
		p->n_sp->soffset = i;
	}
}


int edebug = 0;
void
ecomp(NODE *p)
{

#ifdef PCC_DEBUG
	if (edebug)
		fwalk(p, eprint, 0);
#endif
	if (!reached) {
		werror("statement not reached");
		reached = 1;
	}
	p = optim(p);
	walkf(p, prtdcon);
	send_passt(IP_LOCCTR, PROG);
	ecode(p);
}

#ifdef STDPRTREE
#if defined(MULTIPASS)
void	
p2tree(NODE *p)
{
	struct symtab *q;
	int ty;

# ifdef MYP2TREE
	MYP2TREE(p);  /* local action can be taken here; then return... */
# endif

	ty = optype(p->n_op);

	printf("%d\t", p->n_op);

	if (ty == LTYPE) {
		printf(CONFMT, p->n_lval);
		printf("\t");
	}
	if (ty != BITYPE) {
		if (p->n_op == NAME || p->n_op == ICON)
			printf("0\t");
		else
			printf("%d\t", p->n_rval);
		}

	printf("%o\t", p->n_type);

	/* handle special cases */

	switch (p->n_op) {

	case NAME:
	case ICON:
		/* print external name */
		if (p->n_sp != NULL) {
			if (p->n_sp->sflags & SLABEL ||
			    p->n_sp->sclass == ILABEL) {
				printf(LABFMT, p->n_sp->soffset);
			} else
				printf("%s\n", exname(q->sname));
		} else
			printf("\n");
		break;

	case STARG:
	case STASG:
	case STCALL:
	case UNARY STCALL:
		/* print out size */
		/* use lhs size, in order to avoid hassles
		 * with the structure `.' operator
		 */

		/* note: p->left not a field... */
		printf(CONFMT, (CONSZ)tsize(STRTY, p->n_left->n_df,
		    p->n_left->n_sue));
		printf("\t%d\t\n", talign(STRTY, p->n_left->n_sue));
		break;

	default:
		printf(	 "\n" );
	}

	if (ty != LTYPE)
		p2tree(p->n_left);
	if (ty == BITYPE)
		p2tree(p->n_right);
}
#else
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

#endif
#endif

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
#if !defined(MULTIPASS)
	send_passt(IP_NODE, p);
#endif
}

/*
 * Send something further on to the next pass.
 */
void
send_passt(int type, ...)
{
	struct interpass *ip;
	va_list ap;

	va_start(ap, type);
	ip = isinlining ? permalloc(sizeof(*ip)) : tmpalloc(sizeof(*ip));
	ip->type = type;
	switch (type) {
	case IP_NODE:
		ip->ip_node = va_arg(ap, NODE *);
		break;
	case IP_PROLOG:
	case IP_NEWBLK:
	case IP_EPILOG:
		ip->ip_regs = va_arg(ap, int);
		ip->ip_auto = va_arg(ap, int);
		ip->ip_retl = va_arg(ap, int);
		break;
	case IP_LOCCTR:
		ip->ip_locc = va_arg(ap, int);
		break;
	case IP_DEFLAB:
		ip->ip_lbl = va_arg(ap, int);
		break;
	case IP_INIT:
	case IP_DEFNAM:
		ip->ip_name = va_arg(ap, char *);
		ip->ip_vis = va_arg(ap, int);
		break;
	default:
		cerror("bad send_passt type %d", type);
	}
	va_end(ap);
	if (isinlining)
		inline_addarg(ip);
	else if (Oflag)
		topt_compile(ip);
	else
		pass2_compile(ip);
}
