#if 0
static char *sccsid ="@(#)optim.c	4.7 (Berkeley) 1/8/86";
#endif

# include "pass1.h"

# define SWAP(p,q) {sp=p; p=q; q=sp;}
# define RCON(p) (p->n_right->n_op==ICON)
# define RO(p) p->n_right->n_op
# define RV(p) p->n_right->n_lval
# define LCON(p) (p->n_left->n_op==ICON)
# define LO(p) p->n_left->n_op
# define LV(p) p->n_left->n_lval

	/* is p a constant without a name */
# define nncon(p)	((p)->n_op == ICON && (p)->n_sp == NULL)

int oflag = 0;

/*
 * fortran function arguments
 */
NODE *
fortarg(NODE *p)
{
	if( p->n_op == CM ){
		p->n_left = fortarg( p->n_left );
		p->n_right = fortarg( p->n_right );
		return(p);
	}

	while( ISPTR(p->n_type) ){
		p = buildtree( UNARY MUL, p, NIL );
	}
	return( optim(p) );
}

	/* mapping relationals when the sides are reversed */
short revrel[] ={ EQ, NE, GE, GT, LE, LT, UGE, UGT, ULE, ULT };

/*
 * local optimizations, most of which are probably
 * machine independent
 */
NODE *
optim(NODE *p)
{
	int o, ty;
	NODE *sp;
	int i;
	TWORD t;

	if( (t=BTYPE(p->n_type))==ENUMTY || t==MOETY ) econvert(p);
	if( oflag ) return(p);
	ty = optype( o=p->n_op);
	if( ty == LTYPE ) return(p);

	if( ty == BITYPE ) p->n_right = optim(p->n_right);
	p->n_left = optim(p->n_left);

	/* collect constants */

	switch(o){

	case SCONV:
	case PCONV:
		return( clocal(p) );

	case FORTCALL:
		p->n_right = fortarg( p->n_right );
		break;

	case UNARY AND:
		if( LO(p) != NAME || !andable(p->n_left) ) return(p);

		LO(p) = ICON;

		setuleft:
		/* paint over the type of the left hand side with the type of the top */
		p->n_left->n_type = p->n_type;
		p->n_left->n_df = p->n_df;
		p->n_left->n_sue = p->n_sue;
		p->n_op = FREE;
		return( p->n_left );

	case UNARY MUL:
		if( LO(p) != ICON ) break;
		LO(p) = NAME;
		goto setuleft;

	case MINUS:
		if( !nncon(p->n_right) ) break;
		RV(p) = -RV(p);
		o = p->n_op = PLUS;

	case MUL:
	case PLUS:
	case AND:
	case OR:
	case ER:
		/* commutative ops; for now, just collect constants */
		/* someday, do it right */
		if( nncon(p->n_left) || ( LCON(p) && !RCON(p) ) ) SWAP( p->n_left, p->n_right );
		/* make ops tower to the left, not the right */
		if( RO(p) == o ){
			NODE *t1, *t2, *t3;
			t1 = p->n_left;
			sp = p->n_right;
			t2 = sp->n_left;
			t3 = sp->n_right;
			/* now, put together again */
			p->n_left = sp;
			sp->n_left = t1;
			sp->n_right = t2;
			p->n_right = t3;
			}
		if(o == PLUS && LO(p) == MINUS && RCON(p) && RCON(p->n_left) &&
		   conval(p->n_right, MINUS, p->n_left->n_right)){
			zapleft:
			RO(p->n_left) = FREE;
			LO(p) = FREE;
			p->n_left = p->n_left->n_left;
		}
		if( RCON(p) && LO(p)==o && RCON(p->n_left) &&
		    conval( p->n_right, o, p->n_left->n_right ) ){
			goto zapleft;
			}
		else if( LCON(p) && RCON(p) &&
			 conval( p->n_left, o, p->n_right ) ){
			zapright:
			RO(p) = FREE;
			p->n_left = makety(p->n_left, p->n_type,
			    p->n_df, p->n_sue);
			p->n_op = FREE;
			return( clocal( p->n_left ) );
			}
		/* FALL THROUGH */

	case ASG MUL:
		/* change muls to adds or shifts */

		if( (o == MUL || o == ASG MUL) &&
		    nncon(p->n_right) && (i=ispow2(RV(p)))>=0){
			if( i == 0 ) /* multiplication by 1 */
				goto zapright;
			/* c2 will turn 'i << 1' into 'i + i' for us */
			else {
				p->n_op = (asgop(o) ? ASG LS : LS);
				o = p->n_op;
				p->n_right->n_type = INT;
				p->n_right->n_sue = MKSUE(INT);
				RV(p) = i;
				}
			}

		/* change +'s of negative consts back to - */
		if( o==PLUS && nncon(p->n_right) && RV(p)<0 ){
			RV(p) = -RV(p);
			o = p->n_op = MINUS;
			}
		/* FALL THROUGH */
	case ASG AND:
	case ASG PLUS:
	case ASG MINUS:
	case RS:
	case LS:
		/* Operations with zero */
		if( nncon(p->n_right) && RV(p) == 0 ) {
			if( o == MUL || o == ASG MUL ||
			    o == AND || o == ASG AND ) {
				if( asgop(o) )
					p->n_op = ASSIGN;
				else if( optype(LO(p)) == LTYPE ) {
					p->n_op = FREE;
					LO(p) = FREE;
					p = p->n_right;
					}
				else
					p->n_op = COMOP; /* side effects */
				}
			else if( o == PLUS || o == MINUS ||
				 o == ASG PLUS || o == ASG MINUS ||
				 o == OR || o == ER ||
				 o == LS || o == RS )
				goto zapright;
			}
		if( o != LS && o != RS )
			break;
		/* FALL THROUGH */

	case ASG RS:
	case ASG LS:
		if( !ISUNSIGNED(p->n_left->n_type) )
			break;
		if( p->n_right->n_op == ICON &&
		    p->n_right->n_lval < 0 ) {
			/*
			 * Technically negative shifts are illegal
			 * but we can support them, at least with
			 * constants; you take potluck with variables.
			 */
			p->n_right->n_lval = -p->n_right->n_lval;
			switch( o ){
			case LS:	p->n_op = RS; break;
			case ASG LS:	p->n_op = ASG RS; break;
			case RS:	p->n_op = LS; break;
			case ASG RS:	p->n_op = ASG LS; break;
				}
			}
		break;

	case ASG DIV:
	case DIV:
		if( nncon( p->n_right ) ){
			if( RV(p) == 0 ) uerror("division by zero");
			else if( RV(p) == 1 ) goto zapright;
			/* Unsigned division by a power of two */
			else if( (i=ispow2(RV(p)))>=0 &&
				 (ISUNSIGNED(p->n_left->n_type) ||
				  ISUNSIGNED(p->n_right->n_type)) ){
				p->n_op = (asgop(o) ? ASG RS : RS);
				p->n_right->n_type = INT;
				p->n_right->n_sue = MKSUE(INT);
				RV(p) = i;
				}
			}
		break;

	case ASG MOD:
	case MOD:
		if( nncon(p->n_right) ){
			if( RV(p) == 0 ) uerror("modulus of zero");
			else if( RV(p) == 1 ){ /* mod by one gives zero */
				RV(p) = 0;
				if( asgop(o) )
					p->n_op = ASSIGN;
				else if( optype(LO(p)) == LTYPE ) {
					p->n_op = FREE;
					LO(p) = FREE;
					p = p->n_right;
					}
				else
					p->n_op = COMOP; /* side effects */
				}
			else if ((i=ispow2(RV(p)))>=0 &&
				 (ISUNSIGNED(p->n_left->n_type) ||
				  ISUNSIGNED(p->n_right->n_type)) ){
				/* Unsigned mod by a power of two */
				p->n_op = (asgop(o) ? ASG AND : AND);
				RV(p)--;
				}
			}
		break;

	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
	case ULT:
	case ULE:
	case UGT:
	case UGE:
		if( !LCON(p) ) break;

		/* exchange operands */

		sp = p->n_left;
		p->n_left = p->n_right;
		p->n_right = sp;
		p->n_op = revrel[p->n_op - EQ ];
		break;

		}

	return(p);
	}

int
ispow2(CONSZ c)
{
	int i;
	if( c <= 0 || (c&(c-1)) ) return(-1);
	for( i=0; c>1; ++i) c >>= 1;
	return(i);
}
