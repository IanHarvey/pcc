#if 0
static char *sccsid ="@(#)order.c	1.20 (Berkeley) 5/31/88";
#endif

# include "pass2.h"

int canaddr(NODE *);

/*
 * should the assignment op p be stored,
 * given that it lies as the right operand of o
 * (or the left, if o==UNARY MUL)
 */
void
stoasg(NODE *p, int o)
{
	if (x2debug)
		printf("stoasg(%p, %o)\n", p, o);
}

/* should we delay the INCR or DECR operation p */
int
deltest(NODE *p)
{
	return 0;
}

/*
 * Check if p can be autoincremented.
 * XXX - nothing can be autoincremented for now.
 */
int
autoincr(NODE *p)
{
	return 0;
}

/*
 * Called from store().
 * If a subtree must be stored (running out of registers) setup correct
 * stotree/stocook here.
 */
void
mkadrs(NODE *p)
{
	register int o;

	if (x2debug)
		printf("mkadrs(%p)\n", p);

	o = p->n_op;

	if (asgop(o)) {
		if (p->n_left->n_su >= p->n_right->n_su){
			if (p->n_left->n_op == UNARY MUL) {
				SETSTO(p->n_left->n_left, INTEMP);
			} else if (p->n_left->n_op == FLD &&
			    p->n_left->n_left->n_op == UNARY MUL) {
				SETSTO(p->n_left->n_left->n_left, INTEMP);
			} else { /* should be only structure assignment */
				SETSTO(p->n_left, INTEMP);
			}
		} else
			SETSTO(p->n_right, INTEMP);
	} else {
		if (p->n_left->n_su > p->n_right->n_su) {
			SETSTO(p->n_left, INTEMP);
		} else {
			SETSTO(p->n_right, INTEMP);
		}
	}
}

/* is it legal to make an OREG or NAME entry which has an
 * offset of off, (from a register of r), if the
 * resulting thing had type t */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	return(0);  /* YES */
}

# define max(x,y) ((x)<(y)?(y):(x))

/*
 * set the su field in the node to the sethi-ullman
 * number, or local equivalent
 */
void
sucomp(NODE *p)
{
	int sul, sur, szr, o;

	/* Assume typesize regs from the beginning */
	o = p->n_op;
	p->n_su = szty(p->n_type);

	if (udebug)
		printf("enter sucomp(%p): o %s ty %d\n", p, opst[o], optype(o));

	switch (optype(o)) {
	case LTYPE:
		if (o == OREG && istreg(p->n_rval))
				p->n_su++;
		if (p->n_su == szty(p->n_type) &&
		    (p->n_op!=REG || !istreg(p->n_rval)) &&
		    (p->n_type==INT || p->n_type==UNSIGNED ||
		    p->n_type==DOUBLE || ISPTR(p->n_type) ||
		    ISARY(p->n_type)))
			p->n_su = 0;
		if (udebug)
			printf("sucomp(%p): LTYPE su %d\n", p, p->n_su);
		return;
	case UTYPE:
		switch (o) {
		case UNARY CALL:
		case UNARY STCALL:
			p->n_su = fregs;  /* all regs needed */
			return;
		}
		p->n_su =  p->n_left->n_su + (szty(p->n_type) > 1 ? 2 : 0);
		return;
	case BITYPE:
		break;
	default:
		cerror("sucomp error");
	}
	/*
	 * Use subtree computations.
	 */
	sul = p->n_left->n_su;
	sur = p->n_right->n_su;
	szr = szty(p->n_right->n_type);

	switch (o) {
	case ASSIGN:
		if (udebug)
			printf("sucomp(%p): ASSIGN\n", p);
		p->n_su = max(sur, sul+szr);
		if (udebug)
			printf("sucomp(%p): su %d\n", p, p->n_su);
		return;

	case INCR:
	case DECR:
	case MINUS:
	case MINUSEQ:
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
	case AND:
	case ANDEQ:
	case OR:
	case OREQ:
	case LS:
	case LSEQ:
	case RS:
	case RSEQ:
	case ER:
	case EREQ:
	case ASG MUL:
	case ASG PLUS:
	case MUL:
	case PLUS:
	case CM:
	case CBRANCH:
	case PMCONV:
	case PVCONV:
		if (udebug)
			printf("sucomp(%p): PLUS\n", p);
		p->n_su = max(sul, sur+szr);
		if (udebug)
			printf("sucomp(%p): su %d\n", p, p->n_su);
		return;
	case CALL:
	case STCALL:
		/* in effect, takes all free registers */
		p->n_su = fregs;
		return;

	case STASG:
		p->n_su = max( max( 1+sul, sur), fregs );
		return;

	case DIV:
	case ASG DIV:
	case MOD:
	case ASG MOD:
		/* DIV/MOD insns require register pairs */
		p->n_su = max(sul, sur) + 1;
		return;

	default:
		cerror("sucomp %d", o);
	}
}

int radebug = 0;

/* do register allocation */
void
rallo(NODE *p, int down)
{
	register int o, down1, down2, ty;

	if (radebug)
		printf("rallo(%p, %d)\n", p, down);

	down2 = NOPREF;
	p->n_rall = down;
	down1 = ( down &= ~MUSTDO );

	ty = optype( o = p->n_op );
	switch( o ) {
	case ASSIGN:	
		down1 = NOPREF;
		down2 = down;
		break;

	case CALL:
	case STASG:
	case EQ:
	case NE:
	case GT:
	case GE:
	case LT:
	case LE:
		down1 = NOPREF;
		break;

	case FORCE:	
		down1 = 1|MUSTDO; /* Return val in register 1 */
		break;

	}

	if (ty != LTYPE)
		rallo(p->n_left, down1);
	if (ty == BITYPE)
		rallo(p->n_right, down2);
}

void
offstar(NODE *p)
{
	if (x2debug)
		printf("offstar(%p)\n", p);

	if( p->n_op == PLUS || p->n_op == MINUS ){
		if( p->n_right->n_op == ICON ){
			p = p->n_left;
			order(p, INTAREG|INAREG);
			return;
		}
	}

	if (p->n_op == UNARY MUL && !canaddr(p)) {
		offstar(p->n_left);
		return;
	}

	order(p, INTAREG|INAREG);
}

/*
 * Rewrite increment/decrement operation.
 */
int
setincr(NODE *p)
{
	if (x2debug)
		printf("setincr(%p)\n", p);

	return(0);
}

/*
 * Rewrite operations on binary operators (like +, -, etc...).
 * Called as a result of table lookup.
 */
int
setbin(NODE *p)
{
	register int ro, rt;

	rt = p->n_right->n_type;
	ro = p->n_right->n_op;

	if (x2debug)
		printf("setbin(%p)\n", p);

	/*
	 * If right node is not addressable, but left is, ask the
	 * compiler to put the result in a register so that the
	 * value can safely be dealt with.
	 */
	if (canaddr(p->n_left) && !canaddr(p->n_right)) { /* address rhs */
		if (ro == UNARY MUL) {
			offstar(p->n_right->n_left);
		} else {
			order(p->n_right, INAREG|INTAREG|SOREG);
		}
		return 1;
	}
	/*
	 * If left hand side is not in a temporary register, put it
	 * there. It will be clobbered as a result of the operation.
	 */
	if (!istnode(p->n_left)) { /* try putting LHS into a reg */
		order(p->n_left, INAREG|INTAREG|INBREG|INTBREG|SOREG);
		return(1);
	} else if (ro == UNARY MUL && rt != CHAR && rt != UCHAR) {
		offstar(p->n_right->n_left);
		return(1);
	} else if (rt == CHAR || rt == UCHAR || rt == SHORT || rt == USHORT ||
#ifndef SPRECC
	    rt == FLOAT ||
#endif
	    (ro != REG && ro != NAME && ro != OREG && ro != ICON)) {
		order(p->n_right, INAREG|INBREG);
		return(1);
	}
	switch (p->n_op) {
	case LE:
	case LT:
	case GE:
	case GT:
		if (!istnode(p->n_right)) {
			order(p->n_right, INTAREG|INTBREG);
			return(1);
		}
		if (!istnode(p->n_left)) {
			order(p->n_left, INTAREG|INTBREG);
			return(1);
		}
		break;
	case EQ:
	case NE:
		if (!ISLONGLONG(p->n_right->n_type))
			break;
		if (p->n_right->n_op != ICON)
			break;
		order(p->n_right, INTAREG|SOREG);
		return 1;
	default:
		break;
	}
	return(0);
}

/* structure assignment */
int
setstr(NODE *p)
{
	if( p->n_right->n_op != REG ){
		order( p->n_right, INTAREG );
		return(1);
		}
	p = p->n_left;
	if( p->n_op != NAME && p->n_op != OREG ){
		if( p->n_op != UNARY MUL ) cerror( "bad setstr" );
		order( p->n_left, INTAREG );
		return( 1 );
		}
	return( 0 );
}

/* setup for assignment operator */
int
setasg(NODE *p)
{
	NODE *l = p->n_left, *r = p->n_right;

	if (x2debug)
		printf("setasg(%p)\n", p);

	if (p->n_op != ASSIGN)
		cerror("setasg != ASSIGN");

	/*
	 * If right node is not a value, force the compiler to put it
	 * in a register so that the value can safely be stored.
	 */
	if (!canaddr(r)) {
		if (r->n_op == UNARY MUL)
			offstar(r->n_left);
		else
			order(r, INAREG|INBREG);
		return(1);
	}

	/*
	 * If neither left nor right is in a register, force the right
	 * one to end up in one.
	 */
	if (l->n_op != REG && r->n_op != REG) {
		order(r, INTAREG|INTBREG);
		return(1);
	}
	if (l->n_op == UNARY MUL) {
		offstar(l->n_left);
		return(1);
	}
	return(0);
}
void hardops(NODE *p);

/* setup for =ops */
int
setasop(NODE *p)
{
	NODE *n;
	register int rt, ro, pt;

	if (x2debug)
		printf("setasop(%p)\n", p);

	rt = p->n_right->n_type;
	ro = p->n_right->n_op;

	/* For non-word pointers, ease for adjbp */
	pt = BTYPE(p->n_type);
	if ((p->n_type & TMASK) && (pt == SHORT || pt == USHORT ||
	    pt == UCHAR || pt == CHAR) && p->n_right->n_op != REG) {
		order(p->n_right, INAREG|INBREG);
		return(1);
	}

	if (ro == UNARY MUL && rt != CHAR) {
		offstar(p->n_right->n_left);
		return(1);
	}
	if (ISLONGLONG(rt)) {
		if (ISLONGLONG(p->n_type)) {
			if (ro == ICON)
				order(p->n_right, INAREG|INBREG);
			else
				hardops(p);
		} else { 
			/* Must insert a SCONV here */
			n = talloc();
			n->n_left = p->n_right;
			p->n_right = n;
			n->n_type = p->n_type;
			n->n_op = SCONV;
			n->n_rall = NOPREF;
			order(n, INTAREG|INTBREG);
		}
		return(1);
	}
	if (rt == CHAR || rt == SHORT || rt == UCHAR || rt == USHORT ||
#ifndef SPRECC
	    rt == FLOAT ||
#endif
	    (ro != REG && ro != ICON && ro != NAME && ro != OREG)) {
		order(p->n_right, INAREG|INBREG);
		return(1);
	}


	p = p->n_left;
	if (p->n_op == FLD)
		p = p->n_left;

	switch (p->n_op) {
	case REG:
	case ICON:
	case NAME:
	case OREG:
		return(0);

	case UNARY MUL:
		if (canaddr(p))
			return(0);
		offstar(p);
		return(1);

	}
	cerror("illegal setasop");
	/*NOTREACHED*/
	return 0;
}

void genargs(NODE *p);
void
genargs(NODE *p)
{
	NODE *pasg, *q;
	int align;
	int size;
	int count;

	/* generate code for the arguments */

	/*  first, do the arguments on the right */
	while (p->n_op == CM) {
		genargs(p->n_right);
		q = p->n_left;
		nfree(p);
		p = q;
	}

	if (p->n_op == STARG) { /* structure valued argument */

		size = p->n_stsize;
		align = p->n_stalign;
		if (p->n_left->n_op == ICON) {
			q = p->n_left;
			nfree(p);
			p = q;
		} else {
			/* make it look beautiful... */
			p->n_op = UNARY MUL;
			canon(p);  /* turn it into an oreg */
			for (count = 0; p->n_op != OREG && count<10; ++count){
				offstar(p->n_left);
				canon(p);
			}
			if (p->n_op != OREG)
				cerror( "stuck starg" );
		}

		pasg = talloc();
		pasg->n_op = STARG;
		pasg->n_rall = NOPREF;
		pasg->n_stsize = size;
		pasg->n_stalign = align;
		pasg->n_left = p;

 		order(pasg, FORARG);
		return;
	}

	/* ordinary case */
	order(p, FORARG);
}

int argsize(NODE *p);
int
argsize(NODE *p)
{
	int t = 0;

	if (p->n_op == CM) {
		t = argsize(p->n_left);
		p = p->n_right;
	}
	if (p->n_type == DOUBLE || p->n_type == FLOAT ||
	    p->n_type == LONGLONG || p->n_type == ULONGLONG) {
		SETOFF(t, 1);
		return (t + 2);
	} else if (p->n_op == STARG) {
 		SETOFF(t, 1);  /* alignment */
 		return(t + ((p->n_stsize+3)/4)*4);  /* size */
	} else {
		SETOFF(t, 1);
		return(t + 1);
	}
}
