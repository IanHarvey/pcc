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

	o = p->in.op;

	if (asgop(o)) {
		if (p->in.left->in.su >= p->in.right->in.su){
			if (p->in.left->in.op == UNARY MUL) {
				SETSTO(p->in.left->in.left, INTEMP);
			} else if (p->in.left->in.op == FLD &&
			    p->in.left->in.left->in.op == UNARY MUL) {
				SETSTO(p->in.left->in.left->in.left, INTEMP);
			} else { /* should be only structure assignment */
				SETSTO(p->in.left, INTEMP);
			}
		} else
			SETSTO(p->in.right, INTEMP);
	} else {
		if (p->in.left->in.su > p->in.right->in.su) {
			SETSTO(p->in.left, INTEMP);
		} else {
			SETSTO(p->in.right, INTEMP);
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
	o = p->in.op;
	p->in.su = szty(p->in.type);

	if (udebug)
		printf("enter sucomp(%p): o %s ty %d\n", p, opst[o], optype(o));

	switch (optype(o)) {
	case LTYPE:
		if (o == OREG && istreg(p->tn.rval))
				p->in.su++;
		if (p->in.su == szty(p->in.type) &&
		    (p->in.op!=REG || !istreg(p->tn.rval)) &&
		    (p->in.type==INT || p->in.type==UNSIGNED ||
		    p->in.type==DOUBLE || ISPTR(p->in.type) ||
		    ISARY(p->in.type)))
			p->in.su = 0;
		if (udebug)
			printf("sucomp(%p): LTYPE su %d\n", p, p->in.su);
		return;
	case UTYPE:
		switch (o) {
		case UNARY CALL:
		case UNARY STCALL:
			p->in.su = fregs;  /* all regs needed */
			return;
		}
		p->in.su =  p->in.left->in.su + (szty(p->in.type) > 1 ? 2 : 0);
		return;
	case BITYPE:
		break;
	default:
		cerror("sucomp error");
	}
	/*
	 * Use subtree computations.
	 */
	sul = p->in.left->in.su;
	sur = p->in.right->in.su;
	szr = szty(p->in.right->in.type);

	switch (o) {
	case ASSIGN:
		if (udebug)
			printf("sucomp(%p): ASSIGN\n", p);
		p->in.su = max(sur, sul+szr);
		if (udebug)
			printf("sucomp(%p): su %d\n", p, p->in.su);
		return;

	case INCR:
	case DECR:
	case COMOP:
	case COLON:
	case QUEST:
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
	case ANDAND:
	case OROR:
	case PMCONV:
	case PVCONV:
		if (udebug)
			printf("sucomp(%p): PLUS\n", p);
		p->in.su = max(sul, sur+szr);
		if (udebug)
			printf("sucomp(%p): su %d\n", p, p->in.su);
		return;
	case CALL:
	case STCALL:
		/* in effect, takes all free registers */
		p->in.su = fregs;
		return;

	case STASG:
		p->in.su = max( max( 1+sul, sur), fregs );
		return;

	case DIV:
	case ASG DIV:
	case MOD:
	case ASG MOD:
		/* DIV/MOD insns require register pairs */
		p->in.su = max(sul, sur) + 1;
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
	p->in.rall = down;
	down1 = ( down &= ~MUSTDO );

	ty = optype( o = p->in.op );
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
	case NOT:
	case ANDAND:
	case OROR:
		down1 = NOPREF;
		break;

	case FORCE:	
		down1 = 1|MUSTDO; /* Return val in register 1 */
		break;

	}

	if (ty != LTYPE)
		rallo(p->in.left, down1);
	if (ty == BITYPE)
		rallo(p->in.right, down2);
}

void
offstar(NODE *p)
{
	if (x2debug)
		printf("offstar(%p)\n", p);

	if( p->in.op == PLUS || p->in.op == MINUS ){
		if( p->in.right->in.op == ICON ){
			p = p->in.left;
			order(p, INTAREG|INAREG);
			return;
		}
	}

	if (p->in.op == UNARY MUL && !canaddr(p)) {
		offstar(p->in.left);
		return;
	}

	order(p, INTAREG|INAREG);
}

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

	rt = p->in.right->in.type;
	ro = p->in.right->in.op;

	if (x2debug)
		printf("setbin(%p)\n", p);

	/*
	 * If right node is not addressable, but left is, ask the
	 * compiler to put the result in a register so that the
	 * value can safely be dealt with.
	 */
	if (canaddr(p->in.left) && !canaddr(p->in.right)) { /* address rhs */
		if (ro == UNARY MUL) {
			offstar(p->in.right->in.left);
		} else {
			order(p->in.right, INAREG|INTAREG|SOREG);
		}
		return 1;
	}
	/*
	 * If left hand side is not in a temporary register, put it
	 * there. It will be clobbered as a result of the operation.
	 */
	if (!istnode(p->in.left)) { /* try putting LHS into a reg */
		order(p->in.left, INAREG|INTAREG|INBREG|INTBREG|SOREG);
		return(1);
	} else if (ro == UNARY MUL && rt != CHAR && rt != UCHAR) {
		offstar(p->in.right->in.left);
		return(1);
	} else if (rt == CHAR || rt == UCHAR || rt == SHORT || rt == USHORT ||
#ifndef SPRECC
	    rt == FLOAT ||
#endif
	    (ro != REG && ro != NAME && ro != OREG && ro != ICON)) {
		order(p->in.right, INAREG|INBREG);
		return(1);
	}
	switch (p->in.op) {
	case LE:
	case LT:
	case GE:
	case GT:
		if (!istnode(p->in.right)) {
			order(p->in.right, INTAREG|INTBREG);
			return(1);
		}
		if (!istnode(p->in.left)) {
			order(p->in.left, INTAREG|INTBREG);
			return(1);
		}
		break;
	case EQ:
	case NE:
		if (!ISLONGLONG(p->in.right->in.type))
			break;
		if (p->in.right->in.op != ICON)
			break;
		order(p->in.right, INTAREG|SOREG);
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
	if( p->in.right->in.op != REG ){
		order( p->in.right, INTAREG );
		return(1);
		}
	p = p->in.left;
	if( p->in.op != NAME && p->in.op != OREG ){
		if( p->in.op != UNARY MUL ) cerror( "bad setstr" );
		order( p->in.left, INTAREG );
		return( 1 );
		}
	return( 0 );
}

/* setup for assignment operator */
int
setasg(NODE *p)
{
	NODE *l = p->in.left, *r = p->in.right;

	if (x2debug)
		printf("setasg(%p)\n", p);

	if (p->in.op != ASSIGN)
		cerror("setasg != ASSIGN");

	/*
	 * If right node is not a value, force the compiler to put it
	 * in a register so that the value can safely be stored.
	 */
	if (!canaddr(r)) {
		if (r->in.op == UNARY MUL)
			offstar(r->in.left);
		else
			order(r, INAREG|INBREG);
		return(1);
	}

	/*
	 * If neither left nor right is in a register, force the right
	 * one to end up in one.
	 */
	if (l->in.op != REG && r->in.op != REG) {
		order(r, INTAREG|INTBREG);
		return(1);
	}
	if (l->in.op == UNARY MUL) {
		offstar(l->in.left);
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

	rt = p->in.right->in.type;
	ro = p->in.right->in.op;

	/* For non-word pointers, ease for adjbp */
	pt = BTYPE(p->in.type);
	if ((p->in.type & TMASK) && (pt == SHORT || pt == USHORT ||
	    pt == UCHAR || pt == CHAR) && p->in.right->in.op != REG) {
		order(p->in.right, INAREG|INBREG);
		return(1);
	}

	if (ro == UNARY MUL && rt != CHAR) {
		offstar(p->in.right->in.left);
		return(1);
	}
	if (ISLONGLONG(rt)) {
		if (ISLONGLONG(p->in.type)) {
			if (ro == ICON)
				order(p->in.right, INAREG|INBREG);
			else
				hardops(p);
		} else { 
			/* Must insert a SCONV here */
			n = talloc();
			n->in.left = p->in.right;
			p->in.right = n;
			n->in.type = p->in.type;
			n->in.op = SCONV;
			n->in.rall = NOPREF;
			order(n, INTAREG|INTBREG);
		}
		return(1);
	}
	if (rt == CHAR || rt == SHORT || rt == UCHAR || rt == USHORT ||
#ifndef SPRECC
	    rt == FLOAT ||
#endif
	    (ro != REG && ro != ICON && ro != NAME && ro != OREG)) {
		order(p->in.right, INAREG|INBREG);
		return(1);
	}


	p = p->in.left;
	if (p->in.op == FLD)
		p = p->in.left;

	switch (p->in.op) {
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
	NODE *pasg;
	int align;
	int size;
	int count;

	/* generate code for the arguments */

	/*  first, do the arguments on the right */
	while (p->in.op == CM) {
		genargs(p->in.right);
		p->in.op = FREE;
		p = p->in.left;
	}

	if (p->in.op == STARG) { /* structure valued argument */

		size = p->stn.stsize;
		align = p->stn.stalign;
		if (p->in.left->in.op == ICON) {
			p->in.op = FREE;
			p = p->in.left;
		} else {
			/* make it look beautiful... */
			p->in.op = UNARY MUL;
			canon(p);  /* turn it into an oreg */
			for (count = 0; p->in.op != OREG && count<10; ++count){
				offstar(p->in.left);
				canon(p);
			}
			if (p->in.op != OREG)
				cerror( "stuck starg" );
		}

		pasg = talloc();
		pasg->in.op = STARG;
		pasg->in.rall = NOPREF;
		pasg->stn.stsize = size;
		pasg->stn.stalign = align;
		pasg->in.left = p;

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

	if (p->in.op == CM) {
		t = argsize(p->in.left);
		p = p->in.right;
	}
	if (p->in.type == DOUBLE || p->in.type == FLOAT ||
	    p->in.type == LONGLONG || p->in.type == ULONGLONG) {
		SETOFF(t, 1);
		return (t + 2);
	} else if (p->in.op == STARG) {
 		SETOFF(t, 1);  /* alignment */
 		return(t + ((p->stn.stsize+3)/4)*4);  /* size */
	} else {
		SETOFF(t, 1);
		return(t + 1);
	}
}
