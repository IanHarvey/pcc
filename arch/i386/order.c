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
	case ASG PLUS:
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

	case ASG MUL:
	case MUL:
	case DIV:
	case ASG DIV:
	case MOD:
	case ASG MOD:
		/* DIV/MOD/MUL insns require two registers */
		p->n_su = max(sul, sur) + 2;
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
	register int o, downl, downr, ty;

	if (radebug)
		printf("rallo(%p, %d)\n", p, down);

	downr = NOPREF;
	p->n_rall = down;
	downl = ( down &= ~MUSTDO );

	ty = optype( o = p->n_op );
	switch( o ) {
	case ASSIGN:	
		downl = NOPREF;
		downr = down;
		break;

	case CALL:
	case STASG:
	case EQ:
	case NE:
	case GT:
	case GE:
	case LT:
	case LE:
		downl = NOPREF;
		break;

	case DIV:
	case ASG DIV:
		downl = EAX|MUSTDO;
		downr = ECX|MUSTDO;
		break;

	case RS:
	case ASG RS:
	case LS:
	case ASG LS:
		downr = ECX|MUSTDO;
		break;

	case FORCE:	
		downl = EAX|MUSTDO; /* Return val in EAX */
		break;

	}

	if (ty != LTYPE)
		rallo(p->n_left, downl);
	if (ty == BITYPE)
		rallo(p->n_right, downr);
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
	int cl, cr, rv = 0;

	if (x2debug)
		printf("setbin(%p)\n", p);

	switch (p->n_op) {
	case RS:
	case LS:
		/* Be sure left node is addressable */
		if (!canaddr(p->n_left)) {
			order(p->n_left, INAREG|INTAREG|INTEMP);
			rv = 1;
		} else
		/* Right node must be either a constant or a register */
		if (p->n_right->n_op != REG && p->n_right->n_op != ICON) {
			order(p->n_right, INTAREG|INTBREG);
			rv = 1;
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
		cl = canaddr(p->n_left);
		cr = canaddr(p->n_right);

		if (cl && !cr) {
			order(p->n_right, INAREG|INTAREG);
			rv = 1;
		} else if (!cl && cr) {
			order(p->n_left, INAREG|INTAREG);
			rv = 1;
		} else if (!cl && !cr) {
			order(p->n_right, INAREG|INTAREG);
			order(p->n_left, INAREG|INTAREG|INTEMP);
			rv = 1;
		} else if (cl && cr && p->n_left->n_op != REG) {
			order(p->n_left, INAREG|INTAREG);
			rv = 1;
		}
		break;
	}

	if (x2debug)
		printf("after setbin(%p) rv=%d\n", p, rv);

	return rv;
#if 0
	register int ro, rt;

	rt = p->n_right->n_type;
	ro = p->n_right->n_op;

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
#endif
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
	int rv = 0;

	if (x2debug)
		printf("setasg(%p)\n", p);

	if (!canaddr(p->n_right)) {
		order(p->n_right, INTAREG);
		rv = 1;
	} else if (p->n_left->n_op == UNARY MUL) {
		offstar(p->n_left->n_left);
		rv = 1;
	} else if (p->n_right->n_op != REG && p->n_right->n_op != ICON) {
		order(p->n_right, INTAREG);
		rv = 1;
	}
	if (x2debug)
		printf("setasg(%p)ut, rv = %d\n", p, rv);

	return rv;
#if 0
	NODE *l = p->n_left, *r = p->n_right;

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
#endif
}
void hardops(NODE *p);

/* setup for =ops */
int
setasop(NODE *p)
{
	int rv = 0;
	NODE *r = p->n_right;

	if (x2debug)
		printf("setasop(%p)\n", p);

	switch (p->n_op) {
	case ASG PLUS:
	case ASG MINUS:
	case ASG MUL:
	case ASG OR:
	case ASG AND:
	case ASG ER:
		if (!canaddr(p->n_left)) {
			order(p->n_left, INAREG|INTAREG|INTEMP);
			rv = 1;
		} else if (!canaddr(p->n_right)) {
			order(p->n_right, INAREG|INTAREG|INTEMP);
			rv = 1;
		} else if (p->n_left->n_op != REG) {
			order(p->n_left, INAREG|INTAREG);
			rv = 1;
		}
		break;
	case ASG DIV:
		/*
		 * left must be in a tmpreg and right in an oreg 
		 * after this.
		 */
		if (p->n_left->n_op != REG || !istreg(p->n_left->n_rval)) {
			order(p->n_left, INAREG|INTAREG);
			rv = 1;
		} else if (p->n_right->n_op != NAME &&
		    p->n_right->n_op != OREG) {
			order(p->n_right, INTEMP);
			rv = 1;
		}
		break;
	case ASG RS:
	case ASG LS:
		if (!canaddr(p->n_left)) {
			order(p->n_left, INAREG|INTAREG);
			rv = 1;
		} else if (r->n_op != NAME && r->n_op != REG) {
			order(r, INAREG|INTAREG);
			rv = 1;
		}
		break;
	}
	if (x2debug)    
		printf("leave setasop(%p) rv=%d", p, rv);
	return rv;
#if 0
	NODE *n;
	register int rt, ro, pt;

	rt = p->n_right->n_type;
	ro = p->n_right->n_op;

	if (ro == UNARY MUL && rt != CHAR) {
		offstar(p->n_right->n_left);
		return(1);
	}
	if (ISLONGLONG(p->n_type)) {
		if (p->n_left->n_op != REG || !istreg(p->n_left->n_rval))
			return 0;
		order(p->n_right, INTEMP);
		return 1;
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
#endif
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
		SETOFF(t, 4);
		return (t + 8);
	} else if (p->n_op == STARG) {
 		SETOFF(t, 4);  /* alignment */
 		return(t + ((p->n_stsize+3)/4)*4);
	} else {
		SETOFF(t, 4);
		return(t + 4);
	}
}
