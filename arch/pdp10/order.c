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
	TWORD ty = p->n_type;

	return ty == PTR+CHAR || ty == PTR+UCHAR ||
	    ty == PTR+SHORT || ty == PTR+USHORT;
}

/*
 * Check if p can be autoincremented.
 * Nothing can be autoincremented on PDP10.
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
	case OR:
	case LS:
	case RS:
	case ER:
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

	case MUL:
	case DIV:
	case MOD:
		/* DIV/MOD/MUL insns require register pairs */
		/* XXX - force longlongs out to temp for now. */
		if (ISLONGLONG(p->n_type))
			p->n_su = max(sul, sur) + 4;
		else
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
	NODE *q;

	if (x2debug)
		printf("offstar(%p)\n", p);

	if( p->n_op == PLUS || p->n_op == MINUS ){
		if( p->n_right->n_op == ICON ){
			q = p->n_left;
			if (q->n_op == PCONV && q->n_left->n_op == REG) {
				q->n_left->n_type = q->n_type;
				q->n_left->n_qual = q->n_qual;
				p->n_left = q->n_left;
				nfree(q);
			} else
				order(q, INTAREG|INAREG);
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
 * findops() failed, see if we can rewrite it to match.
 */
int
setbin(NODE *p)
{
	TWORD ty;
	NODE *r, *s;

	ty = p->n_type;
	switch (p->n_op) {
	case MINUS:
		switch (ty) {
		case PTR+CHAR:
		case PTR+UCHAR:
		case PTR+SHORT:
		case PTR+USHORT:
			/*
			 * Must negate the right side and change op to PLUS.
			 */
			r = p->n_right;
			if (r->n_op == ICON) {
				r->n_lval = -r->n_lval;
			} else {
				s = talloc();
				s->n_type = r->n_type;
				s->n_op = UNARY MINUS;
				s->n_left = r;
				p->n_right = s;
			}
			p->n_op = PLUS;
			return 1;
		}
	}
	return 0;
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
		if (r->n_op == UNARY MUL) {
			offstar(r->n_left);
		} else
			order(r, INAREG|INBREG);
		return(1);
	}

	if (l->n_op == UNARY MUL) {
		offstar(l->n_left);
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
	return(0);
}
void hardops(NODE *p);

void genargs(NODE *p);
void
genargs(NODE *p)
{
	extern int offarg;
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
		if (offarg)
			offarg += p->n_stsize/SZINT;
		return;
	}

	/* ordinary case */
	order(p, FORARG);
	if (offarg)
		offarg += szty(p->n_type);
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
 		return(t + p->n_stsize/SZINT);  /* size */
	} else {
		SETOFF(t, 1);
		return(t + 1);
	}
}

int
special(NODE *p, int shape)
{
	switch (shape) {
	case SUSHCON:
		if (p->n_op == ICON && p->n_name[0] == '\0' &&
		    (p->n_lval > 0 && p->n_lval <= 0777777))
			return 1;
		break;

	case SNSHCON:
		if (p->n_op == ICON && p->n_name[0] == '\0' &&
		    (p->n_lval < 0 && p->n_lval > -01000000))
			return 1;
		break;
	case SILDB:
		if (p->n_op == ASSIGN && p->n_left->n_op == REG &&
		    p->n_right->n_op == PLUS &&
		    p->n_right->n_left->n_op == REG &&
		    p->n_right->n_right->n_op == ICON && 
		    p->n_right->n_right->n_lval == 1 &&
		    p->n_right->n_left->n_rval == p->n_left->n_rval)
			return 1;
		break;
	}
	return 0;
}
