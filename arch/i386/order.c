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

/* is it legal to make an OREG or NAME entry which has an
 * offset of off, (from a register of r), if the
 * resulting thing had type t */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	return(0);  /* YES */
}

/*
 * Turn a UMUL-referenced node into OREG.
 */
int
offstar(NODE *p)
{
	if (x2debug)
		printf("offstar(%p)\n", p);

	if( p->n_op == PLUS || p->n_op == MINUS ){
		if( p->n_right->n_op == ICON ){
			geninsn(p->n_left, INTAREG|INAREG);
			p->n_su = -1;
			return 1;
		}
	}
	geninsn(p, INTAREG|INAREG);
	return 0;
}

/*
 * Shape matches for UMUL.  Cooperates with offstar().
 */
int
shumul(NODE *p)
{

	if (x2debug)
		printf("shumul(%p)\n", p);

	/* Always turn it into OREG on x86 */
	return SOREG;
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

	if (x2debug)
		printf("setbin(%p)\n", p);
	return 0;

}

/* setup for assignment operator */
int
setasg(NODE *p, int cookie)
{
	if (x2debug)
		printf("setasg(%p)\n", p);
	return(0);
}

/* setup for unary operator */
int
setuni(NODE *p, int cookie)
{
	return 0;
}

/*
 * Special handling of some instruction register allocation.
 */
void
nspecial(struct optab *q, int *left, int *right, int *res, int *mask)
{
	switch (q->op) {
	case DIV:
	case MOD:
		*left = REGBIT(EAX);
		*right = 0;
		*res = q->op == DIV ? REGBIT(EAX) : REGBIT(EDX);
		*mask = REGBIT(EAX)|REGBIT(EDX);
		break;
	default:
		comperr("nspecial");
	}
}

/* register allocation */
regcode
regalloc(NODE *p, struct optab *q, int wantreg)
{
	regcode regc, regc2;

	if (q->op == SCONV && (q->rtype & TLONGLONG)) {
		/*
		 * Cast to longlong from char->short->int.
		 * cltd instruction; input in eax, out in eax+edx.
		 */
		if (regblk[EAX] & 1 || regblk[EDX] & 1)
			comperr("regalloc: needed regs inuse, node %p", p);
		regc = alloregs(p->n_left, EAX);
		if (REGNUM(regc) != EAX) {
			p->n_left = movenode(p->n_left, EAX);
			freeregs(regc);
		}
		MKREGC(regc, EAX, 2);
		regblk[EAX] |= 1;
		regblk[EDX] |= 1;
	} else if (q->op == MUL && (q->rtype & TCHAR)) {
		/*
		 * 8-bit mul, left in al, res in ax.
		 */
		if (regblk[EAX] & 1)
			comperr("regalloc: needed regs inuse, node %p", p);
		if (p->n_su & DORIGHT) {
			regc2 = alloregs(p->n_right, EDX);
			if (REGNUM(regc2) == EAX) {
				freeregs(regc2);
				p->n_right = movenode(p->n_right, EDX);
				regblk[EDX] |= 1;
			} else
				regblk[REGNUM(regc2)] |= 1;
		}
		regc = alloregs(p->n_left, EAX);
		if (REGNUM(regc) != EAX) {
			p->n_left = movenode(p->n_left, EAX);
			freeregs(regc);
			regblk[EAX] |= 1;
		}
		if ((p->n_su & DORIGHT) == 0)
			regc2 = alloregs(p->n_right, NOPREF);
		freeregs(regc2);
	} else if (q->op == DIV || q->op == MOD) {
		/*
		 * 32-bit div/mod.
		 */
		if (regblk[EAX] & 1 || regblk[EDX] & 1)
			comperr("regalloc: needed regs inuse, node %p", p);
		if (p->n_su & DORIGHT) {
			regc = alloregs(p->n_right, ECX);
			if (REGNUM(regc) != ECX) {
				p->n_right = movenode(p->n_right, ECX);
				if ((p->n_su & RMASK) == ROREG) {
					p->n_su &= ~RMASK;
					p->n_su |= RREG;
					p->n_right->n_su &= ~LMASK;
					p->n_right->n_su |= LOREG;
				}
				freeregs(regc);
				regblk[ECX] |= 1;
			}
		}
		regc = alloregs(p->n_left, EAX);
		if (REGNUM(regc) != EAX) {
			p->n_left = movenode(p->n_left, EAX);
			freeregs(regc);
			regblk[EAX] |= 1;
		}
		if ((p->n_su & RMASK) && !(p->n_su & DORIGHT)) {
			regc = alloregs(p->n_right, ECX);
			if (REGNUM(regc) != ECX) {
				p->n_right = movenode(p->n_right, ECX);
				if ((p->n_su & RMASK) == ROREG) {
					p->n_su &= ~RMASK;
					p->n_su |= RREG;
					p->n_right->n_su &= ~LMASK;
					p->n_right->n_su |= LOREG;
				}
			}
		}
		regblk[EAX] &= ~1;
		regblk[ECX] &= ~1;
		regblk[EDX] &= ~1;
		if (q->op == DIV) {
			MKREGC(regc, EAX, 1);
			regblk[EAX] |= 1;
		} else {
			MKREGC(regc, EDX, 1);
			regblk[EDX] |= 1;
		}
	} else if (q->op == LS || q->op == RS) {
		/*
		 * Shift instructions need shift count in cl.
		 */
		if (regblk[ECX] & 1)
			comperr("regalloc: cl inuse, node %p", p);
		if (p->n_su & DORIGHT) {
			regc = alloregs(p->n_right, ECX);
			if (REGNUM(regc) != ECX) {
				p->n_right = movenode(p->n_right, ECX);
				freeregs(regc);
				regblk[ECX] |= 1;
			}
		}
		if ((p->n_su & LMASK) == LREG) {
			regc = alloregs(p->n_left, NOPREF);
			if (REGNUM(regc) == ECX) {
				/* must move */
				regc = getregs(NOPREF, 1, 0);
				p->n_left = movenode(p->n_left, REGNUM(regc));
				regblk[ECX] &= ~1;
			}
		}
		if ((p->n_su & RMASK) == RREG && !(p->n_su & DORIGHT)) {
			regc2 = alloregs(p->n_right, ECX);
			if (REGNUM(regc2) != ECX) {
				p->n_right = movenode(p->n_right, ECX);
				freeregs(regc2);
			}
		}
		regblk[ECX] &= ~1;
	} else
		comperr("regalloc: bad optab");
	p->n_rall = REGNUM(regc);
	return regc;
}

/*
 * Splitup a function call and give away its arguments first.
 */
void
gencall(NODE *p, NODE *prev)
{
	NODE *n = 0; /* XXX gcc */
	static int storearg(NODE *);
	int o = p->n_op;
	int ty = optype(o);

	if (ty == LTYPE)
		return;

	switch (o) {
	case CALL:
		/* Normal call, just push args and be done with it */
		p->n_op = UCALL;
//printf("call\n");
		gencall(p->n_left, p);
		p->n_rval = storearg(p->n_right);
//printf("end call\n");
		break;

	case UFORTCALL:
	case FORTCALL:
		comperr("FORTCALL");

	case USTCALL:
	case STCALL:
		/*
		 * Structure return.  Look at the node above
		 * to decide about buffer address:
		 * - FUNARG, allocate space on stack, don't remove.
		 * - nothing, allocate space on stack and remove.
		 * - STASG, get the address of the left side as arg.
		 * - FORCE, this ends up in a return, get supplied addr.
		 * (this is not pretty, but what to do?)
		 */
		if (prev == NULL || prev->n_op == FUNARG) {
			/* Create nodes to generate stack space */
			n = mkbinode(ASSIGN, mklnode(REG, 0, STKREG, INT),
			    mkbinode(MINUS, mklnode(REG, 0, STKREG, INT),
			    mklnode(ICON, p->n_stsize, 0, INT), INT), INT);
//printf("stsize %d\n", p->n_stsize);
			pass2_compile(ipnode(n));
		} else if (prev->n_op == STASG) {
			n = prev->n_left;
			if (n->n_op == UMUL)
				n = nfree(n);
			else if (n->n_op == NAME) {
				n->n_op = ICON; /* Constant reference */
				n->n_type = INCREF(n->n_type);
			} else
				comperr("gencall stasg");
		} else if (prev->n_op == FORCE) {
			; /* do nothing here */
		} else {
			comperr("gencall bad op %d", prev->n_op);
		}

		/* Deal with standard arguments */
		gencall(p->n_left, p);
		if (o == STCALL) {
			p->n_op = USTCALL;
			p->n_rval = storearg(p->n_right);
		} else
			p->n_rval = 0;
		/* push return struct address */
		if (prev == NULL || prev->n_op == FUNARG) {
			n = mklnode(REG, 0, STKREG, INT);
			if (p->n_rval)
				n = mkbinode(PLUS, n,
				    mklnode(ICON, p->n_rval, 0, INT), INT);
			pass2_compile(ipnode(mkunode(FUNARG, n, 0, INT)));
			if (prev == NULL)
				p->n_rval += p->n_stsize/4;
		} else if (prev->n_op == FORCE) {
			/* return value for this function */
			n = mklnode(OREG, 8, FPREG, INT);
			pass2_compile(ipnode(mkunode(FUNARG, n, 0, INT)));
			p->n_rval++;
		} else {
			pass2_compile(ipnode(mkunode(FUNARG, n, 0, INT)));
			n = p;
			*prev = *p;
			nfree(n);
		}
//printf("end stcall\n");
		break;

	default:
		if (ty != UTYPE)
			gencall(p->n_right, p);
		gencall(p->n_left, p);
		break;
	}
}

/*
 * Create separate node trees for function arguments.
 */
static int
storearg(NODE *p)
{
	static void storecall(NODE *);
	struct interpass *ip;
	NODE *np;
	int tsz;
	extern int thisline;

#if 0
	np = (p->n_op == CM ? p->n_right : p);
	gencall(np);
#endif

	ip = tmpalloc(sizeof(struct interpass));
	ip->type = IP_NODE;
	ip->lineno = thisline;

	if (p->n_op == CM) {
		np = p->n_left;
		if (p->n_right->n_op == STARG) {
			NODE *op = p;
			p = p->n_right;
			nfree(op);
			tsz = (p->n_stsize+3)/4;
		} else {
			p->n_type = p->n_right->n_type;
			p->n_left = p->n_right;
			tsz = szty(p->n_type);
		}
		p->n_op = FUNARG;
		ip->ip_node = p;
		pass2_compile(ip);
		return storearg(np) + tsz;
	} else {
		if (p->n_op != STARG) {
			np = talloc();
			np->n_type = p->n_type;
			np->n_op = FUNARG;
			np->n_left = p;
			p = np;
			tsz = szty(p->n_type);
		} else {
			p->n_op = FUNARG;
			tsz = (p->n_stsize+3)/4;
		}
		ip->ip_node = p;
		pass2_compile(ip);
		return tsz;
	}
}

/*
 * Tell if a register can hold a specific datatype.
 */
int
mayuse(int reg, TWORD type)
{
	if (reg == EAX || reg == EBX || reg == ECX || reg == EDX)
		return 1; /* May hold any type */
	if ((reg == EDI || reg == ESI) && (type >= CHAR && type <= USHORT))
		return 0; /* May not hold char/short */
	return 1;  /* Everything else is OK */
}
