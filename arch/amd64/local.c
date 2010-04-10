/*	$Id$	*/
/*
 * Copyright (c) 2008 Michael Shalayeff
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


#include "pass1.h"

/*	this file contains code which is dependent on the target machine */

/*
 * Check if a constant is too large for a type.
 */
static int
toolarge(TWORD t, CONSZ con)
{
	U_CONSZ ucon = con;

	switch (t) {
	case ULONG:
	case LONG:
	case ULONGLONG:
	case LONGLONG:
		break; /* cannot be too large */
#define	SCHK(i)	case i: if (con > MAX_##i || con < MIN_##i) return 1; break
#define	UCHK(i)	case i: if (ucon > MAX_##i) return 1; break
	SCHK(INT);
	SCHK(SHORT);
	case BOOL:
	SCHK(CHAR);
	UCHK(UNSIGNED);
	UCHK(USHORT);
	UCHK(UCHAR);
	default:
		cerror("toolarge");
	}
	return 0;
}

#define	IALLOC(sz)	(isinlining ? permalloc(sz) : tmpalloc(sz))

/*
 * Make a symtab entry for PIC use.
 */
static struct symtab *
picsymtab(char *p, char *s, char *s2)
{
	struct symtab *sp = IALLOC(sizeof(struct symtab));
	size_t len = strlen(p) + strlen(s) + strlen(s2) + 1;
	
	sp->sname = sp->soname = IALLOC(len);
	strlcpy(sp->soname, p, len);
	strlcat(sp->soname, s, len);
	strlcat(sp->soname, s2, len);
	sp->sclass = EXTERN;
	sp->sflags = sp->slevel = 0;
	return sp;
}

int gotnr; /* tempnum for GOT register */
int argstacksize;

/*
 * Create a reference for an extern variable.
 */
static NODE *
picext(NODE *p)
{
	NODE *q;
	struct symtab *sp;
	char *c;

	c = p->n_sp->soname ? p->n_sp->soname : p->n_sp->sname;
	sp = picsymtab("", c, "@GOTPCREL(%rip)");
	q = block(NAME, NIL, NIL, INCREF(p->n_type), p->n_df, p->n_sue);
	q->n_sp = sp;
	q = block(UMUL, q, 0, p->n_type, p->n_df, p->n_sue);
	q->n_sp = sp;
	nfree(p);
	return q;
}

/*
 * Create a reference for a static variable.
 */
static NODE *
picstatic(NODE *p)
{
	struct symtab *sp;
	char *c, buf[32];

	if (p->n_sp->slevel > 0)
		snprintf(c = buf, 32, LABFMT, (int)p->n_sp->soffset);
	else
		c = p->n_sp->soname ? p->n_sp->soname : p->n_sp->sname;
	sp = picsymtab("", c, "(%rip)");
	sp->sclass = STATIC;
	sp->stype = p->n_sp->stype;
	p->n_sp = sp;
	return p;
}

#ifdef TLS
/*
 * Create a reference for a TLS variable.
 */
static NODE *
tlspic(NODE *p)
{
	NODE *q, *r;
	struct symtab *sp, *sp2;

	/*
	 * creates:
	 *   leal var@TLSGD(%ebx),%eax
	 *   call ___tls_get_addr@PLT
	 */

	/* calc address of var@TLSGD */
	q = tempnode(gotnr, PTR|VOID, 0, MKSUE(VOID));
	sp = picsymtab("", p->n_sp->soname, "@TLSGD");
	r = xbcon(0, sp, INT);
	q = buildtree(PLUS, q, r);

	/* assign to %eax */
	r = block(REG, NIL, NIL, PTR|VOID, 0, MKSUE(VOID));
	r->n_rval = EAX;
	q = buildtree(ASSIGN, r, q);

	/* call ___tls_get_addr */
	sp2 = lookup("___tls_get_addr@PLT", 0);
	sp2->stype = EXTERN|INT|FTN;
	r = nametree(sp2);
	r = buildtree(ADDROF, r, NIL);
	r = block(UCALL, r, NIL, INT, 0, MKSUE(INT));

	/* fusion both parts together */
	q = buildtree(COMOP, q, r);
	q = block(UMUL, q, 0, p->n_type, p->n_df, p->n_sue);
	q->n_sp = p->n_sp; /* for init */

	nfree(p);
	return q;
}

static NODE *
tlsnonpic(NODE *p)
{
	NODE *q, *r;
	struct symtab *sp, *sp2;
	int ext = p->n_sp->sclass;

	sp = picsymtab("", p->n_sp->soname,
	    ext == EXTERN ? "@INDNTPOFF" : "@NTPOFF");
	q = xbcon(0, sp, INT);
	if (ext == EXTERN)
		q = block(UMUL, q, NIL, PTR|VOID, 0, MKSUE(VOID));

	sp2 = lookup("%gs:0", 0);
	sp2->stype = EXTERN|INT;
	r = nametree(sp2);

	q = buildtree(PLUS, q, r);
	q = block(UMUL, q, 0, p->n_type, p->n_df, p->n_sue);
	q->n_sp = p->n_sp; /* for init */

	nfree(p);
	return q;
}

static NODE *
tlsref(NODE *p)
{
	if (kflag)
		return (tlspic(p));
	else
		return (tlsnonpic(p));
}
#endif

/* clocal() is called to do local transformations on
 * an expression tree preparitory to its being
 * written out in intermediate code.
 *
 * the major essential job is rewriting the
 * automatic variables and arguments in terms of
 * REG and OREG nodes
 * conversion ops which are not necessary are also clobbered here
 * in addition, any special features (such as rewriting
 * exclusive or) are easily handled here as well
 */
NODE *
clocal(NODE *p)
{

	register struct symtab *q;
	register NODE *r, *l;
	register int o;
	register int m;
	TWORD t;

#ifdef PCC_DEBUG
	if (xdebug) {
		printf("clocal: %p\n", p);
		fwalk(p, eprint, 0);
	}
#endif
	switch( o = p->n_op ){

	case NAME:
		if ((q = p->n_sp) == NULL)
			return p; /* Nothing to care about */

		switch (q->sclass) {

		case PARAM:
		case AUTO:
			/* fake up a structure reference */
			r = block(REG, NIL, NIL, PTR+STRTY, 0, 0);
			r->n_lval = 0;
			r->n_rval = FPREG;
			p = stref(block(STREF, r, p, 0, 0, 0));
			break;

		case USTATIC:
			if (kflag == 0)
				break;
			/* FALLTHROUGH */
		case STATIC:
#ifdef TLS
			if (q->sflags & STLS) {
				p = tlsref(p);
				break;
			}
#endif
			if (kflag == 0) {
				if (q->slevel == 0)
					break;
				p->n_lval = 0;
			} else if (blevel > 0)
				p = picstatic(p);
			break;

		case REGISTER:
			p->n_op = REG;
			p->n_lval = 0;
			p->n_rval = q->soffset;
			break;

		case EXTERN:
		case EXTDEF:
#ifdef TLS
			if (q->sflags & STLS) {
				p = tlsref(p);
				break;
			}
#endif
			if (kflag == 0)
				break;
			if (blevel > 0)
				p = picext(p);
			break;
		}
		break;

	case ADDROF:
		if (kflag == 0 || blevel == 0)
			break;
		/* char arrays may end up here */
		l = p->n_left;
		if (l->n_op != NAME ||
		    (l->n_type != ARY+CHAR && l->n_type != ARY+WCHAR_TYPE))
			break;
		l = p;
		p = picstatic(p->n_left);
		nfree(l);
		if (p->n_op != UMUL)
			cerror("ADDROF error");
		l = p;
		p = p->n_left;
		nfree(l);
		break;

	case CALL:
	case STCALL:
		if (p->n_type == VOID)
			break; /* nothing to do */
		/* have the call at left of a COMOP to avoid arg trashing */
		r = tempnode(0, p->n_type, p->n_df, p->n_sue);
		m = regno(r);
		r = buildtree(ASSIGN, r, p);
		p = tempnode(m, r->n_type, r->n_df, r->n_sue);
		p = buildtree(COMOP, r, p);
		break;

	case UCALL:
	case USTCALL:
		/* For now, always clear eax */
		l = block(REG, NIL, NIL, INT, 0, MKSUE(INT));
		regno(l) = RAX;
		p->n_right = clocal(buildtree(ASSIGN, l, bcon(0)));
		p->n_op -= (UCALL-CALL);
		break;

	case CBRANCH:
		l = p->n_left;

		/*
		 * Remove unnecessary conversion ops.
		 */
		if (clogop(l->n_op) && l->n_left->n_op == SCONV) {
			if (coptype(l->n_op) != BITYPE)
				break;
			if (l->n_right->n_op == ICON) {
				r = l->n_left->n_left;
				if (r->n_type >= FLOAT && r->n_type <= LDOUBLE)
					break;
				if (ISPTR(r->n_type))
					break; /* no opt for pointers */
				if (toolarge(r->n_type, l->n_right->n_lval))
					break;
				/* Type must be correct */
				t = r->n_type;
				nfree(l->n_left);
				l->n_left = r;
				l->n_type = t;
				l->n_right->n_type = t;
			}
		}
		break;

	case PCONV:
		/* Remove redundant PCONV's. Be careful */
		l = p->n_left;
		if (l->n_op == ICON) {
			l->n_lval = (unsigned)l->n_lval;
			goto delp;
		}
		if (l->n_type < LONG) {
			/* float etc? */
			p->n_left = block(SCONV, l, NIL,
			    UNSIGNED, 0, MKSUE(UNSIGNED));
			break;
		}
		/* if left is SCONV, cannot remove */
		if (l->n_op == SCONV)
			break;

		/* avoid ADDROF TEMP */
		if (l->n_op == ADDROF && l->n_left->n_op == TEMP)
			break;

		/* if conversion to another pointer type, just remove */
		if (p->n_type > BTMASK && l->n_type > BTMASK)
			goto delp;
		break;

	delp:	l->n_type = p->n_type;
		l->n_qual = p->n_qual;
		l->n_df = p->n_df;
		l->n_sue = p->n_sue;
		nfree(p);
		p = l;
		break;
		
	case SCONV:
		l = p->n_left;

		/* Float conversions may need extra casts */
		if (p->n_type == FLOAT || p->n_type == DOUBLE) {
			if (l->n_type < INT) {
				p->n_left = block(SCONV, l, NIL,
				    ISUNSIGNED(l->n_type) ? UNSIGNED : INT,
				    l->n_df, l->n_sue);
				break;
			}
		}

		if (p->n_type == l->n_type) {
			nfree(p);
			return l;
		}

		if ((p->n_type & TMASK) == 0 && (l->n_type & TMASK) == 0 &&
		    btdims[p->n_type].suesize == btdims[l->n_type].suesize) {
			if (p->n_type != FLOAT && p->n_type != DOUBLE &&
			    l->n_type != FLOAT && l->n_type != DOUBLE &&
			    l->n_type != LDOUBLE && p->n_type != LDOUBLE) {
				if (l->n_op == NAME || l->n_op == UMUL ||
				    l->n_op == TEMP) {
					l->n_type = p->n_type;
					nfree(p);
					return l;
				}
			}
		}

		if (DEUNSIGN(p->n_type) == INT && DEUNSIGN(l->n_type) == INT &&
		    coptype(l->n_op) == BITYPE) {
			l->n_type = p->n_type;
			nfree(p);
			return l;
		}

		o = l->n_op;
		m = p->n_type;

		if (o == ICON) {
			CONSZ val = l->n_lval;

			if (!ISPTR(m)) /* Pointers don't need to be conv'd */
			    switch (m) {
			case BOOL:
				l->n_lval = l->n_lval != 0;
				break;
			case CHAR:
				l->n_lval = (char)val;
				break;
			case UCHAR:
				l->n_lval = val & 0377;
				break;
			case SHORT:
				l->n_lval = (short)val;
				break;
			case USHORT:
				l->n_lval = val & 0177777;
				break;
			case UNSIGNED:
				l->n_lval = val & 0xffffffff;
				break;
			case INT:
				l->n_lval = (int)val;
				break;
			case LONG:
			case LONGLONG:
				l->n_lval = (long long)val;
				break;
			case ULONG:
			case ULONGLONG:
				l->n_lval = val;
				break;
			case VOID:
				break;
			case LDOUBLE:
			case DOUBLE:
			case FLOAT:
				l->n_op = FCON;
				l->n_dcon = val;
				break;
			default:
				cerror("unknown type %d", m);
			}
			l->n_type = m;
			l->n_sue = MKSUE(m);
			nfree(p);
			return l;
		} else if (l->n_op == FCON) {
			l->n_lval = l->n_dcon;
			l->n_sp = NULL;
			l->n_op = ICON;
			l->n_type = m;
			l->n_sue = MKSUE(m);
			nfree(p);
			return clocal(l);
		}
		if (DEUNSIGN(p->n_type) == SHORT &&
		    DEUNSIGN(l->n_type) == SHORT) {
			nfree(p);
			p = l;
		}
		if ((p->n_type == CHAR || p->n_type == UCHAR ||
		    p->n_type == SHORT || p->n_type == USHORT) &&
		    (l->n_type == FLOAT || l->n_type == DOUBLE ||
		    l->n_type == LDOUBLE)) {
			p = block(SCONV, p, NIL, p->n_type, p->n_df, p->n_sue);
			p->n_left->n_type = INT;
			return p;
		}
		break;

	case MOD:
	case DIV:
		if (o == DIV && p->n_type != CHAR && p->n_type != SHORT)
			break;
		if (o == MOD && p->n_type != CHAR && p->n_type != SHORT)
			break;
		/* make it an int division by inserting conversions */
		p->n_left = block(SCONV, p->n_left, NIL, INT, 0, MKSUE(INT));
		p->n_right = block(SCONV, p->n_right, NIL, INT, 0, MKSUE(INT));
		p = block(SCONV, p, NIL, p->n_type, 0, MKSUE(p->n_type));
		p->n_left->n_type = INT;
		break;

	case PMCONV:
	case PVCONV:
		r = p;
		p = buildtree(o == PMCONV ? MUL : DIV, p->n_left, p->n_right);
		nfree(r);
		break;

	case FORCE:
		/* put return value in return reg */
		p->n_op = ASSIGN;
		p->n_right = p->n_left;
		p->n_left = block(REG, NIL, NIL, p->n_type, 0, MKSUE(INT));
		p->n_left->n_rval = p->n_left->n_type == BOOL ? 
		    RETREG(CHAR) : RETREG(p->n_type);
		break;

	case LS:
	case RS:
		/* shift count must be in a char
		 * unless longlong, where it must be int */
		if (p->n_type == LONGLONG || p->n_type == ULONGLONG) {
			if (p->n_right->n_type != INT)
				p->n_right = block(SCONV, p->n_right, NIL,
				    INT, 0, MKSUE(INT));
			break;
		}
		if (p->n_right->n_type == CHAR || p->n_right->n_type == UCHAR)
			break;
		p->n_right = block(SCONV, p->n_right, NIL,
		    CHAR, 0, MKSUE(CHAR));
		break;

	case STASG: /* Early conversion to memcpy */
		l = buildtree(ADDROF, p->n_left, NIL);
		r = p->n_right;
		o = tsize(p->n_type, p->n_df, p->n_sue)/SZCHAR;
#define  cmop(x,y) block(CM, x, y, INT, 0, MKSUE(INT))
		r = cmop(cmop(l, r), bcon(o));

		q = lookup(addname("memcpy"), 0);
		if (q->stype == UNDEF) {
			p->n_op = NAME;
			p->n_sp = q;
			p->n_type = FTN|INT;
			defid(p, EXTERN);
		}
		nfree(p);
		p = doacall(q, nametree(q), r);

		break;
	}
#ifdef PCC_DEBUG
	if (xdebug) {
		printf("clocal end: %p\n", p);
		fwalk(p, eprint, 0);
	}
#endif
	return(p);
}

/*
 * Change CALL references to either direct (static) or PLT.
 */
static void
fixnames(NODE *p, void *arg)
{
	struct symtab *sp;
	NODE *q;
	char *c;

	if ((cdope(p->n_op) & CALLFLG) == 0)
		return;
	if (p->n_left->n_op != UMUL || p->n_left->n_left->n_op != NAME)
		return;
	q = p->n_left->n_left;


	sp = q->n_sp;

	if (sp == NULL)
		return;	/* nothing to do */
	if (sp->sclass == STATIC && !ISFTN(sp->stype))
		return; /* function pointer */

	if (sp->sclass != STATIC && sp->sclass != EXTERN &&
	    sp->sclass != EXTDEF)
		cerror("fixnames");

	if ((c = strstr(sp->soname, "@GOT")) == NULL)
		cerror("fixnames2");
	if (sp->sclass == STATIC) {
		*c = 0;
	} else
		memcpy(c, "@PLT", sizeof("@PLT"));

	
	*p->n_left = *q;
	p->n_left->n_op = ICON;
	nfree(q);
}

void
myp2tree(NODE *p)
{
	struct symtab *sp;

	if (kflag)
		walkf(p, fixnames, 0); /* XXX walkf not needed */
	if (p->n_op != FCON)
		return;

#if 0
	/* put floating constants in memory */
	setloc1(RDATA);
	defalign(ALLDOUBLE);
	deflab1(i = getlab());
	ninval(0, btdims[p->n_type].suesize, p);
#endif

	sp = IALLOC(sizeof(struct symtab));
	sp->sclass = STATIC;
	sp->ssue = MKSUE(p->n_type);
	sp->slevel = 1; /* fake numeric label */
	sp->soffset = getlab();
	sp->sflags = 0;
	sp->stype = p->n_type;
	sp->squal = (CON >> TSHIFT);

	defloc(sp);
	ninval(0, sp->ssue->suesize, p);

	p->n_op = NAME;
	p->n_lval = 0;
	p->n_sp = sp;
}

/*ARGSUSED*/
int
andable(NODE *p)
{
	return(1);	/* all names can have & taken on them */
}

/*
 * at the end of the arguments of a ftn, set the automatic offset
 */
void
cendarg()
{
	autooff = AUTOINIT;
}

/*
 * Return 1 if a variable of type type is OK to put in register.
 */
int
cisreg(TWORD t)
{
	if (t == FLOAT || t == DOUBLE || t == LDOUBLE)
		return 0; /* not yet */
	return 1;
}

/*
 * return a node, for structure references, which is suitable for
 * being added to a pointer of type t, in order to be off bits offset
 * into a structure
 * t, d, and s are the type, dimension offset, and sizeoffset
 * For pdp10, return the type-specific index number which calculation
 * is based on its size. For example, short a[3] would return 3.
 * Be careful about only handling first-level pointers, the following
 * indirections must be fullword.
 */
NODE *
offcon(OFFSZ off, TWORD t, union dimfun *d, struct suedef *sue)
{
	register NODE *p;

	if (xdebug)
		printf("offcon: OFFSZ %lld type %x dim %p siz %d\n",
		    off, t, d, sue->suesize);

	p = bcon(0);
	p->n_lval = off/SZCHAR;	/* Default */
	return(p);
}

/*
 * Allocate off bits on the stack.  p is a tree that when evaluated
 * is the multiply count for off, t is a storeable node where to write
 * the allocated address.
 */
void
spalloc(NODE *t, NODE *p, OFFSZ off)
{
	NODE *sp;

cerror("spalloc");
	p = buildtree(MUL, p, bcon(off/SZCHAR)); /* XXX word alignment? */

	/* sub the size from sp */
	sp = block(REG, NIL, NIL, p->n_type, 0, MKSUE(INT));
	sp->n_lval = 0;
	sp->n_rval = STKREG;
	ecomp(buildtree(MINUSEQ, sp, p));

	/* save the address of sp */
	sp = block(REG, NIL, NIL, PTR+INT, t->n_df, t->n_sue);
	sp->n_lval = 0;
	sp->n_rval = STKREG;
	t->n_type = sp->n_type;
	ecomp(buildtree(ASSIGN, t, sp)); /* Emit! */

}

/*
 * Print out a string of characters.
 * Assume that the assembler understands C-style escape
 * sequences.
 */
void
instring(struct symtab *sp)
{
	char *s, *str = sp->sname;

	defloc(sp);

	/* be kind to assemblers and avoid long strings */
	printf("\t.ascii \"");
	for (s = str; *s != 0; ) {
		if (*s++ == '\\') {
			(void)esccon(&s);
		}
		if (s - str > 60) {
			fwrite(str, 1, s - str, stdout);
			printf("\"\n\t.ascii \"");
			str = s;
		}
	}
	fwrite(str, 1, s - str, stdout);
	printf("\\0\"\n");
}

static int inbits, inval;

/*
 * set fsz bits in sequence to zero.
 */
void
zbits(OFFSZ off, int fsz)
{
	int m;

	if (idebug)
		printf("zbits off %lld, fsz %d inbits %d\n", off, fsz, inbits);
	if ((m = (inbits % SZCHAR))) {
		m = SZCHAR - m;
		if (fsz < m) {
			inbits += fsz;
			return;
		} else {
			fsz -= m;
			printf("\t.byte %d\n", inval);
			inval = inbits = 0;
		}
	}
	if (fsz >= SZCHAR) {
		printf("\t.zero %d\n", fsz/SZCHAR);
		fsz -= (fsz/SZCHAR) * SZCHAR;
	}
	if (fsz) {
		inval = 0;
		inbits = fsz;
	}
}

/*
 * Initialize a bitfield.
 */
void
infld(CONSZ off, int fsz, CONSZ val)
{
	if (idebug)
		printf("infld off %lld, fsz %d, val %lld inbits %d\n",
		    off, fsz, val, inbits);
	val &= ((CONSZ)1 << fsz)-1;
	while (fsz + inbits >= SZCHAR) {
		inval |= (val << inbits);
		printf("\t.byte %d\n", inval & 255);
		fsz -= (SZCHAR - inbits);
		val >>= (SZCHAR - inbits);
		inval = inbits = 0;
	}
	if (fsz) {
		inval |= (val << inbits);
		inbits += fsz;
	}
}

/*
 * print out a constant node, may be associated with a label.
 * Do not free the node after use.
 * off is bit offset from the beginning of the aggregate
 * fsz is the number of bits this is referring to
 */
void
ninval(CONSZ off, int fsz, NODE *p)
{
	union { float f; double d; long double l; int i[3]; } u;
	struct symtab *q;
	char *c;
	TWORD t;

	t = p->n_type;
	if (t > BTMASK)
		t = LONG; /* pointer */

	while (p->n_op == SCONV || p->n_op == PCONV) {
		NODE *l = p->n_left;
		l->n_type = p->n_type;
		p = l;
	}

	if (kflag && (p->n_op == PLUS || p->n_op == UMUL)) {
		if (p->n_op == UMUL)
			p = p->n_left;
		p = p->n_right;
		q = p->n_sp;

		if ((c = strstr(q->soname, "@GOT")) != NULL)
			*c = 0; /* ignore GOT ref here */
	}
	if (p->n_op != ICON && p->n_op != FCON)
		cerror("ninval: init node not constant");

	if (p->n_op == ICON && p->n_sp != NULL && DEUNSIGN(t) != LONG)
		uerror("element not constant");

	switch (t) {
	case LONG:
	case ULONG:
		printf("\t.quad 0x%llx", p->n_lval);
		if ((q = p->n_sp) != NULL) {
			if ((q->sclass == STATIC && q->slevel > 0)) {
				printf("+" LABFMT, q->soffset);
			} else {
				char *name;
				if ((name = q->soname) == NULL)
					name = exname(q->sname);
				printf("+%s", name);
			}
		}
		printf("\n");
		break;
	case INT:
	case UNSIGNED:
		printf("\t.long 0x%x\n", (int)p->n_lval & 0xffffffff);
		break;
	case SHORT:
	case USHORT:
		printf("\t.short 0x%x\n", (int)p->n_lval & 0xffff);
		break;
	case BOOL:
		if (p->n_lval > 1)
			p->n_lval = p->n_lval != 0;
		/* FALLTHROUGH */
	case CHAR:
	case UCHAR:
		printf("\t.byte %d\n", (int)p->n_lval & 0xff);
		break;
	case LDOUBLE:
		u.i[2] = 0;
		u.l = (long double)p->n_dcon;
#if defined(HOST_BIG_ENDIAN)
		/* XXX probably broken on most hosts */
		printf("\t.long\t0x%x,0x%x,0x%x\n", u.i[2], u.i[1], u.i[0]);
#else
		printf("\t.long\t0x%x,0x%x,0x%x\n", u.i[0], u.i[1], u.i[2]);
#endif
		break;
	case DOUBLE:
		u.d = (double)p->n_dcon;
#if defined(HOST_BIG_ENDIAN)
		printf("\t.long\t0x%x,0x%x\n", u.i[1], u.i[0]);
#else
		printf("\t.long\t0x%x,0x%x\n", u.i[0], u.i[1]);
#endif
		break;
	case FLOAT:
		u.f = (float)p->n_dcon;
		printf("\t.long\t0x%x\n", u.i[0]);
		break;
	default:
		cerror("ninval");
	}
}

/* make a name look like an external name in the local machine */
char *
exname(char *p)
{
	return (p == NULL ? "" : p);
}

/*
 * map types which are not defined on the local machine
 */
TWORD
ctype(TWORD type)
{
	switch (BTYPE(type)) {
	case LONGLONG:
		MODTYPE(type,LONG);
		break;

	case ULONGLONG:
		MODTYPE(type,ULONG);

	}
	return (type);
}

void
calldec(NODE *p, NODE *q) 
{
}

void
extdec(struct symtab *q)
{
}

/* make a common declaration for id, if reasonable */
void
defzero(struct symtab *sp)
{
	int off;

#ifdef TLS
	if (sp->sflags & STLS) {
		if (sp->sclass == EXTERN)
			sp->sclass = EXTDEF;
		simpleinit(sp, bcon(0));
		return;
	}
#endif

	off = tsize(sp->stype, sp->sdf, sp->ssue);
	off = (off+(SZCHAR-1))/SZCHAR;
	printf("	.%scomm ", sp->sclass == STATIC ? "l" : "");
	if (sp->slevel == 0) {
		char *c = sp->soname ? sp->soname : exname(sp->sname);
		printf("%s,0%o\n", c, off);
	} else
		printf(LABFMT ",0%o\n", sp->soffset, off);
}

static char *
section2string(char *name, int len)
{
	char *s;
	int n;

	if (strncmp(name, "link_set", 8) == 0) {
		const char *postfix = ",\"aw\",@progbits";
		n = len + strlen(postfix) + 1;
		s = IALLOC(n);
		strlcpy(s, name, n);
		strlcat(s, postfix, n);
		return s;
	}

	return newstring(name, len);
}

char *nextsect;
#ifdef TLS
static int gottls;
#endif
static char *alias;
static int constructor;
static int destructor;

/*
 * Give target the opportunity of handling pragmas.
 */
int
mypragma(char **ary)
{
#ifdef TLS
	if (strcmp(ary[1], "tls") == 0 && ary[2] == NULL) {
		gottls = 1;
		return 1;
	}
#endif
	if (strcmp(ary[1], "constructor") == 0 || strcmp(ary[1], "init") == 0) {
		constructor = 1;
		return 1;
	}
	if (strcmp(ary[1], "destructor") == 0 || strcmp(ary[1], "fini") == 0) {
		destructor = 1;
		return 1;
	}
	if (strcmp(ary[1], "section") == 0 && ary[2] != NULL) {
		nextsect = section2string(ary[2], strlen(ary[2]));
		return 1;
	}
	if (strcmp(ary[1], "alias") == 0 && ary[2] != NULL) {
		alias = tmpstrdup(ary[2]);
		return 1;
	}

	return 0;
}

/*
 * Called when a identifier has been declared.
 */
void
fixdef(struct symtab *sp)
{
#ifdef TLS
	/* may have sanity checks here */
	if (gottls)
		sp->sflags |= STLS;
	gottls = 0;
#endif
	if (alias != NULL && (sp->sclass != PARAM)) {
		printf("\t.globl %s\n", exname(sp->soname));
		printf("%s = ", exname(sp->soname));
		printf("%s\n", exname(alias));
		alias = NULL;
	}
	if ((constructor || destructor) && (sp->sclass != PARAM)) {
		NODE *p = talloc();

		p->n_op = NAME;
		p->n_sp =
		  (struct symtab *)(constructor ? "constructor" : "destructor");
		sp->ssue = sueget(sp->ssue);
		sp->ssue->suega = gcc_attr_parse(p);
		constructor = destructor = 0;
	}
}

NODE *
i386_builtin_return_address(NODE *f, NODE *a)
{
	int nframes;

	if (a == NULL || a->n_op != ICON)
		goto bad;

	nframes = a->n_lval;

	tfree(f);
	tfree(a);

	f = block(REG, NIL, NIL, PTR+VOID, 0, MKSUE(VOID));
	regno(f) = FPREG;

	while (nframes--)
		f = block(UMUL, f, NIL, PTR+VOID, 0, MKSUE(VOID));

	f = block(PLUS, f, bcon(4), INCREF(PTR+VOID), 0, MKSUE(VOID));
	f = buildtree(UMUL, f, NIL);

	return f;
bad:
        uerror("bad argument to __builtin_return_address");
        return bcon(0);
}

NODE *
i386_builtin_frame_address(NODE *f, NODE *a)
{
	int nframes;

	if (a == NULL || a->n_op != ICON)
		goto bad;

	nframes = a->n_lval;

	tfree(f);
	tfree(a);

	f = block(REG, NIL, NIL, PTR+VOID, 0, MKSUE(VOID));
	regno(f) = FPREG;

	while (nframes--)
		f = block(UMUL, f, NIL, PTR+VOID, 0, MKSUE(VOID));

	return f;
bad:
        uerror("bad argument to __builtin_frame_address");
        return bcon(0);
}

void
pass1_lastchance(struct interpass *ip)
{
}
