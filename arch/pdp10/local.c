#if 0
static char *sccsid ="@(#)local.c	1.17 (Berkeley) 5/11/88";
#endif

# include "pass1.h"
# include "pass2.h" /* XXX */

/*	this file contains code which is dependent on the target machine */

static int pointp(TWORD t);
static int newfun(char *name, TWORD type);

#define	PTRNORMAL	1
#define	PTRCHAR		2
#define	PTRSHORT	3
static int ptype(TWORD t);

NODE *
clocal(NODE *p)
{
	/* this is called to do local transformations on
	   an expression tree preparitory to its being
	   written out in intermediate code.
	*/

	/* the major essential job is rewriting the
	   automatic variables and arguments in terms of
	   REG and OREG nodes */
	/* conversion ops which are not necessary are also clobbered here */
	/* in addition, any special features (such as rewriting
	   exclusive or) are easily handled here as well */

	register struct symtab *q;
	register NODE *r, *l, *oop;
	register int o;
	register int m, ml;
	int val, siz;

	switch( o = p->in.op ){

	case NAME:
		if( p->tn.rval < 0 ) { /* already processed; ignore... */
			return(p);
			}
		q = &stab[p->tn.rval];
		switch( q->sclass ){

		case PARAM:
		case AUTO:
			/* fake up a structure reference */
			r = block(REG, NIL, NIL, PTR+STRTY, 0, 0);
			r->tn.lval = 0;
			r->tn.rval = STKREG;
			p = stref(block(STREF, r, p, 0, 0, 0));
			break;

		case STATIC:
			if( q->slevel == 0 ) break;
			p->tn.lval = 0;
			p->tn.rval = -q->offset;
			break;

		case REGISTER:
			p->in.op = REG;
			p->tn.lval = 0;
			p->tn.rval = q->offset;
			break;

			}
		break;

	case PCONV:
		l = p->in.left;
		/*
		 * Handle frame pointer directly without conversion,
		 * for efficiency.
		 */
		if (l->in.op == REG && l->tn.rval == STKREG) {
rmpc:			l->in.type = p->in.type;
			l->fn.cdim = p->fn.cdim;
			l->fn.csiz = p->fn.csiz;
			p->in.op = FREE;
			return l;
		}
		/* Convert ICON with name to new type */
		if (l->in.op == ICON && l->tn.rval != NONAME &&
		    l->in.type == INCREF(STRTY) && 
		    ttype(p->in.type, TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT)) {
			l->tn.lval *= (BTYPE(p->in.type) == CHAR ||
			    BTYPE(p->in.type) == UCHAR ? 4 : 2);
			goto rmpc;
		}
		/* Convert only address constants, never convert other */
		if (l->in.op == ICON) {
			if (l->tn.rval == NONAME)
				goto rmpc;
			if (p->in.type == INCREF(CHAR) ||
			    p->in.type == INCREF(UCHAR) ||
			    p->in.type == INCREF(UNDEF))
				l->tn.lval = (l->tn.lval & 07777777777) |
				    0700000000000;
			else if (p->in.type == INCREF(SHORT) ||
			    p->in.type == INCREF(USHORT))
				l->tn.lval = (l->tn.lval & 07777777777) |
				    0750000000000;
			else
				l->tn.lval = l->tn.lval & 07777777777;
			goto rmpc;
		}

		/* Remove more conversions of identical pointers */
		/* Be careful! optim() may do bad things */
		if (ISPTR(DECREF(p->in.type))) {
			if (ISPTR(DECREF(l->in.type))) {
				if ((optype(l->in.op) == UTYPE ||
				    optype(l->in.op) == BITYPE) &&
				    (l->in.left->in.op == REG))
					l->in.left->in.type = p->in.type;
				goto rmpc;
			}
		}

		/* Change PCONV from int to double pointer to right shift */
		if (ISPTR(p->in.type) && ISPTR(DECREF(p->in.type)) &&
		    (l->in.type == INT || l->in.type == UNSIGNED)) {
			p->in.op = RS;
			p->in.right = bcon(2);
			p->in.type = INT;
			break;
		}
		
		/* Check for cast integral -> pointer */
		if (BTYPE(l->in.type) == l->in.type)
			break;

		/* Remove conversions to identical pointers */
		switch (ptype(p->in.type)) {
		case PTRNORMAL:
			if (ptype(l->in.type) == PTRNORMAL)
				goto rmpc;
			break;

		case PTRSHORT:
			if (ptype(l->in.type) == PTRSHORT)
				goto rmpc;
			break;

		case PTRCHAR:
			if (ptype(l->in.type) == PTRCHAR)
				goto rmpc;
			break;
		}

		break;

	case SCONV:
		l = p->in.left;

		if ((p->in.type & TMASK) == 0 && (l->in.type & TMASK) == 0 &&
		    dimtab[BTYPE(p->in.type)] == dimtab[BTYPE(l->in.type)]) {
			p->in.op = FREE;
			return l;
		}
		/* cast to (void) XXX should be removed in MI code */
		if (p->in.type == UNDEF) {
			p->in.op = FREE;
			return l;
		}
		m = p->in.type;
		ml = l->in.type;
		if (m == ml) {
			p->in.op = FREE;
			return l;
		}
		o = l->in.op;
		if (ml == FLOAT || ml == DOUBLE) {
			if (o != FCON && o != DCON)
				break;
			ml = ISUNSIGNED(m) ? UNSIGNED : INT; /* LONG? */
			r = block(ICON, (NODE *)NULL, (NODE *)NULL, ml, 0, 0);
			if (o == FCON)
				r->tn.lval = ml == INT ?
					(int) p->in.left->fpn.fval :
					(unsigned) p->in.left->fpn.fval;
			else
				r->tn.lval = ml == INT ?
					(int) p->in.left->dpn.dval :
					(unsigned) p->in.left->dpn.dval;
			r->tn.rval = NONAME;
			p->in.left->in.op = FREE;
			p->in.left = r;
			o = ICON;
			if (m == ml) {
				p->in.op = FREE;
				return p->in.left;
			}
		}
		if (o == ICON) {
			CONSZ val = l->tn.lval;

			switch (m) {
			case CHAR:
				l->tn.lval = val & 0777;
				if (val & 0400)
					l->tn.lval |= ~((CONSZ)0777);
				break;
			case UCHAR:
				l->tn.lval = val & 0777;
				break;
			case USHORT:
				l->tn.lval = val & 0777777;
				break;
			case SHORT:
				l->tn.lval = val & 0777777;
				if (val & 0400000)
					l->tn.lval |= ~((CONSZ)0777777);
				break;
			case UNSIGNED:
				l->tn.lval = val & 0777777777777;
				break;
			case INT:
				l->tn.lval = val & 0777777777777;
				if (val & 0400000000000LL)
					l->tn.lval |= ~((CONSZ)0777777777777);
				break;
			case LONGLONG:	/* XXX */
			case ULONGLONG:
				l->tn.lval = val;
				break;
			case UNDEF:
				break;
			default:
				cerror("unknown type %d", m);
			}
			l->in.type = m;
			p->in.op = FREE;
			return l;
		}
		break;

	case PMCONV:
	case PVCONV:
                if( p->in.right->in.op != ICON ) cerror( "bad conversion", 0);
                p->in.op = FREE;
                return(buildtree(o==PMCONV?MUL:DIV, p->in.left, p->in.right));

	case RS:
	case ASG RS:
		/* convert >> to << with negative shift count */
		/* Beware! constant shifts will be converted back in optim() */

		if (p->in.right->in.op != UNARY MINUS) {
			p->in.right = buildtree(UNARY MINUS, p->in.right, NIL);
		} else {
			r = p->in.right;
			p->in.right = p->in.right->in.left;
			r->in.op = FREE;
		}
		if (p->in.op == RS)
			p->in.op = LS;
		else
			p->in.op = ASG LS;
		break;

	case ULT: /* exor sign bit to avoid unsigned comparitions */
	case ULE:
	case UGT:
	case UGE:
		r = block(ICON, NIL, NIL, INT, 0, INT);
		r->tn.lval = 0400000000000;
		r->tn.rval = NONAME;
		p->in.left = buildtree(ER, p->in.left, r);
		if (ISUNSIGNED(p->in.left->in.type))
			p->in.left->in.type = DEUNSIGN(p->in.left->in.type);

		r = block(ICON, NIL, NIL, INT, 0, INT);
		r->tn.lval = 0400000000000;
		r->tn.rval = NONAME;
		p->in.right = buildtree(ER, p->in.right, r);
		if (ISUNSIGNED(p->in.right->in.type))
			p->in.right->in.type = DEUNSIGN(p->in.right->in.type);

		p->in.op -= (ULT-LT);
		break;

	case UNARY MUL: /* Convert structure assignment to memcpy() */
		if (p->in.left->in.op != STASG)
			break;
		oop = p;
		p = p->in.left;
		siz = dimtab[p->fn.csiz]/SZCHAR;
		l = p->in.left;
		r = p->in.right;
		if (l->in.type == STRTY) {
			if (l->in.op == UNARY MUL) {
				p->in.left = l->in.left;
				l->in.op = FREE;
				l = p->in.left;
			} else {
				l = block(UNARY AND, l, NIL, INCREF(STRTY),
				    0, INT);
			}
		}
		if (l->in.type != INCREF(STRTY) || r->in.type != INCREF(STRTY))
			cerror("bad stasg, l = %o, r = %o", l->in.type, r->in.type);
		val = newfun("__structcpy", p->in.type);

		/* structure pointer block */
		l = block(CM, l, r, INT, 0, INT);
		/* Size block */
		r = block(CM, l, bcon(siz), INT, 0, INT);

		l = block(ICON, NIL, NIL, stab[val].stype, 0, INT);
		l->tn.rval = val;
		p->in.left = l;
		p->in.right = r;
		p->in.op = CALL;
		oop->in.left = p;
		return oop;
	}

	return(p);
}

int
newfun(char *name, TWORD type)
{
	struct symtab *sp;
	int val;

	val = lookup(name, 0);
	sp = &stab[val];
	if (sp->stype == UNDEF) {
		sp->stype = INCREF(type | FTN);
		sp->sclass = EXTERN;
		sp->slevel = 0;
		sp->snext = schain[0];
		schain[0] = sp;
	}
#ifdef notdef
	else if (!ISFTN(DECREF(sp->stype)))
		uerror("reserved name '%s' used illegally", name);
#endif
	return val;
}

/*ARGSUSED*/
int
andable(NODE *p)
{
	return(1);  /* all names can have & taken on them */
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
 * is an automatic variable of type t OK for a register variable
 * Everything is trusted to be in register here.
 */
int
cisreg(TWORD t)
{
	return(1);
}

int
ptype(TWORD t)
{
	int tt = BTYPE(t);
	int e, rv;

	if (!ISPTR(t))
		cerror("not a pointer");

	e = t & ~BTMASK;
	while (e) {
		rv = e;
		if (DECREF(e) == 0)
			break;
		e = DECREF(e);
	}
	if (ISFTN(rv))
		return PTRNORMAL;

	switch (tt) {
	case INT:
	case LONG:
	case LONGLONG:
	case FLOAT:
	case DOUBLE:
	case STRTY:
	case UNIONTY:
	case ENUMTY:
	case UNSIGNED:
	case ULONG:
	case ULONGLONG:
		return PTRNORMAL;
	case UNDEF:
	case CHAR:
	case UCHAR:
		if (DECREF(t) == tt || ISARY(rv))
			return PTRCHAR;
		return PTRNORMAL;
	case SHORT:
	case USHORT:
		if (DECREF(t) == tt || ISARY(rv))
			return PTRSHORT;
		return PTRNORMAL;
	default:
		break;
	}
	cerror("unknown type");
	return PTRNORMAL; /* XXX */
}

/*
 * Help routine to the one below; return true if it's not a word pointer.
 */
static int
pointp(TWORD t)
{
	int rv = 0;

	if (ISPTR(t) && ((t & TMASK1) == 0))
		return 1;

	t &= ~BTMASK;
	while (t) {
		rv = ISARY(t);
		t = DECREF(t);
	}
	return rv;
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
offcon(OFFSZ off, TWORD t, int d, int s)
{
	register NODE *p;

	if (xdebug)
		printf("offcon: OFFSZ %lld type %x dim %d siz %d\n",
		    off, t, dimtab[d], dimtab[s]);

	p = bcon(0);
	p->tn.lval = off/SZINT;	/* Default */
	if (ISPTR(DECREF(t)))
		return p;	/* Pointer/pointer reference */
	switch (BASETYPE & t) {
	case INT:
	case UNSIGNED:
	case LONG:
	case ULONG:
	case STRTY:
	case UNIONTY:
	case ENUMTY:
	case LONGLONG:
	case ULONGLONG:
		break;

	case SHORT:
	case USHORT:
		if (pointp(t))
			p->tn.lval = off/SZSHORT;
		break;

	case UNDEF: /* void pointers */
	case CHAR:
	case UCHAR:
		if (pointp(t))
			p->tn.lval = off/SZCHAR;
		break;

	default:
		cerror("offcon, off %llo size %d type %x", off, dimtab[s], t);
	}
	if (xdebug)
		printf("offcon return 0%llo\n", p->tn.lval);
	return(p);
}


static int inwd;	/* current bit offsed in word */
static CONSZ word;	/* word being built from fields */

/*
 * Generate initialization code for assigning a constant c
 * to a field of width sz
 * we assume that the proper alignment has been obtained
 * inoff is updated to have the proper final value
 * we also assume sz  < SZINT
 */
void
incode(NODE *p, int sz)
{
	inoff += sz;
	if (nerrors)
		return;
	if ((sz + inwd) > SZINT)
		cerror("incode: field > int");

	word |= ((p->tn.lval & ((1 << sz) - 1)) << (36 - inwd - sz));

	inwd += sz;
	if (inoff % SZINT == 0) {
		p1print("	.long 0%llo\n", word);
		word = inwd = 0;
	}
}

/* output code to initialize space of size sz to the value d */
/* the proper alignment has been obtained */
/* inoff is updated to have the proper final value */
/* on the target machine, write it out in octal! */
void
fincode(double d, int sz)
{
	cerror("fincode");
#if 0
	if(!nerrors)
		printf("	%s	0%c%.20e\n",
		    sz == SZDOUBLE ? ".double" : ".float",
		sz == SZDOUBLE ? 'd' : 'f', d);
	inoff += sz;
#endif
}

void
cinit(NODE *p, int sz)
{
	NODE *l;

	/*
	 * as a favor (?) to people who want to write
	 *     int i = 9600/134.5;
	 * we will, under the proper circumstances, do
	 * a coercion here.
	 */
	switch (p->in.type) {
	case INT:
	case UNSIGNED:
		l = p->in.left;
		if (l->in.op != SCONV ||
		    (l->in.left->tn.op != DCON && l->in.left->tn.op != FCON))
			break;
		l->in.op = FREE;
		l = l->in.left;
		l->tn.lval = l->tn.op == DCON ? (long)(l->dpn.dval) :
			(long)(l->fpn.fval);
		l->tn.rval = NONAME;
		l->tn.op = ICON;
		l->tn.type = INT;
		p->in.left = l;
		break;
	}
	/* arrange for the initialization of p into a space of size sz */
	/* the proper alignment has been opbtained */
	/* inoff is updated to have the proper final value */
	ecode( p );
	inoff += sz;
}

/*
 * define n bits of zeros in a vfd
 */
void
vfdzero(int n)
{
	if (n <= 0)
		return;
	inoff += n;
	if (nerrors)
		return;
	inwd += n;
	if (inoff%ALINT ==0) {
		p1print("	.long %llo\n", word);
		word = inwd = 0;
	}
}

/* make a name look like an external name in the local machine */
char *
exname(char *p)
{
	static char text[BUFSIZ+1];
	int i = 0;

	for(; *p; ++i)
		text[i] = *p++;

	text[i] = '\0';
	return (text);
}

/*
 * map types which are not defined on the local machine
 */
int
ctype(TWORD type)
{
	switch (BTYPE(type)) {
	case LONG:
		MODTYPE(type,INT);
		break;

	case ULONG:
		MODTYPE(type,UNSIGNED);
	}
	return (type);
}

/* curid is a variable which is defined but
 * is not initialized (and not a function );
 * This routine returns the stroage class for an uninitialized declaration
 */
int
noinit()
{
	return(EXTERN);
}

/* make a common declaration for id, if reasonable */
void
commdec(int id)
{
	struct symtab *q;
	int off;

	if (nerrors)
		return;
	q = &stab[id];
	off = tsize(q->stype, q->dimoff, q->sizoff);
	off = (off+(SZINT-1))/SZINT;
	p1print("\t.comm %s,0%o\n", exname(q->sname), off);
}
