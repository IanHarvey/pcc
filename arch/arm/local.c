/*
 * We define location operations which operate on the expression tree
 * during the first pass (before sending to the backend for code generation.)
 */

#include <assert.h>

#include "pass1.h"

/*
 * clocal() is called to do local transformations on
 * an expression tree before being sent to the backend.
 */
NODE *
clocal(NODE *p)
{
	struct symtab *q;
	NODE *l, *r;
	int o;
	int ty;

	o = p->n_op;
	switch (o) {

#if 0
	case CALL:
                r = tempnode(0, p->n_type, p->n_df, p->n_sue);
                ecomp(buildtree(ASSIGN, r, p));
                return r;
#endif

	case NAME:
		if ((q = p->n_sp) == NULL)
			return p;

		switch (q->sclass) {
		case PARAM:
		case AUTO:
                        /* fake up a structure reference */
                        r = block(REG, NIL, NIL, PTR+STRTY, 0, 0);
                        r->n_lval = 0;
                        r->n_rval = FPREG;
                        p = stref(block(STREF, r, p, 0, 0, 0));
                        break;
                case REGISTER:
                        p->n_op = REG;
                        p->n_lval = 0;
                        p->n_rval = q->soffset;
                        break;
                case STATIC:
                        if (q->slevel > 0) {
                                p->n_lval = 0;
                                p->n_sp = q;
                        }
			break;
		default:
			ty = p->n_type;
			p = block(ADDROF, p, NIL, INCREF(ty), p->n_df, p->n_sue);
			p = block(UMUL, p, NIL, ty, p->n_df, p->n_sue);
			break;
		}
		break;

#if 0
	case STNAME:
		ty = p->n_type;
		p = block(ADDROF, p, NIL, INCREF(ty), p->n_df, p->n_sue);
		p = block(UMUL, p, NIL, ty, p->n_df, p->n_sue);
		break;
#endif

        case FORCE:
                /* put return value in return reg */
                p->n_op = ASSIGN;
                p->n_right = p->n_left;
                p->n_left = block(REG, NIL, NIL, p->n_type, 0, MKSUE(INT));
                p->n_left->n_rval = p->n_left->n_type == BOOL ? 
                    RETREG(BOOL_TYPE) : RETREG(p->n_type);
                break;

	case PMCONV:
	case PVCONV:
		nfree(p);
		return buildtree(o == PMCONV ? MUL : DIV, p->n_left, p->n_right);

	case SCONV:
		l = p->n_left;
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

#if 0 // table.c will handle these okay
               if (DEUNSIGN(p->n_type) == INT && DEUNSIGN(l->n_type) == INT &&
                    coptype(l->n_op) == BITYPE) {
                        l->n_type = p->n_type;
                        nfree(p);
                        return l;
                }
#endif

                if (l->n_op == ICON) {
                        CONSZ val = l->n_lval;

                        if (!ISPTR(p->n_type)) /* Pointers don't need to be conv'd */
                        switch (p->n_type) {
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
                        case ULONG:
                        case UNSIGNED:
                                l->n_lval = val & 0xffffffff;
                                break;
                        case ENUMTY:
                        case MOETY:
                        case LONG:
                        case INT:
                                l->n_lval = (int)val;
                                break;
                        case LONGLONG:
                                l->n_lval = (long long)val;
                                break;
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
                                cerror("unknown type %d", l->n_type);
                        }
			l->n_type = p->n_type;
			l->n_sue = MKSUE(p->n_type);
                        nfree(p);
                        return l;
                }
#if 0 // table.c will handle these okay
                if (DEUNSIGN(p->n_type) == SHORT &&
                    DEUNSIGN(l->n_type) == SHORT) {
                        nfree(p);
                        p = l;
                }
#endif
                if ((DEUNSIGN(p->n_type) == CHAR ||
                    DEUNSIGN(p->n_type) == SHORT) &&
                    (l->n_type == FLOAT || l->n_type == DOUBLE ||
                    l->n_type == LDOUBLE)) {
                        p = block(SCONV, p, NIL, p->n_type, p->n_df, p->n_sue);
                        p->n_left->n_type = INT;
                        return p;
                }
		break;

	case PCONV:
		l = p->n_left;
		if (l->n_op == ICON) {
			l->n_lval = (unsigned)l->n_lval;
			goto delp;
		}
		if (l->n_type < INT || DEUNSIGN(l->n_type) == LONGLONG) {
			p->n_left = block(SCONV, l, NIL,
			    UNSIGNED, 0, MKSUE(UNSIGNED));
			break;
		}
		if (l->n_op == SCONV)
			break;
		if (l->n_op == ADDROF && l->n_left->n_op == TEMP)
			goto delp;
		if (p->n_type > BTMASK && l->n_type > BTMASK)
			goto delp;
		break;

	delp:
		l->n_type = p->n_type;
		l->n_qual = p->n_qual;
		l->n_df = p->n_df;
		l->n_sue = p->n_sue;
		nfree(p);
		p = l;
		break;
	}

	return p;
}

/*
 * Called before sending the tree to the backend.
 */
void
myp2tree(NODE *p)
{
}

/*
 * Called during the first pass to determine if a NAME can be addressed.
 *
 * Return nonzero if supported, otherwise return 0.
 */
int
andable(NODE *p)
{
	if (ISFTN(p->n_type))
		return 1;
	return 0;
}

/*
 * Called just after function arguments are built.  Re-initialize the
 * offset of the arguments on the stack.
 * Is this necessary anymore?  bfcode() is called immediately after.
 */
void
cendarg()
{
	autooff = AUTOINIT;
}

/*
 * Return 1 if a variable of type 't' is OK to put in register.
 */
int
cisreg(TWORD t)
{
	if (t == FLOAT || t == DOUBLE || t == LDOUBLE)
		return 0; /* not yet */
	return 1;
}

/*
 * Used for generating pointer offsets into structures and arrays.
 *
 * For a pointer of type 't', generate an the offset 'off'.
 */
NODE *
offcon(OFFSZ off, TWORD t, union dimfun *d, struct suedef *sue)
{
	return bcon(off/SZCHAR);
}

/*
 * Allocate bits from the stack for dynamic-sized arrays.
 *
 * 'p' is the tree which represents the type being allocated.
 * 'off' is the number of 'p's to be allocated.
 * 't' is the storeable node where the address is written.
 */
void
spalloc(NODE *t, NODE *p, OFFSZ off)
{
	NODE *sp;

	p = buildtree(MUL, p, bcon(off/SZCHAR)); /* XXX word alignment? */

	/* sub the size from sp */
	sp = block(REG, NIL, NIL, p->n_type, 0, MKSUE(INT));
	sp->n_lval = 0;
	sp->n_rval = SP;
	ecomp(buildtree(MINUSEQ, sp, p));

	/* save the address of sp */
	sp = block(REG, NIL, NIL, PTR+INT, t->n_df, t->n_sue);
	sp->n_lval = 0;
	sp->n_rval = SP;
	t->n_type = sp->n_type;
	ecomp(buildtree(ASSIGN, t, sp));
}

static int inbits = 0, inval = 0;

/*
 * set 'fsz' bits in sequence to zero.
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
		printf("\t.space %d\n", fsz/SZCHAR);
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
	val &= (1 << fsz)-1;
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
 * Print an integer constant node, may be associated with a label.
 * Do not free the node after use.
 * 'off' is bit offset from the beginning of the aggregate
 * 'fsz' is the number of bits this is referring to
 */
void
ninval(CONSZ off, int fsz, NODE *p)
{
	union { float f; double d; long double l; int i[3]; } u;
	struct symtab *q;
	TWORD t;
	int i;

	t = p->n_type;
	if (t > BTMASK)
		t = INT; /* pointer */

	if (p->n_op != ICON && p->n_op != FCON)
		cerror("ninval: init node not constant");

	if (p->n_op == ICON && p->n_sp != NULL && DEUNSIGN(t) != INT)
		uerror("element not constant");

	switch (t) {
	case LONGLONG:
	case ULONGLONG:
		i = (p->n_lval >> 32);
		p->n_lval &= 0xffffffff;
		p->n_type = INT;
		ninval(off, 32, p);
		p->n_lval = i;
		ninval(off+32, 32, p);
		break;
	case INT:
	case UNSIGNED:
		printf("\t.word 0x%x", (int)p->n_lval);
		if ((q = p->n_sp) != NULL) {
			if ((q->sclass == STATIC && q->slevel > 0) ||
			    q->sclass == ILABEL) {
				printf("+" LABFMT, q->soffset);
			} else
				printf("+%s", exname(q->sname));
		}
		printf("\n");
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
		printf("\t.word\t0x%x,0x%x,0x%x\n", u.i[0], u.i[1], u.i[2]);
		break;
	case DOUBLE:
		u.d = (double)p->n_dcon;
		printf("\t.word\t0x%x,0x%x\n", u.i[0], u.i[1]);
		break;
	case FLOAT:
		u.f = (float)p->n_dcon;
		printf("\t.word\t0x%x\n", u.i[0]);
		break;
	default:
		cerror("ninval");
	}
}

/*
 * Prefix a leading underscore to a global variable (if necessary).
 */
char *
exname(char *p)
{
	return (p == NULL ? "" : p);
}

/*
 * Map types which are not defined on the local machine.
 */
TWORD
ctype(TWORD type)
{
	switch (BTYPE(type)) {
	case LONG:
		MODTYPE(type,INT);
		break;
	case ULONG:
		MODTYPE(type,UNSIGNED);
		break;
	}
	return (type);
}

/*
 * Before calling a function do any tree re-writing for the local machine.
 *
 * 'p' is the function tree (NAME)
 * 'q' is the CM-separated list of arguments.
 */
void
calldec(NODE *p, NODE *q) 
{
}

/*
 * While handling uninitialised variables, handle variables marked extern.
 */
void
extdec(struct symtab *q)
{
}

/*
 * make a common declaration for 'id', if reasonable
 */
void
commdec(struct symtab *q)
{
	int off;

	off = tsize(q->stype, q->sdf, q->ssue);
	off = (off+(SZCHAR-1))/SZCHAR;
#ifdef GCC_COMPAT
	printf("\t.comm %s,%d,%d\n", exname(gcc_findname(q)), off, off);
#else
	printf("\t.comm %s,%,%d\n", exname(q->sname), off, off);
#endif
}

/*
 * make a local common declaration for 'id', if reasonable
 */
void
lcommdec(struct symtab *q)
{
	int off;

	off = tsize(q->stype, q->sdf, q->ssue);
	off = (off+(SZCHAR-1))/SZCHAR;
	if (q->slevel == 0)
#ifdef GCC_COMPAT
		printf("\t.lcomm %s,%d\n", exname(gcc_findname(q)), off);
#else
		printf("\t.lcomm %s,%d\n", exname(q->sname), off);
#endif
	else
		printf("\t.lcomm " LABFMT ",%d\n", q->soffset, off);
}

/*
 * Print a (non-prog) label.
 */
void
deflab1(int label)
{
	printf(LABFMT ":\n", label);
}

static char *loctbl[] = { "text","data","section .rodata,","section .rodata" };

void
setloc1(int locc)
{
	if (locc == lastloc)
		return;
	lastloc = locc;
	printf("\t.%s\n", loctbl[locc]);
}
