#if 0
static char *sccsid ="@(#)local.c	1.17 (Berkeley) 5/11/88";
#endif

# include "pass1.h"

/*	this file contains code which is dependent on the target machine */

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
	register NODE *r;
	register int o;
	register int m, ml;

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

		case ULABEL:
		case LABEL:
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

	case LT:
	case LE:
	case GT:
	case GE:
		if( ISPTR( p->in.left->in.type ) || ISPTR( p->in.right->in.type ) ){
			p->in.op += (ULT-LT);
			}
		break;

	case PCONV:
		/* do pointer conversions for char and longs */
		ml = p->in.left->in.type;
		if( ( ml==CHAR || ml==UCHAR || ml==SHORT || ml==USHORT ) && p->in.left->in.op != ICON ) break;

		/* pointers all have the same representation; the type is inherited */

		p->in.left->in.type = p->in.type;
		p->in.left->fn.cdim = p->fn.cdim;
		p->in.left->fn.csiz = p->fn.csiz;
		p->in.op = FREE;
		return( p->in.left );

	case SCONV:
		m = p->in.type;
		ml = p->in.left->in.type;
		if(m == ml)
			goto clobber;
		o = p->in.left->in.op;
		if(m == FLOAT || m == DOUBLE) {
			if(o==SCONV &&
			 ml == DOUBLE &&
			 p->in.left->in.left->in.type==m) {
				p->in.op = p->in.left->in.op = FREE;
				return(p->in.left->in.left);
				}
			/* see makety() for constant conversions */
			break;
			}
		if(ml == FLOAT || ml == DOUBLE){
			if(o != FCON && o != DCON)
				break;
			ml = ISUNSIGNED(m) ? UNSIGNED : INT; /* LONG? */
			r = block( ICON, (NODE *)NULL, (NODE *)NULL, ml, 0, 0 );
			if( o == FCON )
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
			if( m == ml )
				goto clobber;
			}
		/* now, look for conversions downwards */

		if( o == ICON ){ /* simulate the conversion here */
			CONSZ val;
			val = p->in.left->tn.lval;
			switch( m ){
			case CHAR:
				p->in.left->tn.lval = val & 0777;
				break;
			case UCHAR:
				p->in.left->tn.lval = val & 0777;
				break;
			case USHORT:
				p->in.left->tn.lval = val & 0777777;
				break;
			case SHORT:
				p->in.left->tn.lval = val & 0777777;
				break;
			case UNSIGNED:
				p->in.left->tn.lval = val & 0777777777777;
				break;
			case INT:
				p->in.left->tn.lval = val & 0777777777777;
				break;
			default:
				cerror("unknown type %d");
			}
			p->in.left->in.type = m;
		} else
			break;

	clobber:
		p->in.op = FREE;
		return( p->in.left );  /* conversion gets clobbered */

	case PVCONV:
	case PMCONV:
		if( p->in.right->in.op != ICON ) cerror( "bad conversion", 0);
		p->in.op = FREE;
		return( buildtree( o==PMCONV?MUL:DIV, p->in.left, p->in.right ) );

	case RS:
	case ASG RS:
		/* convert >> to << with negative shift count */
		/* only if type of left operand is not unsigned */

		if( ISUNSIGNED(p->in.left->in.type) ) break;
		if( p->in.right->in.op != UNARY MINUS )
			p->in.right = buildtree( UNARY MINUS, p->in.right, NIL );
		else {
			r = p->in.right;
			p->in.right = p->in.right->in.left;
			r->in.op = FREE;
		}
		if( p->in.op == RS ) p->in.op = LS;
		else p->in.op = ASG LS;
		break;

	case FLD:
		/* make sure that the second pass does not make the
		   descendant of a FLD operator into a doubly indexed OREG */

		if( p->in.left->in.op == UNARY MUL
				&& (r=p->in.left->in.left)->in.op == PCONV)
			if( r->in.left->in.op == PLUS || r->in.left->in.op == MINUS ) 
				if( ISPTR(r->in.type) ) {
					if( ISUNSIGNED(p->in.left->in.type) )
						p->in.left->in.type = UCHAR;
					else
						p->in.left->in.type = CHAR;
				}
		break;
		}

	return(p);
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

/* return a node, for structure references, which is suitable for
   being added to a pointer of type t, in order to be off bits offset
   into a structure */
NODE *
offcon(OFFSZ off, TWORD t, int d, int s)
{
	register NODE *p;

	/* t, d, and s are the type, dimension offset, and sizeoffset */
	/* in general they  are necessary for offcon, but not on VAX */

	p = bcon(0);
	p->tn.lval = off/SZINT;
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
		printf("	.long 0%llo\n", word);
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
cerror("vfdzero");
	inoff += n;
	if (nerrors)
		return;
	inwd += n;
	if (inoff%ALINT ==0) {
		printf("	.flong	%llo\n", word);
		word = inwd = 0;
	}
}

/* make a name look like an external name in the local machine */
char *
exname(char *p)
{
	static char text[BUFSIZ+1];
	int i;

	for( i=0; *p; ++i )
		text[i] = *p++;

	text[i] = '\0';
	return( text );
}

/*
 * map types which are not defined on the local machine
 */
int
ctype(TWORD type)
{
#if 0
	switch (BTYPE(type)) {
	case LONG:
		MODTYPE(type,INT);
		break;

	case ULONG:
		MODTYPE(type,UNSIGNED);
	}
#endif
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
	off = tsize(q->stype, q->dimoff, q->sizoff)/SZINT;
	if (off == 0)
		off = 1;
	printf("\t.comm %s,%o\n", exname(q->sname), off);
}

#if 0
void
prtdcon(p)
	register NODE *p;
{
	int o = p->in.op;
	int i;

	if (o != DCON && o != FCON)
		return;
	/*
	 * Clobber constants of value zero so
	 * we can generate more efficient code.
	 */
	if ((o == DCON && p->dpn.dval == 0) ||
	    (o == FCON && p->fpn.fval == 0)) {
		p->in.op = ICON;
		p->tn.rval = NONAME;
		return;
	}
	locctr(DATA);
	defalign(o == DCON ? ALDOUBLE : ALFLOAT);
	deflab(i = getlab());
	if (o == FCON)
		fincode(p->fpn.fval, SZFLOAT);
	else
		fincode(p->dpn.dval, SZDOUBLE);
	p->tn.lval = 0;
	p->tn.rval = -i;
	p->in.type = (o == DCON ? DOUBLE : FLOAT);
	p->in.op = NAME;
}
#endif

void
ecode(NODE *p)
{
	/* walk the tree and write out the nodes.. */

	if (nerrors)
		return;
	p2tree(p);
	p2compile(p);
}
