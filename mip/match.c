#if 0
static char *sccsid ="@(#)match.c	4.7 (Berkeley) 12/10/87";
#endif

# include "pass2.h"

# ifdef WCARD1
# ifdef WCARD2
# define NOINDIRECT
# endif
# endif

int e2print(NODE *p, int down, int *a, int *b);

extern int vdebug;

int fldsz, fldshf;

static int mamask[] = { /* masks for matching dope with shapes */
	SIMPFLG,		/* OPSIMP */
	SIMPFLG|ASGFLG,		/* ASG OPSIMP */
	COMMFLG,	/* OPCOMM */
	COMMFLG|ASGFLG,	/* ASG OPCOMM */
	MULFLG,		/* OPMUL */
	MULFLG|ASGFLG,	/* ASG OPMUL */
	DIVFLG,		/* OPDIV */
	DIVFLG|ASGFLG,	/* ASG OPDIV */
	UTYPE,		/* OPUNARY */
	TYFLG,		/* ASG OPUNARY is senseless */
	LTYPE,		/* OPLEAF */
	TYFLG,		/* ASG OPLEAF is senseless */
	0,		/* OPANY */
	ASGOPFLG|ASGFLG,	/* ASG OPANY */
	LOGFLG,		/* OPLOG */
	TYFLG,		/* ASG OPLOG is senseless */
	FLOFLG,		/* OPFLOAT */
	FLOFLG|ASGFLG,	/* ASG OPFLOAT */
	SHFFLG,		/* OPSHFT */
	SHFFLG|ASGFLG,	/* ASG OPSHIFT */
	SPFLG,		/* OPLTYPE */
	TYFLG,		/* ASG OPLTYPE is senseless */
	};

int s2debug = 0;

/*
 * return true if shape is appropriate for the node p
 * side effect for SFLD is to set up fldsz, etc
 */
int
tshape(NODE *p, int shape)
{
	int o, mask;

	o = p->n_op;

# ifndef BUG3
	if (s2debug) {
		printf("tshape(%p, ", p);
		prcook(shape);
		printf(") op = %s\n", opst[o]);
	}
# endif

	if( shape & SPECIAL ){

		switch( shape ){
		case SZERO:
		case SONE:
		case SMONE:
		case SSCON:
		case SCCON:
			if( o != ICON || p->n_name[0] ) return(0);
			}

		switch( shape ){

		case SZERO:
			return( p->n_lval == 0 );
		case SONE:
			return( p->n_lval == 1 );
		case SMONE:
			return( p->n_lval == -1 );
		case SSCON:
			return( p->n_lval > -32769 && p->n_lval < 32768 );
		case SCCON:
			return( p->n_lval > -129 && p->n_lval < 128 );

		case SSOREG:	/* non-indexed OREG */
			if( o == OREG && !R2TEST(p->n_rval) ) return(1);
			else return(0);

		default:
# ifdef MULTILEVEL
			if( shape & MULTILEVEL )
				return( mlmatch(p,shape,0) );
			else
# endif
			return( special( p, shape ) );
			}
		}

	if( shape & SANY ) return(1);

	if( (shape&INTEMP) && shtemp(p) ) return(1);

	if( (shape&SWADD) && (o==NAME||o==OREG) ){
		if( BYTEOFF(p->n_lval) ) return(0);
		}

# ifdef WCARD1
	if( shape & WCARD1 )
		return( wcard1(p) & shape );
# endif

# ifdef WCARD2
	if( shape & WCARD2 )
		return( wcard2(p) & shape );
# endif
	switch( o ){

	case NAME:
		return( shape&SNAME );
	case ICON:
		mask = SCON;
		return( shape & mask );

	case FLD:
		if( shape & SFLD ){
			if( !flshape( p->n_left ) ) return(0);
			/* it is a FIELD shape; make side-effects */
			o = p->n_rval;
			fldsz = UPKFSZ(o);
# ifdef RTOLBYTES
			fldshf = UPKFOFF(o);
# else
			fldshf = SZINT - fldsz - UPKFOFF(o);
# endif
			return(1);
			}
		return(0);

	case CCODES:
		return( shape&SCC );

	case REG:
		/* distinctions:
		SAREG	any scalar register
		STAREG	any temporary scalar register
		SBREG	any lvalue (index) register
		STBREG	any temporary lvalue register
		*/
		mask = isbreg(p->n_rval) ? SBREG : SAREG;
		if (istreg(p->n_rval) && busy[p->n_rval]<=1 )
			mask |= mask==SAREG ? STAREG : STBREG;
		return( shape & mask );

	case OREG:
		return( shape & SOREG );

# ifndef NOINDIRECT
	case UNARY MUL:
		/* return STARNM or STARREG or 0 */
		return( shumul(p->n_left) & shape );
# endif

		}

	return(0);
	}

/*
 * does the type t match tword
 */
int
ttype(TWORD t, int tword)
{
	if (tword & TANY)
		return(1);

	if (t == UNDEF)
		t=INT; /* void functions eased thru tables */
# ifndef BUG3
	if (t2debug)
		printf("ttype(%o, %o)\n", t, tword);
# endif
	if (ISPTR(t) && (tword&TPTRTO)) {
		do {
			t = DECREF(t);
		} while (ISARY(t));
			/* arrays that are left are usually only
			 * in structure references...
			 */
		return (ttype(t, tword&(~TPTRTO)));
	}
	if (t != BTYPE(t))
		return (tword & TPOINT); /* TPOINT means not simple! */
	if (tword & TPTRTO)
		return(0);

	switch (t) {
	case CHAR:
		return( tword & TCHAR );
	case SHORT:
		return( tword & TSHORT );
	case STRTY:
	case UNIONTY:
		return( tword & TSTRUCT );
	case INT:
		return( tword & TINT );
	case UNSIGNED:
		return( tword & TUNSIGNED );
	case USHORT:
		return( tword & TUSHORT );
	case UCHAR:
		return( tword & TUCHAR );
	case ULONG:
		return( tword & TULONG );
	case LONG:
		return( tword & TLONG );
	case LONGLONG:
		return( tword & TLONGLONG );
	case ULONGLONG:
		return( tword & TULONGLONG );
	case FLOAT:
		return( tword & TFLOAT );
	case DOUBLE:
		return( tword & TDOUBLE );
	}

	return(0);
}

struct optab *rwtable;

struct optab *opptr[DSIZE];

void
setrew()
{
	/* set rwtable to first value which allows rewrite */
	struct optab *q;
	int i;

# ifdef MULTILEVEL
	/* also initialize multi-level tree links */
	mlinit();
# endif

	for (q = table; q->op != FREE; ++q) {
		if (q->needs == REWRITE) {
			rwtable = q;
			goto more;
		}
	}
	cerror( "bad setrew" );


	more:
	for (i=0; i<DSIZE; ++i) {
		if (dope[i] == 0)
			continue;

		/* there is an op... */
		for (q=table; q->op != FREE; ++q) {
			/*  beware; things like LTYPE that match
			    multiple things in the tree must
			    not try to look at the NIL at this
			    stage of things!  Put something else
			    first in table.c  */
			/* at one point, the operator matching was 15% of the
			    total comile time; thus, the function
			    call that was here was removed...
			*/

			if (q->op < OPSIMP) {
				if (q->op==i)
					break;
			} else {
				int opmtemp;
				if ((opmtemp=mamask[q->op - OPSIMP])&SPFLG) {
					if (i==NAME || i==ICON || i==OREG)
						break;
					else if (shltype( i, NIL))
						break;
				} else if ((dope[i]&(opmtemp|ASGFLG))==opmtemp)
					break;
			}
		}
		opptr[i] = q;
	}
}

#ifdef MATCHSTATS
struct matchstats {
	unsigned ms_total;
	unsigned ms_opsimp;
	unsigned ms_opglob;
	unsigned ms_cookie;
	unsigned ms_shape;
	unsigned ms_type;
	unsigned ms_rewrite;
	unsigned ms_allo;
	unsigned ms_done;
	unsigned ms_nope;
} ms;
#define CMS(x) { ++x; continue; }
#else
#define CMS(x) continue;
#endif

/*
 * called by: order, gencall
 * look for match in table and generate code if found unless
 * entry specified REWRITE.
 * returns MDONE, MNOPE, or rewrite specification from table
 */
int
match(NODE *p, int cookie)
{
	struct optab *q;
	NODE *r;
	int rval;

	rcount();
	if (cookie == FORREW)
		q = rwtable;
	else
		q = opptr[p->n_op];

# ifndef BUG4
	if (mdebug) {
		printf("match(%p, ", p);
		prcook(cookie);
		printf(")\n");
		fwalk(p, e2print, 0);
	}
# endif

	for (; q->op != FREE; ++q) {

		/* at one point the call that was here was over 15% of
		 * the total time; thus the function call was expanded inline
		 */
#ifdef MATCHSTATS
		++ms.ms_total;
#endif

		/*
		 * Optimizing to avoid unneccessary OPSIMP checks.
		 */
		if (q->op < OPSIMP) {
			if (q->op != p->n_op)
				CMS(ms.ms_opsimp)
		} else {
			int opmtemp;

			if ((opmtemp=mamask[q->op - OPSIMP])&SPFLG) {
				if (p->n_op!=NAME && p->n_op!=ICON &&
				    p->n_op!= OREG && !shltype(p->n_op, p))
					CMS(ms.ms_opglob)
			} else if ((dope[p->n_op]&(opmtemp|ASGFLG)) != opmtemp)
				CMS(ms.ms_opglob)
		}

		/* Check if cookie matches this entry */
		if (!(q->visit & cookie))
			CMS(ms.ms_cookie)

		/* see if left child matches */
		r = getlr(p, 'L');
		if (mdebug) {
			printf("matching left shape (%s) against (",
			    opst[r->n_op]);
			prcook(q->lshape);
			printf(")\n");
			printf("matching left type (");
			tprint(r->n_type);
			printf(") against (");
			prttype(q->ltype);
			printf(")\n");
		}
		if (!tshape( r, q->lshape))
			CMS(ms.ms_shape)
		if (!ttype(r->n_type, q->ltype))
			CMS(ms.ms_type)

		/* see if right child matches */
		r = getlr(p, 'R');
		if (mdebug) {
			printf("matching right shape (%s) against (",
			    opst[r->n_op]);
			prcook(q->rshape);
			printf(")\n");
			printf("matching right type (");
			tprint(r->n_type);
			printf(") against (");
			prttype(q->rtype);
			printf(")\n");
		}
		if (!tshape(r, q->rshape))
			CMS(ms.ms_shape)
		if (!ttype(r->n_type, q->rtype))
			CMS(ms.ms_type)

		/*
		 * REWRITE means no code from this match but go ahead
		 * and rewrite node to help future match
		 */
		if (q->needs & REWRITE) {
#ifdef MATCHSTATS
			++ms.ms_rewrite;
#endif
			rval = q->rewrite;
			goto leave;
		}
		if (!allo(p, q)) { /* if can't generate code, skip entry */
			if (mdebug)
				printf("allo(p, q) failed\n");
			CMS(ms.ms_allo)
		}

		/* resources are available */

		expand(p, cookie, q->cstring);		/* generate code */
		reclaim(p, q->rewrite, cookie);
#ifdef MATCHSTATS
		++ms.ms_done;
#endif

		rval = MDONE;
		goto leave;

	}

#ifdef MATCHSTATS
	++ms.ms_nope;
#endif
	rval = MNOPE;
leave:
# ifndef BUG4
	if (odebug) {
		printf("leave match(%p, ", p);
		prcook(cookie);
		printf(") == ");
		if (rval == MNOPE)
			puts("MNOPE");
		else if (rval == MDONE)
			puts("MDONE");
		else {
			prcook(cookie);
			putchar('\n');
		}
	}
# endif

	return rval;
}

/*
 * generate code by interpreting table entry
 */
void
expand(NODE *p, int cookie, char *cp)
{
# ifdef NEWZZZ
	char c;
# endif
	CONSZ val;

	for( ; *cp; ++cp ){
		switch( *cp ){

		default:
			PUTCHAR( *cp );
			continue;  /* this is the usual case... */

		case 'T':
			/* rewrite register type is suppressed */
			continue;

		case 'Z':  /* special machine dependent operations */
# ifdef NEWZZZ
			switch( c = *++cp ) {

			case '1':
			case '2':
			case '3':
			case 'R':
			case 'L':	/* get down first */
				zzzcode( getlr( p, c ), *++cp );
				break;
			default:   /* normal zzzcode processing otherwise */
				zzzcode( p, c );
				break;
			}
# else
			zzzcode( p, *++cp );
# endif
			continue;

		case 'F':  /* this line deleted if FOREFF is active */
			if( cookie & FOREFF ) while( *++cp != '\n' ) ; /* VOID */
			continue;

		case 'S':  /* field size */
			printf( "%d", fldsz );
			continue;

		case 'H':  /* field shift */
			printf( "%d", fldshf );
			continue;

		case 'M':  /* field mask */
		case 'N':  /* complement of field mask */
			val = 1;
			val <<= fldsz;
			--val;
			val <<= fldshf;
			adrcon( *cp=='M' ? val : ~val );
			continue;

		case 'L':  /* output special label field */
			if (*++cp == 'C')
				printf(LABFMT, p->n_label);
			else
				printf(LABFMT, (int)getlr(p,*cp)->n_lval);
			continue;

		case 'O':  /* opcode string */
			hopcode( *++cp, p->n_op );
			continue;

		case 'B':  /* byte offset in word */
			val = getlr(p,*++cp)->n_lval;
			val = BYTEOFF(val);
			printf( CONFMT, val );
			continue;

		case 'C': /* for constant value only */
			conput( getlr( p, *++cp ) );
			continue;

		case 'I': /* in instruction */
			insput( getlr( p, *++cp ) );
			continue;

		case 'A': /* address of */
			adrput( getlr( p, *++cp ) );
			continue;

		case 'U': /* for upper half of address, only */
			upput( getlr( p, *++cp ), SZLONG );
			continue;

			}

		}

	}

NODE *
getlr(NODE *p, int c)
{

	/* return the pointer to the left or right side of p, or p itself,
	   depending on the optype of p */

	switch (c) {

	case '1':
	case '2':
	case '3':
		return( &resc[c-'1'] );

	case 'L':
		return( optype( p->n_op ) == LTYPE ? p : p->n_left );

	case 'R':
		return( optype( p->n_op ) != BITYPE ? p : p->n_right );

	}
	cerror( "bad getlr: %c", c );
	/* NOTREACHED */
	return NULL;
}
# ifdef MULTILEVEL

union mltemplate{
	struct ml_head{
		int tag; /* identifies class of tree */
		int subtag; /* subclass of tree */
		union mltemplate * nexthead; /* linked by mlinit() */
		} mlhead;
	struct ml_node{
		int op; /* either an operator or op description */
		int nshape; /* shape of node */
		/* both op and nshape must match the node.
		 * where the work is to be done entirely by
		 * op, nshape can be SANY, visa versa, op can
		 * be OPANY.
		 */
		int ntype; /* type descriptor from mfile2 */
		} mlnode;
	};

# define MLSZ 30

extern union mltemplate mltree[];
int mlstack[MLSZ];
int *mlsp; /* pointing into mlstack */
NODE * ststack[MLSZ];
NODE **stp; /* pointing into ststack */

mlinit(){
	union mltemplate **lastlink;
	register union mltemplate *n;
	register mlop;

	lastlink = &(mltree[0].nexthead);
	n = &mltree[0];
	for( ; (n++)->mlhead.tag != 0;
		*lastlink = ++n, lastlink = &(n->mlhead.nexthead) ){
# ifndef BUG3
		if( vdebug )printf("mlinit: %d\n",(n-1)->mlhead.tag);
# endif
	/* wander thru a tree with a stack finding
	 * its structure so the next header can be located.
	 */
		mlsp = mlstack;

		for( ;; ++n ){
			if( (mlop = n->mlnode.op) < OPSIMP ){
				switch( optype(mlop) ){

					default:
						cerror("(1)unknown opcode: %o",mlop);
					case BITYPE:
						goto binary;
					case UTYPE:
						break;
					case LTYPE:
						goto leaf;
					}
				}
			else{
				if( mamask[mlop-OPSIMP] &
					(SIMPFLG|COMMFLG|MULFLG|DIVFLG|LOGFLG|FLOFLG|SHFFLG) ){
				binary:
					*mlsp++ = BITYPE;
					}
				else if( ! (mamask[mlop-OPSIMP] & UTYPE) ){/* includes OPANY */

				leaf:
					if( mlsp == mlstack )
						goto tree_end;
					else if ( *--mlsp != BITYPE )
						cerror("(1)bad multi-level tree descriptor around mltree[%d]",
						n-mltree);
					}
				}
			}
		tree_end: /* n points to final leaf */
		;
		}
# ifndef BUG3
		if( vdebug > 3 ){
			printf("mltree={\n");
			for( n= &(mltree[0]); n->mlhead.tag != 0; ++n)
				printf("%o: %d, %d, %o,\n",n,
				n->mlhead.tag,n->mlhead.subtag,n->mlhead.nexthead);
			printf("	}\n");
			}
# endif
	}

mlmatch( subtree, target, subtarget ) NODE * subtree; int target,subtarget;{
	/*
	 * does subtree match a multi-level tree with
	 * tag "target"?  Return zero on failure,
	 * non-zero subtag on success (or MDONE if
	 * there is a zero subtag field).
	 */
	union mltemplate *head; /* current template header */
	register union mltemplate *n; /* node being matched */
	NODE * st; /* subtree being matched */
	register int mlop;

# ifndef BUG3
	if( vdebug ) printf("mlmatch(%o,%d)\n",subtree,target);
# endif
	for( head = &(mltree[0]); head->mlhead.tag != 0;
		head=head->mlhead.nexthead){
# ifndef BUG3
		if( vdebug > 1 )printf("mlmatch head(%o) tag(%d)\n",
			head->mlhead.tag);
# endif
		if( head->mlhead.tag != target )continue;
		if( subtarget && head->mlhead.subtag != subtarget)continue;
# ifndef BUG3
		if( vdebug ) printf("mlmatch for %d\n",target);
# endif

		/* potential for match */

		n = head + 1;
		st = subtree;
		stp = ststack;
		mlsp = mlstack;
		/* compare n->op, ->nshape, ->ntype to
		 * the subtree node st
		 */
		for( ;; ++n ){ /* for each node in multi-level template */
			/* opmatch */
			if( n->op < OPSIMP ){
				if( st->op != n->op )break;
				}
			else {
				register opmtemp;
				if((opmtemp=mamask[n->op-OPSIMP])&SPFLG){
					if(st->op!=NAME && st->op!=ICON && st->op!=OREG && 
						! shltype(st->op,st)) break;
					}
				else if((dope[st->op]&(opmtemp|ASGFLG))!=opmtemp) break;
				}
			/* check shape and type */

			if( ! tshape( st, n->mlnode.nshape ) ) break;
			if( ! ttype( st->type, n->mlnode.ntype ) ) break;

			/* that node matched, let's try another */
			/* must advance both st and n and halt at right time */

			if( (mlop = n->mlnode.op) < OPSIMP ){
				switch( optype(mlop) ){

					default:
						cerror("(2)unknown opcode: %o",mlop);
					case BITYPE:
						goto binary;
					case UTYPE:
						st = st->left;
						break;
					case LTYPE:
						goto leaf;
					}
				}
			else{
				if( mamask[mlop - OPSIMP] &
					(SIMPFLG|COMMFLG|MULFLG|DIVFLG|LOGFLG|FLOFLG|SHFFLG) ){
				binary:
					*mlsp++ = BITYPE;
					*stp++ = st;
					st = st->left;
					}
				else if( ! (mamask[mlop-OPSIMP] & UTYPE) ){/* includes OPANY */

				leaf:
					if( mlsp == mlstack )
						goto matched;
					else if ( *--mlsp != BITYPE )
						cerror("(2)bad multi-level tree descriptor around mltree[%d]",
						n-mltree);
					st = (*--stp)->right;
					}
				else /* UNARY */ st = st->left;
				}
			continue;

			matched:
			/* complete multi-level match successful */
# ifndef BUG3
			if( vdebug ) printf("mlmatch() success\n");
# endif
			if( head->mlhead.subtag == 0 ) return( MDONE );
			else {
# ifndef BUG3
				if( vdebug )printf("\treturns %d\n",
					head->mlhead.subtag );
# endif
				return( head->mlhead.subtag );
				}
			}
		}
	return( 0 );
	}
# endif

static char *tarr[] = {
	"CHAR", "SHORT", "INT", "LONG", "FLOAT", "DOUBLE", "POINT", "UCHAR",
	"USHORT", "UINT", "ULONG", "PTRTO", "ANY", "STRUCT", "LONGLONG",
	"ULONGLONG",
};

void
prttype(int t)
{
	int i, gone = 0;

	for (i = 0; i < 16; i++)
		if ((t >> i) & 1) {
			if (gone) putchar('|');
			gone++;
			printf("%s", tarr[i]);
		}
}
