#if 0
static char *sccsid ="@(#)code.c	1.10 (Berkeley) 5/31/88";
#endif


# include "pass1.h"
# include <a.out.h>
# include <stab.h>

int gdebug;
int labelno;
int proflg = 0;	/* are we generating profiling code? */
int fdefflag;  /* are we within a function definition ? */
int strftn = 0;  /* is the current function one which returns a value */

# define putstr(s)	fputs((s), stdout)

#if 0
void makeheap(struct sw *, int, int);
void walkheap(int, int);
int selectheap(int);
#endif

/* output a branch to label n */
void
branch(int n)
{
	if (nerrors)
		return;
	printf("	jrst L%d\n", n);
}

int lastloc = { -1 };

#if 0
short log2tab[] = {0, 0, 1, 2, 2, 3, 3, 3, 3};
#define LOG2SZ 9
#endif

/*
 * cause the alignment to become a multiple of n
 * Nothing to do on PDP10.
 */
void
defalign(int n)
{
	if ((n % SZINT) != 0)
		cerror("defalign: %d", n);
}

int
locctr(int l)
{
	int temp;
	/* l is PROG, ADATA, DATA, STRNG, ISTRNG, or STAB */

	if (l == lastloc)
		return(l);
	temp = lastloc;
	lastloc = l;
	if (nerrors)
		return(temp);

	switch (l) {
	case PROG:
		putstr("	.text\n");
		psline();
		break;

	case DATA:
	case ADATA:
		putstr("	.data\n");
		break;

	case STRNG:
		putstr("	.data	1\n");
		break;

	case ISTRNG:
		putstr("	.data	2\n");
		break;

	case STAB:
		putstr("	.stab\n");
		break;

	case BSS:
		putstr("\t.bss\n");
		break;

	default:
		cerror("illegal location counter");
	}

	return (temp);
}

/*
 * output something to define the current position as label n
 */
void
deflab(int n)
{
	if (nerrors)
		return;
	printf("L%d:\n", n);
}


/*
 * return a number usable for a label
 */
int
getlab()
{
	static int crslab = 10;
	return ++crslab;
}


#if 0
int ent_mask[] = {
	0,0,0,0,0, 0xfc0, 0xf80, 0xf00, 0xe00, 0xc00, 0x800, 0};

#endif
int reg_use = 015;

/*
 * code for the end of a function
 */
void
efcode()
{
	if( strftn ){  /* copy output (in R2) to caller */
		cerror("efcode: struct return");
#ifdef notyet
		register NODE *l, *r;
		register struct symtab *p;
		register TWORD t;
		int i;

		p = &stab[curftn];
		t = p->stype;
		t = DECREF(t);

		deflab( retlab );

		i = getlab();	/* label for return area */
#ifndef LCOMM
		putstr("	.data\n" );
		putstr("	.align	2\n" );
		printf("L%d:	.space	%d\n", i, tsize(t, p->dimoff, p->sizoff)/SZCHAR );
		putstr("	.text\n" );
#else
		{ int sz = tsize(t, p->dimoff, p->sizoff) / SZCHAR;
		if (sz % (SZINT/SZCHAR))
			sz += (SZINT/SZCHAR) - (sz % (SZINT/SZCHAR));
		printf("	.lcomm	L%d,%d\n", i, sz);
		}
#endif
		psline();
		printf("	movab	L%d,r1\n", i);

		reached = 1;
		l = block( REG, NIL, NIL, PTR|t, p->dimoff, p->sizoff );
		l->tn.rval = 1;  /* R1 */
		l->tn.lval = 0;  /* no offset */
		r = block( REG, NIL, NIL, PTR|t, p->dimoff, p->sizoff );
		r->tn.rval = 0;  /* R0 */
		r->tn.lval = 0;
		l = buildtree( UNARY MUL, l, NIL );
		r = buildtree( UNARY MUL, r, NIL );
		l = buildtree( ASSIGN, l, r );
		l->in.op = FREE;
		ecomp( l->in.left );
		printf( "	movab	L%d,r0\n", i );
		/* turn off strftn flag, so return sequence will be generated */
		strftn = 0;
#endif
	}
	branch(retlab);
#if 0
#ifndef VMS
	printf( "	.set	L%d,0x%x\n", ftnno, ent_mask[reg_use] );
#else
	printf( "	.set	L%d,%d	# Hex = 0x%x\n", ftnno, 0x3c| ent_mask[reg_use], ent_mask[reg_use]  );
	/* KLS kludge, under VMS if you use regs 2-5, you must save them. */
#endif
#endif
	reg_use = 015;
	p2bend();
	fdefflag = 0;
}

int ftlab1, ftlab2;

/*
 * code for the beginning of a function; a is an array of
 * indices in stab for the arguments; n is the number
 */
void
bfcode(int a[], int n)
{
//	int i;
	int temp;
	struct symtab *p;
	int off;

	if (nerrors)
		return;
	(void) locctr(PROG);
	p = &stab[curftn];
	defnam(p);
	temp = p->stype;
	temp = DECREF(temp);
	strftn = (temp==STRTY) || (temp==UNIONTY);

	retlab = getlab();

	/* routine prolog */

	ftlab1 = getlab();
	ftlab2 = getlab();
	printf("	jrst L%d\n", ftlab1);
	printf("L%d:\n", ftlab2);

#ifdef notyet
	if( proflg ) {	/* profile code */
		i = getlab();
		printf("	movab	L%d,r0\n", i);
		putstr("	jsb 	mcount\n");
		putstr("	.data\n");
		putstr("	.align	2\n");
		printf("L%d:	.long	0\n", i);
		putstr("	.text\n");
		psline();
	}
#endif

	off = ARGINIT;

#if 0
	for( i=0; i<n; ++i ){
		p = &stab[a[i]];
		if( p->sclass == REGISTER ){
			temp = p->offset;  /* save register number */
			p->sclass = PARAM;  /* forget that it is a register */
			p->offset = NOOFFSET;
			(void) oalloc( p, &off );
/*tbl*/		printf( "	%s	%d(ap),r%d\n", toreg(p->stype), p->offset/SZCHAR, temp );
			p->offset = temp;  /* remember register number */
			p->sclass = REGISTER;   /* remember that it is a register */
			}
		else if( p->stype == STRTY || p->stype == UNIONTY ) {
			p->offset = NOOFFSET;
			if( oalloc( p, &off ) ) cerror( "bad argument" );
			SETOFF( off, ALSTACK );
			}
		else {
			if( oalloc( p, &off ) ) cerror( "bad argument" );
			}

		}
#endif

#ifdef notyet
	if (gdebug && !nerrors)
		pstabdot(N_SLINE, lineno);
#endif
	fdefflag = 1;
}


/*
 * by now, the automatics and register variables are allocated
 */
void
bccode()
{
	SETOFF(autooff, SZINT);
	/* set aside store area offset */
	p2bbeg(autooff, regvar);
	reg_use = (reg_use > regvar ? regvar : reg_use);
}

/* called just before final exit */
/* flag is 1 if errors, 0 if none */
void
ejobcode(int flag )
{
}

#if 0
#ifndef aobeg
aobeg(){
	/* called before removing automatics from stab */
	}
#endif aobeg

#ifndef aocode
/*ARGSUSED*/
aocode(p) struct symtab *p; {
	/* called when automatic p removed from stab */
	}
#endif aocode

#ifndef aoend
aoend(){
	/* called after removing all automatics from stab */
	}
#endif aoend

#endif
/*
 * define the current location as the name p->sname
 */
void
defnam(struct symtab *p)
{
	if (p->sclass == EXTDEF)
		printf("	.globl	%s\n", exname(p->sname));
	if (p->sclass == STATIC && p->slevel>1)
		deflab(p->offset);
	else
		printf("%s:\n", exname(p->sname));

}

void
bycode(int t, int i)
{
	cerror("bycode");
#if 0
#ifdef ASSTRINGS
static	int	lastoctal = 0;
#endif

	/* put byte i+1 in a string */

	if ( nerrors ) return;
#ifdef ASSTRINGS

	i &= 077;
	if ( t < 0 ){
		if ( i != 0 )	putstr( "\"\n" );
	} else {
		if ( i == 0 ) putstr("\t.ascii\t\"");
		if ( t == '\\' || t == '"'){
			lastoctal = 0;
			printf("\\%c", t);
		}
			/*
			 *	We escape the colon in strings so that
			 *	c2 will, in its infinite wisdom, interpret
			 *	the characters preceding the colon as a label.
			 *	If we didn't escape the colon, c2 would
			 *	throw away any trailing blanks or tabs after
			 *	the colon, but reconstruct a assembly
			 *	language semantically correct program.
			 *	C2 hasn't been taught about strings.
			 */
		else if ( t == ':' || t < 040 || t >= 0177 ){
			lastoctal++;
			printf("\\%o",t);
		}
		else if ( lastoctal && '0' <= t && t <= '9' ){
			lastoctal = 0;
			printf("\"\n\t.ascii\t\"%c", t );
		}
		else
		{	
			lastoctal = 0;
			putchar(t);
		}
		if ( i == 077 ) putstr("\"\n");
	}
#else

	i &= 07;
	if( t < 0 ){ /* end of the string */
		if( i != 0 ) putchar( '\n' );
		}

	else { /* stash byte t into string */
		if( i == 0 ) putstr( "	.byte	" );
		else putchar( ',' );
		printf( "0x%x", t );
		if( i == 07 ) putchar( '\n' );
		}
#endif
#endif
	}

/*
 * n integer words of zeros
 */
void
zecode(int n)
{
	if (n <= 0)
		return;
	printf("	.block	%d\n", n);
}

/*
 * return the alignment of field of type t
 */
int
fldal(unsigned int t)
{
	uerror("illegal field type");
	return(ALINT);
}

/* fix up type of field p */
void
fldty(struct symtab *p)
{
}

/*
 * print location of error
 * XXX - merge where in pass 1 & 2!
 */
void
where(int c)
{
	/* c is either 'u', 'c', or 'w' */
	fprintf(stderr, "%s, line %d: ", ftitle, lineno);
}

#if 0

#ifdef TRUST_REG_CHAR_AND_REG_SHORT
/* tbl - toreg() returns a pointer to a char string
		  which is the correct  "register move" for the passed type 
 */
struct type_move {TWORD fromtype; char tostrng[8];} toreg_strs[] =
	{
	INT, "movl",
	UNSIGNED,	"movl",
	DOUBLE, "movq",
	CHAR, "cvtbl",
	SHORT, "cvtwl",
	UCHAR,	"movzbl",
	USHORT,	"movzwl",
	0, ""
	};

char
*toreg(type)
	TWORD type;
{
	struct type_move *p;

	for ( p=toreg_strs; p->fromtype != 0; p++)
		if (p->fromtype == type) return(p->tostrng);

	/* type not found, must be a pointer type */
	return("movl");
}
/* tbl */
#endif

struct sw heapsw[SWITSZ];	/* heap for switches */
#endif

/*	p points to an array of structures, each consisting
	of a constant value and a label.
	The first is >=0 if there is a default label;
	its value is the label number
	The entries p[1] to p[n] are the nontrivial cases
	*/
void
genswitch(struct sw *p, int n)
{
	cerror("genswitch");
#if 0
	int i;
	CONSZ j, range;
	int dlab, swlab;

	if( nerrors ) return;
	range = p[n].sval-p[1].sval;

	if( range>0 && range <= 3*n && n>=4 ){ /* implement a direct switch */

		swlab = getlab();
		dlab = p->slab >= 0 ? p->slab : getlab();

		/* already in r0 */
		printf("	casel	r0,$%ld,$%ld\n", p[1].sval, range);
		printf("L%d:\n", swlab);
		for( i=1,j=p[1].sval; i<=n; j++) {
			printf("	.word	L%d-L%d\n", (j == p[i].sval ? ((j=p[i++].sval), p[i-1].slab) : dlab),
				swlab);
			}

		if( p->slab >= 0 ) branch( dlab );
		else printf("L%d:\n", dlab);
		return;

		}

	if( n>8 ) {	/* heap switch */

		heapsw[0].slab = dlab = p->slab >= 0 ? p->slab : getlab();
		makeheap(p, n, 1);	/* build heap */

		walkheap(1, n);	/* produce code */

		if( p->slab >= 0 )
			branch( dlab );
		else
			printf("L%d:\n", dlab);
		return;
	}

	/* debugging code */

	/* out for the moment
	if( n >= 4 ) werror( "inefficient switch: %d, %d", n, (int) (range/n) );
	*/

	/* simple switch code */

	for( i=1; i<=n; ++i ){
		/* already in r0 */

		putstr( "	cmpl	r0,$" );
		printf( CONFMT, p[i].sval );
		printf( "\n	jeql	L%d\n", p[i].slab );
		}

	if( p->slab>=0 ) branch( p->slab );
#endif
}

#if 0
void
makeheap(p, m, n)
register struct sw *p;
{
	int q;

	q = selectheap(m);
	heapsw[n] = p[q];
	if( q>1 ) makeheap(p, q-1, 2*n);
	if( q<m ) makeheap(p+q, m-q, 2*n+1);
}

int
selectheap(m) {
	register int l,i,k;

	for(i=1; ; i*=2)
		if( (i-1) > m ) break;
	l = ((k = i/2 - 1) + 1)/2;
	return( l + (m-k < l ? m-k : l));
}

void
walkheap(start, limit)
{
	int label;


	if( start > limit ) return;
	printf("	cmpl	r0,$%ld\n",  heapsw[start].sval);
	printf("	jeql	L%d\n", heapsw[start].slab);
	if( (2*start) > limit ) {
		printf("	jbr 	L%d\n", heapsw[0].slab);
		return;
	}
	if( (2*start+1) <= limit ) {
		label = getlab();
		printf("	jgtr	L%d\n", label);
	} else
		printf("	jgtr	L%d\n", heapsw[0].slab);
	walkheap( 2*start, limit);
	if( (2*start+1) <= limit ) {
		printf("L%d:\n", label);
		walkheap( 2*start+1, limit);
	}
}
#endif
