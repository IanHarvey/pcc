#if 0
static char *sccsid ="@(#)code.c	1.10 (Berkeley) 5/31/88";
#endif


# include "pass1.h"
# include <stab.h>
# include <stdlib.h>
# include <string.h>

int gdebug;
int labelno;
int proflg = 0;	/* are we generating profiling code? */
int fdefflag;  /* are we within a function definition ? */
int strftn = 0;  /* is the current function one which returns a value */

# define putstr(s)	fputs((s), stdout)

/* output a branch to label n */
void
branch(int n)
{
	if (nerrors)
		return;
	p1print("	jrst L%d\n", n);
}

int lastloc = { -1 };

/*
 * cause the alignment to become a multiple of n
 * Nothing to do on PDP10.
 */
void
defalign(int n)
{
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
		p1print("	.text\n");
		psline();
		break;

	case DATA:
	case ADATA:
		p1print("	.data\n");
		break;

	case ISTRNG:
	case STRNG:
		p1print("	.text\n"); /* XXX should be .rodata */
		break;

	case STAB:
		p1print("	.stab\n");
		break;

	default:
		cerror("illegal location counter");
	}

	return (temp);
}

/*
 * output something to define the current position as label n
 * XXX - should be in pass2.
 */
void
deflab(int n)
{
	if (nerrors)
		return;
	p1print("L%d:\n", n);
}

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

		p = cftnsp;
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
bfcode(struct symtab **a, int n)
{
	int temp;
	struct symtab *p;
	int off;

	if (nerrors)
		return;
	(void) locctr(PROG);
	p = cftnsp;
	defnam(p);
	temp = p->stype;
	temp = DECREF(temp);
	strftn = (temp==STRTY) || (temp==UNIONTY);

	retlab = getlab();

	/* routine prolog */

	ftlab1 = getlab();
	ftlab2 = getlab();
	p1print("	jrst L%d\n", ftlab1);
	p1print("L%d:\n", ftlab2);

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

/*
 * define the current location as the name p->sname
 */
void
defnam(struct symtab *p)
{
	if (p->sclass == EXTDEF)
		p1print("	.globl	%s\n", exname(p->sname));
	if (p->sclass == STATIC && p->slevel>1)
		deflab(p->soffset);
	else
		p1print("%s:\n", exname(p->sname));

}

void
bycode(int t, int i)
{
	static	int	lastoctal = 0;

	/* put byte i+1 in a string */

	if (nerrors)
		return;

	if (t < 0) {
		if (i != 0) {
			p1print("\"\n");
		}
	} else {
		if (i == 0) {
			p1print("\t.ascii\t\"");
		}
		if (t == '\\' || t == '"') {
			lastoctal = 0;
			p1print("\\%c", t);
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
		else if (t == ':' || t < 040 || t >= 0177) {
			lastoctal++;
			p1print("\\%o",t);
		} else if (lastoctal && '0' <= t && t <= '9') {
			lastoctal = 0;
			p1print("\"\n\t.ascii\t\"%c", t);
		} else {	
			lastoctal = 0;
			p1print("%c", t);
		}
	}
}

/*
 * n integer words of zeros
 */
void
zecode(int n)
{
	if (n <= 0)
		return;
	p1print("	.block	%d\n", n);
	inoff += n * SZINT;
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

/* p points to an array of structures, each consisting
 * of a constant value and a label.
 * The first is >=0 if there is a default label;
 * its value is the label number
 * The entries p[1] to p[n] are the nontrivial cases
 */
void
genswitch(struct swents **p, int n)
{
	int i;

	/* simple switch code */
	for (i = 1; i <= n; ++i) {
		/* already in 1 */
		if (p[i]->sval >= 0 && p[i]->sval <= 0777777)
			p1print("	cain 1,0%llo\n", p[i]->sval);
		else
			p1print("	camn 1,[ .long 0%llo ]\n", p[i]->sval);
		p1print("	jrst L%d\n", p[i]->slab);
	}
	if (p[0]->slab > 0)
		branch(p[0]->slab);
}
