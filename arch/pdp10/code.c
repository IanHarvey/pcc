#if 0
static char *sccsid ="@(#)code.c	1.10 (Berkeley) 5/31/88";
#endif


# include "pass1.h"
# include <stab.h>

/*
 * cause the alignment to become a multiple of n
 * Nothing to do on PDP10.
 */
void
defalign(int n)
{
}

/*
 * code for the end of a function
 */
void
efcode()
{
}

/*
 * code for the beginning of a function; a is an array of
 * indices in stab for the arguments; n is the number
 */
void
bfcode(struct symtab **a, int n)
{
	send_passt(IP_LOCCTR, PROG);
	defnam(cftnsp);
}


/*
 * by now, the automatics and register variables are allocated
 */
void
bccode()
{
	SETOFF(autooff, SZINT);
}

/* called just before final exit */
/* flag is 1 if errors, 0 if none */
void
ejobcode(int flag )
{
}

/*
 * Print character t at position i in one string, until t == -1.
 * Locctr & label is already defined.
 */
void
bycode(int t, int i)
{
	static	int	lastoctal = 0;

	/* put byte i+1 in a string */

	if (t < 0) {
		if (i != 0)
			puts("\"");
	} else {
		if (i == 0)
			printf("\t.ascii \"");
		if (t == '\\' || t == '"') {
			lastoctal = 0;
			putchar('\\');
			putchar(t);
		} else if (t < 040 || t >= 0177) {
			lastoctal++;
			printf("\\%o",t);
		} else if (lastoctal && '0' <= t && t <= '9') {
			lastoctal = 0;
			printf("\"\n\t.ascii \"%c", t);
		} else {	
			lastoctal = 0;
			putchar(t);
		}
	}
}

/*
 * n integer words of zeros
 */
void
zecode(int n)
{
	printf("	.block %d\n", n);
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
 * XXX - fix genswitch.
 */
void
genswitch(struct swents **p, int n)
{
	int i;

	/* simple switch code */
	for (i = 1; i <= n; ++i) {
		/* already in 1 */
		if (p[i]->sval >= 0 && p[i]->sval <= 0777777)
			printf("	cain 1,0%llo\n", p[i]->sval);
		else if (p[i]->sval < 0)
			printf("	camn 1,[ .long -0%llo ]\n", -p[i]->sval);
		else
			printf("	camn 1,[ .long 0%llo ]\n", p[i]->sval);
		printf("	jrst L%d\n", p[i]->slab);
	}
	if (p[0]->slab > 0)
		branch(p[0]->slab);
}
