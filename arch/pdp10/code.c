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

int lastloc = { -1 };

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
	fdefflag = 0;
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

void
bycode(int t, int i)
{
	static	int	lastoctal = 0;
	char ch[10];

	/* put byte i+1 in a string */

	if (nerrors)
		return;

	if (t < 0) {
		if (i != 0) {
			send_passt(IP_INIT, "\"\n");
		}
	} else {
		if (i == 0) {
			send_passt(IP_INIT, "\t.ascii\t\"");
		}
		if (t == '\\' || t == '"') {
			lastoctal = 0;
			sprintf(ch, "\\%c", t);
			send_passt(IP_INIT, ch);
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
			sprintf(ch, "\\%o",t);
			send_passt(IP_INIT, ch);
		} else if (lastoctal && '0' <= t && t <= '9') {
			lastoctal = 0;
			sprintf(ch, "%c", t);
			send_passt(IP_INIT, "\"\n\t.ascii\t\"");
			send_passt(IP_INIT, ch);
		} else {	
			lastoctal = 0;
			sprintf(ch, "%c", t);
			send_passt(IP_INIT, ch);
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
	char *ch;

	/* simple switch code */
	for (i = 1; i <= n; ++i) {
		ch = isinlining ? permalloc(40) : tmpalloc(40);
		/* already in 1 */
		if (p[i]->sval >= 0 && p[i]->sval <= 0777777)
			sprintf(ch, "	cain 1,0%llo\n", p[i]->sval);
		else if (p[i]->sval < 0)
			sprintf(ch, "	camn 1,[ .long -0%llo ]\n", -p[i]->sval);
		else
			sprintf(ch, "	camn 1,[ .long 0%llo ]\n", p[i]->sval);
		send_passt(IP_INIT, ch);
		ch = isinlining ? permalloc(40) : tmpalloc(40);
		sprintf(ch, "	jrst L%d\n", p[i]->slab);
		send_passt(IP_INIT, ch);
	}
	if (p[0]->slab > 0)
		branch(p[0]->slab);
}
