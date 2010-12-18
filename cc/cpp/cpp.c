/*	$Id$	*/

/*
 * Copyright (c) 2004,2010 Anders Magnusson (ragge@ludd.luth.se).
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

/*
 * The C preprocessor.
 * This code originates from the V6 preprocessor with some additions
 * from V7 cpp, and at last ansi/c99 support.
 */

#include "config.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <sys/stat.h>

#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "compat.h"
#include "cpp.h"
#include "y.tab.h"

#define	MAXARG	250	/* # of args to a macro, limited by char value */
#define	SBSIZE	600000

static usch	sbf[SBSIZE];
/* C command */

int tflag;	/* traditional cpp syntax */
#ifdef CPP_DEBUG
int dflag;	/* debug printouts */
#define	DPRINT(x) if (dflag) printf x
#define	DDPRINT(x) if (dflag > 1) printf x
#else
#define DPRINT(x)
#define DDPRINT(x)
#endif

#define	GCC_VARI

int ofd;
usch outbuf[CPPBUF];
int obufp, istty;
int Cflag, Mflag, dMflag, Pflag;
usch *Mfile;
struct initar *initar;
int readmac;

/* avoid recursion */
struct recur {
	struct recur *next;
	struct symtab *sp;
};

/* include dirs */
struct incs {
	struct incs *next;
	usch *dir;
	dev_t dev;
	ino_t ino;
} *incdir[2];
#define	INCINC 0
#define	SYSINC 1

static struct symtab *filloc;
static struct symtab *linloc;
static struct symtab *pragloc;
int	trulvl;
int	flslvl;
int	elflvl;
int	elslvl;
usch *stringbuf = sbf;

/*
 * Macro replacement list syntax:
 * - For object-type macros, replacement strings are stored as-is.
 * - For function-type macros, macro args are substituted for the
 *   character WARN followed by the argument number.
 * - The value element points to the end of the string, to simplify
 *   pushback onto the input queue.
 * 
 * The first character (from the end) in the replacement list is
 * the number of arguments:
 *   VARG  - ends with ellipsis, next char is argcount without ellips.
 *   OBJCT - object-type macro
 *   0 	   - empty parenthesis, foo()
 *   1->   - number of args.
 *
 * WARN is used:
 *	- in stored replacement lists to tell that an argument comes
 *	- When expanding replacement lists to tell that the list ended.
 */

#define	GCCARG	0xfd	/* has gcc varargs that may be replaced with 0 */
#define	VARG	0xfe	/* has varargs */
#define	OBJCT	0xff
#define	WARN	1	/* SOH, not legal char */
#define	CONC	2	/* STX, not legal char */
#define	SNUFF	3	/* ETX, not legal char */
#define	NOEXP	4	/* EOT, not legal char */
#define	EXPAND	5	/* ENQ, not legal char */
#define	PRAGS	6	/* start of converted pragma */
#define	PRAGE	14	/* end of converted pragma */

/* args for lookup() */
#define	FIND	0
#define	ENTER	1

static void expdef(const usch *proto, struct recur *, int gotwarn);
void define(void);
static int canexpand(struct recur *, struct symtab *np);
void include(void);
void include_next(void);
void line(void);
void flbuf(void);
void usage(void);
usch *xstrdup(const char *str);
const usch *prtprag(const usch *opb);
static void addidir(char *idir, struct incs **ww);

int
main(int argc, char **argv)
{
	struct initar *it;
	struct symtab *nl;
	register int ch;
	const usch *fn1, *fn2;

#ifdef TIMING
	struct timeval t1, t2;

	(void)gettimeofday(&t1, NULL);
#endif

	while ((ch = getopt(argc, argv, "CD:I:MPS:U:d:i:tvV?")) != -1)
		switch (ch) {
		case 'C': /* Do not discard comments */
			Cflag++;
			break;

		case 'i': /* include */
		case 'U': /* undef */
		case 'D': /* define something */
			/* XXX should not need malloc() here */
			if ((it = malloc(sizeof(struct initar))) == NULL)
				error("couldn't apply -%c %s", ch, optarg);
			it->type = ch;
			it->str = optarg;
			it->next = initar;
			initar = it;
			break;

		case 'M': /* Generate dependencies for make */
			Mflag++;
			break;

		case 'P': /* Inhibit generation of line numbers */
			Pflag++;
			break;

		case 'S':
		case 'I':
			addidir(optarg, &incdir[ch == 'I' ? INCINC : SYSINC]);
			break;

#ifdef CPP_DEBUG
		case 'V':
			dflag++;
			break;
#endif
		case 'v':
			printf("cpp: %s\n", VERSSTR);
			break;
		case 'd':
			if (optarg[0] == 'M') {
				dMflag = 1;
				Mflag = 1;
			}
			/* ignore others */
			break;

		case 't':
			tflag = 1;
			break;

		case '?':
			usage();
		default:
			error("bad arg %c\n", ch);
		}
	argc -= optind;
	argv += optind;

	filloc = lookup((const usch *)"__FILE__", ENTER);
	linloc = lookup((const usch *)"__LINE__", ENTER);
	pragloc = lookup((const usch *)"_Pragma", ENTER);
	filloc->value = linloc->value = (const usch *)""; /* Just something */
	pragloc->value = (const usch *)"";

	if (tflag == 0) {
		time_t t = time(NULL);
		usch *n = (usch *)ctime(&t);

		/*
		 * Manually move in the predefined macros.
		 */
		nl = lookup((const usch *)"__TIME__", ENTER);
		savch(0); savch('"');  n[19] = 0; savstr(&n[11]); savch('"');
		savch(OBJCT);
		nl->value = stringbuf-1;

		nl = lookup((const usch *)"__DATE__", ENTER);
		savch(0); savch('"'); n[24] = n[11] = 0; savstr(&n[4]);
		savstr(&n[20]); savch('"'); savch(OBJCT);
		nl->value = stringbuf-1;

		nl = lookup((const usch *)"__STDC__", ENTER);
		savch(0); savch('1'); savch(OBJCT);
		nl->value = stringbuf-1;

		nl = lookup((const usch *)"__STDC_VERSION__", ENTER);
		savch(0); savstr((const usch *)"199901L"); savch(OBJCT);
		nl->value = stringbuf-1;
	}

	if (Mflag && !dMflag) {
		usch *c;

		if (argc < 1)
			error("-M and no infile");
		if ((c = (usch *)strrchr(argv[0], '/')) == NULL)
			c = (usch *)argv[0];
		else
			c++;
		Mfile = stringbuf;
		savstr(c); savch(0);
		if ((c = (usch *)strrchr((char *)Mfile, '.')) == NULL)
			error("-M and no extension: ");
		c[1] = 'o';
		c[2] = 0;
	}

	if (argc == 2) {
		if ((ofd = open(argv[1], O_WRONLY|O_CREAT, 0600)) < 0)
			error("Can't creat %s", argv[1]);
	} else
		ofd = 1; /* stdout */
	istty = isatty(ofd);

	if (argc && strcmp(argv[0], "-")) {
		fn1 = fn2 = (usch *)argv[0];
	} else {
		fn1 = NULL;
		fn2 = (const usch *)"";
	}
	if (pushfile(fn1, fn2, 0, NULL))
		error("cannot open %s", argv[0]);

	flbuf();
	close(ofd);
#ifdef TIMING
	(void)gettimeofday(&t2, NULL);
	t2.tv_sec -= t1.tv_sec;
	t2.tv_usec -= t1.tv_usec;
	if (t2.tv_usec < 0) {
		t2.tv_usec += 1000000;
		t2.tv_sec -= 1;
	}
	fprintf(stderr, "cpp total time: %ld s %ld us\n",
	     t2.tv_sec, t2.tv_usec);
#endif
	return 0;
}

static void
addidir(char *idir, struct incs **ww)
{
	struct incs *w;
	struct stat st;

	if (stat(idir, &st) == -1 || S_ISDIR(st.st_mode) == 0)
		return; /* ignore */
	if (*ww != NULL) {
		for (w = *ww; w->next; w = w->next) {
			if (w->dev == st.st_dev && w->ino == st.st_ino)
				return;
		}
		if (w->dev == st.st_dev && w->ino == st.st_ino)
			return;
		ww = &w->next;
	}
	if ((w = calloc(sizeof(struct incs), 1)) == NULL)
		error("couldn't add path %s", idir);
	w->dir = (usch *)idir;
	w->dev = st.st_dev;
	w->ino = st.st_ino;
	*ww = w;
}

usch *
gotident(struct symtab *nl)
{
	struct symtab *thisnl;
	usch *osp, *ss2, *base;
	int c;

	thisnl = NULL;
	readmac++;
	base = osp = stringbuf;
	goto found;

	while ((c = sloscan()) != 0) {
		switch (c) {
		case IDENT:
			if (flslvl)
				break;
			osp = stringbuf;

			DPRINT(("IDENT0: %s\n", yytext));
			nl = lookup((usch *)yytext, FIND);
			if (nl == 0 || thisnl == 0)
				goto found;
			if (thisnl == nl) {
				nl = 0;
				goto found;
			}
			ss2 = stringbuf;
			if ((c = sloscan()) == WSPACE) {
				savstr((usch *)yytext);
				c = sloscan();
			}
			if (c != EXPAND) {
				unpstr((const usch *)yytext);
				if (ss2 != stringbuf)
					unpstr(ss2);
				unpstr(nl->namep);
				(void)sloscan(); /* get yytext correct */
				nl = 0; /* ignore */
			} else {
				thisnl = NULL;
				if (nl->value[0] == OBJCT) {
					unpstr(nl->namep);
					(void)sloscan(); /* get yytext correct */
					nl = 0;
				}
			}
			stringbuf = ss2;

found:			if (nl == 0 || subst(nl, NULL) == 0) {
				if (nl)
					savstr(nl->namep);
				else
					savstr((usch *)yytext);
			} else if (osp != stringbuf) {
				DPRINT(("IDENT1: unput osp %p stringbuf %p\n",
				    osp, stringbuf));
				ss2 = stringbuf;
				cunput(EXPAND);
				while (ss2 > osp)
					cunput(*--ss2);
				thisnl = nl;
				stringbuf = osp; /* clean up heap */
			}
			break;

		case EXPAND:
			DPRINT(("EXPAND!\n"));
			thisnl = NULL;
			break;

		case CMNT:
			getcmnt();
			break;

		case '\n':
			/* sloscan() will not eat */
			(void)cinput();
			savch(c);
			break;

		case STRING:
		case NUMBER:
		case WSPACE:
			savstr((usch *)yytext);
			break;

		default:
			if (c < 256)
				savch(c);
			else
				savstr((usch *)yytext);
			break;
		}
		if (thisnl == NULL) {
			readmac--;
			savch(0);
			return base;
		}
	}
	error("premature EOF");
	/* NOTREACHED */
	return NULL; /* XXX gcc */
}

void
line()
{
	static usch *lbuf;
	static int llen;
	usch *p;
	int c;

	if ((c = yylex()) != NUMBER)
		goto bad;
	ifiles->lineno = (int)(yylval.node.nd_val - 1);

	if ((c = yylex()) == '\n')
		return;

	if (c != STRING)
		goto bad;

	p = (usch *)yytext;
	if (*p == 'L')
		p++;
	c = strlen((char *)p);
	if (llen < c) {
		/* XXX may loose heap space */
		lbuf = stringbuf;
		stringbuf += c;
		llen = c;
	}
	p[strlen((char *)p)-1] = 0;
	if (strlcpy((char *)lbuf, (char *)&p[1], SBSIZE) >= SBSIZE)
		error("line exceeded buffer size");

	ifiles->fname = lbuf;
	if (yylex() == '\n')
		return;

bad:	error("bad line directive");
}

/*
 * Search for and include next file.
 * Return 1 on success.
 */
static int
fsrch(const usch *fn, int idx, struct incs *w)
{
	int i;

	for (i = idx; i < 2; i++) {
		if (i > idx)
			w = incdir[i];
		for (; w; w = w->next) {
			usch *nm = stringbuf;

			savstr(w->dir); savch('/');
			savstr(fn); savch(0);
			if (pushfile(nm, fn, i, w->next) == 0)
				return 1;
			stringbuf = nm;
		}
	}
	return 0;
}

/*
 * Include a file. Include order:
 * - For <...> files, first search -I directories, then system directories.
 * - For "..." files, first search "current" dir, then as <...> files.
 */
void
include()
{
	struct symtab *nl;
	usch *osp;
	usch *fn, *safefn;
	int c, it;

	if (flslvl)
		return;
	osp = stringbuf;

	while ((c = sloscan()) == WSPACE)
		;
	if (c == IDENT) {
		/* sloscan() will not expand idents */
		if ((nl = lookup((usch *)yytext, FIND)) == NULL)
			goto bad;
		unpstr(gotident(nl));
		stringbuf = osp;
		c = yylex();
	}
	if (c != STRING && c != '<')
		goto bad;

	if (c == '<') {
		fn = stringbuf;
		while ((c = sloscan()) != '>' && c != '\n') {
			if (c == '\n') /* XXX check - cannot reach */
				goto bad;
			savstr((usch *)yytext);
		}
		savch('\0');
		while ((c = sloscan()) == WSPACE)
			;
		if (c != '\n')
			goto bad;
		it = SYSINC;
		safefn = fn;
	} else {
		usch *nm = stringbuf;

		yytext[strlen(yytext)-1] = 0;
		fn = (usch *)&yytext[1];
		/* first try to open file relative to previous file */
		/* but only if it is not an absolute path */
		if (*fn != '/') {
			savstr(ifiles->orgfn);
			if ((stringbuf =
			    (usch *)strrchr((char *)nm, '/')) == NULL)
				stringbuf = nm;
			else
				stringbuf++;
		}
		safefn = stringbuf;
		savstr(fn); savch(0);
		c = yylex();
		if (c != '\n')
			goto bad;
		if (pushfile(nm, safefn, 0, NULL) == 0)
			goto okret;
		/* XXX may loose stringbuf space */
	}

	if (fsrch(safefn, 0, incdir[0]))
		goto okret;

	error("cannot find '%s'", safefn);
	/* error() do not return */

bad:	error("bad include");
	/* error() do not return */
okret:
	prtline();
}

void
include_next()
{
	struct symtab *nl;
	usch *osp;
	usch *fn;
	int c;

	if (flslvl)
		return;
	osp = stringbuf;
	while ((c = sloscan()) == WSPACE)
		;
	if (c == IDENT) {
		/* sloscan() will not expand idents */
		if ((nl = lookup((usch *)yytext, FIND)) == NULL)
			goto bad;
		unpstr(gotident(nl));
		stringbuf = osp;
		c = yylex();
	}
	if (c != STRING && c != '<')
		goto bad;

	fn = stringbuf;
	if (c == STRING) {
		savstr((usch *)&yytext[1]);
		stringbuf[-1] = 0;
	} else { /* < > */
		while ((c = sloscan()) != '>') {
			if (c == '\n')
				goto bad;
			savstr((usch *)yytext);
		}
		savch('\0');
	}
	while ((c = sloscan()) == WSPACE)
		;
	if (c != '\n')
		goto bad;

	if (fsrch(fn, ifiles->idx, ifiles->incs) == 0)
		error("cannot find '%s'", fn);
	prtline();
	return;

bad:	error("bad include");
	/* error() do not return */
}

static int
definp(void)
{
	int c;

	do
		c = sloscan();
	while (c == WSPACE);
	return c;
}

void
getcmnt(void)
{
	int c;

	savstr((usch *)yytext);
	savch(cinput()); /* Lost * */
	for (;;) {
		c = cinput();
		if (c == '*') {
			c = cinput();
			if (c == '/') {
				savstr((const usch *)"*/");
				return;
			}
			cunput(c);
			c = '*';
		}
		savch(c);
	}
}

/*
 * Compare two replacement lists, taking in account comments etc.
 */
static int
cmprepl(const usch *o, const usch *n)
{
	for (; *o; o--, n--) {
		/* comment skip */
		if (*o == '/' && o[-1] == '*') {
			while (*o != '*' || o[-1] != '/')
				o--;
			o -= 2;
		}
		if (*n == '/' && n[-1] == '*') {
			while (*n != '*' || n[-1] != '/')
				n--;
			n -= 2;
		}
		while (*o == ' ' || *o == '\t')
			o--;
		while (*n == ' ' || *n == '\t')
			n--;
		if (*o != *n)
			return 1;
	}
	return 0;
}

static int
isell(void)
{
	int ch;

	if ((ch = cinput()) != '.') {
		cunput(ch);
		return 0;
	}
	if ((ch = cinput()) != '.') {
		cunput(ch);
		cunput('.');
		return 0;
	}
	return 1;
}

void
define()
{
	struct symtab *np;
	usch *args[MAXARG], *ubuf, *sbeg;
	int c, i, redef;
	int mkstr = 0, narg = -1;
	int ellips = 0;
#ifdef GCC_VARI
	usch *gccvari = NULL;
	int wascon;
#endif

	if (flslvl)
		return;
	if (sloscan() != WSPACE || sloscan() != IDENT)
		goto bad;

	if (isdigit((int)yytext[0]))
		goto bad;

	np = lookup((usch *)yytext, ENTER);
	redef = np->value != NULL;

	readmac = 1;
	sbeg = stringbuf;
	if ((c = sloscan()) == '(') {
		narg = 0;
		/* function-like macros, deal with identifiers */
		c = definp();
		for (;;) {
			if (c == ')')
				break;
			if (c == '.' && isell()) {
				ellips = 1;
				if (definp() != ')')
					goto bad;
				break;
			}
			if (c == IDENT) {
				/* make sure there is no arg of same name */
				for (i = 0; i < narg; i++)
					if (!strcmp((char *) args[i], yytext))
						error("Duplicate macro "
						  "parameter \"%s\"", yytext);
				args[narg++] = xstrdup(yytext);
				if ((c = definp()) == ',') {
					if ((c = definp()) == ')')
						goto bad;
					continue;
				}
#ifdef GCC_VARI
				if (c == '.' && isell()) {
					if (definp() != ')')
						goto bad;
					gccvari = args[--narg];
					break;
				}
#endif
				if (c == ')')
					break;
			}
			goto bad;
		}
		c = sloscan();
	} else if (c == '\n') {
		/* #define foo */
		;
	} else if (c != WSPACE)
		goto bad;

	while (c == WSPACE)
		c = sloscan();

	/* replacement list cannot start with ## operator */
	if (c == '#') {
		if ((c = sloscan()) == '#')
			goto bad;
		savch('\0');
#ifdef GCC_VARI
		wascon = 0;
#endif
		goto in2;
	}

	/* parse replacement-list, substituting arguments */
	savch('\0');
	while (c != '\n') {
#ifdef GCC_VARI
		wascon = 0;
loop:
#endif
		switch (c) {
		case WSPACE:
			/* remove spaces if it surrounds a ## directive */
			ubuf = stringbuf;
			savstr((usch *)yytext);
			c = sloscan();
			if (c == '#') {
				if ((c = sloscan()) != '#')
					goto in2;
				stringbuf = ubuf;
				savch(CONC);
				if ((c = sloscan()) == WSPACE)
					c = sloscan();
#ifdef GCC_VARI
				if (c == '\n')
					break;
				wascon = 1;
				goto loop;
#endif
			}
			continue;

		case '#':
			c = sloscan();
			if (c == '#') {
				/* concat op */
				savch(CONC);
				if ((c = sloscan()) == WSPACE)
					c = sloscan();
#ifdef GCC_VARI
				if (c == '\n')
					break;
				wascon = 1;
				goto loop;
#else
				continue;
#endif
			} 
in2:			if (narg < 0) {
				/* no meaning in object-type macro */
				savch('#');
				continue;
			}
			/* remove spaces between # and arg */
			savch(SNUFF);
			if (c == WSPACE)
				c = sloscan(); /* whitespace, ignore */
			mkstr = 1;
			if (c == IDENT && strcmp(yytext, "__VA_ARGS__") == 0)
				continue;

			/* FALLTHROUGH */
		case IDENT:
			if (strcmp(yytext, "__VA_ARGS__") == 0) {
				if (ellips == 0)
					error("unwanted %s", yytext);
				savch(VARG);
				savch(WARN);
				if (mkstr)
					savch(SNUFF), mkstr = 0;
				break;
			}
			if (narg < 0)
				goto id; /* just add it if object */
			/* check if its an argument */
			for (i = 0; i < narg; i++)
				if (strcmp(yytext, (char *)args[i]) == 0)
					break;
			if (i == narg) {
#ifdef GCC_VARI
				if (gccvari &&
				    strcmp(yytext, (char *)gccvari) == 0) {
					savch(wascon ? GCCARG : VARG);
					savch(WARN);
					if (mkstr)
						savch(SNUFF), mkstr = 0;
					break;
				}
#endif
				if (mkstr)
					error("not argument");
				goto id;
			}
			savch(i);
			savch(WARN);
			if (mkstr)
				savch(SNUFF), mkstr = 0;
			break;

		case CMNT: /* save comments */
			getcmnt();
			break;

		default:
id:			savstr((usch *)yytext);
			break;
		}
		c = sloscan();
	}
	readmac = 0;
	/* remove trailing whitespace */
	while (stringbuf > sbeg) {
		if (stringbuf[-1] == ' ' || stringbuf[-1] == '\t')
			stringbuf--;
		/* replacement list cannot end with ## operator */
		else if (stringbuf[-1] == CONC)
			goto bad;
		else
			break;
	}
#ifdef GCC_VARI
	if (gccvari) {
		savch(narg);
		savch(VARG);
	} else
#endif
	if (ellips) {
		savch(narg);
		savch(VARG);
	} else
		savch(narg < 0 ? OBJCT : narg);
	if (redef) {
		if (cmprepl(np->value, stringbuf-1))
			error("%s redefined\nprevious define: %s:%d",
			    np->namep, np->file, np->line);
		stringbuf = sbeg;  /* forget this space */
	} else
		np->value = stringbuf-1;

#ifdef CPP_DEBUG
	if (dflag) {
		const usch *w = np->value;

		printf("!define: ");
		if (*w == OBJCT)
			printf("[object]");
		else if (*w == VARG)
			printf("[VARG%d]", *--w);
		while (*--w) {
			switch (*w) {
			case WARN: printf("<%d>", *--w); break;
			case CONC: printf("<##>"); break;
			case SNUFF: printf("<\">"); break;
			default: putchar(*w); break;
			}
		}
		putchar('\n');
	}
#endif
	for (i = 0; i < narg; i++)
		free(args[i]);
	return;

bad:	error("bad define");
}

void
xwarning(usch *s)
{
	usch *t;
	usch *sb = stringbuf;
	int dummy;

	flbuf();
	savch(0);
	if (ifiles != NULL) {
		t = sheap("%s:%d: warning: ", ifiles->fname, ifiles->lineno);
		write (2, t, strlen((char *)t));
	}
	dummy = write (2, s, strlen((char *)s));
	dummy = write (2, "\n", 1);
	stringbuf = sb;
}

void
xerror(usch *s)
{
	usch *t;
	int dummy;

	flbuf();
	savch(0);
	if (ifiles != NULL) {
		t = sheap("%s:%d: error: ", ifiles->fname, ifiles->lineno);
		dummy = write (2, t, strlen((char *)t));
	}
	dummy = write (2, s, strlen((char *)s));
	dummy = write (2, "\n", 1);
	exit(1);
}

/*
 * store a character into the "define" buffer.
 */
void
savch(int c)
{
	if (stringbuf-sbf < SBSIZE) {
		*stringbuf++ = (usch)c;
	} else {
		stringbuf = sbf; /* need space to write error message */
		error("Too much defining");
	} 
}

/*
 * convert _Pragma to #pragma for output.
 */
static void
pragoper(void)
{
	usch *opb;
	int t, plev;

	if ((t = sloscan()) == WSPACE)
		t = sloscan();
	if (t != '(')
		goto bad;
	if ((t = sloscan()) == WSPACE)
		t = sloscan();
	opb = stringbuf;
	for (plev = 0; ; t = sloscan()) {
		if (t == '(')
			plev++;
		if (t == ')')
			plev--;
		if (plev < 0)
			break;
		savstr((usch *)yytext);
	}

	savch(0);
	cunput(WARN);
	unpstr(opb);
	stringbuf = opb;
	expmac(NULL);
	cunput('\n');
	while (stringbuf > opb)
		cunput(*--stringbuf);
	savch(PRAGS);
	while ((t = sloscan()) != '\n') {
		if (t == WSPACE)
			continue;
		if (t != STRING)
			goto bad;
		savstr((usch *)yytext);
	}

	savch(PRAGE);
	while (stringbuf > opb)
		cunput(*--stringbuf);
	return;
bad:	error("bad pragma operator");
}

/*
 * substitute namep for sp->value.
 */
int
subst(struct symtab *sp, struct recur *rp)
{
	struct recur rp2;
	register const usch *vp, *cp;
	register usch *obp;
	int c, nl;

	DPRINT(("subst: %s\n", sp->namep));
	/*
	 * First check for special macros.
	 */
	if (sp == filloc) {
		(void)sheap("\"%s\"", ifiles->fname);
		return 1;
	} else if (sp == linloc) {
		(void)sheap("%d", ifiles->lineno);
		return 1;
	} else if (sp == pragloc) {
		pragoper();
		return 1;
	}
	vp = sp->value;

	rp2.next = rp;
	rp2.sp = sp;

	if (*vp-- != OBJCT) {
		int gotwarn = 0;

		/* should we be here at all? */
		/* check if identifier is followed by parentheses */

		obp = stringbuf;
		nl = 0;
		do {
			c = cinput();
			*stringbuf++ = (usch)c;
			if (c == WARN) {
				gotwarn++;
				if (rp == NULL)
					break;
			}
			if (c == '\n')
				nl++;
		} while (c == ' ' || c == '\t' || c == '\n' || 
			    c == '\r' || c == WARN);

		DPRINT(("c %d\n", c));
		if (c == '(' ) {
			cunput(c);
			stringbuf = obp;
			ifiles->lineno += nl;
			expdef(vp, &rp2, gotwarn);
			return 1;
		} else {
	 		*stringbuf = 0;
			unpstr(obp);
			unpstr(sp->namep);
			if ((c = sloscan()) != IDENT)
				error("internal sync error");
			stringbuf = obp;
			return 0;
		}
	} else {
		cunput(WARN);
		cp = vp;
		while (*cp) {
			if (*cp != CONC)
				cunput(*cp);
			cp--;
		}
		expmac(&rp2);
	}
	return 1;
}

/*
 * Maybe an indentifier (for macro expansion).
 */
static int
mayid(usch *s)
{
	for (; *s; s++)
		if (!isdigit(*s) && !isalpha(*s) && *s != '_')
			return 0;
	return 1;
}

/*
 * do macro-expansion until WARN character read.
 * read from lex buffer and store result on heap.
 * will recurse into lookup() for recursive expansion.
 * when returning all expansions on the token list is done.
 */
void
expmac(struct recur *rp)
{
	struct symtab *nl;
	int c, noexp = 0, orgexp;
	usch *och, *stksv;

#ifdef CPP_DEBUG
	if (dflag) {
		struct recur *rp2 = rp;
		printf("\nexpmac\n");
		while (rp2) {
			printf("do not expand %s\n", rp2->sp->namep);
			rp2 = rp2->next;
		}
	}
#endif
	readmac++;
	while ((c = sloscan()) != WARN) {
		switch (c) {
		case NOEXP: noexp++; break;
		case EXPAND: noexp--; break;

		case NUMBER: /* handled as ident if no .+- in it */
			if (!mayid((usch *)yytext))
				goto def;
			/* FALLTHROUGH */
		case IDENT:
			/*
			 * Handle argument concatenation here.
			 * If an identifier is found and directly 
			 * after EXPAND or NOEXP then push the
			 * identifier back on the input stream and
			 * call sloscan() again.
			 * Be careful to keep the noexp balance.
			 */
			och = stringbuf;
			savstr((usch *)yytext);
			DDPRINT(("id: str %s\n", och));

			orgexp = 0;
			while ((c = sloscan()) == EXPAND || c == NOEXP)
				if (c == EXPAND)
					orgexp--;
				else
					orgexp++;

			DDPRINT(("id1: typ %d noexp %d orgexp %d\n",
			    c, noexp, orgexp));
			if (c == IDENT ||
			    (c == NUMBER && mayid((usch *)yytext))) {
				DDPRINT(("id2: str %s\n", yytext));
				/* OK to always expand here? */
				savstr((usch *)yytext);
				switch (orgexp) {
				case 0: /* been EXP+NOEXP */
					if (noexp == 0)
						break;
					if (noexp != 1)
						error("case 0");
					cunput(NOEXP);
					noexp = 0;
					break;
				case -1: /* been EXP */
					if (noexp != 1)
						error("case -1");
					noexp = 0;
					break;
				case 1:
					if (noexp != 0)
						error("case 1");
					cunput(NOEXP);
					break;
				default:
					error("orgexp = %d", orgexp);
				}
				unpstr(och);
				stringbuf = och;
				continue; /* New longer identifier */
			}
			unpstr((const usch *)yytext);
			if (orgexp == -1)
				cunput(EXPAND);
			else if (orgexp == -2)
				cunput(EXPAND), cunput(EXPAND);
			else if (orgexp == 1)
				cunput(NOEXP);
			unpstr(och);
			stringbuf = och;


			sloscan(); /* XXX reget last identifier */

			if ((nl = lookup((usch *)yytext, FIND)) == NULL)
				goto def;

			if (canexpand(rp, nl) == 0)
				goto def;
			/*
			 * If noexp == 0 then expansion of any macro is 
			 * allowed.  If noexp == 1 then expansion of a
			 * fun-like macro is allowed iff there is an 
			 * EXPAND between the identifier and the '('.
			 */
			if (noexp == 0) {
				if ((c = subst(nl, rp)) == 0)
					goto def;
				break;
			}
			if (noexp != 1)
				error("bad noexp %d", noexp);
			stksv = NULL;
			if ((c = sloscan()) == WSPACE) {
				stksv = xstrdup(yytext);
				c = sloscan();
			}
			/* only valid for expansion if fun macro */
			if (c == EXPAND && *nl->value != OBJCT) {
				noexp--;
				if (subst(nl, rp))
					break;
				savstr(nl->namep);
				if (stksv)
					savstr(stksv);
			} else {
				unpstr((const usch *)yytext);
				if (stksv)
					unpstr(stksv);
				savstr(nl->namep);
			}
			if (stksv)
				free(stksv);
			break;

		case CMNT:
			getcmnt();
			break;

		case '\n':
			cinput();
			savch(' ');
			break;

		case STRING:
			/* remove EXPAND/NOEXP from strings */
			if (yytext[1] == NOEXP) {
				savch('"');
				och = (usch *)&yytext[2];
				while (*och != EXPAND)
					savch(*och++);
				savch('"');
				break;
			}
			/* FALLTHROUGH */

def:		default:
			savstr((usch *)yytext);
			break;
		}
	}
	if (noexp)
		error("expmac noexp=%d", noexp);
	readmac--;
	DPRINT(("return from expmac\n"));
}

/*
 * expand a function-like macro.
 * vp points to end of replacement-list
 * reads function arguments from sloscan()
 * result is written on top of heap
 */
void
expdef(const usch *vp, struct recur *rp, int gotwarn)
{
	const usch **args, *ap, *bp, *sp;
	usch *sptr;
	int narg, c, i, plev, snuff, instr;
	int ellips = 0, shot = gotwarn;

	DPRINT(("expdef rp %s\n", (rp ? (const char *)rp->sp->namep : "")));
	if ((c = sloscan()) != '(')
		error("got %c, expected (", c);
	if (vp[1] == VARG) {
		narg = *vp--;
		ellips = 1;
	} else
		narg = vp[1];
	if ((args = malloc(sizeof(usch *) * (narg+ellips))) == NULL)
		error("expdef: out of mem");

	/*
	 * read arguments and store them on heap.
	 * will be removed just before return from this function.
	 */
	sptr = stringbuf;
	instr = 0;
	for (i = 0; i < narg && c != ')'; i++) {
		args[i] = stringbuf;
		plev = 0;
		while ((c = sloscan()) == WSPACE || c == '\n')
			if (c == '\n')
				putch(cinput());
		DDPRINT((":AAA (%d)", c));
		if (instr == -1)
			savch(NOEXP), instr = 1;
		if (c == NOEXP)
			instr = 1;
		for (;;) {
			if (plev == 0 && (c == ')' || c == ','))
				break;
			if (c == '(')
				plev++;
			if (c == ')')
				plev--;
			savstr((usch *)yytext);
			while ((c = sloscan()) == '\n') {
				putch(cinput());
				savch('\n');
			}
			while (c == CMNT) {
				getcmnt();
				c = sloscan();
			}
			if (c == EXPAND)
				instr = 0;
			if (c == 0)
				error("eof in macro");
		}
		while (args[i] < stringbuf &&
		    (stringbuf[-1] == ' ' || stringbuf[-1] == '\t'))
			stringbuf--;
		if (instr == 1)
			savch(EXPAND), instr = -1;
		savch('\0');
	}
	if (ellips)
		args[i] = (const usch *)"";
	if (ellips && c != ')') {
		args[i] = stringbuf;
		plev = 0;
		instr = 0;
		while ((c = sloscan()) == WSPACE)
			;
		if (c == NOEXP)
			instr++;
		DDPRINT((":AAY (%d)", c));
		for (;;) {
			if (plev == 0 && c == ')')
				break;
			if (c == '(')
				plev++;
			if (c == ')')
				plev--;
			if (plev == 0 && c == ',' && instr) {
				savch(EXPAND);
				savch(',');
				savch(NOEXP);
			} else
				savstr((usch *)yytext);
			while ((c = sloscan()) == '\n') {
				cinput();
				savch('\n');
			}
			if (c == EXPAND)
				instr--;
		}
		while (args[i] < stringbuf &&
		    (stringbuf[-1] == ' ' || stringbuf[-1] == '\t'))
			stringbuf--;
		savch('\0');
		
	}
	if (narg == 0 && ellips == 0)
		while ((c = sloscan()) == WSPACE || c == '\n')
			if (c == '\n')
				cinput();

	if (c != ')' || (i != narg && ellips == 0) || (i < narg && ellips == 1))
		error("wrong arg count");

	while (gotwarn--)
		cunput(WARN);

	sp = vp;
	instr = snuff = 0;

	/*
	 * push-back replacement-list onto lex buffer while replacing
	 * arguments. 
	 */
	cunput(WARN);
	while (*sp != 0) {
		if (*sp == SNUFF)
			cunput('\"'), snuff ^= 1;
		else if (*sp == CONC)
			;
		else if (*sp == WARN) {

			if (sp[-1] == VARG) {
				bp = ap = args[narg];
				sp--;
#ifdef GCC_VARI
			} else if (sp[-1] == GCCARG) {
				ap = args[narg];
				if (ap[0] == 0)
					ap = (const usch *)"0";
				bp = ap;
				sp--;
#endif
			} else
				bp = ap = args[(int)*--sp];
			if (sp[2] != CONC && !snuff && sp[-1] != CONC) {
				struct recur *r2 = rp->next;
				cunput(WARN);
				while (*bp)
					bp++;
				while (bp > ap)
					cunput(*--bp);
				DPRINT(("expand arg %d string %s\n", *sp, ap));
				bp = ap = stringbuf;
				savch(NOEXP);
				while (shot && r2)
					r2 = r2->next, shot--;
				expmac(r2);
				savch(EXPAND);
				savch('\0');
			}
			while (*bp)
				bp++;
			while (bp > ap) {
				bp--;
				if (snuff && !instr && 
				    (*bp == ' ' || *bp == '\t' || *bp == '\n')){
					while (*bp == ' ' || *bp == '\t' ||
					    *bp == '\n') {
						bp--;
					}
					cunput(' ');
				}
				cunput(*bp);
				if ((*bp == '\'' || *bp == '"')
				     && bp[-1] != '\\' && snuff) {
					instr ^= 1;
					if (instr == 0 && *bp == '"')
						cunput('\\');
				}
				if (instr && (*bp == '\\' || *bp == '"'))
					cunput('\\');
			}
		} else
			cunput(*sp);
		sp--;
	}
	stringbuf = sptr;

	/* scan the input buffer (until WARN) and save result on heap */
	expmac(rp);
	free(args);
}

usch *
savstr(const usch *str)
{
	usch *rv = stringbuf;

	do {
		if (stringbuf >= &sbf[SBSIZE])   {
			stringbuf = sbf; /* need space to write error message */
			error("out of macro space!");
		}
	} while ((*stringbuf++ = *str++));
	stringbuf--;
	return rv;
}

int
canexpand(struct recur *rp, struct symtab *np)
{
	struct recur *w;

	for (w = rp; w && w->sp != np; w = w->next)
		;
	if (w != NULL)
		return 0;
	return 1;
}

void
unpstr(const usch *c)
{
	const usch *d = c;

	while (*d)
		d++;
	while (d > c) {
		cunput(*--d);
	}
}

void
flbuf()
{
	if (obufp == 0)
		return;
	if (Mflag == 0 && write(ofd, outbuf, obufp) < 0)
		error("obuf write error");
	obufp = 0;
}

void
putch(int ch)
{
	outbuf[obufp++] = (usch)ch;
	if (obufp == CPPBUF || (istty && ch == '\n'))
		flbuf();
}

void
putstr(const usch *s)
{
	for (; *s; s++) {
		if (*s == PRAGS) {
			s = prtprag(s);
			continue;
		}
		outbuf[obufp++] = *s;
		if (obufp == CPPBUF || (istty && *s == '\n'))
			flbuf();
	}
}

/*
 * convert a number to an ascii string. Store it on the heap.
 */
static void
num2str(int num)
{
	static usch buf[12];
	usch *b = buf;
	int m = 0;
	
	if (num < 0)
		num = -num, m = 1;
	do {
		*b++ = (usch)(num % 10 + '0');
		num /= 10;
	} while (num);
	if (m)
		*b++ = '-';
	while (b > buf)
		savch(*--b);
}

/*
 * similar to sprintf, but only handles %s and %d. 
 * saves result on heap.
 */
usch *
sheap(const char *fmt, ...)
{
	va_list ap;
	usch *op = stringbuf;

	va_start(ap, fmt);
	for (; *fmt; fmt++) {
		if (*fmt == '%') {
			fmt++;
			switch (*fmt) {
			case 's':
				savstr(va_arg(ap, usch *));
				break;
			case 'd':
				num2str(va_arg(ap, int));
				break;
			case 'c':
				savch(va_arg(ap, int));
				break;
			default:
				break; /* cannot call error() here */
			}
		} else
			savch(*fmt);
	}
	va_end(ap);
	*stringbuf = 0;
	return op;
}

void
usage()
{
	error("Usage: cpp [-Cdt] [-Dvar=val] [-Uvar] [-Ipath] [-Spath]");
}

#ifdef notyet
/*
 * Symbol table stuff.
 * The data structure used is a patricia tree implementation using only
 * bytes to store offsets.  
 * The information stored is (lower address to higher):
 *
 *	unsigned char bitno[2]; bit number in the string
 *	unsigned char left[3];	offset from base to left element
 *	unsigned char right[3];	offset from base to right element
 */
#endif

/*
 * This patricia implementation is more-or-less the same as
 * used in ccom for string matching.
 */
struct tree {
	int bitno;
	struct tree *lr[2];
};

#define BITNO(x)		((x) & ~(LEFT_IS_LEAF|RIGHT_IS_LEAF))
#define LEFT_IS_LEAF		0x80000000
#define RIGHT_IS_LEAF		0x40000000
#define IS_LEFT_LEAF(x)		(((x) & LEFT_IS_LEAF) != 0)
#define IS_RIGHT_LEAF(x)	(((x) & RIGHT_IS_LEAF) != 0)
#define P_BIT(key, bit)		(key[bit >> 3] >> (bit & 7)) & 1
#define CHECKBITS		8

static struct tree *sympole;
static int numsyms;

/*
 * Allocate a symtab struct and store the string.
 */
static struct symtab *
getsymtab(const usch *str)
{
	struct symtab *sp = malloc(sizeof(struct symtab));

	if (sp == NULL)
		error("getsymtab: couldn't allocate symtab");
	sp->namep = savstr(str);
	savch('\0');
	sp->value = NULL;
	sp->file = ifiles ? ifiles->orgfn : (const usch *)"<initial>";
	sp->line = ifiles ? ifiles->lineno : 0;
	return sp;
}

/*
 * Do symbol lookup in a patricia tree.
 * Only do full string matching, no pointer optimisations.
 */
struct symtab *
lookup(const usch *key, int enterf)
{
	struct symtab *sp;
	struct tree *w, *new, *last;
	int len, cix, bit, fbit, svbit, ix, bitno;
	const usch *k, *m, *sm;

	/* Count full string length */
	for (k = key, len = 0; *k; k++, len++)
		;

	switch (numsyms) {
	case 0: /* no symbols yet */
		if (enterf != ENTER)
			return NULL;
		sympole = (struct tree *)getsymtab(key);
		numsyms++;
		return (struct symtab *)sympole;

	case 1:
		w = sympole;
		svbit = 0; /* XXX gcc */
		break;

	default:
		w = sympole;
		bitno = len * CHECKBITS;
		for (;;) {
			bit = BITNO(w->bitno);
			fbit = bit > bitno ? 0 : P_BIT(key, bit);
			svbit = fbit ? IS_RIGHT_LEAF(w->bitno) :
			    IS_LEFT_LEAF(w->bitno);
			w = w->lr[fbit];
			if (svbit)
				break;
		}
	}

	sp = (struct symtab *)w;

	sm = m = sp->namep;
	k = key;

	/* Check for correct string and return */
	for (cix = 0; *m && *k && *m == *k; m++, k++, cix += CHECKBITS)
		;
	if (*m == 0 && *k == 0) {
		if (enterf != ENTER && sp->value == NULL)
			return NULL;
		return sp;
	}

	if (enterf != ENTER)
		return NULL; /* no string found and do not enter */

	ix = *m ^ *k;
	while ((ix & 1) == 0)
		ix >>= 1, cix++;

	/* Create new node */
	if ((new = malloc(sizeof *new)) == NULL)
		error("getree: couldn't allocate tree");
	bit = P_BIT(key, cix);
	new->bitno = cix | (bit ? RIGHT_IS_LEAF : LEFT_IS_LEAF);
	new->lr[bit] = (struct tree *)getsymtab(key);

	if (numsyms++ == 1) {
		new->lr[!bit] = sympole;
		new->bitno |= (bit ? LEFT_IS_LEAF : RIGHT_IS_LEAF);
		sympole = new;
		return (struct symtab *)new->lr[bit];
	}

	w = sympole;
	last = NULL;
	for (;;) {
		fbit = w->bitno;
		bitno = BITNO(w->bitno);
		if (bitno == cix)
			error("bitno == cix");
		if (bitno > cix)
			break;
		svbit = P_BIT(key, bitno);
		last = w;
		w = w->lr[svbit];
		if (fbit & (svbit ? RIGHT_IS_LEAF : LEFT_IS_LEAF))
			break;
	}

	new->lr[!bit] = w;
	if (last == NULL) {
		sympole = new;
	} else {
		last->lr[svbit] = new;
		last->bitno &= ~(svbit ? RIGHT_IS_LEAF : LEFT_IS_LEAF);
	}
	if (bitno < cix)
		new->bitno |= (bit ? LEFT_IS_LEAF : RIGHT_IS_LEAF);
	return (struct symtab *)new->lr[bit];
}

usch *
xstrdup(const char *str)
{
	size_t len = strlen(str)+1;
	usch *rv;

	if ((rv = malloc(len)) == NULL)
		error("xstrdup: out of mem");
	strlcpy((char *)rv, str, len);
	return rv;
}

const usch *
prtprag(const usch *s)
{
	int ch;

	s++;
	putstr((const usch *)"\n#pragma ");
	while (*s != PRAGE) {
		if (*s == 'L')
			s++;
		if (*s == '\"') {
			s++;
			while ((ch = *s++) != '\"') {
				if (ch == '\\' && (*s == '\"' || *s == '\\'))
					ch = *s++;
				putch(ch);
			}
		} else {
			s++;
			putch(*s);
		}
	}
	putstr((const usch *)"\n");
	prtline();
	return ++s;
}
