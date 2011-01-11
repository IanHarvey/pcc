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

/* args for lookup() */
#define	FIND	0
#define	ENTER	1

static int readargs(struct symtab *sp, const usch **args);
void prline(const usch *s);
static void prrep(const usch *s);
static void exparg(const usch *bp, int);
static void subarg(struct symtab *sp, const usch **args, int);
void define(void);
void include(void);
void include_next(void);
void line(void);
void flbuf(void);
void usage(void);
usch *xstrdup(const char *str);
static void addidir(char *idir, struct incs **ww);
void imp(const char *);
#define IMP(x) if (dflag>1) imp(x)

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
	filloc->value = linloc->value = stringbuf;
	savch(OBJCT);

	/* create a complete macro for pragma */
	pragloc = lookup((const usch *)"_Pragma", ENTER);
	savch(0);
	savstr("_Pragma(");
	savch(0);
	savch(WARN);
	savch(')');
	pragloc->value = stringbuf;
	savch(1);

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
		if (kfind(nl))
			unpstr(stringbuf);
		else
			unpstr(nl->namep);
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
		if (kfind(nl))
			unpstr(stringbuf);
		else
			unpstr(nl->namep);
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
	usch *args[MAXARGS+1], *ubuf, *sbeg;
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
				if (narg == MAXARGS)
					error("Too many macro args");
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

static int
addmac(struct symtab *sp)
{
	int c, i;

	/* Check if it exists; then save some space */
	/* May be more difficult to debug cpp */
	for (i = 1; i < norepptr; i++)
		if (norep[i] == sp)
			return i;
	if ((c = norepptr) == RECMAX)
		error("too many macros");
	norep[norepptr++] = sp;
	return c;
}

/* Allow next nr in lex buffer to expand */
int
doexp(void)
{
	int i, n = cinput();
	DDPRINT(("doexp %s(%d) blocking:", norep[n]->namep, n));
	if (n != bptr[--bidx])
		error("expansion sync error");
	if (dflag>1) {
		for (i = bidx-1; i >= 0; i--)
			printf(" '%s'", norep[bptr[i]]->namep);
		printf("\n");
	}
	return n;
}

/* Block next nr in lex buffer to expand */
int
donex(void)
{
	int n, i;

	if (bidx == RECMAX)
		error("too deep macro recursion");
	bptr[bidx++] = n = cinput();
	/* XXX - check for sp buffer overflow */
	if (dflag>1) {
		printf("donex %d (%d) blocking:\n", bidx, n);
		printf("donex %s(%d) blocking:", norep[n]->namep, n);
		for (i = bidx-1; i >= 0; i--)
			printf(" '%s'", norep[bptr[i]]->namep);
		printf("\n");
	}
	return n;
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
 * Syntax is already correct.
 */
static void
pragoper(void)
{
	usch *s;
	int t;

	while ((t = sloscan()) != '(') {
		if (t == EXP)
			doexp();
		else if (t == NEX)
			donex();
	}

	while ((t = sloscan()) == WSPACE)
		;
	if (t != STRING)
		error("pragma must have string argument");
	savstr((const usch *)"\n#pragma ");
	s = (usch *)yytext;
	if (*s == 'L')
		s++;
	for (; *s; s++) {
		if (*s == '\"')
			continue;
		if (*s == '\\' && (s[1] == '\"' || s[1] == '\\'))
			s++;
		savch(*s);
	}
	sheap("\n# %d \"%s\"\n", ifiles->lineno, ifiles->fname);
	while ((t = sloscan()) == WSPACE)
		;
	if (t != ')')
		error("pragma syntax error");
}

/*
 * Return true if it is OK to expand this symbol.
 */
static int
okexp(struct symtab *sp)
{
	int i;

	if (sp == NULL)
		return 0;
	for (i = 0; i < bidx; i++)
		if (norep[bptr[i]] == sp)
			return 0;
	return 1;
}

/*
 * Handle defined macro keywords found on input stream.
 * When finished print out the full expanded line.
 */
int
kfind(struct symtab *sp)
{
	struct symtab *nl;
	const usch *argary[MAXARGS+1];
	usch *bp, *sbp;
	int c;

	DPRINT(("%d:enter kfind(%s)\n",0,sp->namep));
	IMP("KFIND");
	if (*sp->value == OBJCT) {
		cunput(WARN);
		exparg(sp->namep, 1);
upp:		sbp = bp = stringbuf;
		unpstr(bp);
		IMP("ENDX");
		while ((c = sloscan()) != WARN) {
			switch (c) {
			case NEX:
				donex();
				break;
			case EXP:
				doexp();
				break;
			case STRING:
				/* Remove embedded directives */
				for (bp = (usch *)yytext; *bp; bp++) {
					if (*bp == EXP || *bp == NEX)
						bp++;
					else if (*bp != CONC)
						savch(*bp);
				}
				break;

			case IDENT:
				/*
				 * Tricky: if this is the last identifier
				 * in the expanded list, and it is defined
				 * as a function-like macro, then push it 
				 * back on the input stream and let fastscan
				 * handle it as a new macro.
				 * BUT: if this macro is blocked then this
				 * should not me done.
				 */
				nl = lookup((usch *)yytext, FIND);
				/* Deal with pragmas here */
				if (nl == pragloc) {
					pragoper();
					break;
				}
				if (nl == NULL || !okexp(nl) ||
				    *nl->value == OBJCT) {
					/* Not fun-like macro */
					savstr((usch *)yytext);
					break;
				}
				while ((c = cinput()) == NEX || c == EXP)
					c == EXP ? doexp() : donex();
				if (c == WARN) {
					/* succeeded, push back */
					unpstr((usch *)yytext);
				} else {
					savstr((usch *)yytext);
				}
				cunput(c);
				break;

			default:
				savstr((usch *)yytext);
				break;
			}
		}
		IMP("END2");
		if (bidx != 0)
			error("exp/noexp sync error");
		norepptr = 1;
		savch(0);
		stringbuf = sbp;
		return 1;
	}
	/* Is a function-like macro */

	/* Search for '(' */
	bp = stringbuf;
	while (iswsnl(c = cinput()))
		savch(c);
	savch(0);
	stringbuf = bp;
	if (c != '(') {
		cunput(c);
		unpstr(bp);
		return 0; /* Failed */
	}

	/* Found one, output \n to be in sync */
	for (; *bp; bp++) {
		if (*bp == '\n')
			putch('\n'), ifiles->lineno++;
	}

	/* fetch arguments */
	if (readargs(sp, argary))
		error("readargs");

	c = addmac(sp);
	bp = stringbuf;
	cunput(WARN);
	cunput(WARN);
	cunput(c);
	cunput(EXP);
	IMP("KEXP");

	subarg(sp, argary, 1);

	cunput(c);
	cunput(NEX);
	IMP("KNEX");

	stringbuf = bp;

	IMP("MID1");

	while ((c = cinput()) != WARN) {
		savch(c);
		if (c == EXP || c == NEX)
			savch(cinput());
	}
	savch(0);

	exparg(bp, 0);

	IMP("END");

	goto upp;

}

/*
 * Replace and push-back on input stream the eventual replaced macro.
 */
int
submac(struct symtab *sp, int lvl)
{
	const usch *argary[MAXARGS+1];
	usch savc[100], savn[100];
	const usch *cp;
	usch *obp;
	int ch, nl, i, ccnt, bsv, gotnex;

	DPRINT(("%d:submac1: trying '%s'\n", lvl, sp->namep));
	if (*sp->value == OBJCT) {
		if (!okexp(sp))
			return 0; /* cannot expand */

		if (sp == filloc) {
			unpstr(sheap("\"%s\"", ifiles->fname));
			return 1;
		} else if (sp == linloc) {
			unpstr(sheap("%d", ifiles->lineno));
			return 1;
		}

		DPRINT(("submac: exp object macro '%s'\n",sp->namep));
		/* expand object-type macros */
		ch = addmac(sp);

		cunput(ch);
		cunput(EXP);
		for (cp = sp->value-1; *cp; cp--)
			cunput(*cp);
		cunput(ch);
		cunput(NEX);
		return 1;
	}

	/*
	 * Function-like macro; see if it is followed by a (
	 * Be careful about the expand/noexpand balance.
	 * Store read data on heap meanwhile.
	 * For directive	#define foo() kaka
	 * If input is 		<NEX><NEX>foo<EXP>()<EXP> then
	 * output should be 	<NEX><NEX><EXP>kaka<EXP>.
	 */
	obp = stringbuf;
	gotnex = nl = ccnt = 0;
	i = bidx;
	do {
		switch (ch = cinput()) {
		case NEX: /* disallow expansion */
			savc[ccnt] = ch;
			savn[ccnt] = cinput();
			gotnex = 1;
			ccnt++;
			break;
		case EXP: /* allow expansion */
			savc[ccnt] = ch;
			if (gotnex == 0) {
				savn[ccnt] = doexp();
			} else {
				savn[ccnt] = cinput();
			}
			ccnt++;
			break;

		case '\n':
			nl++;
			/* FALLTHROUGH */
		default:
			savch(ch);
		}
	} while (iswsnl(ch) || ch == EXP || ch == NEX);

	/*
	 * Is there any macro to expand?
	 * The okexp() call must be done here because an EXP may 
	 * have been found.
	 */
	bsv = okexp(sp);
	bidx = i;
	
	if (ch != '(' || !bsv) {
		DPRINT(("submac: failed '%s' %s\n",sp->namep,
		    ch != '(' ? "no (" : "already expanded"));
		cunput(ch);
		*--stringbuf = 0; /* remove saved ch */
		/* push back exp/noexp first */
		for (i = ccnt-1; i >= 0; i--)
			cunput(savn[i]), cunput(savc[i]);
		/* push back rest */
		unpstr(obp);
		stringbuf = obp;
		return 0;
	}

	/*
	 * A function-like macro has been found.  Read in the arguments,
	 * expand them and push-back everything for another scan.
	 */
	DPRINT(("%d:submac: continue macro '%s'\n", lvl, sp->namep));
	savch(0);
	if (readargs(sp, argary)) {
		/* Bailed out in the middle of arg list */
		/* XXX EXP balance */
		unpstr(obp);
		if (dflag>1)printf("%d:noreadargs\n", lvl);
		stringbuf = obp;
		return 0;
	}
	ifiles->lineno += nl;

	/* when all args are read from input stream */
	ch = addmac(sp);

	cunput(ch);
	cunput(EXP);
	DDPRINT(("%d:cunput(EXP)\n", lvl));

	subarg(sp, argary, lvl+1);

	cunput(ch);
	cunput(NEX);
	DDPRINT(("%d:cunput(NEX)\n", lvl));

	for (i = ccnt-1; i >= 0; i--)
		cunput(savn[i]), cunput(savc[i]);

	stringbuf = obp; /* Reset heap */
	DPRINT(("%d:Return submac\n", lvl));
	IMP("SM1");
	return 1;
}

/*
 * Read arguments and put in argument array.
 * If WARN is encountered return 1, otherwise 0.
 */
int
readargs(struct symtab *sp, const usch **args)
{
	const usch *vp = sp->value;
	usch savn[100];
	int c, i, plev, narg, ellips = 0;
	int gotnex, ccnt, warn;

	DPRINT(("readargs\n"));

	narg = *vp--;
	if (narg == VARG) {
		narg = *vp--;
		ellips = 1;
	}

	IMP("RDA1");
	/*
	 * read arguments and store them on heap.
	 */
	gotnex = ccnt = warn = 0;
	c = '(';
	for (i = 0; i < narg && c != ')'; i++) {
		args[i] = stringbuf;
		plev = 0;
		while ((c = sloscan()) == WSPACE || c == '\n')
			if (c == '\n')
				putch(cinput());
		for (;;) {
			if (c == NEX) {
				gotnex = 1;
				savch(NEX);
				savch(cinput());
				goto oho;
			}
			if (c == EXP) {
				if (gotnex == 0) {
					savn[ccnt++] = cinput();
				} else {
					savch(EXP);
					savch(cinput());
				}
				goto oho;
			}
			if (c == WARN) {
				warn++;
				goto oho;
			}
			if (plev == 0 && (c == ')' || c == ','))
				break;
			if (c == '(')
				plev++;
			if (c == ')')
				plev--;
			savstr((usch *)yytext);
oho:			while ((c = sloscan()) == '\n') {
				putch(cinput());
				savch('\n');
			}
			while (c == CMNT) {
				getcmnt();
				c = sloscan();
			}
			if (c == 0)
				error("eof in macro");
		}
		while (args[i] < stringbuf &&
		    (iswsnl(stringbuf[-1]) &&
		    !(stringbuf[-2] == EXP || stringbuf[-2] == NEX)))
			stringbuf--;
		savch('\0');
		if (dflag) {
			printf("readargs: save arg %d '", i);
			prline(args[i]);
			printf("'\n");
		}
	}

	IMP("RDA2");
	/* Handle varargs readin */
	if (ellips)
		args[i] = (const usch *)"";
	if (ellips && c != ')') {
		args[i] = stringbuf;
		plev = 0;
		while ((c = sloscan()) == WSPACE)
			;
		for (;;) {
			if (plev == 0 && c == ')')
				break;
			if (c == '(')
				plev++;
			if (c == ')')
				plev--;
			savstr((usch *)yytext);
			while ((c = sloscan()) == '\n') {
				cinput();
				savch('\n');
			}
		}
		while (args[i] < stringbuf && iswsnl(stringbuf[-1]))
			stringbuf--;
		savch('\0');
		
	}
	if (narg == 0 && ellips == 0)
		while ((c = sloscan()) == WSPACE || c == '\n')
			if (c == '\n')
				cinput();

	if (c != ')' || (i != narg && ellips == 0) || (i < narg && ellips == 1))
		error("wrong arg count");
	while (warn)
		cunput(WARN), warn--;
	while (ccnt)
		cunput(savn[--ccnt]), cunput(EXP);
	return 0;
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
 * expand a function-like macro.
 * vp points to end of replacement-list
 * reads function arguments from sloscan()
 * result is pushed-back for more scanning.
 */
void
subarg(struct symtab *nl, const usch **args, int lvl)
{
	int narg, instr, snuff;
	const usch *sp, *bp, *ap, *vp;

	DPRINT(("%d:subarg(%s)\n", lvl,nl->namep));
	vp = nl->value;
	narg = *vp--;
	if (narg == VARG)
		narg = *vp--;

	sp = vp;
	instr = snuff = 0;
	if (dflag>1) {
		printf("%d:subarg ARGlist for %s: '", lvl, nl->namep);
		prrep(vp);
		printf("'\n");
	}

	/*
	 * push-back replacement-list onto lex buffer while replacing
	 * arguments.  Arguments are macro-expanded if required.
	 */
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
			if (dflag>1){
				printf("%d:subarg GOTwarn; arglist '", lvl);
				prline(bp);
				printf("'\n");
			}
			if (sp[2] != CONC && !snuff && sp[-1] != CONC) {
				/*
				 * Expand an argument; 6.10.3.1: 
				 * "A parameter in the replacement list,
				 *  is replaced by the corresponding argument
				 *  after all macros contained therein have
				 *  been expanded.".
				 */
				exparg(bp, lvl+1);
				unpstr(stringbuf);
			} else {
			while (*bp)
				bp++;
			while (bp > ap) {
				bp--;
				if (snuff && !instr && iswsnl(*bp)) {
					while (iswsnl(*bp))
						bp--;
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
			}
		} else
			cunput(*sp);
		sp--;
	}
	DPRINT(("%d:Return subarg\n", lvl));
	IMP("SUBARG");
}

/*
 * Do a (correct) expansion of an argument.
 * Data is read from the string bp, pushed-back and expanded.
 * When nothing more to expand, return with expanded arg in lex buffer.
 */
void
exparg(const usch *bp, int lvl)
{
	struct symtab *nl;
	int c, isexp, i, ccnt;
	usch *och;
	usch *osb = stringbuf;
	int anychange;
	usch savc[100], savn[100];

	if (dflag) {
		printf("%d:exparg3: Expand '", lvl);
		prline(bp);
		printf("' block %d: ", bidx);
		for (i = 1; i < bidx; i++)
			printf("'%s' ", norep[bptr[i]]->namep);
		printf("\n");
	}

rescan:
	cunput(WARN);
	unpstr(bp);
	if (dflag>1) {
		printf("%d:exparg WARN '", lvl);
		prline(bp);
		printf("'\n");
	}

	anychange = 0;
	readmac++;
	while ((c = sloscan()) != WARN) {
		DDPRINT(("%d:exparg swdata %d\n", lvl, c));
		IMP("EA0");
		switch (c) {
		case NEX:
			DDPRINT(("%d:exparg donex\n", lvl));
			savch(NEX);
			savch(donex());
			break;

		case EXP:
			DDPRINT(("%d:exparg doexp\n", lvl));
			savch(EXP);
			savch(doexp());
			break;

		case NUMBER: /* handled as ident if no .+- in it */
			if (!mayid((usch *)yytext))
				goto def;
			/* FALLTHROUGH */
		case IDENT:
			/*
			 * Handle argument concatenation here.
			 */
			DDPRINT(("%d:exparg ident %d\n", lvl, c));
			och = stringbuf;
			isexp = ccnt = 0;
			savstr((usch *)yytext);

			/* Must see if ident was expandable before EXP */
			if ((nl = lookup(och, FIND)) && okexp(nl))
				isexp = 1;

			DPRINT(("%d:exparg: '%s' isexp %d\n", lvl, och, isexp));

			while ((c = cinput()) == EXP || c == NEX) {
				savc[ccnt] = c;
				savn[ccnt] = cinput();
				ccnt++;
			}
			cunput(c);
			c = sloscan();

			DPRINT(("%d:exparg neoent %d %d\n", lvl, ccnt, c));

			if (c == IDENT ||
			    (c == NUMBER && mayid((usch *)yytext))) {
				DPRINT(("id2: str %s\n", yytext));
				/* OK to always expand here? */
				/* push back exp/noexp */
				for (i = ccnt-1; i >= 0; i--)
					cunput(savn[i]), cunput(savc[i]);
				savstr((usch *)yytext);
				unpstr(och);
				stringbuf = och;
				continue; /* Refetch new longer identifier */
			}

			unpstr((const usch *)yytext);

			/* push back exp/noexp */
			for (i = ccnt-1; i >= 0; i--)
				cunput(savn[i]), cunput(savc[i]);

			DPRINT(("%d:exparg: pb '%s' str '%s'\n", lvl, yytext, och));
			IMP("EA1");
			/* try to expand the string we have */
			if (isexp) {
				/* expand the previous IDENT */
				if (submac(nl, lvl+1)) {
					/* Could expand, result on lexbuffer */
					stringbuf = och; /* clear saved name */
					anychange = 1;
				}
				
			}
			IMP("EA2");
			break;

		case CMNT:
			getcmnt();
			break;

		case '\n':
			cinput();
			savch(' ');
			break;

def:		default:
			savstr((usch *)yytext);
			break;
		}
	}
	*stringbuf = 0;
	if (dflag) {
		printf("%d:exparg return: change %d final '", lvl, anychange);
		prline(osb);
		printf("'\n");
	}
	bp = stringbuf = osb;
	if (anychange)
		goto rescan;
	readmac--;
}

void
imp(const char *str)
{
	printf("%s (%d) '", str, bidx);
	prline(ifiles->curptr);
	printf("'\n");
}

void
prrep(const usch *s)
{
	while (*s) {
		switch (*s) {
		case WARN: printf("<ARG(%d)>", *--s); break;
		case CONC: printf("<CONC>"); break;
		case SNUFF: printf("<SNUFF>"); break;
		case NEX: printf("<NEX(%d)>",*--s); break;
		case EXP: printf("<EXP(%d)>",*--s); break;
		default: printf("%c", *s); break;
		}
		s--;
	}
}

void
prline(const usch *s)
{
	while (*s) {
		switch (*s) {
		case WARN: printf("<WARN>"); break;
		case CONC: printf("<CONC>"); break;
		case SNUFF: printf("<SNUFF>"); break;
		case NEX: printf("<NEX(%d)>",*++s); break;
		case EXP: printf("<EXP(%d)>",*++s); break;
		case '\n': printf("<NL>"); break;
		default: printf("%c", *s); break;
		}
		s++;
	}
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

void
unpstr(const usch *c)
{
	const usch *d = c;

	if (dflag>1) {
		printf("Xunpstr: '");
		prline(c);
		printf("'\n");
	}
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
