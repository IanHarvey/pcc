/*	$Id$	*/

/*
 * Copyright (c) 2004 Anders Magnusson (ragge@ludd.luth.se).
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
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
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
 * Copyright(C) Caldera International Inc. 2001-2002. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code and documentation must retain the above
 * copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 * 	This product includes software developed or owned by Caldera
 *	International, Inc.
 * Neither the name of Caldera International, Inc. nor the names of other
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * USE OF THE SOFTWARE PROVIDED FOR UNDER THIS LICENSE BY CALDERA
 * INTERNATIONAL, INC. AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL CALDERA INTERNATIONAL, INC. BE LIABLE
 * FOR ANY DIRECT, INDIRECT INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OFLIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 */
/*
 * The C preprocessor.
 * This code originates from the V6 preprocessor with some additions
 * from V7 cpp, and at last ansi/c99 support.
 */
#include <sys/wait.h>

#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "cpp.h"

#define	MAXARG	250	/* # of args to a macro, limited by char value */
#define	SBSIZE	20000
#define	SYMSIZ	2000

static usch	sbf[SBSIZE];
/* C command */

/* buffer used internally */
#ifndef CPPBUF
#define	CPPBUF	BUFSIZ
#endif

int tflag;	/* traditional cpp syntax */
#ifdef CPP_DEBUG
int dflag;	/* debug printouts */
#endif
FILE *obuf;
static int exfail;
struct symtab symtab[SYMSIZ];

/* avoid recursion */
struct recur {
	struct recur *next;
	struct symtab *sp;
};

/* include dirs */
struct incs {
	struct incs *next;
	char *dir;
} *incdir[2];
#define	INCINC 0
#define	SYSINC 1

static struct symtab *filloc;
static struct symtab *linloc;
static struct symtab *datloc;
static struct symtab *timloc;
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
 * 
 * The first character in the replacement list is the number of arguments:
 *   OBJCT - object-type macro
 *   0 	   - empty parenthesis, foo()
 *   1->   - number of args.
 */

#define	OBJCT	0xff
#define	WARN	1	/* SOH, not legal char */
#define	CONC	2	/* STX, not legal char */
#define	SNUFF	3	/* ETX, not legal char */
#define	NOEXP	4	/* EOT, not legal char */
#define	EXPAND	5	/* ENQ, not legal char */

/* args for lookup() */
#define	FIND	0
#define	ENTER	1
#define	FORGET	3

static void expdef(usch *proto, struct recur *, int gotwarn);
static void savch(int c);
static void insym(struct symtab **sp, char *namep);
static void control(void);
static usch *savstr(usch *str);
static void define(void);
static void expmac(struct recur *);
static int canexpand(struct recur *, struct symtab *np);
static void include(void);
static void line(void);

int
main(int argc, char **argv)
{
	struct incs *w, *w2;
	struct symtab *nl, *thisnl;
	register int c, gotspc, ch;
	usch *osp;

	while ((ch = getopt(argc, argv, "D:I:S:U:td")) != -1)
		switch (ch) {
		case 'D': /* Define something */
			osp = optarg;
			while (*osp && *osp != '=')
				osp++;
			if (*osp == '=') {
				*osp++ = 0;
				while (*osp)
					osp++;
				*osp = OBJCT;
			} else {
				static char c[3] = { 0, '1', OBJCT };
				osp = &c[2];
			}
			nl = lookup(optarg, ENTER);
			if (nl->value)
				error("%s redefined", optarg);
			nl->value = osp;
			break;

		case 'S':
		case 'I':
			w = calloc(sizeof(struct incs), 1);
			w->dir = optarg;
			w2 = incdir[ch == 'I' ? INCINC : SYSINC];
			if (w2 != NULL) {
				while (w2->next)
					w2 = w2->next;
				w2->next = w;
			} else
				incdir[ch == 'I' ? INCINC : SYSINC] = w;
			break;

		case 'U':
			nl = lookup(optarg, FIND);
			if (nl && nl->value)
				nl->value = NULL;
			break;
#ifdef CPP_DEBUG
		case 'd':
			dflag = 1;
			break;
#endif
		case 't':
			tflag = 1;
			break;

		default:
			fprintf(stderr, "bad arg %c\n", ch);
			exit(1);
		}
	argc -= optind;
	argv += optind;

	exfail = 0;
	if (argc) {
		if (freopen(argv[0], "r", stdin) == NULL) {
			fprintf(stderr, "Can't open %s", argv[0]);
			exit(8);
		}
	}
	if (pushfile(argc ? argv[0] : "<stdin>"))
		error("cannot open %s", argv[0]);

	if (argc == 2) {
		if ((obuf = fopen(argv[1], "w")) == 0) {
			fprintf(stderr, "Can't creat %s\n", argv[1]);
			exit(8);
		}
	} else
		obuf = stdout;

	prtline();

	insym(&filloc, "__FILE__");
	insym(&linloc, "__LINE__");
	insym(&datloc, "__DATE__");
	insym(&timloc, "__TIME__");

	thisnl = NULL;
	while ((c = yylex()) != 0) {
		switch (c) {
		case CONTROL:
			control();
			break;

		case IDENT:
			if (flslvl)
				break;
			osp = stringbuf;
			nl = lookup(yytext, FIND);
			if (nl == 0 || thisnl == 0)
				goto found;
			if (thisnl == nl) {
				nl = 0;
				goto found;
			}
			gotspc = 0;
			if ((c = yylex()) == WSPACE)
				gotspc = 1, c = yylex();
			if (c != EXPAND) {
				unpstr(yytext);
				if (gotspc)
					cunput(' ');
				unpstr(nl->namep);
				(void)yylex(); /* get yytext correct */
				nl = 0; /* ignore */
			} else
				thisnl = NULL;

found:			if (nl == 0 || subst(yytext, nl, NULL) == 0) {
				fputs(yytext, obuf);
			} else if (osp != stringbuf) {
				cunput(EXPAND);
				while (stringbuf > osp)
					cunput(*--stringbuf);
				thisnl = nl;
			}
			stringbuf = osp; /* clean up heap */
			break;

		case EXPAND:
			thisnl = NULL;
			break;

		case CHARCON:
		case NUMBER:
		case FPOINT:
		case STRING:
		case WSPACE:
		case NL:
		default:
			if (flslvl == 0)
				fputs(yytext, obuf);
			break;
		}
	}
	fclose(obuf);
	return exfail;
}

/*
 * do something when a '#' is found.
 */
void
control()
{
	struct symtab *np;
	int t;

#define CHECK(x) (yytext[0] == #x[0]) && strcmp(yytext, #x) == 0

	if ((t = yylex()) == WSPACE)
		t = yylex();
	if (t != IDENT)
		return error("bad control '%s'", yytext);

	if (CHECK(include)) {
		if (flslvl)
			goto exit;
		include();
		return;
	} else if (CHECK(else)) {
		if (flslvl) {
			if (elflvl > trulvl)
				;
			else if (--flslvl!=0) {
				flslvl++;
			} else {
				trulvl++;
				prtline();
			}
		} else if (trulvl) {
			flslvl++;
			trulvl--;
		} else
			error("If-less else");
		if (elslvl==trulvl+flslvl) error("Too many else");
		elslvl=trulvl+flslvl;
	} else if (CHECK(endif)) {
		if (flslvl) {
			flslvl--;
			if (flslvl == 0)
				prtline();
		} else if (trulvl)
			trulvl--;
		else
			error("If-less endif");
		if (flslvl == 0)
			elflvl = 0;
		elslvl = 0;
	} else if (CHECK(error)) {
		usch *ch = stringbuf;
		if (flslvl)
			goto exit;
		while (yylex() != NL)
			savstr(yytext);
		savch('\n');
		error("error: %s", ch);
#define GETID() if (yylex() != WSPACE || yylex() != IDENT) goto cfail
	} else if (CHECK(define)) {
		if (flslvl)
			goto exit;
		GETID();
		define();
	} else if (CHECK(ifdef)) {
		GETID();
		if (flslvl == 0 && lookup(yytext, FIND) != 0)
			trulvl++;
		else
			flslvl++;
	} else if (CHECK(ifndef)) {
		GETID();
		if (flslvl == 0 && lookup(yytext, FIND) == 0)
			trulvl++;
		else
			flslvl++;
	} else if (CHECK(undef)) {
		GETID();
		if (flslvl == 0 && (np = lookup(yytext, FIND)))
			np->value = 0;
	} else if (CHECK(line)) {
		if (flslvl)
			goto exit;
		line();
	} else if (CHECK(if)) {
		if (flslvl==0 && yyparse())
			++trulvl;
		else
			++flslvl;
	} else if (CHECK(elif)) {
		if (flslvl == 0)
			elflvl = trulvl;
		if (flslvl) {
			if (elflvl > trulvl)
				;
			else if (--flslvl!=0)
				++flslvl;
			else {
				if (yyparse()) {
					++trulvl;
					prtline();
				} else
					++flslvl;
			}
		} else if (trulvl) {
			++flslvl;
			--trulvl;
		} else
			error("If-less elif");
	} else
		error("undefined control '%s'", yytext);

	return;

cfail:
	error("control line syntax error");

exit:
	while (yylex() != NL)
		;
	putc('\n', obuf);
#undef CHECK
}

void
line()
{
	struct symtab *nl;
	int c;

	if (yylex() != WSPACE)
		goto bad;
	if ((c = yylex()) == IDENT) {
		/* Do macro preprocessing first */
		usch *osp = stringbuf;
		if ((nl = lookup(yytext, FIND)) == NULL)
			goto bad;
		if (subst(yytext, nl, NULL) == 0)
			goto bad;
		while (stringbuf > osp)
			cunput(*--stringbuf);
		c = yylex();
	}

	if (c != NUMBER)
		goto bad;
	setline(atoi(yytext));

	if ((c = yylex()) != NL && c != WSPACE)
		goto bad;
	if (c == NL)
		return setline(curline()+1);
	if (yylex() != STRING)
		goto bad;
	yytext[strlen(yytext)-1] = 0;
	setfile(&yytext[1]);
	return;

bad:	error("bad line directive");
}

/*
 * Include a file. Include order:
 * - if name inside <>, only search system includes.
 * - if name inside "", first search current dir, then -I dirs, 
 *   then system includes.
 */
void
include()
{
	struct incs *w;
	struct symtab *nl;
	usch *osp;
	char *fn;
	int i, c, it;

	osp = stringbuf;
	if (yylex() != WSPACE)
		goto bad;
again:	if ((c = yylex()) != STRING && c != '<' && c != IDENT)
		goto bad;

	if (c == IDENT) {
		if ((nl = lookup(yytext, FIND)) == NULL)
			goto bad;
		if (subst(yytext, nl, NULL) == 0)
			goto bad;
		savch('\0');
		unpstr(osp);
		goto again;
	} else if (c == '<') {
		fn = stringbuf;
		while ((c = yylex()) != '>' && c != NL) {
			if (c == NL)
				goto bad;
			savstr(yytext);
		}
		savch('\0');
		it = SYSINC;
	} else {
		yytext[strlen(yytext)-1] = 0;
		fn = &yytext[1];
		it = INCINC;
	}

	/* create search path and try to open file */
	for (i = it; i < 2; i++) {
		for (w = incdir[i]; w; w = w->next) {
			usch *nm = stringbuf;

			savstr(w->dir); savch('/');
			savstr(fn); savch(0);
			if (pushfile(nm) == 0)
				goto ret;
			stringbuf = nm;
		}
	}
	error("cannot find '%s'", fn);
	stringbuf = osp;
	return;

bad:	error("bad include");
ret:	prtline();
	stringbuf = osp;
}

void
define()
{
	struct symtab *np;
	usch *args[MAXARG], *ubuf;
	int c, i;
	int mkstr = 0, narg = -1;

	np = lookup(yytext, ENTER);
	if (np->value)
		error("%s redefined", np->namep);

	if ((c = yylex()) == '(') {
		narg = 0;
		/* function-like macros, deal with identifiers */
		while ((c = yylex()) != ')') {
			if (c == WSPACE) c = yylex();
			if (c == ',') c = yylex();
			if (c == WSPACE) c = yylex();
			if (c == ')')
				break;
				if (c != IDENT)
			error("define error");
			args[narg] = alloca(strlen(yytext)+1);
			strcpy(args[narg], yytext);
			narg++;
		}
	} else if (c == NL) {
		/* #define foo */
		cunput('\n');
	} else if (c != WSPACE)
		error("bad define");

	if ((c = yylex()) == WSPACE)
		c = yylex();

	/* parse replacement-list, substituting arguments */
	savch('\0');
	while (c != NL) {
		switch (c) {
		case WSPACE:
			/* remove spaces if it surrounds a ## directive */
			ubuf = stringbuf;
			savstr(yytext);
			c = yylex();
			if (c == CONCAT) {
				stringbuf = ubuf;
				savch(CONC);
				if ((c = yylex()) == WSPACE)
					c = yylex();
			}
			continue;

		case CONCAT:
			/* No spaces before concat op */
			savch(CONC);
			if ((c = yylex()) == WSPACE)
				c = yylex();
			continue;

		case MKSTR:
			if (narg < 0) {
				/* no meaning in object-type macro */
				savch('#');
				break;
			}
			/* remove spaces between # and arg */
			savch(SNUFF);
			if ((c = yylex()) == WSPACE)
				c = yylex(); /* whitespace, ignore */
			mkstr = 1;

			/* FALLTHROUGH */
		case IDENT:
			if (narg < 0)
				goto id; /* just add it if object */
			/* check if its an argument */
			for (i = 0; i < narg; i++)
				if (strcmp(yytext, args[i]) == 0)
					break;
			if (i == narg) {
				if (mkstr)
					error("not argument");
				goto id;
			}
			savch(i);
			savch(WARN);
			if (mkstr)
				savch(SNUFF), mkstr = 0;
			break;

		default:
id:			savstr(yytext);
			break;
		}
		c = yylex();
	}
	savch(narg < 0 ? OBJCT : narg);
	np->value = stringbuf-1;
	putc('\n', obuf);

#ifdef CPP_DEBUG
	if (dflag) {
		usch *w = np->value;

		printf("!define: ");
		if (*w == OBJCT)
			printf("[object]");
		else
			printf("[%d]", *w);
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
}

void
insym(sp, namep)
struct symtab **sp;
char *namep;
{
	register struct symtab *np;

	*sp = np = lookup(namep, ENTER);
	np->value = NULL;
}

void
error(char *s, ...)
{
	va_list ap;

	va_start(ap, s);
	fprintf(stderr, "%s:%d: ", curfile(), curline());
	vfprintf(stderr, s, ap);
	fputc('\n', stderr);
	exfail++;
	va_end(ap);
	exit(8);
}

/*
 * store a character into the "define" buffer.
 */
void
savch(c)
{
	*stringbuf++ = c;
	if (stringbuf-sbf < SBSIZE)
		return;
	error("Too much defining");
	exit(1);
}

/*
 * Do a symbol lookup.
 * If enterf == FIND, only lookup.
 */
struct symtab *
lookup(namep, enterf)
	char *namep;
{
	register char *np;
	register struct symtab *sp;
	int i, c, around;

if (dflag)printf("lookup '%s'\n", namep);
	np = namep;
	around = i = 0;
	while ((c = *np++))
		i =+ c;
	i %= SYMSIZ;
	sp = &symtab[i];

	while (sp->namep) {
		if (*sp->namep == *namep && strcmp(sp->namep, namep) == 0)
			return(sp);
		if (++sp >= &symtab[SYMSIZ]) {
			if (around++)
				error("too many defines");
			else
				sp = symtab;
		}
	}
	if (enterf == ENTER) {
		sp->namep = savstr(namep), savch('\0');
	}
	return(sp->namep ? sp : 0);
}

/*
 * substitute namep for sp->value.
 */
int
subst(np, sp, rp)
char *np;
struct symtab *sp;
struct recur *rp;
{
	struct recur rp2;
	register usch *vp, *cp;
	int c, rv = 0;

if (dflag)printf("subst\n");
	if ((vp = sp->value) == 0) {
		/*
		 * If no value is assigned, it may be a special macro,
		 * otherwise a deleted macro.
		 */
		if (sp == filloc) {
			savch('"');
			savstr(curfile());
			savch('"');
		} else if (sp == linloc) {
			char buf[12];
			sprintf(buf, "%d", curline());
			savstr(buf);
		} else if (sp == datloc) {
			time_t t = time(NULL);
			char *n = ctime(&t);
			savch('"');
			n[24] = n[11] = 0;
			savstr(&n[4]);
			savstr(&n[20]);
			savch('"');
		} else if (sp == timloc) {
			time_t t = time(NULL);
			char *n = ctime(&t);
			savch('"');  
			n[19] = 0;
			savstr(&n[11]);
			savch('"');
		} else
			return 0;
		return 1;
	}

	rp2.next = rp;
	rp2.sp = sp;

	if (*vp-- != OBJCT) {
		int gotwarn = 0;

		/* should we be here at all? */
		/* check if identifier is followed by parentheses */
		rv = 1;
		do {
			if ((c = yylex()) == NL)
				putc('\n', obuf);
			if (c == WARN) {
				gotwarn++;
				if (rp == NULL)
					goto noid;
			}
		} while (c == WSPACE || c == NL || c == WARN);

		cp = yytext;
		while (*cp)
			cp++;
		while ((char *)cp > yytext)
			cunput(*--cp);
if (dflag)printf("c %d\n", c);
		if (c == '(' ) {
			expdef(vp, &rp2, gotwarn);
			return rv;
		} else {
			/* restore identifier */
noid:			while (gotwarn--)
				cunput(WARN);
			cunput(' ');
			cp = sp->namep;
			while (*cp)
				cp++;
			while (cp > sp->namep)
				cunput(*--cp);
			if ((c = yylex()) != IDENT)
				error("internal sync error");
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
 * do macro-expansion until WARN character read.
 * will recurse into lookup() for recursive expansion.
 * when returning all expansions on the token list is done.
 */
void
expmac(struct recur *rp)
{
	struct symtab *nl;
	int c, noexp = 0, gotspc;
	usch *och;

if (dflag)printf("expmac\n");
if (dflag && rp)printf("do not expand %s\n", rp->sp->namep);
	while ((c = yylex()) != WARN) {
		switch (c) {
		case NOEXP: noexp++; break;
		case EXPAND: noexp--; break;

		case IDENT:
			/* workaround if an arg will be concatenated */
			och = stringbuf;
			savstr(yytext);
			savch('\0');
//printf("id: str %s\n", och);
			if ((c = yylex()) == EXPAND) {
//printf("funnet expand\n");
				if ((c = yylex()) == NOEXP) {
//printf("funnet noexp\n");
					if ((c = yylex()) == IDENT) {
//printf("funnet ident %s%s\n", och, yytext);
						stringbuf--;
						savstr(yytext);
						savch('\0');
						cunput(NOEXP);
						unpstr(och);
						noexp--;
						stringbuf = och;
						continue;
					} else {
//printf("ofunnet ident\n");
						unpstr(yytext);
						unpstr(och);
						stringbuf = och;
						continue;
					}
				} else {
//printf("ofunnet inoexp\n");
					unpstr(yytext);
					cunput(EXPAND);
					unpstr(och);
					yylex();
				}
			} else {
				unpstr(yytext);
				unpstr(och);
				yylex();
//printf("ofunnet expand: yytext %s\n", yytext);
			}
			stringbuf = och;

			if ((nl = lookup(yytext, FIND)) == NULL)
				goto def;

			if (canexpand(rp, nl) == 0)
				goto def;
			if (noexp == 0) {
				if ((c = subst(nl->namep, nl, rp)) == 0)
					goto def;
				break;
			}
			if (noexp != 1)
				error("bad noexp %d", noexp);
			gotspc = 0;
			if ((c = yylex()) == WSPACE)
				gotspc = 1, c = yylex();
			if (c == EXPAND) {
				noexp--;
				if (subst(nl->namep, nl, rp))
					break;
				savstr(nl->namep);
				if (gotspc)
					savch(' ');
			} else {
				unpstr(yytext);
				if (gotspc)
					cunput(' ');
				savstr(nl->namep);
			}
			break;

def:		default:
			savstr(yytext);
			break;
		}
	}
if (dflag)printf("return from expmac\n");
}

/*
 * expand a function-like macro.
 * vp points to end of replacement-list
 * reads function arguments from yylex()
 * result is written on top of heap
 */
void
expdef(vp, rp, gotwarn)
	usch *vp;
	struct recur *rp;
{
	usch **args, *sptr, *ap, *bp, *sp;
	int narg, c, i, plev, snuff, instr;

if (dflag)printf("expdef %s rp %s\n", vp, (rp ? (char *)rp->sp->namep : ""));
	if ((c = yylex()) != '(')
		error("got %c, expected )", c);
	narg = vp[1];
	args = alloca(sizeof(usch *) * narg);


	/*
	 * read arguments and store them on heap.
	 * will be removed just before return from this function.
	 */
	sptr = stringbuf;
	for (i = 0; i < narg && c != ')'; i++) {
		args[i] = stringbuf;
		plev = 0;
		if ((c = yylex()) == WSPACE)
			c = yylex();
		for (;;) {
			if (plev == 0 && (c == ')' || c == ','))
				break;
			if (c == '(')
				plev++;
			if (c == ')')
				plev--;
			savstr(yytext);
			c = yylex();
		}
		while (args[i] < stringbuf &&
		    (stringbuf[-1] == ' ' || stringbuf[-1] == '\t'))
			stringbuf--;
		savch('\0');
	}
	if (narg == 0)
		c = yylex();
	if (c != ')' || i != narg)
		error("wrong arg count");

	while (gotwarn--)
		cunput(WARN);

#ifdef CPP_DEBUG
	if (dflag) {

	}
#endif
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

			bp = ap = args[(int)*--sp];
			if (sp[2] != CONC && !snuff && sp[-1] != CONC) {
				cunput(WARN);
				while (*bp)
					bp++;
				while (bp > ap)
					cunput(*--bp);
if (dflag) printf("expand arg %d string %s\n", *sp, ap);
				bp = ap = stringbuf;
				savch(NOEXP);
				expmac(NULL);
				savch(EXPAND);
				savch('\0');
			}
			while (*bp)
				bp++;
			while (bp > ap) {
				bp--;
//printf("*bp %d\n", *bp);
				if (snuff && !instr && 
				    (*bp == ' ' || *bp == '\t' || *bp == '\n')){
					while (*bp == ' ' || *bp == '\t' ||
					    *bp == '\n') {
						if (*bp == '\n')
							putc('\n', obuf);
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
}

usch *
savstr(usch *str)
{
	char *rv = stringbuf;

	while ((*stringbuf++ = *str++))
		if (stringbuf >= &sbf[SBSIZE])
			error("out of macro space!");
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
unpstr(usch *c)
{
	usch *d = c;

	while (*d)
		d++;
	while (d > c) {
		cunput(*--d);
	}
}
