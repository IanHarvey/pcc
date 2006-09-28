%{
/*	$Id$	*/

/*
 * Copyright (c) 2004 Anders Magnusson. All rights reserved.
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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>

#include "cpp.h"
#include "y.tab.h"
%}

%{
static long long cvtdig(int rad);
static long long charcon(void);
static void elsestmt(void);
static void ifdefstmt(void);
static void ifndefstmt(void);
static void endifstmt(void);
static void ifstmt(void);
static void cpperror(void);
static void pragmastmt(void);
static void undefstmt(void);
static void cpperror(void);
static void elifstmt(void);
void  include(void);
void  define(void);


static int inch(void);

/* definition for include file info */
struct includ {
	struct includ *next;
	char *fname;
	int lineno;
	int infil;
	usch *curptr;
	usch *maxread;
	usch *ostr;
	usch *buffer;
	usch bbuf[NAMEMAX+CPPBUF+1];
} *ifiles;

static int scale;

#ifdef FLEX_SCANNER /* should be set by autoconf instead */
static int
yyinput(char *b, int m)
{
	int c, i;

	for (i = 0; i < m; i++) {
		if ((c = inch()) < 0)
			break;
		*b++ = c;
		if (c == '\n') {
			i++;
			break;
		}
	}
	return i;
}
#undef YY_INPUT
#define YY_INPUT(b,r,m) (r = yyinput(b, m))
#else	/* Assume lex here */
#undef input
#undef unput
#define input() inch()
#define unput(ch) unch(ch)
#endif
#define PRTOUT(x) if (YYSTATE || slow) return x; if (!flslvl) putstr(yytext);
%}

D	[0-9]
L	[a-zA-Z_]
H	[a-fA-F0-9]
E	[Ee][+-]?{D}+
FS	(f|F|l|L)
IS	(u|U|l|L)*
WS	[\t ]

%s IFR CONTR DEF

%%

"\n"			{	int os = YYSTATE;
				BEGIN 0;
				ifiles->lineno++;
				if (flslvl == 0) {
					if (curline() == 1)
						prtline();
					else
						putch('\n');
				}
				if (os != 0 || slow)
					return '\n';
			}

<IFR>"=="		{ return EQ; }
<IFR>"!="		{ return NE; }
<IFR>"<="		{ return LE; }
<IFR>"<<"		{ return LS; }
<IFR>">>"		{ return RS; }
<IFR>">="		{ return GE; }
<IFR>"||"		{ return OROR; }
<IFR>"&&"		{ return ANDAND; }
<IFR>"defined"		{ return DEFINED; }
<IFR>{WS}+		{ ; }


0[xX]{H}+{IS}?		{ 	scale = 16;
			num:	if (YYSTATE)
					yylval.val = cvtdig(scale);
				PRTOUT(NUMBER);
			}
0{D}+{IS}?		{ scale = 8; goto num; }
{D}+{IS}?		{ scale = 10; goto num; }
L?'(\\.|[^\\'])+'	{	if (YYSTATE)
					yylval.val = charcon();
				PRTOUT(CHARCON);
			}

<IFR>.			{ return yytext[0]; }

{D}+{E}{FS}?		{ PRTOUT(FPOINT); }
{D}*"."{D}+({E})?{FS}?	{ PRTOUT(FPOINT); }
{D}+"."{D}*({E})?{FS}?	{ PRTOUT(FPOINT); }

^{WS}*#{WS}*		{ BEGIN CONTR; }
{WS}+			{ PRTOUT(WSPACE); }

<CONTR>"ifndef"		{ ifndefstmt(); }
<CONTR>"ifdef"		{ ifdefstmt(); }
<CONTR>"if"{WS}+	{ BEGIN IFR; ifstmt(); }
<CONTR>"include"	{ include(); }
<CONTR>"else"		{ elsestmt(); }
<CONTR>"endif"		{ endifstmt(); }
<CONTR>"error"		{ cpperror(); }
<CONTR>"define"		{ BEGIN DEF; define(); }
<CONTR>"undef"		{ undefstmt(); }
<CONTR>"line"		{ ; }
<CONTR>"pragma"		{ pragmastmt(); }
<CONTR>"elif"{WS}+	{ BEGIN IFR; elifstmt(); }



"//".*$			{ /* if (tflag) yyless(..) */
				if (Cflag)
					putstr(yytext);
				else
					putch(' ');
			}
"/*"			{	int c;
				if (Cflag)
					putstr(yytext);
			more:	while ((c = input()) && c != '*') {
					if (c == '\n')
						putch(c), ifiles->lineno++;
					else if (Cflag)
						putch(c);
				}
				if (c == 0)
					return 0;
				if (Cflag)
					putch(c);
				if ((c = input()) && c != '/') {
					if (Cflag)
						putch('*');
					unput(c);
					goto more;
				}
				if (Cflag)
					putch(c);
				if (c == 0)
					return 0;
				if (!tflag && !Cflag)
					unput(' ');
			}

<DEF>"##"		{ return CONCAT; }
<DEF>"#"		{ return MKSTR; }
<DEF>"..."		{ return ELLIPS; }

L?\"(\\.|[^\\"])*\"	{ PRTOUT(STRING); }
{L}({L}|{D})*		{
#if 0
				if (slow)
                                        return IDENT;
                                if (lookup(yytext, FIND) == 0)
                                        putstr(yytext);
                                else
                                        return IDENT;

#else
				struct symtab *nl;
				if (slow)
					return IDENT;
				if (flslvl)
					; /* do nothing */
				else if ((nl = lookup(yytext, FIND)) != 0) {
					gotident(nl);
#if 0
					usch *osp = stringbuf;
					putstr(strexpand(nl));
					stringbuf = osp;
#endif
				} else
					putstr(yytext);
#endif
			}

.			{ PRTOUT(yytext[0]); /* return yytext[0]; if (YYSTATE) return yytext[0];
				if (!flslvl && yytext[0]) putstr(yytext); */
			}

%%

usch *yyp, yybuf[CPPBUF];

int yylex(void);
int yywrap(void);

#if 0
static struct includ *
getbuf(usch *file)
{
	struct includ *ic;
	usch *ostr = stringbuf;

	stringbuf = (usch *)ROUND((int)stringbuf);
	ic = (struct includ *)stringbuf;
	stringbuf += sizeof(struct includ);
	ic->ostr = ostr;

	return ic;
}
#endif

static void
putbuf(struct includ *ic)
{
if (stringbuf < (usch *)&ic[1])
;
else
	stringbuf = ic->ostr;
}

static int
inpch(void)
{
	struct includ *ic;
	int len;

	if (ifiles->curptr < ifiles->maxread)
		return *ifiles->curptr++;

	if (ifiles->infil < 0) {
		ic = ifiles;
		ifiles = ifiles->next;
		putbuf(ic);
		return inpch();
	}
	if ((len = read(ifiles->infil, ifiles->buffer, CPPBUF)) < 0)
		error("read error on file %s", ifiles->fname);
	if (len == 0)
		return -1;
	ifiles->curptr = ifiles->buffer;
	ifiles->maxread = ifiles->buffer + len;
	return inpch();
}

#if 0
static void
unput(int c)
{
	struct includ *ic;

	if (ifiles->curptr > ifiles->bbuf) {
		*--ifiles->curptr = c;
	} else {
		ic = getbuf(NULL);
		ic->fname = ifiles->fname;
		ic->lineno = ifiles->lineno;
		ic->infil = -1;
		ic->curptr = &ic->bbuf[NAMEMAX+CPPBUF+1];
		ic->maxread = ic->curptr;
		ic->next = ifiles;
		ifiles = ic;
		*--ifiles->curptr = c;
	}
}
#endif

#define unch(c) *--ifiles->curptr = c

static int
inch(void)
{
	int c;

again:	switch (c = inpch()) {
	case '\\': /* continued lines */
		if ((c = inpch()) == '\n') {
			ifiles->lineno++;
			putc('\n', obuf);
			goto again;
		}
		unch(c);
		return '\\';
	case '?': /* trigraphs */
		if ((c = inpch()) != '?') {
			unch(c);
			return '?';
		}
		switch (c = inpch()) {
		case '=': c = '#'; break;
		case '(': c = '['; break;
		case ')': c = ']'; break;
		case '<': c = '{'; break;
		case '>': c = '}'; break;
		case '/': c = '\\'; break;
		case '\'': c = '^'; break;
		case '!': c = '|'; break;
		case '-': c = '~'; break;
		default:
			unch(c);
			unch('?');
			return '?';
		}
		unch(c);
		goto again;
	default:
		return c;
	}
}

#if 0
int
yylex()
{
	static int wasnl = 1;
	int c, oc, rval;

fast:	
	yyp = yytext;
	c = input();
	if (c != ' ' && c != '\t' && c != '#')
		wasnl = 0;
#define ONEMORE()	{ *yyp++ = c; c = slofgetc(); }
again:	switch (c) {
	case -1:
		rval = 0;
		break;

	case '\'': /* charcon */
	case '"': /* string */
chstr:		oc = c;
		if (slow == 0) {
			do {
				putch(c);
				if (c == '\\')
					putch(slofgetc());
			} while ((c = slofgetc()) != EOF && c != oc);
			if (c == oc)
				putch(c);
			goto fast;
		} else {
			do {
				*yyp++ = c;
				if (c == '\\')
					*yyp++ = slofgetc();
			} while ((c = slofgetc()) != EOF && c != oc);
			*yyp++ = c; *yyp = 0;
		}
		rval = oc == '"' ? STRING : CHARCON;
		break;

	case '0': case '1': case '2': case '3': case '4': 
	case '5': case '6': case '7': case '8': case '9': 
		*yyp++ = c;
		c = slofgetc();
		if (yyp[-1] == '0' && (c == 'x' || c == 'X')) {
			do {
				ONEMORE();
			} while (isxdigit(c));
		} else {
			while (isdigit(c))
				ONEMORE();
		}
		if (c != '.' && c != 'e' && c != 'E') {
			/* not floating point number */
			while (c == 'l' || c == 'L' || c == 'u' || c == 'U') {
				ONEMORE();
			}
			cunput(c);
			*yyp = 0;
			rval = NUMBER;
			break;
		}
		/* it's a floating point number here */
		if (c == '.') { /* decimal point */
F:			do { /* may be followed by digits */
				ONEMORE();
			} while (isdigit(c));
			if (c == 'e' || c == 'E') {
E:				ONEMORE();
				if (c == '-' || c == '+') {
					ONEMORE();
				}
				while (isdigit(c))
					ONEMORE();
			}
			if (c == 'f' || c == 'F' || c == 'l' || c == 'L')
				ONEMORE();
			cunput(c);
			*yyp = 0;
			rval = FPOINT;
			break;
		} else
			goto E;

	case '.':
		ONEMORE();
		if (isdigit(c))
			goto F;
		if (!slow) {
			UNPUT(c);
			putch('.');
			goto fast;
		}
		if (c == '.') {
			ONEMORE();
			if (c == '.') {
				*yyp++ = '.'; *yyp = 0;
				rval = ELLIPS;
				break;
			}
			cunput(c);
			cunput('.');
			*--yyp = 0;
			rval = '.';
			break;
		}
		cunput(c);
		*yyp = 0;
		rval = '.';
		break;

	case '\\':
		c = input();
		if (c == '\n') {
			ifiles->lineno++;
			putch('\n');
			goto fast;
		}
		if (!slow) {
			putch('\\');
			goto again;
		}
		UNPUT(c);
		*yyp++ = '\\'; *yyp = 0;
		rval = '\\';
		break;
		
	case '\n':
		wasnl = 1;
		ifiles->lineno++;
		rval = '\n';
		if (slow)
			break;
		if (flslvl == 0) {
			if (curline() == 1)
				prtline();
			else
				putch('\n');
		}
		goto fast;

	case '#':
		if (wasnl) {
			wasnl = 0;
			rval = CONTROL;
			break;
		}
		if (!slow) {
			putch('#');
			goto fast;
		}
		*yyp++ = c;
		c = input();
		if (c == '#') {
			*yyp++ = c;
			*yyp = 0;
			rval = CONCAT;
		} else {
			unput(c);
			*yyp = 0;
			rval = MKSTR;
		}
		break;

	case ' ':
	case '\t': /* whitespace */
		do {
			*yyp++ = c;
			c = input();
		} while (c == ' ' || c == '\t');
		if (wasnl && c == '#') {
			wasnl = 0;
			rval = CONTROL;
		} else {
			unput(c);
			*yyp = 0;
			rval = WSPACE;
		}
		break;

	case '/':
		if ((c = slofgetc()) == '/') {
			if (Cflag)
				fprintf(obuf, "//");
			while ((c = slofgetc()) && c != '\n')
				if (Cflag)
					putc(c, obuf);
			goto again;
		} else if (c == '*') {
			if (Cflag)
				fprintf(obuf, "/*");
			oc = 0;
			do { 
				while ((c = slofgetc()) && c != '*') {
					if (c == '\n') {
						putc(c, obuf);
						ifiles->lineno++;
					} else if (Cflag)
						putc(c, obuf);
				}
				if (Cflag)
					putc(c, obuf);
				if ((c = slofgetc()) == '/')
					break;
				unput(c);
			} while (c);
			if (Cflag)
				putc(c, obuf);
			if (tflag) {
				rval = yylex();
			} else {
				*yyp++ = ' '; *yyp = 0;
				rval = WSPACE;
			}
		} else {
			unput(c);
			*yyp++ = '/'; *yyp = 0;
			rval = '/';
		}
		break;

	case 'L': /* may be STRING, CHARCON or identifier */
		*yyp++ = c;
		if ((c = slofgetc()) == '"' || c == '\'')
			goto chstr;
gotid:		while (isalnum(c) || c == '_') {
			*yyp++ = c;
			c = slofgetc();
		}
		*yyp = 0;
		unput(c);
		rval = IDENT;
		break;

	default:
		if (isalpha(c) || c == '_')
			goto gotid;
		if (!slow && c > 5) {
			putch(c);
			goto fast;
		}
		yytext[0] = c; yytext[1] = 0;
		rval = c;
		break;
	}
	return rval;
}
#endif

/*
 * A new file included.
 * If ifiles == NULL, this is the first file and already opened (stdin).
 * Return 0 on success, -1 on failure to open file.
 */
int
pushfile(char *file)
{
	struct includ ibuf;
	struct includ *old;
	struct includ *ic;
	int c;

	ic = &ibuf;
	old = ifiles;

	slow = 0;
	if (file != NULL) {
		if ((ic->infil = open(file, O_RDONLY)) < 0)
			return -1;
		ic->fname = file;
	} else {
		ic->infil = 0;
		ic->fname = "<stdin>";
	}
	ic->buffer = ic->bbuf+NAMEMAX;
	ic->curptr = ic->buffer;
	ifiles = ic;
	ic->lineno = 1;
	ic->maxread = ic->curptr;
	prtline();

	if ((c = yylex()) != 0)
		error("yylex returned %d", c);

	if (trulvl || flslvl)
		error("unterminated conditional");

	ifiles = old;
	close(ic->infil);
	return 0;
}

/*
 * Print current position to output file.
 */
void
prtline()
{
	fprintf(obuf, "# %d \"%s\"\n", ifiles->lineno, ifiles->fname);
}

void
cunput(int c)
{
extern int dflag;
if (dflag)printf(": '%c'(%d)", c, c);
	unput(c);
}

void
setline(int line)
{
	if (ifiles)
		ifiles->lineno = line-1;
}

void
setfile(char *name)
{
	if (ifiles)
		ifiles->fname = strdup(name);
}

int
curline()
{
	return ifiles ? ifiles->lineno : 0;
}

char *
curfile()
{
	return ifiles ? ifiles->fname : "";
}

int yywrap(void) { return 1; }

static long long
cvtdig(int rad)
{
	long long rv = 0;
	char *y = yytext;
	int c;

	c = *y++;
	while (isxdigit(c)) {
		if (c >= 'a')
			c = c - 'a' + 10;
		else if (c >= 'A')
			c = c - 'A' + 10;
		else
			c = c - '0';
		rv = rv * rad + c;
		c = *y++;
	}
	return rv;
}

static long long
charcon(void)
{
	return 0;
}

static void
chknl(void)
{
	int t;

	if ((t = yylex()) == WSPACE)
		t = yylex();
	if (t != '\n')
		error("newline expected");
}

static void
elsestmt(void)
{
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
	if (elslvl==trulvl+flslvl)
		error("Too many else");
	elslvl=trulvl+flslvl;
	chknl();
}

static void
ifdefstmt(void)          
{ 
	if (yylex() != WSPACE || yylex() != IDENT)
		error("bad ifdef");
	if (flslvl == 0 && lookup(yytext, FIND) != 0)
		trulvl++;
	else
		flslvl++;
	chknl();
}

static void
ifndefstmt(void)          
{ 
	if (yylex() != WSPACE || yylex() != IDENT)
		error("bad ifndef");
	if (flslvl == 0 && lookup(yytext, FIND) == 0)
		trulvl++;
	else
		flslvl++;
	chknl();
}

static void
endifstmt(void)          
{
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
	chknl();
}

static void
ifstmt(void)
{
	if (flslvl == 0) {
		if (yyparse())
			++trulvl;
		else
			++flslvl;
		unput('\n');
	} else
		++flslvl;
}

static void
elifstmt(void)
{
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
}

static usch *
svinp(void)
{
	int c;
	usch *cp = stringbuf;

	while ((c = input()) && c != '\n')
		savch(c);
	savstr("\n");
	BEGIN 0;
	return cp;
}

static void
cpperror(void)
{
	usch *cp;

	if (yylex() != WSPACE)
		error("bad error");
	cp = svinp();
	if (flslvl)
		stringbuf = cp;
	else
		error("error: %s", cp);
}

static void
undefstmt(void)
{
	struct symtab *np;

	slow = 1;
	if (yylex() != WSPACE || yylex() != IDENT)
		error("bad undef");
	if (flslvl == 0 && (np = lookup(yytext, FIND)))
		np->value = 0;
	slow = 0;
	chknl();
}

static void
pragmastmt(void)
{
	if (yylex() != WSPACE)
		error("bad pragma");
	/* macexpand() */
	chknl();
}
