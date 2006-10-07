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
static int charcon(void);
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

#define	ACK	6	/* not legal input char */

static int inch(void);

static int scale, inmac;

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
#define fprintf(x, ...) error(__VA_ARGS__)
#define	ECHO putstr(yytext)
#undef fileno
#define fileno(x) 0

#if YY_FLEX_SUBMINOR_VERSION >= 31
/* Hack to avoid unneccessary warnings */
FILE *yyget_in	(void);
FILE *yyget_out  (void);
int yyget_leng	(void);
char *yyget_text  (void);
void yyset_in (FILE *  in_str );
void yyset_out (FILE *	out_str );
int yyget_debug  (void);
void yyset_debug (int  bdebug );
int yylex_destroy  (void);
#endif
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
					if (ifiles->lineno == 1)
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
<IFR>"defined"		{	int p, c;
				inmac++;
				if ((p = c = yylex()) == '(')
					c = yylex();
				if (c != IDENT || (p != IDENT && p != '('))
					error("syntax error");
				if (p == '(' && yylex() != ')')
					error("syntax error");
				inmac--;
				return IDENT;
			}

<IFR>{WS}+		{ ; }
<IFR>{L}({L}|{D})*	{
				struct symtab *nl;
				/*
				 * Tricky: expand everything once, push it
				 * back on the input and then do not expand
				 * it again.
				 */
				if (inmac == 0) {
					if ((nl = lookup(yytext, FIND))) {
						usch *rv, *sp = stringbuf;
						rv = gotident(nl);
						cunput(ACK);
						unpstr(rv);
						stringbuf = sp;
						inmac = 1;
					} else {
						yylval.nl = 0;
						return IDENT;
					}
				} else {
					yylval.nl = lookup(yytext, FIND);
					return IDENT;
				}
			}

0[xX]{H}+{IS}?		{	scale = 16;
			num:	if (YYSTATE)
					yylval.val = cvtdig(scale);
				PRTOUT(NUMBER);
			}
0{D}+{IS}?		{ scale = 8; goto num; }
{D}+{IS}?		{ scale = 10; goto num; }
L?'(\\.|[^\\'])+'	{	if (YYSTATE)
					yylval.val = charcon();
				PRTOUT(NUMBER);
			}

<IFR>"\006"		{ inmac = 0; /* end of pushbacked macro */ }
<IFR>.			{ return yytext[0]; }

{D}+{E}{FS}?		{ PRTOUT(FPOINT); }
{D}*"."{D}+({E})?{FS}?	{ PRTOUT(FPOINT); }
{D}+"."{D}*({E})?{FS}?	{ PRTOUT(FPOINT); }

^{WS}*#{WS}*		{ BEGIN CONTR; }
{WS}+			{ PRTOUT(WSPACE); }

<CONTR>"ifndef"		{ ifndefstmt(); }
<CONTR>"ifdef"		{ ifdefstmt(); }
<CONTR>"if"{WS}+	{ BEGIN IFR; ifstmt(); BEGIN 0; }
<CONTR>"include"	{ BEGIN 0; include(); prtline(); }
<CONTR>"else"		{ elsestmt(); }
<CONTR>"endif"		{ endifstmt(); }
<CONTR>"error"		{ cpperror(); BEGIN 0; }
<CONTR>"define"		{ BEGIN DEF; define(); BEGIN 0; }
<CONTR>"undef"		{ undefstmt(); }
<CONTR>"line"		{ BEGIN 0; line(); }
<CONTR>"pragma"		{ pragmastmt(); }
<CONTR>"elif"{WS}+	{ BEGIN IFR; elifstmt(); BEGIN 0; }



"//".*$			{ /* if (tflag) yyless(..) */
				if (Cflag)
					putstr(yytext);
				else if (!flslvl)
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
				if (!tflag && !Cflag && !flslvl)
					unput(' ');
			}

<DEF>"##"		{ return CONCAT; }
<DEF>"#"		{ return MKSTR; }
<DEF>"..."		{ return ELLIPS; }
<DEF>"__VA_ARGS__"	{ return VA_ARGS; }

L?\"(\\.|[^\\"])*\"	{ PRTOUT(STRING); }
{L}({L}|{D})*		{
				struct symtab *nl;
				if (slow)
					return IDENT;
				if (flslvl)
					; /* do nothing */
				else if ((nl = lookup(yytext, FIND)) != 0) {
					usch *op = stringbuf;
					putstr(gotident(nl));
					stringbuf = op;
				} else
					putstr(yytext);
			}

.			{ PRTOUT(yytext[0]); }

%%

usch *yyp, yybuf[CPPBUF];

int yylex(void);
int yywrap(void);

static int
inpch(void)
{
	int len;

	if (ifiles->curptr < ifiles->maxread)
		return *ifiles->curptr++;

	if (ifiles->infil < 0) {
		ifiles = ifiles->next;
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

#define unch(c) *--ifiles->curptr = c

static int
inch(void)
{
	int c;

again:	switch (c = inpch()) {
	case '\\': /* continued lines */
		if ((c = inpch()) == '\n') {
			ifiles->lineno++;
			putch('\n');
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
	int c, otrulvl;

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

	otrulvl = trulvl;

	if ((c = yylex()) != 0)
		error("yylex returned %d", c);

	if (otrulvl != trulvl || flslvl)
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
	usch *os = stringbuf;

	putstr(sheap("# %d \"%s\"\n", ifiles->lineno, ifiles->fname));
	stringbuf = os;
}

void
cunput(int c)
{
#ifdef CPP_DEBUG
	extern int dflag;
	if (dflag)printf(": '%c'(%d)", c, c);
#endif
	unput(c);
}

int yywrap(void) { return 1; }

static int
dig2num(int c)
{
	if (c >= 'a')
		c = c - 'a' + 10;
	else if (c >= 'A')
		c = c - 'A' + 10;
	else
		c = c - '0';
	return c;
}

static long long
cvtdig(int rad)
{
	long long rv = 0;
	char *y = yytext;
	int c;

	c = *y++;
	if (rad == 16)
		y++;
	while (isxdigit(c)) {
		rv = rv * rad + dig2num(c);
		c = *y++;
	}
	return rv;
}

static int
charcon(void)
{
	usch *p = (usch *)yytext;
	int val, c;

	if (*p == 'L')
		p++;
	p++; /* first ' */
	val = 0;
	if (*p++ == '\\') {
		switch (*p++) {
		case 'a': val = '\a'; break;
		case 'b': val = '\b'; break;
		case 'f': val = '\f'; break;
		case 'n': val = '\n'; break;
		case 'r': val = '\r'; break;
		case 't': val = '\t'; break;
		case 'v': val = '\v'; break;
		case '\"': val = '\"'; break;
		case '\'': val = '\''; break;
		case '\\': val = '\\'; break;
		case 'x':
			while (isxdigit(c = *p)) {
				val = val * 16 + dig2num(c);
				p++;
			}
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7':
			p--;
			while (isdigit(c = *p)) {
				val = val * 8 + (c - '0');
				p++;
			}
			break;
		default: val = p[-1];
		}

	} else
		val = p[-1];
	return val;
}

static void
chknl(void)
{
	int t;

	while ((t = yylex()) == WSPACE)
		;
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
	slow = 1;
	if (yylex() != WSPACE || yylex() != IDENT)
		error("bad ifdef");
	slow = 0;
	if (flslvl == 0 && lookup(yytext, FIND) != 0)
		trulvl++;
	else
		flslvl++;
	chknl();
}

static void
ifndefstmt(void)	  
{ 
	slow = 1;
	if (yylex() != WSPACE || yylex() != IDENT)
		error("bad ifndef");
	slow = 0;
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
		slow = 1;
		if (yyparse())
			++trulvl;
		else
			++flslvl;
		slow = 0;
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
			slow = 1;
			if (yyparse()) {
				++trulvl;
				prtline();
			} else
				++flslvl;
			slow = 0;
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
	int c;

	if (flslvl)
		return;
	c = yylex();
	if (c != WSPACE && c != '\n')
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
	slow = 1;
	if (yylex() != WSPACE)
		error("bad pragma");
	while (yylex() != '\n')  /* no pragma support */
		;
	slow = 0;
}
