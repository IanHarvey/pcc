%{
/*	$Id$	*/

/*
 * Copyright (c) 2002 Anders Magnusson. All rights reserved.
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
%}


D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%{
#include <signal.h>
#include <errno.h>  
#include <string.h>

#include "pass1.h"

static void cvtdig(int radix);
static void charcon(void);
static int floatcon(void);
%}

%%
"auto"			{ yylval.intval = AUTO; return(CLASS); }
"break"			{ yylval.intval = BREAK; return(BREAK); }
"case"			{ yylval.intval = CASE; return(CASE); }
"char"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)CHAR, 0, CHAR);
			  return(TYPE); }
"const"			{ return(CONST); }
"continue"		{ yylval.intval = CONTINUE; return(CONTINUE); }
"default"		{ yylval.intval = DEFAULT; return(DEFAULT); }
"do"			{ yylval.intval = DO; return(DO); }
"double"		{ stwart = instruct;
				yylval.nodep = mkty((TWORD)DOUBLE, 0, DOUBLE);
			  return(TYPE); }
"else"			{ yylval.intval = ELSE; return(ELSE); }
"enum"			{ stwart = SEENAME|TAGNAME; yylval.intval = ENUM;
			  return(ENUM); }
"extern"		{ yylval.intval = EXTERN; return(CLASS); }
"float"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)FLOAT, 0, FLOAT);
			  return(TYPE); }
"for"			{ yylval.intval = FOR; return(FOR); }
"goto"			{ yylval.intval = GOTO; return(GOTO); }
"if"			{ yylval.intval = IF; return(IF); }
"int"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)INT, 0, INT);
			  return(TYPE); }
"long"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)LONG, 0, LONG);
			  return(TYPE); }
"register"		{ yylval.intval = REGISTER; return(CLASS); }
"return"		{ yylval.intval = RETURN; return(RETURN); }
"short"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)SHORT, 0, SHORT);
			  return(TYPE); }
"signed"		{ return(SIGNED); }
"sizeof"		{ yylval.intval = SIZEOF; return(SIZEOF); }
"static"		{ yylval.intval = STATIC; return(CLASS); }
"struct"		{ stwart = INSTRUCT|SEENAME|TAGNAME;
			  yylval.intval = INSTRUCT; return(STRUCT); }
"switch"		{ yylval.intval = SWITCH; return(SWITCH); }
"typedef"		{ yylval.intval = TYPEDEF; return(CLASS); }
"union"			{ stwart = INUNION|SEENAME|TAGNAME;
				yylval.intval = INUNION; return(STRUCT); }
"unsigned"		{ stwart = instruct; yylval.nodep =
				mkty((TWORD)UNSIGNED, 0, UNSIGNED);
			  return(TYPE); }
"void"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)UNDEF, 0, UNDEF);
			  return(TYPE); }
"volatile"		{ return(VOLATILE); }
"while"			{ yylval.intval = WHILE; return(WHILE); }

{L}({L}|{D})*		{ int id; struct symtab *sp;

				id = lookup( hash(yytext),
				    /* tag name for struct/union/enum */
				    (stwart&TAGNAME)? STAG:
				    /* member name for struct/union */
				    (stwart&(INSTRUCT|INUNION|FUNNYNAME)) ?
				    SMOS : 0);
				sp = &stab[id];
				if( sp->sclass == TYPEDEF && !stwart ){
					stwart = instruct;
					yylval.nodep = mkty(sp->stype,
					    sp->dimoff, sp->sizoff);
					return(TYPE);
				}
				stwart = (stwart&SEENAME) ? instruct : 0;
				yylval.intval = id;
				return(NAME);
			}

0[xX]{H}+{IS}?		{ cvtdig(16); return(ICON); }
0{D}+{IS}?		{ cvtdig(8); return(ICON); }
{D}+{IS}?		{ cvtdig(10); return(ICON); }
L?'(\\.|[^\\'])+'	{ charcon(); return(ICON); }

{D}+{E}{FS}?		{ return floatcon(); }
{D}*"."{D}+({E})?{FS}?	{ return floatcon(); }
{D}+"."{D}*({E})?{FS}?	{ return floatcon(); }

L?\"(\\.|[^\\"])*\"	{ return(STRING); }

"..."			{ return(ELLIPSIS); }
">>="			{ yylval.intval = RSEQ; return(ASSIGN); }
"<<="			{ yylval.intval = LSEQ; return(ASSIGN); }
"+="			{ yylval.intval = PLUSEQ; return(ASSIGN); }
"-="			{ yylval.intval = MINUSEQ; return(ASSIGN); }
"*="			{ yylval.intval = MULEQ; return(ASSIGN); }
"/="			{ yylval.intval = DIVEQ; return(ASSIGN); }
"%="			{ yylval.intval = MODEQ; return(ASSIGN); }
"&="			{ yylval.intval = ANDEQ; return(ASSIGN); }
"^="			{ yylval.intval = EREQ; return(ASSIGN); }
"|="			{ yylval.intval = OREQ; return(ASSIGN); }
">>"			{ yylval.intval = RS; return(SHIFTOP); }
"<<"			{ yylval.intval = LS; return(SHIFTOP); }
"++"			{ yylval.intval = INCR; return(INCOP); }
"--"			{ yylval.intval = DECR; return(INCOP); }
"->"			{ yylval.intval=STREF; stwart = FUNNYNAME;
				return(STROP); }
"&&"			{ yylval.intval = ANDAND; return(ANDAND); }
"||"			{ yylval.intval = OROR; return(OROR); }
"<="			{ yylval.intval = LE; return(RELOP); }
">="			{ yylval.intval = GE; return(RELOP); }
"=="			{ yylval.intval = EQ; return(EQUOP); }
"!="			{ yylval.intval = NE; return(EQUOP); }
";"			{ yylval.intval = 0; return(SM); }
("{"|"<%")		{ yylval.intval = 0; return(LC); }
("}"|"%>")		{ yylval.intval = 0; return(RC); }
","			{ yylval.intval = CM; return(CM); }
":"			{ yylval.intval = 0; return(COLON); }
"="			{ yylval.intval = ASSIGN; return(ASSIGN); }
"("			{ yylval.intval = 0; return(LP); }
")"			{ yylval.intval = 0; return(RP); }
("["|"<:")		{ yylval.intval = 0; return(LB); }
("]"|":>")		{ yylval.intval = 0; return(RB); }
"."			{ stwart = FUNNYNAME; yylval.intval = DOT;
				return(STROP); }
"&"			{ yylval.intval = AND; return(AND); }
"!"			{ yylval.intval = NOT; return(UNOP); }
"~"			{ yylval.intval = COMPL; return(UNOP); }
"-"			{ yylval.intval = MINUS; return(MINUS); }
"+"			{ yylval.intval = PLUS; return(PLUS); }
"*"			{ yylval.intval = MUL; return(MUL); }
"/"			{ yylval.intval = DIV; return(DIVOP); }
"%"			{ yylval.intval = MOD; return(DIVOP); }
"<"			{ yylval.intval = LT; return(RELOP); }
">"			{ yylval.intval = GT; return(RELOP); }
"^"			{ yylval.intval = ER; return(ER); }
"|"			{ yylval.intval = OR; return(OR); }
"?"			{ yylval.intval = 0; return(QUEST); }

[ \t\v\f]		{ }
"\n"			{ ++lineno; }
.			{ /* ignore bad characters */ }

%%

int
yywrap(void)
{
	return(1);
}

int yyparse(void);
void yyaccpt(void);
struct sigvec fpe_sigvec;
void fpe(int);

/* control multiple files */
int
main(int argc, char *argv[])
{

	int i;
	char *cp;
	extern int idebug, bdebug, tdebug, edebug;
	extern int ddebug, xdebug, gdebug, adebug;
	extern int proflg;
	extern unsigned int offsz;
	int fdef = 0;
	extern char *release;

	offsz = caloff();
	for( i=1; i<argc; ++i ){
		if( *(cp=argv[i]) == '-' && *++cp == 'X' ){
			while( *++cp ){
				switch( *cp ){

				case 'r':
					fprintf( stderr, "Release: %s\n",
						release );
					break;

				case 'd':
					++ddebug;
					break;
				case 'i':
					++idebug;
					break;
				case 'b':
					++bdebug;
					break;
				case 't':
					++tdebug;
					break;
				case 'e':
					++edebug;
					break;
				case 'x':
					++xdebug;
					break;
				case 'P':	/* profiling */
					++proflg;
					break;
				case 'g':
					++gdebug;
					break;
				case 'a':
					++adebug;
					break;
					}
				}
			}
			else {
			if( *(argv[i]) != '-' ) switch( fdef++ ) {
				case 0:
				case 1:
					if( freopen(argv[i], fdef==1 ? "r" : "w"
, fdef==1 ? stdin : stdout) == NULL) {
						fprintf(stderr, "ccom:can't open
 %s\n", argv[i]);
						exit(1);
					}
					break;

				default:
					;
				}
			}
		}

	(void) p2init( argc, argv );

	for( i=0; i<SYMTSZ; ++i ) stab[i].stype = TNULL;

	lineno = 1;

	tinit();
	mkdope();

	/* dimension table initialization */

	dimtab[NULL] = 0;
	dimtab[CHAR] = SZCHAR;
	dimtab[INT] = SZINT;
	dimtab[FLOAT] = SZFLOAT;
	dimtab[DOUBLE] = SZDOUBLE;
	dimtab[LONG] = SZLONG;
	dimtab[SHORT] = SZSHORT;
	dimtab[UCHAR] = SZCHAR;
	dimtab[USHORT] = SZSHORT;
	dimtab[UNSIGNED] = SZINT;
	dimtab[ULONG] = SZLONG;
	/* starts past any of the above */
	curdim = 16;
	reached = 1;

	fpe_sigvec.sv_handler = fpe;
	(void) sigvec(SIGFPE, &fpe_sigvec, (struct sigvec *) NULL);

	(void) yyparse();
	yyaccpt();

	ejobcode( nerrors ? 1 : 0 );
	return(nerrors?1:0);

	}

int
floatcon(void)
{
	char *eptr;

	dcon = strtod(yytext, &eptr);
	if (*eptr == 'f' || *eptr == 'F') {
		fcon = dcon;
		return FCON;
	}
	return DCON;
}

static int
esccon(char **sptr)
{
	char *wr = *sptr;
	char val;

	switch (*wr++) {
	case 'a': val = '\a'; break;
	case 'b': val = '\b'; break;
	case 'f': val = '\f'; break;
	case 'n': val = '\n'; break;
	case 'r': val = '\r'; break;
	case 't': val = '\t'; break;
	case 'v': val = '\v'; break;
	case 'x': val = strtol(wr, &wr, 16); break;
	case '0': case '1': case '2': case '3': case '4': 
	case '5': case '6': case '7': case '8': case '9': 
		val = strtol(--wr, &wr, 8); break;
	default: val = wr[-1];
	}
	*sptr = wr;
	return val;
}

void
cvtdig(int radix)
{
	CONSZ v;
	char *eptr;

	errno = 0;
	lastcon = strtol(yytext, &eptr, radix);
	if (errno == ERANGE) {
		printf("cvtdig sket sej\n");
	}
	v = lastcon & ~077777L;
	if (v == 0 || v == ~077777L)
		yylval.intval = 0;
	else
	yylval.intval = 1;
	if (*eptr == 'l' || *eptr == 'L')
		yylval.intval = 1;
}

/*
 * Convert a character constant to an integer.
 */
void
charcon(void)
{
	int val, i = 0;
	char *pp = yytext;

	if (*pp == 'L')
		pp++;
	pp++;
	lastcon = 0;
	yylval.intval = 0;
	while (*pp != '\'') {
		if (*pp++ == '\\') {
			val = esccon(&pp);
		} else
			val = pp[-1];
		makecc(val, i);
		i++;
	}

	if (i == 0)
		uerror("empty character constant");
	if (i > (SZINT/SZCHAR) || ( (pflag||hflag)&&i>1))
		uerror("too many characters in character constant");
}

/* match a string or character constant, up to lxmatch */
void
lxstr(int ct)
{
	char *wr = yytext;
	int i = 0, val;

	/* Assume that lex works */
	if (*wr++ == 'L')		/* Skip first '"' */
		wr++;			/* Skip 'L' if exists */
	yytext[strlen(yytext)-1] = 0;	/* Skip last '"' */

	while (*wr != 0) {
		if (*wr++ == '\\')
			val = esccon(&wr);
		else
			val = wr[-1];
		if (strflg) {
			if (ct == 0 || i < ct)
				putbyte(val);
			else if (i == ct)
				werror("non-null byte ignored in string"
				    "initializer");
		} else
			bycode(val, i);
		i++;
	}

	if (strflg) {
		if (ct == 0 || i < ct)
			putbyte(0);
	} else {
		bycode(0, i++);
		bycode(-1, i);
		dimtab[curdim] = i; /* in case of later sizeof ... */
	}
}

#define NSAVETAB	4096
char	*savetab;
unsigned saveleft;

char *
savestr(char *cp)
{
	int len;

	len = strlen(cp) + 1;
	if (len > saveleft) {
		saveleft = NSAVETAB;
		if (len > saveleft)
			saveleft = len;
		savetab = malloc(saveleft);
		if (savetab == 0)
			cerror("Ran out of memory (savestr)");
	}
	(void) strncpy(savetab, cp, len);
	cp = savetab;
	savetab += len;
	saveleft -= len;
	return (cp);
}

/*
 * The definition for the segmented hash tables.
 */
#define MAXHASH 20
#define HASHINC 1013
struct ht {
	char	**ht_low;
	char	**ht_high;
	int	ht_used;
} htab[MAXHASH];

char *
hash(char *s)
{
	char **h;
	int i;
	char *cp;
	struct ht *htp;
	int sh;
	/*
	 * The hash function is a modular hash of
	 * the sum of the characters with the sum
	 * doubled before each successive character
	 * is added.
	 */
	cp = s;
	i = 0;
	while (*cp)
		i = i*2 + *cp++;
	sh = (i&077777) % HASHINC;
	cp = s;
	/*
	 * There are as many as MAXHASH active
	 * hash tables at any given point in time.
	 * The search starts with the first table
	 * and continues through the active tables
	 * as necessary.
	 */
	for (htp = htab; htp < &htab[MAXHASH]; htp++) {
		if (htp->ht_low == 0) {
			char **hp =
			    (char **) calloc(sizeof (char **), HASHINC);
			if (hp == 0)
				cerror("ran out of memory (hash)");
			htp->ht_low = hp;
			htp->ht_high = htp->ht_low + HASHINC;
		}
		h = htp->ht_low + sh;
		/*
		 * quadratic rehash increment
		 * starts at 1 and incremented
		 * by two each rehash.
		 */
		i = 1;
		do {
			if (*h == 0) {
				if (htp->ht_used > (HASHINC * 3)/4)
					break;
				htp->ht_used++;
				*h = savestr(cp);
				return (*h);
			}
			if (**h == *cp && strcmp(*h, cp) == 0)
				return (*h);
			h += i;
			i += 2;
			if (h >= htp->ht_high)
				h -= HASHINC;
		} while (i < HASHINC);
	}
	cerror("ran out of hash tables");
	/*NOTREACHED*/
	return 0;
}

