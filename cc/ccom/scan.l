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
static void control(int);
static int floatcon(void);

#define	CPP_PRAGMA	1
#define	CPP_IDENT 	2
#define	CPP_LINE 	3
#define	CPP_HASH	4
%}

%%
"asm"			{ return(ASM); }
"auto"			{ yylval.intval = AUTO; return(CLASS); }
"break"			{ yylval.intval = BREAK; return(BREAK); }
"case"			{ yylval.intval = CASE; return(CASE); }
"char"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)CHAR, 0, CHAR);
			  return(TYPE); }
"const"			{ yylval.nodep =
				block(QUALIFIER, NIL, NIL, CONST, 0, 0);
			  return(QUALIFIER); }
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
"inline"		{ return(FUNSPEC); }
"int"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)INT, 0, INT);
			  return(TYPE); }
"long"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)LONG, 0, LONG);
			  return(TYPE); }
"register"		{ yylval.intval = REGISTER; return(CLASS); }
"restrict"		{ ; /* just ignore */ }
"return"		{ yylval.intval = RETURN; return(RETURN); }
"short"			{ stwart = instruct;
				yylval.nodep = mkty((TWORD)SHORT, 0, SHORT);
			  return(TYPE); }
"signed"		{ stwart = instruct; yylval.nodep =
				mkty((TWORD)SIGNED, 0, SIGNED);
			  return(TYPE); }
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
"volatile"		{ yylval.nodep =
				block(QUALIFIER, NIL, NIL, VOLATILE, 0, 0);
			  return(QUALIFIER); }
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

L?\"(\\.|[^\\"])*\"	{ char *c = yytext;

				if (*c++ == 'L')
					c++;
				c[strlen(c)-1] = 0; /* last " */
				yylval.strp = strdup(c);
				return(STRING);
			}

"..."			{ return(ELLIPSIS); }
">>="			{ yylval.intval = RSEQ; return(ASOP); }
"<<="			{ yylval.intval = LSEQ; return(ASOP); }
"+="			{ yylval.intval = PLUSEQ; return(ASOP); }
"-="			{ yylval.intval = MINUSEQ; return(ASOP); }
"*="			{ yylval.intval = MULEQ; return(ASOP); }
"/="			{ yylval.intval = DIVEQ; return(ASOP); }
"%="			{ yylval.intval = MODEQ; return(ASOP); }
"&="			{ yylval.intval = ANDEQ; return(ASOP); }
"^="			{ yylval.intval = EREQ; return(ASOP); }
"|="			{ yylval.intval = OREQ; return(ASOP); }
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
^#pragma[ \t].*		{ control(CPP_PRAGMA); }
^#ident[ \t].*		{ control(CPP_IDENT); }
^#line[ \t].*		{ control(CPP_LINE); }
^#.*			{ control(CPP_HASH); }

[ \t\v\f]		{ }
"\n"			{ ++lineno; }
.			{ /* ignore bad characters */ }

%%

int
yywrap(void)
{
	return(1);
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

int
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
	char *ch = yytext;
	int n;

	if (radix == 16)
		ch += 2; /* Skip 0x */
	
	lastcon = 0;
	while ((*ch >= '0' && *ch <= '9') || (*ch >= 'a' && *ch <= 'f') ||
	    (*ch >= 'A' && *ch <= 'F')) {
		lastcon *= radix;
		n = *ch;
		n = (n <= '9' ? n - '0' : (n > 'F' ? n - 'a' : n - 'A') + 10);
		ch++;
		lastcon += n;
	}
	v = lastcon & ~077777L;
	if (v == 0 || v == ~077777L)
		yylval.intval = 0;
	else
		yylval.intval = 1;
	if (*ch == 'l' || *ch == 'L')
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
	if (i > (SZINT/SZCHAR) || ( (pflag)&&i>1))
		uerror("too many characters in character constant");
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

void
control(int t)
{
	char *wr = yytext;
	char *fr = ftitle;
	char *eptr;
	int val;

	wr++;	/* Skip initial '#' */
	switch (t) {
	case CPP_PRAGMA:
	case CPP_IDENT:
		return;	/* Just skip these for now. */

	case CPP_LINE:
		wr += 4;
		/* FALLTHROUGH */
	case CPP_HASH:
		val = strtol(wr, &eptr, 10);
		if (wr == eptr)	/* Illegal string */
			goto bad;
		wr = eptr;
		lineno = val;
		while (*wr && *wr != '\"')
			wr++;
		if (*wr++ != '\"')
			goto bad;
		while (*wr && *wr != '\"')
			*fr++ = *wr++;
		*fr = 0;
	}
	return;
bad:
	werror("%s: illegal control", yytext);
}
