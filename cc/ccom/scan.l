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
"asm"			{ return(C_ASM); }
"auto"			{ yylval.intval = AUTO; return(C_CLASS); }
"break"			{ return(C_BREAK); }
"case"			{ return(C_CASE); }
"char"			{ yylval.nodep = mkty((TWORD)CHAR, 0, CHAR);
			  return(C_TYPE); }
"const"			{ yylval.nodep =
				block(QUALIFIER, NIL, NIL, CONST, 0, 0);
			  return(C_QUALIFIER); }
"continue"		{ return(C_CONTINUE); }
"default"		{ return(C_DEFAULT); }
"do"			{ return(C_DO); }
"double"		{ yylval.nodep = mkty((TWORD)DOUBLE, 0, DOUBLE);
			  return(C_TYPE); }
"else"			{ return(C_ELSE); }
"enum"			{ return(C_ENUM); }
"extern"		{ yylval.intval = EXTERN; return(C_CLASS); }
"float"			{ yylval.nodep = mkty((TWORD)FLOAT, 0, FLOAT);
			  return(C_TYPE); }
"for"			{ return(C_FOR); }
"goto"			{ return(C_GOTO); }
"if"			{ return(C_IF); }
"inline"		{ return(C_FUNSPEC); }
"int"			{ yylval.nodep = mkty((TWORD)INT, 0, INT);
			  return(C_TYPE); }
"long"			{ yylval.nodep = mkty((TWORD)LONG, 0, LONG);
			  return(C_TYPE); }
"register"		{ yylval.intval = REGISTER; return(C_CLASS); }
"restrict"		{ ; /* just ignore */ }
"return"		{ return(C_RETURN); }
"short"			{ yylval.nodep = mkty((TWORD)SHORT, 0, SHORT);
			  return(C_TYPE); }
"signed"		{ yylval.nodep = mkty((TWORD)SIGNED, 0, SIGNED);
			  return(C_TYPE); }
"sizeof"		{ return(C_SIZEOF); }
"static"		{ yylval.intval = STATIC; return(C_CLASS); }
"struct"		{ yylval.intval = INSTRUCT; return(C_STRUCT); }
"switch"		{ return(C_SWITCH); }
"typedef"		{ yylval.intval = TYPEDEF; return(C_CLASS); }
"union"			{ yylval.intval = INUNION; return(C_STRUCT); }
"unsigned"		{ yylval.nodep = mkty((TWORD)UNSIGNED, 0, UNSIGNED);
			  return(C_TYPE); }
"void"			{ yylval.nodep = mkty((TWORD)UNDEF, 0, UNDEF);
			  return(C_TYPE); }
"volatile"		{ yylval.nodep =
				block(QUALIFIER, NIL, NIL, VOLATILE, 0, 0);
			  return(C_QUALIFIER); }
"while"			{ return(C_WHILE); }

{L}({L}|{D})*		{ 	struct symtab *s;

				yylval.strp = addname(yytext);
				s = lookup(yylval.strp, SNOCREAT);
				if (s && got_type == 0 && s->sclass == TYPEDEF)
					return got_type = 1, C_TYPENAME;
				got_type = 0;
				return(C_NAME);
			}

0[xX]{H}+{IS}?		{ cvtdig(16); return(C_ICON); }
0{D}+{IS}?		{ cvtdig(8); return(C_ICON); }
{D}+{IS}?		{ cvtdig(10); return(C_ICON); }
L?'(\\.|[^\\'])+'	{ charcon(); return(C_ICON); }

{D}+{E}{FS}?		{ return floatcon(); }
{D}*"."{D}+({E})?{FS}?	{ return floatcon(); }
{D}+"."{D}*({E})?{FS}?	{ return floatcon(); }

L?\"(\\.|[^\\"])*\"	{	char *c = yytext;
				int i;

				if (*c++ == 'L')
					c++;
				c[(i = strlen(c))-1] = 0; /* last " */
				yylval.strp = tmpalloc(i);
				memcpy(yylval.strp, c, i);
				return(C_STRING);
			}

"..."			{ return(C_ELLIPSIS); }
">>="			{ yylval.intval = RSEQ; return(C_ASOP); }
"<<="			{ yylval.intval = LSEQ; return(C_ASOP); }
"+="			{ yylval.intval = PLUSEQ; return(C_ASOP); }
"-="			{ yylval.intval = MINUSEQ; return(C_ASOP); }
"*="			{ yylval.intval = MULEQ; return(C_ASOP); }
"/="			{ yylval.intval = DIVEQ; return(C_ASOP); }
"%="			{ yylval.intval = MODEQ; return(C_ASOP); }
"&="			{ yylval.intval = ANDEQ; return(C_ASOP); }
"^="			{ yylval.intval = EREQ; return(C_ASOP); }
"|="			{ yylval.intval = OREQ; return(C_ASOP); }
">>"			{ yylval.intval = RS; return(C_SHIFTOP); }
"<<"			{ yylval.intval = LS; return(C_SHIFTOP); }
"++"			{ yylval.intval = INCR; return(C_INCOP); }
"--"			{ yylval.intval = DECR; return(C_INCOP); }
"->"			{ yylval.intval = STREF; return(C_STROP); }
"&&"			{ yylval.intval = ANDAND; return(C_ANDAND); }
"||"			{ yylval.intval = OROR; return(C_OROR); }
"<="			{ yylval.intval = LE; return(C_RELOP); }
">="			{ yylval.intval = GE; return(C_RELOP); }
"=="			{ yylval.intval = EQ; return(C_EQUOP); }
"!="			{ yylval.intval = NE; return(C_EQUOP); }
";"			{ return(';'); }
("{"|"<%")		{ return('{'); }
("}"|"%>")		{ return('}'); }
","			{ return(','); }
":"			{ return(':'); }
"="			{ return('='); }
"("			{ return('('); }
")"			{ return(')'); }
("["|"<:")		{ return('['); }
("]"|":>")		{ return(']'); }
"."			{ yylval.intval = DOT; return(C_STROP); }
"&"			{ return('&'); }
"!"			{ yylval.intval = NOT; return(C_UNOP); }
"~"			{ yylval.intval = COMPL; return(C_UNOP); }
"-"			{ return('-'); }
"+"			{ return('+'); }
"*"			{ return('*'); }
"/"			{ yylval.intval = DIV; return(C_DIVOP); }
"%"			{ yylval.intval = MOD; return(C_DIVOP); }
"<"			{ yylval.intval = LT; return(C_RELOP); }
">"			{ yylval.intval = GT; return(C_RELOP); }
"^"			{ return('^'); }
"|"			{ return('|'); }
"?"			{ return('?'); }
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
		return C_FCON;
	}
	return C_DCON;
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
