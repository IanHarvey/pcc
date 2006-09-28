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

%{
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
void yyerror(char *);
%}

%term stop
%term EQ NE LE GE LS RS
%term ANDAND OROR
/*
 * The following terminals are not used in the yacc code.
 */
%term STRING CHARCON NUMBER FPOINT WSPACE
%term IDENT CONCAT MKSTR ELLIPS DEFINED

%left ','
%right '='
%right '?' ':'
%left OROR
%left ANDAND
%left '|' '^'
%left '&'
%binary EQ NE
%binary '<' '>' LE GE
%left LS RS
%left '+' '-'
%left '*' '/' '%'
%right '!' '~' UMINUS
%left '(' '.'

%union {
	long long val;
	struct symtab *nl;
	char *str;
}

%type <val> term NUMBER e
%type <nl> IDENT

%%
S:	e '\n'	{ return($1 != 0);}


e:	  e '*' e
		{$$ = $1 * $3;}
	| e '/' e
		{$$ = $1 / $3;}
	| e '%' e
		{$$ = $1 % $3;}
	| e '+' e
		{$$ = $1 + $3;}
	| e '-' e
		{$$ = $1 - $3;}
	| e LS e
		{$$ = $1 << $3;}
	| e RS e
		{$$ = $1 >> $3;}
	| e '<' e
		{$$ = $1 < $3;}
	| e '>' e
		{$$ = $1 > $3;}
	| e LE e
		{$$ = $1 <= $3;}
	| e GE e
		{$$ = $1 >= $3;}
	| e EQ e
		{$$ = $1 == $3;}
	| e NE e
		{$$ = $1 != $3;}
	| e '&' e
		{$$ = $1 & $3;}
	| e '^' e
		{$$ = $1 ^ $3;}
	| e '|' e
		{$$ = $1 | $3;}
	| e ANDAND e
		{$$ = $1 && $3;}
	| e OROR e
		{$$ = $1 || $3;}
	| e '?' e ':' e
		{$$ = $1 ? $3 : $5;}
	| e ',' e
		{$$ = $3;}
	| term
		{$$ = $1;}
term:
	  '-' term %prec UMINUS
		{$$ = -$2;}
	| '!' term
		{$$ = !$2;}
	| '~' term
		{$$ = ~$2;}
	| '(' e ')'
		{$$ = $2;}
	| NUMBER
		{$$= $1;}
	| DEFINED IDENT { $$ = $2 != NULL; }
	| DEFINED '(' IDENT ')' { $$ = $3 != NULL; }
%%

#include "cpp.h"

void
yyerror(char *err)
{
	error(err);
}

#if 0

static int
charcon(void)
{
	extern usch *yys;
	int val, c;

	val = 0;
	if (*yys++ == '\\') {
		switch (*yys++) {
		case 'a': val = '\a'; break;
		case 'b': val = '\b'; break;
		case 'f': val = '\f'; break;
		case 'n': val = '\n'; break;
		case 'r': val = '\r'; break;
		case 't': val = '\t'; break;
		case 'v': val = '\v'; break;
		case '\"': val = '\"'; break;
		case '\\': val = '\\'; break;
		case 'x': 
			while (isxdigit(c = *yys)) {
				if (c >= 'a')
					c = c - 'a' + 10;
				else if (c >= 'A')
					c = c - 'A' + 10;
				else
					c = c - '0';
				val = val * 16 + c;
				yys++;
			}
			break;
		case '0': case '1': case '2': case '3': case '4': 
		case '5': case '6': case '7':
			yys--;
			while (isdigit(c = *yys)) {
				val = val * 8 + (c - '0');
				yys++;
			}
			break;
		default: val = yys[-1];
		}
	} else
		val = yys[-1];
	if (*yys++ != '\'')
		error("bad char const");
	return val;
}

int
yylex(void)
{
	extern usch *yys;
	int c;
	usch *sp, *s;

	sp = stringbuf;
again:	c = *yys++;
	if (isdigit(c)) {
		/* may be decimal, octal or hex */
		yylval.val = 0;
		if (c == '0') {
			c = *yys++;
			if (c == 'x' || c == 'X') {
				/* hex digit */
				c = *yys++;
				while (isxdigit(c)) {
					if (c >= 'a')
						c = c - 'a' + 10;
					else if (c >= 'A')
						c = c - 'A' + 10;
					else
						c = c - '0';
					yylval.val = yylval.val * 16 + c;
				}
			} else {
				/* octal */
				while (isdigit(c)) {
					yylval.val = yylval.val * 8 + c - '0';
					c = *yys++;
				}
			}
		} else {
			while (isdigit(c)) {
				yylval.val = yylval.val * 10 + c - '0';
				c = *yys++;
			}
		}
		yys--;
		return ICON;
	} else if (c == ' ' || c == '\t') {
		goto again;
	} else if (isalpha(c)) {
		if (c == 'd' && strncmp(yys, "efined", 6) == 0 && 
		    (yys[6] == ' ' || yys[6] == '\t' || yys[6] == '(')) {
			int par;

			yys += 6;
			if ((par = c = *yys++) == '(')
				c = *yys++;
			while (c == ' ' || c == '\t') c = *yys++;
			if (!isalpha(c) && c != '_')
				goto bad;
			s = yys-1;
			while (isalnum(*yys) || *yys == '_')
				yys++;
			c = *yys; *yys = 0;
			yylval.val = (lookup(s, FIND) != NULL);
			*yys = c;
			while (c == ' ' || c == '\t') c = *yys++;
			if (par == '(' && c != ')')
				goto bad;
			return ICON;
		}
		/* everything is already macro-replaced, remaining is 0 */
		yylval.val = 0;
		while (isalnum(*yys) || *yys == '_')
			yys++;
		return ICON;
	}
	switch (c) {

	case '=':
		if ((c = *yys++) == '=')
			return EQ;
		c = '=';
		break;

	case '!':
		if ((c = *yys++) == '=')
			return NE;
		c = '!';
		break;

	case '<':
		if ((c = *yys++) == '=')
			return LE;
		if (c == '<')
			return LS;
		c = '<';
		break;

	case '>':
		if ((c = *yys++) == '=')
			return GE;
		if (c == '>')
			return RS;
		c = '>';
		break;

	case '|':
		if ((c = *yys++) == '|')
			return OROR;
		c = '|';
		break;

	case '&':
		if ((c = *yys++) == '&')
			return ANDAND;
		c = '&';
		break;

	case 0:
		return stop;

	case '\'':
		yylval.val = charcon();
		return ICON;

	default:
		return c;
	}
	yys--;
	return c;
bad:	error("bad #if token %d", c);
	return 0; /* XXX GCC */
}
#endif
