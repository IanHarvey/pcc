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
#include <string.h>
int yylex2(void);
#define yylex yylex2
%}

%term number stop
%term EQ NE LE GE LS RS
%term ANDAND OROR
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
%%
S:	e stop	={ return($1);}


e:	  e '*' e
		={$$ = $1 * $3;}
	| e '/' e
		={$$ = $1 / $3;}
	| e '%' e
		={$$ = $1 % $3;}
	| e '+' e
		={$$ = $1 + $3;}
	| e '-' e
		={$$ = $1 - $3;}
	| e LS e
		={$$ = $1 << $3;}
	| e RS e
		={$$ = $1 >> $3;}
	| e '<' e
		={$$ = $1 < $3;}
	| e '>' e
		={$$ = $1 > $3;}
	| e LE e
		={$$ = $1 <= $3;}
	| e GE e
		={$$ = $1 >= $3;}
	| e EQ e
		={$$ = $1 == $3;}
	| e NE e
		={$$ = $1 != $3;}
	| e '&' e
		={$$ = $1 & $3;}
	| e '^' e
		={$$ = $1 ^ $3;}
	| e '|' e
		={$$ = $1 | $3;}
	| e ANDAND e
		={$$ = $1 && $3;}
	| e OROR e
		={$$ = $1 || $3;}
	| e '?' e ':' e
		={$$ = $1 ? $3 : $5;}
	| e ',' e
		={$$ = $3;}
	| term
		={$$ = $1;}
term:
	  '-' term %prec UMINUS
		={$$ = -$1;}
	| '!' term
		={$$ = !$1;}
	| '~' term
		={$$ = ~$1;}
	| '(' e ')'
		={$$ = $2;}
	| number
		={$$= $1;}
%%

#undef yylex
#include "cpp.h"

static int gotdef;

void
yyerror(char *err)
{
	error(err);
}

int
yylex2(void)
{
	struct symtab *nl;
	int c;
	usch *osp;

again:	c = yylex();
	switch (c) {
	case NUMBER:
		yylval = atoi(yytext);
		return number;

	case WSPACE:
		goto again;

	case IDENT:
		/* first check for the special "defined" keyword */
		if (strcmp(yytext, "defined") == 0) {
			int par, d;
			gotdef = 1;
			if ((par = c = yylex2()) == '(')
				c = yylex2();
			if (c != IDENT)
				goto bad;
			d = (lookup(yytext, FIND) != NULL);
			if (par == '(' && ((c = yylex2()) != ')'))
				goto bad;
			gotdef = 0;
			yylval = d;
			return number;
		}
		if (gotdef)
			return IDENT;

		/* Is this a defined macro? */
		yylval = 0;
		if ((nl = lookup(yytext, FIND)) == NULL)
			return number;
		osp = stringbuf;
		if (subst(yytext, nl, NULL) == 0)
			return number; /* failed substitution */
		while (stringbuf > osp)
			cunput(*--stringbuf);
		goto again;

	case '=':
		if ((c = yylex()) == '=')
			return EQ;
		c = '=';
		break;

	case '!':
		if ((c = yylex()) == '=')
			return NE;
		c = '!';
		break;

	case '<':
		if ((c = yylex()) == '=')
			return LE;
		if (c == '<')
			return LS;
		c = '<';
		break;

	case '>':
		if ((c = yylex()) == '=')
			return GE;
		if (c == '>')
			return RS;
		c = '>';
		break;

	case '|':
		if ((c = yylex()) == '|')
			return OROR;
		c = '|';
		break;

	case '&':
		if ((c = yylex()) == '&')
			return ANDAND;
		c = '&';
		break;

	case NL:
		putc('\n', obuf);
		return stop;

	default:
		if (c < 256)
			return c;
bad:		error("bad #if token %d", c);
	}
	unpstr(yytext);
	return c;
}
#define yylex yylex2
