/*	$Id$	*/

/*
 * Copyright (c) 2004,2009 Anders Magnusson. All rights reserved.
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
 * Tokenizer for the C preprocessor.
 * There are three main routines:
 *	- fastscan() loops over the input stream searching for magic
 *		characters that may require actions.
 *	- sloscan() tokenize the input stream and returns tokens.
 *	- yylex() returns something from the input stream that
 *		is suitable for yacc.
 *
 *	Other functions of common use:
 *	- inpch() returns a raw character from the current input stream.
 *	- inch() is like inpch but \\n and trigraphs are expanded.
 *	- unch() pushes back a character to the input stream.
 *
 * Input data can be read from either stdio or a buffer.
 * If a buffer is read, it will return EOF when ended and then jump back
 * to the previous buffer.
 *	- setibuf(usch *ptr). Buffer to read from, until NULL, return EOF.
 *		When EOF returned, pop buffer.
 *	- setobuf(usch *ptr).  Buffer to write to
 *
 * There are three places data is read:
 *	- fastscan() which has a small loop that will scan over input data.
 *	- flscan() where everything is skipped except directives (flslvl)
 *	- inch() that everything else uses.
 *
 * 5.1.1.2 Translation phases:
 *	1) Convert UCN to UTF-8 which is what pcc uses internally (chkucn).
 *	   Remove \r (unwanted)
 *	   Convert trigraphs (chktg)
 *	2) Remove \\\n.  Need extra care for identifiers and #line.
 *	3) Tokenize.
 *	   Remove comments (fastcmnt)
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>

#include "compat.h"
#include "cpp.h"

static void cvtdig(int);
static int dig2num(int);
static int charcon(usch *);
static void elsestmt(void);
static void ifdefstmt(void);
static void ifndefstmt(void);
static void endifstmt(void);
static void ifstmt(void);
static void cpperror(void);
static void cppwarning(void);
static void undefstmt(void);
static void pragmastmt(void);
static void elifstmt(void);

static int inpch(void);
static int chktg(void);
static int chkucn(void);
static void unch(int c);

#define	PUTCH(ch) if (!flslvl) putch(ch)
/* protection against recursion in #include */
#define MAX_INCLEVEL	100
static int inclevel;

usch yytext[CPPBUF];

struct includ *ifiles;

/* some common special combos for init */
#define C_NL	(C_SPEC|C_WSNL)
#define C_DX	(C_SPEC|C_ID|C_DIGIT|C_HEX)
#define C_I	(C_SPEC|C_ID|C_ID0)
#define C_IP	(C_SPEC|C_ID|C_ID0|C_EP)
#define C_IX	(C_SPEC|C_ID|C_ID0|C_HEX)
#define C_IXE	(C_SPEC|C_ID|C_ID0|C_HEX|C_EP)

usch spechr[256] = {
	0,	0,	0,	0,	C_SPEC,	C_SPEC,	0,	0,
	0,	C_WSNL,	C_NL,	0,	0,	C_WSNL,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,

	C_WSNL,	C_2,	C_SPEC,	0,	0,	0,	C_2,	C_SPEC,
	0,	0,	0,	C_2,	0,	C_2,	0,	C_SPEC|C_2,
	C_DX,	C_DX,	C_DX,	C_DX,	C_DX,	C_DX,	C_DX,	C_DX,
	C_DX,	C_DX,	0,	0,	C_2,	C_2,	C_2,	0,

	0,	C_IX,	C_IX,	C_IX,	C_IX,	C_IXE,	C_IX,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_IP,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	0,	C_SPEC,	0,	0,	C_I,

	0,	C_IX,	C_IX,	C_IX,	C_IX,	C_IXE,	C_IX,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_IP,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	0,	C_2,	0,	0,	0,

/* utf-8 */
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,

	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,

	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,

	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,	C_I,
};

/*
 * fill up the input buffer
 */
static int
inpbuf(void)
{
	int len;

	if (ifiles->infil == -1)
		return 0;
	len = read(ifiles->infil, ifiles->buffer, CPPBUF);
	if (len == -1)
		error("read error on file %s", ifiles->orgfn);
	if (len > 0) {
		ifiles->buffer[len] = 0;
		ifiles->curptr = ifiles->buffer;
		ifiles->maxread = ifiles->buffer + len;
	}
	return len;
}

/*
 * return a raw character from the input stream
 */
static inline int
inpch(void)
{

	do {
		if (ifiles->curptr < ifiles->maxread)
			return *ifiles->curptr++;
	} while (inpbuf() > 0);

	return -1;
}

/*
 * push a character back to the input stream
 */
static void
unch(int c)
{
	if (c == -1)
		return;

	if (ibufp) {
		ibufp--;
		if (*ibufp != c)
			error("unch");
		return;
	}

	ifiles->curptr--;
	if (ifiles->curptr < ifiles->bbuf)
		error("pushback buffer full");
	*ifiles->curptr = (usch)c;
}

static int
ibread(void)
{
	int ch = *ibufp++;
	if (ch == 0) {
		ibufp = NULL;
		ch = WARN;
	}
	return ch;
}

/*
 * Check for (and convert) trigraphs.
 */
static int
chktg(void)
{
	int ch;

	if ((ch = inpch()) != '?') {
		unch(ch);
		return 0;
	}

	switch (ch = inpch()) {
	case '=':  return '#';
	case '(':  return '[';
	case ')':  return ']';
	case '<':  return '{';
	case '>':  return '}';
	case '/':  return '\\';
	case '\'': return '^';
	case '!':  return '|';
	case '-':  return '~';
	}

	unch(ch);
	unch('?');
	return 0;
}

/*
 * 5.1.1.2 Translation phase 1.
 */
static int
inc1(void)
{
	int ch, c2;

	do {
		ch = inpch();
	} while (/* ch == '\r' || */ (ch == '\\' && chkucn()));
	if (ch == '?' && (c2 = chktg()))
		ch = c2;
	return ch;
}


/*
 * 5.1.1.2 Translation phase 2.
 */
static int
inc2(void)
{
	int ch, c2;

	if (ibufp)
		return ibread();

	if ((ch = inc1()) != '\\')
		return ch;
	if ((c2 = inc1()) == '\n') {
		ifiles->escln++;
		ch = inc2();
	} else
		unch(c2);
	return ch;
}

/*
 * deal with comments in the fast scanner.
 * ps prints out the initial '/' if failing to batch comment.
 */
static int
fastcmnt(int ps)
{
	int ch;

	if ((ch = inc2()) == '/') { /* C++ comment */
		while ((ch = inc2()) != '\n')
			;
		unch(ch);
	} else if (ch == '*') {
		for (;;) {
			if ((ch = inc2()) < 0)
				break;
			if (ch == '*') {
				if ((ch = inc2()) == '/') {
					break;
				} else
					unch(ch);
			} else if (ch == '\n') {
				ifiles->lineno++;
				putch('\n');
			}
		}
	} else {
		if (ps) PUTCH('/'); /* XXX ? */
		unch(ch);
                return 0;
        }
	if (ch < 0)
		error("file ends in comment");
        return 1;
}

/*
 * return next char, partly phase 3.
 */
static int
inch(void)
{
	int ch, n;

	if (ibufp)
		return ibread();

	ch = inc2();
	n = ifiles->lineno;
	if (ch == '/' && Cflag == 0 && fastcmnt(0)) {
		/* Comments 5.1.1.2 p3 */
		/* no space if traditional or multiline */
		ch = (tflag || n != ifiles->lineno) ? inch() : ' ';
	}
	return ch;
}

/*
 * check for universal-character-name on input, and
 * unput to the pushback buffer encoded as UTF-8.
 */
static int
chkucn(void)
{
	unsigned long cp, m;
	int ch, n;

	if ((ch = inch()) == -1)
		return 0;
	if (ch == 'u')
		n = 4;
	else if (ch == 'U')
		n = 8;
	else {
		unch(ch);
		return 0;
	}

	cp = 0;
	while (n-- > 0) {
		if ((ch = inch()) == -1 || (spechr[ch] & C_HEX) == 0) {
			warning("invalid universal character name");
			// XXX should actually unput the chars and return 0
			unch(ch); // XXX eof
			break;
		}
		cp = cp * 16 + dig2num(ch);
	}

	if ((cp < 0xa0 && cp != 0x24 && cp != 0x40 && cp != 0x60)
	    || (cp >= 0xd800 && cp <= 0xdfff))	/* 6.4.3.2 */
		error("universal character name cannot be used");

	if (cp > 0x7fffffff)
		error("universal character name out of range");

	n = 0;
	m = 0x7f;
	while (cp > m) {
		unch(0x80 | (cp & 0x3f));
		cp >>= 6;
		m >>= (n++ ? 1 : 2);
	}
	unch(((m << 1) ^ 0xfe) | cp);
	return 1;
}

/*
 * deal with comments when -C is active.
 * Save comments in expanded macros???
 */
static int
Ccmnt(void (*d)(int))
{
	int ch;

	if ((ch = inch()) == '/') { /* C++ comment */
		d(ch);
		do {
			d(ch);
		} while ((ch = inch()) != '\n');
		unch(ch);
		return 1;
	} else if (ch == '*') {
		d('/');
		d('*');
		for (;;) {
			ch = inch();
			d(ch);
			if (ch == '*') {
				if ((ch = inch()) == '/') {
					d(ch);
					return 1;
				} else
					unch(ch);
			} else if (ch == '\n') {
				ifiles->lineno++;
			}
		}
	}
	d('/');
        unch(ch);
        return 0;
}

/*
 * Traverse over spaces and comments from the input stream,
 * Returns first non-space character.
 */
static int
fastspc(void)
{
	int ch;

	while ((ch = inch()), ISWS(ch))
		;
	return ch;
}

/*
 * As above but only between \n and #.
 */
static int
fastspcg(void)
{
	int ch, c2;

	while ((ch = inch()) == '/' || ch == '%' || ISWS(ch)) {
		if (ch == '%') {
			if ((c2 = inch()) == ':')
				ch = '#'; /* digraphs */
			else
				unch(c2);
			break;
		}
		if (ch == '/') {
			if (Cflag)
				return ch;
			if (fastcmnt(0) == 0)
				break;
			putch(' ');
		} else
			putch(ch);
	}
	return ch;
}

/*
 * readin chars and store in yytext. Warn about too long names.
 */
static void
fastid(int ch)
{
	int i = 0;

	do {
		yytext[i++] = ch;
	} while (spechr[ch = inch()] & C_ID);
	yytext[i] = 0;
	unch(ch);
}

/*
 * readin chars and store on heap. Warn about too long names.
 */
static void
heapid(int ch)
{
	do {
		savch(ch);
	} while (spechr[ch = inch()] & C_ID);
	savch(0);
	unch(ch);
}

/*
 * get a string or character constant and save it as given by d.
 */
static void
faststr(int bc, void (*d)(int))
{
	int ch;

	d(bc);
	while ((ch = inc2()) != bc) {
		if (ch == '\n') {
			warning("unterminated literal");
			unch(ch);
			return;
		}
		if (ch < 0)
			return;
		if (ch == '\\') {
			d(ch);
			ch = inc2();
		}
		d(ch);
	}
	d(ch);
}

/*
 * get a preprocessing number and save it as given by d.
 * Initial char ch is always stored.
 * returns first non-pp-number char.
 *
 *	pp-number:	digit
 *			. digit
 *			pp-number digit
 *			pp-number identifier-nondigit
 *			pp-number e sign
 *			pp-number E sign
 *			pp-number p sign
 *			pp-number P sign
 *			pp-number .
 */
static int
fastnum(int ch, void (*d)(int))
{
	int c2;

	if ((spechr[ch] & C_DIGIT) == 0) {
		/* not digit, dot */
		d(ch);
		ch = inch();
		if ((spechr[ch] & C_DIGIT) == 0)
			return ch;
	}
	for (;;) {
		d(ch);
		if ((ch = inch()) < 0)
			return -1;
		if ((spechr[ch] & C_EP)) {
			if ((c2 = inch()) != '-' && c2 != '+') {
				if (c2 >= 0)
					unch(c2);
				break;
			}
			d(ch);
			ch = c2;
		} else if (ch == '.' || (spechr[ch] & C_ID)) {
			continue;
		} else
			break;
	}
	return ch;
}

/*
 * Scan quickly the input file searching for:
 *	- '#' directives
 *	- keywords (if not flslvl)
 *	- comments
 *
 *	Handle strings, numbers and trigraphs with care.
 *	Only data from pp files are scanned here, never any rescans.
 *	This loop is always at trulvl.
 */
static void
fastscan(void)
{
	struct symtab *nl;
	int ch, c2, i;
	usch *cp;

	goto run;

	for (;;) {
		/* tight loop to find special chars */
		/* should use getchar/putchar here */
		for (;;) {
			ch = inch();
xloop:			if (ch < 0)
				return; /* EOF */
			if ((spechr[ch] & C_SPEC) != 0)
				break;
			putch(ch);
		}

		switch (ch) {
		case EBLOCK:
		case WARN:
		case CONC:
			error("bad char passed");
			break;

		case '/': /* Comments */
			if (Cflag == 0) {
				if (fastcmnt(1))
					putch(' '); /* 5.1.1.2 p3 */
			} else
				Ccmnt(putch);
			break;

		case '\n': /* newlines, for pp directives */
			/* take care of leftover \n */
			i = ifiles->escln + 1;
			ifiles->lineno += i;
			ifiles->escln = 0;
			while (i-- > 0)
				putch('\n');

			/* search for a # */
run:			while ((ch = inch()) == '\t' || ch == ' ')
				putch(ch);
			if (ch == '%') {
				if ((c2 = inch()) != ':')
					unch(c2);
				else
					ch = '#';
			}
			if (ch  == '#')
				ppdir();
			else
				goto xloop;
			break;

		case '\'': /* character constant */
			if (tflag) {
				putch(ch);
				break;	/* character constants ignored */
			}
			/* FALLTHROUGH */
		case '\"': /* strings */
			faststr(ch, putch);
			break;

		case '.':  /* for pp-number */
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			ch = fastnum(ch, putch);
			goto xloop;

		case 'L':
			if ((ch = inch()) == '\"' || ch == '\'') {
				putch('L');
				goto xloop;
			}
			unch(ch);
			ch = 'L';

			/* FALLTHROUGH */
		default:
#ifdef PCC_DEBUG
			if ((spechr[ch] & C_ID) == 0)
				error("fastscan");
#endif
		ident:
			if (flslvl)
				error("fastscan flslvl");
			cp = stringbuf;
			heapid(ch);
			if ((nl = lookup(cp, FIND)) && kfind(nl))
				putstr(stringbuf);
			else
				putstr(cp);
			stringbuf = cp;
			break;

		case '\\':
			if (chkucn()) {
				ch = inch();
				goto ident;
			}
			putch('\\');
			break;
		}
	}

/*eof:*/	warning("unexpected EOF");
	putch('\n');
}

static int yytp;
static void
yyts(int c)
{
	yytext[yytp++] = c;
}

int
sloscan(void (*d)(int), int flags)
{
	int ch, c2;
	int yyp;

zagain:
	ch = inch();

yagain:	yyp = 0;
	yytext[yyp++] = (usch)ch;
	switch (ch) {
	case -1: /* EOF */
		return 0;

	case '\n': /* do not pass NL */
		unch(ch);
		yytext[yyp] = 0;
		return ch;

	case '\r': /* Ignore CR */
		goto zagain;

	case '.':
	case '0': case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
		yytp = 0;
		unch(fastnum(ch, yyts));
		yyts(0);
		if (yytext[0] == '.' && yytext[1] == 0)
			return '.';
		return NUMBER;

	case '\'':
		if (tflag)
			goto any;
		yytp = 0;
		faststr(ch, yyts);
		yyts(0);
		return NUMBER;

	case ' ':
	case '\t':
		do {
			ch = inch();
		} while (ISWS(ch));
		if (flags & SLO_IGNOREWS)
			goto yagain;
		unch(ch);
		yytext[yyp] = 0;
		return WSPACE;

	case '/':
		if (Cflag == 0) {
			if (fastcmnt(0))
				error("comment and no Cflag");
		} else {
			extern int readmac;

#if 0
			if (readmac) {
				unch(c2 = inch());
				yytext[1] = 0;
				if (c2 == '*')
					return CMNT;
			}
#endif
			yytp = 1;
			Ccmnt(d ? d : putch);
			yyts(0);
			if (readmac)
				return CMNT;
			goto zagain;
		}
		goto any;

	case '\"':
		if (tflag && defining)
			goto any;
		yytp = 0;
	strng:
		faststr(ch, yyts);
		yyts(0);
		return STRING;

	case '\\':
		if (chkucn()) {
			--yyp;
			goto ident;
		}
		goto any;

	case 'L':
		c2 = inch();
		if ((c2 == '\"' || c2 == '\'') && !tflag) {
			yytp = 0;
			yyts(ch);
			ch = c2;
			goto strng;
		}
		unch(c2);
		/* FALLTHROUGH */

	/* Yetch, all identifiers */
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
	case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
	case 's': case 't': case 'u': case 'v': case 'w': case 'x':
	case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	case 'G': case 'H': case 'I': case 'J': case 'K':
	case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
	case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
	case 'Y': case 'Z':
	case '_': /* {L}({L}|{D})* */

	ident:
		fastid(ch);
		return IDENT;

	case EBLOCK:
		unch(ch);
		return cinput();

	default:
		if ((ch & 0x80))
			goto ident;

	any:
		yytext[yyp] = 0;
		return yytext[0];
	} /* endcase */

	/* NOTREACHED */
}

int
yylex(void)
{
	static int ifdef, noex;
	struct symtab *nl;
	int ch, c2;

	ch = sloscan(NULL, SLO_IGNOREWS);
		;
	if (ch < 128 && (spechr[ch] & C_2))
		c2 = inch();
	else
		c2 = 0;

	switch (ch) {
	case '=':
		if (c2 == '=') return EQ;
		break;
	case '!':
		if (c2 == '=') return NE;
		break;
	case '|':
		if (c2 == '|') return OROR;
		break;
	case '&':
		if (c2 == '&') return ANDAND;
		break;
	case '<':
		if (c2 == '<') return LS;
		if (c2 == '=') return LE;
		break;
	case '>':
		if (c2 == '>') return RS;
		if (c2 == '=') return GE;
		break;
	case '+':
	case '-':
		if (ch == c2)
			error("invalid preprocessor operator %c%c", ch, c2);
		break;

	case '/':
		if (Cflag == 0 || c2 != '*')
			break;
		/* Found comment that need to be skipped */
		for (;;) {
			ch = inch();
		c1:	if (ch != '*')
				continue;
			if ((ch = inch()) == '/')
				break;
			goto c1;
		}
		return yylex();

	case NUMBER:
		if (yytext[0] == '\'') {
			yynode.op = NUMBER;
			yynode.nd_val = charcon(yytext);
		} else
			cvtdig(yytext[0] != '0' ? 10 :
			    yytext[1] == 'x' || yytext[1] == 'X' ? 16 : 8);
		return NUMBER;

	case IDENT:
		if (strcmp((char *)yytext, "defined") == 0) {
			ifdef = 1;
			return DEFINED;
		}
		nl = lookup(yytext, FIND);
		if (ifdef) {
			yynode.nd_val = nl != NULL;
			ifdef = 0;
		} else if (nl && noex == 0) {
			usch *och = stringbuf;
			int i;

			i = kfind(nl);
			unch(WARN);
			if (i)
				unpstr(stringbuf);
			else
				unpstr(nl->namep);
			stringbuf = och;
			noex = 1;
			return yylex();
		} else {
			yynode.nd_val = 0;
		}
		yynode.op = NUMBER;
		return NUMBER;
	case WARN:
		noex = 0;
		/* FALLTHROUGH */
	case PHOLD:
		return yylex();
	default:
		return ch;
	}
	unch(c2);
	return ch;
}

/*
 * Let the command-line args be faked defines at beginning of file.
 */
static void
prinit(struct initar *it, struct includ *ic)
{
	const char *pre, *post;
	char *a;

	if (it->next)
		prinit(it->next, ic);
	pre = post = NULL; /* XXX gcc */
	switch (it->type) {
	case 'D':
		pre = "#define ";
		if ((a = strchr(it->str, '=')) != NULL) {
			*a = ' ';
			post = "\n";
		} else
			post = " 1\n";
		break;
	case 'U':
		pre = "#undef ";
		post = "\n";
		break;
	case 'i':
		pre = "#include \"";
		post = "\"\n";
		break;
	default:
		error("prinit");
	}
	strlcat((char *)ic->buffer, pre, CPPBUF+1);
	strlcat((char *)ic->buffer, it->str, CPPBUF+1);
	if (strlcat((char *)ic->buffer, post, CPPBUF+1) >= CPPBUF+1)
		error("line exceeds buffer size");

	ic->lineno--;
	while (*ic->maxread)
		ic->maxread++;
}

/*
 * A new file included.
 * If ifiles == NULL, this is the first file and already opened (stdin).
 * Return 0 on success, -1 if file to be included is not found.
 */
int
pushfile(const usch *file, const usch *fn, int idx, void *incs)
{
	extern struct initar *initar;
	struct includ ibuf;
	struct includ *ic;
	int otrulvl;

	ic = &ibuf;
	ic->next = ifiles;

	if (file != NULL) {
		if ((ic->infil = open((const char *)file, O_RDONLY)) < 0)
			return -1;
		ic->orgfn = ic->fname = file;
		if (++inclevel > MAX_INCLEVEL)
			error("limit for nested includes exceeded");
	} else {
		ic->infil = 0;
		ic->orgfn = ic->fname = (const usch *)"<stdin>";
	}
#ifndef BUF_STACK
	ic->bbuf = malloc(BBUFSZ);
#endif
	ic->buffer = ic->bbuf+NAMEMAX;
	ic->curptr = ic->buffer;
	ifiles = ic;
	ic->lineno = 1;
	ic->escln = 0;
	ic->maxread = ic->curptr;
	ic->idx = idx;
	ic->incs = incs;
	ic->fn = fn;
	prtline();
	if (initar) {
		int oin = ic->infil;
		ic->infil = -1;
		*ic->maxread = 0;
		prinit(initar, ic);
		initar = NULL;
		if (dMflag)
			printf("%s", (char *)ic->buffer);
		fastscan();
		prtline();
		ic->infil = oin;
	}

	otrulvl = trulvl;

	fastscan();

	if (otrulvl != trulvl || flslvl)
		error("unterminated conditional");

#ifndef BUF_STACK
	free(ic->bbuf);
#endif
	ifiles = ic->next;
	close(ic->infil);
	inclevel--;
	return 0;
}

/*
 * Print current position to output file.
 */
void
prtline(void)
{
	usch *sb = stringbuf;

	if (Mflag) {
		if (dMflag)
			return; /* no output */
		if (ifiles->lineno == 1 &&
		    (MMDflag == 0 || ifiles->idx != SYSINC)) {
			printf("%s: %s\n", Mfile, ifiles->fname);
			if (MPflag &&
			    strcmp((const char *)ifiles->fname, (char *)MPfile))
				printf("%s:\n", ifiles->fname);
		}
	} else if (!Pflag) {
		sheap("\n# %d \"%s\"", ifiles->lineno, ifiles->fname);
		if (ifiles->idx == SYSINC)
			sheap(" 3");
		sheap("\n");
		putstr(sb);
	}
	stringbuf = sb;
}

void
cunput(int c)
{
#ifdef PCC_DEBUG
//	if (dflag)printf(": '%c'(%d)\n", c > 31 ? c : ' ', c);
#endif
	unch(c);
}

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

/*
 * Convert string numbers to unsigned long long and check overflow.
 */
static void
cvtdig(int rad)
{
	unsigned long long rv = 0;
	unsigned long long rv2 = 0;
	usch *y = yytext;
	int c;

	c = *y++;
	if (rad == 16)
		y++;
	while ((spechr[c] & C_HEX)) {
		rv = rv * rad + dig2num(c);
		/* check overflow */
		if (rv / rad < rv2)
			error("constant \"%s\" is out of range", yytext);
		rv2 = rv;
		c = *y++;
	}
	y--;
	while (*y == 'l' || *y == 'L')
		y++;
	yynode.op = *y == 'u' || *y == 'U' ? UNUMBER : NUMBER;
	yynode.nd_uval = rv;
	if ((rad == 8 || rad == 16) && yynode.nd_val < 0)
		yynode.op = UNUMBER;
	if (yynode.op == NUMBER && yynode.nd_val < 0)
		/* too large for signed, see 6.4.4.1 */
		error("constant \"%s\" is out of range", yytext);
}

static int
charcon(usch *p)
{
	int val, c;

	p++; /* skip first ' */
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
			while ((spechr[c = *p] & C_HEX)) {
				val = val * 16 + dig2num(c);
				p++;
			}
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7':
			p--;
			while ((spechr[c = *p] & C_DIGIT)) {
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
chknl(int ignore)
{
	int t;

	if ((t = sloscan(NULL, SLO_IGNOREWS)) != '\n') {
		if (t) {
			if (ignore) {
				warning("newline expected, got \"%s\"", yytext);
				/* ignore rest of line */
				while ((t = sloscan(NULL, 0)) && t != '\n')
					;
			}
			else
				error("newline expected, got \"%s\"", yytext);
		} else {
			if (ignore)
				warning("no newline at end of file");
			else
				error("no newline at end of file");
		}
	}
}

static void
elsestmt(void)
{
	if (flslvl) {
		if (elflvl > trulvl)
			;
		else if (--flslvl!=0)
			flslvl++;
		else
			trulvl++;
	} else if (trulvl) {
		flslvl++;
		trulvl--;
	} else
		error("#else in non-conditional section");
	if (elslvl==trulvl+flslvl)
		error("too many #else");
	elslvl=trulvl+flslvl;
	chknl(1);
}

static void
ifdefstmt(void)
{
	int t;

	if ((t = sloscan(NULL, SLO_IGNOREWS)) != IDENT)
		error("bad #ifdef");
	if (lookup(yytext, FIND) == NULL)
		flslvl++;
	else
		trulvl++;
	chknl(0);
}

static void
ifndefstmt(void)
{
	int t;

	if ((t = sloscan(NULL, SLO_IGNOREWS)) != IDENT)
		error("bad #ifndef");
	if (lookup(yytext, FIND) != NULL)
		flslvl++;
	else
		trulvl++;
	chknl(0);
}

static void
endifstmt(void)
{
	if (flslvl)
		flslvl--;
	else if (trulvl)
		trulvl--;
	else
		error("#endif in non-conditional section");
	if (flslvl == 0)
		elflvl = 0;
	elslvl = 0;
	chknl(1);
}

static void
ifstmt(void)
{
	yyparse() ? trulvl++ : flslvl++;
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
			flslvl++;
		else if (yyparse())
			trulvl++;
		else
			flslvl++;
	} else if (trulvl) {
		flslvl++;
		trulvl--;
	} else
		error("#elif in non-conditional section");
}

/* save line into stringbuf */
static usch *
savln(void)
{
	int c;
	usch *cp = stringbuf;

	while ((c = inch()) != -1) {
		if (c == '\n') {
			unch(c);
			break;
		}
		savch(c);
	}
	savch(0);

	return cp;
}

static void
cpperror(void)
{
	usch *cp;
	int c;

	if (flslvl)
		return;
	if ((c = sloscan(NULL, SLO_IGNOREWS)) != '\n')
		error("bad #error");
	cp = savln();
	error("#error %s", cp);
}

static void
cppwarning(void)
{
	usch *cp;
	int c;

	if (flslvl)
		return;
	if ((c = sloscan(NULL, SLO_IGNOREWS)) != WSPACE && c != '\n')
		error("bad #warning");
	cp = savln();
	warning("#warning %s", cp);
	stringbuf = cp;
}

static void
undefstmt(void)
{
	struct symtab *np;

	if (flslvl)
		return;
	if (sloscan(NULL, SLO_IGNOREWS) != IDENT)
		error("bad #undef");
	if ((np = lookup(yytext, FIND)) != NULL)
		np->value = 0;
	chknl(0);
}

static void
identstmt(void)
{
	struct symtab *sp;
	int i;

	if (sloscan(NULL, 0) != WSPACE)
		goto bad;

	if ((i = sloscan(NULL, 0)) == IDENT) {
		if ((sp = lookup(yytext, FIND)) && kfind(sp))
			unpstr(stringbuf);
		i = sloscan(NULL, 0);
	}

	if (i != STRING)
		goto bad;
	return;
bad:
	error("bad #ident directive");
}

static void
pragmastmt(void)
{
	usch *sb;

	if (flslvl)
		return;
	if (sloscan(NULL, 0) != WSPACE)
		error("bad #pragma");
	sb = stringbuf;
	savstr((const usch *)"\n#pragma ");
	savln();
	putstr(sb);
	prtline();
	stringbuf = sb;
}

int
cinput(void)
{

	return inch();
}

#define	DIR_FLSLVL	001
#define	DIR_FLSINC	002
static struct {
	const char *name;
	void (*fun)(void);
	int flags;
} ppd[] = {
	{ "ifndef", ifndefstmt, DIR_FLSINC },
	{ "ifdef", ifdefstmt, DIR_FLSINC },
	{ "if", ifstmt, DIR_FLSINC },
	{ "include", include },
	{ "else", elsestmt, DIR_FLSLVL },
	{ "endif", endifstmt, DIR_FLSLVL },
	{ "error", cpperror },
	{ "warning", cppwarning },
	{ "define", define },
	{ "undef", undefstmt },
	{ "line", line },
	{ "pragma", pragmastmt },
	{ "elif", elifstmt, DIR_FLSLVL },
	{ "ident", identstmt },
#ifdef GCC_COMPAT
	{ "include_next", include_next },
#endif
};
#define	NPPD	(int)(sizeof(ppd) / sizeof(ppd[0]))

static void
skpln(void)
{
	int ch;

	/* just ignore the rest of the line */
	while ((ch = inch()) != -1) {
		if (ch == '\n') {
			unch('\n');
			break;
		}
	}
}

/*
 * do an even faster scan than fastscan while at flslvl.
 * just search for a new directive.
 */
static void
flscan(void)
{
	int ch;

	for (;;) {
		switch (ch = inch()) {
		case -1:
			return;
		case '\n':
			ifiles->lineno++;
			putch('\n');
			if ((ch = fastspcg()) == '#')
				return;
			unch(ch);
			break;
		case '/':
			fastcmnt(0);	/* may be around directives */
			break;
		}
        }
}


/*
 * Handle a preprocessor directive.
 * # is already found.
 */
void
ppdir(void)
{
	int ch, i, oldC;

	oldC = Cflag;
redo:	Cflag = 0;
	if ((ch = fastspc()) == '\n') { /* empty directive */
		unch(ch);
		Cflag = oldC;
		return;
	}
	Cflag = oldC;
	if ((spechr[ch] & C_ID0) == 0)
		goto out;
	fastid(ch);

	/* got some keyword */
	for (i = 0; i < NPPD; i++) {
		if (yytext[0] == ppd[i].name[0] &&
		    strcmp((char *)yytext, ppd[i].name) == 0) {
			if (flslvl == 0) {
				(*ppd[i].fun)();
				if (flslvl == 0)
					return;
			} else {
				if (ppd[i].flags & DIR_FLSLVL) {
					(*ppd[i].fun)();
					if (flslvl == 0)
						return;
				}else if (ppd[i].flags & DIR_FLSINC)
					flslvl++;
			}
			flscan();
			goto redo;
		}
	}

out:
	if (flslvl == 0 && Aflag == 0)
		error("invalid preprocessor directive");

	unch(ch);
	skpln();
}
