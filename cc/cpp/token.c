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

/* definition for include file info */
struct includ {
	struct includ *next;
	char *fname;
	int lineno;
#ifdef NEWBUF
	int infil;
	usch *curptr;
	int curlen;
	usch *ostr;
	usch buffer[CPPBUF];
#else
	FILE *ifil;
#endif
} *ifiles, *freebufs;

usch *yyp, yystr[CPPBUF];

int yylex(void);
int yywrap(void);

#ifdef NEWBUF
static struct includ *
getbuf(usch *file)
{
	struct includ *ic;

	if (freebufs) {
		/* Have buffer for use already */
		ic = freebufs;
		freebufs = freebufs->next;
		ic->ostr = NULL;
	} else {
		stringbuf = (usch *)ROUND((int)stringbuf);
		ic = (struct includ *)stringbuf;
		stringbuf += sizeof(struct includ);
		ic->ostr = stringbuf;
	}
	if (file) {
		ic->fname = savstr(file); /* XXX - will loose space */
		savch('\0');
	}
	if (ic->ostr)
		ic->ostr = stringbuf;
	return ic;
}

static void
putbuf(struct includ *ic)
{
	if (ic->ostr == stringbuf) {
		/* no new macros, free this buffer */
		stringbuf = (usch *)ic;
	} else {
		ic->next = freebufs;
		freebufs = ic;
	}
}

static int
input(void)
{
	struct includ *ic;

	if (ifiles->curptr < ifiles->buffer+ifiles->curlen)
		return *ifiles->curptr++;
	if (ifiles->infil < 0) {
		ic = ifiles;
		ifiles = ifiles->next;
		putbuf(ic);
		return input();
	}
	if ((ifiles->curlen = read(ifiles->infil, ifiles->buffer, CPPBUF)) < 0)
		error("read error on file %s", ifiles->fname);
	if (ifiles->curlen == 0)
		return -1;
	ifiles->curptr = ifiles->buffer;
	return input();
}

static void
unput(int c)
{
	struct includ *ic;

	if (ifiles->curptr > ifiles->buffer) {
		*--ifiles->curptr = c;
	} else {
		ic = getbuf(NULL);
		ic->fname = ifiles->fname;
		ic->lineno = ifiles->lineno;
		ic->infil = -1;
		ic->curlen = CPPBUF;
		ic->curptr = ic->buffer+CPPBUF;
		ic->next = ifiles;
		ifiles = ic;
		*--ifiles->curptr = c;
	}
}
#else
#define input() fgetc(ifiles->ifil)
#define unput(c) ungetc(c, ifiles->ifil)
#endif
static int
slofgetc(void)
{
	int c;

again:	switch (c = input()) {
	case '\\': /* continued lines */
		if ((c = input()) == '\n') {
			ifiles->lineno++;
			putc('\n', obuf);
			goto again;
		}
		cunput(c);
		return '\\';
	case '?': /* trigraphs */
		if ((c = input()) != '?') {
			cunput(c);
			return '?';
		}
		switch (c = input()) {
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
			cunput(c);
			cunput('?');
			return '?';
		}
		cunput(c);
		goto again;
	default:
		return c;
	}
}

int
yylex()
{
	static int wasnl = 1;
	int c, oc, rval;

	yyp = yystr;
	c = input();
	if (c != ' ' && c != '\t' && c != '#')
		wasnl = 0;
#define ONEMORE()	{ *yyp++ = c; c = slofgetc(); }
again:	switch (c) {
	case -1:
		rval = yywrap() ? 0 : yylex();
		break;

	case '\'': /* charcon */
	case '"': /* string */
chstr:		oc = c;
		do {
			*yyp++ = c;
			if (c == '\\')
				*yyp++ = slofgetc();
		} while ((c = slofgetc()) != EOF && c != oc);
		*yyp++ = c; *yyp = 0;
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
			putc('\n', obuf);
			c = input();
			goto again;
		} else {
			cunput(c);
			*yyp++ = '\\'; *yyp = 0;
			rval = '\\';
		}
		break;
		
	case '\n':
		wasnl = 1;
		ifiles->lineno++;
		*yyp++ = c; *yyp = 0;
		rval = NL;
		break;

	case '#':
		if (wasnl) {
			wasnl = 0;
			rval = CONTROL;
			break;
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
		yystr[0] = c; yystr[1] = 0;
		rval = c;
		break;
	}
	return rval;
}

#ifdef NEW_READFILE
/*
 * A new file included.  Read buffers are allocated on the stack and
 * all subroutines are called from here.  This function will be called
 * recursive when multiple files are included.
 */
int
pushfile(char *file)
{
	struct includ incl, *ic = &incl;

	ic->lineno = 1;
	if (ifiles != NULL) { /* not if first file */
		if ((ic->infil = open(file, O_RDONLY)) < 0)
			return -1;
	} else
		ic->infil = 0; /* STDIN_FILENO */

	ic->fname = savstr(file); /* XXX - will loose space */
	savch('\0');
	ic->curptr = ic->buffer;
	ic->next = ifiles;
	ifiles = ic;

	while ((c = qscan()) != 0) {
		switch (c) {
		case CONTROL:
			control();
			break;

		



}
#else
/*
 * A new file included.
 * If ifiles == NULL, this is the first file and already opened (stdin).
 * Return 0 on success, -1 on failure to open file.
 */
int
pushfile(char *file)
{
	struct includ *ic;

#ifdef NEWBUF
	ic = getbuf(file);
#else
	ic = malloc(sizeof(struct includ));
	ic->fname = strdup(file);
#endif
	if (ifiles != NULL) {
#ifdef NEWBUF
		if ((ic->infil = open(file, O_RDONLY)) < 0)
			return -1;
#else
		if ((ic->ifil = fopen(file, "r")) == NULL)
			return -1;
#endif
	} else
#ifdef NEWBUF
		ic->infil = 0;
#else
		ic->ifil = stdin;
#endif
#ifdef NEWBUF
	ic->curptr = ic->buffer;
#endif
	ic->next = ifiles;
	ifiles = ic;
	ic->lineno = 0;
	unput('\n');

	return 0;
}
#endif

/*
 * End of included file (or everything).
 */
void
popfile()
{
	struct includ *ic;

	ic = ifiles;
	ifiles = ifiles->next;
#ifdef NEWBUF
	close(ic->infil);
	putbuf(ic);
#else
	fclose(ic->ifil);
	free(ic->fname);
	free(ic);
#endif
	prtline();
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

int
yywrap()
{
	if (ifiles->next == 0)
		return 1;
	popfile();
	return 0;
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
		free(ifiles->fname), ifiles->fname = strdup(name);
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
