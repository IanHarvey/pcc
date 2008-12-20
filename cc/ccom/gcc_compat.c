/*      $Id$     */
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
 * Routines to support some of the gcc extensions to C.
 */
#ifdef GCC_COMPAT

#include "pass1.h"
#include "cgram.h"

#include <string.h>

/* Remove heading and trailing __ */
static char *
decap(char *s)
{
	if (s[0] == '_' && s[1] == '_') {
		int len = strlen(s);

		if (s[len-1] == '_' && s[len-2] == '_') {
			s = tmpstrdup(s); /* will trash */
			s[len-2] = 0;
		}
		s += 2;
	}
	return s;
}

static struct kw {
	char *name, *ptr;
	int rv;
} kw[] = {
/*
 * Do NOT change the order of these entries unless you know 
 * what you're doing!
 */
/* 0 */	{ "__asm", NULL, C_ASM },
/* 1 */	{ "__signed", NULL, 0 },
/* 2 */	{ "__inline", NULL, C_FUNSPEC },
/* 3 */	{ "__const", NULL, 0 },
/* 4 */	{ "__asm__", NULL, C_ASM },
/* 5 */	{ "__inline__", NULL, C_FUNSPEC },
/* 6 */	{ "__thread", NULL, 0 },
/* 7 */	{ "__FUNCTION__", NULL, 0 },
/* 8 */	{ "__volatile", NULL, 0 },
/* 9 */	{ "__volatile__", NULL, 0 },
/* 10 */{ "__restrict", NULL, -1 },
/* 11 */{ "__typeof__", NULL, C_TYPEOF },
/* 12 */{ "typeof", NULL, C_TYPEOF },
/* 13 */{ "__extension__", NULL, -1 },
/* 14 */{ "__signed__", NULL, 0 },
/* 15 */{ "__attribute__", NULL, 0 },
	{ NULL, NULL, 0 },
};

void
gcc_init()
{
	struct kw *kwp;

	for (kwp = kw; kwp->name; kwp++)
		kwp->ptr = addname(kwp->name);

}

#define	TS	"\n#pragma tls\n# %d\n"
#define	TLLEN	sizeof(TS)+10
/*
 * See if a string matches a gcc keyword.
 */
int
gcc_keyword(char *str, NODE **n)
{
	extern int inattr, parlvl, parbal;
	char tlbuf[TLLEN], *tw;
	struct kw *kwp;
	int i;

	for (i = 0, kwp = kw; kwp->name; kwp++, i++)
		if (str == kwp->ptr)
			break;
	if (kwp->name == NULL)
		return 0;
	if (kwp->rv)
		return kwp->rv;
	switch (i) {
	case 1:  /* __signed */
	case 14: /* __signed__ */
		*n = mkty((TWORD)SIGNED, 0, MKSUE(SIGNED));
		return C_TYPE;
	case 3: /* __const */
		*n = block(QUALIFIER, NIL, NIL, CON, 0, 0);
		return C_QUALIFIER;
	case 6: /* __thread */
		snprintf(tlbuf, TLLEN, TS, lineno);
		tw = &tlbuf[strlen(tlbuf)];
		while (tw > tlbuf)
			cunput(*--tw);
		return -1;
	case 7: /* __FUNCTION__ */
		if (cftnsp == NULL) {
			uerror("__FUNCTION__ outside function");
			yylval.strp = "";
		} else
			yylval.strp = cftnsp->sname; /* XXX - not C99 */
		return C_STRING;
	case 8: /* __volatile */
	case 9: /* __volatile__ */
		*n = block(QUALIFIER, NIL, NIL, VOL, 0, 0);
		return C_QUALIFIER;
	case 15: /* __attribute__ */
		inattr = 1;
		parlvl = parbal;
		return C_ATTRIBUTE;
	}
	cerror("gcc_keyword");
	return 0;
}

#ifndef TARGET_TYPE_ATTR
#define	TARGET_TYPE_ATTR(p, sue)	1
#endif
#ifndef TARGET_VAR_ATTR
#define	TARGET_VAR_ATTR(p, sue)		1
#endif
#ifndef	ALMAX
#define	ALMAX (ALLDOUBLE > ALLONGLONG ? ALLDOUBLE : ALLONGLONG)
#endif

/*
 * Get type attributes from an argument list.
 */
static void
gcc_ta(NODE *p, void *arg)
{
	struct suedef *sue = arg;
	char *n2, *name = NULL;

	if (p->n_op == NAME) {
		name = (char *)p->n_sp;
	} else if (p->n_op == CALL || p->n_op == UCALL) {
		name = (char *)p->n_left->n_sp;
	} else
		cerror("bad type attribute");

	n2 = name;
	name = decap(name);
	if (strcmp(name, "aligned") == 0) {
		/* Align the type to a given max alignment */
		if (p->n_op == CALL) {
			sue->suealigned = icons(eve(p->n_right)) * SZCHAR;
			p->n_op = UCALL;
		} else
			sue->suealigned = ALMAX;
	} else if (strcmp(name, "packed") == 0) {
		/* pack members of a struct */
		if (p->n_op != NAME)
			uerror("packed takes no args");
		sue->suepacked = SZCHAR; /* specify pack size? */
	} else if (TARGET_TYPE_ATTR(p, sue) == 0)
		werror("unsupported attribute %s", n2);
}

/*
 * Get variable attributes from an argument list.
 */
static void
gcc_va(NODE *p, void *arg)
{
	struct suedef *sue = arg;
	char *n2, *name = NULL;

	if (p->n_op == NAME) {
		name = (char *)p->n_sp;
	} else if (p->n_op == CALL || p->n_op == UCALL) {
		name = (char *)p->n_left->n_sp;
	} else
		cerror("bad variable attribute");

	n2 = name;
	name = decap(name);
	if (strcmp(name, "aligned") == 0) {
		/* Align the variable to a given max alignment */
		if (p->n_op == CALL) {
			sue->suealigned = icons(eve(p->n_right)) * SZCHAR;
			p->n_op = UCALL;
		} else
			sue->suealigned = ALMAX;
	} else if (strcmp(name, "section") == 0) {
		if (p->n_right->n_op != STRING)
			uerror("bad section");
		sue->suesection = p->n_right->n_name;
#ifdef notyet
	} else if (strcmp(name, "packed") == 0) {
		/* pack members of a struct */
		if (p->n_op != NAME)
			uerror("packed takes no args");
		sue->suepacked = SZCHAR; /* specify pack size? */
#endif
	} else if (TARGET_VAR_ATTR(p, sue) == 0)
		werror("unsupported attribute %s", n2);
}

/*
 * Extract type attributes from a node tree and setup a suedef 
 * struct based on its contents.
 */
struct suedef *
gcc_type_attrib(NODE *p)
{
	struct suedef *sue = tmpcalloc(sizeof(struct suedef));

	flist(p, gcc_ta, sue);
	tfree(p);
	return sue;
}

struct suedef *
gcc_var_attrib(NODE *p)
{
	struct suedef *sue = permalloc(sizeof(struct suedef)); /* XXX ??? */

	memset(sue, 0, sizeof(struct suedef));
	flist(p, gcc_va, sue);
	tfree(p);
	return sue;
}


#ifdef notyet
struct gcc_attrib {
	int atype;
	union {
		int iarg; char *sarg;
	} a1;
	union {
		int iarg; char *sarg;
	} a2;
	union {
		int iarg; char *sarg;
	} a3;
};

typedef struct gcc_attrib g_attr_t;

/*
 * Parse an attribute tree and create a attribute struct.
 * If failed, return NULL>
 */
g_attr_t *
gcc_attrib_parse(NODE *p)
{
}

#endif

#endif
