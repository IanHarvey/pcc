/*
 * Copyright (c) 2008 David Crawshaw <david@zentus.com>
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "pass1.h"

void
defloc(struct symtab *sp)
{
	static char *loctbl[] = { "text", "data", "section .rodata" };
	static int lastloc = -1;
	TWORD t;
	int s;

	t = sp->stype;
	s = ISFTN(t) ? PROG : ISCON(cqual(t, sp->squal)) ? RDATA : DATA;
	if (s == PROG)
		return;
	if (s != lastloc)
		printf("\t.%s\n", loctbl[s]);
	lastloc = s;
	printf("\t.align 4\n");
	if (sp->sclass == EXTDEF)
		printf("\t.global %s\n", sp->soname);
	if (sp->slevel == 0)
		printf("%s:\n", sp->soname);
	else
		printf(LABFMT ":\n", sp->soffset);
}

void
efcode()
{
	/* XXX */
}

void
bfcode(struct symtab **sp, int cnt)
{
	/* XXX */
}

void
bccode()
{
	SETOFF(autooff, SZINT);
}

void
ejobcode(int flag)
{
}

void
bjobcode()
{
}

NODE *
funcode(NODE *p)
{
	/* XXX */
	return p;
}

int
fldal(unsigned int t)
{
	uerror("illegal field type");
	return ALINT;
}

void
fldty(struct symtab *p)
{
}

int
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	return 0;
}
