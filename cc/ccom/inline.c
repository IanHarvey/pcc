/*	$Id$	*/
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
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


#include "pass1.h"

#include <stdlib.h>
#include <stdarg.h>

/*
 * ilink from ipole points to the next struct in the list of functions.
 */
static struct istat {
	struct istat *ilink;
	SIMPLEQ_HEAD(, interpass) shead;
	char *name;
	int type;
} *ipole;

#define	IP_REF	(MAXIP+1)

int isinlining, recovernodes;
int inlnodecnt, inlstatcnt;

#define	ialloc() permalloc(sizeof(struct istat)); inlstatcnt++
#define	nalloc() permalloc(sizeof(NODE))

static void
tcnt(NODE *p)
{
	inlnodecnt++;
}

static NODE *
treecpy(NODE *p)
{
	NODE *q;

	q = talloc();
	inlnodecnt++;
	*q = *p;
	switch (optype(q->n_op)) {
	case BITYPE:
		q->n_right = treecpy(p->n_right);
	case UTYPE:
		q->n_left = treecpy(p->n_left);
	}
	return q;
}

static struct istat *
findfun(char *name)
{
	struct istat *is = ipole;
	while (is) {
		if (is->name == name)
			return is;
		is = is->ilink;
	}
	return NULL;
}

static void
refnode(char *str)
{
	struct interpass *ip;

	if (sdebug)
		printf("refnode(%s)\n", str);

	ip = permalloc(sizeof(*ip));
	ip->type = IP_REF;
	ip->ip_name = str;
	inline_addarg(ip);
}

void
inline_addarg(struct interpass *ip)
{
	SIMPLEQ_INSERT_TAIL(&ipole->shead, ip, sqelem);
	if (ip->type == IP_NODE)
		walkf(ip->ip_node, tcnt); /* Count as saved */
}

void
inline_start(char *name)
{
	struct istat *is;

	if (sdebug)
		printf("inline_start(\"%s\")\n", name);

	if (isinlining)
		cerror("already inlining function");
	if (findfun(name))
		cerror("inline function already defined");

	is = ialloc();
	is->ilink = ipole;
	ipole = is;
	is->name = name;
	is->type = 0;
	SIMPLEQ_INIT(&is->shead);
	isinlining++;
}

void
inline_end()
{
	if (sdebug)
		printf("inline_end()\n");

	isinlining = 0;
}

void
inline_ref(char *name)
{
	struct istat *w = ipole;

	if (sdebug)
		printf("inline_ref(\"%s\")\n", name);
	if (isinlining)
		refnode(name);
	else
		while (w != NULL) {
			if (w->name == name) {
				if (w->type == 0)
					w->type = 1;
				return;
			}
			w = w->ilink;
		}
}

static void
puto(struct istat *w)
{
	struct interpass *ip;

	while ((ip = SIMPLEQ_FIRST(&w->shead)) != NULL) {
		SIMPLEQ_REMOVE_HEAD(&w->shead, sqelem);
		if (ip->type == IP_REF)
			inline_ref(ip->ip_name);
		else
			pass2_compile(ip);
	}
}

void
inline_prtout()
{
	struct istat *w = ipole;
	int gotone = 0;

	if (w == NULL)
		return;
	recovernodes++;
	while (w != NULL) {
		if (w->type == 1) {
			puto(w);
			w->type = 2;
			gotone++;
		}
		w = w->ilink;
	}
	if (gotone)
		inline_prtout();
	recovernodes--;
}
