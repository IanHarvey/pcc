

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
		ip->ip_node = treecpy(ip->ip_node);
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

	SIMPLEQ_FOREACH(ip, &w->shead, sqelem) {
		if (ip->type == IP_REF)
			inline_ref(ip->ip_name);
		else if (Oflag)
			topt_compile(ip);
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
