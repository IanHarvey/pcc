

#include "pass1.h"

#include <stdlib.h>
#include <stdarg.h>

/*
 * ilink from ipole points to the next struct in the list of functions.
 * ilink also point to the last struct in the statement list, if it
 *   is the first element at left of the list.
 * next points to next struct in the list of saved statements.
 */
static struct istat {
	struct istat *ilink;
	struct istat *next;
	char *name;
	int type;
	int numval;
	NODE *p;
} *ipole;

#define	ISNODE	1
#define	ISSTR	2
#define	ISREF	3

int isinlining;
int inlnodecnt, inlstatcnt;

#define	ialloc() permalloc(sizeof(struct istat)); inlstatcnt++
#define	nalloc() permalloc(sizeof(NODE))

static NODE *
treecpy(NODE *p)
{
	NODE *q;

	q = nalloc();
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
	struct istat *is;

	if (sdebug)
		printf("refnode(%s)\n", str);

	is = ialloc();
	is->ilink = is->next = NULL;
	if (ipole->next == NULL) {
		ipole->next = is;
	} else {
		ipole->next->ilink->next = is;
	}
	ipole->next->ilink = is;
	is->type = ISREF;
	is->name = str;
}

void
inline_savestring(char *str)
{
	struct istat *is;

	if (sdebug)
		printf("inline_savestring(%s)\n", str);

	is = ialloc();
	is->ilink = is->next = NULL;
	if (ipole->next == NULL) {
		ipole->next = is;
	} else {
		ipole->next->ilink->next = is;
	}
	ipole->next->ilink = is;
	is->type = ISSTR;
	is->name = str;
}

void
inline_savenode(NODE *p)
{
	struct istat *is;

	if (sdebug)
		printf("inline_savenode(%p)\n", p);

	is = ialloc();
	is->ilink = is->next = NULL;
	if (ipole->next == NULL) {
		ipole->next = is;
	} else {
		ipole->next->ilink->next = is;
	}
	ipole->next->ilink = is;
	is->type = ISNODE;
	is->p = treecpy(p);
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
	is->next = NULL;
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
	while (w != NULL) {
		switch (w->type) {
		case ISNODE:
#if defined(MULTIPASS)
			cerror("node in inline");
#else
			p2compile(w->p);
			break;
#endif
		case ISSTR:
			printf("%s", w->name);
			break;
		case ISREF:
			inline_ref(w->name);
			break;
		default:
			cerror("puto %d", w->type);
		}
		w = w->next;
	}
}

void
inline_prtout()
{
	struct istat *w = ipole;
	int gotone = 0;

	if (w == NULL)
		return;
	while (w != NULL) {
		if (w->type == 1) {
			puto(w->next);
			w->type = 2;
			gotone++;
		}
		w = w->ilink;
	}
	if (gotone)
		return inline_prtout();
}
