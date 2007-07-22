/*	$Id$	*/

/*
 * Copyright (c) 2004, 2007 Anders Magnusson (ragge@ludd.ltu.se).
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

#include "pass1.h"
#include <string.h>

/*
 * Four machine-dependent routines may be called during initialization:
 * 
 * instring(char *str)	- Print out a string.
 * zbits(OFFSZ, int)	- sets int bits of zero at position OFFSZ.
 * infld(CONSZ off, int fsz, CONSZ val)
 *			- sets the bitfield val starting at off and size fsz.
 * inval(CONSZ off, int fsz, NODE *)
 *			- prints an integer constant which may have
 *			  a label associated with it, located at off and
 *			  size fsz.
 *
 * Initialization may be of different kind:
 * - Initialization at compile-time, all values are constants and laid
 *   out in memory. Static or extern variables outside functions.
 * - Initialization at run-time, written to their values as code.
 *
 * Currently run-time-initialized variables are only initialized by using
 * move instructions.  An optimization might be to detect that it is
 * initialized with constants and therefore copied from readonly memory.
 */

/*
 * The base element(s) of an initialized variable is kept in a linked 
 * list, allocated while initialized.
 *
 * When a scalar is found, entries are popped of the instk until it's
 * possible to find an entry for a new scalar; then onstk() is called 
 * to get the correct type and size of that scalar.
 *
 * If a right brace is found, pop the stack until a matching left brace
 * were found while filling the elements with zeros.  This left brace is
 * also marking where the current level is for designated initializations.
 *
 * Position entries are increased when traversing back down into the stack.
 */

/*
 * Good-to-know entries from symtab:
 *	soffset - # of bits from beginning of this structure.
 */

/*
 * TO FIX:
 * - Alignment of structs on like i386 char members.
 * - Runtime init of structs.
 * - Correct calculation of multi-dim arrays.
 * - Runtime init of arrays.
 */

/*
 * Struct used in array initialisation.
 */
static struct instk {
	struct	instk *in_prev; /* linked list */
	struct	symtab **in_xp;	 /* member in structure initializations */
	int	in_n;		/* number of arrays seen so far */
	TWORD	in_t;		/* type for this level */
	struct	symtab *in_sym; /* stab index */
	int	in_fl;	/* flag which says if this level is controlled by {} */
} *pstk, pbase;

static struct symtab *csym;

#define ISSOU(ty) (ty == STRTY || ty == UNIONTY)

#ifdef PCC_DEBUG
static void prtstk(struct instk *in);
#endif

/*
 * Linked lists for initializations.
 */
struct ilist {
	struct ilist *next;
	CONSZ off;	/* bit offset of this entry */
	int fsz;	/* bit size of this entry */
	NODE *n;	/* node containing this data info */
};

struct llist {
	SLIST_ENTRY(llist) next;
	CONSZ begsz;	/* bit offset of this entry */
	struct ilist *il;
} *curll;
static SLIST_HEAD(, llist) lpole;
static CONSZ basesz;
static int numents; /* # of array entries allocated */

static struct ilist *
getil(struct ilist *next, CONSZ b, int sz, NODE *n)
{
	struct ilist *il = tmpalloc(sizeof(struct ilist));

	il->off = b;
	il->fsz = sz;
	il->n = n;
	il->next = next;
	return il;
}

/*
 * Allocate a new block for initializers using CONSZ, but only for
 * storing INTs.  Also get a new llist entry that is appended to the
 * end of the llist. Return that entry.
 */
static struct llist *
getll(void)
{
	struct llist *ll;

	ll = tmpalloc(sizeof(struct llist));
	ll->begsz = numents * basesz;
	SLIST_INSERT_LAST(&lpole, ll, next);
	numents++;
	return ll;
}

/*
 * Return structure containing off bitnumber.
 * Allocate more entries, if needed.
 * This is not bright implemented.
 */
static struct llist *
setll(OFFSZ off)
{
	struct llist *ll;

	/* Ensure that we have enough entries */
	while (off >= basesz * numents)
		(void)getll();
	SLIST_FOREACH(ll, &lpole, next)
		if (ll->begsz <= off && ll->begsz + basesz > off)
			break;
	return ll; /* ``cannot fail'' */
}

/*
 * beginning of initialization; allocate space to store initialized data.
 * remember storage class for writeout in endinit().
 * p is the newly declarated type.
 */
void
beginit(struct symtab *sp)
{
	struct instk *is = &pbase;
	struct llist *ll;

#ifdef PCC_DEBUG
	if (idebug)
		printf("beginit(), sclass %s\n", scnames(sp->sclass));
#endif

	csym = sp;

	numents = 0; /* no entries in array list */
	basesz = tsize(DECREF(sp->stype), sp->sdf, sp->ssue);
	SLIST_INIT(&lpole);
	curll = ll = getll(); /* at least first entry in list */

	/* first element */
	is->in_xp = ISSOU(sp->stype) ? sp->ssue->suelem : NULL;
	is->in_n = 0;
	is->in_t = sp->stype;
	is->in_sym = sp;
	is->in_fl = 0;
	is->in_prev = NULL;
	pstk = is;
}

/*
 * Push a new entry on the initializer stack.
 * The new entry will be "decremented" to the new sub-type of the previous
 * entry when called.
 * Popping of entries is done elsewhere.
 */
static void
stkpush(void)
{
	struct instk *is;
	struct symtab *sq, *sp = pstk->in_sym;
	TWORD t = pstk->in_t;

#ifdef PCC_DEBUG
	if (idebug) {
		printf("stkpush: '%s' %s ", sp->sname, scnames(sp->sclass));
		tprint(stdout, t, 0);
	}
#endif

	is = tmpalloc(sizeof(struct instk));
	/*
	 * Figure out what the next initializer will be, and push that on 
	 * the stack.  If this is an array, just decrement type, if it
	 * is a struct or union, extract the next element.
	 */
	
	is->in_fl = 0;
	is->in_n = 0;
	if (ISSOU(t)) {
		sq = *pstk->in_xp;
		is->in_xp = ISSOU(sq->stype) ? sq->ssue->suelem : 0;
		is->in_t = sq->stype;
		is->in_sym = sq;
	} else if (ISARY(t)) {
		is->in_xp = ISSOU(DECREF(t)) ? pstk->in_sym->ssue->suelem : 0;
		is->in_t = DECREF(t);
		is->in_sym = sp;
	} else
		cerror("onstk");
	is->in_prev = pstk;
	pstk = is;

#ifdef PCC_DEBUG
	if (idebug) {
		printf(" newtype ");
		tprint(stdout, is->in_t, 0);
		printf("\n");
	}
#endif
}

/*
 * pop down to either next level than can handle a new initializer or
 * to the next braced level.
 */
static void
stkpop(void)
{
#ifdef PCC_DEBUG
	if (idebug)
		printf("stkpop\n");
#endif
	for (; pstk; pstk = pstk->in_prev) {
		if (pstk->in_t == STRTY) {
			pstk->in_xp++;
			if (*pstk->in_xp != NULL)
				break;
		}
		if (ISSOU(pstk->in_t) && pstk->in_fl)
			break; /* need } */
		if (ISARY(pstk->in_t)) {
			pstk->in_n++;
			if (pstk->in_sym->sdf->ddim == 0 ||
			    pstk->in_n < pstk->in_sym->sdf->ddim)
				break; /* ger more elements */
		}
	}
}

/*
 * Find current bit offset of the top element on the stack from
 * the beginning of the aggregate.
 */
static CONSZ
findoff(void)
{
	struct instk *is;
	CONSZ off;

#ifdef PCC_DEBUG
	if (ISARY(pstk->in_t) || ISSOU(pstk->in_t))
		cerror("findoff on bad type");
#endif

	/*
	 * Offset calculations. If:
	 * - previous type is STRTY, soffset has in-struct offset.
	 * - this type is ARY, offset is ninit*stsize.
	 */
	for (off = 0, is = pstk; is; is = is->in_prev) {
		if (is->in_prev && is->in_prev->in_t == STRTY)
			off += is->in_sym->soffset;
		if (ISARY(is->in_t)) /* XXX multi-dim arrays */
			off += is->in_sym->ssue->suesize * is->in_n;
	}
	if (idebug>1) {
		printf("findoff: off %lld\n", off);
		prtstk(pstk);
	}
	return off;
}

/*
 * Insert the node p with size fsz at position off.
 * Bit fields are already dealt with, so a node of correct type
 * with correct alignment and correct bit offset is given.
 */
static void
nsetval(CONSZ off, int fsz, NODE *p)
{
	struct llist *ll;
	struct ilist *il;

	if (idebug>1)
		printf("setval: off %lld fsz %d p %p\n", off, fsz, p);

	if (fsz == 0)
		return;

	ll = setll(off);
	off -= ll->begsz;
	if (ll->il == NULL) {
		ll->il = getil(NULL, off, fsz, p);
	} else {
		for (il = ll->il; il->next; il = il->next)
			if (il->off <= off && il->next->off > off)
				break;
		if (il->off == off) {
			/* replace */
			nfree(il->n);
			il->n = p;
		} else
			il->next = getil(il->next, off, fsz, p);
	}
}

/*
 * Align data and set correct location.
 */
static void
setscl(struct symtab *sp)
{
	int lbl; /* XXX ? */

	setloc1((sp->squal << TSHIFT) & CON ? RDATA : DATA);
	defalign(talign(sp->stype, sp->ssue));
	lbl = 0;
	if (sp->sclass == EXTDEF ||
	    (sp->sclass == STATIC && sp->slevel == 0)) {
		defnam(sp);
	} else {
		/* XXX ??? */
		lbl = sp->soffset == NOOFFSET ? getlab() : sp->soffset;
		deflab1(lbl);
	}
}

#if 0
/*
 * Create a tree as a struct reference for run-time init.
 */
static NODE *
mkptree(struct instk *in)
{
	NODE *r;

	if (in->in_prev == NULL) {
		spname = csym;
		return buildtree(NAME, NIL, NIL);
	}
	if (ISARY(in->in_t))
		return buildtree(UMUL,
		    buildtree(PLUS, mkptree(in->in_prev), bcon(in->in_n)), NIL);
	if (ISSOU(in->in_t) ||
	    in->in_sym->sclass == MOS || in->in_sym->sclass == MOU) {
		r = block(NAME, NIL, NIL, INT, 0, MKSUE(INT));
		r->n_name = in->in_sym->sname;
		return buildtree(STREF,
		    buildtree(ADDROF, mkptree(in->in_prev), NIL), r);
	}
prtstk(in);
	cerror("mkptree");
	return 0; /* XXX */
}
#endif

/*
 * take care of generating a value for the initializer p
 * inoff has the current offset (last bit written)
 * in the current word being generated
 */
void
scalinit(NODE *p)
{
	CONSZ woff;
	NODE *q;
	int fsz;

#ifdef PCC_DEBUG
	if (idebug > 2) {
		printf("scalinit(%p)\n", p);
		fwalk(p, eprint, 0);
		prtstk(pstk);
	}
#endif

	if (nerrors)
		return;

	if (csym->sclass != AUTO && p->n_op != ICON && p->n_op != FCON)
		cerror("scalinit not leaf");

	/* Out of elements? */
	if (pstk == NULL) {
		uerror("excess of initializing elements");
		return;
	}

	/*
	 * Get to the simple type if needed.
	 */
	while (ISSOU(pstk->in_t) || ISARY(pstk->in_t))
		stkpush();
		
	/* let buildtree do typechecking (and casting) */
	q = block(NAME, NIL,NIL, pstk->in_t, pstk->in_sym->sdf,
	    pstk->in_sym->ssue);
	p = buildtree(ASSIGN, q, p);
	nfree(p->n_left);
	q = p->n_right;
	nfree(p);

	/* bitfield sizes are special */
	if (pstk->in_sym->sclass & FIELD)
		fsz = -(pstk->in_sym->sclass & FLDSIZ);
	else
		fsz = tsize(pstk->in_t, pstk->in_sym->sdf, pstk->in_sym->ssue);
	woff = findoff();

	nsetval(woff, fsz, q);

	stkpop();
#ifdef PCC_DEBUG
	if (idebug > 2) {
		printf("scalinit e(%p)\n", p);
	}
#endif
}

/*
 * Generate code to insert a value into a bitfield.
 */
static void
insbf(OFFSZ off, int fsz, int val)
{
	struct symtab sym;
	NODE *p, *r;
	TWORD typ;

#ifdef PCC_DEBUG
	if (idebug > 1)
		printf("insbf: off %lld fsz %d val %d\n", off, fsz, val);
#endif

	if (fsz == 0)
		return;

	/* small opt: do char instead of bf asg */
	if ((off & (ALCHAR-1)) == 0 && fsz == SZCHAR)
		typ = CHAR;
	else
		typ = INT;
	/* Fake a struct reference */
	spname = csym;
	p = buildtree(ADDROF,
	    buildtree(NAME, NIL, NIL), NIL);
	r = block(ICON, NIL, NIL, typ, 0, MKSUE(typ));
	sym.stype = typ;
	sym.squal = 0;
	sym.sdf = 0;
	sym.ssue = MKSUE(typ);
	sym.soffset = off;
	sym.sclass = typ == INT ? FIELD | fsz : MOU;
	r->n_sp = &sym;
	p = block(STREF, p, r, INT, 0, MKSUE(INT));
	ecode(buildtree(ASSIGN, stref(p), bcon(val)));
}

/*
 * Clear a bitfield, starting at off and size fsz.
 */
static void
clearbf(OFFSZ off, OFFSZ fsz)
{
	/* Pad up to the next even initializer */
	if ((off & (ALCHAR-1)) || (fsz < SZCHAR)) {
		int ba = ((off + (SZCHAR-1)) & ~(SZCHAR-1)) - off;
		if (ba > fsz)
			ba = fsz;
		insbf(off, ba, 0);
		off += ba;
		fsz -= ba;
	}
	while (fsz >= SZCHAR) {
		insbf(off, SZCHAR, 0);
		off += SZCHAR;
		fsz -= SZCHAR;
	}
	if (fsz)
		insbf(off, fsz, 0);
}

/*
 * final step of initialization.
 * print out init nodes and generate copy code (if needed).
 */
void
endinit(void)
{
	struct llist *ll;
	struct ilist *il;
	int fsz;
	OFFSZ lastoff, tbit;

#ifdef PCC_DEBUG
	if (idebug)
		printf("endinit()\n");
#endif

	setscl(csym);

	/* Calculate total block size */
	if (ISARY(csym->stype) && csym->sdf->ddim == 0) {
		tbit = numents*basesz; /* open-ended arrays */
		csym->sdf->ddim = numents;
		csym->soffset = NOOFFSET;
		if (csym->sclass == AUTO) /* Get stack space */
			oalloc(csym, &autooff);
	} else
		tbit = tsize(csym->stype, csym->sdf, csym->ssue);

	/* Traverse all entries and print'em out */
	lastoff = 0;
	SLIST_FOREACH(ll, &lpole, next) {
		for (il = ll->il; il; il = il->next) {
#ifdef PCC_DEBUG
			if (idebug > 1) {
				printf("off %lld size %d val %lld type ",
				    ll->begsz+il->off, il->fsz, il->n->n_lval);
				tprint(stdout, il->n->n_type, 0);
				printf("\n");
			}
#endif
			fsz = il->fsz;
			if (csym->sclass == AUTO) {
				struct symtab sym;
				NODE *p, *r, *n;

				if (ll->begsz + il->off > lastoff)
					clearbf(lastoff,
					    (ll->begsz + il->off) - lastoff);

				/* Fake a struct reference */
				spname = csym;
				p = buildtree(ADDROF,
				    buildtree(NAME, NIL, NIL), NIL);
				n = il->n;
				r = block(ICON, NIL, NIL, INT, 0, MKSUE(INT));
				sym.stype = n->n_type;
				sym.squal = n->n_qual;
				sym.sdf = n->n_df;
				sym.ssue = n->n_sue;
				sym.soffset = ll->begsz + il->off;
				sym.sclass = fsz < 0 ? FIELD | -fsz : 0;
				r->n_sp = &sym;
				p = block(STREF, p, r, INT, 0, MKSUE(INT));
				ecode(buildtree(ASSIGN, stref(p), il->n));
				if (fsz < 0)
					fsz = -fsz;

			} else {
				if (ll->begsz + il->off > lastoff)
					zbits(lastoff,
					    (ll->begsz + il->off) - lastoff);
				if (fsz < 0) {
					fsz = -fsz;
					infld(il->off, fsz, il->n->n_lval);
				} else
					ninval(il->off, fsz, il->n);
				nfree(il->n);
			}
			lastoff = ll->begsz + il->off + fsz;
		}
	}
	if (csym->sclass == AUTO) {
		clearbf(lastoff, tbit-lastoff);
	} else
		zbits(lastoff, tbit-lastoff);
}

/*
 * process an initializer's left brace
 */
void
ilbrace()
{

#ifdef PCC_DEBUG
	if (idebug)
		printf("ilbrace()\n");
	if (idebug > 2)
		prtstk(pstk);
#endif

	if (pstk == NULL)
		return;

	stkpush();
	pstk->in_fl = 1; /* mark lbrace */
}

/*
 * called when a '}' is seen
 */
void
irbrace()
{
#ifdef PCC_DEBUG
	if (idebug)
		printf("irbrace()\n");
	if (idebug > 2)
		prtstk(pstk);
#endif

	if (pstk == NULL)
		return;

	/* Got right brace, search for corresponding in the stack */
	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {
		if(!pstk->in_fl)
			continue;

		/* we have one now */

		pstk->in_fl = 0;  /* cancel { */
		if (ISARY(pstk->in_t))
			pstk->in_n = pstk->in_sym->sdf->ddim;
		stkpop();
		return;
	}
}

/*
 * Initialize a specific element, as per C99.
 */
static void
elminit(NODE *des, NODE *p)
{
//	cerror("elminit");
#ifdef notyet
	NODE *q, *pole = NULL;
	struct symtab *sp = csym;
	struct instk *is = pstk;
	OFFSZ off = 0;

	/* reverse links */
	for (; des;) {
		q = des;
		des = des->n_left;
		q->n_left = pole;
		pole = q;
	}

	/* find at which offset this element should be written */
	for (q = des; q->n_left; q = q->n_left) {
		switch (q->n_op) {
		case LB: /* array */
			an = q->n_right->n_lval;
			if (!ISARY(sp->stype))
#endif
}

/*
 * Convert a string to an array of char for asginit.
 */
static void
strcvt(NODE *des, NODE *p)
{
	char *s;
	int i;

	for (s = p->n_sp->sname; *s != 0; s++) {
		if (*s == '\\')
			i = esccon(&s);  
		else
			i = *s;
		asginit(des, bcon(i));
	} 
	nfree(p);
}

/*
 * Do an assignment to a struct element.
 */
void
asginit(NODE *des, NODE *p)
{
	int g;

	/* convert string to array of char */
	if (p && DEUNSIGN(p->n_type) == ARY+CHAR) {
		if ((g = pstk->in_fl) == 0)
			ilbrace();

		strcvt(des, p);
		if (g == 0)
			irbrace();
		return;
	}

	if (p == NULL) { /* only end of compound stmt */
		irbrace();
	} else if (des == NULL) { /* assign next element */
		scalinit(p);
	} else
		elminit(des, p);
}

#ifdef PCC_DEBUG
void
prtstk(struct instk *in)
{
	int i, o = 0;

	printf("init stack:\n");
	for (; in != NULL; in = in->in_prev) {
		for (i = 0; i < o; i++)
			printf("  ");
		printf("%p) '%s' ", in, in->in_sym->sname);
		tprint(stdout, in->in_t, 0);
		printf(" %s ", scnames(in->in_sym->sclass));
		if (in->in_sym->sdf && in->in_sym->sdf->ddim)
		    printf("arydim=%d ", in->in_sym->sdf->ddim);
		printf("ninit=%d ", in->in_n);
		if (BTYPE(in->in_t) == STRTY)
			printf("stsize=%d ", in->in_sym->ssue->suesize);
		if (in->in_fl) printf("{ ");
		printf("soff=%d ", in->in_sym->soffset);
		if (in->in_t == STRTY)
			printf("curel %s ", in->in_xp[0]->sname);
		printf("\n");
		o++;
	}
}
#endif

/*
 * Do a simple initialization.
 * At block 0, just print out the value, at higher levels generate
 * appropriate code.
 */
void
simpleinit(struct symtab *sp, NODE *p)
{
	/* May be an initialization of an array of char by a string */
	if (DEUNSIGN(p->n_type) == ARY+CHAR && DEUNSIGN(sp->stype) == ARY+CHAR){
		/* Handle "aaa" as { 'a', 'a', 'a' } */
		beginit(sp);
		strcvt(NULL, p);
		if (csym->sdf->ddim == 0)
			scalinit(bcon(0)); /* Null-term arrays */
		endinit();
		return;
	}

	switch (sp->sclass) {
	case STATIC:
	case EXTDEF:
		spname = sp;
		p = optim(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
		setscl(sp);
		ninval(0, p->n_right->n_sue->suesize, p->n_right);
		tfree(p);
		break;

	case AUTO:
	case REGISTER:
		if (ISSOU(sp->stype) || ISARY(sp->stype))
			cerror("no aggregate init");
		spname = sp;
		ecomp(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
		break;

	default:
		uerror("illegal initialization");
	}
}
