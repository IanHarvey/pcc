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

# include "pass1.h"
#include <string.h>

/*
 * Three machine-dependent routines may be called during initialization:
 * 
 * zecode(CONSZ)	- sets a CONSZ sized block to zeros.
 * ninval(NODE *)	- prints an integer constant which may have
 *			  a label associated with it.
 * indata(CONSZ, int)	- prints an integer constant of size int bits.
 *
 * bitfields are merged into an integer in this file.
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
 * When a compile-time initializer is found, allocate space for the base
 * element in memory; if it is an open-ended array then have the elements
 * on a linked list.
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
 *
 * Scalinit b|rjar alltid med att kl{ttra upp till n{sta initierbara
 * v{rde samt tilldela det.  Sen kl{ttrar den ner till antingen n{sta 
 * niv} d{r den kan f|rv{nta sig en initialiserare eller till n{sta
 * niv} d{r }-flaggan {r satt.  Det g|r att rbrace() vet direkt om det 
 * kommit r{tt eller inte.
 * 
 */

/*
 * Good-to-know entries from symtab:
 *	soffset - # of bits from beginning of this structure.
 */

/*
 * Struct used in array initialisation.
 */
static struct instk {
	struct	instk *in_prev; /* linked list */
	int	in_sz;		/* size of array element */
	struct	symtab **in_xp;	 /* member in structure initializations */
	int	in_n;		/* number of initializations seen */
	struct	suedef *in_sue;
	union	dimfun *in_df;	/* dimoff/protos */
	TWORD	in_t;		/* type */
	struct	symtab *in_sym; /* stab index */
	int	in_fl;	/* flag which says if this level is controlled by {} */
	OFFSZ	in_off;		/* offset of the beginning of this level */
} *pstk, pbase;

static struct symtab *csym;

#define ISSOU(ty) (ty == STRTY || ty == UNIONTY)

//static void inforce(OFFSZ n);
static void stkpush(void);
//static void infld(CONSZ con, int sz);
#ifdef PCC_DEBUG
static void prtstk(struct instk *in);
#endif

/*
 * Linked list for initializations.  Initialized data is stored in INT size
 * blocks, and it is assumed that CONSZ can at least store an int.
 */
struct llist {
	SLIST_ENTRY(llist) next;
	CONSZ begsz;	/* bit offset of this entry */
	CONSZ *data;
} * curll;
static SLIST_HEAD(, llist) lpole;
static CONSZ basesz;
static int numents; /* # of array entries allocated */

/*
 * Allocate a new block for initializers using CONSZ, but only for
 * storing INTs.  Also get a new llist entry that is appended to the
 * end of the llist. Return that entry.
 */
static struct llist *
getll(void)
{
	struct llist *ll;
	int nch;

	ll = tmpalloc(sizeof(struct llist));
	nch = ((basesz+SZINT-1)/SZINT) * sizeof(CONSZ);
	ll->data = memset(tmpalloc(nch), 0, nch);
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
setll(CONSZ off)
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
	if (idebug >= 3)
		printf("beginit(), symtab = %p\n", sp);
#endif

	csym = sp;

	numents = 0; /* no entries in array list */
	basesz = tsize(DECREF(sp->stype), sp->sdf, sp->ssue);
	SLIST_INIT(&lpole);
	curll = ll = getll(); /* at least first entry in list */

	/* first element */
	is->in_sz = ISARY(sp->stype) ?
	    tsize(DECREF(sp->stype), sp->sdf, sp->ssue) : 0;
	is->in_xp = ISSOU(sp->stype) ? sp->ssue->suelem : NULL;
	is->in_n = 0;
	is->in_sue = sp->ssue;
	is->in_df = sp->sdf;
	is->in_t = sp->stype;
	is->in_sym = sp;
	is->in_fl = 1;
	is->in_off = 0;
	is->in_prev = NULL;
	pstk = is;
}

/*
 * Push a new entry on the initializer stack.
 * The new entry will be "decremented" to the new sub-type of the previous
 * entry when called.
 * Popping of entries is done elsewhere.
 */
void
stkpush()
{   
	struct instk *is;
	struct symtab *sq, *sp = pstk->in_sym;
	TWORD t = pstk->in_t;

#ifdef PCC_DEBUG
	if (idebug) {
		printf("stkpush: '%s' ", sp->sname);
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
	is->in_off = pstk->in_off;
	is->in_n = 0;
	if (ISSOU(t)) {
		sq = *pstk->in_xp;
		is->in_sz = 0;
		is->in_xp = ISSOU(sq->stype) ? sq->ssue->suelem : 0;
		is->in_sue = sq->ssue;
		is->in_df = sq->sdf;
		is->in_t = sq->stype;
		is->in_sym = sq;
	} else if (ISARY(t)) {
		is->in_sz = tsize(DECREF(t), pstk->in_df+1, pstk->in_sue);
		is->in_xp = ISSOU(DECREF(t)) ? pstk->in_sue->suelem : 0;
		is->in_sue = sp->ssue;
		is->in_df = sp->sdf;
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
		if (ISARY(pstk->in_t))
			pstk->in_n++;
		if (pstk->in_fl)
			break; /* need } */
		if (ISARY(pstk->in_t) && 
		    (pstk->in_sz == 0 || pstk->in_n < pstk->in_sz))
			break; /* ger more elements */
		pstk->in_prev->in_off += pstk->in_off;
	}
}

/*
 * Find current bit offset of the top element on the stack from
 * the beginning of the aggregate.
 */
static CONSZ
findoff(void)
{
	struct instk *is = pstk->in_prev;
	CONSZ off;

#ifdef PCC_DEBUG
	if (ISARY(pstk->in_t) || ISSOU(pstk->in_t))
		cerror("findoff on bad type");
#endif

	for (off = 0; is; is = is->in_prev) {
		if (ISARY(is->in_t)) {
			off += is->in_sz * is->in_n;
//printf("findoff: is->in_sz %d is->in_n %d\n", is->in_sz, is->in_n);
		} else if (ISSOU(is->in_t)) {
			cerror("findoff notyet struct");
		} else
			cerror("findoff bad stack");
	}
	if (idebug>1)
		printf("findoff: off %lld\n", off);
	return off;
}

/*
 * Set the value val with size fsz at offset off in the allocated block.
 */
static void
setval(CONSZ off, int fsz, CONSZ val)
{
	CONSZ *d, lw;
	int word, woff, lws;

	if (idebug>1)
		printf("setval: off %lld fsz %d val %lld\n", off, fsz, val);

	if (fsz == 0)
		return;

	d = setll(off)->data;	/* Get data area */
	off %= basesz;		/* Get offset in this area */
	if (off + fsz > basesz)
		cerror("setval: wrapping init");
	word = off / SZINT;	/* Get word in this area */
	woff = off % SZINT;	/* Get offset in word */
	if (fsz + woff > SZINT) {
		lws = SZINT - woff;
		lw = val & (((CONSZ)1 << lws)-1); /* XXX can this fail? */
		d[word] |= (lw << woff);
		fsz -= lws;
		if (fsz > SZINT)
			cerror("setval");
		word++;
		val >>= lws;
		woff = 0;
	}

	val &= ((CONSZ)1 << fsz)-1; /* XXX can this fail? */
	d[word] |= (val << woff);
//printf("setval: word %d woff %d fsz %d val %lld\n", word, woff, fsz, val);

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
		lbl = sp->soffset == NOOFFSET ? getlab() : sp->soffset;
		deflab1(lbl);
	}
}

/*
 * take care of generating a value for the initializer p
 * inoff has the current offset (last bit written)
 * in the current word being generated
 */
void
scalinit(NODE *p)
{
//	union dimfun *d;
//	struct suedef *sue;
	NODE *q;
//	TWORD t;

#ifdef PCC_DEBUG
	if (idebug > 2) {
		printf("scalinit(%p), in_off=" CONFMT "\n", p, pstk->in_off);
		fwalk(p, eprint, 0);
		prtstk(pstk);
	}
#endif

	if (nerrors)
		return;

	if (p->n_op != ICON && p->n_op != FCON)
		cerror("scalinit not leaf");
	/*
	 * Get to the simple type if needed.
	 */
	while (ISSOU(pstk->in_t) || ISARY(pstk->in_t))
		stkpush();
		
	/* let buildtree do typechecking */
	q = block(NAME, NIL,NIL, pstk->in_t, pstk->in_df, pstk->in_sue);
	p = buildtree(ASSIGN, q, p);
	nfree(p->n_left);
	q = p->n_right;
	nfree(p);

	/* handle bitfields special */
	if (pstk->in_sz < 0) {
		cerror("bitfields");
//		infld(p->n_left->n_lval, -pstk->in_sz);
		pstk->in_off += -pstk->in_sz;
		tfree(p);
	} else {
		CONSZ woff;
		int fsz;

		fsz = tsize(pstk->in_t, pstk->in_df, pstk->in_sue);
		woff = findoff();

		/* Convert floats to an array of ints */
		if (q->n_op == FCON) {
			CONSZ *iary;

			fsz = ftoint(q, &iary);
			for (; fsz >= SZINT; woff += SZINT, fsz -= SZINT)
				setval(woff, SZINT, *iary++);
			if (fsz)
				q->n_lval = *iary;
		}
		setval(woff, fsz, q->n_lval);

		tfree(q);
	}
	stkpop();
#ifdef PCC_DEBUG
	if (idebug > 2) {
		printf("scalinit e(%p), in_off=" CONFMT "\n", p, pstk->in_off);
	}
#endif
}

/*
 * final step of initialization.
 * print out init nodes and generate copy code (if needed).
 */
void
endinit(void)
{
	struct llist *ll;
	int i;

#ifdef PCC_DEBUG
	if (idebug)
		printf("endinit()\n");
#endif

	setscl(csym);

	/* Traverse all entries and print'em out */
	SLIST_FOREACH(ll, &lpole, next) {
		for (i = 0; i < basesz; i += SZINT) {
//printf("endinit: base %lld val %lld\n", ll->begsz, ll->data[0]);
			indata(ll->data[i/SZINT], basesz - i < SZINT ?
			    basesz - i : SZINT);
		}
	}
}

/*
 * process an initializer's left brace
 */
void
ilbrace()
{
//	struct instk *w;

#ifdef PCC_DEBUG
	if (idebug)
		printf("ilbrace()\n");
	if (idebug > 2)
		prtstk(pstk);
#endif

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

	/* Got right brace, search for corresponding in the stack */
	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {
		if(!pstk->in_fl)
			continue;

		/* we have one now */

		pstk->in_fl = 0;  /* cancel { */
		pstk->in_off = pstk->in_sue->suesize;
		stkpop();
		return;
	}

	/* these right braces match ignored left braces: throw out */
}

#if 0
/*
 * make inoff have the offset the next alignment of n
 */
void
vfdalign(int n)
{
	OFFSZ m;

	m = inoff;
	SETOFF( m, n );
	inforce( m );
}

/*	
 * force inoff to have the value n
 */
void	
inforce(OFFSZ n)
{	
	/* inoff is updated to have the value n */
	OFFSZ wb;
	int rest;
	/* rest is used to do a lot of conversion to ints... */

	if( inoff == n ) return;
	if (inoff > n)
		cerror("initialization alignment error: inoff %lld n %lld",
		    inoff, n);

	wb = inoff;
	SETOFF( wb, SZINT );

	/* wb now has the next higher word boundary */

	if( wb >= n ){ /* in the same word */
		rest = n - inoff;
		if (rest > 0)
			vfdzero( rest );
		return;
		}

	/* otherwise, extend inoff to be word aligned */

	rest = wb - inoff;
	if (rest > 0)
		vfdzero( rest );

	/* now, skip full words until near to n */

	rest = (n-inoff)/SZINT;
	if (rest > 0) {
		ADDENT(TZERO, rest, val);
		inoff += rest * SZINT;
	}

	/* now, the remainder of the last word */

	rest = n-inoff;
	if (rest > 0)
		vfdzero( rest );
	if( inoff != n ) cerror( "inoff error");

	}

/*
 * collect 
 */
void
infld(CONSZ con, int sz)
{  
	inoff += sz;
	if ((sz + inwd) > SZINT)
		cerror("infld: field > int");

#ifdef RTOLBYTES
	word |= ((unsigned)(con<<(SZINT-sz))) >> (SZINT-sz-inwd);
#else
#error fix big endian support
#endif

	inwd += sz;
	if (inoff % SZINT == 0) {
		ADDENT(TNOD, bcon(word), nod);
		word = inwd = 0;
	}
}

/*
 * define n bits of zeros in a vfd
 */
void
vfdzero(int n)
{
	inoff += n;
	inwd += n;
	if (inoff%ALINT ==0) {
		ADDENT(TNOD, bcon(word), nod);
		word = inwd = 0; 
	} 
}
#endif

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
 * Do an assignment to a struct element.
 */
void
asginit(NODE *des, NODE *p)
{
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
		printf(" ");
		if (in->in_df && in->in_df->ddim)
		    printf("arydim=%d ", in->in_df->ddim);
		if (in->in_sz) printf("sz=%d ", in->in_sz);
		printf("ninit=%d ", in->in_n);
		if (BTYPE(in->in_t) == STRTY)
			printf("stsize=%d ", in->in_sue->suesize);
		if (in->in_fl) printf("{ ");
		if (in->in_off) printf("off=%lld ", in->in_off);
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
		switch (sp->sclass) {
		case EXTDEF:
		case STATIC:
			setloc1(DATA);
			if (sp->sclass == STATIC && sp->slevel > 0)
				deflab1(sp->soffset);
			else
				defnam(sp);
			instring(p->n_sp->sname);
			nfree(p);
			break;
		default:
			cerror("notyet str[] init");
		}
		return;
	}

	switch (sp->sclass) {
	case STATIC:
	case EXTDEF:
		spname = sp;
		p = optim(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
		setscl(sp);
		if (p->n_right->n_op != ICON) /* XXX - fix long long etc. */
			cerror("yet only icon");
		ninval(p->n_right);
		tfree(p);
		break;

	case AUTO:
	case REGISTER:
		spname = sp;
		ecomp(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
		break;

	default:
		uerror("illegal initialization");
	}
}
