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

#define NEWINIT /* new init-style */
/*
 * Four machine-dependent routines may be called during initialization:
 * 
 * inval(CONSZ)		- writes an integer constant.
 * zecode(CONSZ)	- sets a CONSZ sized block to zeros.
 * finval(NODE *)	- outputs a floating-point constant.
 * ninval(NODE *)	- prints an integer constant which may have
 *			  a label associated with it.
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
#ifdef NEWINIT
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
#endif

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

int ibseen;	/* the number of } constructions which have been filled */
int ilocctr;	/* location counter for current initialization */
int maystr;	/* do not store a string if found */

static int inwd;		/* current bit offsed in word */
static CONSZ word;		/* word being built from fields */
static struct symtab *csym;

#define ISSOU(ty) (ty == STRTY || ty == UNIONTY)

static int howinit;	/* store in read-only or read-write segment */
#define ROINIT	0	/* Data is read-only (pseudo) */
#define RWINIT	1	/* Data is read-write */
#define DOCOPY	2	/* must copy (initialized on stack) */
#define SIMPLE	4	/* simple assignment for automatics */

#ifdef NEWINIT
#else
static void instk(struct symtab *p, TWORD t, union dimfun *d,
    struct suedef *, OFFSZ off);
#endif
static void vfdalign(int n);
static void inforce(OFFSZ n);
//static void infld(CONSZ, int sz);
//static void kinit(NODE *p, int sz);
static void stkpush(void);
#ifdef PCC_DEBUG
static void prtstk(struct instk *in);
#endif

/*
 * Initializations of a given object will be stored in a
 * linked list until all initializations are done.  This is
 * needed to get C99 element initializations to work.
 */
struct ilist {
	struct ilist *next;
	int type;
#define TNOD	1
#define TZERO	3	/* val used */
	union {
		NODE *nod;
		CONSZ val;
	} u;
} *ilist, **iend;

#define ADDENT(t, v, f) { \
	struct ilist *i = tmpalloc(sizeof(struct ilist)); \
	i->type = t; \
	i->u.f = v; \
	i->next = NULL; \
	*iend = i; \
	iend = &i->next; \
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

#ifdef PCC_DEBUG
	if (idebug >= 3)
		printf("beginit(), symtab = %p\n", sp);
#endif

	switch (sp->sclass) {
	case STATIC:
	case EXTDEF:
		howinit = (sp->squal >> TSHIFT) & CON ? ROINIT : RWINIT;
		break;

	case AUTO:
	case REGISTER:
		if (ISARY(sp->stype) || ISSOU(sp->stype))
			howinit = ROINIT|DOCOPY;
		else
			howinit = RWINIT|SIMPLE;
		break;

	default:
		uerror("illegal initialization");
		return;
	}

	csym = sp;
	inoff = 0;
	ibseen = 0;
	ilist = NULL;
	iend = &ilist;

#ifdef NEWINIT
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
#else
	instk(p, p->stype, p->sdf, p->ssue, inoff);
#endif
}

#ifdef NEWINIT
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

#ifdef PCC_DEBUG
	if (idebug)
		printf("stkpush: '%s' ", sp->sname);
#endif

	is = tmpalloc(sizeof(struct instk));
	/*
	 * Figure out what the next initializer will be, and push that on 
	 * the stack.  If this is an array, just decrement type, if it
	 * is a struct or union, extract the next element.
	 */
	if (ISSOU(sp->stype)) {
		sq = *pstk->in_xp;
		is->in_sym = sq;
		is->in_t = sq->stype;
		is->in_df = sq->sdf;
		is->in_sue = sq->ssue;
		is->in_off += sq->soffset;
		is->in_fl = 0;
	} else if (ISARY(sp->stype)) {
		is->in_sym = sp;
		is->in_t = DECREF(sp->stype);
	} else
		cerror("onstk");
	is->in_prev = pstk;
	pstk = is;
}

#else

/*
 * make a new entry on the parameter stack to initialize p
 */
void
instk(struct symtab *p, TWORD t, union dimfun *d, struct suedef *sue, OFFSZ off)
{   
	struct instk *sp;

	for (;;) {
#ifdef PCC_DEBUG
		if (idebug) {
			printf("instk: '%s' ", p->sname);
			tprint(stdout, t, 0);
			printf(" ");
			if (d) printf("dim=%d ", d->ddim);
			if (sue) printf("stsize=%d ", sue->suesize);
			printf("off=%lld\n", off);
		}
#endif

		/* save information on the stack */
		sp = tmpalloc(sizeof(struct instk));
		sp->in_prev = pstk;
		pstk = sp;
		pstk->in_fl = 0;	/* { flag */
		pstk->in_sym = p;
		pstk->in_t = t;
		pstk->in_df = d;
		pstk->in_sue = sue;
		pstk->in_n = 0;	 /* number seen */
		pstk->in_xp = ISSOU(t) ? sue->suelem : NULL;
		pstk->in_off = off;/* offset at the beginning of this element */

		/* if t is an array, DECREF(t) can't be a field */
		/* in_sz has size of array elements, and -size for fields */
		if (ISARY(t)) {
			pstk->in_sz = tsize(DECREF(t), d+1, sue);
		} else if (p->sclass & FIELD){
			pstk->in_sz = - (p->sclass & FLDSIZ);
		} else {
			pstk->in_sz = 0;
		}

#if 0
		if (iclass == REGISTER &&
		    (ISSOU(t) || ISARY(t)))
				uerror("erroneous register initialization");
#endif

		/* now, if this is not a scalar, put on another element */

		if (ISARY(t)) {
			t = DECREF(t);
			++d;
		} else if (ISSOU(t)) {
			if (pstk->in_sue == 0) {
				uerror("can't initialize undefined %s",
				    t == STRTY ? "structure" : "union");
				return;
			}
			p = *pstk->in_xp;
			if (((p->sclass != MOS && t == STRTY) ||
			    (p->sclass != MOU && t == UNIONTY)) &&
			    !(p->sclass&FIELD))
				cerror("insane %s member list",
				    t == STRTY ? "structure" : "union");
			t = p->stype;
			d = p->sdf;
			sue = p->ssue;
			off += p->soffset;
		} else {
			if (DEUNSIGN(pstk->in_t) == CHAR &&
			    pstk->in_prev != NULL &&
			    ISARY(pstk->in_prev->in_t))
				maystr = 1;
			else
				maystr = 0;
			return;
		}
	}
}
#endif

#ifdef NEWINIT
/*
 * pop down to either next level than can handle a new initializer or
 * to the next braced level.
 */
static void
stkpop(void)
{
	for (; pstk; pstk = pstk->in_prev) {
		if (ISARY(pstk->in_t))
			pstk->in_n++;
		if (pstk->in_fl)
			break; /* need } */
		if (ISARY(pstk->in_t) && pstk->in_n < pstk->in_sz)
			break; /* ger more elements */
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
		printf("scalinit(%p)\n", p);
		fwalk(p, eprint, 0);
		prtstk(pstk);
	}
#endif

	if (nerrors)
		return;

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

	ADDENT(TNOD, q, nod);

	stkpop();
}

#else

/*
 * take care of generating a value for the initializer p
 * inoff has the current offset (last bit written)
 * in the current word being generated
 */
void
doinit(NODE *p)
{
	union dimfun *d;
	struct suedef *sue;
	int sz;
	TWORD t;
	int o;

#ifdef PCC_DEBUG
	if (idebug > 2) {
		printf("doinit(%p)\n", p);
		fwalk(p, eprint, 0);
		prtstk(pstk);
	}
#endif

	if (nerrors)
		return;

	if (howinit & SIMPLE) {
		/* generate tree and get out */
		spname = pstk->in_sym;
		ecomp(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
		return;
	}
	if (howinit & DOCOPY && ISSOU(p->n_type)) {
		/* struct copy in initialization */
		spname = csym;
		ecomp(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
		howinit = SIMPLE; /* to avoid endinit() */
		return;
	}

	if (p->n_op == NAME && DEUNSIGN(p->n_type) == ARY+CHAR &&
	    DEUNSIGN(pstk->in_t) == CHAR &&
	    pstk->in_prev != NULL && ISARY(pstk->in_prev->in_t)) {
		char *c;
		int len, alen;

		/* String is an array of char, store it so */
		ilbrace();  /* simulate { */
		inforce(pstk->in_off);
		c = p->n_sp->sname;
		len = p->n_df->ddim;
		alen = pstk->in_prev->in_df->ddim;
		if (alen && alen < len)
			len = alen;
		while (len-- > 0) {
			if (*c == '\\') {
				c++;
				o = esccon(&c);
			} else
				o = *c++;
			infld(o, SZCHAR);
			gotscal();
		}
		/* This check may fail if \0 are in the string */
		if (alen && c[-1])
			werror("too many chars in string");
		irbrace();
		tfree(p);
		return;
	}
	if( ibseen ){
		uerror( "} expected");
		return;
	}
     
	t = pstk->in_t;	 /* type required */
	d = pstk->in_df;
	sue = pstk->in_sue;
	if (pstk->in_sz < 0) {	/* bit field */
		sz = -pstk->in_sz;
	} else {
		sz = tsize( t, d, sue );
	}

	inforce( pstk->in_off );

	/* let buildtree do typechecking */
	p = buildtree( ASSIGN, block(NAME, NIL,NIL, t, d, sue), p );
	nfree(p->n_left);

	/* Make an INIT node of it all */
	p->n_left = optim( p->n_right );
	o = p->n_left->n_op;
	if( o == ADDROF ){
		NODE *l = p->n_left->n_left;
		nfree(p->n_left);
		p->n_left = l;
	}
	p->n_op = INIT;

	/* print it out */
	if( sz < SZINT ){ /* special case: bit fields, etc. */
		if (o != ICON)
			uerror( "illegal initialization" );
		else
			infld( p->n_left->n_lval, sz );
		tfree(p);
	} else if( o == FCON ){
		if (sz == SZFLOAT)
			p->n_left->n_type = FLOAT;
		else if (sz == SZDOUBLE)
			p->n_left->n_type = DOUBLE;
		else if (sz == SZLDOUBLE)
			p->n_left->n_type = LDOUBLE;
		else
			cerror("bad float size");
		ADDENT(TNOD, p->n_left, nod);
		inoff += sz;
		nfree(p);
	} else {
		kinit( optim(p), sz );
	}

	gotscal();
}

#endif

/*
 * final step of initialization.
 * print out init nodes and generate copy code (if needed).
 */
void
endinit(void)
{
	struct ilist *p;
	struct suedef *sue;
	union dimfun *d;
	TWORD t;
	int on, n, lbl;

#ifdef PCC_DEBUG
	if (idebug)
		printf("endinit(), inoff = %lld\n", (long long)inoff);
#endif

	maystr = 0;
	on = 0; /* XXX gcc */
	if (howinit & SIMPLE)
		return;

	while (pstk->in_prev)
		pstk = pstk->in_prev;

	t = pstk->in_t;
	d = pstk->in_df;
	sue = pstk->in_sue;
	n = pstk->in_n;

	if( ISARY(t) ){
		vfdalign(pstk->in_sz);	/* fill out part of the last element, if
 needed */
		n = inoff/pstk->in_sz;	/* real number of initializers */
		if (d->ddim >= n) {
			/* once again, t is an array, so no fields */
			inforce(tsize(t, d, sue));
			n = d->ddim;
		}
		if (d->ddim != 0 && d->ddim != n)
			uerror("too many initializers");
#if 0
		if (n == 0)
			werror("empty array declaration");
#endif
		on = d->ddim;
		d->ddim = n;
	} else if (ISSOU(t)) {
		/* clearly not fields either */
		inforce( tsize( t, d, sue ) );
	} else if (n > 1)
		uerror("bad scalar initialization");
	else
	/* this will never be called with a field element... */
		inforce(tsize(t, d, sue));

	setloc1(howinit & RWINIT ? DATA : RDATA);
	vfdalign( AL_INIT );
	defalign(talign(csym->stype, csym->ssue));
	lbl = 0;
	if (csym->sclass == EXTDEF ||
	    (csym->sclass == STATIC && csym->slevel == 0))
		defnam(csym);
	else if (csym->soffset == NOOFFSET || csym->sclass == AUTO)
		deflab1(lbl = getlab());
	else
		deflab1(lbl = csym->soffset);

	for (p = ilist; p ; p = p->next) {
		/* print it out */
		switch (p->type) {
		case TNOD:
			ninval(p->u.nod);
			tfree(p->u.nod);
			break;
		case TZERO:
			zecode(p->u.val);
			break;
		}
	}
	if (lbl) {
		struct symtab *sp;
		NODE *r, *l;

		/*
		 * generate copy code for initializations on stack.
		 */
		/* Fixa: kopieringslängd char c[12] = "hhhh"; */
		if (ISARY(t) && on == 0) {
			extern  int autooff;
			
			/*
			 * adjust stack in case of open arrays.
			 */
			csym->soffset = NOOFFSET;
			oalloc(csym, &autooff);
		}
		spname = csym;
		if ((ISARY(t) || ISSOU(t)) && csym->sclass == AUTO) {
			sp = isinlining ?
			    permalloc(sizeof *sp) : tmpalloc(sizeof *sp);
			l = buildtree(NAME, NIL, NIL);
			sp->sclass = ILABEL;
			sp->soffset = lbl;
			r = block(ICON, NIL, NIL,
			    INCREF(l->n_type), l->n_df, l->n_sue);
			r->n_sp = sp;
			l = block(STASG, l, r, l->n_type, l->n_df, l->n_sue);
			l = block(UMUL, l, NIL, l->n_type, l->n_df, l->n_sue);
			ecomp(l);
		} else
			csym->soffset = lbl;
	}
	inoff = 0;
}

#ifndef NEWINIT
void
gotscal(void)
{	
	int t, n;
	struct symtab *p;

#ifdef PCC_DEBUG
	if (idebug)
		printf("gotscal()\n");
#endif

	for( ; pstk->in_prev != NULL; ) {
	
		if( pstk->in_fl ) ++ibseen;

		pstk = pstk->in_prev;
 
		t = pstk->in_t;

		if( ISSOU(t)){
			++pstk->in_xp;
			if ((p = *pstk->in_xp) == NULL || t == UNIONTY)
				continue;
		
			instk(p, p->stype, p->sdf, p->ssue,
			    p->soffset + pstk->in_off);
			break;
		} else if( ISARY(t) ){
			n = ++pstk->in_n;
			if (n >= pstk->in_df->ddim && pstk->in_prev != NULL)
				continue;

			instk(pstk->in_sym, DECREF(pstk->in_t), pstk->in_df+1,
			    pstk->in_sue, pstk->in_off + n*pstk->in_sz);
			break;
		} else if (ISFTN(t))
			cerror("gotscal");

	}
printf("gotscal exit\n");
}
#endif

/*
 * process an initializer's left brace
 */
void
ilbrace()
{
	struct instk *w;

#ifdef PCC_DEBUG
	if (idebug)
		printf("ilbrace()\n");
	if (idebug > 2)
		prtstk(pstk);
#endif

	for (w = pstk; w->in_prev != NULL; w = w->in_prev) {

		if (!ISSOU(w->in_t) && !ISARY(w->in_t))
			continue; /* not an aggregate */

		if (w->in_fl) { /* already associated with a { */
			if (w->in_n)
				uerror( "illegal {");
			continue;
		}	

		/* we have one ... */
		w->in_fl = 1;
		break;
	}	
#ifdef PCC_DEBUG
	if (idebug > 1)
		printf("%p) flag %d\n", w, w->in_fl);
#endif
 
	/* cannot find one */
	/* ignore such right braces */
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

	if (ibseen) {
		--ibseen;
		return;
	}

	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {
		if(!pstk->in_fl)
			continue;

		/* we have one now */

		pstk->in_fl = 0;  /* cancel { */
// XXX		gotscal();  /* take it away... */
		return;
	}

	/* these right braces match ignored left braces: throw out */
}

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

#ifndef NEWINIT

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

void
kinit(NODE *p, int sz)
{
	NODE *l;

	/*
	 * as a favor (?) to people who want to write
	 *     int i = 9600/134.5;
	 * we will, under the proper circumstances, do
	 * a coercion here.
	 */
	switch (p->n_type) {
	case INT:
	case UNSIGNED:
		l = p->n_left;
		if (l->n_op != SCONV || l->n_left->n_op != FCON)
			break;
		nfree(l);
		l = l->n_left;
		l->n_lval = (long)(l->n_dcon);
		l->n_sp = NULL;
		l->n_op = ICON;
		l->n_type = INT;
		p->n_left = l;
		break;
	}
	/* arrange for the initialization of p into a space of size sz */
	/* the proper alignment has been opbtained */
	/* inoff is updated to have the proper final value */
	ADDENT(TNOD, p, nod);
	word = inwd = 0;
	inoff += sz;
}
#endif

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
	int lbl;

	/* May be an initialization of an array of char by a string */
	if (DEUNSIGN(p->n_type) == ARY+CHAR && DEUNSIGN(sp->stype) == ARY+CHAR){
		cerror("notyet str[] init");
	}

	switch (sp->sclass) {
	case STATIC:
	case EXTDEF:
		spname = sp;
		p = optim(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
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
