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
 * notice, this list of conditionsand the following disclaimer in the
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

/*
 * Many changes from the 32V sources, among them:
 * - New symbol table manager (moved to another file).
 * - Prototype saving/checks.
 */

# include "pass1.h"

struct symtab *spname;
struct symtab *cftnsp;
static int strunem;		/* currently parsed member type */
int arglistcnt, dimfuncnt;	/* statistics */
int symtabcnt, suedefcnt;	/* statistics */
int maxautooff;		/* the highest (lowest) offset for automatics */
int regvar;		/* the next free register for register variables */
int minrvar;		/* the smallest that regvar gets witing a function */
int autooff,		/* the next unused automatic offset */
    argoff,		/* the next unused argument offset */
    strucoff;		/* the next structure offset position */
int retlab = NOLAB;	/* return label for subroutine */
int brklab;
int contlab;
int flostat;
int instruct, blevel;
OFFSZ inoff;
int reached;

struct params;

#define ISSTR(ty) (ty == STRTY || ty == UNIONTY || ty == ENUMTY)
#define ISSOU(ty) (ty == STRTY || ty == UNIONTY)
#define MKTY(p, t, d, s) r = talloc(); *r = *p; \
	r = argcast(r, t, d, s); *p = *r; nfree(r);

/*
 * Info stored for delaying string printouts.
 */
struct strsched {
	struct strsched *next;
	int locctr;
	struct symtab *sym;
} *strpole;

/*
 * Argument list member info when storing prototypes.
 */
union arglist {
	TWORD type;
	union dimfun *df;
	struct suedef *sue;
};

/*
 * Linked list stack while reading in structs.
 */
struct rstack {
	struct	rstack *rnext;
	int	rinstruct;
	int	rclass;
	int	rstrucoff;
	struct	params *rlparam;
	struct	symtab *rsym;
};

/*
 * Linked list for parameter (and struct elements) declaration.
 */
static struct params {
	struct params *next, *prev;
	struct symtab *sym;
} *lpole, *lparam;
static int nparams;

/*
 * Struct used in array initialisation.
 */
static struct instk {
	struct	instk *in_prev;	/* linked list */
	int	in_sz;   	/* size of array element */
	struct	symtab **in_xp;  /* member in structure initializations */
	int	in_n;  		/* number of initializations seen */
	struct	suedef *in_sue;
	union	dimfun *in_df;	/* dimoff/protos */
	TWORD	in_t;		/* type */
	TWORD	in_q;		/* qualifier */
	struct	symtab *in_sym; /* stab index */
	int	in_fl;	/* flag which says if this level is controlled by {} */
	OFFSZ	in_off;		/* offset of the beginning of this level */
} *pstk;

/* defines used for getting things off of the initialization stack */

static NODE *arrstk[10];
static int arrstkp;
static int intcompare;

void fixtype(NODE *p, int class);
int fixclass(int class, TWORD type);
int falloc(struct symtab *p, int w, int new, NODE *pty);
int oalloc(struct symtab *p, int *poff);
static void dynalloc(struct symtab *p, int *poff);
void inforce(OFFSZ n);
void vfdalign(int n);
static void instk(struct symtab *p, TWORD t, TWORD q, union dimfun *d,
    struct suedef *, OFFSZ off);
void gotscal(void);
static void ssave(struct symtab *);
static void strprint(void);

int ddebug = 0;

void
defid(NODE *q, int class)
{
	struct symtab *p;
	TWORD type, qual;
	TWORD stp, stq;
	int scl;
	union dimfun *dsym, *ddef;
	int slev, temp;

	if (q == NIL)
		return;  /* an error was detected */

	p = q->n_sp;

#ifdef PCC_DEBUG
	if (ddebug) {
		printf("defid(%s (%p), ", p->sname, p);
		tprint(stdout, q->n_type, q->n_qual);
		printf(", %s, (%p,%p)), level %d\n", scnames(class),
		    q->n_df, q->n_sue, blevel);
	}
#endif

	fixtype(q, class);

	type = q->n_type;
	qual = q->n_qual;
	class = fixclass(class, type);

	stp = p->stype;
	stq = p->squal;
	slev = p->slevel;

#ifdef PCC_DEBUG
	if (ddebug) {
		printf("	modified to ");
		tprint(stdout, type, qual);
		printf(", %s\n", scnames(class));
		printf("	previous def'n: ");
		tprint(stdout, stp, stq);
		printf(", %s, (%p,%p)), level %d\n",
		    scnames(p->sclass), p->sdf, p->ssue, slev);
	}
#endif

	if (blevel == 1) {
		switch (class) {
		default:
			if (!(class&FIELD))
				uerror("declared argument %s missing",
				    p->sname );
		case MOS:
		case STNAME:
		case MOU:
		case UNAME:
		case MOE:
		case ENAME:
		case TYPEDEF:
			;
		}
	}

	if (stp == UNDEF)
		goto enter; /* New symbol */

	if (type != stp)
		goto mismatch;

	if (blevel > slev && (class == AUTO || class == REGISTER))
		/* new scope */
		goto mismatch;

	/*
	 * test (and possibly adjust) dimensions.
	 * also check that prototypes are correct.
	 */
	dsym = p->sdf;
	ddef = q->n_df;
	for (temp = type; temp & TMASK; temp = DECREF(temp)) {
		if (ISARY(temp)) {
			if (dsym->ddim == 0) {
				dsym->ddim = ddef->ddim;
			} else if (ddef->ddim != 0 && dsym->ddim!=ddef->ddim) {
				goto mismatch;
			}
			++dsym;
			++ddef;
		} else if (ISFTN(temp)) {
			/* add a late-defined prototype here */
			if (cftnsp == NULL && dsym->dfun == NULL)
				dsym->dfun = ddef->dfun;
			if (!oldstyle && ddef->dfun != NULL &&
			    chkftn(dsym->dfun, ddef->dfun))
				uerror("declaration doesn't match prototype");
			dsym++, ddef++;
		}
	}

	/* check that redeclarations are to the same structure */
	if ((temp == STRTY || temp == UNIONTY || temp == ENUMTY) &&
	    p->ssue != q->n_sue &&
	    class != STNAME && class != UNAME && class != ENAME) {
		goto mismatch;
	}

	scl = p->sclass;

#ifdef PCC_DEBUG
	if (ddebug)
		printf("	previous class: %s\n", scnames(scl));
#endif

	if (class&FIELD) {
		/* redefinition */
		if (!falloc(p, class&FLDSIZ, 1, NIL)) {
			/* successful allocation */
			ssave(p);
			return;
		}
		/* blew it: resume at end of switch... */
	} else switch(class) {

	case EXTERN:
		switch( scl ){
		case STATIC:
		case USTATIC:
			if( slev==0 ) return;
			break;
		case EXTDEF:
		case EXTERN:
		case FORTRAN:
		case UFORTRAN:
			return;
			}
		break;

	case STATIC:
		if (scl==USTATIC || (scl==EXTERN && blevel==0)) {
			p->sclass = STATIC;
			return;
		}
		break;

	case USTATIC:
		if (scl==STATIC || scl==USTATIC)
			return;
		break;

	case TYPEDEF:
		if (scl == class)
			return;
		break;

	case UFORTRAN:
		if (scl == UFORTRAN || scl == FORTRAN)
			return;
		break;

	case FORTRAN:
		if (scl == UFORTRAN) {
			p->sclass = FORTRAN;
			return;
		}
		break;

	case MOU:
	case MOS:
		if (scl == class) {
			if (oalloc(p, &strucoff))
				break;
			if (class == MOU)
				strucoff = 0;
			ssave(p);
			return;
		}
		break;

	case MOE:
		break;

	case EXTDEF:
		switch (scl) {
		case EXTERN:
			p->sclass = EXTDEF;
			return;
		case USTATIC:
			p->sclass = STATIC;
			return;
		}
		break;

	case STNAME:
	case UNAME:
	case ENAME:
		if (scl != class)
			break;
		if (p->ssue->suesize == 0)
			return;  /* previous entry just a mention */
		break;

	case AUTO:
	case REGISTER:
		;  /* mismatch.. */
	}

	mismatch:

	/*
	 * Only allowed for automatic variables.
	 */
	if (blevel == slev || class == EXTERN || class == FORTRAN ||
	    class == UFORTRAN) {
		uerror("redeclaration of %s", p->sname);
		return;
	}
	q->n_sp = p = hide(p);

	enter:  /* make a new entry */

#ifdef PCC_DEBUG
	if(ddebug)
		printf("	new entry made\n");
#endif
	p->stype = type;
	p->squal = qual;
	p->sclass = class;
	p->slevel = blevel;
	p->soffset = NOOFFSET;
	p->suse = lineno;
	if (class == STNAME || class == UNAME || class == ENAME) {
		p->ssue = permalloc(sizeof(struct suedef));
		suedefcnt++;
		p->ssue->suesize = 0;
		p->ssue->suelem = NULL; 
		p->ssue->suealign = ALSTRUCT;
	} else {
		switch (BTYPE(type)) {
		case STRTY:
		case UNIONTY:
		case ENUMTY:
			p->ssue = q->n_sue;
			break;
		default:
			p->ssue = MKSUE(BTYPE(type));
		}
	}

	/* copy dimensions */
	p->sdf = q->n_df;
	/* Do not save param info for old-style functions */
	if (ISFTN(type) && oldstyle)
		p->sdf->dfun = NULL;

	/* allocate offsets */
	if (class&FIELD) {
		(void) falloc(p, class&FLDSIZ, 0, NIL);  /* new entry */
		ssave(p);
	} else switch (class) {

	case AUTO:
		if (arrstkp)
			dynalloc(p, &autooff);
		else
			oalloc(p, &autooff);
		if (autooff > maxautooff)
			maxautooff = autooff;
		break;
	case STATIC:
	case EXTDEF:
		p->soffset = getlab();
		if (class == STATIC && blevel > 0)
			p->sflags |= SLABEL;
#ifdef GCC_COMPAT
		{	extern char *renname;
			if (renname)
				gcc_rename(p, renname);
			renname = NULL;
		}
#endif
		break;

	case EXTERN:
	case UFORTRAN:
	case FORTRAN:
		p->soffset = getlab();
		p->slevel = 0;
#ifdef GCC_COMPAT
		{	extern char *renname;
			if (renname)
				gcc_rename(p, renname);
			renname = NULL;
		}
#endif
		break;
	case MOU:
	case MOS:
		oalloc(p, &strucoff);
		if (class == MOU)
			strucoff = 0;
		ssave(p);
		break;

	case MOE:
		p->soffset = strucoff++;
		ssave(p);
		break;
	case REGISTER:
		p->soffset = regvar--;
		if (blevel == 1)
			p->sflags |= SSET;
		if (regvar < minrvar)
			minrvar = regvar;
		break;
	}

	/* user-supplied routine to fix up new definitions */
	FIXDEF(p);

#ifdef PCC_DEBUG
	if (ddebug)
		printf( "	sdf, ssue, offset: %p, %p, %d\n",
		    p->sdf, p->ssue, p->soffset);
#endif

}

void
ssave(struct symtab *sym)
{
	struct params *p;

	p = tmpalloc(sizeof(struct params));
	p->next = NULL;
	p->sym = sym;

	if (lparam == NULL) {
		p->prev = (struct params *)&lpole;
		lpole = p;
	} else {
		lparam->next = p;
		p->prev = lparam;
	}
	lparam = p;
}

/*
 * end of function
 */
void
ftnend()
{
	extern struct savbc *savbc;
	extern struct swdef *swpole;

	if (retlab != NOLAB && nerrors == 0) { /* inside a real function */
		send_passt(IP_DEFLAB, retlab);
		efcode(); /* struct return handled here */
		branch(retlab = getlab());
		send_passt(IP_EPILOG, minrvar, maxautooff, retlab);
	}

	tcheck();
	brklab = contlab = retlab = NOLAB;
	flostat = 0;
	if (nerrors == 0) {
		if (savbc != NULL)
			cerror("bcsave error");
		if (lparam != NULL)
			cerror("parameter reset error");
		if (swpole != NULL)
			cerror("switch error");
	}
	savbc = NULL;
	lparam = NULL;
	maxautooff = autooff = AUTOINIT;
	minrvar = regvar = MAXRVAR;
	reached = 1;

	if (isinlining)
		inline_end();
	inline_prtout();

	strprint();

	tmpfree(); /* Release memory resources */
	send_passt(IP_LOCCTR, DATA);
}

#define TNULL   INCREF(MOETY)   /* pointer to MOETY -- impossible type */

void
dclargs()
{
	union dimfun *df;
	union arglist *al, *al2, *alb;
	struct params *a;
	struct symtab *p, **parr;
	int i;

	argoff = ARGINIT;

	/*
	 * Deal with fun(void) properly.
	 */
	if (nparams == 1 && lparam->sym->stype == VOID)
		goto done;

	/*
	 * Generate a list for bfcode().
	 * Parameters were pushed in reverse order.
	 */
	if (nparams != 0)
		parr = tmpalloc(sizeof(struct symtab *) * nparams);

	for (a = lparam, i = 0; a != NULL && a != (struct params *)&lpole;
	    a = a->prev) {

		p = a->sym;
		parr[i++] = p;
		if (p->stype == FARG) {
			p->stype = INT;
			p->ssue = MKSUE(INT);
		}
		if (ISARY(p->stype)) {
			p->stype += (PTR-ARY);
			p->sdf++;
		} else if (ISFTN(p->stype)) {
			werror("function declared as argument");
			p->stype = INCREF(p->stype);
		}
	  	/* always set aside space, even for register arguments */
		oalloc(p, &argoff);
	}
	if (oldstyle && (df = cftnsp->sdf) && (al = df->dfun)) {
		/*
		 * Check against prototype of oldstyle function.
		 */
		alb = al2 = tmpalloc(sizeof(union arglist) * nparams * 3 + 1);
		for (i = 0; i < nparams; i++) {
			TWORD type = parr[i]->stype;
			(al2++)->type = type;
			if (ISSTR(BTYPE(type)))
				(al2++)->sue = parr[i]->ssue;
			while (!ISFTN(type) && !ISARY(type) && type > BTMASK)
				type = DECREF(type);
			if (type > BTMASK)
				(al2++)->df = parr[i]->sdf;
		}
		al2->type = TNULL;
		intcompare = 1;
		if (chkftn(al, alb))
			uerror("function doesn't match prototype");
		intcompare = 0;
	}
done:	cendarg();
	send_passt(IP_LOCCTR, PROG);
	defalign(ALINT);
	ftnno = getlab();
	retlab = getlab();
	bfcode(parr, nparams);
	send_passt(IP_PROLOG, -1, -1, DECREF(cftnsp->stype));
	lparam = NULL;
	nparams = 0;
}

/*
 * reference to a structure or union, with no definition
 */
NODE *
rstruct(char *tag, int soru)
{
	struct symtab *p;
	NODE *q;

	p = (struct symtab *)lookup(tag, STAGNAME);
	switch (p->stype) {

	case UNDEF:
	def:
		q = block(NAME, NIL, NIL, 0, 0, 0);
		q->n_sp = p;
		q->n_type = (soru&INSTRUCT) ? STRTY :
		    ((soru&INUNION) ? UNIONTY : ENUMTY);
		defid(q, (soru&INSTRUCT) ? STNAME :
		    ((soru&INUNION) ? UNAME : ENAME));
		nfree(q);
		break;

	case STRTY:
		if (soru & INSTRUCT)
			break;
		goto def;

	case UNIONTY:
		if (soru & INUNION)
			break;
		goto def;

	case ENUMTY:
		if (!(soru&(INUNION|INSTRUCT)))
			break;
		goto def;

	}
	q = mkty(p->stype, 0, p->ssue);
	q->n_sue = p->ssue;
	return q;
}

void
moedef(char *name)
{
	NODE *q;

	q = block(NAME, NIL, NIL, MOETY, 0, 0);
	q->n_sp = lookup(name, 0);
	defid(q, MOE);
	nfree(q);
}

/*
 * begining of structure or union declaration
 */
struct rstack *
bstruct(char *name, int soru)
{
	struct rstack *r;
	struct symtab *s;
	NODE *q;

	if (name != NULL)
		s = lookup(name, STAGNAME);
	else
		s = NULL;

	r = tmpalloc(sizeof(struct rstack));
	r->rinstruct = instruct;
	r->rclass = strunem;
	r->rstrucoff = strucoff;

	strucoff = 0;
	instruct = soru;
	q = block(NAME, NIL, NIL, 0, 0, 0);
	q->n_sp = s;
	if (instruct==INSTRUCT) {
		strunem = MOS;
		q->n_type = STRTY;
		if (s != NULL)
			defid(q, STNAME);
	} else if(instruct == INUNION) {
		strunem = MOU;
		q->n_type = UNIONTY;
		if (s != NULL)
			defid(q, UNAME);
	} else { /* enum */
		strunem = MOE;
		q->n_type = ENUMTY;
		if (s != NULL)
			defid(q, ENAME);
	}
	r->rsym = q->n_sp;
	r->rlparam = lparam;
	nfree(q);

	return r;
}

/*
 * Called after a struct is declared to restore the environment.
 */
NODE *
dclstruct(struct rstack *r)
{
	NODE *n;
	struct params *l, *m;
	struct suedef *sue;
	struct symtab *p;
	int al, sa, sz;
	TWORD temp;
	int i, high, low;

	if (r->rsym == NULL) {
		sue = permalloc(sizeof(struct suedef));
		suedefcnt++;
		sue->suesize = 0;
		sue->suealign = ALSTRUCT;
	} else
		sue = r->rsym->ssue;

#ifdef PCC_DEBUG
	if (ddebug)
		printf("dclstruct(%s)\n", r->rsym ? r->rsym->sname : "??");
#endif
	temp = (instruct&INSTRUCT)?STRTY:((instruct&INUNION)?UNIONTY:ENUMTY);
	instruct = r->rinstruct;
	strunem = r->rclass;
	al = ALSTRUCT;

	high = low = 0;

	if ((l = r->rlparam) == NULL)
		l = lpole;
	else
		l = l->next;

	/* memory for the element array must be allocated first */
	for (m = l, i = 1; m != NULL; m = m->next)
		i++;
	sue->suelem = permalloc(sizeof(struct symtab *) * i);

	for (i = 0; l != NULL; l = l->next) {
		sue->suelem[i++] = p = l->sym;

		if (p == NULL)
			cerror("gummy structure member");
		if (temp == ENUMTY) {
			if (p->soffset < low)
				low = p->soffset;
			if (p->soffset > high)
				high = p->soffset;
			p->ssue = sue;
			continue;
		}
		sa = talign(p->stype, p->ssue);
		if (p->sclass & FIELD) {
			sz = p->sclass&FLDSIZ;
		} else {
			sz = tsize(p->stype, p->sdf, p->ssue);
		}
		if (sz > strucoff)
			strucoff = sz;  /* for use with unions */
		/*
		 * set al, the alignment, to the lcm of the alignments
		 * of the members.
		 */
		SETOFF(al, sa);
	}
	sue->suelem[i] = NULL;
	SETOFF(strucoff, al);

	if (temp == ENUMTY) {
		TWORD ty;

#ifdef ENUMSIZE
		ty = ENUMSIZE(high,low);
#else
		if ((char)high == high && (char)low == low)
			ty = ctype(CHAR);
		else if ((short)high == high && (short)low == low)
			ty = ctype(SHORT);
		else
			ty = ctype(INT);
#endif
		strucoff = tsize(ty, 0, MKSUE(ty));
		sue->suealign = al = talign(ty, MKSUE(ty));
	}

	sue->suesize = strucoff;
	sue->suealign = al;

#ifdef PCC_DEBUG
	if (ddebug>1) {
		int i;

		printf("\tsize %d align %d elem %p\n",
		    sue->suesize, sue->suealign, sue->suelem);
		for (i = 0; sue->suelem[i] != NULL; ++i) {
			printf("\tmember %s(%p)\n",
			    sue->suelem[i]->sname, sue->suelem[i]);
		}
	}
#endif

	strucoff = r->rstrucoff;
	if ((lparam = r->rlparam) != NULL)
		lparam->next = NULL;
	n = mkty(temp, 0, sue);
	return n;
}

/*
 * error printing routine in parser
 */
void yyerror(char *s);
void
yyerror(char *s)
{
	uerror(s);
}

void yyaccpt(void);
void
yyaccpt(void)
{
	ftnend();
}

/*
 * p is top of type list given to tymerge later.
 * Find correct CALL node and declare parameters from there.
 */
void
ftnarg(NODE *p)
{
	NODE *q;
	struct symtab *s;

#ifdef PCC_DEBUG
	if (ddebug > 2)
		printf("ftnarg(%p)\n", p);
#endif
	/*
	 * Enter argument onto param stack.
	 * Do not declare parameters until later (in dclargs);
	 * the function must be declared first.
	 * put it on the param stack in reverse order, due to the
	 * nature of the stack it will be reclaimed correct.
	 */
	for (; p->n_op != NAME; p = p->n_left) {
		if (p->n_op == (UCALL) && p->n_left->n_op == NAME)
			return;	/* Nothing to enter */
		if (p->n_op == CALL && p->n_left->n_op == NAME)
			break;
	}

	p = p->n_right;
	blevel = 1;

	while (p->n_op == CM) {
		q = p->n_right;
		if (q->n_op != ELLIPSIS) {
			s = lookup((char *)q->n_sp, 0);
			if (s->stype != UNDEF) {
				if (s->slevel > 0)
					uerror("parameter '%s' redefined",
					    s->sname);
				s = hide(s);
			}
			s->soffset = NOOFFSET;
			s->sclass = PARAM;
			s->stype = q->n_type;
			s->sdf = q->n_df;
			s->ssue = q->n_sue;
			ssave(s);
			nparams++;
#ifdef PCC_DEBUG
			if (ddebug > 2)
				printf("	saving sym %s (%p) from (%p)\n",
				    s->sname, s, q);
#endif
		}
		p = p->n_left;
	}
	s = lookup((char *)p->n_sp, 0);
	if (s->stype != UNDEF) {
		if (s->slevel > 0)
			uerror("parameter '%s' redefined", s->sname);
		s = hide(s);
	}
	s->soffset = NOOFFSET;
	s->sclass = PARAM;
	s->stype = p->n_type;
	s->sdf = p->n_df;
	s->ssue = p->n_sue;
	ssave(s);
	nparams++;
	blevel = 0;

#ifdef PCC_DEBUG
	if (ddebug > 2)
		printf("	saving sym %s (%p) from (%p)\n",
		    s->sname, s, p);
#endif
}

/*
 * compute the alignment of an object with type ty, sizeoff index s
 */
int
talign(unsigned int ty, struct suedef *sue)
{
	int i;

	if (ISPTR(ty))
		return(ALPOINT); /* shortcut */

	if(sue == NULL && ty!=INT && ty!=CHAR && ty!=SHORT &&
	    ty!=UNSIGNED && ty!=UCHAR && ty!=USHORT) {
		return(fldal(ty));
	}

	for( i=0; i<=(SZINT-BTSHIFT-1); i+=TSHIFT ){
		switch( (ty>>i)&TMASK ){

		case FTN:
			cerror("compiler takes alignment of function");
		case PTR:
			return(ALPOINT);
		case ARY:
			continue;
		case 0:
			break;
			}
		}

	switch( BTYPE(ty) ){

	case UNIONTY:
	case ENUMTY:
	case STRTY:
		return((unsigned int)sue->suealign);
	case CHAR:
	case UCHAR:
		return (ALCHAR);
	case FLOAT:
		return (ALFLOAT);
	case LDOUBLE:
		return (ALLDOUBLE);
	case DOUBLE:
		return (ALDOUBLE);
	case LONGLONG:
	case ULONGLONG:
		return (ALLONGLONG);
	case LONG:
	case ULONG:
		return (ALLONG);
	case SHORT:
	case USHORT:
		return (ALSHORT);
	default:
		return (ALINT);
	}
}

/* compute the size associated with type ty,
 *  dimoff d, and sizoff s */
/* BETTER NOT BE CALLED WHEN t, d, and s REFER TO A BIT FIELD... */
OFFSZ
tsize(TWORD ty, union dimfun *d, struct suedef *sue)
{

	int i;
	OFFSZ mult;

	mult = 1;

	for( i=0; i<=(SZINT-BTSHIFT-1); i+=TSHIFT ){
		switch( (ty>>i)&TMASK ){

		case FTN:
			cerror( "compiler takes size of function");
		case PTR:
			return( SZPOINT * mult );
		case ARY:
			mult *= d->ddim;
			d++;
			continue;
		case 0:
			break;

			}
		}

	if (sue == NULL)
		cerror("bad tsize sue");
	if (ty != STRTY && ty != UNIONTY) {
		if (sue->suesize == 0) {
			uerror("unknown size");
			return(SZINT);
		}
	} else {
		if (sue->suelem == NULL)
			uerror("unknown structure/union/enum");
	}

	return((unsigned int)sue->suesize * mult);
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
	if (rest > 0)
		zecode(rest);

	/* now, the remainder of the last word */

	rest = n-inoff;
	if (rest > 0)
		vfdzero( rest );
	if( inoff != n ) cerror( "inoff error");

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


int idebug = 0;

int ibseen = 0;  /* the number of } constructions which have been filled */

int iclass;  /* storage class of thing being initialized */

int ilocctr = 0;  /* location counter for current initialization */

/*
 * beginning of initilization; set location ctr and set type
 */
void
beginit(struct symtab *p, int class)
{
#ifdef PCC_DEBUG
	if (idebug >= 3)
		printf("beginit(), symtab = %p\n", p);
#endif

	iclass = p->sclass;
	if (class == EXTERN || class == FORTRAN)
		iclass = EXTERN;
	switch (iclass) {

	case UNAME:
	case EXTERN:
		return;
	case AUTO:
	case REGISTER:
		break;
	case EXTDEF:
	case STATIC:
		ilocctr = ISARY(p->stype)?ADATA:DATA;
		send_passt(IP_LOCCTR, ilocctr);
		defalign(talign(p->stype, p->ssue));
		defnam(p);
	}

	inoff = 0;
	ibseen = 0;

	pstk = 0;

	instk(p, p->stype, p->squal, p->sdf, p->ssue, inoff);

}

/*
 * make a new entry on the parameter stack to initialize p
 */
void
instk(struct symtab *p, TWORD t, TWORD q,
    union dimfun *d, struct suedef *sue, OFFSZ off)
{
	struct instk *sp;

	for (;;) {
#ifdef PCC_DEBUG
		if (idebug)
			printf("instk((%p, %x, %x, %p,%p, %lld)\n",
			    p, t, q, d, sue, (long long)off);
#endif

		/* save information on the stack */
		sp = tmpalloc(sizeof(struct instk));
		sp->in_prev = pstk;
		pstk = sp;

		pstk->in_fl = 0;	/* { flag */
		pstk->in_sym = p;
		pstk->in_t = t;
		pstk->in_q = q;
		pstk->in_df = d;
		pstk->in_sue = sue;
		pstk->in_n = 0;  /* number seen */
		pstk->in_xp = (t == STRTY || t == UNIONTY) ? sue->suelem : NULL;
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

		if (iclass==AUTO || iclass == REGISTER) {
			if (t == STRTY || t == UNIONTY)
				return; /* dealt with in doinit() */
			if (ISARY(t))
				uerror("no automatic aggregate initialization");
		}

		/* now, if this is not a scalar, put on another element */

		if (ISARY(t)) {
			t = DECREF(t);
			q = DECREF(q);
			++d;
			continue;
		} else if (t == STRTY || t == UNIONTY) {
			if (pstk->in_sue == 0) {
				uerror("can't initialize undefined %s",
				    t == STRTY ? "structure" : "union");
				iclass = -1;
				return;
			}
			p = *pstk->in_xp;
			if (((p->sclass != MOS && t == STRTY) ||
			    (p->sclass != MOU && t == UNIONTY)) &&
			    !(p->sclass&FIELD))
				cerror("insane %s member list",
				    t == STRTY ? "structure" : "union");
			t = p->stype;
			q = p->squal;
			d = p->sdf;
			sue = p->ssue;
			off += p->soffset;
			continue;
		} else
			return;
	}
}

/*
 * Write last part of string.
 */
NODE *
strend(struct stri *si)
{
	struct symtab *s;
	int lxarg, i, val;
	char *wr = si->str;
	NODE *p;

	i = 0;
	if ((iclass == EXTDEF || iclass==STATIC) &&
	    (pstk->in_t == CHAR || pstk->in_t == UCHAR) &&
	    pstk->in_prev != NULL && ISARY(pstk->in_prev->in_t)) {
		/* treat "abc" as { 'a', 'b', 'c', 0 } */
		ilbrace();  /* simulate { */
		inforce(pstk->in_off);
		/*
		 * if the array is inflexible (not top level),
		 * pass in the size and be prepared to throw away
		 * unwanted initializers
		 */

		lxarg = pstk->in_prev->in_prev != NULL ?
		    pstk->in_prev->in_df->ddim : 0;
		while (*wr != 0) {
			if (*wr++ == '\\')
				val = esccon(&wr);
			else
				val = wr[-1];
			if (lxarg == 0 || i < lxarg)
				putbyte(val);
			else if (i == lxarg)
				werror("non-null byte ignored in string"
				    "initializer");
			i++;
		}

		if (lxarg == 0 || i < lxarg)
			putbyte(0);
		irbrace();  /* simulate } */
		return(NIL);
	}
	/* make a label, and get the contents and stash them away */
	if (iclass != SNULL) { /* initializing */
		/* fill out previous word, to permit pointer */
		vfdalign(ALPOINT);
	}

	/* If an identical string is already emitted, just forget this one */
	si->str = addstring(si->str);	/* enter string in string table */
	s = lookup(si->str, SSTRING);	/* check for existance */

	if (s->soffset == 0) { /* No string */
		struct strsched *sc;
		s->sclass = ILABEL;

		/*
		 * Delay printout of this string until after the current
		 * function, or the end of the statement.
		 */
		sc = tmpalloc(sizeof(struct strsched));
		sc->locctr = blevel==0 ? ISTRNG : STRNG;
		sc->sym = s;
		sc->next = strpole;
		strpole = sc;
		s->soffset = getlab();
	}

	p = buildtree(STRING, NIL, NIL);
	p->n_df = tmpalloc(sizeof(union dimfun));
	p->n_df->ddim = si->len+1;
	p->n_sp = s;
	return(p);
}

/*
 * Print out new strings, before temp memory is cleared.
 */
void
strprint()
{
	char *wr;
	int i, val;

	while (strpole != NULL) {
		send_passt(IP_LOCCTR, strpole->locctr);
		send_passt(IP_DEFLAB, strpole->sym->soffset);

		i = 0;
		wr = strpole->sym->sname;
		while (*wr != 0) {
			if (*wr++ == '\\')
				val = esccon(&wr);
			else
				val = wr[-1];
			bycode(val, i);
			i++;
		}
		bycode(0, i++);
		bycode(-1, i);
		strpole = strpole->next;
	}
}

/*
 * simulate byte v appearing in a list of integer values
 */
void
putbyte(int v)
{
	NODE *p;
	p = bcon(v);
	incode( p, SZCHAR );
	tfree( p );
	gotscal();
}

void
endinit(void)
{
	struct suedef *sue;
	TWORD t;
	union dimfun *d, *d1;
	int n;

#ifdef PCC_DEBUG
	if (idebug)
		printf("endinit(), inoff = %lld\n", (long long)inoff);
#endif

	switch( iclass ){

	case EXTERN:
	case AUTO:
	case REGISTER:
		return;
		}

	while (pstk->in_prev)
		pstk = pstk->in_prev;

	t = pstk->in_t;
	d = pstk->in_df;
	sue = pstk->in_sue;
	n = pstk->in_n;

	if( ISARY(t) ){
		d1 = d;

		vfdalign(pstk->in_sz);  /* fill out part of the last element, if needed */
		n = inoff/pstk->in_sz;  /* real number of initializers */
		if (d1->ddim >= n) {
			/* once again, t is an array, so no fields */
			inforce(tsize(t, d, sue));
			n = d1->ddim;
		}
		if (d1->ddim != 0 && d1->ddim != n)
			uerror("too many initializers");
		if (n == 0)
			werror("empty array declaration");
		d->ddim = n;
	}

	else if (t == STRTY || t == UNIONTY) {
		/* clearly not fields either */
		inforce( tsize( t, d, sue ) );
	} else if (n > 1)
		uerror("bad scalar initialization");
	else
	/* this will never be called with a field element... */
		inforce(tsize(t, d, sue));

	lparam = NULL;
	vfdalign( AL_INIT );
	inoff = 0;
	iclass = SNULL;

}

/*
 * take care of generating a value for the initializer p
 * inoff has the current offset (last bit written)
 * in the current word being generated
 */
void
doinit(NODE *p)
{
	union dimfun *d;
	NODE *u;
	struct suedef *sue;
	int sz;
	TWORD t;
	int o;

	/* note: size of an individual initializer is assumed to fit into an int */

	if( iclass < 0 ) return;
	if( iclass == EXTERN || iclass == UNAME ){
		uerror( "cannot initialize extern or union" );
		iclass = -1;
		return;
		}

	if( iclass == AUTO || iclass == REGISTER ){
		/* do the initialization and get out, without regard 
		    for filing out the variable with zeros, etc. */
		bccode();
		send_passt(IP_NEWBLK, regvar, autooff);
		spname = pstk->in_sym;
		p = buildtree( ASSIGN, buildtree( NAME, NIL, NIL ), p );
		ecomp(p);
		return;
		}

	if( p == NIL ) return;  /* for throwing away strings that have been turned into lists */

	if( ibseen ){
		uerror( "} expected");
		return;
		}

#ifdef PCC_DEBUG
	if (idebug > 1)
		printf("doinit(%p)\n", p);
#endif

	t = pstk->in_t;  /* type required */
	d = pstk->in_df;
	sue = pstk->in_sue;
	if (pstk->in_sz < 0) {  /* bit field */
		sz = -pstk->in_sz;
	} else {
		sz = tsize( t, d, sue );
	}

	inforce( pstk->in_off );

	u = block(NAME, NIL,NIL, t, d, sue);
	u->n_qual = pstk->in_q;
	p = buildtree( ASSIGN, u, p );
	nfree(p->n_left);
	p->n_left = p->n_right;
	p->n_right = NIL;
	p->n_left = optim( p->n_left );
	o = p->n_left->n_op;
	if( o == ADDROF ){
		NODE *l = p->n_left->n_left;
		nfree(p->n_left);
		p->n_left = l;
	}
	p->n_op = INIT;

	if( sz < SZINT ){ /* special case: bit fields, etc. */
		if (o != ICON)
			uerror( "illegal initialization" );
		else {
			incode( p->n_left, sz );
			nfree(p);
		}
	} else if( o == FCON ){
		fincode(p->n_left, sz );
		nfree(p);
	} else {
		cinit( optim(p), sz );
	}

	gotscal();
}

void
gotscal(void)
{
	int t, n;
	struct symtab *p;
	OFFSZ temp;

	for( ; pstk->in_prev != NULL; ) {

		if( pstk->in_fl ) ++ibseen;

		pstk = pstk->in_prev;
		
		t = pstk->in_t;

		if( t == STRTY || t == UNIONTY){
			++pstk->in_xp;
			if ((p = *pstk->in_xp) == NULL)
				continue;

			/* otherwise, put next element on the stack */
			instk(p, p->stype, p->squal, p->sdf, p->ssue,
			    p->soffset + pstk->in_off);
			return;
		} else if( ISARY(t) ){
			n = ++pstk->in_n;
			if (n >= pstk->in_df->ddim && pstk->in_prev != NULL)
				continue;

			/* put the new element onto the stack */

			temp = pstk->in_sz;
			instk(pstk->in_sym, DECREF(pstk->in_t), DECREF(pstk->in_q),
			    pstk->in_df+1, pstk->in_sue, pstk->in_off+n*temp);
			return;
		} else if (ISFTN(t))
			cerror("gotscal");

	}
}

/*
 * process an initializer's left brace
 */
void
ilbrace()
{
	int t;
	struct instk *temp;

	temp = pstk;

	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {

		t = pstk->in_t;
		if (t != UNIONTY && t != STRTY && !ISARY(t))
			continue; /* not an aggregate */
		if (pstk->in_fl) { /* already associated with a { */
			if (pstk->in_n)
				uerror( "illegal {");
			continue;
		}

		/* we have one ... */
		pstk->in_fl = 1;
		break;
	}

	/* cannot find one */
	/* ignore such right braces */

	pstk = temp;
}

/*
 * called when a '}' is seen
 */
void
irbrace()
{
#ifdef PCC_DEBUG
	if (idebug)
		printf( "irbrace(): lparam = %p on entry\n", lparam);
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
		gotscal();  /* take it away... */
		return;
	}

	/* these right braces match ignored left braces: throw out */
}

/*
 * update the offset pointed to by poff; return the
 * offset of a value of size `size', alignment `alignment',
 * given that off is increasing
 */
int
upoff(int size, int alignment, int *poff)
{
	int off;

	off = *poff;
	SETOFF(off, alignment);
	if (off < 0)
		cerror("structure or stack overgrown"); /* wrapped */
	*poff = off+size;
	return (off);
}

/*
 * allocate p with offset *poff, and update *poff
 */
int
oalloc(struct symtab *p, int *poff )
{
	int al, off, tsz;
	int noff;

#if 0
	if ((p->sclass == AUTO) || (Oflag && (p->sclass == REGISTER))) {
		if (!ISARY(p->stype)) {
			NODE *tn = tempnode(0, p->stype, p->sdf, p->ssue);
			p->soffset = tn->n_lval;
			p->sflags |= STNODE;
			nfree(tn);
			return 0;
		}
	}
#endif

	al = talign(p->stype, p->ssue);
	noff = off = *poff;
	tsz = tsize(p->stype, p->sdf, p->ssue);
#ifdef BACKAUTO
	if (p->sclass == AUTO) {
		noff = off + tsz;
		if (noff < 0)
			cerror("stack overflow");
		SETOFF(noff, al);
		off = -noff;
	} else
#endif
	if (p->sclass == PARAM && (p->stype == CHAR || p->stype == UCHAR ||
	    p->stype == SHORT || p->stype == USHORT)) {
		off = upoff(SZINT, ALINT, &noff);
#ifndef RTOLBYTES
		off = noff - tsz;
#endif
	} else {
		off = upoff(tsz, al, &noff);
	}

	if (p->sclass != REGISTER) {
	/* in case we are allocating stack space for register arguments */
		if (p->soffset == NOOFFSET)
			p->soffset = off;
		else if(off != p->soffset)
			return(1);
	}

	*poff = noff;
	return(0);
}

/*
 * Allocate space on the stack for dynamic arrays.
 * Strategy is as follows:
 * - first entry is a pointer to the dynamic datatype.
 * - if it's a one-dimensional array this will be the only entry used.
 * - if it's a multi-dimensional array the following (numdim-1) integers
 *   will contain the sizes to multiply the indexes with.
 * - code to write the dimension sizes this will be generated here.
 * - code to allocate space on the stack will be generated here.
 */
static void
dynalloc(struct symtab *p, int *poff)
{
	union dimfun *df;
	NODE *n, *nn, *tn, *pol;
	TWORD t;
	int i, no;

	bccode(); /* Init code generation */
	send_passt(IP_NEWBLK, regvar, autooff);

	/*
	 * The pointer to the array is stored in a TEMP node, which number
	 * is in the soffset field;
	 */
	t = p->stype;
	p->sflags |= (STNODE|SDYNARRAY);
	p->stype = INCREF(p->stype);	/* Make this an indirect pointer */
	p->soffset = 0;
	tn = tempnode(&p->soffset, p->stype, p->sdf, p->ssue);

	df = p->sdf;

	pol = NIL;
	for (i = 0; ISARY(t); t = DECREF(t), df++) {
		if (df->ddim >= 0)
			continue;
		n = arrstk[i++];
		no = 0;
		nn = tempnode(&no, INT, 0, MKSUE(INT));
		ecomp(buildtree(ASSIGN, nn, n)); /* Save size */

		df->ddim = -no;
		n = tempnode(&no, INT, 0, MKSUE(INT));
		if (pol == NIL)
			pol = n;
		else
			pol = buildtree(MUL, pol, n);
	}
	/* Create stack gap */
	spalloc(tn, pol, tsize(t, 0, p->ssue));
	arrstkp = 0;
}

/*
 * allocate a field of width w
 * new is 0 if new entry, 1 if redefinition, -1 if alignment
 */
int
falloc(struct symtab *p, int w, int new, NODE *pty)
{
	int al,sz,type;

	type = (new<0)? pty->n_type : p->stype;

	/* this must be fixed to use the current type in alignments */
	switch( new<0?pty->n_type:p->stype ){

	case ENUMTY: {
		struct suedef *sue;
		sue = new < 0 ? pty->n_sue : p->ssue;
		al = sue->suealign;
		sz = sue->suesize;
		break;
	}

	case CHAR:
	case UCHAR:
		al = ALCHAR;
		sz = SZCHAR;
		break;

	case SHORT:
	case USHORT:
		al = ALSHORT;
		sz = SZSHORT;
		break;

	case INT:
	case UNSIGNED:
		al = ALINT;
		sz = SZINT;
		break;

	default:
		if( new < 0 ) {
			uerror( "illegal field type" );
			al = ALINT;
			}
		else {
			al = fldal( p->stype );
			sz =SZINT;
			}
		}

	if( w > sz ) {
		uerror( "field too big");
		w = sz;
		}

	if( w == 0 ){ /* align only */
		SETOFF( strucoff, al );
		if( new >= 0 ) uerror( "zero size field");
		return(0);
		}

	if( strucoff%al + w > sz ) SETOFF( strucoff, al );
	if( new < 0 ) {
		strucoff += w;  /* we know it will fit */
		return(0);
		}

	/* establish the field */

	if( new == 1 ) { /* previous definition */
		if( p->soffset != strucoff || p->sclass != (FIELD|w) ) return(1);
		}
	p->soffset = strucoff;
	strucoff += w;
	p->stype = type;
	fldty( p );
	return(0);
}

/*
 * handle unitialized declarations
 * assumed to be not functions
 */
void
nidcl(NODE *p, int class)
{
	int commflag;

	/* compute class */
	if (class == SNULL) {
		if (blevel > 1)
			class = AUTO;
		else if (blevel != 0 || instruct)
			cerror( "nidcl error" );
		else /* blevel = 0 */
			commflag = (class = noinit()) == EXTERN;
	}

	defid(p, class);

	switch (class) {
	case EXTDEF:
		/* simulate initialization by 0 */
		beginit(p->n_sp, class);
		endinit();
		break;
	case EXTERN:
		if (commflag)
			commdec(p->n_sp);
		break;
	case STATIC:
		lcommdec(p->n_sp);
		break;
	}
}

/*
 * Merges a type tree into one type. Returns one type node with merged types
 * and class stored in the su field. Frees all other nodes.
 * XXX - classes in typedefs?
 */
NODE *
typenode(NODE *p)
{
	NODE *l;
	int class = 0, adj, noun, sign;
	TWORD qual = 0;

	adj = INT;	/* INT, LONG or SHORT */
	noun = UNDEF;	/* INT, CHAR or FLOAT */
	sign = 0;	/* 0, SIGNED or UNSIGNED */

	/* Remove initial QUALIFIERs */
	if (p && p->n_op == QUALIFIER) {
		qual = p->n_type;
		l = p->n_left;
		nfree(p);
		p = l;
	}

	/* Handle initial classes special */
	if (p && p->n_op == CLASS) {
		class = p->n_type;
		l = p->n_left;
		nfree(p);
		p = l;
	}

	/* Remove more QUALIFIERs */
	if (p && p->n_op == QUALIFIER) {
		qual |= p->n_type;
		l = p->n_left;
		nfree(p);
		p = l;
	}

	if (p && p->n_op == TYPE && p->n_left == NIL) {
#ifdef CHAR_UNSIGNED
		if (p->n_type == CHAR)
			p->n_type = UCHAR;
#endif
		p->n_lval = class;
		p->n_qual = qual >> TSHIFT;
		return p;
	}

	while (p != NIL) { 
		if (p->n_op == QUALIFIER) {
			qual |= p->n_type;
			goto next;
		}
		if (p->n_op == CLASS) {
			if (class != 0)
				uerror("too many storage classes");
			class = p->n_type;
			goto next;
		}
		if (p->n_op != TYPE)
			cerror("typenode got notype %d", p->n_op);
		switch (p->n_type) {
		case SIGNED:
		case UNSIGNED:
			if (sign != 0)
				goto bad;
			sign = p->n_type;
			break;
		case LONG:
			if (adj == LONG) {
				adj = LONGLONG;
				break;
			}
			/* FALLTHROUGH */
		case SHORT:
			if (adj != INT)
				goto bad;
			adj = p->n_type;
			break;
		case INT:
		case CHAR:
		case FLOAT:
		case DOUBLE:
			if (noun != UNDEF)
				goto bad;
			noun = p->n_type;
			break;
		case VOID:
			if (noun != UNDEF || adj != INT)
				goto bad;
			adj = noun = VOID;
			break;
		default:
			goto bad;
		}
	next:
		l = p->n_left;
		nfree(p);
		p = l;
	}

#ifdef CHAR_UNSIGNED
	if (noun == CHAR && sign == 0)
		sign = UNSIGNED;
#endif
	if (noun == UNDEF) {
		noun = INT;
	} else if (noun == FLOAT) {
		if (sign != 0 || adj == SHORT)
			goto bad;
		noun = (adj == LONG ? DOUBLE : FLOAT);
	} else if (noun == DOUBLE) {
		if (sign != 0 || adj == SHORT)
			goto bad;
		noun = (adj == LONG ? LDOUBLE : DOUBLE);
	} else if (noun == CHAR && adj != INT)
		goto bad;

	if (adj != INT && (noun != DOUBLE && noun != LDOUBLE))
		noun = adj;
	if (sign == UNSIGNED)
		noun += (UNSIGNED-INT);

	p = block(TYPE, NIL, NIL, noun, 0, 0);
	p->n_qual = qual >> TSHIFT;
	if (strunem != 0)
		class = strunem;
	p->n_lval = class;
	return p;

bad:	uerror("illegal type combination");
	return mkty(INT, 0, 0);
}

struct tylnk {
	struct tylnk *next;
	union dimfun df;
};

static void tyreduce(NODE *p, struct tylnk **, int *);

#define TELLIPSIS INCREF(INCREF(MOETY))

static void
tylkadd(union dimfun dim, struct tylnk **tylkp, int *ntdim)
{
	(*tylkp)->next = tmpalloc(sizeof(struct tylnk));
	*tylkp = (*tylkp)->next;
	(*tylkp)->next = NULL;
	(*tylkp)->df = dim;
	(*ntdim)++;
}

/* merge type typ with identifier idp  */
NODE *
tymerge(NODE *typ, NODE *idp)
{
	NODE *p;
	union dimfun *j;
	struct tylnk *base, tylnk, *tylkp;
	unsigned int t;
	int ntdim, i;

	if (typ->n_op != TYPE)
		cerror("tymerge: arg 1");

#ifdef PCC_DEBUG
	if (ddebug > 2) {
		printf("tymerge(%p,%p)\n", typ, idp);
		fwalk(typ, eprint, 0);
		fwalk(idp, eprint, 0);
	}
#endif

	idp->n_type = typ->n_type;
	idp->n_qual = INCQAL(typ->n_qual) | idp->n_qual;

	tylkp = &tylnk;
	tylkp->next = NULL;
	ntdim = 0;

	tyreduce(idp, &tylkp, &ntdim);
	idp->n_sue = typ->n_sue;

	for (t = typ->n_type, j = typ->n_df; t&TMASK; t = DECREF(t))
		if (ISARY(t) || ISFTN(t))
			tylkadd(*j++, &tylkp, &ntdim);

	if (ntdim) {
		union dimfun *a = permalloc(sizeof(union dimfun) * ntdim);
		dimfuncnt += ntdim;
		for (i = 0, base = tylnk.next; base; base = base->next, i++)
			a[i] = base->df;
		idp->n_df = a;
	} else
		idp->n_df = NULL;

	/* now idp is a single node: fix up type */

	idp->n_type = ctype(idp->n_type);
	idp->n_qual = DECQAL(idp->n_qual);

	/* in case ctype has rewritten things */
	if ((t = BTYPE(idp->n_type)) != STRTY && t != UNIONTY && t != ENUMTY)
		idp->n_sue = MKSUE(t);

	if (idp->n_op != NAME) {
		for (p = idp->n_left; p->n_op != NAME; p = p->n_left)
			nfree(p);
		nfree(p);
		idp->n_op = NAME;
	}

	return(idp);
}

/*
 * Retrieve all CM-separated argument types, sizes and dimensions and
 * put them in an array.
 */
static union arglist *
arglist(NODE *n)
{
	union arglist *al;
	NODE *w = n, **ap;
	int num, cnt, i, j, k;
	TWORD ty;

	/* First: how much to allocate */
	for (num = cnt = 0, w = n; w->n_op == CM; w = w->n_left) {
		cnt++;	/* Number of levels */
		num++;	/* At least one per step */
		if (w->n_right->n_op == ELLIPSIS)
			continue;
		ty = w->n_right->n_type;
		if (BTYPE(ty) == STRTY || BTYPE(ty) == UNIONTY ||
		    BTYPE(ty) == ENUMTY)
			num++;
		while (ISFTN(ty) == 0 && ISARY(ty) == 0 && ty > BTMASK)
			ty = DECREF(ty);
		if (ty > BTMASK)
			num++;
	}
	cnt++;
	ty = w->n_type;
	if (BTYPE(ty) == STRTY || BTYPE(ty) == UNIONTY ||
	    BTYPE(ty) == ENUMTY)
		num++;
	while (ISFTN(ty) == 0 && ISARY(ty) == 0 && ty > BTMASK)
		ty = DECREF(ty);
	if (ty > BTMASK)
		num++;
	num += 2; /* TEND + last arg type */

	/* Second: Create list to work on */
	ap = tmpalloc(sizeof(NODE *) * cnt);
	al = permalloc(sizeof(union arglist) * num);
	arglistcnt += num;

	for (w = n, i = 0; w->n_op == CM; w = w->n_left)
		ap[i++] = w->n_right;
	ap[i] = w;

	/* Third: Create actual arg list */
	for (k = 0, j = i; j >= 0; j--) {
		if (ap[j]->n_op == ELLIPSIS) {
			al[k++].type = TELLIPSIS;
			ap[j]->n_op = ICON; /* for tfree() */
			continue;
		}
		/* Convert arrays to pointers */
		if (ISARY(ap[j]->n_type)) {
			ap[j]->n_type += (PTR-ARY);
			ap[j]->n_df++;
		}
		/* Convert (silently) functions to pointers */
		if (ISFTN(ap[j]->n_type))
			ap[j]->n_type = INCREF(ap[j]->n_type);
		ty = ap[j]->n_type;
		al[k++].type = ty;
		if (BTYPE(ty) == STRTY || BTYPE(ty) == UNIONTY ||
		    BTYPE(ty) == ENUMTY)
			al[k++].sue = ap[j]->n_sue;
		while (ISFTN(ty) == 0 && ISARY(ty) == 0 && ty > BTMASK)
			ty = DECREF(ty);
		if (ty > BTMASK)
			al[k++].df = ap[j]->n_df;
	}
	al[k++].type = TNULL;
	tfree(n);
	return al;
}

/*
 * build a type, and stash away dimensions,
 * from a parse tree of the declaration
 * the type is build top down, the dimensions bottom up
 */
void
tyreduce(NODE *p, struct tylnk **tylkp, int *ntdim)
{
	union dimfun dim;
	NODE *r;
	int o;
	TWORD t, q;

	o = p->n_op;
	if (o == NAME)
		return;

	t = INCREF(p->n_type);
	q = p->n_qual;
	switch (o) {
	case CALL:
		t += (FTN-PTR);
		dim.dfun = arglist(p->n_right);
		break;
	case UCALL:
		t += (FTN-PTR);
		dim.dfun = NULL;
		break;
	case LB:
		t += (ARY-PTR);
		if (p->n_right->n_op != ICON) {
			r = p->n_right;
			o = RB;
		} else {
			dim.ddim = p->n_right->n_lval;
			nfree(p->n_right);
			if (dim.ddim == 0 && p->n_left->n_op == LB)
				uerror("null dimension");
		}
		break;
	}

	p->n_left->n_type = t;
	p->n_left->n_qual = INCQAL(q) | p->n_left->n_qual;
	tyreduce(p->n_left, tylkp, ntdim);

	if (o == LB || o == (UCALL) || o == CALL)
		tylkadd(dim, tylkp, ntdim);
	if (o == RB) {
		dim.ddim = -1;
		tylkadd(dim, tylkp, ntdim);
		arrstk[arrstkp++] = r;
	}

	p->n_sp = p->n_left->n_sp;
	p->n_type = p->n_left->n_type;
	p->n_qual = p->n_left->n_qual;
}

static NODE *
argcast(NODE *p, TWORD t, union dimfun *d, struct suedef *sue)
{
	NODE *u, *r = talloc();

	r->n_op = NAME;
	r->n_type = t;
	r->n_qual = 0; /* XXX */
	r->n_df = d;
	r->n_sue = sue;

	u = buildtree(CAST, r, p);
	nfree(u->n_left);
	r = u->n_right;
	nfree(u);
	return r;
}


/*
 * Do prototype checking and add conversions before calling a function.
 */
NODE *
doacall(NODE *f, NODE *a)
{
	NODE *w, *r;
	union arglist *al;
	struct ap {
		struct ap *next;
		NODE *node;
	} *at, *apole = NULL;
	int argidx, hasarray;
	TWORD type, arrt;

	/*
	 * Do some basic checks.
	 */
	if (f->n_df == NULL || (al = f->n_df[0].dfun) == NULL) {
		if (Wimplicit_function_declaration) {
			if (f->n_sp != NULL)
				werror("no prototype for function '%s()'",
				    f->n_sp->sname);
			else
				werror("no prototype for function pointer");
		}
		goto build;
	}
	if (al->type == VOID) {
		if (a != NULL)
			uerror("function takes no arguments");
		goto build; /* void function */
	} else {
		if (a == NULL) {
			uerror("function needs arguments");
			goto build;
		}
	}

	/*
	 * Create a list of pointers to the nodes given as arg.
	 */
	for (w = a; w->n_op == CM; w = w->n_left) {
		at = tmpalloc(sizeof(struct ap));
		at->node = w->n_right;
		at->next = apole;
		apole = at;
	}
	at = tmpalloc(sizeof(struct ap));
	at->node = w;
	at->next = apole;
	apole = at;

	/*
	 * Do the typechecking by walking up the list.
	 */
	argidx = 1;
	while (al->type != TNULL) {
		if (al->type == TELLIPSIS)
			goto build;
		if (apole == NULL) {
			uerror("too few arguments to function");
			goto build;
		}
		type = apole->node->n_type;
		arrt = al->type;
		if ((hasarray = ISARY(arrt)))
			arrt += (PTR-ARY);
		if (ISARY(type))
			type += (PTR-ARY);

		/* Check structs */
		if (type <= BTMASK && arrt <= BTMASK) {
			if (type != arrt) {
				if (ISSOU(BTYPE(type)) || ISSOU(BTYPE(arrt))) {
incomp:					uerror("incompatible types for arg %d",
					    argidx);
				} else {
					MKTY(apole->node, arrt, 0, 0)
				}
			} else if (ISSOU(BTYPE(type))) {
				if (apole->node->n_sue != al[1].sue)
					goto incomp;
			}
			goto out;
		}

		/* Hereafter its only pointers left */
		if (((type <= BTMASK) && ISSOU(BTYPE(type))) ||
		    ((arrt <= BTMASK) && ISSOU(BTYPE(arrt))))
			goto incomp;

		if (type == arrt) {
			if (ISSOU(BTYPE(type))) {
				if (apole->node->n_sue == al[1].sue)
					goto out;
			} else
				goto out;
		}
		if (BTYPE(arrt) == ENUMTY && BTYPE(type) == INT &&
		    (arrt & ~BTMASK) == (type & ~BTMASK))
			goto skip; /* XXX enumty destroyed in optim() */
		if (BTYPE(arrt) == VOID && type > BTMASK)
			goto skip; /* void *f = some pointer */
		if (arrt > BTMASK && BTYPE(type) == VOID)
			goto skip; /* some *f = void pointer */
		if (apole->node->n_op == ICON && apole->node->n_lval == 0)
			goto skip; /* Anything assigned a zero */

		if ((type & ~BTMASK) == (arrt & ~BTMASK)) {
			/* do not complain for intermixed char/uchar */
			if ((BTYPE(type) == CHAR || BTYPE(type) == UCHAR) &&
			    (BTYPE(arrt) == CHAR || BTYPE(arrt) == UCHAR))
				goto skip;
		}

		werror("implicit conversion of argument %d due to prototype",
		    argidx);

skip:		if (ISSTR(BTYPE(arrt))) {
			MKTY(apole->node, arrt, 0, al[1].sue)
		} else {
			MKTY(apole->node, arrt, 0, 0)
		}

out:		al++;
		if (ISSTR(BTYPE(arrt)))
			al++;
		while (arrt > BTMASK && !ISFTN(arrt))
			arrt = DECREF(arrt);
		if (ISFTN(arrt) || hasarray)
			al++;
		apole = apole->next;
		argidx++;
	}
	if (apole != NULL)
		uerror("too many arguments to function");

build:	return buildtree(a == NIL ? UCALL : CALL, f, a);
}

static int
chk2(TWORD type, union dimfun *dsym, union dimfun *ddef)
{
	while (type > BTMASK) {
		switch (type & TMASK) {
		case ARY:
			if ((dsym++)->ddim != (ddef++)->ddim)
				return 1;
			break;
		case FTN:
			/* old-style function headers with function pointers
			 * will most likely not have a prototype.
			 * This is not considered an error.  */
			if (ddef->dfun == NULL) {
#ifdef notyet
				werror("declaration not a prototype");
#endif
			} else if (chkftn(dsym->dfun, ddef->dfun))
				return 1;
			dsym++, ddef++;
			break;
		}
		type = DECREF(type);
	}
	return 0;
}

/*
 * Compare two function argument lists to see if they match.
 */
int
chkftn(union arglist *usym, union arglist *udef)
{
	TWORD t2;
	int ty, tyn;

	if (usym == NULL)
		return 0;
	if (cftnsp != NULL && udef == NULL && usym->type == VOID)
		return 0; /* foo() { function with foo(void); prototype */
	if (udef == NULL && usym->type != TNULL)
		return 1;
	while (usym->type != TNULL) {
		if (usym->type == udef->type)
			goto done;
		/*
		 * If an old-style declaration, then all types smaller than
		 * int are given as int parameters.
		 */
		if (intcompare) {
			ty = BTYPE(usym->type);
			tyn = BTYPE(udef->type);
			if (ty == tyn || ty != INT)
				return 1;
			if (tyn == CHAR || tyn == UCHAR ||
			    tyn == SHORT || tyn == USHORT)
				goto done;
			return 1;
		} else
			return 1;

done:		ty = BTYPE(usym->type);
		t2 = usym->type;
		if (ISSTR(ty)) {
			usym++, udef++;
			if (usym->sue != udef->sue)
				return 1;
		}

		while (ISFTN(t2) == 0 && ISARY(t2) == 0 && t2 > BTMASK)
			t2 = DECREF(t2);
		if (t2 > BTMASK) {
			usym++, udef++;
			if (chk2(t2, usym->df, udef->df))
				return 1;
		}
		usym++, udef++;
	}
	if (usym->type != udef->type)
		return 1;
	return 0;
}

void
fixtype(NODE *p, int class)
{
	unsigned int t, type;
	int mod1, mod2;
	/* fix up the types, and check for legality */

	if( (type = p->n_type) == UNDEF ) return;
	if ((mod2 = (type&TMASK))) {
		t = DECREF(type);
		while( mod1=mod2, mod2 = (t&TMASK) ){
			if( mod1 == ARY && mod2 == FTN ){
				uerror( "array of functions is illegal" );
				type = 0;
				}
			else if( mod1 == FTN && ( mod2 == ARY || mod2 == FTN ) ){
				uerror( "function returns illegal type" );
				type = 0;
				}
			t = DECREF(t);
			}
		}

	/* detect function arguments, watching out for structure declarations */
	if (instruct && ISFTN(type)) {
		uerror("function illegal in structure or union");
		type = INCREF(type);
	}
	p->n_type = type;
}

/*
 * give undefined version of class
 */
int
uclass(int class)
{
	if (class == SNULL)
		return(EXTERN);
	else if (class == STATIC)
		return(USTATIC);
	else if (class == FORTRAN)
		return(UFORTRAN);
	else
		return(class);
}

int
fixclass(int class, TWORD type)
{
	/* first, fix null class */
	if (class == SNULL) {
		if (instruct&INSTRUCT)
			class = MOS;
		else if (instruct&INUNION)
			class = MOU;
		else if (blevel == 0)
			class = EXTDEF;
		else
			class = AUTO;
	}

	/* now, do general checking */

	if( ISFTN( type ) ){
		switch( class ) {
		default:
			uerror( "function has illegal storage class" );
		case AUTO:
			class = EXTERN;
		case EXTERN:
		case EXTDEF:
		case FORTRAN:
		case TYPEDEF:
		case STATIC:
		case UFORTRAN:
		case USTATIC:
			;
			}
		}

	if( class&FIELD ){
		if( !(instruct&INSTRUCT) ) uerror( "illegal use of field" );
		return( class );
		}

	switch( class ){

	case MOU:
		if( !(instruct&INUNION) ) uerror( "illegal MOU class" );
		return( class );

	case MOS:
		if( !(instruct&INSTRUCT) ) uerror( "illegal MOS class" );
		return( class );

	case MOE:
		if( instruct & (INSTRUCT|INUNION) ) uerror( "illegal MOE class" );
		return( class );

	case REGISTER:
		if (blevel == 0)
			uerror( "illegal register declaration" );
		else if (regvar >= MINRVAR && cisreg(type) && !Oflag)
			return(class);
		if (blevel == 1)
			return(PARAM);
		else
			return(AUTO);

	case AUTO:
		if( blevel < 2 ) uerror( "illegal ULABEL class" );
		return( class );

	case UFORTRAN:
	case FORTRAN:
# ifdef NOFORTRAN
		NOFORTRAN;    /* a condition which can regulate the FORTRAN usage */
# endif
		if( !ISFTN(type) ) uerror( "fortran declaration must apply to function" );
		else {
			type = DECREF(type);
			if( ISFTN(type) || ISARY(type) || ISPTR(type) ) {
				uerror( "fortran function has wrong type" );
				}
			}
	case STNAME:
	case UNAME:
	case ENAME:
	case EXTERN:
	case STATIC:
	case EXTDEF:
	case TYPEDEF:
	case USTATIC:
		return( class );

	default:
		cerror( "illegal class: %d", class );
		/* NOTREACHED */

	}
	return 0; /* XXX */
}

/*
 * Generates a goto statement; sets up label number etc.
 */
void
gotolabel(char *name)
{
	struct symtab *s = lookup(name, SLBLNAME);

	if (s->soffset == 0)
		s->soffset = -getlab();
	branch(s->soffset < 0 ? -s->soffset : s->soffset);
}

/*
 * Sets a label for gotos.
 */
void
deflabel(char *name)
{
	struct symtab *s = lookup(name, SLBLNAME);

	if (s->soffset > 0)
		uerror("label '%s' redefined", name);
	if (s->soffset == 0)
		s->soffset = getlab();
	if (s->soffset < 0)
		s->soffset = -s->soffset;
	send_passt(IP_DEFLAB, s->soffset);
}

struct symtab *
getsymtab(char *name, int flags)
{
	struct symtab *s;

	if (flags & STEMP) {
		s = tmpalloc(sizeof(struct symtab));
	} else {
		s = permalloc(sizeof(struct symtab));
		symtabcnt++;
	}
	s->sname = name;
	s->snext = NULL;
	s->stype = UNDEF;
	s->squal = 0;
	s->sclass = SNULL;
	s->sflags = flags & SMASK;
	s->soffset = 0;
	s->slevel = blevel;
	return s;
}

/*
 * define the current location as the name p->sname
 */
void
defnam(struct symtab *p)
{
	if (p->sclass == STATIC && p->slevel > 1)
		send_passt(IP_DEFLAB, p->soffset);
	else
#ifdef GCC_COMPAT
		send_passt(IP_DEFNAM, gcc_findname(p), p->sclass == EXTDEF);
#else
		send_passt(IP_DEFNAM, p->sname, p->sclass == EXTDEF);
#endif
}


#ifdef PCC_DEBUG
static char *
ccnames[] = { /* names of storage classes */
	"SNULL",
	"AUTO",
	"EXTERN",
	"STATIC",
	"REGISTER",
	"EXTDEF",
	"LABEL",
	"ULABEL",
	"MOS",
	"PARAM",
	"STNAME",
	"MOU",
	"UNAME",
	"TYPEDEF",
	"FORTRAN",
	"ENAME",
	"MOE",
	"UFORTRAN",
	"USTATIC",
	};

char *
scnames(int c)
{
	/* return the name for storage class c */
	static char buf[12];
	if( c&FIELD ){
		sprintf( buf, "FIELD[%d]", c&FLDSIZ );
		return( buf );
		}
	return( ccnames[c] );
	}
#endif
