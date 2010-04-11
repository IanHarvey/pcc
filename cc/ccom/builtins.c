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

# include "pass1.h"


#ifndef NO_C_BUILTINS
/*
 * replace an alloca function with direct allocation on stack.
 * return a destination temp node.
 */
static NODE *
builtin_alloca(NODE *f, NODE *a)
{
	struct symtab *sp;
	NODE *t, *u;

#ifdef notyet
	if (xnobuiltins)
		return NULL;
#endif
	sp = f->n_sp;

	t = tempnode(0, VOID|PTR, 0, MKSUE(INT) /* XXX */);
	u = tempnode(regno(t), VOID|PTR, 0, MKSUE(INT) /* XXX */);
	spalloc(t, a, SZCHAR);
	tfree(f);
	return u;
}

/*
 * See if there is a goto in the tree.
 * XXX this function is a hack for a flaw in handling of 
 * compound expressions and inline functions and should not be 
 * needed.
 */
static int
hasgoto(NODE *p)
{
	int o = coptype(p->n_op);

	if (o == LTYPE)
		return 0;
	if (p->n_op == GOTO)
		return 1;
	if (o == UTYPE)
		return hasgoto(p->n_left);
	if (hasgoto(p->n_left))
		return 1;
	return hasgoto(p->n_right);
}

/*
 * Determine if a value is known to be constant at compile-time and
 * hence that PCC can perform constant-folding on expressions involving
 * that value.
 */
static NODE *
builtin_constant_p(NODE *f, NODE *a)
{
	int isconst = (a->n_op == ICON);

	tfree(f);
	if (hasgoto(a)) {
		a = buildtree(COMOP, a, bcon(0));
	} else {
		tfree(a);
		a = bcon(isconst);
	}

	return a;
}

/*
 * Hint to the compiler whether this expression will evaluate true or false.
 * Just ignored for now.
 */
static NODE *
builtin_expect(NODE *f, NODE *a)
{

	tfree(f);
	if (a && a->n_op == CM) {
		tfree(a->n_right);
		f = a->n_left;
		nfree(a);
		a = f;
	}

	return a;
}

/*
 * Just invoke memcpy(3).
 */
static NODE *
builtin_memcpy(NODE *f, NODE *a)
{
	f->n_sp = lookup("memcpy", SNORMAL);
	return buildtree(CALL, f, a);
}

/*
 * Just invoke memset(3).
 */
static NODE *
builtin_memset(NODE *f, NODE *a)
{

	f->n_sp = lookup("memset", SNORMAL);
	return buildtree(CALL, f, a);
}

/*
 * Take integer absolute value.
 * Simply does: ((((x)>>(8*sizeof(x)-1))^(x))-((x)>>(8*sizeof(x)-1)))
 */
static NODE *
builtin_abs(NODE *f, NODE *a)
{
	NODE *p, *q, *r, *t, *t2, *t3;
	int tmp1, tmp2, shift;

	if (a->n_type != INT)
		a = cast(a, INT, 0);

	tfree(f);

	if (a->n_op == ICON) {
		if (a->n_lval < 0)
			a->n_lval = -a->n_lval;
		p = a;
	} else {
		t = tempnode(0, a->n_type, a->n_df, a->n_sue);
		tmp1 = regno(t);
		p = buildtree(ASSIGN, t, a);

		t = tempnode(tmp1, a->n_type, a->n_df, a->n_sue);
		shift = (int)tsize(a->n_type, a->n_df, a->n_sue) - 1;
		q = buildtree(RS, t, bcon(shift));

		t2 = tempnode(0, a->n_type, a->n_df, a->n_sue);
		tmp2 = regno(t2);
		q = buildtree(ASSIGN, t2, q);

		t = tempnode(tmp1, a->n_type, a->n_df, a->n_sue);
		t2 = tempnode(tmp2, a->n_type, a->n_df, a->n_sue);
		t3 = tempnode(tmp2, a->n_type, a->n_df, a->n_sue);
		r = buildtree(MINUS, buildtree(ER, t, t2), t3);

		p = buildtree(COMOP, p, buildtree(COMOP, q, r));
	}

	return p;
}

#ifndef TARGET_STDARGS
static NODE *
builtin_stdarg_start(NODE *f, NODE *a)
{
	NODE *p, *q;
	int sz;

	/* must first deal with argument size; use int size */
	p = a->n_right;
	if (p->n_type < INT) {
		sz = (int)(SZINT/tsize(p->n_type, p->n_df, p->n_sue));
	} else
		sz = 1;

	/* do the real job */
	p = buildtree(ADDROF, p, NIL); /* address of last arg */
#ifdef BACKAUTO
	p = optim(buildtree(PLUS, p, bcon(sz))); /* add one to it (next arg) */
#else
	p = optim(buildtree(MINUS, p, bcon(sz))); /* add one to it (next arg) */
#endif
	q = block(NAME, NIL, NIL, PTR+VOID, 0, 0); /* create cast node */
	q = buildtree(CAST, q, p); /* cast to void * (for assignment) */
	p = q->n_right;
	nfree(q->n_left);
	nfree(q);
	p = buildtree(ASSIGN, a->n_left, p); /* assign to ap */
	tfree(f);
	nfree(a);
	return p;
}

static NODE *
builtin_va_arg(NODE *f, NODE *a)
{
	NODE *p, *q, *r, *rv;
	int sz, nodnum;

	/* create a copy to a temp node of current ap */
	p = ccopy(a->n_left);
	q = tempnode(0, p->n_type, p->n_df, p->n_sue);
	nodnum = regno(q);
	rv = buildtree(ASSIGN, q, p);

	r = a->n_right;
	sz = (int)tsize(r->n_type, r->n_df, r->n_sue)/SZCHAR;
	/* add one to ap */
#ifdef BACKAUTO
	rv = buildtree(COMOP, rv , buildtree(PLUSEQ, a->n_left, bcon(sz)));
#else
#error fix wrong eval order in builtin_va_arg
	ecomp(buildtree(MINUSEQ, a->n_left, bcon(sz)));
#endif

	nfree(a->n_right);
	nfree(a);
	nfree(f);
	r = tempnode(nodnum, INCREF(r->n_type), r->n_df, r->n_sue);
	return buildtree(COMOP, rv, buildtree(UMUL, r, NIL));

}

static NODE *
builtin_va_end(NODE *f, NODE *a)
{
	tfree(f);
	tfree(a);
	return bcon(0); /* nothing */
}

static NODE *
builtin_va_copy(NODE *f, NODE *a)
{
	tfree(f);
	f = buildtree(ASSIGN, a->n_left, a->n_right);
	nfree(a);
	return f;
}
#endif /* TARGET_STDARGS */

static struct bitable {
	char *name;
	NODE *(*fun)(NODE *f, NODE *a);
	int narg;
} bitable[] = {
	{ "__builtin_alloca", builtin_alloca, 1 },
	{ "__builtin_constant_p", builtin_constant_p, 1 },
	{ "__builtin_abs", builtin_abs, 1 },
	{ "__builtin_expect", builtin_expect, 2 },
	{ "__builtin_memcpy", builtin_memcpy, 3 },
	{ "__builtin_memset", builtin_memset, 3 },
#ifndef TARGET_STDARGS
	{ "__builtin_stdarg_start", builtin_stdarg_start, 2 },
	{ "__builtin_va_start", builtin_stdarg_start, 2 },
	{ "__builtin_va_arg", builtin_va_arg, 2 },
	{ "__builtin_va_end", builtin_va_end, 1 },
	{ "__builtin_va_copy", builtin_va_copy, 2 },
#endif
#ifdef TARGET_BUILTINS
	TARGET_BUILTINS
#endif
};

static int
acnt(NODE *a, int narg)
{
	if (a == NIL)
		return narg;
	for (; a->n_op == CM; a = a->n_left, narg--)
		;
	return narg != 1;
}

NODE *
builtin_check(NODE *f, NODE *a)
{
	int i;

	for (i = 0; i < (int)(sizeof(bitable)/sizeof(bitable[0])); i++) {
		if (strcmp(bitable[i].name, f->n_sp->sname))
			continue;
		if (bitable[i].narg >= 0 && acnt(a, bitable[i].narg)) {
			uerror("bad argument to %s", bitable[i].name);
			return bcon(0);
		}
		return (*bitable[i].fun)(f, a);
	}
	return NIL;
}
#endif
