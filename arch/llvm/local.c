/*	$Id$	*/
/*
 * Copyright (c) 2025 LLVM Backend Implementation
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

#include "pass1.h"

#ifndef LANG_CXX
#define	NODE P1ND
#define	ccopy p1tcopy
#define	tfree p1tfree
#define	nfree p1nfree
#define	fwalk p1fwalk
#define	talloc p1alloc
#endif

/*	this file contains code which is dependent on the target machine */

/*
 * Perform target-specific transformations on the parse tree.
 */
NODE *
clocal(NODE *p)
{
	/* For LLVM, we mostly pass through the tree unchanged */
	return p;
}

/*
 * Perform pass2-specific tree transformations.
 */
void
myp2tree(NODE *p)
{
	/* LLVM IR generation will handle tree transformations */
}

/*
 * Check if it's possible to take the address of a node.
 */
int
andable(NODE *p)
{
	/* In LLVM, we can take the address of most things */
	return 1;
}

/*
 * Return true if a type should be placed in a register.
 */
int
cisreg(TWORD t)
{
	/* LLVM uses virtual registers for everything */
	return 1;
}

/*
 * Allocate space on the stack.
 */
NODE *
spalloc(NODE *t, NODE *p, OFFSZ off)
{
	NODE *sp;

	p = buildtree(MINUS, p, bcon(off));
	p = block(ASSIGN, p, NIL, p->n_type, p->n_df, p->n_ap);

	sp = talloc();
	sp->n_op = STASG;
	sp->n_lval = 0;
	sp->n_left = p;
	sp->n_right = t;

	return sp;
}

/*
 * Print out a constant for initialization.
 */
void
ninval(CONSZ off, int fsz, NODE *p)
{
	/* Output LLVM IR constant */
	union { float f; double d; int i; } u;

	switch (p->n_type) {
	case CHAR:
	case UCHAR:
		printf("i8 %lld", (long long)off);
		break;
	case SHORT:
	case USHORT:
		printf("i16 %lld", (long long)off);
		break;
	case INT:
	case UNSIGNED:
		printf("i32 %lld", (long long)off);
		break;
	case LONG:
	case ULONG:
	case LONGLONG:
	case ULONGLONG:
		printf("i64 %lld", (long long)off);
		break;
	case FLOAT:
		u.i = off;
		printf("float %e", u.f);
		break;
	case DOUBLE:
		u.d = *(double *)&off;
		printf("double %e", u.d);
		break;
	default:
		printf("i64 %lld", (long long)off);
		break;
	}
}

/*
 * Return the external name for a symbol.
 */
char *
exname(char *p)
{
	/* LLVM uses simple name mangling */
	if (p == NULL)
		return "";
	return p;
}

/*
 * Return a string representing the type for LLVM IR.
 */
char *
ctype(TWORD type)
{
	switch (type) {
	case CHAR:
	case UCHAR:
		return "i8";
	case SHORT:
	case USHORT:
		return "i16";
	case INT:
	case UNSIGNED:
		return "i32";
	case LONG:
	case ULONG:
	case LONGLONG:
	case ULONGLONG:
		return "i64";
	case FLOAT:
		return "float";
	case DOUBLE:
		return "double";
	case LDOUBLE:
		return "x86_fp80";
	case VOID:
		return "void";
	default:
		if (ISPTR(type))
			return "i8*";
		return "i64";
	}
}

/*
 * Called before processing a function call.
 */
void
calldec(NODE *p, NODE *q)
{
	/* LLVM calling convention setup if needed */
}

/*
 * Called for external declarations.
 */
void
extdec(struct symtab *q)
{
	/* Handle external declarations */
}
