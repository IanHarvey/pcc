/*	$Id$	*/
/*
 * Copyright (c) 2025 QBE Backend Contributors
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

/*
 * Machine-dependent code for QBE backend (Pass 1).
 */

/*
 * Clocal performs local transformations on expression trees.
 */
NODE *
clocal(NODE *p)
{
	int o = p->n_op;

	switch (o) {
	case NAME:
		/* Handle special name processing if needed */
		return p;

	case CALL:
	case UCALL:
		/* QBE handles calls natively, no special processing */
		return p;

	case PCONV:
		/* Pointer conversion */
		if (p->n_type == VOID) {
			/* Remove unnecessary void pointer conversions */
			return p;
		}
		break;

	case SCONV:
		/* Type conversion - let QBE handle it */
		break;

	case STCALL:
	case USTCALL:
		/* Structure calls - handled normally */
		return p;

	case ADDROF:
		/* Address-of operator */
		return p;

	case STASG:
		/* Structure assignment */
		return p;
	}

	return p;
}

/*
 * Called before local transformations.
 */
void
myp2tree(NODE *p)
{
	struct symtab *sp;

	if (p->n_op != FCON)
		return;

	/* Handle float constants if needed */
}

/*
 * Called after local transformations.
 */
int
andable(NODE *p)
{
	/* Can this node be referenced by address? */
	return 1; /* QBE can handle most addressing modes */
}

/*
 * Is it legal to make an OREG from p?
 */
int
cisreg(TWORD t)
{
	/* QBE uses virtual registers, so most types work */
	return 1;
}

/*
 * Return a class identifier for a register.
 */
int
fldexpand(NODE *p, int cookie, char **cp)
{
	return 0;
}

/*
 * Does the bitfield fit?
 */
int
fldsz(NODE *p)
{
	return p->n_rval;
}

/*
 * Called during initial variable allocation.
 */
void
pass1_lastchance(struct interpass *ip)
{
	/* Nothing special needed for QBE */
}

/*
 * Target-specific assignment conversion.
 */
NODE *
builtin_cfa(const struct bitable *bt, NODE *a)
{
	return bfcode_builtin_cfa(bt, a);
}

NODE *
builtin_return_address(const struct bitable *bt, NODE *a)
{
	return bfcode_builtin_return_address(bt, a);
}

NODE *
builtin_frame_address(const struct bitable *bt, NODE *a)
{
	return bfcode_builtin_frame_address(bt, a);
}

/*
 * Variable attribute handling.
 */
#ifndef LANG_CXX
void
fixdef(struct symtab *sp)
{
	/* Handle any target-specific symbol table fixups */
}

void
pass1_conput(FILE *fp, NODE *p)
{
	/* Emit constants if needed */
}
#endif
