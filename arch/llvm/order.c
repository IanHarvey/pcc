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

#include "pass2.h"
#include <string.h>

/*
 * Check if it's legal to make an OREG or NAME entry which has an
 * offset of off, (from a register of r), if the resulting thing had type t.
 * For LLVM, we're very permissive since we use GEP instructions.
 */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	/* LLVM can handle any offset via getelementptr */
	return 0;
}

/*
 * Turn a UMUL-referenced node into OREG.
 * For LLVM, this is simplified since we use load/store instructions.
 */
void
offstar(NODE *p, int shape)
{
	/* LLVM uses explicit load/store, so we don't need complex OREG handling */
	if (isreg(p))
		return;

	/* Generate code to compute the address into a register */
	(void)geninsn(p, INAREG);
}

/*
 * Do the actual conversion of offstar-found OREGs into real OREGs.
 */
void
myormake(NODE *q)
{
	/* LLVM doesn't need OREG conversion, we use explicit addressing */
}

/*
 * Return the register class for a specific type.
 */
int
gclass(TWORD t)
{
	if (t == FLOAT || t == DOUBLE || t == LDOUBLE)
		return CLASSB;
	return CLASSA;
}

/*
 * Check if a node can be used as an address.
 */
int
canaddr(NODE *p)
{
	/* In LLVM, most things can be addressed */
	return 1;
}

/*
 * Decide if a value should be placed in a specific register for a call.
 */
int
acceptable(struct optab *op)
{
	/* All operations are acceptable in LLVM */
	return 1;
}

/*
 * Return register number for a given class and color.
 */
int
aliasmap(int class, int regnum)
{
	/* Direct mapping for LLVM */
	return regnum;
}

/*
 * Return the register class mask for a type.
 */
int
classmask(int class)
{
	switch (class) {
	case CLASSA:
		return (1 << 16) - 1; /* First 16 registers */
	case CLASSB:
		return ((1 << 8) - 1) << 16; /* Next 8 registers */
	default:
		return 0;
	}
}

/*
 * Return the type class mask.
 */
int
tclassmask(int class)
{
	return classmask(class);
}

/*
 * Check if we need to adjust the stack for a function call.
 */
int
stkretts(TWORD t)
{
	/* LLVM handles return values via explicit instructions */
	return 0;
}

/*
 * Check if this is a small enough struct to return in registers.
 */
int
stkretarg(TWORD t, int sz)
{
	/* LLVM handles struct returns explicitly */
	return 0;
}

/*
 * Return the register offset for argument passing.
 */
int
argstkadj(struct interpass_prolog *ipp, int nrarg)
{
	/* LLVM doesn't need stack adjustment for args */
	return 0;
}
