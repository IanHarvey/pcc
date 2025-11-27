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

#include "pass2.h"

#include <string.h>

/*
 * QBE register ordering and selection.
 * Since QBE uses virtual registers, ordering is simple.
 */

/*
 * Should a subtree be evaluated first?
 * We don't really care for QBE since it has unlimited virtual registers.
 */
int
setbin(NODE *p)
{
	return 0;
}

/*
 * Set evaluation order of a binary node if it differs from default.
 */
int
setasg(NODE *p, int cookie)
{
	return 0;
}

/*
 * Set evaluation order of unary operations.
 */
int
setuni(NODE *p, int cookie)
{
	return 0;
}

/*
 * Set registers for a node.
 */
int
setorder(NODE *p)
{
	return 0;
}

/*
 * Set left and right register requirements.
 */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	return 0;
}

/*
 * Rewrite for specific register.
 */
void
offstar(NODE *p, int shape)
{
	/* Nothing needed for QBE */
}

/*
 * Setup for integer register allocation.
 */
void
myreader(struct interpass *ipole)
{
	/* QBE handles register allocation, nothing needed */
}

/*
 * Called after register allocation.
 */
void
mycanon(NODE *p)
{
	/* Nothing needed */
}

/*
 * Called when a register is needed.
 */
void
myormake(NODE *p)
{
	/* Nothing needed */
}

/*
 * Rewrite OREG node.
 */
char *
tmpname(void)
{
	static char name[32];
	static int num = 0;
	sprintf(name, "%%tmp%d", num++);
	return name;
}

/*
 * Generate code for conditional jump.
 */
void
cbgen(int o, int lab)
{
	/* Will be handled by table entries */
}

/*
 * Setup struct or struct pointer operations.
 */
void
mygenregs(struct p2env *p2e)
{
	/* Nothing special needed */
}

/*
 * Last chance to fix function call.
 */
int
gclass(TWORD t)
{
	if (t == FLOAT || t == DOUBLE || t == LDOUBLE)
		return CLASSB;
	return CLASSA;
}

/*
 * Special handling for small types.
 */
void
lastchance(struct interpass *ip)
{
	/* Nothing needed */
}
