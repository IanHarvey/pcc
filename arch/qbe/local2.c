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

#include <string.h>

#include "pass2.h"

/*
 * Register names for QBE virtual registers.
 */
char *rnames[] = {
	/* Integer/long registers (31 total) */
	"%t0", "%t1", "%t2", "%t3", "%t4", "%t5", "%t6", "%t7",
	"%t8", "%t9", "%t10", "%t11", "%t12", "%t13", "%t14", "%t15",
	"%t16", "%t17", "%t18", "%t19", "%t20", "%t21", "%t22", "%t23",
	"%t24", "%t25", "%t26", "%t27", "%t28", "%t29", "%t30",

	/* Float/double registers (31 total) */
	"%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7",
	"%f8", "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",
	"%f16", "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",
	"%f24", "%f25", "%f26", "%f27", "%f28", "%f29", "%f30",
};

/*
 * Return register class and color mapping.
 */
int
COLORMAP(int c, int *r)
{
	int num;

	switch (c) {
	case CLASSA:
		num = 31;
		if (r && *r >= num)
			return 0;
		break;
	case CLASSB:
		num = 31;
		if (r) {
			if (*r >= num)
				return 0;
			*r += 31; /* Offset for float registers */
		}
		break;
	default:
		return 0;
	}
	return num;
}

/*
 * Print a node's register.
 */
static void
prtreg(NODE *p)
{
	int r = p->n_rval;

	if (r < 0 || r >= MAXREGS)
		comperr("bad register %d", r);

	printf("%s", rnames[r]);
}

/*
 * Print the node's value.
 */
void
adrput(FILE *io, NODE *p)
{
	int r;
	char *s;

	switch (p->n_op) {
	case REG:
		prtreg(p);
		return;

	case NAME:
	case OREG:
		if (p->n_op == NAME) {
			s = p->n_name;
			printf("$%s", s);
		} else {
			/* OREG - offset from register */
			if (p->n_lval != 0)
				printf("%lld", (long long)p->n_lval);
			printf("(");
			prtreg(p);
			printf(")");
		}
		return;

	case ICON:
		/* Integer constant */
		printf("%lld", (long long)p->n_lval);
		return;

	case FCON:
		/* Floating point constant */
		printf("%s_%lf", p->n_type == FLOAT ? "s" : "d", p->n_dcon);
		return;

	default:
		comperr("bad address op %d", p->n_op);
	}
}

/*
 * Emit special characters.
 */
void
zzzcode(NODE *p, int c)
{
	switch (c) {
	case 'A': /* Print address mode */
		adrput(stdout, p);
		break;

	case 'L': /* Print left child */
		adrput(stdout, p->n_left);
		break;

	case 'R': /* Print right child */
		adrput(stdout, p->n_right);
		break;

	case 'D': /* Print destination register */
		prtreg(p);
		break;

	case 'T': /* Print type suffix */
		printf("%s", qbe_type_suffix(p->n_type));
		break;

	default:
		comperr("bad zzzcode %c", c);
	}
}

/*
 * Does it cost anything to rewrite a node?
 */
int
rewfld(NODE *p)
{
	return 0; /* QBE doesn't care */
}

/*
 * Return true if a node should be delayed.
 */
int
flshape(NODE *p)
{
	return 0;
}

/*
 * Return true if shape is acceptable.
 */
int
shtemp(NODE *p)
{
	return 0;
}

/*
 * Called before register allocation.
 */
void
adrcon(CONSZ val)
{
	printf("%lld", (long long)val);
}

/*
 * Emit a constant value.
 */
void
conput(FILE *fp, NODE *p)
{
	switch (p->n_op) {
	case ICON:
		fprintf(fp, "%lld", (long long)p->n_lval);
		break;
	case FCON:
		fprintf(fp, "%s_%lf", p->n_type == FLOAT ? "s" : "d", p->n_dcon);
		break;
	default:
		comperr("bad conput");
	}
}

/*
 * Emit assembly instruction name.
 */
void
insput(NODE *p)
{
	comperr("insput");
}

/*
 * Print a comment.
 */
void
upput(NODE *p, int size)
{
	/* Not needed for QBE */
}

/*
 * Print lower part of address.
 */
void
aput(NODE *p)
{
	/* Not needed for QBE */
}

/*
 * Check if register is special.
 */
int
special(NODE *p, int shape)
{
	return SRNOPE;
}

/*
 * Print current line number.
 */
void
deflab(int label)
{
	printf("@.L%d\n", label);
}

/*
 * Branch to label.
 */
void
gencode(NODE *p, int cookie)
{
	/* This will be called via table matching - we'll implement in table.c */
	geninsn(p, cookie);
}

/*
 * Print an integer label.
 */
char *
exname(char *p)
{
	return p;
}

/*
 * Make a name look like an external name.
 */
char *
getexname(struct symtab *p)
{
	return exname(p->soname ? p->soname : p->sname);
}

/*
 * Print a memory reference.
 */
void
rmove(int s, int d, TWORD t)
{
	printf("\t");
	printf("%s =", rnames[d]);
	printf("%s ", qbe_type_suffix(t));
	printf("copy %s\n", rnames[s]);
}

/*
 * Linker set directive.
 */
void
setlocctr(int l)
{
	/* Not needed for QBE */
}

/*
 * Perform local optimizations.
 */
void
lastcall(NODE *p)
{
	/* Nothing needed */
}

/*
 * Signal whether we can do a rewrite.
 */
int
myoptim(struct interpass *ip)
{
	return 0;
}
