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
#include <ctype.h>
#include <string.h>

static int vreg_counter = 0;
static int label_counter = 0;

/* Register names for LLVM (virtual registers) */
char *rnames[] = {
	"%r0", "%r1", "%r2", "%r3", "%r4", "%r5", "%r6", "%r7",
	"%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",
	"%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7",
};

/*
 * Define a label.
 */
void
deflab(int label)
{
	printf("L%d:\n", label);
}

/*
 * Generate function prologue.
 */
void
prologue(struct interpass_prolog *ipp)
{
	char *name = ipp->ipp_name;

	/* Output LLVM function definition */
	printf("define %s @%s(", ctype(DECREF(ipp->ipp_type)), name);

	/* Output parameter list */
	/* This is simplified - real implementation would iterate over parameters */
	printf(") {\n");
	printf("entry:\n");

	vreg_counter = 0;
	label_counter = 0;
}

/*
 * Generate function epilogue.
 */
void
eoftn(struct interpass_prolog *ipp)
{
	/* Output function end */
	printf("}\n\n");
}

/*
 * Return LLVM type string for a given type.
 */
char *
llvm_type(TWORD t)
{
	switch (t) {
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
		if (ISPTR(t))
			return "i8*";
		return "i64";
	}
}

/*
 * Generate LLVM IR for special operations.
 */
void
zzzcode(NODE *p, int c)
{
	NODE *l, *r;
	int o;

	switch (c) {
	case 'A': /* Address operation */
		adrput(stdout, p);
		break;

	case 'B': /* Binary operation */
		l = p->n_left;
		r = p->n_right;
		o = p->n_op;

		/* Generate appropriate LLVM instruction */
		switch (o) {
		case PLUS:
			printf("  %%v%d = add %s ", vreg_counter++, llvm_type(p->n_type));
			adrput(stdout, l);
			printf(", ");
			adrput(stdout, r);
			printf("\n");
			break;
		case MINUS:
			printf("  %%v%d = sub %s ", vreg_counter++, llvm_type(p->n_type));
			adrput(stdout, l);
			printf(", ");
			adrput(stdout, r);
			printf("\n");
			break;
		case MUL:
			printf("  %%v%d = mul %s ", vreg_counter++, llvm_type(p->n_type));
			adrput(stdout, l);
			printf(", ");
			adrput(stdout, r);
			printf("\n");
			break;
		case DIV:
			printf("  %%v%d = sdiv %s ", vreg_counter++, llvm_type(p->n_type));
			adrput(stdout, l);
			printf(", ");
			adrput(stdout, r);
			printf("\n");
			break;
		default:
			comperr("unknown binary op in zzzcode");
		}
		break;

	default:
		comperr("unknown zzzcode: %c", c);
	}
}

/*
 * Print out an address or operand.
 */
void
adrput(FILE *io, NODE *p)
{
	int r;

	switch (p->n_op) {
	case REG:
		r = p->n_rval;
		if (r < 0 || r >= MAXREGS)
			comperr("bad register %d", r);
		fprintf(io, "%s", rnames[r]);
		break;

	case ICON:
		/* Integer constant */
		if (p->n_name[0] == '\0') {
			fprintf(io, "%lld", (long long)p->n_lval);
		} else {
			fprintf(io, "@%s", p->n_name);
			if (p->n_lval != 0)
				fprintf(io, "+%lld", (long long)p->n_lval);
		}
		break;

	case NAME:
		/* Named location */
		fprintf(io, "@%s", p->n_name);
		if (p->n_lval != 0)
			fprintf(io, "+%lld", (long long)p->n_lval);
		break;

	case OREG:
		/* Offset from register */
		fprintf(io, "%%v%d", vreg_counter);
		break;

	case TEMP:
		/* Temporary */
		fprintf(io, "%%t%d", (int)p->n_lval);
		break;

	default:
		comperr("unsupported address mode: %s", opst[p->n_op]);
	}
}

/*
 * Print out an instruction.
 */
void
insput(NODE *p)
{
	comperr("insput called - should use zzzcode");
}

/*
 * Generate conditional branch.
 */
void
cbgen(int op, int lab)
{
	/* Generate LLVM conditional branch */
	const char *cond = "";

	switch (op) {
	case EQ: cond = "eq"; break;
	case NE: cond = "ne"; break;
	case LT: cond = "slt"; break;
	case LE: cond = "sle"; break;
	case GT: cond = "sgt"; break;
	case GE: cond = "sge"; break;
	case ULT: cond = "ult"; break;
	case ULE: cond = "ule"; break;
	case UGT: cond = "ugt"; break;
	case UGE: cond = "uge"; break;
	default:
		comperr("unknown conditional: %d", op);
	}

	printf("  br i1 %%cond, label %%L%d, label %%L%d\n",
	       lab, label_counter++);
}

/*
 * Process intermediate representation.
 */
void
myreader(struct interpass *ipole)
{
	/* Default reader - process intermediate code */
}

/*
 * Canonicalize tree for LLVM.
 */
void
mycanon(NODE *p)
{
	/* Canonicalization - usually empty for LLVM */
}

/*
 * Target-specific optimizations.
 */
void
myoptim(struct interpass *ip)
{
	/* LLVM-specific optimizations */
}

/*
 * Handle inline assembly.
 */
int
myxasm(struct interpass *ip, NODE *p)
{
	/* Inline assembly - emit as LLVM inline asm */
	return 0;
}

/*
 * Generate switch statements.
 */
int
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	/* Generate LLVM switch instruction */
	return 0;
}

/*
 * Process -m flags.
 */
void
mflags(char *str)
{
	/* Handle LLVM-specific -m flags */
}

/*
 * Register class mapping.
 */
void
cmapinit(void)
{
	/* Initialize register class mapping */
}

/*
 * Color mapping for register allocation.
 */
int
COLORMAP(int c, int *r)
{
	/* Map colors to physical registers */
	/* For LLVM, we use virtual registers so this is simplified */
	*r = c;
	return c < MAXREGS ? 1 : 0;
}
