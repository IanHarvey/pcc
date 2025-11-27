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
#undef NIL
#define	NIL NULL
#define	NODE P1ND
#define	nfree p1nfree
#define	ccopy p1tcopy
#define	tfree p1tfree
#endif

/*
 * Return QBE type suffix for a given type.
 */
const char *
qbe_type_suffix(TWORD t)
{
	switch (t) {
	case CHAR:
	case UCHAR:
		return "b";
	case SHORT:
	case USHORT:
		return "h";
	case INT:
	case UNSIGNED:
		return "w";
	case LONG:
	case ULONG:
	case LONGLONG:
	case ULONGLONG:
	case STRTY:
	case UNIONTY:
		return "l";
	case FLOAT:
		return "s";
	case DOUBLE:
	case LDOUBLE:
		return "d";
	default:
		if (ISPTR(t))
			return "l";
		return "w";
	}
}

/*
 * Emit QBE type for instructions.
 */
void
qbe_emit_type(TWORD t)
{
	printf("%s", qbe_type_suffix(t));
}

/*
 * Print out assembler segment name.
 * QBE uses data and section directives.
 */
void
setseg(int seg, char *name)
{
	switch (seg) {
	case PROG:
		/* Code section - no directive needed in QBE */
		break;
	case DATA:
	case LDATA:
		printf("data ");
		break;
	case RDATA:
		printf("data ");
		break;
	case UDATA:
		/* Uninitialized data */
		break;
	default:
		break;
	}
}

/*
 * Define everything needed to print out some data (or text).
 * In QBE, we use 'export' for global symbols.
 */
void
defloc(struct symtab *sp)
{
	char *name;

	name = getexname(sp);

	if (sp->sclass == EXTDEF) {
		printf("export ");
	}

	if (ISFTN(sp->stype)) {
		/* Function definition - emit function header */
		printf("function ");
		/* Return type */
		if (sp->stype == INCREF(VOID)) {
			/* void function */
		} else {
			qbe_emit_type(sp->stype);
			printf(" ");
		}
		printf("$%s(", name);
	} else {
		/* Data definition */
		printf("$%s = ", name);
		if (sp->sclass == EXTDEF)
			printf("align %d ", talign(sp->stype, sp->sap) / SZCHAR);
		printf("{ ");
	}
}

/*
 * Print integer constant.
 * For QBE, just output the value in the appropriate format.
 */
int
ninval(CONSZ off, int fsz, NODE *p)
{
	/* QBE backend - simplified constant output */
	switch (p->n_type) {
	case LONGLONG:
	case ULONGLONG:
	case LONG:
	case ULONG:
		printf("l %lld", (long long)off);
		break;
	case INT:
	case UNSIGNED:
		printf("w %d", (int)off);
		break;
	case SHORT:
	case USHORT:
		printf("h %d", (int)(short)off);
		break;
	case CHAR:
	case UCHAR:
		printf("b %d", (int)(char)off);
		break;
	default:
		/* For pointers and other types */
		printf("l %lld", (long long)off);
		break;
	}
	printf(", ");
	return 1;
}

/*
 * Emit a string constant.
 */
void
instring(struct symtab *sp)
{
	char *s;
	int val;

	printf("data $%s = { ", getexname(sp));

	for (s = sp->sname; *s != 0; ) {
		if (*s++ == '\\') {
			switch (*s) {
			case 'n': val = '\n'; break;
			case 't': val = '\t'; break;
			case 'r': val = '\r'; break;
			case '\\': val = '\\'; break;
			case '0': val = '\0'; break;
			default: val = *s; break;
			}
			s++;
		} else {
			val = s[-1];
		}
		printf("b %d, ", val);
	}
	printf("b 0 }\n");
}

/*
 * Print beginning of function.
 */
void
prologue(struct interpass_prolog *ipp)
{
	printf("# Function: %s\n", ipp->ipp_name ? ipp->ipp_name : "unknown");
}

/*
 * Print end of function.
 */
void
eoftn(struct interpass_prolog *ipp)
{
	if (ipp->ipp_ip.ip_lbl == 0)
		return; /* no code in function */
	printf("}\n\n");
}

/*
 * Called before function code.
 */
void
bfcode(struct symtab **sp, int cnt)
{
	/* Nothing special needed for QBE */
}

/*
 * Called after function declaration.
 */
void
efcode(void)
{
	printf(") {\n");
	printf("@start\n");
}

/*
 * Called at end of function to deal with any cleanup.
 */
void
ejobcode(int flag)
{
	/* Nothing needed */
}

/*
 * Called before generating code for a return statement.
 */
void
bccode(void)
{
	/* Nothing needed */
}

/*
 * Fix up type of field access.
 */
void
fldty(struct symtab *p)
{
	/* QBE doesn't need special handling */
}

/*
 * Fix up type of argument.
 */
void
fldal(unsigned int t)
{
	/* QBE doesn't need special handling */
}

/*
 * Print out a variable.
 */
void
defzero(struct symtab *sp)
{
	int sz = tsize(sp->stype, sp->sdf, sp->sap);
	printf("data $%s = align %d { z %d }\n",
	    getexname(sp),
	    talign(sp->stype, sp->sap) / SZCHAR,
	    sz / SZCHAR);
}
