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

#define TLL TLONG|TULONG
#define ANYSIGNED TINT|TSHORT|TCHAR
#define ANYUSIGNED TUNSIGNED|TUSHORT|TUCHAR
#define ANYFIXED ANYSIGNED|ANYUSIGNED
#define TUWORD TUNSIGNED
#define TSWORD TINT
#define TWORD TUWORD|TSWORD
#define TANYINT TLL|ANYFIXED

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/*
 * For LLVM IR backend, we use a minimal instruction table.
 * Most operations are handled directly in zzzcode() which generates
 * LLVM IR instructions on the fly.
 */

/* Conversions - mostly no-ops in LLVM IR, handled by explicit cast instructions */
{ PCONV, INAREG,
	SAREG, TANYINT|TPOINT,
	SAREG, TANYINT|TPOINT,
		0, RLEFT,
		"", },

{ SCONV, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RLEFT,
		"", },

/* Load operations */
{ UMUL, INAREG,
	SAREG, TPOINT,
	SANY, TANYINT,
		NAREG, RESC1,
		"ZB", }, /* Load via zzzcode */

/* Store operations */
{ ASSIGN, FOREFF,
	SAREG, TANYINT|TPOINT,
	SAREG, TANYINT|TPOINT,
		0, RDEST,
		"ZB", }, /* Store via zzzcode */

/* Arithmetic operations */
{ PLUS, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

{ MINUS, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

{ MUL, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

{ DIV, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

{ MOD, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

/* Bitwise operations */
{ AND, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

{ OR, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

{ ER, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

/* Shift operations */
{ LS, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

{ RS, INAREG,
	SAREG, TANYINT,
	SAREG, TANYINT,
		NAREG, RESC1,
		"ZB", },

/* Unary operations */
{ UMINUS, INAREG,
	SAREG, TANYINT,
	SANY, TANY,
		NAREG, RESC1,
		"ZB", },

{ COMPL, INAREG,
	SAREG, TANYINT,
	SANY, TANY,
		NAREG, RESC1,
		"ZB", },

/* Comparison operations */
{ EQ, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ NE, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ LT, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ LE, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ GT, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ GE, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ ULT, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ ULE, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ UGT, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

{ UGE, FORCC,
	SAREG, TANYINT,
	SAREG, TANYINT,
		0, RNULL,
		"ZB", },

/* Floating point operations */
{ PLUS, INBREG,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG, RESC1,
		"ZB", },

{ MINUS, INBREG,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG, RESC1,
		"ZB", },

{ MUL, INBREG,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG, RESC1,
		"ZB", },

{ DIV, INBREG,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG, RESC1,
		"ZB", },

/* Function calls */
{ CALL, INAREG,
	SCON, TANY,
	SANY, TANY,
		NAREG, RESC1,
		"ZB", },

{ CALL, INBREG,
	SCON, TANY,
	SANY, TANY,
		NBREG, RESC1,
		"ZB", },

{ UCALL, INAREG,
	SCON, TANY,
	SANY, TANY,
		NAREG, RESC1,
		"ZB", },

{ UCALL, INBREG,
	SCON, TANY,
	SANY, TANY,
		NBREG, RESC1,
		"ZB", },

/* Register moves */
{ REG, INAREG,
	SANY, TANYINT|TPOINT,
	SAREG, TANYINT|TPOINT,
		NAREG, RESC1,
		"", },

{ REG, INBREG,
	SANY, TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG, TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG, RESC1,
		"", },

/* Constants */
{ ICON, INAREG,
	SANY, TANY,
	SANY, TANYINT|TPOINT,
		NAREG, RESC1,
		"", },

/* End of table marker */
{ FREE, FREE, FREE, FREE, FREE, FREE, FREE, FREE, "", },
};

/*
 * Return the number of entries in the instruction table.
 */
int
tablesize = sizeof(table)/sizeof(table[0]);
