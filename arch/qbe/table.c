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

#define TLL TLONG|TULONG|TLONGLONG|TULONGLONG
#define ANYSIGNED TINT|TSHORT|TCHAR
#define ANYUSIGNED TUNSIGNED|TUSHORT|TUCHAR
#define ANYFIXED ANYSIGNED|ANYUSIGNED
#define TUWORD TUNSIGNED
#define TSWORD TINT
#define TWORD TUWORD|TSWORD
#define TANYINT TLL|ANYFIXED
#define SHINT SAREG  /* Any integer */
#define ININT INAREG
#define SHFL SBREG   /* shape for floating point */
#define INFL INBREG  /* in floating point register */

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/*
 * Pointer conversions - QBE treats pointers as longs
 */
{ PCONV, INAREG,
	SAREG, TLL|TPOINT,
	SANY, TPOINT,
		0, RLEFT,
		"", },

{ PCONV, INAREG,
	SAREG, TWORD,
	SANY, TPOINT,
		NAREG, RESC1,
		"\tD =l extsw AL\n", },

/*
 * Integer conversions
 */
/* No-op conversions (same size or smaller) */
{ SCONV, INAREG,
	SAREG, TLL,
	SANY, TLL,
		0, RLEFT,
		"", },

{ SCONV, INAREG,
	SAREG, TWORD,
	SANY, TWORD|ANYFIXED,
		0, RLEFT,
		"", },

/* Extending conversions */
{ SCONV, INAREG,
	SAREG, TCHAR,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w extsb AL\n", },

{ SCONV, INAREG,
	SAREG, TUCHAR,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w extub AL\n", },

{ SCONV, INAREG,
	SAREG, TSHORT,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w extsh AL\n", },

{ SCONV, INAREG,
	SAREG, TUSHORT,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w extuh AL\n", },

{ SCONV, INAREG,
	SAREG, TWORD,
	SANY, TLL,
		NAREG, RESC1,
		"\tD =l extsw AL\n", },

{ SCONV, INAREG,
	SAREG, TUWORD,
	SANY, TLL,
		NAREG, RESC1,
		"\tD =l extuw AL\n", },

/*
 * Floating point conversions
 */
{ SCONV, INBREG,
	SBREG, TFLOAT,
	SANY, TDOUBLE,
		NBREG, RESC1,
		"\tD =d exts AL\n", },

{ SCONV, INBREG,
	SBREG, TDOUBLE,
	SANY, TFLOAT,
		NBREG, RESC1,
		"\tD =s truncd AL\n", },

/* Int to float */
{ SCONV, INBREG,
	SAREG, TWORD,
	SANY, TFLOAT,
		NBREG, RESC1,
		"\tD =s swtof AL\n", },

{ SCONV, INBREG,
	SAREG, TWORD,
	SANY, TDOUBLE,
		NBREG, RESC1,
		"\tD =d swtod AL\n", },

/* Float to int */
{ SCONV, INAREG,
	SBREG, TFLOAT,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w stosi AL\n", },

{ SCONV, INAREG,
	SBREG, TDOUBLE,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w dtosi AL\n", },

/*
 * Arithmetic operations
 */

/* Integer addition */
{ PLUS, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l add AL, AR\n", },

{ PLUS, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w add AL, AR\n", },

/* Floating point addition */
{ PLUS, INBREG,
	SBREG, TFLOAT,
	SBREG, TFLOAT,
		NBREG|NBSL, RESC1,
		"\tD =s add AL, AR\n", },

{ PLUS, INBREG,
	SBREG, TDOUBLE,
	SBREG, TDOUBLE,
		NBREG|NBSL, RESC1,
		"\tD =d add AL, AR\n", },

/* Integer subtraction */
{ MINUS, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l sub AL, AR\n", },

{ MINUS, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w sub AL, AR\n", },

/* Floating point subtraction */
{ MINUS, INBREG,
	SBREG, TFLOAT,
	SBREG, TFLOAT,
		NBREG|NBSL, RESC1,
		"\tD =s sub AL, AR\n", },

{ MINUS, INBREG,
	SBREG, TDOUBLE,
	SBREG, TDOUBLE,
		NBREG|NBSL, RESC1,
		"\tD =d sub AL, AR\n", },

/* Integer multiplication */
{ MUL, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l mul AL, AR\n", },

{ MUL, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w mul AL, AR\n", },

/* Floating point multiplication */
{ MUL, INBREG,
	SBREG, TFLOAT,
	SBREG, TFLOAT,
		NBREG|NBSL, RESC1,
		"\tD =s mul AL, AR\n", },

{ MUL, INBREG,
	SBREG, TDOUBLE,
	SBREG, TDOUBLE,
		NBREG|NBSL, RESC1,
		"\tD =d mul AL, AR\n", },

/* Integer division */
{ DIV, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l div AL, AR\n", },

{ DIV, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w div AL, AR\n", },

/* Unsigned division */
{ DIV, INAREG,
	SAREG, TULONG|TULONGLONG,
	SAREG, TULONG|TULONGLONG,
		NAREG|NASL, RESC1,
		"\tD =l udiv AL, AR\n", },

{ DIV, INAREG,
	SAREG, TUWORD,
	SAREG, TUWORD,
		NAREG|NASL, RESC1,
		"\tD =w udiv AL, AR\n", },

/* Floating point division */
{ DIV, INBREG,
	SBREG, TFLOAT,
	SBREG, TFLOAT,
		NBREG|NBSL, RESC1,
		"\tD =s div AL, AR\n", },

{ DIV, INBREG,
	SBREG, TDOUBLE,
	SBREG, TDOUBLE,
		NBREG|NBSL, RESC1,
		"\tD =d div AL, AR\n", },

/* Modulo (remainder) */
{ MOD, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l rem AL, AR\n", },

{ MOD, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w rem AL, AR\n", },

/* Unsigned modulo */
{ MOD, INAREG,
	SAREG, TULONG|TULONGLONG,
	SAREG, TULONG|TULONGLONG,
		NAREG|NASL, RESC1,
		"\tD =l urem AL, AR\n", },

{ MOD, INAREG,
	SAREG, TUWORD,
	SAREG, TUWORD,
		NAREG|NASL, RESC1,
		"\tD =w urem AL, AR\n", },

/*
 * Bitwise operations
 */

/* AND */
{ AND, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l and AL, AR\n", },

{ AND, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w and AL, AR\n", },

/* OR */
{ OR, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l or AL, AR\n", },

{ OR, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w or AL, AR\n", },

/* XOR */
{ ER, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		NAREG|NASL, RESC1,
		"\tD =l xor AL, AR\n", },

{ ER, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w xor AL, AR\n", },

/*
 * Shift operations
 */

/* Left shift */
{ LS, INAREG,
	SAREG, TLL,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =l shl AL, AR\n", },

{ LS, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w shl AL, AR\n", },

/* Right shift (arithmetic) */
{ RS, INAREG,
	SAREG, TLL,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =l sar AL, AR\n", },

{ RS, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w sar AL, AR\n", },

/* Right shift (logical) */
{ RS, INAREG,
	SAREG, TULONG|TULONGLONG,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =l shr AL, AR\n", },

{ RS, INAREG,
	SAREG, TUWORD,
	SAREG, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w shr AL, AR\n", },

/*
 * Unary operations
 */

/* Complement */
{ COMPL, INAREG,
	SAREG, TLL,
	SANY, TLL,
		NAREG|NASL, RESC1,
		"\tD =l xor AL, -1\n", },

{ COMPL, INAREG,
	SAREG, TWORD,
	SANY, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w xor AL, -1\n", },

/* Unary minus */
{ UMINUS, INAREG,
	SAREG, TLL,
	SANY, TLL,
		NAREG|NASL, RESC1,
		"\tD =l sub 0, AL\n", },

{ UMINUS, INAREG,
	SAREG, TWORD,
	SANY, TWORD,
		NAREG|NASL, RESC1,
		"\tD =w sub 0, AL\n", },

{ UMINUS, INBREG,
	SBREG, TFLOAT,
	SANY, TFLOAT,
		NBREG|NBSL, RESC1,
		"\tD =s neg AL\n", },

{ UMINUS, INBREG,
	SBREG, TDOUBLE,
	SANY, TDOUBLE,
		NBREG|NBSL, RESC1,
		"\tD =d neg AL\n", },

/*
 * Memory operations
 */

/* Load from memory */
{ OPLTYPE, INAREG,
	SANY, TANY,
	SOREG, TLL,
		NAREG, RESC1,
		"\tD =l loadl AL\n", },

{ OPLTYPE, INAREG,
	SANY, TANY,
	SOREG, TWORD,
		NAREG, RESC1,
		"\tD =w loadw AL\n", },

{ OPLTYPE, INBREG,
	SANY, TANY,
	SOREG, TFLOAT,
		NBREG, RESC1,
		"\tD =s loads AL\n", },

{ OPLTYPE, INBREG,
	SANY, TANY,
	SOREG, TDOUBLE,
		NBREG, RESC1,
		"\tD =d loadd AL\n", },

/* Assignment */
{ ASSIGN, FOREFF,
	SOREG, TLL,
	SAREG, TLL,
		0, 0,
		"\tstorel AR, AL\n", },

{ ASSIGN, FOREFF,
	SOREG, TWORD,
	SAREG, TWORD,
		0, 0,
		"\tstorew AR, AL\n", },

{ ASSIGN, FOREFF,
	SOREG, TFLOAT,
	SBREG, TFLOAT,
		0, 0,
		"\tstores AR, AL\n", },

{ ASSIGN, FOREFF,
	SOREG, TDOUBLE,
	SBREG, TDOUBLE,
		0, 0,
		"\tstored AR, AL\n", },

/* Register assignment */
{ ASSIGN, INAREG,
	SAREG, TLL,
	SAREG, TLL,
		0, RDEST,
		"\tD =l copy AR\n", },

{ ASSIGN, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RDEST,
		"\tD =w copy AR\n", },

{ ASSIGN, INBREG,
	SBREG, TFLOAT,
	SBREG, TFLOAT,
		0, RDEST,
		"\tD =s copy AR\n", },

{ ASSIGN, INBREG,
	SBREG, TDOUBLE,
	SBREG, TDOUBLE,
		0, RDEST,
		"\tD =d copy AR\n", },

/*
 * Comparisons
 */

/* Integer comparisons */
{ EQ, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w ceql AL, AR\n", },

{ EQ, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w ceqw AL, AR\n", },

{ NE, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w cnel AL, AR\n", },

{ NE, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w cnew AL, AR\n", },

{ LT, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w csltl AL, AR\n", },

{ LT, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w csltw AL, AR\n", },

{ LE, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w cslel AL, AR\n", },

{ LE, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w cslew AL, AR\n", },

{ GT, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w csgtl AL, AR\n", },

{ GT, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w csgtw AL, AR\n", },

{ GE, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w csgel AL, AR\n", },

{ GE, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w csgew AL, AR\n", },

/* Unsigned comparisons */
{ ULT, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w cultl AL, AR\n", },

{ ULT, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w cultw AL, AR\n", },

{ ULE, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w culel AL, AR\n", },

{ ULE, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w culew AL, AR\n", },

{ UGT, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w cugtl AL, AR\n", },

{ UGT, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w cugtw AL, AR\n", },

{ UGE, FORCC,
	SAREG, TLL,
	SAREG, TLL,
		0, RESCC,
		"\tD =w cugel AL, AR\n", },

{ UGE, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RESCC,
		"\tD =w cugew AL, AR\n", },

/*
 * Control flow
 */

/* Branch */
{ GOTO, FOREFF,
	SCON, TANY,
	SANY, TANY,
		0, RNOP,
		"\tjmp ZA\n", },

/* Return */
{ RETURN, FOREFF,
	SAREG, TLL,
	SANY, TLL,
		0, 0,
		"\tret AL\n", },

{ RETURN, FOREFF,
	SAREG, TWORD,
	SANY, TWORD,
		0, 0,
		"\tret AL\n", },

{ RETURN, FOREFF,
	SBREG, TFLOAT,
	SANY, TFLOAT,
		0, 0,
		"\tret AL\n", },

{ RETURN, FOREFF,
	SBREG, TDOUBLE,
	SANY, TDOUBLE,
		0, 0,
		"\tret AL\n", },

/* Function calls */
{ CALL, INAREG,
	SCON, TANY,
	SANY, TLL,
		NAREG, RESC1,
		"\tD =l call $AL(ZC)\n", },

{ CALL, INAREG,
	SCON, TANY,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w call $AL(ZC)\n", },

{ CALL, INBREG,
	SCON, TANY,
	SANY, TFLOAT,
		NBREG, RESC1,
		"\tD =s call $AL(ZC)\n", },

{ CALL, INBREG,
	SCON, TANY,
	SANY, TDOUBLE,
		NBREG, RESC1,
		"\tD =d call $AL(ZC)\n", },

{ UCALL, INAREG,
	SCON, TANY,
	SANY, TLL,
		NAREG, RESC1,
		"\tD =l call $AL()\n", },

{ UCALL, INAREG,
	SCON, TANY,
	SANY, TWORD,
		NAREG, RESC1,
		"\tD =w call $AL()\n", },

/* Free table entry */
{ FREE, FREE, FREE, FREE, FREE, FREE, FREE, FREE, "", },

};

int tablesize = sizeof(table)/sizeof(table[0]);
