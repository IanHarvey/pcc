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
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
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


# include "pass2.h"

# define TLL TLONGLONG|TULONGLONG
# define ANYSIGNED TINT|TLONG|TSHORT|TCHAR
# define ANYUSIGNED TUNSIGNED|TULONG|TUSHORT|TUCHAR
# define ANYFIXED ANYSIGNED|ANYUSIGNED
# define TUWORD TUNSIGNED|TULONG
# define TSWORD TINT|TLONG
# define TWORD TUWORD|TSWORD

struct optab table[] = {

/*
 * A bunch of pointer conversions.
 * First pointer to integer.
 */
/* Convert char pointer to int */
{ SCONV,	INTAREG,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR,
	SANY,	TWORD,
	SANY,	TWORD,
		NAREG,	RLEFT,
		"	lsh AL,2\n"
		"	move A1,AL\n"
		"	lsh A1,-040\n"
		"	trz A1,074\n"
		"	ior AL,A1\n"
		"	tlz AL,0740000\n", },

/* Convert short pointer to int */
{ SCONV,	INTAREG,
	SAREG|STAREG,	TPTRTO|TSHORT|TUSHORT,
	SANY,	TWORD,
	SANY,	TWORD,
		NAREG,	RLEFT,
		"	lsh AL,2\n"
		"	move A1,AL\n"
		"	lsh A1,-041\n"
		"	trz A1,2\n"
		"	ior AL,A1\n"
		"	tlz AL,0740000\n", },

/* Convert int/unsigned/long/ulong/struct/union/func ptr to int */
{ SCONV,	INTAREG,
	SAREG|STAREG,	TPTRTO|TWORD|TSTRUCT|TPOINT,
	SANY,		TWORD,
	SANY,		TWORD,
		0,	RLEFT,
		"	lsh AL,2\n", },

/*
 * Convert int/long to pointers.
 */
/* Convert int to char pointer */
{ PCONV,	INTAREG,
	STAREG,	TWORD,
	SANY,	TPTRTO|TCHAR|TUCHAR,
	SANY,	TPTRTO|TCHAR|TUCHAR,
		NAREG,	RLEFT,
		"	move A1,AL\n"
		"	lsh A1,036\n"
		"	tlo A1,0700000\n"
		"	tlz A1,0040000\n"
		"	lsh AL,-2\n"
		"	ior AL,A1\n", },

/* Convert int/long to short pointer */
{ PCONV,	INTAREG,
	STAREG,	TWORD,
	SANY,	TPTRTO|TSHORT|TUSHORT,
	STAREG,	TWORD,
		NAREG,	RLEFT,
		"	move A1,AL\n"
		"	lsh AL,-2\n"
		"	tlo AL,0750000\n"
		"	lsh A1,035\n"
		"	tlz A1,0760000\n"
		"	add AL,A1\n", },

/* Convert int/long to int/struct/multiple ptr */
{ PCONV,	INTAREG,
	STAREG,	TWORD,
	SANY,	TPOINT|TWORD|TSTRUCT,
	STAREG,	TWORD,
		0,	RLEFT,
		"	lsh AL,-2\n", },

/*
 * Pointer to pointer conversions.
 */
/* Convert char ptr to short ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TCHAR|TUCHAR,
	SANY,	TPTRTO|TSHORT|TUSHORT,
	STAREG,	TPTRTO|TCHAR|TUCHAR,
		0,	RLEFT,
		"	tlo AL,050000\n"
		"	tlne AL,020000\n"
		"	tlz AL,010000\n", },

/* Convert char/short pointer to int/struct/multiple ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SANY,	TPOINT|TWORD|TSTRUCT,
	STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0,	RLEFT,
		"	tlz AL,0770000\n", },

/* Convert short pointer to char ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TSHORT|TUSHORT,
	SANY,	TPTRTO|TCHAR|TUCHAR,
	STAREG,	TPTRTO|TSHORT|TUSHORT,
		0,	RLEFT,
		"	tlz AL,050000\n", },

/* Convert int/struct/foo pointer to char ptr */
{ PCONV,	INTAREG,
	STAREG,	TPOINT|TWORD|TSTRUCT,
	SANY,	TPTRTO|TCHAR|TUCHAR,
	STAREG,	TPOINT|TWORD|TSTRUCT,
		0,	RLEFT,
		"	tlo AL,0700000\n", },

/* Convert int/struct/foo pointer to short ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TWORD|TSTRUCT,
	SANY,	TPTRTO|TSHORT|TUSHORT,
	STAREG,	TPTRTO|TWORD|TSTRUCT,
		0,	RLEFT,
		"	tlo AL,0750000\n", },

/*
 * A bunch conversions of integral<->integral types
 */

/* convert short/char to int. This is done when register is loaded */
{ SCONV,	INTAREG,
	STAREG,	TSHORT|TUSHORT|TCHAR|TUCHAR|TWORD,
	SANY,	TWORD,
	STAREG,	TSHORT|TUSHORT|TCHAR|TUCHAR|TWORD,
		0,	RLEFT,
		"", },

/* convert int to short/char. This is done when register is loaded */
{ SCONV,	INTAREG,
	STAREG,	TWORD,
	SANY,	TSHORT|TUSHORT|TCHAR|TUCHAR|TWORD,
	STAREG,	TWORD,
		0,	RLEFT,
		"", },

/* convert int/long to unsigned long long */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TULONGLONG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		NAREG|NASL,	RESC1|RESC2,
		"	move U1,AL\n"
		"	setz A1,\n"
		"	tlze U1,0400000\n"
		"	tro A1,01\n" , },

/* convert int/long to long long */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TLONGLONG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		NAREG|NASL,	RESC1|RESC2,
		"	move U1,AL\n"
		"	move A1,U1\n"
		"	ash A1,-043\n", },

/* convert long long to int/long */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		NAREG|NASL,	RESC1,
		"	move A1,UL\n", },

/* convert long long to unsigned char - XXX - signed char */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TCHAR|TUCHAR,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		NAREG|NASL,	RESC1,
		"	move A1,UL\n"
		"	andi A1,0777\n", },

/* convert long long to short - XXX - signed short */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TSHORT|TUSHORT,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		NAREG|NASL,	RESC1,
		"	move A1,UL\n"
		"	hrrz A1,A1\n", },

/* floating point conversions */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE|TFLOAT,
	SANY,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE|TFLOAT,
		NAREG|NASL,	RESC1,
		"	fix A1,AL\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TFLOAT,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	fltr A1,AL\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TDOUBLE,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	fltr A1,AL\n	setz U1,\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE,
	SANY,	TFLOAT,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE,
		NAREG|NASL,	RESC1,
		"	move A1,AL\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TFLOAT,
	SANY,	TDOUBLE,
	SAREG|STAREG|SNAME|SOREG,	TFLOAT,
		NAREG|NASL,	RESC1,
		"	move A1,AL\n	setz U1,\n", },

/*
 * Store constant initializers.
 */
{ INIT, FOREFF,
	SCON,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SANY,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SCON,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0,	RNOP,
		"	.long Zd\n", },
{ INIT,	FOREFF,
	SCON,	TANY,
	SANY,	TWORD|TPOINT,
	SCON,	TANY,
		0,	RNOP,
		"	.long CL\n", },

{ INIT,	FOREFF,
	SCON,	TANY,
	SANY,	TLL,
	SCON,	TANY,
		0,	RNOP,
		"	.long UL\n	.long CL\n", },

/*
 * Subroutine calls.
 */

{ UNARY CALL,	INTAREG,
	SCON,	TANY,
	SANY,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT|TFLOAT|TDOUBLE|TLL|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	pushj 017,AL\n", },

{ UNARY CALL,	INTAREG,
	SAREG|STAREG,	TANY,
	SANY,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT|TFLOAT|TDOUBLE|TLL|TPOINT,
	SAREG|STAREG,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	pushj 017,(AL)\n", },

{ UNARY CALL,	INTAREG,
	SNAME|SOREG,	TANY,
	SANY,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT|TFLOAT|TDOUBLE|TLL|TPOINT,
	SNAME|SOREG,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	pushj 017,@AL\n", },

/*
 * INCR can be slightly optimized.
 */
{ INCR,		FOREFF,
	STAREG|SAREG|SNAME|SOREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TPTRTO,
	SONE,	TANY,
	STAREG|SAREG|SNAME|SOREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TPTRTO,
		0,	0,
		"	ibp AL\n", },

{ INCR,		INTAREG,
	STAREG|SAREG|SNAME|SOREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TPTRTO,
	SONE,	TANY,
	STAREG|SAREG|SNAME|SOREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TPTRTO,
		NAREG,	RESC1,
		"	move A1,AL\n"
		"	ibp AL\n", },

/*
 * PLUS operators.
 */
/* Add a value to a char/short pointer */
{ PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,			TWORD,
	0,	0,
		NDRIGHT,	RRIGHT,
		"	adjbp AR,AL\n", },

/* No more search for char/short pointer addition */
{ PLUS,	INAREG|INTAREG|FOREFF,
	SANY,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SANY,	TANY,
	0,	0,
		REWRITE, 0,
		"DIEDIEDIE!\n", },

/* Add char/short/int to memory */
{ PLUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG,			TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	addm AR,AL # foo \n", },

/* Add a small constant to a register */
{ PLUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD|TPOINT,
	SUSHCON,	TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	addi AL,AR\n", },

/* Add a larger constant to a register */
{ PLUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD|TPOINT,
	SCON,	TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	add AL,[ .long AR ]\n", },

/* Add long long to register */
{ PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,			TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		NDLEFT,	RLEFT,
		"	dadd AL,AR\n", },

/* Add int (or int pointer) to register */
{ PLUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	add AL,AR # foo \n", },

/* char/short are allowed to be added if they are in registers */
{ PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,
		NDLEFT,	RLEFT,
		"	add AL,AR\n", },

/* Safety belt for plus */
{ PLUS,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },

/*
 * MINUS operators.
 */
/* Rewrite subtracts from char/short pointers (to negative adds) */
{ MINUS,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TCHAR|TUCHAR|TSHORT|TUSHORT|TPTRTO,
	SANY,	TANY,
	0,	0,
		REWRITE,	BITYPE,
		"DIEDIEDIE", },

/* Subtract char/short/int word in memory from reg */
{ MINUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG,	TWORD|TPOINT,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	sub AL,AR\n", },

/* Subtract a small constant from reg */
{ MINUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TWORD|TPOINT,
	SUSHCON,	TWORD|TPOINT,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	subi AL,AR\n", },

/* Subtract a large constant from reg */
{ MINUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	sub AL,[ .long AR ]\n", },

/* Subtract char/short/int word in memory from reg, save in memory */
{ MINUS,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDRIGHT,	RRIGHT,
		"	subm AL,AR\n", },

/* Subtract long long from register */
{ MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,			TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		NDLEFT,	RLEFT,
		"	dsub AL,AR\n", },

/* char/short are allowed to be subtracted if they are in registers */
{ MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,
		NDLEFT,	RLEFT,
		"	sub AL,AR\n", },

/* Safety belt for plus */
{ MINUS,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },

/*
 * AND/OR/ER operators.
 * Simpler that the ops above in that they only work on integral types.
 */
/* And char/short/int with integer memory */
{ AND,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	and AL,AR\n", },

/* And char/short/int with register */
{ AND,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	and AL,AR\n", },

/* And char/short/int with small constant */
{ AND,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SUSHCON,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	andi AL,AR\n", },

/* And char/short/int with large constant */
{ AND,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SCON,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	and AL,[ .long AR ]\n", },

/* long long AND */
{ AND,	INAREG|FOREFF,
	SAREG|STAREG,			TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		NDLEFT,	RLEFT,
		"	and AL,AR\n"
		"	and UL,UR\n", },

/* Safety belt for AND */
{ AND,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },


/* OR char/short/int with integer memory */
{ OR,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	ior AL,AR\n", },

/* OR char/short/int with register */
{ OR,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	ior AL,AR\n", },

/* OR char/short/int with small constant */
{ OR,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SUSHCON,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	iori AL,AR\n", },

/* OR char/short/int with large constant */
{ OR,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SCON,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	ior AL,[ .long AR ]\n", },

/* long long OR */
{ OR,	INAREG|FOREFF,
	SAREG|STAREG,			TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		NDLEFT,	RLEFT,
		"	ior AL,AR\n"
		"	ior UL,UR\n", },

/* Safety belt for OR */
{ OR,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },


/* ER char/short/int with integer memory */
{ ER,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	xor AL,AR\n", },

/* ER char/short/int with register */
{ ER,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SAREG|STAREG,			TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	xor AL,AR\n", },

/* ER char/short/int with small constant */
{ ER,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SUSHCON,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	xori AL,AR\n", },

/* ER char/short/int with large constant */
{ ER,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SCON,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	0,	0,	/* Unneccessary if dest is left */
		NDLEFT,	RLEFT,
		"	xor AL,[ .long AR ]\n", },

/* long long ER */
{ ER,	INAREG|FOREFF,
	SAREG|STAREG,			TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		NDLEFT,	RLEFT,
		"	xor AL,AR\n"
		"	xor UL,UR\n", },

/* Safety belt for ER */
{ ER,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },

/*
 * The next rules handle all shift operators.
 */
{ ASG LS,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SAREG|STAREG,	TWORD,
	SAREG|STAREG,	TWORD,
		0,	RLEFT,
		"	lsh AL,(AR)\n", },

{ ASG LS,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SNAME|SOREG,	TWORD,
	SAREG|STAREG,	TWORD,
		0,	RLEFT,
		"	OR AL,@AR\n", },

{ ASG LS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TWORD,
	SCON,		TWORD,
	STAREG|SAREG,	TWORD,
		0,	RLEFT,
		"	ZF AL,ZH\n", },

{ ASG RS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TSWORD,
	SCON,		TWORD,
	STAREG|SAREG,	TSWORD,
		0,	RLEFT,
		"	ash AL,-ZH\n", },

{ ASG RS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TUWORD,
	SCON,		TWORD,
	STAREG|SAREG,	TUWORD,
		0,	RLEFT,
		"	lsh AL,-ZH\n", },

{ ASG RS,       INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TULONGLONG,
	SCON,		TANY,
	STAREG|SAREG,	TULONGLONG,
		0,	RLEFT,
		"	ashc AL,-ZH\n", },

{ ASG LS,       INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TLL,
	SCON,		TANY,
	STAREG|SAREG,	TLL,
		0,	RLEFT,
		"	ashc AL,ZH\n", },

{ ASG LS,       INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TLL,
	SAREG|STAREG /* |SNAME|SOREG */,	TANY,
	STAREG|SAREG,	TLL,
		0,	RLEFT,
		"	ashc AL,(AR)\n", },

/*
 * The next rules takes care of assignments. "=".
 */
{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SZERO,	TANY,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	0,
		"	setzm AL\n", },

{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SMONE,	TANY,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	0,
		"	setom AL\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	STAREG|SAREG,		TWORD|TPOINT,
	SCON,		TWORD|TPOINT,
	STAREG|SAREG,		TWORD|TPOINT,
		0,	RLEFT,
		"	ZC\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT|TFLOAT,
	SAREG|STAREG,		TWORD|TPOINT|TFLOAT,
	SAREG|SNAME|SOREG,	TWORD|TPOINT|TFLOAT,
		0,	RRIGHT,
		"	movem AR,AL\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,			TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG,	TWORD|TPOINT,
	SAREG|STAREG,			TWORD|TPOINT,
		0,	RLEFT,
		"	move AL,AR\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SAREG|SNAME|SOREG,	TLL|TDOUBLE,
	SAREG|STAREG,		TLL|TDOUBLE,
	SAREG|SNAME|SOREG,	TLL|TDOUBLE,
		0,	RRIGHT,
		"	dmovem AR,AL\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SOREG|SNAME,	TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG|STAREG,	TANY,
	SOREG|SNAME,	TSHORT|TUSHORT|TCHAR|TUCHAR,
		0,	RRIGHT,
		"ZV", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,	TUSHORT|TUCHAR,
	SOREG,		TANY,
	SAREG|STAREG,	TUSHORT|TUCHAR,
		0,	RLEFT,
		"	ldb AL,Zg\n", },

/*
 * DIV/MOD/MUL 
 * These can be done way more efficient.
 */
/* long long div. XXX - work only with unsigned */
{ DIV,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		(2*NAREG)|NASL,	RESC1,
		"	dmove A2,AL ; dmove A1,[ .long 0,0 ]\n"
		"	ddiv A1,AR\n", },

/* long long div. with constant. XXX - work only with unsigned */
{ DIV,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SCON,	TLL,
	0,	0,
		(2*NAREG)|NASL,	RESC1,
		"	dmove A2,AL ; dmove A1,[ .long 0,0 ]\n"
		"	ddiv A1,ZP\n", },

/* Simple divide. XXX - fix so next reg can be free */
{ DIV,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
	0,	0,
		NDRIGHT,	RRIGHT,
		"	idivm AL,AR\n", },

/* Safety belt for DIV */
{ DIV,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },

/* long long MOD */
{ MOD,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		2*NAREG|NASL,	RESC2,
		"	dmove A2,AL ; dmove A1,[ .long 0,0 ]\n"
		"	ddiv A1,AR\n", },

/* integer MOD */
{ MOD,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	0,	0,
		2*NAREG|NASL,	RESC2,
		"	move A2,AL\n"
		"	setz A1,\n"
		"	idiv A1,AR\n", },

/* integer MOD for char/short */
{ MOD,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
	0,	0,
		2*NAREG|NASL,	RESC2,
		"	move A2,AL\n"
		"	setz A1,\n"
		"	idiv A1,AR\n", },

/* Safety belt for MOD */
{ MOD,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },

/* long long MUL */
{ MUL,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	0,	0,
		2*NAREG|NASL,	RESC2,
		"	dmove A1,AL\n"
		"	dmul A1,AR\n", },

/* integer multiply to memory*/
{ MUL,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG,			TWORD,
	0,	0,
		NDLEFT,		RLEFT,
		"	imulm AR,AL\n", },

/* integer multiply */
{ MUL,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,			TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	0,	0,
		NDLEFT,		RLEFT,
		"	imul AL,AR\n", },

/* integer multiply for char/short */
{ MUL,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
	0,	0,
		NDLEFT,		RLEFT,
		"	imul AL,AR\n", },

/* integer multiply with small constant */
{ MUL,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SUSHCON,	TWORD,
	0,	0,
		NDLEFT,		RLEFT,
		"	imuli AL,AR\n", },

/* integer multiply with large constant */
{ MUL,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SCON,		TWORD,
	0,	0,
		NDLEFT,		RLEFT,
		"	imul AL,[ .long AR ]\n", },

/* Safety belt for MUL */
{ MUL,	FORREW|FOREFF|INAREG|INTAREG,
	SANY,	TANY,
	SANY,	TANY,
	0,	0,
		REWRITE,	0,
		"DIEDIEDIE", },

/*
 * dummy UNARY MUL entry to get U* to possibly match OPLTYPE
 */
{ UNARY MUL,	FOREFF,
	SCC,	TANY,
	SCC,	TANY,
	SCC,	TANY,
		0,	RNULL,
		"	HELP HELP HELP\n", },

{ REG,	INTEMP,
	SANY,	TANY,
	SAREG,	TDOUBLE|TLL,
	SANY,	TANY,
		2*NTEMP,	RESC1,
		"	dmovem AR,A1\n", },

{ REG,	INTEMP,
	SANY,	TANY,
	SAREG,	TANY,
	SANY,	TANY,
		NTEMP,	RESC1,
		"	movem AR,A1\n", },

/* Match char/short pointers first, requires special handling */
{ OPLOG,	FORCC,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0, 	RESCC,
		"ZZ", },

/* Can check anything by just comparing if EQ/NE */
{ OPLOG,	FORCC,
	SAREG|STAREG,	TSWORD|TPOINT,
	SZERO,	TSWORD|TPOINT,
	SAREG|STAREG,	TSWORD|TPOINT,
		0, 	RESCC,
		"	jumpZe AL,LC # bu\n", },

{ EQ,		FORCC,
	SAREG|STAREG,	TWORD|TPOINT,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
	SAREG|STAREG,	TWORD|TPOINT,
		0, 	RESCC,
		"ZR", },

{ NE,		FORCC,
	SAREG|STAREG,	TWORD|TPOINT,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
	SAREG|STAREG,	TWORD|TPOINT,
		0, 	RESCC,
		"ZR", },

{ OPLOG,	FORCC,
	SAREG|STAREG,	TWORD,
	SAREG|STAREG|SOREG|SNAME|SCON,	TSWORD,
	SAREG|STAREG,	TWORD,
		0, 	RESCC,
		"ZR", },

{ OPLOG,	FORCC,
	SAREG|STAREG,	TCHAR|TUCHAR,
	SCON,		TANY,
	SAREG|STAREG,	TCHAR|TUCHAR,
		0, 	RESCC,
		"ZR", },

{ OPLOG,	FORCC,
	SAREG|STAREG,	TWORD|TPOINT|TFLOAT,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT|TFLOAT,
	SAREG|STAREG,	TWORD|TPOINT|TFLOAT,
		0, 	RESCC,
		"ZR", },

{ OPLOG,	FORCC,  
	SAREG|STAREG,	TLL|TDOUBLE, /* XXX - does double work here? */
	SAREG|STAREG|SOREG|SNAME,	TLL|TDOUBLE,
	SAREG|STAREG,	TLL|TDOUBLE, /* XXX - does double work here? */
		0,	RESCC,
		"ZQ", },

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
	SCON,	TANY,
		0,	RNOP,
		"	jrst LL\n", },

/*
 * Convert LTYPE to reg.
 */
{ OPLTYPE,	INAREG|INTAREG,
	SANY,	ANYFIXED,
	SMONE,	TANY,
	SANY,	ANYFIXED,
		NAREG,	RESC1,
		"	seto A1,\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	ANYFIXED,
	SZERO,	TANY,
	SANY,	ANYFIXED,
		NAREG,	RESC1,
		"	setz A1,\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	ANYFIXED,
	SCON,	ANYFIXED,
	SANY,	ANYFIXED,
		NAREG|NASR,	RESC1,
		"	ZD A1,ZE	# suspekt\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TWORD|TPOINT|TFLOAT,
	SAREG|STAREG|SOREG|SNAME,	TWORD|TPOINT|TFLOAT,
	SANY,	TWORD|TPOINT|TFLOAT,
		NAREG|NASR,	RESC1,
		"	move A1,AR\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TLL,
	SCON,	TLL,
	SANY,	TLL,
		NAREG,	RESC1,
		"	dmove A1,ZO\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TLL|TDOUBLE,
	SANY,	TLL|TDOUBLE,
	SANY,	TLL|TDOUBLE,
		NAREG|NASR,	RESC1,
		"	dmove A1,AR\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SOREG,		TSHORT|TUSHORT|TCHAR|TUCHAR,
	SANY,		TANY,
	SOREG,		TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASR,	RESC1,
		"ZU", },

{ OPLTYPE,	INAREG|INTAREG,
	SNAME,	TUCHAR,
	SANY,	TANY,
	SNAME,	TUCHAR,
		NAREG|NASR,	RESC1,
		"	ldb A1,[ .long AL ]\n" },

{ OPLTYPE,	INAREG|INTAREG,
	SNAME,	TCHAR,
	SANY,	TANY,
	SNAME,	TCHAR,
		NAREG|NASR,	RESC1,
		"	ldb A1,[ .long AL ]\n"
		"	ash A1,033\n"
		"	ash A1,-033\n", },
		
{ OPLTYPE,	INAREG|INTAREG,
	SNAME,	TSHORT|TUSHORT,
	SANY,	TANY,
	0,	0,
		NDRIGHT|NAREG|NASR,	RESC1,
		"Zi", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
	SANY,	TWORD|TPOINT,
		NAREG|NASR,	RESC1,
		"Zc", },

/*
 * Negate a word.
 */
{ UNARY MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		NAREG|NASR,	RESC1,
		"	movn A1,AL\n", },

{ UNARY MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SANY,	TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TWORD,
		0,	RLEFT,
		"	movn AL,AL\n", },

{ UNARY MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		NAREG|NASR,	RESC1,
		"	dmovn A1,AL\n", },

{ COMPL,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TANY,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		NAREG|NASL,	RESC1,
		"	setcm A1,AL\n"
		"	setcm U1,UL\n", },

{ COMPL,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TANY,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	setcm A1,AL\n", },

{ COMPL,	INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT,
	SANY,	TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	setcm A1,AL\n", },

/*
 * Arguments to functions.
 * These three should be possible to convert to one!
 */
{ REG,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD|TPOINT|TFLOAT,
	SANY,	TANY,
		0,	RNULL,
		"	push 017,AR\n", },

{ OREG,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD,
	SANY,	TANY,
		0,	RNULL,
		"	push 017,AR\n", },

{ NAME,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD,
	SANY,	TANY,
		0,	RNULL,
		"	push 017,AR\n", },

{ ICON,	FORARG,
	SANY,	TANY,
	SCON,	TCHAR|TUCHAR|TPTRTO,
	SANY,	TANY,
		0,	RNULL,
		"	push 017,[ .long AR]\n", },

{ ICON,	FORARG,
	SANY,	TANY,
	SCON,	TSHORT|TUSHORT|TPTRTO,
	SANY,	TANY,
		0,	RNULL,
		"	push 017,[ .long AR]\n", },

{ ICON,	FORARG,
	SANY,	TANY,
	SCON,	TWORD,
	SANY,	TANY,
		0,	RNULL,
		"	push 017,[ .long AR]\n", },

{ REG,	FORARG,
	SANY,		TANY,
	SAREG|STAREG,	TLL|TDOUBLE,
	SANY,		TANY,
		0,	RNULL,
		"	push 017,AR\n	push 017,UR\n", },


# define DF(x) FORREW,SANY,TANY,SANY,TANY,SANY,TANY,REWRITE,x,""

{ UNARY MUL, DF( UNARY MUL ), },

{ INCR, DF(INCR), },

{ DECR, DF(INCR), },

{ ASSIGN, DF(ASSIGN), },

{ STASG, DF(STASG), },

{ FLD, DF(FLD), },

{ OPLEAF, DF(NAME), },

{ OPLOG,	FORCC,
	SANY,	TANY,
	SANY,	TANY,
	SANY,	TANY,
		REWRITE,	BITYPE,
		"", },

{ INIT, DF(INIT), },

{ OPUNARY, DF(UNARY MINUS), },

{ ASG OPANY, DF(ASG PLUS), },

{ OPANY, DF(BITYPE), },

{ FREE,	FREE, FREE, FREE, FREE,	FREE, FREE, FREE, FREE, FREE, "help; I'm in trouble\n" },
};
