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
		0,	RLEFT,
		"	lsh AL,2\n", },

/*
 * Convert int/long to pointers.
 */
/* Convert int to char pointer */
{ PCONV,	INTAREG,
	STAREG,	TWORD,
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
		0,	RLEFT,
		"	lsh AL,-2\n", },

/*
 * Pointer to pointer conversions.
 */
/* Convert char ptr to short ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TCHAR|TUCHAR,
	SANY,	TPTRTO|TSHORT|TUSHORT,
		0,	RLEFT,
		"	tlo AL,050000\n"
		"	tlne AL,020000\n"
		"	tlz AL,010000\n", },

/* Convert char/short pointer to int/struct/multiple ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SANY,	TPOINT|TWORD|TSTRUCT,
		0,	RLEFT,
		"	tlz AL,0770000\n", },

/* Convert short pointer to char ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TSHORT|TUSHORT,
	SANY,	TPTRTO|TCHAR|TUCHAR,
		0,	RLEFT,
		"	tlz AL,050000\n", },

/* Convert int/struct/foo pointer to char ptr */
{ PCONV,	INTAREG,
	STAREG,	TPOINT|TWORD|TSTRUCT,
	SANY,	TPTRTO|TCHAR|TUCHAR,
		0,	RLEFT,
		"	tlo AL,0700000\n", },

/* Convert int/struct/foo pointer to short ptr */
{ PCONV,	INTAREG,
	STAREG,	TPTRTO|TWORD|TSTRUCT,
	SANY,	TPTRTO|TSHORT|TUSHORT,
		0,	RLEFT,
		"	tlo AL,0750000\n", },

/*
 * A bunch conversions of integral<->integral types
 */

/* convert short/char to int. This is done when register is loaded */
{ SCONV,	INTAREG,
	STAREG,	TSHORT|TUSHORT|TCHAR|TUCHAR|TWORD,
	SANY,	TWORD,
		0,	RLEFT,
		"", },

/* convert int to short/char. This is done when register is loaded */
{ SCONV,	INTAREG,
	STAREG,	TWORD,
	SANY,	TSHORT|TUSHORT|TCHAR|TUCHAR|TWORD,
		0,	RLEFT,
		"", },

/* convert int/long to unsigned long long */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TULONGLONG,
		NAREG|NASL,	RESC1|RESC2,
		"	move U1,AL\n"
		"	setz A1,\n", },

/* convert int/long to long long */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TLONGLONG,
		NAREG|NASL,	RESC1|RESC2,
		"	move U1,AL\n"
		"	move A1,U1\n"
		"	ash A1,-043\n", },

/* convert long long to int/long */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TWORD,
		NAREG|NASL,	RESC1,
		"	move A1,UL\n", },

/* convert long long to unsigned char - XXX - signed char */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	move A1,UL\n"
		"	andi A1,0777\n", },

/* convert long long to short - XXX - signed short */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	move A1,UL\n"
		"	hrrz A1,A1\n", },

/* floating point conversions */
{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE|TFLOAT,
	SANY,	TWORD,
		NAREG|NASL,	RESC1,
		"	fix A1,AL\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TFLOAT,
		NAREG|NASL,	RESC1,
		"	fltr A1,AL\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TDOUBLE,
		NAREG|NASL,	RESC1,
		"	fltr A1,AL\n	setz U1,\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE,
	SANY,	TFLOAT,
		NAREG|NASL,	RESC1,
		"	move A1,AL\n", },

{ SCONV,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TFLOAT,
	SANY,	TDOUBLE,
		NAREG|NASL,	RESC1,
		"	move A1,AL\n	setz U1,\n", },

/*
 * Store constant initializers.
 */
{ INIT, FOREFF,
	SCON,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SANY,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0,	RNOP,
		"	.long Zd\n", },
{ INIT,	FOREFF,
	SCON,	TANY,
	SANY,	TWORD|TPOINT,
		0,	RNOP,
		"	.long CL\n", },

{ INIT,	FOREFF,
	SCON,	TANY,
	SANY,	TLL,
		0,	RNOP,
		"	.long UL\n	.long CL\n", },

/*
 * Subroutine calls.
 */

{ UNARY CALL,	INTAREG,
	SCON,	TANY,
	SANY,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT|TFLOAT|TDOUBLE|TLL|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	pushj 017,AL\n", },

{ UNARY CALL,	INTAREG,
	SAREG|STAREG,	TANY,
	SANY,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT|TFLOAT|TDOUBLE|TLL|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	pushj 017,(AL)\n", },

{ UNARY CALL,	INTAREG,
	SNAME|SOREG,	TANY,
	SANY,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT|TFLOAT|TDOUBLE|TLL|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	pushj 017,@AL\n", },

/*
 * The next rules handle all "+="-style operators.
 */
{ ASG ER,	INAREG|FOREFF,
	STAREG|SAREG,	TWORD,
	SCON,		TWORD,
		0,	RLEFT,
		"ZS", },

{ ASG ER,	INAREG|FOREFF,
	STAREG|SAREG,	TLL,
	SCON,		TANY,
		0,	RLEFT,
		"Zf", },

{ ASG ER,	INAREG|FOREFF,
	STAREG|SAREG,	TLL,
	STAREG|SAREG,	TLL,
		0,	RLEFT,
		"	xor AL,AR\n"
		"	xor UL,UR\n", },

{ ASG PLUS,	FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG,		TWORD,
		0,	0,
		"	addm AR,AL\n", },

{ ASG OPSIMP,	INAREG|FOREFF,
	SAREG|STAREG,		TWORD|TFLOAT,
	SAREG|STAREG|SNAME|SOREG,	TWORD|TFLOAT,
		0,	RLEFT,
		"	OR AL,AR\n", },

{ ASG MINUS,	INAREG|FOREFF,
	SAREG|STAREG,		TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	dsub AL,AR\n", },

{ ASG PLUS,	INAREG|FOREFF,
	SAREG|STAREG,		TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	dadd AL,AR\n", },

{ ASG PLUS,	INAREG|FOREFF,
	SAREG|STAREG,		TDOUBLE,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	dfad AL,AR\n", },

{ ASG OPSIMP,	INAREG|FOREFF,
	STAREG|SAREG,	TWORD,
	SCON,		TWORD,
		0,	RLEFT,
		"	ZF AL,ZG\n", },

{ ASG PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SONE,	TANY,
		0,	RLEFT,
		"	ibp AL\n", },

/* Add to char/short pointer. XXX - should be able to remove the movem */
{ ASG PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,			TWORD,
		0,	RRIGHT,
		"	adjbp AR,AL\n"
		"	movem AR,AL\n", },

/* Add to char/short pointer. XXX - should be able to remove the movem */
{ ASG PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG|SOREG|SNAME,	TWORD,
		NAREG,	RESC1,
		"	move A1,AR\n"
		"	adjbp A1,AL\n", },

/* Sub from char/short pointer. XXX - subject to fix */
{ ASG MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,			TWORD,
		0,	RRIGHT,
		"	movn AR,AR\n"
		"	adjbp AR,AL\n"
		"	movem AR,AL\n", },

/* Sub from char/short pointer. XXX - subject to fix */
{ MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG|SOREG|SNAME,	TWORD,
		NAREG,	RESC1,
		"	movn A1,AR\n"
		"	adjbp A1,AL\n", },

/* Sub from char/short pointer. */
{ MINUS,	INTAREG,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SSCON,		TWORD,
		NAREG,	RESC1,
		"	movni A1,AR\n"
		"	adjbp A1,AL\n", },

/* Sub from char/short pointer. XXX - subject to fix */
{ ASG MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SSCON,		TWORD,
		NAREG,	RESC1,
		"	movni A1,AR\n"
		"	adjbp A1,AL\n"
		"	movem A1,AL\n", },

/* Sub from char/short pointer. XXX - subject to fix */
{ ASG MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SOREG|SNAME,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SCON,		TWORD,
		NAREG,	RESC1,
		"	movn A1,[ .long AR ]\n"
		"	adjbp A1,AL\n"
		"	movem A1,AL\n", },

{ ASG PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR,
	SCON,		TWORD,
		0,	RLEFT,
		"ZX", },

{ ASG PLUS,	INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SCON,		TPTRTO|TSTRUCT,
		0,	RLEFT,
		"	add AL,[ .long AR ]\n", },

{ ASG PLUS,     INAREG|FOREFF,
	SAREG|STAREG,			TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	add AL,AR\n", },

{ ASG MINUS,     INAREG|INTAREG|FOREFF,
	SAREG|STAREG,			TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	sub AL,AR\n", },

{ ASG MINUS,     INAREG|INTAREG|FOREFF,
	SAREG|STAREG,			TDOUBLE,
	SAREG|STAREG|SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	dfsb AL,AR\n", },

/* Add a value to a char/short pointer */
{ PLUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		NAREG,	RESC1,
		"	move A1,AR\n"
		"	adjbp A1,AL\n", },

{ PLUS,	INTAREG|FOREFF,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR,
	SCON,		TWORD,
		NAREG,	RESC1,
		"ZY", },

{ ASG OPSIMP,	INAREG|FOREFF,
	STAREG|SAREG,	TWORD|TPOINT,
	SCON,		TWORD|TPOINT,
		0,	RLEFT,
		"	ZF AL,ZG\n", },

{ ASG AND,	INAREG|FOREFF,
	SAREG|STAREG,			TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	and AL,AR\n"
		"	and UL,UR\n", },

{ PLUS,	INTAREG,
	STAREG,		TFLOAT,
	SAREG|STAREG|SNAME|SOREG,	TFLOAT,
		0,	RLEFT,
		"	fadr AL,AR\n", },

/*
 * The next rules handle all shift operators.
 */
{ ASG LS,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SAREG|STAREG,	TWORD,
		0,	RLEFT,
		"	lsh AL,(AR)\n", },

{ ASG LS,	INTAREG|INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SNAME|SOREG,	TWORD,
		0,	RLEFT,
		"	OR AL,@AR\n", },

{ ASG LS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TWORD,
	SCON,		TWORD,
		0,	RLEFT,
		"	ZF AL,ZH\n", },

{ ASG RS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TSWORD,
	SCON,		TWORD,
		0,	RLEFT,
		"	ash AL,-ZH\n", },

{ ASG RS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TUWORD,
	SCON,		TWORD,
		0,	RLEFT,
		"	lsh AL,-ZH\n", },

{ ASG RS,       INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TULONGLONG,
	SCON,		TANY,
		0,	RLEFT,
		"	lshc AL,-ZH\n", },

{ ASG LS,       INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TLL,
	SCON,		TANY,
		0,	RLEFT,
		"	lshc AL,ZH\n", },

{ ASG LS,       INTAREG|INAREG|FOREFF,
	STAREG|SAREG,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TANY,
		0,	RLEFT,
		"	lshc AL,@AR\n", },

/*
 * The next rules takes care of assignments. "=".
 */
{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SZERO,	TANY,
		0,	0,
		"	setzm AL\n", },

{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SMONE,	TANY,
		0,	0,
		"	setom AL\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	STAREG|SAREG,		TWORD,
	SCON,		TWORD,
		0,	RLEFT,
		"	ZC\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT|TFLOAT,
	SAREG|STAREG,		TWORD|TPOINT|TFLOAT,
		0,	RRIGHT,
		"	movem AR,AL\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG,			TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	move AL,AR\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SAREG|SNAME|SOREG,	TLL|TDOUBLE,
	SAREG|STAREG,		TLL|TDOUBLE,
		0,	RRIGHT,
		"	dmovem AR,AL\n", },

{ ASSIGN,	INAREG|INTAREG|FOREFF,
	SOREG|SNAME,	TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG|STAREG,	TANY,
		0,	RRIGHT,
		"ZV", },

/*
 * DIV/MUL 
 * These can be done way more efficient.
 */
{ ASG DIV,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		2*NAREG,	RLEFT,
		"	dmove Z1,AL ; dmove A1,[ .long 0,0 ]\n"
		"	ddiv A1,AR\n"
		"	dmovem A1,AL\n", },

{ ASG MOD,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		2*NAREG,	RLEFT,
		"	dmove Z1,AL ; dmove A1,[ .long 0,0 ]\n"
		"	ddiv A1,AR\n"
		"	dmovem Z1,AL\n", },

{ ASG MUL,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SAREG|STAREG|SNAME|SOREG,	TLL,
		2*NAREG,	RLEFT,
		"	dmove A1,AL\n"
		"	dmul A1,AR\n"
		"	dmovem Z1,AL\n", },

{ ASG DIV,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SCON,		TWORD,
		2*NAREG,	RLEFT,
		"	move A1,AL\n"
		"	setz U1,\n"
		"Zb"
		"	movem A1,AL\n", },

{ ASG DIV,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		2*NAREG,	RLEFT,
		"	move A1,AL\n"
		"	setz U1,\n"
		"	idiv A1,AR\n"
		"	movem A1,AL\n", },

{ DIV,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SCON,		TWORD,
		2*NAREG,	RESC1,
		"	move A1,AL\n"
		"	setz U1,\n"
		"Zb", },

{ DIV,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		2*NAREG,	RESC1,
		"	move A1,AL\n"
		"	setz U1,\n"
		"	idiv A1,AR\n", },

{ ASG MOD,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SCON,		TWORD,
		2*NAREG,	RLEFT,
		"	move A1,AL\n"
		"	setz U1,\n"
		"Zb"
		"	movem U1,AL\n", },

{ ASG MOD,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		2*NAREG,	RLEFT,
		"	move A1,AL\n"
		"	setz U1,\n"
		"	idiv A1,AR\n"
		"	movem U1,AL\n", },

{ MOD,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SCON,		TWORD,
		2*NAREG,	RESC2,
		"	move A1,AL\n"
		"	setz U1,\n"
		"Zb", },

{ MOD,	INAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		2*NAREG,	RESC2,
		"	move A1,AL\n"
		"	setz U1,\n"
		"	idiv A1,AR\n", },

{ ASG MUL,	INAREG|FOREFF,
	SAREG|STAREG,	TWORD,
	SCON,		TWORD,
		0,	RLEFT,
		"Za", },

{ ASG MUL,	INAREG|FOREFF,
	SAREG|STAREG,			TWORD,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
		0,	RLEFT,
		"	imul AL,AR\n", },

/*
 * dummy UNARY MUL entry to get U* to possibly match OPLTYPE
 */
{ UNARY MUL,	FOREFF,
	SCC,	TANY,
	SCC,	TANY,
		0,	RNULL,
		"	HELP HELP HELP\n", },

{ REG,	INTEMP,
	SANY,	TANY,
	SAREG,	TDOUBLE|TLL,
		2*NTEMP,	RESC1,
		"	dmovem AR,A1\n", },

{ REG,	INTEMP,
	SANY,	TANY,
	SAREG,	TANY,
		NTEMP,	RESC1,
		"	movem AR,A1\n", },

/* Match char/short pointers first, requires special handling */
{ OPLOG,	FORCC,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
	SAREG|STAREG,	TPTRTO|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0, 	RESCC,
		"ZZ", },

/* Can check anything by just comparing if EQ/NE */
{ OPLOG,	FORCC,
	SAREG|STAREG,	TSWORD|TPOINT,
	SZERO,	TSWORD|TPOINT,
		0, 	RESCC,
		"	jumpZe AL,LC # bu\n", },

{ EQ,		FORCC,
	SAREG|STAREG,	TWORD|TPOINT,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
		0, 	RESCC,
		"ZR", },

{ NE,		FORCC,
	SAREG|STAREG,	TWORD|TPOINT,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
		0, 	RESCC,
		"ZR", },

{ OPLOG,	FORCC,
	SAREG|STAREG,	TWORD,
	SAREG|STAREG|SOREG|SNAME|SCON,	TSWORD,
		0, 	RESCC,
		"ZR", },

{ OPLOG,	FORCC,
	SAREG|STAREG,	TWORD|TPOINT|TFLOAT,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT|TFLOAT,
		0, 	RESCC,
		"ZR", },

{ OPLOG,	FORCC,  
	SAREG|STAREG,	TLL|TDOUBLE, /* XXX - does double work here? */
	SAREG|STAREG|SOREG|SNAME,	TLL|TDOUBLE,
		0,	RESCC,
		"ZQ", },

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jrst LL\n", },

/*
 * Convert LTYPE to reg.
 */
{ OPLTYPE,	INAREG|INTAREG,
	SANY,	ANYFIXED,
	SMONE,	TANY,
		NAREG,	RESC1,
		"	seto A1,\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	ANYFIXED,
	SZERO,	TANY,
		NAREG,	RESC1,
		"	setz A1,\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	ANYFIXED,
	SCON,	ANYFIXED,
		NAREG|NASR,	RESC1,
		"	ZD A1,ZE	# suspekt\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TWORD|TPOINT|TFLOAT,
	SAREG|STAREG|SOREG|SNAME,	TWORD|TPOINT|TFLOAT,
		NAREG|NASR,	RESC1,
		"	move A1,AR\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TLL,
	SCON,	TLL,
		NAREG,	RESC1,
		"	dmove A1,ZO\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TLL|TDOUBLE,
	SANY,	TLL|TDOUBLE,
		NAREG|NASR,	RESC1,
		"	dmove A1,AR\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SOREG,		TSHORT|TUSHORT|TCHAR|TUCHAR,
	SANY,		TANY,
		NAREG|NASR,	RESC1,
		"ZU", },

{ OPLTYPE,	INAREG|INTAREG,
	SNAME,	TUCHAR|TUSHORT,
	SANY,	TANY,
		NAREG|NASR,	RESC1,
		"	ldb A1,[ .long AL ]\n" },

{ OPLTYPE,	INAREG|INTAREG,
	SNAME,	TCHAR,
	SANY,	TANY,
		NAREG|NASR,	RESC1,
		"	ldb A1,[ .long AL ]\n"
		"	ash A1,033\n"
		"	ash A1,-033\n", },
		
{ OPLTYPE,	INAREG|INTAREG,
	SNAME,	TSHORT,
	SANY,	TANY,
		NAREG|NASR,	RESC1,
		"	ldb A1,[ .long AL ]\n"
		"	ash A1,022\n"
		"	ash A1,-022\n", },

{ OPLTYPE,	INAREG|INTAREG,
	SANY,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		NAREG|NASR,	RESC1,
		"Zc", },

/*
 * Negate a word.
 */
{ UNARY MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TWORD,
		NAREG|NASR,	RESC1,
		"	movn A1,AL\n", },

{ UNARY MINUS,	INAREG|INTAREG|FOREFF,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TLL,
		NAREG|NASR,	RESC1,
		"	dmovn A1,AL\n", },

{ COMPL,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TLL,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	setcm A1,AL\n"
		"	setcm U1,UL\n", },

{ COMPL,	INTAREG,
	SAREG|STAREG|SNAME|SOREG,	TWORD,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	setcm A1,AL\n", },

{ COMPL,	INTAREG,
	SAREG|STAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT,
	SANY,	TCHAR|TUCHAR|TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	setcm A1,AL\n", },

/*
 * Arguments to functions.
 * These three should be possible to convert to one!
 */
{ REG,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD|TPOINT|TFLOAT,
		0,	RNULL,
		"	push 017,AR\n", },

{ OREG,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD,
		0,	RNULL,
		"	push 017,AR\n", },

{ NAME,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD,
		0,	RNULL,
		"	push 017,AR\n", },

{ ICON,	FORARG,
	SANY,	TANY,
	SCON,	TCHAR|TUCHAR|TPTRTO,
		0,	RNULL,
		"	push 017,[ .long AR]\n", },

{ ICON,	FORARG,
	SANY,	TANY,
	SCON,	TSHORT|TUSHORT|TPTRTO,
		0,	RNULL,
		"	push 017,[ .long AR]\n", },

{ ICON,	FORARG,
	SANY,	TANY,
	SCON,	TWORD,
		0,	RNULL,
		"	push 017,[ .long AR]\n", },

{ REG,	FORARG,
	SANY,		TANY,
	SAREG|STAREG,	TLL|TDOUBLE,
		0,	RNULL,
		"	push 017,AR\n	push 017,UR\n", },


# define DF(x) FORREW,SANY,TANY,SANY,TANY,REWRITE,x,""

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
		REWRITE,	BITYPE,
		"", },

{ INIT, DF(INIT), },

{ OPUNARY, DF(UNARY MINUS), },

{ ASG OPANY, DF(ASG PLUS), },

{ OPANY, DF(BITYPE), },

{ FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	"help; I'm in trouble\n" },
};
