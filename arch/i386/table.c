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
#define	 SHINT	SAREG	/* short and int */
#define	 ININT	INAREG
#define	 SHCH	SBREG	/* shape for char */
#define	 INCH	INBREG
#define	 SHLL	SCREG	/* shape for long long */
#define	 INLL	INCREG
#define	 SHFL	SDREG	/* shape for float/double */
#define	 INFL	INDREG	/* shape for float/double */

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

#ifdef MULTICLASS
#define	INTAREG	INAREG
#define	INTBREG	INBREG
#endif

/*
 * A bunch conversions of integral<->integral types
 */

/* convert (u)char to (u)char. */
{ SCONV,	INCH,
	SHCH,	TCHAR|TUCHAR,
	SHCH,	TCHAR|TUCHAR,
		0,	RLEFT,
		"", },

/* convert pointers to int. */
{ SCONV,	ININT,
	SHINT,	TPOINT|TWORD,
	SANY,	TWORD,
		0,	RLEFT,
		"", },

/* convert (u)longlong to (u)longlong. */
{ SCONV,	INLL,
	SHLL,	TLL,
	SHLL,	TLL,
		0,	RLEFT,
		"", },

/* convert int to char. This is done when register is loaded */
{ SCONV,	INCH,
	SAREG,	TWORD,
	SANY,	TCHAR|TUCHAR,
		NSPECIAL|NBREG,	RESC1,
		"ZM", },

/* convert short to char. This is done when register is loaded */
{ SCONV,	INCH,
	SAREG,	TSHORT|TUSHORT,
	SANY,	TCHAR|TUCHAR,
		0,	RLEFT,
		"", },

/* convert signed char to int (or pointer). */
{ SCONV,	ININT,
	SHCH|SOREG|SNAME,	TCHAR,
	SAREG,	TWORD|TPOINT,
		NASL|NAREG,	RESC1,
		"	movsbl AL,A1\n", },

/* convert char to short. */
{ SCONV,	ININT,
	SAREG,	TCHAR,
	SAREG,	TSHORT,
		NASL|NAREG,	RESC1,
		"	movsbw ZL,Z1\n", },

/* convert char to unsigned short. */
{ SCONV,	ININT,
	SAREG|SOREG|SNAME,	TCHAR,
	SAREG,	TUSHORT,
		NASL|NAREG,	RESC1,
		"	movsbw ZL,Z1\n", },

/* convert unsigned char to (u)int. */
{ SCONV,	ININT,
	SAREG|SOREG|SNAME,	TUCHAR,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	movzbl ZL,A1\n", },

/* convert short to (u)int. */
{ SCONV,	ININT,
	SAREG|SOREG|SNAME,	TSHORT,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	movswl ZL,A1\n", },

/* convert float/double (in register) to (unsigned) long long */
/* XXX - unsigned is not handled correct */
{ SCONV,	INLL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHLL,	TLONGLONG|TULONGLONG,
		NAREG,	RESC1,
		"	subl $8,%esp\n	fistpq (%esp)\n"
		"	popl AL\n	popl UL\n", },

/* convert float/double to (u) int/short/char. XXX should use NTEMP here */
{ SCONV,	INCH,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHCH,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG,	RESC1,
		"	subl $4,%esp\n	fistpl (%esp)\n	popl A1\n", },

/* convert double <-> float. nothing to do here */
{ SCONV,	INFL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"", },

/* convert unsigned short to (u)int. */
{ SCONV,	ININT,
	SAREG|SOREG|SNAME,	TUSHORT,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	movzwl ZL,A1\n", },

/* convert unsigned char to (u)short. */
{ SCONV,	ININT,
	SHCH|SOREG|SNAME,	TUCHAR,
	SAREG,	TSHORT|TUSHORT,
		NASL|NAREG,	RESC1,
		"	movzbw ZL,Z1\n", },

/* convert unsigned short to (u)long long */
{ SCONV,	INLL,
	SAREG|SOREG|SNAME,	TUSHORT,
	SANY,	TLL,
		NAREG|NASL,	RESC1,
		"	movzwl ZL,A1\n	xorl U1,U1\n", },

/* convert unsigned char to (u)long long */
{ SCONV,	INLL,
	SHCH|SOREG|SNAME,	TUCHAR,
	SANY,			TLL,
		NAREG|NASL,	RESC1,
		"	movzbl ZL,A1\n	xorl U1,U1\n", },

/* convert char to (u)long long */
{ SCONV,	INLL,
	SHCH|SOREG|SNAME,	TCHAR,
	SANY,	TLL,
		NSPECIAL|NAREG|NASL,	RESC1,
		"	movsbl ZL,%eax\n	cltd\n", },

/* convert short to (u)long long */
{ SCONV,	INLL,
	SAREG|SOREG|SNAME,	TSHORT,
	SAREG,	TLL,
		NSPECIAL|NAREG|NASL,	RESC1,
		"	movswl ZL,%eax\n	cltd\n", },

/* convert int to long long */
{ SCONV,	INLL,
	SAREG,	TWORD|TPOINT,
	SAREG,	TLONGLONG,
		NSPECIAL|NAREG|NASL,	RESC1,
		"	cltd\n", },

/* convert long long to int (mem->reg) */
{ SCONV,	INAREG,
	SOREG|SNAME,	TLL,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	movl AL,A1\n", },

/* convert long long to int (reg->reg, do nothing) */
{ SCONV,	INAREG,
	SHLL|SOREG|SNAME,	TLL,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"", },

/* convert (u)long long to (u)short (mem->reg) */
{ SCONV,	INAREG,
	SOREG|SNAME,	TLL,
	SAREG,	TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	movw ZL,Z1\n", },

/* convert (u)long long to (u)short (reg->reg, do nothing) */
{ SCONV,	INAREG,
	SHLL|SOREG|SNAME,	TLL,
	SAREG,	TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"", },

/* convert (u)long long to (u)char (mem->reg) */
{ SCONV,	INCH,
	SOREG|SNAME,	TLL,
	SANY,	TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	movb ZL,Z1\n", },

/* convert (u)long long to (u)char (reg->reg, do nothing) */
{ SCONV,	INCH,
	SHLL,	TLL,
	SANY,	TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"", },

/* convert int to unsigned long long */
{ SCONV,	INAREG,
	SAREG|SOREG|SNAME,	TWORD|TPOINT,
	SAREG,	TULONGLONG,
		NASL|NAREG,	RESC1,
		"	movl AL,A1\n	xorl U1,U1\n", },

/* convert long long (in memory) to floating */
{ SCONV,	INTBREG,
	SOREG|SNAME,	TLONGLONG,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NBREG,	RESC1,
		"	fildq AL\n", },

/* convert unsigned long long to floating */
{ SCONV,	INTBREG,
	SAREG,	TULONGLONG,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NBREG,	RESC1,
		"ZJ", },

/* convert int (in memory) to double */
{ SCONV,	INTBREG,
	SOREG|SNAME,	TWORD,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NBREG,	RESC1,
		"	fildl AL\n", },

/* convert long long (in register) to floating */
{ SCONV,	INTBREG,
	SAREG,	TLONGLONG,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NBREG,	RESC1,
		"	pushl UL\n	pushl AL\n"
		"	fildq (%esp)\n	addl $8,%esp\n", },

/* convert int (in register) to double */
{ SCONV,	INTBREG,
	SAREG,	TWORD,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NBREG,	RESC1,
		"	pushl AL\n	fildl (%esp)\n	addl $4,%esp\n", },

/* convert char (in register) to double XXX - use NTEMP */
{ SCONV,	INTBREG,
	SAREG|SOREG|SNAME,	TCHAR,
	SBREG,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NBREG|NTEMP,	RESC2,
		"	movsbl ZL,A1\n	pushl A1\n"
		"	fildl (%esp)\n	addl $4,%esp\n", },

/* convert (u)char (in register) to double XXX - use NTEMP */
{ SCONV,	INTBREG,
	SAREG|SOREG|SNAME,	TUCHAR,
	SBREG,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NBREG|NTEMP,	RESC2,
		"	movzbl ZL,A1\n	pushl A1\n"
		"	fildl (%esp)\n	addl $4,%esp\n", },

/* convert short (in memory) to float/double */
{ SCONV,	INTBREG,
	SOREG|SNAME,	TSHORT,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NBREG,	RESC1,
		"	fild AL\n", },

/* convert short (in register) to float/double */
{ SCONV,	INTBREG,
	SAREG,	TSHORT,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NBREG,	RESC1,
		"	pushw ZL\n	fild (%esp)\n	addl $2,%esp\n", },

/* convert unsigned short to double XXX - use NTEMP */
{ SCONV,	INTBREG,
	SAREG|SOREG|SNAME,	TUSHORT,
	SBREG,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NBREG|NTEMP|NASL,	RESC2,
		"	movzwl ZL,A1\n	pushl A1\n"
		"	fildl (%esp)\n	addl $4,%esp\n", },

/*
 * Subroutine calls.
 */

{ UCALL,	INAREG|FOREFF,
	SCON,	TANY,
	SAREG,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INBREG|FOREFF,
	SCON,	TANY,
	SBREG,	TANY,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INCREG|FOREFF,
	SCON,	TANY,
	SCREG,	TANY,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INDREG|FOREFF,
	SCON,	TANY,
	SDREG,	TANY,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INTAREG|FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ UCALL,	INTBREG|FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

/* struct return */
{ USTCALL,	INTAREG|FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ USTCALL,	INTAREG|FOREFF,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

/*
 * The next rules handle all binop-style operators.
 */
/* Special treatment for long long */
{ PLUS,		INAREG|FOREFF,
	SHLL,		TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	addl AR,AL\n	adcl UR,UL\n", },

/* Special treatment for long long  XXX - fix commutative check */
{ PLUS,		INAREG|FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SHLL,			TLL,
		0,	RRIGHT,
		"	addl AL,AR\n	adcl UL,UR\n", },

{ PLUS,		INBREG,
	SHFL,		TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	faddl AR\n", },

{ PLUS,		INBREG,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	faddp %st,%st(1)\n", },

/* address as register offset, positive */
{ PLUS,		INTAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	leal CR(AL),A1\n", },

{ PLUS,		INTAREG,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SONE,	TANY,
		NASL,	RLEFT,
		"	incb AL\n", },

/* address as register offset, negative */
{ MINUS,	INTAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	leal -CR(AL),A1\n", },

{ MINUS,		INAREG|FOREFF,
	SHLL,		TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	subl AR,AL\n	sbbl UR,UL\n", },

{ MINUS,		INBREG,
	SHFL,		TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	fsubl AR\n", },

{ MINUS,		INBREG,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fsubZHp %st,%st(1)\n", },

/* Simple r/m->reg ops */
{ OPSIMP,	INAREG|FOREFF,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	Ol AR,AL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SHINT,		TSHORT|TUSHORT,
	SHINT|SNAME|SOREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		"	Ow ZR,ZL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SHCH,		TCHAR|TUCHAR,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	Ob ZR,ZL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SAREG,		TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"	Ol AR,AL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SHINT|SNAME|SOREG,	TSHORT|TUSHORT,
	SCON,	TANY,
		0,	RLEFT,
		"	Ow ZR,ZL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,	TANY,
		0,	RLEFT,
		"	Ob ZR,ZL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SHLL,	TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	orl AR,AL\n	orl UR,UL\n", },


/*
 * The next rules handle all shift operators.
 */
{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD,
	SAREG|SHCH,		TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
		NSPECIAL,	RLEFT,
		"	sall ZA,AL\n", },

{ LS,	INAREG|FOREFF,
	SAREG,	TWORD,
	SCON,	TANY,
		0,	RLEFT,
		"	sall ZA,AL\n", },

{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SAREG,			TANY,
		NSPECIAL,	RLEFT,
		"	shlw ZA,ZL\n", },

{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SCON,	TANY,
		0,	RLEFT,
		"	shlw ZA,ZL\n", },

{ LS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SAREG,			TANY,
		NSPECIAL,	RLEFT,
		"	salb ZA,ZL\n", },

{ LS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,			TANY,
		0,	RLEFT,
		"	salb ZA,ZL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSWORD,
	SAREG,			TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
		NSPECIAL,	RLEFT,
		"	sarl ZA,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSWORD,
	SCON,			TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0,		RLEFT,
		"	sarl ZA,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUWORD,
	SAREG,			TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
		NSPECIAL,	RLEFT,
		"	shrl ZA,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUWORD,
	SCON,			TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0,		RLEFT,
		"	shrl ZA,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT,
	SAREG,			TANY,
		NSPECIAL,	RLEFT,
		"	sarw ZA,ZL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT,
	SCON,			TANY,
		0,		RLEFT,
		"	sarw AR,ZL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT,
	SAREG,			TANY,
		NSPECIAL,	RLEFT,
		"	shrw ZA,ZL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT,
	SCON,			TANY,
		0,		RLEFT,
		"	shrw AR,ZL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR,
	SHCH,			TANY,
		NSPECIAL,	RLEFT,
		"	sarb ZA,ZL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR,
	SCON,			TANY,
		0,		RLEFT,
		"	sarb AR,ZL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TUCHAR,
	SHCH,			TANY,
		NSPECIAL,	RLEFT,
		"	shrb ZA,ZL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TUCHAR,
	SCON,			TANY,
		0,		RLEFT,
		"	shrb AR,ZL\n", },

/*
 * The next rules takes care of assignments. "=".
 */
{ ASSIGN,	FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SCON,		TANY,
		0,	0,
		"	movl AR,AL\n	movl UR,UL\n", },

{ ASSIGN,	FOREFF|INLL,
	SHLL,		TLL,
	SCON,		TANY,
		0,	RLEFT,
		"	movl AR,AL\n	movl UR,UL\n", },

{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SCON,		TANY,
		0,	0,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,		TANY,
		0,	RLEFT,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SCON,		TANY,
		0,	0,
		"	movw ZR,ZL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TSHORT|TUSHORT,
	SCON,		TANY,
		0,	RLEFT,
		"	movw ZR,ZL\n", },

{ ASSIGN,	FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,		TANY,
		0,	0,
		"	movb AR,AL\n", },

{ ASSIGN,	FOREFF|INCH,
	SHCH,		TCHAR|TUCHAR,
	SCON,		TANY,
		0,	RLEFT,
		"	movb AR,AL\n", },

{ ASSIGN,	FOREFF|INLL,
	SHLL|SNAME|SOREG,	TLL,
	SHLL,			TLL,
		0,	RRIGHT,
		"	movl AR,AL\n	movl UR,UL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SAREG,		TWORD|TPOINT,
		0,	RRIGHT,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SAREG,		TSHORT|TUSHORT,
		0,	RRIGHT,
		"	movw ZR,ZL\n", },

{ ASSIGN,	FOREFF|INCH,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SHCH,		TCHAR|TUCHAR|TWORD,
		0,	RRIGHT,
		"	movb AR,AL\n", },

{ ASSIGN,	FOREFF|INTAREG,
	SFLD,		TANY,
	SAREG,	TANY,
		NAREG,	RRIGHT,
		"ZE", },

{ ASSIGN,	FOREFF|INTAREG,
	SFLD,		TANY,
	SAREG|SNAME|SOREG|SCON,	TANY,
		NAREG,	0,
		"ZE", },

/* order of table entries is very important here! */
{ ASSIGN,	FOREFF|INFL,
	SNAME|SOREG,	TLDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RRIGHT,
		"	fld %st(0)\n	fstpt AL\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TLDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	0,
		"	fstpt AL\n", },

{ ASSIGN,	FOREFF|INFL,
	SNAME|SOREG,	TDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RRIGHT,
		"	fstl AL\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	0,
		"	fstpl AL\n", },

{ ASSIGN,	FOREFF|INFL,
	SNAME|SOREG,	TFLOAT,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RRIGHT,
		"	fsts AL\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TFLOAT,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	0,
		"	fstps AL\n", },
/* end very important order */

/* Not really an assign node */
{ MOVE,		FOREFF|INTAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG,	RRIGHT,
		"	movl AL,AR\n" },

{ MOVE,		FOREFF|INTAREG,
	SAREG,	TSHORT|TUSHORT,
	SAREG,	TSHORT|TUSHORT,
		NAREG,	RRIGHT,
		"	movw ZL,ZR\n" },

{ MOVE,		FOREFF|INTAREG,
	SAREG,	TCHAR|TUCHAR,
	SAREG,	TCHAR|TUCHAR,
		NAREG,	RRIGHT,
		"	movb ZL,ZR\n" },

/* needed for reg->fpreg sconv at force */
{ MOVE,		FOREFF|INTBREG,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RRIGHT,
		"" },

/*
 * DIV/MOD/MUL 
 */

{ DIV,	INAREG,
	SAREG,			TSWORD,
	SAREG|SNAME|SOREG,	TWORD,
		NSPECIAL,	RLEFT,
		"	cltd\n	idivl AR\n", },

{ DIV,	INAREG,
	SAREG,			TUWORD|TPOINT,
	SAREG|SNAME|SOREG,	TUWORD|TPOINT,
		NSPECIAL,	RLEFT,
		"	xorl %edx,%edx\n	divl AR\n", },

{ DIV,	INAREG,
	SAREG,			TUSHORT,
	SAREG|SNAME|SOREG,	TUSHORT,
		NSPECIAL,	RLEFT,
		"	xorl %edx,%edx\n	divw ZR\n", },

{ DIV,	INCH,
	SHCH,			TUCHAR,
	SHCH|SNAME|SOREG,	TUCHAR,
		NSPECIAL,	RLEFT,
		"	xorb %ah,%ah\n	divb ZR\n", },

{ DIV,	INFL,
	SHFL,		TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	fdivl AR\n", },

{ DIV,	INFL,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fdivrp %st,%st(1)\n", },

{ MOD,	INAREG,
	SAREG,			TSWORD,
	SAREG|SNAME|SOREG,	TSWORD,
		NAREG|NSPECIAL,	RESC1,
		"	cltd\n	idivl AR\n", },

{ MOD,	INAREG,
	SAREG,			TUWORD|TPOINT,
	SAREG|SNAME|SOREG,	TUWORD|TPOINT,
		NAREG|NSPECIAL,	RESC1,
		"	xorl %edx,%edx\n	divl AR\n", },

{ MOD,	INAREG,
	SAREG,			TUSHORT,
	SAREG|SNAME|SOREG,	TUSHORT,
		NAREG|NSPECIAL,	RESC1,
		"	xorl %edx,%edx\n	divw ZR\n", },

{ MOD,	INCH,
	SHCH,			TUCHAR,
	SHCH|SNAME|SOREG,	TUCHAR,
		NBREG|NSPECIAL,	RESC1,
		"	xorb %ah,%ah\n	divb ZR\n", },

{ MUL,	INAREG,
	SAREG,				TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,		TWORD|TPOINT,
		0,	RLEFT,
		"	imull AR,AL\n", },

{ MUL,	INAREG,
	SAREG,			TSHORT|TUSHORT,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		"	imulw ZR,ZL\n", },

{ MUL,	INCH,
	SHCH,			TCHAR|TUCHAR,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	imulb ZR\n", },

{ MUL,	INFL,
	SHFL,		TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	fmull AR\n", },

{ MUL,	INFL,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fmulp %st,%st(1)\n", },

/*
 * Indirection operators.
 */
{ UMUL,	INTAREG,
	SAREG,	TPTRTO|TLL,
	SANY,		TLL,
		NAREG|NASL,	RESC1,
		"	movl 4(AL),U1\n	movl (AL),A1\n", },

{ UMUL,	INTAREG,
	SAREG,	TPOINT|TWORD,
	SANY,		TPOINT|TWORD,
		NAREG|NASL,	RESC1,
		"	movl (AL),A1\n", },

{ UMUL,	INTAREG,
	SAREG,	TCHAR|TUCHAR|TPTRTO,
	SANY,		TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	movb (AL),Z1\n", },

{ UMUL,	INTAREG,
	SAREG,	TSHORT|TUSHORT|TPTRTO,
	SANY,		TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	movw (AL),Z1\n", },

{ UMUL,	INTBREG,
	SAREG,	TLDOUBLE|TPTRTO,
	SANY,		TLDOUBLE,
		NBREG,	RESC1,
		"	fldq (AL)\n", },

{ UMUL,	INTBREG,
	SAREG,	TDOUBLE|TPTRTO,
	SANY,		TDOUBLE,
		NBREG,	RESC1,
		"	fldl (AL)\n", },

{ UMUL,	INTBREG,
	SAREG,	TFLOAT|TPTRTO,
	SANY,		TFLOAT,
		NBREG,	RESC1,
		"	flds (AL)\n", },

/*
 * INCR/DECR operators (post-increment)
 */
{ INCR,	INTAREG,
	SAREG|SNAME|SOREG,	TCHAR|TUCHAR|TPTRTO,
	SANY,	TANY,
		NAREG,	RESC1,
		"	movl ZL,Z1\n	incl ZL\n", },

{ INCR,	INTAREG,
	SAREG|SNAME|SOREG,	TWORD,
	SANY,	TANY,
		NAREG,	RESC1,
		"	movl ZL,Z1\n	incl ZL\n", },

{ INCR,	INTAREG,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SANY,	TANY,
		NAREG,	RESC1,
		"	movw ZL,Z1\n	incw ZL\n", },

{ INCR,	INTAREG,
	SAREG|SNAME|SOREG,	TCHAR|TUCHAR,
	SANY,	TANY,
		NAREG,	RESC1,
		"	movb ZL,Z1\n	incb ZL\n", },

/*
 * Logical/branching operators
 */

/* Comparisions, take care of everything */
{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME,	TLL,
	SAREG,			TLL,
		0,	0,
		"ZD", },

{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME,	TWORD|TPOINT,
	SCON|SAREG,	TWORD|TPOINT,
		0, 	RESCC,
		"	cmpl AR,AL\n", },

{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME,	TSHORT|TUSHORT,
	SCON|SAREG,	TANY,
		0, 	RESCC,
		"	cmpw ZR,ZL\n", },

{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME,	TCHAR|TUCHAR,
	SCON|SAREG,	TANY,
		0, 	RESCC,
		"	cmpb ZR,ZL\n", },

{ OPLOG,	FORCC,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		3*NAREG, 	0,
		"ZG", },

{ OPLOG,	FORCC,
	SOREG|SNAME,	TDOUBLE|TFLOAT,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		3*NAREG, 	0,
		"ZG", },

{ OPLOG,	FORCC,
	SANY,	TANY,
	SANY,	TANY,
		REWRITE,	0,
		"diediedie!", },

/* AND/OR/ER/NOT */
{ AND,	INTAREG|FOREFF,
	SAREG|SOREG|SNAME,	TWORD,
	SCON|SAREG,		TWORD,
		0,	RLEFT,
		"	andl AR,AL\n", },

{ AND,	INTAREG|FOREFF,
	SAREG,			TLL,
	SAREG|SOREG|SNAME,	TLL,
		0,	RLEFT,
		"	andl AR,AL\n	andl UR,UL\n", },

{ AND,	INTAREG|FOREFF,
	SAREG,			TWORD,
	SAREG|SOREG|SNAME,	TWORD,
		0,	RLEFT,
		"	andl AR,AL\n", },

{ AND,	INTAREG|FOREFF,  
	SAREG|SOREG|SNAME,	TSHORT|TUSHORT,
	SCON|SAREG,		TSHORT|TUSHORT,
		0,	RLEFT,
		"	andw ZR,ZL\n", },

{ AND,	INTAREG|FOREFF,  
	SAREG,			TSHORT|TUSHORT,
	SAREG|SOREG|SNAME,	TSHORT|TUSHORT,
		0,	RLEFT,
		"	andw ZR,ZL\n", },

{ AND,	INTAREG|FOREFF,
	SAREG|SOREG|SNAME,	TCHAR|TUCHAR,
	SCON|SAREG,		TCHAR|TUCHAR,
		0,	RLEFT,
		"	andb ZR,ZL\n", },

{ AND,	INTAREG|FOREFF,
	SAREG,			TCHAR|TUCHAR,
	SAREG|SOREG|SNAME,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	andb ZR,ZL\n", },






/* AND/OR/ER/NOT */

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jmp LL\n", },

#ifdef GCC_COMPAT
{ GOTO, 	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jmp *AL\n", },
#endif

/*
 * Convert LTYPE to reg.
 */
{ OPLTYPE,	INLL,
	SANY,	TANY,
	SCON|SOREG|SNAME,	TLL,
		NAREG,	RESC1,
		"	movl UL,U1\n	movl AL,A1\n", },

{ OPLTYPE,	INTAREG,
	SANY,	TANY,
	SAREG|SCON|SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	movl AL,A1\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SAREG|SOREG|SNAME|SCON,	TCHAR|TUCHAR,
		NAREG,	RESC1,
		"	movb ZL,Z1\n", },

{ OPLTYPE,	INTAREG,
	SANY,	TANY,
	SAREG|SOREG|SNAME|SCON,	TSHORT|TUSHORT,
		NAREG,	RESC1,
		"	movw ZL,Z1\n", },

{ OPLTYPE,	INTBREG,
	SANY,		TLDOUBLE,
	SOREG|SNAME,	TLDOUBLE,
		NBREG,	RESC1,
		"	fldt AL\n", },

{ OPLTYPE,	INTBREG,
	SANY,		TDOUBLE,
	SOREG|SNAME,	TDOUBLE,
		NBREG,	RESC1,
		"	fldl AL\n", },

{ OPLTYPE,	INTBREG,
	SANY,		TFLOAT,
	SOREG|SNAME,	TFLOAT,
		NBREG,	RESC1,
		"	flds AL\n", },

/*
 * Negate a word.
 */

{ UMINUS,	INAREG|INTAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RLEFT,
		"	negl AL\n	adcl $0,UL\n	negl UL\n", },

{ UMINUS,	INAREG|INTAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	negl AL\n", },

{ UMINUS,	INAREG|INTAREG|FOREFF,
	SAREG,	TSHORT|TUSHORT,
	SAREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		"	negw ZL\n", },

{ UMINUS,	INAREG|INTAREG|FOREFF,
	SAREG,	TCHAR|TUCHAR,
	SAREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	negb ZL\n", },

{ UMINUS,	INBREG|INTBREG|FOREFF,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fchs\n", },

#if 0
{ UMINUS,	INAREG|INTAREG|FOREFF,
	SAREG|SNAME|SOREG,	TLL,
	SANY,	TLL,
		NAREG|NASR,	RESC1,
		"	dmovn A1,AL\n", },
#endif

{ COMPL,	INTAREG,
	SAREG,	TLL,
	SANY,	TANY,
		NASL,	RLEFT,
		"	notl AL\n	notl UL\n", },

{ COMPL,	INTAREG,
	SAREG,	TWORD,
	SANY,	TANY,
		NASL,	RLEFT,
		"	notl AL\n", },

{ COMPL,	INTAREG,
	SAREG,	TSHORT|TUSHORT,
	SANY,	TANY,
		NASL,	RLEFT,
		"	notw ZL\n", },

{ COMPL,	INTAREG,
	SAREG,	TCHAR|TUCHAR,
	SANY,	TANY,
		NASL,	RLEFT,
		"	notb ZL\n", },

/*
 * Arguments to functions.
 */
{ FUNARG,	FOREFF,
	SCON|SAREG|SNAME|SOREG,	TLL,
	SANY,	TLL,
		0,	RNULL,
		"	pushl UL\n	pushl AL\n", },

{ FUNARG,	FOREFF,
	SCON|SAREG|SNAME|SOREG,	TWORD|TPOINT|TFLOAT,
	SANY,	TWORD|TPOINT|TFLOAT,
		0,	RNULL,
		"	pushl AL\n", },

{ FUNARG,	FOREFF,
	SCON,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SANY,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		0,	RNULL,
		"	pushl AL\n", },

{ FUNARG,	FOREFF,
	SAREG|SNAME|SOREG,	TSHORT,
	SANY,	TSHORT,
		NAREG,	0,
		"	movswl ZL,A1\n	pushl A1\n", },

{ FUNARG,	FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT,
	SANY,	TUSHORT,
		NAREG,	0,
		"	movzwl ZL,A1\n	pushl A1\n", },

{ FUNARG,	FOREFF,
	SHCH|SNAME|SOREG,	TCHAR,
	SANY,			TCHAR,
		NAREG,	0,
		"	movsbl AL,A1\n	pushl A1\n", },

{ FUNARG,	FOREFF,
	SAREG|SNAME|SOREG,	TUCHAR,
	SANY,	TUCHAR,
		NAREG,	0,
		"	movzbl AL,A1\n	pushl A1\n", },

{ FUNARG,	FOREFF,
	SNAME|SOREG,	TDOUBLE,
	SANY,	TDOUBLE,
		0,	0,
		"	pushl UL\n	pushl AL\n", },

{ FUNARG,	FOREFF,
	SBREG,	TDOUBLE,
	SANY,		TDOUBLE,
		0,	0,
		"	subl $8,%esp\n	fstpl (%esp)\n", },

{ FUNARG,	FOREFF,
	SNAME|SOREG,	TFLOAT,
	SANY,		TFLOAT,
		0,	0,
		"	pushl AL\n", },

{ FUNARG,	FOREFF,
	SBREG,	TFLOAT,
	SANY,		TFLOAT,
		0,	0,
		"	subl $4,%esp\n	fstps (%esp)\n", },

{ FUNARG,	FOREFF,
	SBREG,	TLDOUBLE,
	SANY,		TLDOUBLE,
		0,	0,
		"	subl $12,%esp\n	fstpt (%esp)\n", },

{ FUNARG,	FOREFF,
	SAREG|SOREG|SNAME|SCON,	TANY,
	SANY,	TSTRUCT,
		3*NAREG|NASL,	0,
		"ZF", },

# define DF(x) FORREW,SANY,TANY,SANY,TANY,REWRITE,x,""

{ UMUL, DF( UMUL ), },

{ INCR, DF(INCR), },

{ DECR, DF(INCR), },

{ ASSIGN, DF(ASSIGN), },

{ STASG, DF(STASG), },

{ FLD, DF(FLD), },

{ OPLEAF, DF(NAME), },

{ INIT, DF(INIT), },

{ OPUNARY, DF(UMINUS), },

{ OPANY, DF(BITYPE), },

{ FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	"help; I'm in trouble\n" },
};

int tablesize = sizeof(table)/sizeof(table[0]);
