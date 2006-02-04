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

#define	INTAREG	INAREG
#define	INTBREG	INBREG

/*
 * A bunch conversions of integral<->integral types
 * There are lots of them, first in table conversions to itself
 * and then conversions from each type to the others.
 */

/* itself to itself, including pointers */

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

/* convert double <-> float. nothing to do here */
{ SCONV,	INFL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"", },

/* char to something */

/* convert char to (unsigned) short. */
{ SCONV,	ININT,
	SBREG|SOREG|SNAME,	TCHAR,
	SAREG,	TSHORT|TUSHORT,
		NASL|NAREG,	RESC1,
		"	movsbw AL,A1\n", },

/* convert unsigned char to (u)short. */
{ SCONV,	ININT,
	SHCH|SOREG|SNAME,	TUCHAR,
	SAREG,	TSHORT|TUSHORT,
		NASL|NAREG,	RESC1,
		"	movzbw AL,A1\n", },

/* convert signed char to int (or pointer). */
{ SCONV,	ININT,
	SHCH|SOREG|SNAME,	TCHAR,
	SAREG,	TWORD|TPOINT,
		NASL|NAREG,	RESC1,
		"	movsbl AL,A1\n", },

/* convert unsigned char to (u)int. */
{ SCONV,	ININT,
	SHCH|SOREG|SNAME,	TUCHAR,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	movzbl AL,A1\n", },

/* convert char to (u)long long */
{ SCONV,	INLL,
	SHCH|SOREG|SNAME,	TCHAR,
	SANY,	TLL,
		NSPECIAL|NAREG|NASL,	RESC1,
		"	movsbl AL,%eax\n	cltd\n", },

/* convert unsigned char to (u)long long */
{ SCONV,	INLL,
	SHCH|SOREG|SNAME,	TUCHAR,
	SANY,			TLL,
		NAREG|NASL,	RESC1,
		"	movzbl ZL,A1\n	xorl U1,U1\n", },

/* convert char (in register) to double XXX - use NTEMP */
{ SCONV,	INFL,
	SHCH|SOREG|SNAME,	TCHAR,
	SHFL,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NASL|NDREG,	RESC2,
		"	movsbl AL,A1\n	pushl A1\n"
		"	fildl (%esp)\n	addl $4,%esp\n", },

/* convert (u)char (in register) to double XXX - use NTEMP */
{ SCONV,	INFL,
	SHCH|SOREG|SNAME,	TUCHAR,
	SHFL,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NASL|NDREG,	RESC2,
		"	movzbl AL,A1\n	pushl A1\n"
		"	fildl (%esp)\n	addl $4,%esp\n", },

/* short to something */

/* convert short (in memory) to char */
{ SCONV,	INCH,
	SNAME|SOREG,	TSHORT|TUSHORT,
	SHCH,		TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"	movb AL,A1\n", },

/* convert short (in reg) to char. */
{ SCONV,	INCH,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL|NBREG|NBSL,	RESC1,
		"ZM", },

/* convert short to (u)int. */
{ SCONV,	ININT,
	SAREG|SOREG|SNAME,	TSHORT,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	movswl AL,A1\n", },

/* convert unsigned short to (u)int. */
{ SCONV,	ININT,
	SAREG|SOREG|SNAME,	TUSHORT,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	movzwl AL,A1\n", },

/* convert short to (u)long long */
{ SCONV,	INLL,
	SAREG|SOREG|SNAME,	TSHORT,
	SHLL,			TLL,
		NSPECIAL|NCREG|NCSL,	RESC1,
		"	movswl AL,%eax\n	cltd\n", },

/* convert unsigned short to (u)long long */
{ SCONV,	INLL,
	SAREG|SOREG|SNAME,	TUSHORT,
	SHLL,			TLL,
		NCREG|NCSL,	RESC1,
		"	movzwl AL,A1\n	xorl U1,U1\n", },

/* convert short (in memory) to float/double */
{ SCONV,	INFL,
	SOREG|SNAME,	TSHORT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NDREG,	RESC1,
		"	fild AL\n", },

/* convert short (in register) to float/double */
{ SCONV,	INFL,
	SAREG,	TSHORT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NDREG,	RESC1,
		"XXX wrong	pushw AL\n	fild (%esp)\n	addl $2,%esp\n", },

/* convert unsigned short to double XXX - use NTEMP */
{ SCONV,	INFL,
	SAREG|SOREG|SNAME,	TUSHORT,
	SHFL,			TLDOUBLE|TDOUBLE|TFLOAT,
		NDREG|NTEMP,	RESC1,
		"XXX wrong	movzwl AL,A1\n	pushl A1\n"
		"	fildl (%esp)\n	addl $4,%esp\n", },

/* int to something */

/* convert int to char. This is done when register is loaded */
{ SCONV,	INCH,
	SAREG,	TWORD,
	SANY,	TCHAR|TUCHAR,
		NSPECIAL|NBREG|NBSL,	RESC1,
		"ZM", },

/* convert int to short. Nothing to do */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SANY,	TSHORT|TUSHORT,
		0,	RLEFT,
		"", },

/* convert int to long long */
{ SCONV,	INLL,
	SAREG,	TWORD|TPOINT,
	SCREG,	TLONGLONG,
		NSPECIAL|NCREG|NCSL,	RESC1,
		"	cltd\n", },

/* convert int to unsigned long long */
{ SCONV,	INLL,
	SAREG|SOREG|SNAME,	TWORD|TPOINT,
	SHLL,	TULONGLONG,
		NCSL|NCREG,	RESC1,
		"	movl AL,A1\n	xorl U1,U1\n", },

/* convert int (in memory) to double */
{ SCONV,	INFL,
	SOREG|SNAME,	TWORD,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		NDREG,	RESC1,
		"	fildl AL\n", },

/* convert int (in register) to double */
{ SCONV,	INFL,
	SAREG,	TWORD,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NDREG,	RESC1,
		"	pushl AL\n	fildl (%esp)\n	addl $4,%esp\n", },

/* long long to something */

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

/* convert long long (in memory) to floating */
{ SCONV,	INFL,
	SOREG|SNAME,	TLONGLONG,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		NDREG,	RESC1,
		"	fildq AL\n", },

/* convert long long (in register) to floating */
{ SCONV,	INFL,
	SHLL,	TLONGLONG,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NDREG,	RESC1,
		"	pushl UL\n	pushl AL\n"
		"	fildq (%esp)\n	addl $8,%esp\n", },

/* convert unsigned long long to floating */
{ SCONV,	INFL,
	SAREG,	TULONGLONG,
	SBREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NBREG,	RESC1,
		"XXX check! ZJ", },

/* float to something */

#if 0 /* go via int by adding an extra sconv in clocal() */
/* convert float/double to (u) char. XXX should use NTEMP here */
{ SCONV,	INCH,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHCH,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NBREG,	RESC1,
		"	subl $4,%esp\n	fistpl (%esp)\n	popl A1\n", },

/* convert float/double to (u) int/short/char. XXX should use NTEMP here */
{ SCONV,	INCH,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHCH,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NCREG,	RESC1,
		"	subl $4,%esp\n	fistpl (%esp)\n	popl A1\n", },
#endif

/* convert float/double to (u)int. XXX should use NTEMP here */
{ SCONV,	INAREG,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	subl $4,%esp\n	fistpl (%esp)\n	popl A1\n", },

/* convert float/double (in register) to (unsigned) long long */
/* XXX - unsigned is not handled correct */
{ SCONV,	INLL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHLL,	TLONGLONG|TULONGLONG,
		NCREG,	RESC1,
		"	subl $8,%esp\n	fistpq (%esp)\n"
		"	popl A1\n	popl U1\n", },

/* slut sconv */

/*
 * Subroutine calls.
 */

/* separate handling of FOREFF */
{ CALL,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call CL\nZC", },

{ UCALL,	FOREFF,
	SCON,	TANY,
	SAREG,	TWORD|TPOINT,
		0,	0,
		"	call CL\nZC", },

{ UCALL,	INAREG,
	SCON,	TANY,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INBREG,
	SCON,	TANY,
	SBREG,	TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INCREG,
	SCON,	TANY,
	SCREG,	TANY,
		NCREG|NCSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INDREG,
	SCON,	TANY,
	SDREG,	TANY,
		NDREG|NDSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

/* separate handling of FOREFF */
{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"	call *AL\nZC", },

{ UCALL,	INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ UCALL,	INBREG,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

/* struct return */
{ USTCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call CL\nZC", },

{ USTCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ USTCALL,	INAREG,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

/*
 * The next rules handle all binop-style operators.
 */
/* Special treatment for long long */
{ PLUS,		INLL|FOREFF,
	SHLL,		TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	addl AR,AL\n	adcl UR,UL\n", },

/* Special treatment for long long  XXX - fix commutative check */
{ PLUS,		INLL|FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SHLL,			TLL,
		0,	RRIGHT,
		"	addl AL,AR\n	adcl UL,UR\n", },

{ PLUS,		INFL,
	SHFL,		TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	faddl AR\n", },

{ PLUS,		INFL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	faddp %st,%st(1)\n", },

/* address as register offset, positive */
{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	leal CR(AL),A1\n", },

{ PLUS,		INCH,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SONE,	TANY,
		0,	RLEFT,
		"	incb AL\n", },

/* address as register offset, negative */
{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	leal -CR(AL),A1\n", },

{ MINUS,	INLL|FOREFF,
	SHLL,	TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	subl AR,AL\n	sbbl UR,UL\n", },

{ MINUS,	INFL,
	SHFL,	TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	fsubl AR\n", },

{ MINUS,	INFL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
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
		"	Ow AR,AL\n", },

{ OPSIMP,	INCH|FOREFF,
	SHCH,		TCHAR|TUCHAR,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	Ob AR,AL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"	Ol AR,AL\n", },

{ OPSIMP,	INAREG|FOREFF,
	SHINT|SNAME|SOREG,	TSHORT|TUSHORT,
	SCON,	TANY,
		0,	RLEFT,
		"	Ow AR,AL\n", },

{ OPSIMP,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,	TANY,
		0,	RLEFT,
		"	Ob ZR,ZL\n", },

{ OPSIMP,	INLL|FOREFF,
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
		"	movw AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TSHORT|TUSHORT,
	SCON,		TANY,
		0,	RLEFT,
		"	movw AR,AL\n", },

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
		"	movw AR,AL\n", },

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

{ ASSIGN,	INFL|FOREFF,
	SHFL,		TLDOUBLE,
	SHFL|SOREG|SNAME,	TLDOUBLE,
		0,	RLEFT,
		"	fldt AR\n", },

{ ASSIGN,	INFL|FOREFF,
	SHFL,		TDOUBLE,
	SHFL|SOREG|SNAME,	TDOUBLE,
		0,	RLEFT,
		"	fldl AR\n", },

{ ASSIGN,	INFL|FOREFF,
	SHFL,		TFLOAT,
	SHFL|SOREG|SNAME,	TFLOAT,
		0,	RLEFT,
		"	flds AR\n", },

/*
 * DIV/MOD/MUL 
 */
/* long long div is emulated */
{ DIV,	INCREG,
	SCREG|SNAME|SOREG|SCON, TLONGLONG,
	SCREG|SNAME|SOREG|SCON, TLONGLONG,
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
		"ZO", },

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
{ UMUL,	INLL,
	SAREG,	TPTRTO|TLL,
	SANY,		TLL,
		NCREG|NCSL,	RESC1,
		"	movl 4(AL),U1\n	movl (AL),A1\n", },

{ UMUL,	INAREG,
	SAREG,	TPOINT|TWORD,
	SANY,		TPOINT|TWORD,
		NAREG|NASL,	RESC1,
		"	movl (AL),A1\n", },

{ UMUL,	INCH,
	SAREG,	TCHAR|TUCHAR|TPTRTO,
	SANY,		TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"	movb (AL),A1\n", },

{ UMUL,	INAREG,
	SAREG,	TSHORT|TUSHORT|TPTRTO,
	SANY,		TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	movw (AL),A1\n", },

{ UMUL,	INFL,
	SAREG,	TLDOUBLE|TPTRTO,
	SANY,		TLDOUBLE,
		NDREG,	RESC1,
		"	fldq (AL)\n", },

{ UMUL,	INFL,
	SAREG,	TDOUBLE|TPTRTO,
	SANY,		TDOUBLE,
		NDREG,	RESC1,
		"	fldl (AL)\n", },

{ UMUL,	INFL,
	SAREG,	TFLOAT|TPTRTO,
	SANY,		TFLOAT,
		NDREG,	RESC1,
		"	flds (AL)\n", },

/*
 * INCR/DECR operators (post-increment)
 */
{ INCR,	INAREG,
	SAREG|SNAME|SOREG,	TCHAR|TUCHAR|TPTRTO,
	SANY,	TANY,
		NAREG,	RESC1,
		"	movl AL,A1\n	incl AL\n", },

{ INCR,	INAREG,
	SAREG|SNAME|SOREG,	TWORD,
	SANY,	TANY,
		NAREG,	RESC1,
		"	movl AL,A1\n	incl AL\n", },

{ INCR,	INAREG,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SANY,	TANY,
		NAREG,	RESC1,
		"	movw AL,A1\n	incw AL\n", },

{ INCR,	INCH,
	SAREG|SNAME|SOREG,	TCHAR|TUCHAR,
	SANY,	TANY,
		NBREG,	RESC1,
		"	movb AL,A1\n	incb AL\n", },

/*
 * Logical/branching operators
 */

/* Comparisions, take care of everything */
{ OPLOG,	FORCC,
	SHLL|SOREG|SNAME,	TLL,
	SHLL,			TLL,
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
		"	cmpw AR,AL\n", },

{ OPLOG,	FORCC,
	SBREG|SOREG|SNAME,	TCHAR|TUCHAR,
	SCON|SBREG,	TANY,
		0, 	RESCC,
		"	cmpb AR,AL\n", },

{ OPLOG,	FORCC,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		3*NDREG, 	0,
		"ZG", },

{ OPLOG,	FORCC,
	SOREG|SNAME,	TDOUBLE|TFLOAT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		3*NDREG, 	0,
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
	SCREG|SCON|SOREG|SNAME,	TLL,
		NCREG,	RESC1,
		"	movl UL,U1\n	movl AL,A1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG|SCON|SOREG|SNAME,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	movl AL,A1\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG|SOREG|SNAME|SCON,	TCHAR|TUCHAR,
		NBREG,	RESC1,
		"	movb AL,A1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG|SOREG|SNAME|SCON,	TSHORT|TUSHORT,
		NAREG,	RESC1,
		"	movw AL,A1\n", },

{ OPLTYPE,	INDREG,
	SANY,		TLDOUBLE,
	SOREG|SNAME,	TLDOUBLE,
		NDREG,	RESC1,
		"	fldt AL\n", },

{ OPLTYPE,	INDREG,
	SANY,		TDOUBLE,
	SOREG|SNAME,	TDOUBLE,
		NDREG,	RESC1,
		"	fldl AL\n", },

{ OPLTYPE,	INDREG,
	SANY,		TFLOAT,
	SOREG|SNAME,	TFLOAT,
		NDREG,	RESC1,
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

{ UMINUS,	INFL|FOREFF,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fchs\n", },

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
	SCON|SCREG|SNAME|SOREG,	TLL,
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
		"	movswl AL,ZN\n	pushl ZN\n", },

{ FUNARG,	FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT,
	SANY,	TUSHORT,
		NAREG,	0,
		"	movzwl AL,ZN\n	pushl ZN\n", },

{ FUNARG,	FOREFF,
	SHCH|SNAME|SOREG,	TCHAR,
	SANY,			TCHAR,
		NAREG,	0,
		"	movsbl AL,A1\n	pushl A1\n", },

{ FUNARG,	FOREFF,
	SHCH|SNAME|SOREG,	TUCHAR,
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

{ STARG,	FOREFF,
	SAREG|SOREG|SNAME|SCON,	TANY,
	SANY,	TSTRUCT,
		NSPECIAL,	0,
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
