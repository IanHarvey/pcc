/*	$Id$	*/
/*
 * Copyright (c) 2017 Anders Magnusson (ragge@ludd.luth.se).
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

/* PCONVs are usually not necessary */
{ PCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"", },

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

/* convert between float/double/long double. */
{ SCONV,	INFL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"ZI", },

/* convert pointers to pointers. */
{ SCONV,	ININT,
	SHINT,	TPOINT,
	SANY,	TPOINT,
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
		NSPECIAL|NCREG|NCSL,	RESC1,
		"	movsbl AL,%eax\n	cltd\n", },

/* convert unsigned char to (u)long long */
{ SCONV,	INLL,
	SHCH|SOREG|SNAME,	TUCHAR,
	SANY,			TLL,
		NCREG|NCSL,	RESC1,
		"	movzbl AL,A1\n	xorl U1,U1\n", },

/* short to something */

/* convert (u)short to (u)short. */
{ SCONV,	INAREG,
	SAREG,	TSHORT|TUSHORT,
	SAREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		"", },

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

/* int to something */

/* convert int to char. This is done when register is loaded */
{ SCONV,	INCH,
	SAREG,	TWORD|TPOINT,
	SANY,	TCHAR|TUCHAR,
		NSPECIAL|NBREG|NBSL,	RESC1,
		"ZM", },

/* convert int to short. Nothing to do */
{ SCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SANY,	TSHORT|TUSHORT,
		0,	RLEFT,
		"", },

/* convert signed int to (u)long long */
{ SCONV,	INLL,
	SHINT,	TSWORD,
	SHLL,	TLL,
		NSPECIAL|NCREG|NCSL,	RESC1,
		"	cltd\n", },

/* convert unsigned int to (u)long long */
{ SCONV,	INLL,
	SHINT|SOREG|SNAME,	TUWORD|TPOINT,
	SHLL,	TLL,
		NCSL|NCREG,	RESC1,
		"	movl AL,A1\n	xorl U1,U1\n", },

/* long long to something */

/* convert (u)long long to (u)char (mem->reg) */
{ SCONV,	INCH,
	SOREG|SNAME,	TLL,
	SANY,	TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"	movb AL,A1\n", },

/* convert (u)long long to (u)char (reg->reg, hopefully nothing) */
{ SCONV,	INCH,
	SHLL,	TLL,
	SANY,	TCHAR|TUCHAR,
		NBREG|NBSL|NTEMP,	RESC1,
		"ZS", },

/* convert (u)long long to (u)short (mem->reg) */
{ SCONV,	INAREG,
	SOREG|SNAME,	TLL,
	SAREG,	TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	movw AL,A1\n", },

/* convert (u)long long to (u)short (reg->reg, hopefully nothing) */
{ SCONV,	INAREG,
	SHLL|SOREG|SNAME,	TLL,
	SAREG,	TSHORT|TUSHORT,
		NAREG|NASL|NTEMP,	RESC1,
		"ZS", },

/* convert long long to int (mem->reg) */
{ SCONV,	INAREG,
	SOREG|SNAME,	TLL,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	movl AL,A1\n", },

/* slut sconv */

/*
 * Subroutine calls.
 */

{ UCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	jms CL\nZC", },

{ CALL,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	jms CL\nZC", },

{ UCALL,	FOREFF,
	SCON,	TANY,
	SAREG,	TWORD|TPOINT,
		0,	0,
		"	jms CL\nZC", },

{ CALL,	INAREG,
	SCON,	TANY,
	SAREG,	TSHORT|TUSHORT|TWORD|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	jms CL\nZC", },

{ UCALL,	INAREG,
	SCON,	TANY,
	SAREG,	TSHORT|TUSHORT|TWORD|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	jms CL\nZC", },

{ CALL,		FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"	dac 20\n	cal i\nZC", },

{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"	dac 20\n	cal i\nZC", },

{ CALL,		INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	dac 20\n	cal i\nZC", },

{ UCALL,	INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	dac 20\n	cal i\nZC", },

{ STCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	0,
		"	jms CL\nZC", },

{ STCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	jms CL\nZC", },

{ STCALL,	INAREG,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	dac 20\n	cal i\nZC", },

/*
 * The next rules handle all binop-style operators.
 */
{ PLUS,		INAREG|FOREFF,
	SNAME,	TWORD|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	isz AL\nZD", },

/* Simple r/m->reg ops */
/* m/r |= r */
{ OPSIMP,	INAREG|FOREFF|FORCC,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SAREG,			TWORD|TPOINT,
		0,	RLEFT|RESCC,
		"	Ol AR,AL\n", },

/* r |= r/m */
{ OPSIMP,	INAREG|FOREFF|FORCC,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT|RESCC,
		"	Ol AR,AL\n", },

/* m/r |= r */
{ OPSIMP,	INAREG|FOREFF|FORCC,
	SHINT|SNAME|SOREG,	TSHORT|TUSHORT,
	SHINT,		TSHORT|TUSHORT,
		0,	RLEFT|RESCC,
		"	Ow AR,AL\n", },

/* r |= r/m */
{ OPSIMP,	INAREG|FOREFF|FORCC,
	SHINT,		TSHORT|TUSHORT,
	SHINT|SNAME|SOREG,	TSHORT|TUSHORT,
		0,	RLEFT|RESCC,
		"	Ow AR,AL\n", },

/* m/r |= r */
{ OPSIMP,	INCH|FOREFF|FORCC,
	SHCH,		TCHAR|TUCHAR,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
		0,	RLEFT|RESCC,
		"	Ob AR,AL\n", },

/* r |= r/m */
{ OPSIMP,	INCH|FOREFF|FORCC,
	SHCH,		TCHAR|TUCHAR,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
		0,	RLEFT|RESCC,
		"	Ob AR,AL\n", },

/* m/r |= const */
{ OPSIMP,	INAREG|FOREFF|FORCC,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RLEFT|RESCC,
		"	Ol AR,AL\n", },

{ OPSIMP,	INAREG|FOREFF|FORCC,
	SHINT|SNAME|SOREG,	TSHORT|TUSHORT,
	SCON,	TANY,
		0,	RLEFT|RESCC,
		"	Ow AR,AL\n", },

{ OPSIMP,	INCH|FOREFF|FORCC,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,	TANY,
		0,	RLEFT|RESCC,
		"	Ob AR,AL\n", },

/* r |= r/m */
{ OPSIMP,	INLL|FOREFF,
	SHLL,	TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	Ol AR,AL\n	Ol UR,UL\n", },

/* m/r |= r/const */
{ OPSIMP,	INLL|FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SHLL|SCON,	TLL,
		0,	RLEFT,
		"	Ol AR,AL\n	Ol UR,UL\n", },

/* Try use-reg instructions first */
{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	leal CR(AL),A1\n", },


/*
 * The next rules handle all shift operators.
 */
/* (u)longlong left shift is emulated */
{ LS,	INCREG,
	SCREG,	TLL,
	SHCH,	TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"ZO", },

/* r/m <<= r */
{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD,
	SHCH,		TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	sall AR,AL\n", },

/* r/m <<= const */
{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD,
	SCON,	TANY,
		0,	RLEFT,
		"	sall AR,AL\n", },

/* r/m <<= r */
{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shlw AR,AL\n", },

/* r/m <<= const */
{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SCON,	TANY,
		0,	RLEFT,
		"	shlw AR,AL\n", },

{ LS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	salb AR,AL\n", },

{ LS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,			TANY,
		0,	RLEFT,
		"	salb AR,AL\n", },

/* (u)longlong right shift is emulated */
{ RS,	INCREG,
	SCREG,	TLL,
	SHCH,	TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"ZO", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSWORD,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	sarl AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSWORD,
	SCON,			TANY,
		0,		RLEFT,
		"	sarl AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUWORD,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shrl AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUWORD,
	SCON,			TANY,
		0,		RLEFT,
		"	shrl AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	sarw AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT,
	SCON,			TANY,
		0,		RLEFT,
		"	sarw AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shrw AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT,
	SCON,			TANY,
		0,		RLEFT,
		"	shrw AR,AL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	sarb AR,AL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR,
	SCON,			TANY,
		0,		RLEFT,
		"	sarb AR,AL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TUCHAR,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shrb AR,AL\n", },

{ RS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TUCHAR,
	SCON,			TANY,
		0,		RLEFT,
		"	shrb AR,AL\n", },

/*
 * The next rules takes care of assignments. "=".
 */
{ ASSIGN,	FOREFF,
	SNAME,	TWORD|TPOINT,
	SZERO,	TANY,
		0,	0,
		"	dzm AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,		TANY,
		0,	RDEST,
		"XXX	movl AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SNAME,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RDEST,
		"	dac AL\n", },

{ STASG,	INAREG|FOREFF,
	SOREG|SNAME,	TANY,
	SAREG,		TPTRTO|TANY,
		NSPECIAL|NAREG,	RDEST,
		"F	movl %esi,A1\nZQF	movl A1,%esi\n", },

/*
 * DIV/MOD/MUL 
 */
/* long long div is emulated */
{ DIV,	INCREG,
	SCREG|SNAME|SOREG|SCON, TLL,
	SCREG|SNAME|SOREG|SCON, TLL,
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
		"ZO", },

{ DIV,	INAREG,
	SAREG,			TSWORD,
	SAREG|SNAME|SOREG,	TWORD,
		NSPECIAL,	RDEST,
		"	cltd\n	idivl AR\n", },

{ DIV,	INAREG,
	SAREG,			TUWORD|TPOINT,
	SAREG|SNAME|SOREG,	TUWORD|TPOINT,
		NSPECIAL,	RDEST,
		"	xorl %edx,%edx\n	divl AR\n", },

{ DIV,	INAREG,
	SAREG,			TUSHORT,
	SAREG|SNAME|SOREG,	TUSHORT,
		NSPECIAL,	RDEST,
		"	xorl %edx,%edx\n	divw AR\n", },

{ DIV,	INCH,
	SHCH,			TUCHAR,
	SHCH|SNAME|SOREG,	TUCHAR,
		NSPECIAL,	RDEST,
		"	xorb %ah,%ah\n	divb AR\n", },

{ DIV,	INFL,
	SHFL,		TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	fdivl AR\n", },

{ DIV,	INFL,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fdivZAp\n", },

/* (u)longlong mod is emulated */
{ MOD,	INCREG,
	SCREG|SNAME|SOREG|SCON, TLL,
	SCREG|SNAME|SOREG|SCON, TLL,
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
		"ZO", },

{ MOD,	INAREG,
	SAREG,			TSWORD,
	SAREG|SNAME|SOREG,	TSWORD,
		NAREG|NSPECIAL,	RESC1,
		"	cltd\n	idivl AR\n", },

{ MOD,	INAREG,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG,	TUWORD|TPOINT,
		NAREG|NSPECIAL,	RESC1,
		"	xorl %edx,%edx\n	divl AR\n", },

{ MOD,	INAREG,
	SAREG,			TUSHORT,
	SAREG|SNAME|SOREG,	TUSHORT,
		NAREG|NSPECIAL,	RESC1,
		"	xorl %edx,%edx\n	divw AR\n", },

{ MOD,	INCH,
	SHCH,			TUCHAR,
	SHCH|SNAME|SOREG,	TUCHAR,
		NBREG|NSPECIAL,	RESC1,
		"	xorb %ah,%ah\n	divb AR\n", },

/* (u)longlong mul is emulated */
{ MUL,	INCREG,
	SCREG,	TLL,
	SCREG,	TLL,
		NSPECIAL,	RDEST,
		"ZO", },

{ MUL,	INAREG,
	SAREG,				TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,		TWORD|TPOINT,
		0,	RLEFT,
		"	imull AR,AL\n", },

{ MUL,	INAREG,
	SAREG,			TSHORT|TUSHORT,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		"	imulw AR,AL\n", },

{ MUL,	INCH,
	SHCH,			TCHAR|TUCHAR,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
		NSPECIAL,	RDEST,
		"	imulb AR\n", },

{ MUL,	INFL,
	SHFL,		TDOUBLE,
	SNAME|SOREG,	TDOUBLE,
		0,	RLEFT,
		"	fmull AR\n", },

{ MUL,	INFL,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,		TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fmulp\n", },

/*
 * Indirection operators.
 */
{ UMUL,	INLL,
	SANY,	TANY,
	SOREG,	TLL,
		NCREG,	RESC1,
		"	movl UL,U1\n	movl AL,A1\n", },

{ UMUL,	INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TPOINT|TWORD,
		NAREG|NASL,	RESC1,
		"	movl AL,A1\n", },

{ UMUL,	INCH,
	SANY,	TANY,
	SOREG,	TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"	movb AL,A1\n", },

{ UMUL,	INAREG,
	SANY,	TANY,
	SOREG,	TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	movw AL,A1\n", },

{ UMUL,	INFL,
	SANY,	TANY,
	SOREG,	TLDOUBLE,
		NDREG|NDSL,	RESC1,
		"	fldt AL\n", },

{ UMUL,	INFL,
	SANY,	TANY,
	SOREG,	TDOUBLE,
		NDREG|NDSL,	RESC1,
		"	fldl AL\n", },

{ UMUL,	INFL,
	SANY,	TANY,
	SOREG,	TFLOAT,
		NDREG|NDSL,	RESC1,
		"	flds AL\n", },

/*
 * Logical/branching operators
 */

/* Comparisions, take care of everything */
{ OPLOG,	FORCC,
	SAREG,	TWORD|TPOINT,
	SNAME,	TWORD|TPOINT,
		0, 	RESCC,
		"	cma ; tad AR ; cma\n", },

#if 0
{ OPLOG,	FORCC,
	SCON|SAREG,	TWORD|TPOINT,
	SAREG|SOREG|SNAME,	TWORD|TPOINT,
		0, 	RESCC,
		"	cmpl AR,AL\n", },
#endif

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
		NSPECIAL, 	RNOP,
		"ZG", },

{ OPLOG,	FORCC,
	SANY,	TANY,
	SANY,	TANY,
		REWRITE,	0,
		"diediedie!", },

/* AND/OR/ER/NOT */
{ AND,	INAREG|FOREFF,
	SAREG|SOREG|SNAME,	TWORD,
	SCON|SAREG,		TWORD,
		0,	RLEFT,
		"	andl AR,AL\n", },

{ AND,	INCREG|FOREFF,
	SCREG,			TLL,
	SCREG|SOREG|SNAME,	TLL,
		0,	RLEFT,
		"	andl AR,AL\n	andl UR,UL\n", },

{ AND,	INAREG|FOREFF,
	SAREG,			TWORD,
	SAREG|SOREG|SNAME,	TWORD,
		0,	RLEFT,
		"	andl AR,AL\n", },

{ AND,	INAREG|FOREFF,  
	SAREG|SOREG|SNAME,	TSHORT|TUSHORT,
	SCON|SAREG,		TSHORT|TUSHORT,
		0,	RLEFT,
		"	andw AR,AL\n", },

{ AND,	INAREG|FOREFF,  
	SAREG,			TSHORT|TUSHORT,
	SAREG|SOREG|SNAME,	TSHORT|TUSHORT,
		0,	RLEFT,
		"	andw AR,AL\n", },

{ AND,	INBREG|FOREFF,
	SBREG|SOREG|SNAME,	TCHAR|TUCHAR,
	SCON|SBREG,		TCHAR|TUCHAR,
		0,	RLEFT,
		"	andb AR,AL\n", },

{ AND,	INBREG|FOREFF,
	SBREG,			TCHAR|TUCHAR,
	SBREG|SOREG|SNAME,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	andb AR,AL\n", },
/* AND/OR/ER/NOT */

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jmp LL\n", },

#if defined(GCC_COMPAT) || defined(LANG_F77)
{ GOTO, 	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jmp *AL\n", },
#endif

/*
 * Convert LTYPE to reg.
 */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	lac ZB\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	lac AL\n", },

/*
 * Negate a word.
 */

{ UMINUS,	INCREG|FOREFF,
	SCREG,	TLL,
	SCREG,	TLL,
		0,	RLEFT,
		"	negl AL\n	adcl $0,UL\n	negl UL\n", },

{ UMINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	negl AL\n", },

{ UMINUS,	INAREG|FOREFF,
	SAREG,	TSHORT|TUSHORT,
	SAREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		"	negw AL\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG,	TCHAR|TUCHAR,
	SBREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	negb AL\n", },

{ UMINUS,	INFL|FOREFF,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fchs\n", },

{ COMPL,	INCREG,
	SCREG,	TLL,
	SANY,	TANY,
		0,	RLEFT,
		"	notl AL\n	notl UL\n", },

{ COMPL,	INAREG,
	SAREG,	TWORD,
	SANY,	TANY,
		0,	RLEFT,
		"	notl AL\n", },

{ COMPL,	INAREG,
	SAREG,	TSHORT|TUSHORT,
	SANY,	TANY,
		0,	RLEFT,
		"	notw AL\n", },

{ COMPL,	INBREG,
	SBREG,	TCHAR|TUCHAR,
	SANY,	TANY,
		0,	RLEFT,
		"	notb AL\n", },

/*
 * Arguments to functions.
 */
{ FUNARG,	FOREFF,
	SAREG,	TWORD|TPOINT,
	SANY,	TWORD|TPOINT,
		0,	RNULL,
		"	dac ZA\n", },

{ STARG,	FOREFF,
	SAREG,	TPTRTO|TSTRUCT,
	SANY,	TSTRUCT,
		NSPECIAL,	0,
		"ZF", },

# define DF(x) FORREW,SANY,TANY,SANY,TANY,REWRITE,x,""

{ UMUL, DF( UMUL ), },

{ ASSIGN, DF(ASSIGN), },

{ STASG, DF(STASG), },

{ FLD, DF(FLD), },

{ OPLEAF, DF(NAME), },

/* { INIT, DF(INIT), }, */

{ OPUNARY, DF(UMINUS), },

{ OPANY, DF(BITYPE), },

{ FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	"help; I'm in trouble\n" },
};

int tablesize = sizeof(table)/sizeof(table[0]);
