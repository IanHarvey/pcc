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
# define TUWORD TUNSIGNED
# define TSWORD TINT
# define TWORD TUWORD|TSWORD
# define ANYSH	SCON|SAREG|SOREG|SNAME
# define ARONS	SAREG|SOREG|SNAME|STARNM

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/* PCONVs are usually not necessary */
{ PCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"", },

/* convert char to int */
{ SCONV,	INAREG,
	SAREG,	TCHAR,
	SAREG,	TINT,
		NAREG|NASL,	RESC1,
		"", }, /* chars are stored as ints in registers */

{ SCONV,	INAREG,
	SOREG|SCON|SNAME,	TCHAR,
	SAREG,	TINT,
		NAREG|NASL,	RESC1,
		"movb	AL,A1\n", },

/* convert uchar to int */
{ SCONV,	INAREG,
	SAREG|SOREG|SCON|SNAME,	TUCHAR,
	SAREG,	TINT,
		NAREG,	RESC1,
		"clr	A1\nbisb	AL,A1\n", },

/* convert (u)int to (u)char.  NOthing to do. */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SANY,	TCHAR|TUCHAR,
		0,	RLEFT,
		"", },

/* convert (u)int to (u)int */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SANY,	TWORD,
		0,	RLEFT,
		"", },

/* convert int to long from memory */
{ SCONV,	INBREG,
	SNAME|SOREG,	TINT,
	SANY,	TLONG,
		NBREG,	RESC1,
		"mov	AL,U1\nsxt	A1\n", },

/* int -> (u)long. XXX - only in r0 and r1 */
{ SCONV,	INBREG,
	SAREG,	TINT,
	SANY,	TLONG|TULONG,
		NSPECIAL|NBREG|NBSL,	RESC1,
		"tst	AL\nsxt	r0\n", },

/* unsigned -> (u)long. XXX - only in r0 and r1 */
{ SCONV,	INBREG,
	SAREG,	TUNSIGNED,
	SANY,	TLONG|TULONG,
		NSPECIAL|NBREG|NBSL,	RESC1,
		"clr	r0\n", },

/* uint -> double */
{ SCONV,	INCREG,
	SAREG|SNAME|SOREG|SCON,	TUNSIGNED,
	SANY,			TFLOAT|TDOUBLE,
		NCREG|NCSL,	RESC1,
		"mov	AL,-(sp)\nclr	-(sp)\n"
		"setl\nmovif	(sp)+,A1\nseti\n", },

/* long -> int */
{ SCONV,	INAREG,
	SBREG,	TLONG|TULONG,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"mov	UL,A1\n", },


/* (u)long -> (u)long, nothing */
{ SCONV,	INBREG,
	SBREG,	TLONG|TULONG,
	SANY,	TLONG|TULONG,
		NBREG|NBSL,	RESC1,
		"", },

/* long -> double */
{ SCONV,	INCREG,
	SBREG|SNAME|SOREG|SCON,	TLONG,
	SANY,		TFLOAT|TDOUBLE,
		NCREG|NCSL,	RESC1,
		"mov	UL,-(sp)\nmov	AL,-(sp)\n"
		"setl\nmovif	(sp)+,A1\nseti\n", },


#if 0
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
		"	pushw AL\n	fild (%esp)\n	addl $2,%esp\n", },

/* convert unsigned short to double XXX - use NTEMP */
{ SCONV,	INFL,
	SAREG|SOREG|SNAME,	TUSHORT,
	SHFL,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NASL|NDREG|NTEMP,	RESC2,
		"	movzwl AL,A1\n	pushl A1\n"
		"	fildl (%esp)\n	addl $4,%esp\n", },

/* int to something */

/* convert int to char. This is done when register is loaded */
{ SCONV,	INCH,
	SAREG,	TWORD|TPOINT,
	SANY,	TCHAR|TUCHAR,
		NSPECIAL|NBREG|NBSL,	RESC1,
		"ZM", },

/* convert int to short. Nothing to do */
{ SCONV,	INAREG,
	SAREG,	TWORD,
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
		NBREG|NBSL,	RESC1,
		"	movb AL,A1\n", },

/* convert (u)long long to (u)char (reg->reg, hopefully nothing) */
{ SCONV,	INCH,
	SHLL,	TLL,
	SANY,	TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
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
		NAREG|NASL,	RESC1,
		"ZS", },

/* convert long long to int (mem->reg) */
{ SCONV,	INAREG,
	SOREG|SNAME,	TLL,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	movl AL,A1\n", },

/* convert long long to int (reg->reg, hopefully nothing) */
{ SCONV,	INAREG,
	SHLL|SOREG|SNAME,	TLL,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"ZS", },

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
	SCREG,	TULONGLONG,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NDREG,	RESC1,
		"ZJ", },

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
#ifdef notdef	/* Must round down and nothing else */
		"	subl $4,%esp\n	fistpl (%esp)\n	popl A1\n", },
#else
		"	subl $12,%esp\n"
		"	fnstcw (%esp)\n"
		"	fnstcw 4(%esp)\n"
		"	movb $12,1(%esp)\n"
		"	fldcw (%esp)\n"
		"	fistpl 8(%esp)\n"
		"	movl 8(%esp),A1\n"
		"	fldcw 4(%esp)\n"
		"	addl $12,%esp\n", },
#endif

/* convert float/double (in register) to (unsigned) long long */
/* XXX - unsigned is not handled correct */
{ SCONV,	INLL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHLL,	TLONGLONG|TULONGLONG,
		NCREG,	RESC1,
#ifdef notdef	/* Must round down and nothing else */
		"	subl $8,%esp\n	fistpq (%esp)\n"
		"	popl A1\n	popl U1\n", },
#else
		"	subl $16,%esp\n"
		"	fnstcw (%esp)\n"
		"	fnstcw 4(%esp)\n"
		"	movb $12,1(%esp)\n"
		"	fldcw (%esp)\n"
		"	fistpq 8(%esp)\n"
		"	movl 8(%esp),A1\n"
		"	movl 12(%esp),U1\n"
		"	fldcw 4(%esp)\n"
		"	addl $16,%esp\n", },
#endif

/* slut sconv */
#endif

/*
 * Subroutine calls.
 */
{ CALL,		INBREG,
	SCON,	TANY,
	SBREG,	TLONG|TULONG,
		NBREG|NBSL,	RESC1,
		"jsr	pc,*CL\nZC", },

{ UCALL,	INBREG,
	SCON,	TANY,
	SBREG,	TLONG|TULONG,
		NBREG|NBSL,	RESC1,
		"jsr	pc,*CL\n", },

{ CALL,		FOREFF,
	SCON|SNAME|SOREG,	TANY,
	SANY,	TANY,
		0,	0,
		"jsr	pc,*AL\nZC", },

{ UCALL,	FOREFF,
	SCON|SNAME|SOREG,	TANY,
	SANY,	TANY,
		0,	0,
		"jsr	pc,*AL\n", },

{ CALL,		INAREG,
	SCON|SOREG|SNAME,	TANY,
	SAREG,	TWORD|TPOINT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"jsr	pc,*AL\nZC", },

{ UCALL,	INAREG,
	SCON|SOREG|SNAME,	TANY,
	SAREG,	TWORD|TPOINT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"jsr	pc,*AL\n", },

{ CALL,		FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"jsr	pc,(AL)\nZC", },

{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"jsr	pc,(AL)\n", },

{ CALL,		INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"jsr	pc,(AL)\nZC", },

{ UCALL,	INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"jsr	pc,(AL)\n", },

#if 0
{ CALL,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call CL\nZC", },

{ UCALL,	FOREFF,
	SCON,	TANY,
	SAREG,	TWORD|TPOINT,
		0,	0,
		"	call CL\n", },

{ CALL,	INAREG,
	SCON,	TANY,
	SAREG,	TSHORT|TUSHORT|TWORD|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INAREG,
	SCON,	TANY,
	SAREG,	TSHORT|TUSHORT|TWORD|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\n", },

{ CALL,	INBREG,
	SCON,	TANY,
	SBREG,	TCHAR|TUCHAR,
		NBREG,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INBREG,
	SCON,	TANY,
	SBREG,	TCHAR|TUCHAR,
		NBREG,	RESC1,	/* should be 0 */
		"	call CL\n", },

{ CALL,		INCREG,
	SCON,	TANY,
	SCREG,	TANY,
		NCREG|NCSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INCREG,
	SCON,	TANY,
	SCREG,	TANY,
		NCREG|NCSL,	RESC1,	/* should be 0 */
		"	call CL\n", },

{ CALL,	INDREG,
	SCON,	TANY,
	SDREG,	TANY,
		NDREG|NDSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INDREG,
	SCON,	TANY,
	SDREG,	TANY,
		NDREG|NDSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ CALL,		FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"	call *AL\nZC", },

{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"	call *AL\nZC", },

{ CALL,		INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ UCALL,	INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ CALL,		INBREG,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ UCALL,	INBREG,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ CALL,		INCREG,
	SAREG,	TANY,
	SANY,	TANY,
		NCREG|NCSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ UCALL,	INCREG,
	SAREG,	TANY,
	SANY,	TANY,
		NCREG|NCSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ CALL,		INDREG,
	SAREG,	TANY,
	SANY,	TANY,
		NDREG|NDSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

{ UCALL,	INDREG,
	SAREG,	TANY,
	SANY,	TANY,
		NDREG|NDSL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

/* struct return */
{ USTCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	0,
		"ZP	call CL\nZC", },

{ USTCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call CL\nZC", },

{ USTCALL,	INAREG,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call *AL\nZC", },

{ STCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	0,
		"ZP	call CL\nZC", },

{ STCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call CL\nZC", },

{ STCALL,	INAREG,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call *AL\nZC", },

#endif
/*
 * The next rules handle all binop-style operators.
 */
/* Add one to anything left but use only for side effects */
{ PLUS,		FOREFF|INAREG|FORCC,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SONE,			TANY,
		0,	RLEFT|RESCC,
		"inc	AL\n", },

{ PLUS,		INBREG|FOREFF,
	SBREG,			TLONG,
	SBREG|SNAME|SOREG|SCON,	TLONG,
		0,	RLEFT,
		"add	AR,AL\nadd	UR,UL\nadc	AL\n", },

/* Add to reg left and reclaim reg */
{ PLUS,		INAREG|FOREFF|FORCC,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RLEFT|RESCC,
		"add	AR,AL\n", },

/* Add to anything left but use only for side effects */
{ PLUS,		FOREFF|INAREG|FORCC,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RLEFT|RESCC,
		"add	AR,AL\n", },

{ PLUS,		INAREG|FOREFF|FORCC,
	SAREG,			TCHAR|TUCHAR,
	SAREG|SNAME|SOREG|SCON,	TCHAR|TUCHAR,
		0,	RLEFT|RESCC,
		"add	AR,AL\n", },

{ MINUS,		INBREG|FOREFF,
	SBREG,			TLONG|TULONG,
	SBREG|SNAME|SOREG|SCON,	TLONG|TULONG,
		0,	RLEFT,
		"sub	AR,AL\nsub	UR,UL\nsbc	AL\n", },

/* Sub one from anything left */
{ MINUS,	FOREFF|INAREG|FORCC,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SONE,			TANY,
		0,	RLEFT|RESCC,
		"dec	AL\n", },

{ MINUS,		INAREG|FOREFF,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"sub	AR,AL\n", },

/* Sub from anything left but use only for side effects */
{ MINUS,	FOREFF|INAREG|FORCC,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RLEFT|RESCC,
		"sub	AR,AL\n", },

/* Sub one left but use only for side effects */
{ MINUS,	FOREFF|FORCC,
	SAREG|SNAME|SOREG,	TCHAR|TUCHAR,
	SONE,			TANY,
		0,	RLEFT|RESCC,
		"decb	AL\n", },

/* Sub from anything left but use only for side effects */
{ MINUS,		FOREFF|FORCC,
	SAREG|SNAME|SOREG,	TCHAR|TUCHAR,
	SAREG|SNAME|SOREG|SCON,	TCHAR|TUCHAR|TWORD|TPOINT,
		0,	RLEFT|RESCC,
		"subb	AR,AL\n", },


#if 0
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

{ PLUS,		INFL|FOREFF,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	faddp\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	incl AL\n", },

{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	leal CR(AL),A1\n", },

{ PLUS,		INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SONE,	TANY,
		0,	RLEFT,
		"	incb AL\n", },

{ PLUS,		INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL|NASR,	RESC1,
		"	leal (AL,AR),A1\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SONE,			TANY,
		0,	RLEFT,
		"	decl AL\n", },

{ MINUS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SONE,	TANY,
		0,	RLEFT,
		"	decb AL\n", },

/* address as register offset, negative */
{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT,
	SPCON,	TANY,
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

{ MINUS,	INFL|FOREFF,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fsubZAp\n", },

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
		"	Ob AR,AL\n", },

{ OPSIMP,	INLL|FOREFF,
	SHLL,	TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	Ol AR,AL\n	Ol UR,UL\n", },
#endif

/*
 * The next rules handle all shift operators.
 */
{ LS,	INBREG|FOREFF,
	SBREG,	TLONG|TULONG,
	SANY,	TANY,
		0,	RLEFT,
		"ashc	AR,AL\n", },

{ LS,	INAREG|FOREFF,
	SAREG,	TWORD,
	SONE,	TANY,
		0,	RLEFT,
		"asl	AL\n", },

{ LS,	INAREG|FOREFF,
	SAREG,	TWORD,
	ANYSH,	TWORD,
		0,	RLEFT,
		"ash	AR,AL\n", },

#if 0
/* (u)longlong left shift is emulated */
{ LS,	INCREG,
	SCREG|SNAME|SOREG|SCON, TLL,
	SAREG|SNAME|SOREG|SCON, TINT, /* will be int */
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
		"ZO", },

{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TWORD,
	SHCH,		TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	sall AR,AL\n", },

{ LS,	INAREG|FOREFF,
	SAREG,	TWORD,
	SCON,	TANY,
		0,	RLEFT,
		"	sall AR,AL\n", },

{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shlw AR,AL\n", },

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
	SCREG|SNAME|SOREG|SCON, TLL,
	SAREG|SNAME|SOREG|SCON, TINT, /* will be int */
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
		"ZO", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSWORD,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	sarl AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSWORD,
	SCON,			TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
		0,		RLEFT,
		"	sarl AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUWORD,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shrl AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUWORD,
	SCON,			TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT,
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

#endif
/*
 * The next rules takes care of assignments. "=".
 */

/* First optimizations, in lack of weight it uses first found */
/* Start with class A registers */

/* Clear word at address (or reg) */
{ ASSIGN,	FOREFF|INAREG,
	ARONS,	TWORD|TPOINT,
	SZERO,		TANY,
		0,	RDEST,
		"clr	AL\n", },

/* Clear byte at address.  No reg here. */
{ ASSIGN,	FOREFF,
	SNAME|SOREG|STARNM,	TCHAR|TUCHAR,
	SZERO,		TANY,
		0,	RDEST,
		"clrb	AL\n", },

/* Clear byte in reg. must clear the whole register. */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TCHAR|TUCHAR,
	SZERO,	TANY,
		0,	RDEST,
		"clr	AL\n", },

/* The next is class B regs */

/* Clear long at address or in reg */
{ ASSIGN,	FOREFF|INBREG,
	SNAME|SOREG|SBREG,	TLONG|TULONG,
	SZERO,			TANY,
		0,	RDEST,
		"clr	AL\nclr	UL\n", },

/* Save 2 bytes if high-order bits are zero */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TLONG|TULONG,
	SSCON,	TLONG,
		0,	RDEST,
		"mov	UR,UL\nsxt	AL\n", },

/* Must have multiple rules for long otherwise regs may be trashed */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,			TLONG|TULONG,
	SCON|SNAME|SOREG,	TLONG|TULONG,
		0,	RDEST,
		"mov	AR,AL\nmov	UR,UL\n", },

{ ASSIGN,	FOREFF|INBREG,
	SNAME|SOREG,	TLONG|TULONG,
	SBREG,			TLONG|TULONG,
		0,	RDEST,
		"mov	AR,AL\nmov	UR,UL\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TLONG|TULONG,
	SCON|SNAME|SOREG,	TLONG|TULONG,
		0,	0,
		"mov	AR,AL\nmov	UR,UL\n", },

{ ASSIGN,	INBREG|FOREFF,
	SBREG,	TLONG|TULONG,
	SBREG,	TLONG|TULONG,
		0,	RDEST,
		"ZE\n", },

{ ASSIGN,	FOREFF|INAREG|FORCC,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RDEST|RESCC,
		"mov	AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG|FORCC,
	ARONS,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RDEST|RESCC,
		"mov	AR,AL\n", },

{ ASSIGN,	FOREFF|FORCC,
	SNAME|SOREG,		TWORD|TPOINT,
	SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RESCC,
		"mov	AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG|FORCC,
	SAREG,		TCHAR|TUCHAR,
	ARONS|SCON,	TCHAR|TUCHAR,
		0,	RDEST|RESCC,
		"movb	AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG|FORCC,
	ARONS,	TCHAR|TUCHAR,
	SAREG,	TCHAR|TUCHAR,
		0,	RDEST|RESCC,
		"movb	AR,AL\n", },

{ ASSIGN,	FOREFF|FORCC,
	SNAME|SOREG|STARNM,		TCHAR|TUCHAR,
	SNAME|SOREG|SCON|STARNM,	TCHAR|TUCHAR,
		0,	RDEST|RESCC,
		"movb	AR,AL\n", },

{ ASSIGN,	FOREFF|INCREG,
	SCREG,		TDOUBLE,
	SNAME|SOREG|SCON,	TDOUBLE,
		0,	RDEST,
		"movf	AR,AL\n", },

{ ASSIGN,	FOREFF|INCREG,
	SCREG,		TFLOAT,
	SNAME|SOREG|SCON,	TFLOAT,
		0,	RDEST,
		"movof	AR,AL\n", },

{ ASSIGN,	FOREFF|INCREG,
	SNAME|SOREG|SCREG,	TDOUBLE,
	SCREG,			TDOUBLE,
		0,	RDEST,
		"movf	AR,AL\n", },

{ ASSIGN,	FOREFF|INCREG,
	SNAME|SOREG|SCREG,	TFLOAT,
	SCREG,			TFLOAT,
		0,	RDEST,
		"movfo	AR,AL\n", },


#if 0
{ ASSIGN,	FOREFF|INLL,
	SHLL,		TLL,
	SCON,		TANY,
		0,	RDEST,
		"	movl AR,AL\n	movl UR,UL\n", },

{ ASSIGN,	FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SCON,		TANY,
		0,	0,
		"	movl AR,AL\n	movl UR,UL\n", },

{ ASSIGN,	FORCC|FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SMIXOR,		TANY,
		0,	RDEST,
		"	xorl AL,AL\n", },

{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SCON,		TANY,
		0,	0,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,		TANY,
		0,	RDEST,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SCON,		TANY,
		0,	0,
		"	movw AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TSHORT|TUSHORT,
	SCON,		TANY,
		0,	RDEST,
		"	movw AR,AL\n", },

{ ASSIGN,	FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,		TANY,
		0,	0,
		"	movb AR,AL\n", },

{ ASSIGN,	FOREFF|INCH,
	SHCH,		TCHAR|TUCHAR,
	SCON,		TANY,
		0,	RDEST,
		"	movb AR,AL\n", },

{ ASSIGN,	FOREFF|INLL,
	SHLL|SNAME|SOREG,	TLL,
	SHLL,			TLL,
		0,	RDEST,
		"	movl AR,AL\n	movl UR,UL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SAREG,		TWORD|TPOINT,
		0,	RDEST,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,			TWORD|TPOINT,
	SAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RDEST,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	SAREG,		TSHORT|TUSHORT,
		0,	RDEST,
		"	movw AR,AL\n", },

{ ASSIGN,	FOREFF|INCH,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SHCH,		TCHAR|TUCHAR|TWORD,
		0,	RDEST,
		"	movb AR,AL\n", },

{ ASSIGN,	FOREFF|INBREG,
	SFLD,		TCHAR|TUCHAR,
	SBREG|SCON,	TCHAR|TUCHAR,
		NAREG|NBREG,	RDEST,
		"	movb AR,A2\n"
		"	movzbl A2,A1\n"
		"	andl $N,AL\n"
		"	sall $H,A1\n"
		"	andl $M,A1\n"
		"	orl A1,AL\n"
		"F	movb AR,AD\n"
		"FZE", },

{ ASSIGN,	FOREFF|INAREG,
	SFLD,		TSHORT|TUSHORT,
	SAREG|SCON,	TSHORT|TUSHORT,
		NAREG,	RDEST,
		"	movw AR,A1\n"
		"	movzwl A1,ZN\n"
		"	andl $N,AL\n"
		"	sall $H,ZN\n"
		"	andl $M,ZN\n"
		"	orl ZN,AL\n"
		"F	movw AR,AD\n"
		"FZE", },

{ ASSIGN,	FOREFF|INAREG,
	SFLD,		TWORD,
	SAREG|SNAME|SOREG|SCON,	TWORD,
		NAREG,	RDEST,
		"	movl AR,A1\n"
		"	andl $N,AL\n"
		"	sall $H,A1\n"
		"	andl $M,A1\n"
		"	orl A1,AL\n"
		"F	movl AR,AD\n"
		"FZE", },

{ ASSIGN,	INDREG|FOREFF,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"", }, /* This will always be in the correct register */

/* order of table entries is very important here! */
{ ASSIGN,	INFL,
	SNAME|SOREG,	TLDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	fst AL\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TLDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	0,
		"	fstpt AL\n", },

{ ASSIGN,	INFL,
	SNAME|SOREG,	TDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	fstl AL\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	0,
		"	fstpl AL\n", },

{ ASSIGN,	INFL,
	SNAME|SOREG,	TFLOAT,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
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
		0,	RDEST,
		"	fldt AR\n", },

{ ASSIGN,	INFL|FOREFF,
	SHFL,		TDOUBLE,
	SHFL|SOREG|SNAME,	TDOUBLE,
		0,	RDEST,
		"	fldl AR\n", },

{ ASSIGN,	INFL|FOREFF,
	SHFL,		TFLOAT,
	SHFL|SOREG|SNAME,	TFLOAT,
		0,	RDEST,
		"	flds AR\n", },

/* Do not generate memcpy if return from funcall */
#if 0
{ STASG,	INAREG|FOREFF,
	SOREG|SNAME|SAREG,	TPTRTO|TSTRUCT,
	SFUNCALL,	TPTRTO|TSTRUCT,
		0,	RRIGHT,
		"", },
#endif

#endif
/* XXX - optimize for small structs */
{ STASG,	FOREFF,
	SAREG,	TPTRTO|TANY,
	SAREG,	TPTRTO|TSTRUCT,
		NAREG,	0,
		"ZQ", },

{ STASG,	INAREG|FOREFF,
	SAREG,	TPTRTO|TANY,
	SAREG,	TPTRTO|TSTRUCT,
		NAREG*2,	RRIGHT,
		"ZR", },

#if 0
{ STASG,	INAREG|FOREFF,
	SOREG|SNAME,	TANY,
	SAREG|SOREG|SNAME,	TPTRTO|TANY,
		NSPECIAL,	RRIGHT,
		"ZQ", },
#endif

/*
 * DIV/MOD/MUL 
 */
/* XXX - mul may use any odd register, only r1 for now */
{ MUL,	INAREG,
	SAREG,				TWORD|TPOINT,
	SAREG|SNAME|SOREG|SCON,		TWORD|TPOINT,
		NSPECIAL,	RLEFT,
		"mul	AR,AL\n", },

{ MUL,	INBREG,
	SBREG|SNAME|SCON|SOREG,		TLONG|TULONG,
	SBREG|SNAME|SCON|SOREG,		TLONG|TULONG,
		NSPECIAL|NBREG|NBSL|NBSR,		RESC1,
		"mov	UR,-(sp)\nmov	AR,-(sp)\n"
		"mov	UL,-(sp)\nmov	AL,-(sp)\n"
		"jsr	pc,lmul\nadd	$10,sp\n", },

{ MUL,	INCREG,
	SCREG,				TFLOAT|TDOUBLE,
	SCREG|SNAME|SOREG,		TFLOAT|TDOUBLE,
		0,	RLEFT,
		"mulf	AR,AL\n", },

/* need extra move to be sure N flag is correct for sxt */
{ DIV,	INAREG,
	ANYSH,		TWORD|TPOINT,
	ANYSH,		TWORD|TPOINT,
		NSPECIAL,	RDEST,
		"mov	AL,r1\nsxt	r0\ndiv	AR,r0\n", },

{ DIV,	INBREG,
	SBREG|SNAME|SCON|SOREG,		TLONG|TULONG,
	SBREG|SNAME|SCON|SOREG,		TLONG|TULONG,
		NSPECIAL|NBREG|NBSL|NBSR,		RESC1,
		"mov	UR,-(sp)\nmov	AR,-(sp)\n"
		"mov	UL,-(sp)\nmov	AL,-(sp)\n"
		"jsr	pc,ldiv\nadd	$10,sp\n", },

{ DIV,	INCREG,
	SCREG,			TFLOAT|TDOUBLE,
	SCREG|SNAME|SOREG,	TFLOAT|TDOUBLE,
		0,	RLEFT,
		"divf	AR,AL\n", },

#if 0
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
	SCREG|SNAME|SOREG|SCON, TLL,
	SCREG|SNAME|SOREG|SCON, TLL,
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
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
#endif
/*
 * Indirection operators.
 */
{ UMUL,	INBREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TLONG|TULONG,
		NBREG,	RESC1, /* |NBSL - may overwrite index reg */
		"mov	AR,A1\nmov	UR,U1\n", },

{ UMUL,	INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TPOINT|TWORD,
		NAREG|NASL,	RESC1,
		"mov AR,A1\n", },

#if 0
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
#endif
/*
 * Logical/branching operators
 */
{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
	SZERO,	TANY,
		0, 	RESCC,
		"tst	AL\n", },

{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME|SCON,	TCHAR|TUCHAR,
	SZERO,	TANY,
		0, 	RESCC,
		"tstb	AL\n", },

{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
	SAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
		0, 	RESCC,
		"cmp	AL,AR\n", },

{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME|SCON,	TCHAR|TUCHAR,
	SAREG|SOREG|SNAME|SCON,	TCHAR|TUCHAR,
		0, 	RESCC,
		"cmpb	AL,AR\n", },

{ OPLOG,	FORCC,
	SBREG|SOREG|SNAME|SCON,	TLONG|TULONG,
	SZERO,	TANY,
		0,	RNULL,
		"ZD", },

{ OPLOG,	FORCC,
	SBREG|SOREG|SNAME,	TLONG|TULONG,
	SBREG|SOREG|SNAME,	TLONG|TULONG,
		0,	RNULL,
		"ZF", },

#if 0
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
	SCON|SAREG,	TWORD|TPOINT,
	SAREG|SOREG|SNAME,	TWORD|TPOINT,
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
		NSPECIAL, 	0,
		"ZG", },

{ OPLOG,	FORCC,
	SOREG|SNAME,	TDOUBLE|TFLOAT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NSPECIAL, 	0,
		"ZG", },

#if 0
/* Ppro and later only */
{ OPLOG,	FORCC,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		0, 	RESCC,
		"ZA	fucomip %st,%st(1)\n", },
#endif

{ OPLOG,	FORCC,
	SANY,	TANY,
	SANY,	TANY,
		REWRITE,	0,
		"diediedie!", },

#endif

/* AND/OR/ER/NOT */
/* Optimize if high order bits are zero */
{ AND,	FOREFF|INBREG|FORCC,
	SOREG|SNAME|SBREG,	TLONG|TULONG,
	SANDSCON,		TLONG|TULONG,
		0,	RLEFT|RESCC,
		"clr	AL\nbic	UR,UL\n", },

{ AND,	INBREG|FORCC,
	SBREG,			TLONG|TULONG,
	SCON|SBREG|SOREG|SNAME,	TLONG|TULONG,
		0,	RLEFT|RESCC,
		"bic	AR,AL\nbic	UR,UL\n", },

{ AND,	INAREG|FORCC,
	SAREG,			TWORD,
	SCON|SAREG|SOREG|SNAME,	TWORD,
		0,	RLEFT|RESCC,
		"bic	AR,AL\n", },

{ OR,	INBREG|FORCC,
	SBREG,			TLONG|TULONG,
	SCON|SBREG|SOREG|SNAME,	TLONG|TULONG,
		0,	RLEFT|RESCC,
		"bis	AR,AL\nbis	UR,UL\n", },

{ OR,	INAREG|FORCC,
	SAREG,			TWORD,
	SCON|SAREG|SOREG|SNAME,	TWORD,
		0,	RLEFT|RESCC,
		"bis	AR,AL\n", },

#if 0
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

#endif
/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"jbr	LL\n", },

#if 0

#if defined(GCC_COMPAT) || defined(LANG_F77)
{ GOTO, 	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jmp *AL\n", },
#endif

#endif
/*
 * Convert LTYPE to reg.
 */
/* Two bytes less if high half of constant is zero */
{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SSCON,	TLONG|TULONG,
		NBREG,	RESC1,
		"mov	UL,U1\nsxt	A1\n", },

/* XXX - avoid OREG index register to be overwritten */
{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SCON|SBREG|SNAME|SOREG,	TLONG|TULONG,
		NBREG,	RESC1,
		"mov	AL,A1\nmov	UL,U1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG|SCON|SOREG|SNAME,	TWORD|TPOINT,
		NAREG|NASR,	RESC1,
		"mov	AL,A1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG|SCON|SOREG|SNAME,	TCHAR,
		NAREG,		RESC1,
		"movb	AR,A1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG|SCON|SOREG|SNAME,	TUCHAR,
		NAREG,		RESC1,
		"clr	A1\nbisb	AL,A1\n", },

{ OPLTYPE,	INCREG,
	SANY,	TANY,
	SCREG|SCON|SOREG|SNAME,	TDOUBLE,
		NCREG,		RESC1,
		"movf	AL,A1\n", },

{ OPLTYPE,	INCREG,
	SANY,	TANY,
	SCREG|SCON|SOREG|SNAME,	TFLOAT,
		NCREG,		RESC1,
		"movof	AL,A1\n", },

#if 0
{ OPLTYPE,	FORCC|INLL,
	SCREG,	TLL,
	SMIXOR,	TANY,
		NCREG,	RESC1,
		"	xorl U1,U1\n	xorl A1,A1\n", },

{ OPLTYPE,	FORCC|INLL,
	SCREG,	TLL,
	SMILWXOR,	TANY,
		NCREG,	RESC1,
		"	movl UL,U1\n	xorl A1,A1\n", },

{ OPLTYPE,	FORCC|INLL,
	SCREG,	TLL,
	SMIHWXOR,	TANY,
		NCREG,	RESC1,
		"	xorl U1,U1\n	movl AL,A1\n", },

{ OPLTYPE,	INLL,
	SANY,	TANY,
	SCREG|SCON|SOREG|SNAME,	TLL,
		NCREG,	RESC1,
		"	movl UL,U1\n	movl AL,A1\n", },

{ OPLTYPE,	FORCC|INAREG,
	SAREG,	TWORD|TPOINT,
	SMIXOR,	TANY,
		NAREG|NASL,	RESC1,
		"	xorl A1,A1\n", },

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

{ OPLTYPE,	FORCC|INAREG,
	SAREG,	TSHORT|TUSHORT,
	SMIXOR,	TANY,
		NAREG,	RESC1,
		"	xorw A1,A1\n", },

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

/* Only used in ?: constructs. The stack already contains correct value */
{ OPLTYPE,	INDREG,
	SANY,	TFLOAT|TDOUBLE|TLDOUBLE,
	SDREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		NDREG,	RESC1,
		"", },
#endif

/*
 * Negate a word.
 */
{ UMINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT|TCHAR|TUCHAR,
	SANY,	TANY,
		0,	RLEFT,
		"neg	AL\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG|SOREG|SNAME,	TLONG,
	SANY,			TANY,
		0,	RLEFT,
		"neg	AL\nneg	UL\nsbc	AL\n", },


#if 0
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
#endif
{ COMPL,	INBREG,
	SBREG,	TLONG|TULONG,
	SANY,	TANY,
		0,	RLEFT,
		"com	AL\ncom	UL\n", },

{ COMPL,	INAREG,
	SAREG,	TWORD,
	SANY,	TANY,
		0,	RLEFT,
		"com	AL\n", },

#if 0
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

#endif
/*
 * Arguments to functions.
 */
{ FUNARG,	FOREFF,
	SCON|SBREG|SNAME|SOREG,	TLONG|TULONG,
	SANY,	TLONG|TULONG,
		0,	RNULL,
		"mov	UL,ZA(sp)\nmov	AL,-(sp)\n", },

{ FUNARG,	FOREFF,
	SCON|SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SANY,	TWORD|TPOINT,
		0,	RNULL,
		"mov	AL,ZA(sp)\n", },

{ FUNARG,	FOREFF,
	SCON,	TCHAR|TUCHAR,
	SANY,	TANY,
		0,	RNULL,
		"mov	AL,ZA(sp)\n", },

{ FUNARG,	FOREFF,
	SNAME|SOREG,	TCHAR,
	SANY,		TCHAR,
		NAREG,	RNULL,
		"movb	AL,A1\nmov	A1,ZA(sp)\n", },

{ FUNARG,	FOREFF,
	SNAME|SOREG,	TUCHAR,
	SANY,		TUCHAR,
		NAREG,	RNULL,
		"clr	ZA(sp)\nbisb	AL,(sp)\n", },

{ FUNARG,	FOREFF,
	SAREG,	TUCHAR|TCHAR,
	SANY,	TUCHAR|TCHAR,
		0,	RNULL,
		"mov	AL,ZA(sp)\n", },

{ FUNARG,	FOREFF,
	SCREG,	TFLOAT|TDOUBLE,
	SANY,		TANY,
		0,	RNULL,
		"movf	AL,ZA(sp)\n", },

#if 0
{ FUNARG,	FOREFF,
	SCON|SCREG|SNAME|SOREG,	TLL,
	SANY,	TLL,
		0,	RNULL,
		"	pushl UL\n	pushl AL\n", },

{ FUNARG,	FOREFF,
	SCON|SAREG|SNAME|SOREG,	TWORD|TPOINT,
	SANY,	TWORD|TPOINT,
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
	SDREG,	TDOUBLE,
	SANY,		TDOUBLE,
		0,	0,
		"	subl $8,%esp\n	fstpl (%esp)\n", },

{ FUNARG,	FOREFF,
	SNAME|SOREG,	TFLOAT,
	SANY,		TFLOAT,
		0,	0,
		"	pushl AL\n", },

{ FUNARG,	FOREFF,
	SDREG,	TFLOAT,
	SANY,		TFLOAT,
		0,	0,
		"	subl $4,%esp\n	fstps (%esp)\n", },

{ FUNARG,	FOREFF,
	SDREG,	TLDOUBLE,
	SANY,		TLDOUBLE,
		0,	0,
		"	subl $12,%esp\n	fstpt (%esp)\n", },

{ STARG,	FOREFF,
	SAREG|SOREG|SNAME|SCON,	TANY,
	SANY,	TSTRUCT,
		NSPECIAL|NAREG,	0,
		"ZF", },

#endif

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
