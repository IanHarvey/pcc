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

/*
 * MIPS port by Jan Enoksson (janeno-1@student.ltu.se) and
 * Simon Olsson (simols-1@student.ltu.se) 2005.
 *
 * It appears that the target machine was big endian.  The original
 * code contained many endian aspects which are now handled in
 * machine-independent code.
 * 
 * On MIPS, the assembler does an amazing amount of work for us.
 * We don't have to worry about PIC, nor about finding the address
 * of SNAMES.  Whenever possible, we defer the work to the assembler.
 */

#include "pass2.h"

#define TLL TLONGLONG|TULONGLONG
#define ANYSIGNED TINT|TLONG|TSHORT|TCHAR
#define ANYUSIGNED TUNSIGNED|TULONG|TUSHORT|TUCHAR
#define ANYFIXED ANYSIGNED|ANYUSIGNED
#define TUWORD TUNSIGNED|TULONG
#define TSWORD TINT|TLONG
#define TWORD TUWORD|TSWORD

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/*
 * A bunch conversions of integral<->integral types
 */

/* convert char to (u)short */
{ SCONV,	INAREG,
	SOREG,  TCHAR,
	SAREG,	TWORD|SHORT|TUSHORT,
		NAREG,	RESC1,
		"	lb A1,AL	# convert oreg char to (u)short/word\n"
		"	nop\n", },

/* convert uchar to (u)short */
{ SCONV,	INAREG,
	SOREG,  TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT,
		NAREG,	RESC1,
		"	lbu A1,AL	# convert oreg uchar to (u)short\n"
		"	nop\n", },

/* convert char to (u)long long - big endian*/
{ SCONV,	INBREG,
	SOREG,	TCHAR,
	SBREG,	TLL,
		NBREG,	RESC1,
      		"	lb U1,AL	# convert oreg char to (u)longlong\n"
      		"	nop\n"
      		"	sra A1,U1,31\n"
      		"	sub A1,$zero,A1\n", },

/* convert uchar to (u)long long */
{ SCONV,	INBREG,
	SOREG,	TUCHAR,
	SBREG,	TLL,
		NBREG,	RESC1,
		"	lbu U1,AL	# convert oreg uchar to (u)longlong\n"
      		"	move A1,$zero\n", },

/* convert (u)short to char */
{ SCONV,	INAREG,
	SOREG,	TSHORT|TUSHORT,
	SAREG,	TCHAR,
		NAREG,	RESC1,
		"	lb A1,AL	# convert oreg (u)short to char (endianness problem?)\n"
		"	nop\n", },

/* convert (u)short to uchar */
{ SCONV,	INAREG,
	SOREG,	TSHORT|TUSHORT,
	SAREG,  TUCHAR,
		NAREG,	RESC1,
		"	lbu A1,AL	# convert oreg (u)short to uchar (endianness problem?)\n"
		"	nop\n", },

/* convert short to (u)long */
{ SCONV,	INAREG,
	SOREG,	TSHORT,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	lh A1,AL	# convert oreg short to (u)int\n"
		"	nop\n", },

/* convert ushort to (u)long */
{ SCONV,	INAREG,
	SOREG,	TUSHORT,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	lhu A1,AL	# convert oreg ushort to (u)int\n"
		"	nop\n", },

/* convert short to (u)long long */
{ SCONV,	INBREG,
	SOREG,	TSHORT,
	SBREG,	TLL,
		NBREG,	RESC1,
      		"	lh U1,AL	# convert oreg short to (u)longlong\n"
      		"	nop\n"
      		"	sra A1,U1,31\n"
      		"	sub A1,$zero,A1\n", },

/* convert ushort to (u)long long */
{ SCONV,	INBREG,
	SOREG,	TUSHORT,
	SBREG,	TLL,
		NBREG,	RESC1,
		"	lhu U1,AL	# convert oreg (u)short to (u)longlong\n"
      		"	move A1,$zero\n", },

/* convert (u)long to char */
{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TCHAR,
		NAREG,	RESC1,
		"	lb A1,AL	# convert oreg word to char (endianness problem here?)\n"
		"	nop\n", },

/* convert (u)long to uchar */
{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TUCHAR,
		NAREG,	RESC1,
		"	lbu A1,AL	# convert oreg word to uchar (endianness problem here?)\n"
		"	nop\n", },
    
/* convert (u)long to short */
{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TSHORT,
		NAREG,	RESC1,
		"	lh A1,AL	# convert oreg word to short (endianness problem here?)\n"
		"	nop\n", },

/* convert (u)long to ushort */
{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TUSHORT,
		NAREG,	RESC1,
		"	lhu A1,AL	# convert oreg word to ushort (endianness problem here?)\n"
		"	nop\n", },

/* convert long to (u)long long */
{ SCONV,	INBREG,
	SOREG,	TSWORD,
	SBREG,	TLL,
		NBREG,	RESC1,
      		"	lw A1,AL	# convert oreg int/long to (u)llong (endianness problem here?)\n"
      		"	nop\n"
      		"	sra U1,A1,31\n"
      		"	sub U1,$zero,U1\n", },

/* convert ulong to (u)long long */
{ SCONV,	INBREG,
	SOREG,	TUWORD,
	SBREG,	TLL,
		NBREG,	RESC1,
		"	lw A1,AL	# convert oreg (u)int to (u)llong (endianness problem here?)\n"
      		"	move U1,$zero\n", },

/* convert (u)long long to char */
{ SCONV,	INAREG,
	SOREG,	TLL,
	SAREG,	TCHAR,
		NAREG,	RESC1,
		"	lb A1,AL	# convert oreg (u)llong to char	(endianness problem here?)\n"
		"	nop\n", },

/* convert (u)long long to uchar */
{ SCONV,	INAREG,
	SOREG,	TLL,
	SAREG,	TUCHAR,
		NAREG,	RESC1,
		"	lbu A1,AL	# convert oreg (u)llong to uchar (endianness problem?)\n"
		"	nop\n", },
    
/* convert (u)long long to short */
{ SCONV,	INAREG,
	SOREG,	TLL,
	SAREG,	TSHORT,
		NAREG,	RESC1,
		"	lh A1,AL	# convert oreg (u)llong to short (endianness problem?)\n"
		"	nop\n", },

/* convert (u)long long to ushort */
{ SCONV,	INAREG,
	SOREG,	TLL,
	SAREG,	TUSHORT,
		NAREG,	RESC1,
		"	lhu A1,AL	# convert oreg (u)llong to ushort (endianness problem here?)\n"
		"	nop\n", },

/* convert (u)long long to long */
{ SCONV,	INAREG,
	SOREG,	TLL,
	SAREG,	TSWORD,
		NAREG,	RESC1,
      		"	lw U1,AL	# convert oreg (u)llong to short (endianness problem here?)\n"
		"	nop\n", },

/* convert (u)long long to ulong */
{ SCONV,	INAREG,
	SOREG,	TLL,
	SAREG,	TUWORD,
		NAREG,	RESC1,
      		"	lwu U1,AL	# convert oreg (u)longlong to uint (endianness problem here?)\n"
		"	nop\n", },

/* Register to register conversion with long long */

{ SCONV,	INBREG,
	SBREG,	TLL,
	SBREG,	TLL,
		0,	RLEFT,
		"	# convert (u)longlong to (u)longlong", },

{ SCONV,	INBREG,
	SAREG,	TPOINT|TWORD|SHORT|TUSHORT|TCHAR|TUCHAR,
	SBREG,	TLL,
		NBREG,	RESC1,
		"	move A1,AL	# convert (u)int/(u)short/(u)char to (u)longlong\n"
		"	sra A1,AL,31\n", },

{ SCONV,	INAREG,
	SBREG,	TLL,
	SAREG,	TPOINT|TWORD|SHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG,	RESC1,
		"	move A1,AL\n", },

/* For register to register conversion with bit length <= 32, do nothing */

{ SCONV,	INAREG,
	SAREG,	TPOINT|TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TPOINT|TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		0,	RLEFT,
		"	# convert reg to reg", },

{ SCONV,	INCREG,
	SCREG,	TFLOAT,
	SCREG,	TDOUBLE|TLDOUBLE,
		NCREG,	RESC1,
		"	cvt.d.s A1,AL	# convert float to (l)double\n", },

{ SCONV,	INCREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SCREG,	TFLOAT,
		NCREG,	RESC1,
		"	cvt.s.d A1,AL	# convert (l)double to float\n", },

{ SCONV,	INCREG,
	SAREG,	TWORD,
	SCREG,	TFLOAT,
		NCREG,	RESC1,
		"	cvt.s.w A1,AL	# convert (u)int to float\n", },

{ SCONV,	INCREG,
	SAREG,	TWORD,
	SCREG,	TDOUBLE|TLDOUBLE,
		NCREG,	RESC1,
		"	cvt.d.w A1,AL	# convert (u)int to (l)double\n", },

{ SCONV,	INAREG,
	SCREG,	TFLOAT,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	cvt.w.s A1,AL	# convert float to (u)int\n", },

{ SCONV,	INAREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	cvt.w.d A1,AL	# convert (l)double to (u)int\n", },

{ SCONV,	INCREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SCREG,	TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"	# convert between double and ldouble\n", },

{ SCONV,	INCREG,
	SBREG,	TLL,
	SCREG,	TFLOAT,
		NSPECIAL|INCREG,	RESC1,
		"ZF", },

{ SCONV,	INCREG,
	SBREG,	TLL,
	SCREG,	TDOUBLE|TLDOUBLE,
		NSPECIAL|INCREG,	RESC1,
		"ZF", },

{ SCONV,	INBREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TLL,
		NSPECIAL|INBREG,	RESC1,
		"ZF", },

{ SCONV,	INBREG,
	SCREG,	TFLOAT,
	SBREG,	TLL,
		NSPECIAL|INBREG,	RESC1,
		"ZF", },

/*
 * Multiplication and division
 */

{ MUL,	INAREG,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SAREG,	TSWORD|TSHORT|TCHAR,
		NAREG|NASR|NASL,	RESC1,
		"	mult AL,AR	# signed multiply\n"
		"	mflo A1\n"
		"	nop\n"
		"	nop\n", },

{ MUL,	INAREG,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
		NAREG|NASR|NASL,	RESC1,
		"	multu AL,AR	# unsigned multiply\n"
		"	mflo A1\n"
		"	nop\n"
		"	nop\n", },

{ MUL,	INBREG,
	SBREG,	TLL,
	SCON,	TLL,
		2*NBREG|NASR|NASL,	RESC1,
		"	li A2,AR\n"
		"	multu AL,A2\n"
		"	mfhi U1\n"
		"	mflo A1\n"
		"	move A2,UR\n"
		"	mult AL,A2\n"
		"	mflo U2\n"
		"	nop\n"
		"	nop\n"
		"	addu A2,U1,U2\n"
		"	li U2,AR\n"
		"	mult UL,U2\n"
		"	mflo U2\n"
		"	nop\n"
		"	nop\n"
		"	addu U1,A2,U2\n", },

{ MUL,	INBREG,
	SBREG,	TLL,
	SBREG,	TLL,
		2*NBREG|NASR|NASL,	RESC1,
		"	multu AL,AR\n"
		"	mfhi U1\n"
		"	mflo A1\n"
		"	mult AL,AR\n"
		"	mflo U2\n"
		"	nop\n"
		"	nop\n"
		"	addu A2,U1,U2\n"
		"	mult UL,AR\n"
		"	mflo U2\n"
		"	nop\n"
		"	nop\n"
		"	addu U1,A2,U2\n", },

{ MUL,	INCREG,
	SCREG,	TFLOAT,
	SCREG,	TFLOAT,
		NCREG,	RESC1,
		"	mul.s A1,AL,AR		# floating-point multiply\n", },

{ MUL,	INCREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SCREG,	TDOUBLE|TLDOUBLE,
		NCREG,	RESC1, 
		"	mul.d	A1,AL,AR	# double-floating-point multiply\n", },

{ DIV,	INAREG,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SAREG,	TSWORD|TSHORT|TCHAR,
		NAREG|NASR|NASL,	RESC1,
		"	div AL,AR	# signed division\n"
		"	mflo A1\n"
		"	nop\n"
		"	nop\n", },

{ DIV,	INAREG,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
		NAREG|NASR|NASL,	RESC1,
		"	divu AL,AR	# unsigned division\n"
		"	mflo A1\n"
		"	nop\n"
		"	nop\n", },

{ DIV, INBREG,
	SBREG,		TLL,
	SBREG|SCON,	TLL,
		NBREG,	RESC1,
		"ZE", },

{ DIV,	INCREG,
	SCREG,	TFLOAT,
	SCREG,	TFLOAT,
		NCREG,	RESC1,
		"	div.s A1,AL,AR		# floating-point division\n", },

{ DIV,	INCREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SCREG,	TDOUBLE|TLDOUBLE,
		NCREG,	RESC1, 
		"	div.d	A1,AL,AR	# double-floating-point division\n", },

{ MOD,  INAREG,
        SAREG,  TSWORD|TSHORT|TCHAR,
        SAREG,  TSWORD|TSHORT|TCHAR,
                NAREG,  RESC1,
                "       div AL,AR	# signed modulo\n"
		"	mfhi A1\n"
		"	nop\n"
		"	nop\n"
                "       sub A1,A1,AL\n", },

{ MOD,  INAREG,
        SAREG,  TUWORD|TUSHORT|TUCHAR,
        SAREG,  TUWORD|TUSHORT|TUCHAR,
                NAREG,  RESC1,
                "       divu AL,AR	# signed modulo\n"
		"	mfhi A1\n"
		"	nop\n"
		"	nop\n"
                "       sub A1,A1,AL\n", },

{ MOD,  INBREG,
        SBREG,  TLL,
        SBREG,  TLL,
                NSPECIAL|NBREG,  RESC1,
                "ZE", },
    
/*
 * Templates for unsigned values needs to come before OPSIMP 
 */

{ PLUS,	INBREG,
	SBREG,	TULONGLONG,
	SBREG,	TULONGLONG,
		NBREG|NAREG,	RESC1,
      		"	addu A1,AL,AR\n"
      		"	sltu A2,A1,AR\n"
      		"	addu U1,UL,UR\n"
      		"	addu U1,U1,A2\n", },

{ PLUS,	INAREG,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SAREG,	TSWORD|TSHORT|TCHAR,
		NBREG|NAREG,	RESC1,
      		"	add A1,AL,AR\n", },

{ PLUS,	INAREG,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
		NBREG|NAREG,	RESC1,
      		"	addu A1,AL,AR\n", },

{ PLUS,	INAREG,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SSCON,	TANY,
		NAREG|NASL,	RESC1,
		"	addi A1,AL,AR\n", },

{ PLUS,	INAREG,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
	SSCON,	TANY,
		NAREG|NASL,	RESC1,
		"	addiu A1,AL,AR\n", },

{ PLUS,	INCREG,
	SCREG,	TFLOAT,
	SCREG,	TFLOAT,
		NCREG|NCSL,	RESC1,
		"	add.s A1,AL,AR\n", },

{ PLUS,	INCREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SCREG,	TDOUBLE|TLDOUBLE,
		NCREG|NCSL,	RESC1,
		"	add.d A1,AL,AR\n", },

{ MINUS,	INBREG,
	SBREG,	TULONGLONG,
	SBREG,	TULONGLONG,
		NBREG|NAREG,	RESC1,
      		"	sltu A2,AL,AR\n"
      		"	subu A1,AL,AR\n"
      		"	subu U1,UL,UR\n"
      		"	subu U1,U1,A2\n", },

{ MINUS,	INAREG,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SAREG,	TSWORD|TSHORT|TCHAR,
		NBREG|NAREG,	RESC1,
      		"	sub A1,AL,AR\n", },

{ MINUS,	INAREG,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
		NBREG|NAREG,	RESC1,
      		"	subu A1,AL,AR\n", },

{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SSCON,	TANY,
		NAREG|NASL,	RESC1,
		"	subu A1,AL,AR\n", },

{ MINUS,	INCREG,
	SCREG,	TFLOAT,
	SCREG,	TFLOAT,
		NCREG|NCSL,	RESC1,
		"	sub.s A1,AL,AR\n", },

{ MINUS,	INCREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SCREG,	TDOUBLE|TLDOUBLE,
		NCREG|NCSL,	RESC1,
		"	sub.d A1,AL,AR\n", },

{ UMINUS,	INAREG,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	neg A1,AL\n", },

{ UMINUS,	INBREG,
	SBREG,	TLL,
	SANY,	TANY,
		NBREG|NAREG|NBSL,	RESC1,
		"	subu A1,$zero,AL\n"
		"	subu U1,$zero,UL\n"
		"	sltu A2,$zero,A1\n"
		"	subu U1,U1,A2\n", },

{ UMINUS,	INCREG,
	SCREG,	TFLOAT,
	SCREG,	TFLOAT,
		NCREG|NCSL,	RESC1,
		"	neg.s A1,AL\n", },

{ UMINUS,	INCREG,
	SCREG,	TDOUBLE|TLDOUBLE,
	SCREG,	TDOUBLE|TLDOUBLE,
		NCREG|NCSL,	RESC1,
		"	neg.d A1,AL\n", },

/* Simple 'op rd, rs, rt' or 'op rt, rs, imm' operations */

{ OPSIMP,	INBREG,
	SBREG,	TLL,
	SBREG,	TLL,
		NBREG|NBSR|NBSL,	RESC1,
      		"	O A1,AL,AR\n"
      		"	O U1,UL,UR\n", },
    
{ OPSIMP,	INAREG,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TUCHAR|TCHAR,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TUCHAR|TCHAR,
		NAREG|NASR|NASL,	RESC1,
		"	O A1,AL,AR\n", },

{ OPSIMP,	INAREG,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TUCHAR|TCHAR,
	SSCON,	TSHORT|TUSHORT|TUCHAR|TCHAR,
		NAREG|NASL,	RESC1,
		"	Oi A1,AL,AR\n", },

/*
 * Shift instructions
 */

{ RS,	INAREG,
	SAREG,	TWORD|TUSHORT|TSHORT|TCHAR|TUCHAR,
	SCON,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	srl A1,AL,AR	# shift right by constant\n", },

{ LS,	INAREG,
	SAREG,	TWORD|TUSHORT|TSHORT|TCHAR|TUCHAR,
	SCON,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	sll A1,AL,AR	# shift left by constant\n", },
    
{ RS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	srlv A1,AL,AR	# shift right by register\n", },

{ LS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	sllv A1,AL,AR	# shift left by register\n", },	

{ RS,	INBREG,
	SBREG,	TLL,
	SCON,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"ZO", },

{ LS,	INBREG,
	SBREG,	TLL,
	SCON,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"ZO", },

{ RS,	INBREG,
	SBREG,	TLL,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NSPECIAL|NBREG,	RESC1,
		"ZE", },

{ LS,	INBREG,
	SBREG,	TLL,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NSPECIAL|NBREG,	RESC1,
		"ZE", },

/*
 * Rule for unary one's complement
 */

{ COMPL,        INAREG,
        SAREG,  TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SANY,   TANY,
                NAREG|NASL,   RESC1,
                "	nor A1,AL	# complement\n", },
    
{ COMPL,        INBREG,
        SBREG,  TLL,
        SANY,   TANY,
                NBREG|NBSL,   RESC1,
                "	nor A1,AL	# complement\n"
                "	nor U1,UL\n", },
    
/*
 * The next rules takes care of assignments. "=".
 */

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TWORD|TPOINT,
	SAREG,		TWORD|TPOINT,
		0,	RDEST,
        	"	sw AR,AL		# store (u)int/(u)long\n"
		"	nop\n", },

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TSHORT|TUSHORT,
	SAREG,		TSHORT|TUSHORT,
		0,	RDEST,
        	"	sh AR,AL		# store (u)short\n"
		"	nop\n", },	

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TCHAR|TUCHAR,
	SAREG,		TCHAR|TUCHAR,
		0,	RDEST,
        	"	sb AR,AL		# store (u)char\n"
		"	nop\n", },	

{ ASSIGN,	FOREFF|INBREG,
	SOREG|SNAME,	TLL,
	SBREG,		TLL,
		0,	RDEST,
      		"	sw UR,UL		# store (u)longlong\n"
		"	nop\n"
      		"	sw AR,AL\n"
		"	nop\n", },

#if 0
{ ASSIGN,	FOREFF|INAREG,
	SNAME,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG,	RDEST,
        	"	la A1,AL		# store word into sname\n"
		"	sw AR,0(A1)\n"
		"	nop\n", },

{ ASSIGN,	FOREFF|INAREG,
	SNAME,	TSHORT|TUSHORT,
	SAREG,	TSHORT|TUSHORT,
		NAREG,	RDEST,
        	"	la A1,AL		# store (u)short into sname\n"
		"	sh AR,0(A1)\n"
		"	nop\n", },

{ ASSIGN,	FOREFF|INAREG,
	SNAME,	TCHAR|TUCHAR,
	SAREG,	TCHAR|TUCHAR,
		NAREG,	RDEST,
        	"	la A1,AL		# store (u)char into sname\n"
		"	sb AR,0(A1)\n"
		"	nop\n", },	
#endif

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TLL,
	SBREG,	TLL,
		0,	RDEST,
      		"	move UL,UR		# register move\n"
      		"	move AL,AR\n", },
    
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TANY,
	SAREG,	TANY,
		0,	RDEST,
        	"	move AL,AR		# register move\n", },

{ ASSIGN,	FOREFF|INCREG,
	SNAME|SOREG,	TFLOAT,
	SCREG,		TFLOAT,
		0,	RDEST,
		"	s.s AR,AL		# store floating-point reg to sname\n", },

{ ASSIGN,	FOREFF|INCREG,
	SNAME|SOREG,	TDOUBLE|TLDOUBLE,
	SCREG,		TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	s.d AR,AL		# store double floating-point reg to sname\n", },

{ STASG,        INAREG|FOREFF,
        SOREG|SNAME,	TANY,
        SAREG,  	TPTRTO|TANY,
                NSPECIAL,       RRIGHT,
                "ZQ", },

/*
 * Compare instructions
 */

{ EQ,	FORCC,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
                0,      RESCC,
                "	beq AL,AR,LC\n", },

{ NE,	FORCC,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
                0,      RESCC,
                "	bne AL,AR,LC\n", },

{ OPLOG,	FORCC,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SZERO,		TANY,
                0,      RESCC,
                "	O AL,LC\n", },

{ OPLOG,	FORCC,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
                NAREG|NASL,     RESCC,
		"	sub A1,AL,AR\n"
                "	O A1,LC\n", },

{ OPLOG,	FORCC,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SCON,		TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
                NAREG|NASL,     RESCC,
		"	sub A1,AL,AR\n"
                "	O A1,LC\n", },

{ OPLOG,	FORCC,
	SBREG,		TLL,
	SBREG,		TLL,
		NAREG,	RESCC,
		"ZD", },

{ OPLOG,	FORCC,
	SCREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SCREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RESCC,
		"ZG", },

/*
 * Convert LTYPE to reg.
 */

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TCHAR,
		NAREG,	RESC1,
		"	lb A1,AL	# load char to reg\n"
		"	nop\n", },
	
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TUCHAR,
		NAREG,	RESC1,
		"	lbu A1,AL	# load uchar to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TSHORT,
		NAREG,	RESC1,
		"	lh A1,AL	# load short to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TUSHORT,
		NAREG,	RESC1,
		"	lhu A1,AL	# load ushort to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	lw A1,AL	# load (u)int/(u)long to reg\n"
		"	nop\n", },

{ OPLTYPE,	INBREG,
	SANY,		TANY,
	SOREG|SNAME,	TLL,
		NBREG,	RESC1,
		"	lw U1,UL	# load (u)longlong to reg\n"
		"	nop\n"
		"	lw A1,AL\n"
      		"	nop\n", },

#if 0

// don't need these with the gas assembler

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SNAME,	TCHAR,
		2*NAREG,	RESC1,
		"	la A2,AL	# load char sname to reg\n"
		"	lb A1,0(A2)\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SNAME,	TUCHAR,
		2*NAREG,	RESC1,
		"	la A2,AL	# load uchar sname to reg\n"
		"	lbu A1,0(A2)\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SNAME,	TSHORT,
		2*NAREG,	RESC1,
		"	la A2,AL	# load short sname to reg\n"
		"	lh A1,0(A2)\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SNAME,	TUSHORT,
		2*NAREG,	RESC1,
		"	la A2,AL	# load ushort sname to reg\n"
		"	lhu A1,0(A2)\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SNAME,	TWORD|TPOINT,
		2*NAREG,	RESC1,
		"	la A2,AL	# load (u)int/(u)long to reg\n"
		"	lw A1,0(A2)\n"
		"	nop\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SNAME,	TLL,
		2*NBREG,	RESC1,
		"	la A2,UL	# load (u)longlong to reg (endiannes problems?)\n"
		"	lw U1,0(A2)\n"
		"	nop\n"
		"	la A2,AR\n"
		"	lw A1,0(A2)\n"
      		"	nop\n", },

#endif

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TPOINT,
		NAREG,	RESC1,
		"	la A1,AL	# load constant address to reg\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TANY,
		NAREG,	RESC1,
		"	li A1,AL	# load constant to reg\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SZERO,	TANY,
		NAREG,	RESC1,
		"	move A1,$zero	# load 0 to reg\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SCON,	TANY,
		NBREG,	RESC1,
		"	li A1,AL	# load constant to reg\n"
		"	li U1,UL\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SZERO,	TANY,
		NBREG,	RESC1,
		"	move A1,$zero	# load 0 to reg\n"
		"	move U1,$zero\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SANY,	TANY,
		NAREG,	RESC1,
		"	move A1,AL\n", },

{ OPLTYPE,	INCREG,
	SANY,	TANY,
	SZERO,	TFLOAT,
		NCREG,	RESC1,
		"	mtc1 $zero,A1	# load 0 to float reg\n", },

{ OPLTYPE,	INCREG,
	SANY,	TANY,
	SZERO,	TDOUBLE|TLDOUBLE,
		NCREG,	RESC1,
		"	mtc1 $zero,A1	# load 0 to (l)double reg\n"
		"	mtc1 $zero,U1\n", },

{ OPLTYPE,	INCREG,
	SANY,	TANY,
	SOREG|SNAME,	TFLOAT,
		NCREG,	RESC1,
		"	l.s A1,AL	# load into floating-point reg\n"
		"	nop\n", },

{ OPLTYPE,	INCREG,
	SANY,	TANY,
	OREG|SNAME,	TDOUBLE|TLDOUBLE,
		NCREG,	RESC1,
		"	l.d A1,AL	# load into double floating-point reg\n"
		"	nop\n", },
    
/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	j LL		# goto label\n"
		"	nop\n", },

/*
 * Subroutine calls.
 */

{ CALL,         FOREFF,
        SCON,		TANY,
        SANY,           TANY,
                0,      0,
                "	subu $sp,$sp,16	# call (args, no result) to scon/sname\n"
                "	jal CL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        FOREFF,
        SCON,		TANY,
        SANY,           TANY,
                0,      0,
                "	jal CL			# call (no args, no result) to scon/sname\n"
		"	nop\n", },

{ CALL,         INAREG,
        SCON,		TANY,
        SAREG,          TANY,
                NAREG,     RESC1,  /* should be 0 */
                "	subu $sp,$sp,16	# call (args, result in v0) to scon/sname\n"
		"	jal CL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        INAREG,
        SCON,		TANY,
        SAREG,          TANY,
                NAREG,     RESC1,  /* should be 0 */
                "	jal CL   # call (no args, result in v0) to scon/sname\n"
		"	nop\n",
 },

{ CALL,         INBREG,
        SCON,		TANY,
        SBREG,          TANY,
                NBREG,     RESC1,  /* should be 0 */
                "	subu $sp,$sp,16	# call (args, result in v0:v1) to scon/sname\n"
		"	jal CL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        INBREG,
        SCON,		TANY,
        SBREG,          TANY,
                NBREG,     RESC1,  /* should be 0 */
                "	jal CL   # call (no args, result in v0:v1) to scon/sname\n"
		"	nop\n",
 },

{ CALL,         INCREG,
        SCON,		TANY,
        SCREG,          TANY,
                NCREG,     RESC1,  /* should be 0 */
                "	subu $sp,$sp,16	# call (args, result in f0:f1) to scon/sname\n"
		"	jal CL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        INCREG,
        SCON,		TANY,
        SCREG,          TANY,
                NCREG,     RESC1,  /* should be 0 */
                "	jal CL   # call (no args, result in v0:v1) to scon/sname\n"
		"	nop\n",
 },

{ CALL,         FOREFF,
        SAREG,		TANY,
        SANY,		TANY,
                0,      0,
                "	subu $sp,$sp,16	# call (args, no result) to scon/sname\n"
                "	jal AL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        FOREFF,
        SAREG,		TANY,
        SANY,		TANY,
                0,      0,
                "	jal AL			# call (no args, no result) to scon/sname\n"
		"	nop\n", },

{ CALL,         INAREG,
        SAREG,		TANY,
        INAREG,		TANY,
                NAREG,     RESC1,  /* should be 0 */
                "	subu $sp,$sp,16	# call (args, result) to scon/sname\n"
                "	jal AL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        INAREG,
        SAREG,		TANY,
        INAREG,		TANY,
                NAREG,     RESC1,  /* should be 0 */
                "	jal AL			# call (no args, result) to scon/sname\n"
		"	nop\n", },

{ CALL,         INBREG,
        SAREG,		TANY,
        INBREG,		TANY,
                NBREG,     RESC1,  /* should be 0 */
                "	subu $sp,$sp,16	# call (args, result) to scon/sname\n"
                "	jal AL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        INBREG,
        SAREG,		TANY,
        INBREG,		TANY,
                NBREG,     RESC1,  /* should be 0 */
                "	jal AL			# call (no args, result) to scon/sname\n"
		"	nop\n", },

{ CALL,         INCREG,
        SAREG,		TANY,
        INCREG,		TANY,
                NCREG,     RESC1,  /* should be 0 */
                "	subu $sp,$sp,16	# call (args, result) to scon/sname\n"
                "	jal AL\n"
		"	nop\n"
		"ZC", },

{ UCALL,        INCREG,
        SCREG,		TANY,
        INCREG,		TANY,
                NCREG,     RESC1,  /* should be 0 */
                "	jal AL			# call (no args, result) to scon/sname\n"
		"	nop\n", },


/* struct return */
{ USTCALL,      FOREFF,
	SCON|SNAME,	TANY,
	SANY,   	TANY,
		0,	0,
		"	jal CL\n"
		"	nop\n", },

{ USTCALL,      INAREG,
	SCON|SNAME,	TANY,
	SANY,   	TANY,
		NAREG|NASL,	RESC1,
		"	jal CL\n"
		"	nop\n", },

{ STCALL,      FOREFF,
	SCON|SNAME,	TANY,
	SANY,   	TANY,
		0,	0,
		"	jal CL\n"
		"	nop\n"
		"ZC", },

{ STCALL,      INAREG,
	SCON|SNAME,	TANY,
	SANY,   	TANY,
		NAREG|NASL,	RESC1,
		"	jal CL\n"
		"	nop\n"
		"ZC", },

/*
 *  Function arguments
 */

{ FUNARG,       FOREFF,
        SAREG,  TWORD|TPOINT,
        SANY,   TWORD|TPOINT,
                0,      0,
                "	subu $sp,$sp,4		# save function arg to stack\n"
		"	sw AL,0($sp)\n"
		"	#nop\n", },

{ FUNARG,       FOREFF,
        SAREG,  TSHORT|TUSHORT,
        SANY,   TSHORT|TUSHORT,
                0,      0,
                "	subu $sp,$sp,4		# save function arg to stack\n"
		"	sh AL,0($sp)\n"
		"	#nop\n", },

{ FUNARG,       FOREFF,
        SAREG,  TCHAR|TUCHAR,
        SANY,   TCHAR|TUCHAR,
                0,      0,
                "	subu $sp,$sp,4		# save function arg to stack\n"
                "	sb AL,0($sp)\n"
		"	#nop\n", },    

{ FUNARG,	FOREFF,
	SBREG,	TLL,
	SANY,	TLL,
		0,	0,
		"	addi $sp,$sp,-8		# save function arg to stack (endian problem here?\n"
		"	sw UL,4($sp)\n"
		"	sw AL,0($sp)\n", },

{ FUNARG,	FOREFF,
	SCREG,	FLOAT,
	SANY,	FLOAT,
		0,	0,
		"	addi $sp,$sp,-4		# save function arg to stack\n"
		"	s.s AL,0($sp)\n", },

{ FUNARG,	FOREFF,
	SCREG,	TDOUBLE|TLDOUBLE,
	SANY,	TDOUBLE|TLDOUBLE,
		0,	0,
		"	addi $sp,$sp,-8		# save function arg to stack\n"
		"	s.d AL,0($sp)\n", },

{ STARG,	FOREFF,
	SAREG|SOREG|SNAME|SCON,	TANY,
	SANY,			TSTRUCT,
		NSPECIAL,	0,
		"ZH", },

/*
 * Indirection operators.
 */
{ UMUL, INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TPOINT|TWORD,
    		NAREG,     RESC1,
        	"	lw A1,AL		# word load\n"
		"	nop\n", },

{ UMUL, INAREG,
	SANY,	TSHORT|TUSHORT,
	SOREG,	TSHORT|TUSHORT,
    		NAREG,     RESC1,
        	"	lh A1,AL		# (u)short load\n"
		"	nop\n", },

{ UMUL, INAREG,
	SANY,	TCHAR|TUCHAR,
	SOREG,	TCHAR|TUCHAR,
    		NAREG,     RESC1,
        	"	lb A1,AL		# (u)char load\n"
		"	nop\n", },

{ UMUL,	INBREG,
	SANY,	TLL,
	SOREG,	TLL,
		NBREG,	RESC1,
		"	lw A1,AL		# (u)longlong load - endian problem here?\n"
		"	nop\n"
		"	lw U1,AL\n"
		"	nop\n", },

{ UMUL,	INCREG,
	SANY,	TFLOAT,
	SOREG,	TFLOAT,
		NCREG,	RESC1,
		"	l.s A1,AL		# float load\n"
		"	nop\n", },

{ UMUL,	INCREG,
	SANY,	TDOUBLE|TLDOUBLE,
	SOREG,	TDOUBLE|TLDOUBLE,
		NCREG,	RESC1,
		"	l.d A1,AL		# float load\n"
		"	nop\n", },

{ UMUL,	INCREG,
	SANY,	TDOUBLE|TLDOUBLE,
	SAREG,	TPOINT,
		NCREG,	RESC1,
		"	l.d A1,0(AL)\n"
		"	nop\n", },
    
{ UMUL, INAREG,
	SANY,	TPOINT|TWORD,
	SNAME,	TPOINT|TWORD,
    		NAREG,     RESC1,
        	"	la A1,AL		# sname word load\n"
		"	lw A1,0(A1)\n"
		"	nop\n", },

{ UMUL, INAREG,
	SANY,	TSHORT|TUSHORT,
	SNAME,	TSHORT|TUSHORT,
    		NAREG,     RESC1,
        	"	la A1,AL		# sname (u)short load\n"
		"	lh A1,0(A1)\n"
		"	nop\n", },

{ UMUL, INAREG,
	SANY,	TCHAR|TUCHAR,
	SNAME,	TCHAR|TUCHAR,
    		NAREG,     RESC1,
        	"	la A1,AL		# sname (u)char load\n"
		"	lb A1,0(A1)\n"
		"	nop\n", },

{ UMUL, INBREG,
	SANY,	TLL,
	SNAME,	TLL,
		NBREG|NAREG,	RESC1,
		"	la A2,AL		# sname (u)long long load - endian problems here?\n"
		"	lw A1,0(A1)\n"
		"	nop\n"
		"	lw U1,4(A1)\n"
		"	nop\n", },
		

{ UMUL, INAREG,
	SANY,	TPOINT|TWORD,
	SAREG,	TPOINT|TWORD,
    		NAREG,     RESC1,
        	"	lw A1,0(AL)		# word load\n"
		"	nop\n", },

{ UMUL, INAREG,
	SANY,	TSHORT|TUSHORT,
	SAREG,	TSHORT|TUSHORT,
    		NAREG,     RESC1,
        	"	lh A1,0(AL)		# (u)short load\n"
		"	nop\n", },

{ UMUL, INAREG,
	SANY,	TCHAR|TUCHAR,
	SAREG,	TCHAR|TUCHAR,
    		NAREG|NASL,     RESC1,
        	"	lb A1,0(AL)		# (u)char load\n"
		"	nop\n", },

{ UMUL, INBREG,
	SANY,	TLL,
	SCREG,	TLL,
		NBREG,	RESC1,
		"	lw A1,0(AL)		# (u)long long load - endianness problems?\n"
		"	nop\n"
		"	lw U1,4(AL)"
		"	nop\n", },

{ FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	"help; I'm in trouble\n" },
};

int tablesize = sizeof(table)/sizeof(table[0]);
