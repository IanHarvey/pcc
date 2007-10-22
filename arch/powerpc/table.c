/*-
 * Copyright (c) 2007 Gregory McGarry <g.mcgarry@ieee.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * A template has five logical sections:
 *
 *	1) subtree (operator); goal to achieve (cookie)
 *	2) left node descendent of operator (node class; type)
 *	3) right node descendent of operator (node class; type)
 *	4) resource requirements (number of scratch registers);
 *	   subtree rewriting rule
 *	5) emitted instructions
 */

#include "pass2.h"

#define TUWORD	TUNSIGNED|TULONG
#define TSWORD	TINT|TLONG
#define TWORD	TUWORD|TSWORD

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/* PCONVs are not necessary */
{ PCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	; pointer conversion", },

/*
 * Conversions of integral types
 */

/* convert (u)char to (u)char */
{ SCONV,	INAREG,
	INAREG,	TCHAR|TUCHAR,
	INAREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	; convert a between (u)uchar and (u)char\n", },

/* convert pointers to (u)int/(u)long */
{ SCONV,	INAREG,
	SAREG,	TPOINT|TWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		"	; convert a pointer/word to an int\n", },

/* convert pointers to pointers */
{ SCONV,	INAREG,
	SAREG,	TPOINT,
	SAREG,	TPOINT,
		0,	RLEFT,
		"	; convert pointers\n", },

/* convert (u)longlong to (u)longlong */
{ SCONV,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		0,	RLEFT,
		"	; convert (u)longlong to (u)longlong\n", },


/* convert char to short */
{ SCONV,	INAREG,
	SAREG,	TCHAR,
	SAREG,	TSHORT|TSWORD,
		NASL|NAREG,	RESC1,
		"	extsb	A1,AL	; convert char to short/int\n", },

/* convert uchar to short */
{ SCONV,	INAREG,
	SAREG,	TUCHAR,
	SAREG,	TSHORT|TSWORD,
		NASL|NAREG,	RESC1,
		"	; convert uchar to short/int\n", },

#if 0
/* convert char to short in memory */
{ SCONV,	INAREG,
	SOREG,	TCHAR,
	SAREG,	TSHORT|TSWORD,
		NASL|NAREG|NSPECIAL,	RESC1,
		"	lbz A1,AL	; convert char to short/int\n"
		"	extsb A1,A1\n", },
#endif

/* convert (u)char to ushort/uint/ulong */
{ SCONV,	INAREG,
	SAREG,	TCHAR|TUCHAR,
	SAREG,	TUSHORT|TUWORD,
		NASL|NAREG|NSPECIAL,	RESC1,
		"	andi. A1,AL,255	; convert (u)char (AL) to ushort/unsigned (A1)\n", },

#if 0
/* convert uchar to short/int/long in memory */
{ SCONV,	INAREG,
	SOREG,	TUCHAR,
	SAREG,	TSHORT|TSWORD,
		NASL|NAREG|NSPECIAL,	RESC1,
		"	lbz A1,AL	; convert uchar to short/int\n", },
#endif

#if 0
/* convert (u)char to ushort/uint/ulong in memory */
{ SCONV,	INAREG,
	SOREG,	TCHAR|TUCHAR,
	SAREG,	TUSHORT|TUWORD|TULONG,
		NASL|NAREG,	RESC1,
		"	lbz A1,AL	; convert (u)char to ushort/unsigned/ulong\n", },
#endif

/* convert uchar/ushort/uint to (u)longlong */
{ SCONV,	INBREG,
	SAREG,	TUCHAR|TUSHORT|TUNSIGNED,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG,		RESC1,
		"	mr A1,AL	; convert uchar/ushort/uint to (u)longlong\n"
		"	li U1,0\n", },

/* convert char/short/int to ulonglong */
{ SCONV,	INBREG,
	SAREG,	TCHAR|TSHORT|TSWORD,
	SBREG,	TULONGLONG,
		NBREG,		RESC1,
		"	mr A1,AL	; convert char/short/int to ulonglong\n"
		"	li U1,0\n", },

/* convert char/short/int to longlong */
{ SCONV,	INBREG,
	SAREG,	TCHAR|TSHORT|TSWORD,
	SBREG,	TLONGLONG,
		NBREG|NBSL,		RESC1,
		"	mr A1,AL	; convert char/short/int to longlong\n"
		"	srawi U1,AL,31\n", },

/* convert (u)short to (u)char  */
{ SCONV,	INAREG,
	SAREG,	TSHORT|TUSHORT,
	SAREG,	TCHAR|TUCHAR,
		NSPECIAL|NAREG|NASL,	RESC1,
		"	andi. A1,AL,255	; convert (u)short to (u)char\n", },

#if 0
/* convert (u)short to (u)char */
{ SCONV,	INAREG,
	SOREG,	TSHORT|TUSHORT,
	SAREG,	TCHAR|TUCHAR,
		NSPECIAL|NAREG|NASL,	RESC1,
		"	lbz	A1,AL	; convert (u)short to (u)char\n", },
#endif

/* convert short to int */
{ SCONV,	INAREG,
	SAREG,	TSHORT,
	SAREG,	TWORD,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	andi. A1,AL,63356	; convert short to int\n", },

#if 0
/* convert (u)short to uint int memory */
{ SCONV,	INAREG,
	SOREG,	TUSHORT|TSHORT,
	SAREG,	TWORD,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	lha A1,AL	; convert (u)short to int\n", },
#endif

/* convert ushort to (u)int. */
{ SCONV,	INAREG,
	SAREG,	TUSHORT,
	SAREG,	TWORD,
		NASL|NAREG|NSPECIAL,	RESC1,
		"	andi. A1,AL,65535	; convert ushort to word\n", },

/* convert (u)int to (u)char */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TCHAR|TUCHAR,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	andi. A1,AL,255		; convert (u)int to (u)char", },

/* convert (u)int to (u)char */
{ SCONV,        INAREG,
        SAREG,  TWORD,
        SANY,   TCHAR|TUCHAR,
	0,	RLEFT,
		"	; convert (u)int to (u)char\n", },

/* convert (u)int to (u)short */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TSHORT|TUSHORT,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	andi. A1,AL,65535	; convert (u)int to (u)short\n", },

/*
 * Subroutine calls.
 */

{ CALL,		FOREFF,
	SCON|SNAME,	TANY,
	SANY,		TANY,
		0,	0,
		"	bl CL	; call (args, no result) to scon/sname (CL)\n", },

{ UCALL,	FOREFF,
	SCON|SNAME,	TANY,
	SANY,		TANY,
		0,	0,
		"	bl CL	; call (no args, no result) to scon/sname (CL)\n", },

{ CALL,		INAREG,
	SCON|SNAME,	TANY,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	bl CL	; call (args, result in r3) to scon/sname (CL)\n", },

{ CALL,		INBREG,
	SCON|SNAME,	TANY,
	SBREG,		TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	bl CL	; call (args, result in r3:r4) to scon/sname (CL)\n", },

{ UCALL,	INAREG,
	SCON|SNAME,	TANY,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	bl CL	; call (no args, result in r3) to scon/sname (CL)\n", },

{ UCALL,	INBREG,
	SCON|SNAME,	TANY,
	SBREG,		TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	bl CL	; call (no args, result in r3:r4) to scon/sname (CL)\n", },

/* struct return */
{ USTCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	0,
		"ZP	call CL\n", },

{ USTCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call CL\n", },

{ USTCALL,	INAREG,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call *AL\n", },

{ STCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	0,
		"ZP	call CL\n", },

{ STCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call CL\n", },

{ STCALL,	INAREG,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"ZP	call *AL\n", },

/*
 * The next rules handle all binop-style operators.
 */

/* XXX AL cannot be R0 */
{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	addi A1,AL,AR		; addition of constant\n", },

/* XXX AL cannot be R0 */
{ PLUS,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1|RESCC,
		"	addic. A1,AL,AR		; addition of constant\n", },

{ PLUS,		INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	addic A1,AL,AR		; 64-bit addition of constant\n"
		"	addze U1,UL", },

{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	add A1,AL,AR\n", },

{ PLUS,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL|NSPECIAL,	RESC1|RESCC,
		"	add. A1,AL,AR\n", },

{ PLUS,		INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	addc A1,AL,AR		; 64-bit add\n"
		"	adde U1,UL,UR\n", },

{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	addi A1,AL,-AR\n", },

{ MINUS,	INAREG|FORCC,
	SAREG,	TWORD|TPOINT,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1|RESCC,
		"	addi. A1,AL,-AR\n", },

{ MINUS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	addic A1,AL,-AR"
		"	addme U1,UL\n", },

{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	subf A1,AR,AL\n", },

{ MINUS,	INAREG|FORCC,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL|NSPECIAL,	RESC1|RESCC,
		"	subf. A1,AR,AL\n", },

{ MINUS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	subfc A1,AR,AL		; 64-bit subtraction\n"
		"	subfe U1,UR,UL\n", },

/*
 * The next rules handle all shift operators.
 */

{ LS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	slw A1,AL,AR		; left shift\n", },

{ LS,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	slw. A1,AL,AR		; left shift\n", },

{ LS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	slwi A1,AL,AR		; left shift by constant\n", },

{ LS,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	slwi. A1,AL,AR		; left shift by constant\n", },

{ LS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SCON,	TANY,
		NBREG,	RESC1,
		"ZO" },

{ RS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	srw A1,AL,AR		; right shift\n", },

{ RS,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	srw. A1,AL,AR		; right shift\n", },

{ RS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	srwi A1,AL,AR		; right shift by constant\n", },

{ RS,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	srwi. A1,AL,AR		; right shift by constant\n", },

{ RS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SCON,	TANY,
		NBREG,	RESC1,
		"ZO" },

/*
 * The next rules takes care of assignments. "=".
 */

/* assign 16-bit constant to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TANY,
	SSCON,		TANY,
		0,	RDEST,
		"	li AL,AR\n", },

/* assign 16-bit constant to register */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,		TANY,
	SSCON,		TANY,
		0,	RDEST,
		"	li AL,AR\n"
		"	li UL,0\n", },

/* assign constant to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TANY,
	SCON,		TANY,
		0,	RDEST,
		"	lis AL,ha16(AR)\n"
		"	addi AL,AL,lo16(AR)\n", },

/* assign constant to register */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,		TANY,
	SCON,		TANY,
		0,	RDEST,
		"	lis AL,ha16(AR)\n"
		"	addi AL,AL,lo16(AR)\n"
		"	lis UL,ha16(UR)\n"\
		"	addi UL,UL,lo16(UR)\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TWORD|TPOINT,
	SOREG,		TWORD|TPOINT,
		NSPECIAL,	RDEST,
		"	lwz AL,AR\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TWORD|TPOINT,
	SNAME,		TWORD|TPOINT,
		NSPECIAL,	RDEST,
		"	lis AL,ha16(AR)		; assign sname to reg\n"
		"	lwz AL,lo16(AR)(AL)\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,		TLONGLONG|TULONGLONG,
	SOREG,		TLONGLONG|TULONGLONG,
		NSPECIAL,	RDEST,
		"	lwz AL,AR		; assign llong to reg\n"
		"	lwz UL,UR\n" },

{ ASSIGN,	FOREFF|INAREG,
	SBREG,		TLONGLONG|TULONGLONG,
	SNAME,		TLONGLONG|TULONGLONG,
		NSPECIAL,	RDEST,
		"	lis AL,ha16(AR)		; assign 64-bit sname to reg\n"
		"	lwz AL,lo16(AR)(AL)\n"
		"	lis UL,ha16(UR)\n"
		"	lwz UL,lo16(UR)(UL)\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,		TLONGLONG|TULONGLONG,
	SOREG,		TSWORD,
		NSPECIAL,	RDEST,
		"	lwz AL,AR		; load int/pointer into llong\n"
		"	srawi UL,AR,31\n" },

/* assign memory to register */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,		TLONGLONG|TULONGLONG,
	SOREG,		TUNSIGNED|TPOINT,
		NSPECIAL,	RDEST,
		"	lwz AL,AR		; load uint/pointer into (u)llong\n"
		"	li UL, 0\n" },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TUCHAR,
	SOREG,		TUCHAR,
		NSPECIAL,	RDEST,
		"	lbz AL,AR\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TUCHAR,
	SNAME,		TUCHAR,
		NSPECIAL,	RDEST,
		"	lis AL,ha16(AR)		; assign uchar sname to reg\n"
		"	lbz AL,lo16(AR)(AL)\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TCHAR,
	SOREG,		TCHAR,
		NSPECIAL,	RDEST,
		"	lbz AL,AR\n"
		"	extsb AL,AL\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TCHAR,
	SNAME,		TCHAR,
		NSPECIAL,	RDEST,
		"	lis AL,ha16(AR)		; assign char sname to reg\n"
		"	lbz AL,lo16(AR)(AL)\n"
		"	extsb AL,AL\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TWORD|TPOINT,
	SOREG,		TSHORT|TUSHORT,
		NSPECIAL,	RDEST,
		"	lha AL,AR\n", },

/* assign memory to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TWORD|TPOINT,
	SNAME,		TSHORT|TUSHORT,
		NSPECIAL,	RDEST,
		"	lis AL,ha16(AR)\n"
		"	lha AL,lo16(AR)(AL)\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INAREG,
	SOREG,		TWORD|TPOINT,
	SAREG,		TWORD|TPOINT,
		NSPECIAL,	RDEST,
		"	stw AR,AL\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INAREG,
	SNAME,		TWORD|TPOINT,
	SAREG,		TWORD|TPOINT,
		NAREG|NSPECIAL,	RDEST,
		"	lis A1,ha16(AL)		; assign reg to sname\n"
		"	stw AR,lo16(AL)(A1)\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INBREG,
	SOREG,		TLONGLONG|TULONGLONG,
	SBREG,		TLONGLONG|TULONGLONG,
		NSPECIAL,	RDEST,
		"	stw AR,AL		; store 64-bit value\n"
		"	stw UR,UL\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INBREG,
	SNAME,		TLONGLONG|TULONGLONG,
	SBREG,		TLONGLONG|TULONGLONG,
		NBREG|NSPECIAL,	RDEST,
		"	lis A1,ha16(AL)		; assign reg to 64-bit sname\n"
		"	stw AR,lo16(AL)(A1)\n"
		"	lis U1,ha16(UL)\n"
		"	stw UR,lo16(UL)(U1)\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INAREG,
	SOREG,		TCHAR|TUCHAR,
	SAREG,		TCHAR|TUCHAR,
		NSPECIAL,	RDEST,
		"	stb AR,AL\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INAREG,
	SNAME,		TCHAR|TUCHAR,
	SAREG,		TCHAR|TUCHAR,
		NAREG|NSPECIAL,	RDEST,
		"	lis A1,ha16(AL)"
		"	stb AR,lo16(AL)(A1)\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INAREG,
	SOREG,		TSHORT|TUSHORT,
	SAREG,		TSHORT|TUSHORT,
		NSPECIAL,	RDEST,
		"	sth AR,AL\n", },

/* assign register to memory */
{ ASSIGN,	FOREFF|INAREG,
	SNAME,		TSHORT|TUSHORT,
	SAREG,		TSHORT|TUSHORT,
		NAREG|NSPECIAL,	RDEST,
		"	lis A1,ha16(AL)\n"
		"	sth AR,lo16(AL)(A1)\n", },

/* assign register to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
		0,	RDEST,
		"	mr AL,AR	; assign AR to AL\n", },

{ ASSIGN,      FOREFF|INBREG,
        SBREG,	TLONGLONG|TULONGLONG,
        SBREG,	TLONGLONG|TULONGLONG,
                0,  RDEST,
		"	mr AL,AR	; assign UR:AR to UL:AL\n"
                "	mr UL,UR\n", },

#if 0
{ ASSIGN,	FOREFF|INAREG,
	SFLD,	TANY,
	SAREG,	TANY,
		NAREG,	RDEST,
		"ZE", },

{ ASSIGN,	FOREFF,
	SFLD,	TANY,
	SAREG,	TANY,
		NAREG,	0,
		"ZE", },
#endif

/* Do not generate memcpy if return from funcall */
#if 0
{ STASG,	INAREG|FOREFF,
	SOREG|SNAME|SAREG,	TPTRTO|TSTRUCT,
	SFUNCALL,	TPTRTO|TSTRUCT,
		0,	RRIGHT,
		"", },
#endif

{ STASG,	INAREG|FOREFF,
	SOREG|SNAME,	TANY,
	SAREG|SOREG|SNAME,	TPTRTO|TANY,
		NSPECIAL,	RRIGHT,
		"ZQ", },

/*
 * DIV/MOD/MUL 
 */

{ DIV,	INAREG,
	SAREG,	TSWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	divw A1,AL,AR\n", },

{ DIV,	INAREG,
	SAREG,	TUWORD|TPOINT,
	SAREG,	TUWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	divwu A1,AL,AR\n", },

{ MOD,	INAREG,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	divw A1,AL,AR		; signed modulo\n"
		"	mullw A1,A1,AR\n"
		"	subf A1,A1,AL\n", },

{ MOD,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TUWORD|TPOINT,
		NAREG,	RESC1,
		"	divwu A1,AL,AR		; unsigned modulo\n"
		"	mullw A1,A1,AR\n"
		"	subf A1,A1,AL\n", },

{ MUL,	INAREG,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SSCON,		TANY,
		NAREG|NASL,	RESC1,
		"	mulli A1,AL,AR\n", },

{ MUL,	INAREG,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	mullw A1,AL,AR\n", },

{ MUL,	INBREG,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NBREG,	RESC1,
		"	mullw A1,AL,AR\n"
		"	mulhw U1,AL,AR\n", },

{ MUL,	INBREG,
	SBREG,		TLONGLONG|TULONGLONG,
	SBREG,		TLONGLONG|TULONGLONG,
		NBREG,	RESC1,
		"	mullw A1,AL,AR\n"
		"	mulhw U1,AL,AR\n", },

/*
 * Indirection operators.
 */

{ UMUL,	INAREG,
	SANY,		TPOINT|TWORD,
	SOREG,		TPOINT|TWORD,
		NAREG|NSPECIAL,	RESC1,
		"	lwz A1,AL		; word load\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TCHAR,
		NAREG|NSPECIAL,	RESC1,
		"	lbz A1,AL		; char load\n"
		"	extsb A1,A1\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TUCHAR,
		NAREG|NSPECIAL,	RESC1,
		"	lbz A1,AL		; uchar load\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TSHORT,
		NAREG|NSPECIAL,	RESC1,
		"	lhz A1,AL		; short load\n"
		"	extsh A1,A1\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TUSHORT,
		NAREG|NSPECIAL,	RESC1,
		"	lhz A1,AL		; ushort load\n", },

{ UMUL, INBREG,
	SANY,		TANY,
	SOREG,		TLONGLONG|TULONGLONG,
		NBREG, RESC1,
		"	lwz A1,AL		; 64-bit load\n"
		"	lwz U1,UL\n", },

/*
 * Logical/branching operators
 */

/* compare with constant */
{ OPLOG,	FORCC,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SSCON,	TANY,
		0, 	RESCC,
		"	cmpwi AL,AR\n", },

/* compare with constant */
{ OPLOG,	FORCC,
	SAREG,	TUWORD|TPOINT|TUSHORT|TUCHAR,
	SSCON,	TANY,
		0, 	RESCC,
		"	cmpli AL,AR\n", },

/* compare with register */
{ OPLOG,	FORCC,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SAREG,	TSWORD|TSHORT|TCHAR,
		0, 	RESCC,
		"	cmpw AL,AR\n", },

/* compare with register */
{ OPLOG,	FORCC,
	SAREG,	TUWORD|TPOINT|TUSHORT|TUCHAR,
	SAREG,	TUWORD|TPOINT|TUSHORT|TUCHAR,
		0, 	RESCC,
		"	cmpl AL,AR\n", },

/* compare with register */
{ OPLOG,	FORCC,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		0, 	0,
		"ZD", },

{ OPLOG,	FORCC,
	SANY,	TANY,
	SANY,	TANY,
		REWRITE,	0,
		"diediedie!", },

/* AND/OR/ER */
{ AND,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	and A1,AL,AR\n", },

{ AND,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	and. A1,AL,AR\n", },

/* AR must be positive */
{ AND,	INAREG,
	SAREG,		TWORD|TSHORT|TUSHORT|TCHAR|TCHAR,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	andi A1,AL,AR\n", },

{ AND,	INAREG|FORCC,
	SAREG,		TWORD|TSHORT|TUSHORT|TCHAR|TCHAR,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	andi. A1,AL,AR\n", },

{ AND,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	and A1,AL,AR		; 64-bit and\n"
		"	and U1,UL,UR\n" },

{ AND,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	andi A1,AL,AR		; 64-bit and with constant\n"
		"	li U1,0\n" },

{ AND,	INBREG|FORCC,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	andi. A1,AL,AR		; 64-bit and with constant\n"
		"	li U1,0\n" },

{ OR,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	or A1,AL,AR\n", },

{ OR,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	or. A1,AL,AR\n", },

{ OR,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TCHAR,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	ori A1,AL,AR\n", },

{ OR,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TCHAR,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	ori. A1,AL,AR\n", },

{ OR,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	or A1,AL,AR		; 64-bit or\n"
		"	or U1,UL,UR\n" },

{ OR,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	ori A1,AL,AR		; 64-bit or with constant\n" },

{ OR,	INBREG|FORCC,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	ori. A1,AL,AR		; 64-bit or with constant\n" },

{ ER,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	xor A1,AL,AR\n", },

{ ER,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	xor. A1,AL,AR\n", },

{ ER,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TCHAR,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	xori A1,AL,AR\n", },

{ ER,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TCHAR,
	SSCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	xori. A1,AL,AR\n", },

{ ER,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	xor A1,AL,AR		; 64-bit xor\n"
		"	xor U1,UL,UR\n" },

{ ER,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	xori A1,AL,AR		; 64-bit xor with constant\n" },

{ ER,	INBREG|FORCC,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	xori. A1,AL,AR		; 64-bit xor with constant\n" },

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	ba LL\n", },

//#ifdef GCC_COMPAT
{ GOTO, 	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	mtctr AL\n"
		"	bctr\n", },
//#endif

/*
 * Convert LTYPE to reg.
 */

{ OPLTYPE,      INBREG,
        SANY,   	TANY,
        SOREG,		TLONGLONG|TULONGLONG,
                NBREG,  RESC1,
                "	lwz A1,AL	; load long from memory\n"
		"	lwz U1,UL\n", },

{ OPLTYPE,      INBREG,
        SANY,   	TANY,
        SNAME,		TLONGLONG|TULONGLONG,
                NBREG,  RESC1,
		"	lis A1,ha16(AL)		; load long from sname\n"
		"	lwz A1,lo16(AL)(A1)\n"
		"	lis U1,ha16(UL)\n"
		"	lwz U1,lo16(UL)(U1)\n", },

/* load word from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	lwz A1,AL		; load word from memory\n", },

/* load word from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SNAME,		TWORD|TPOINT,
		NAREG|NSPECIAL,	RESC1,
		"	lis A1,ha16(AL)		; load word from sname\n"
		"	lwz A1,lo16(AL)(A1)\n", },

/* load char from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG,		TCHAR,
		NAREG,	RESC1,
		"	lbz A1,AL		; load char from memory\n"
		"	extsb A1,A1\n", },

/* load char from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SNAME,		TCHAR,
		NAREG|NSPECIAL,	RESC1,
		"	lis A1,ha16(AL)		; load char from sname\n"
		"	lbz A1,lo16(AL)(A1)\n"
		"	extsb A1,A1\n", },

/* load uchar from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG,		TUCHAR,
		NAREG,	RESC1,
		"	lbz A1,AL		; load uchar from memory\n", },

/* load uchar from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SNAME,		TUCHAR,
		NAREG|NSPECIAL,	RESC1,
		"	lis A1,ha16(AL)		; load uchar from sname"
		"	lbz A1,lo16(AL)(A1)\n", },

/* load short from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG,		TSHORT|TUSHORT,
		NAREG,	RESC1,
		"	lha A1,AL		; load (u)short from memory\n", },

/* load short from memory */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG,		TSHORT|TUSHORT,
		NAREG|NSPECIAL,	RESC1,
		"	lis A1,ha16(AL)		; load (u)short from sname\n"
		"	lha A1,lo16(AL)(A1)\n", },

/* load from 16-bit constant */
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SSCON,		TANY,
		NAREG,	RESC1,
		"	li A1,AL		; load 16-bit constant\n", },

/* load from 16-bit constant */
{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SSCON,	TANY,
		NBREG,	RESC1,
		"	li A1,AL		; load 16-bit constant\n"
		"	li U1,0\n", },

/* load from constant */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TANY,
		NAREG|NASL|NSPECIAL,	RESC1,
		"	lis A1,ha16(AL)	; load constant into register\n"
		"	addi A1,A1,lo16(AL)\n", },

/* load from constant */
{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SCON,	TANY,
		NBREG,	RESC1,
		"	lis A1,ha16(AL)	; load constant into register\n"
		"	addi A1,A1,lo16(AL)\n"
		"	lis U1,ha16(UL)\n"
		"	addi U1,U1,lo16(UL)\n", },

/* load from register */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG,	TANY,
		NAREG,	RESC1,
		"	mr A1,AL	; load AL into A1\n" },

/* load from register */
{ OPLTYPE,      INBREG,
        SANY,   TANY,
        SBREG,	TLONGLONG|TULONGLONG,
                NBREG,  RESC1,
		"	mr A1,AL	; load UL:AL into U1:A1\n"
                "       mr U1,UL\n", },


/*
 * Negate a word.
 */

{ UMINUS,	INAREG,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	neg A1,AL\n", },

{ UMINUS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	subfic A1,AL,0\n"
		"	subfze U1,UL\n", },

{ COMPL,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	not A1,AL\n", },

{ COMPL,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	not A1,AL"
		"	not U1,UL\n", },

/*
 * Arguments to functions.
 */

#if 0
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
#endif

{ FUNARG,	FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT,
	SANY,	TUSHORT,
		NAREG,	0,
		"	movzwl AL,ZN\n	pushl ZN\n", },

#if 0
{ FUNARG,	FOREFF,
	SHCH|SNAME|SOREG,	TCHAR,
	SANY,			TCHAR,
		NAREG,	0,
		"	movsbl AL,A1\n	pushl A1\n", },
#endif

#if 0
{ FUNARG,	FOREFF,
	SHCH|SNAME|SOREG,	TUCHAR,
	SANY,	TUCHAR,
		NAREG,	0,
		"	movzbl AL,A1\n	pushl A1\n", },
#endif

#if 0
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
