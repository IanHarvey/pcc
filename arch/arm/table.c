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

#define COM	"	@ "

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/* PCONVs are not necessary */
{ PCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		COM "pointer conversion\n", },

/*
 * Conversions of integral types
 */

/* convert (u)char to (u)char */
{ SCONV,	INAREG,
	INAREG,	TCHAR|TUCHAR,
	INAREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		COM "convert between (u)char and (u)char\n", },

/* convert (u)short to (u)short */
{ SCONV,	INAREG,
	INAREG,	TSHORT|TUSHORT,
	INAREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		COM "convert between (u)short and (u)short\n", },

/* convert pointers to (u)int/(u)long */
{ SCONV,	INAREG,
	SAREG,	TPOINT|TWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		COM "convert a pointer/word to an int\n", },

/* convert pointers to pointers */
{ SCONV,	INAREG,
	SAREG,	TPOINT,
	SAREG,	TPOINT,
		0,	RLEFT,
		COM "convert pointers\n", },

/* convert (u)longlong to (u)longlong */
{ SCONV,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		0,	RLEFT,
		COM "convert (u)longlong to (u)longlong\n", },


/* convert char to short */
{ SCONV,	INAREG,
	SAREG,	TCHAR,
	SAREG,	TSHORT|TSWORD,
		NASL|NAREG,	RESC1,
		"	XXX extsb A1,AL" COM "convert char to short/int\n", },

/* convert uchar to short */
{ SCONV,	INAREG,
	SAREG,	TUCHAR,
	SAREG,	TSHORT|TSWORD,
		0,	RLEFT,
		COM "convert uchar to short/int\n", },

/* convert uchar to ushort/uint/ulong */
{ SCONV,	INAREG,
	SAREG,	TUCHAR,
	SAREG,	TUSHORT|TUWORD,
		0,		RLEFT,
		COM "convert uchar to ushort/unsigned\n", },

/* XXX is this necessary? */
/* convert char to ushort/uint/ulong */
{ SCONV,	INAREG,
	SAREG,	TCHAR,
	SAREG,	TUSHORT|TUWORD,
		NASL|NAREG,	RESC1,
		"	and A1,AL,255" COM "convert char to ushort/unsigned\n", },

/* convert (u)char/(u)short/(u)int to longlong */
{ SCONV,	INBREG,
	SAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,		RESC1,
		"	mov A1,AL" COM "convert (u)char/(u)short/(u)int to longlong\n"
		"	mov U1,AL,asr #31\n", },

/* convert (u)short/word to (u)char  */
{ SCONV,	INAREG,
	SAREG,	TSHORT|TUSHORT|TWORD,
	SAREG,	TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	and A1,AL,255" COM "convert (u)short/word to (u)char\n", },

/* convert (u)short to int */
{ SCONV,	INAREG,
	SAREG,	TSHORT|TUSHORT,
	SAREG,	TWORD,
		0,	RLEFT,
		COM "convert (u)short to int\n", },

/* convert (u)int to (u)short */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,
		"	and A1,AL,65535" COM "convert (u)int to (u)short\n", },

/* convert (u)longlong to (u)int */
{ SCONV,	INAREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SAREG,	TWORD,
		NAREG,		RESC1,
		"	mov A1,AL" COM "convert (u)longlong to (u)int/long\n", },

/* conversions on load from memory */

/* (u)char */
{ SCONV,	INAREG,
	SOREG,	TCHAR | TUCHAR,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	ldrb A1,AL @ zero_extendqisi2" COM "convert char to int/long\n", },

#if 0 // not possible on ARM - need to do two byte loads
/* short, ushort */
{ SCONV,	INAREG,
	SOREG,	TSHORT|TUSHORT,
	SAREG,	TWORD,
		NASL|NAREG,	RESC1,
		"	lha A1,AL" COM "convert (u)short to int/long\n", },
#endif

/*
 * Subroutine calls.
 */

{ CALL,		FOREFF,
	SCON|SNAME,	TANY,
	SANY,		TANY,
		0,	0,
		"	bl CL" COM "call (args, no result) to scon/sname (CL)\n"
		"ZC", },

{ UCALL,	FOREFF,
	SCON|SNAME,	TANY,
	SANY,		TANY,
		0,	0,
		"	bl CL" COM "call (no args, no result) to scon/sname (CL)\n", },

{ CALL,		INAREG,
	SCON|SNAME,	TANY,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	bl CL" COM "call (args, result in r0) to scon/sname (CL)\n"
		"ZC", },

{ CALL,		INBREG,
	SCON|SNAME,	TANY,
	SBREG,		TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	bl CL" COM "call (args, result in r0:r1) to scon/sname (CL)\n"
		"ZC", },

{ UCALL,	INAREG,
	SCON|SNAME,	TANY,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	bl CL" COM "call (no args, result in r0) to scon/sname (CL)\n", },

{ UCALL,	INBREG,
	SCON|SNAME,	TANY,
	SBREG,		TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,	/* should be 0 */
		"	bl CL" COM "call (no args, result in r0:r1) to scon/sname (CL)\n", },

{ CALL,		FOREFF,
	SAREG,	TANY,
	SANY,		TANY,
		0,	0,
		"	sub sp,sp,16\n"
		"	mov lr,pc\n"
		"	mov pc,AL\n"
		"ZC", },

{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,		TANY,
		0,	0,
		"	mov lr,pc\n"
		"	mov pc,AL\n", },

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

{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SCCON,	TANY,
		NAREG,	RESC1,
		"	add A1,AL,AR" COM "addition of constant\n", },

{ PLUS,		INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	adds A1,AL,AR" COM "64-bit addition of constant\n"
		"	adc U1,UL", },

{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	add A1,AL,AR\n", },

{ PLUS,		INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	adds A1,AL,AR" COM "64-bit add\n"
		"	adc U1,UL,UR\n", },

{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT,
	SCCON,	TANY,
		NAREG|NASL,	RESC1,
		"	sub A1,AL,AR\n", },

{ MINUS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SSCON,	TANY,
		NBREG|NBSL,	RESC1,
		"	adds A1,AL,-AR\n"
		"	adc U1,UL,UR\n", },

{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	sub A1,AR,AL\n", },

{ MINUS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	XXX subfc A1,AR,AL" COM "64-bit subtraction\n"
		"	XXX subfe U1,UR,UL\n", },

/*
 * The next rules handle all shift operators.
 */

{ LS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	mov A1,AL,asl AR" COM "left shift\n", },

{ LS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SCCON,	TANY,
		NAREG|NASL,	RESC1,
		"	mov A1,AL,asl AR" COM "left shift by constant\n", },

{ LS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SCON,	TANY,
		NBREG,	RESC1,
		"ZO" },

{ RS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	mov A1,AL,asr AR" COM "right shift\n", },

{ RS,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SCCON,	TANY,
		NAREG|NASL,	RESC1,
		"	mov A1,AL,asr AR" COM "right shift by constant\n", },

{ RS,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SCON,	TANY,
		NBREG,	RESC1,
		"ZO" },

/*
 * The next rules takes care of assignments. "=".
 */

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TWORD|TPOINT,
	SAREG,		TWORD|TPOINT,
		0,	RDEST,
		"	str AR,AL" COM "assign word\n", },

{ ASSIGN,	FOREFF|INBREG,
	SOREG|SNAME,	LONGLONG|TULONGLONG,
	SBREG,		TLONGLONG|TULONGLONG,
		0,	RDEST,
		"	str AR,AL" COM "assign 64-bit value\n"
		"	str UR,UL\n", },

/* XXX don't know if this works */
{ ASSIGN,	FOREFF|INBREG,
	SAREG,		TPTRTO|TLONGLONG|TULONGLONG,
	SBREG,		TLONGLONG|TULONGLONG,
		0,	RDEST,
		"	stmdb AR,{AL-UL}" COM "assign 64-bit value\n", },

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TCHAR|TUCHAR,
	SAREG,		TCHAR|TUCHAR,
		0,	RDEST,
		"	strb AR,AL" COM "assign (u)char\n", },

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TSHORT|TUSHORT,
	SAREG,		TSHORT|TUSHORT,
		0,	RDEST,
		"	strh AR,AL" COM "assign (u)short\n", },

/* assign register to register */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
		0,	RDEST,
		"	mov AL,AR" COM "assign AR to AL\n", },

{ ASSIGN,      FOREFF|INBREG,
        SBREG,	TLONGLONG|TULONGLONG,
        SBREG,	TLONGLONG|TULONGLONG,
                0,  RDEST,
		"	mov AL,AR" COM "assign UR:AR to UL:AL\n"
                "	mov UL,UR\n", },

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
{ STASG,	INAREG|FOREFF,
	SOREG|SNAME|SAREG,	TPTRTO|TSTRUCT,
	SFUNCALL,	TPTRTO|TSTRUCT,
		0,	RRIGHT,
		"", },

{ STASG,	INAREG|FOREFF,
	SOREG|SNAME,	TANY,
	SAREG,	TPTRTO|TANY,
		NSPECIAL,	RRIGHT,
		"ZQ", },

/*
 * DIV/MOD/MUL 
 */

{ DIV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NSPECIAL|NAREG|NASL,	RESC1,
		"ZE", },

{ MOD,	INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NSPECIAL|NAREG,	RESC1,
		"ZE", },

{ MUL,	INAREG,
	SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SSCON,		TANY,
		NAREG|NASL,	RESC1,
		"	mul A1,AL,AR\n", },

{ MUL,	INBREG,
	SAREG,		TUWORD|TPOINT|TUSHORT|TUCHAR,
	SAREG,		TUWORD|TPOINT|TUSHORT|TUCHAR,
		NBREG,	RESC1,
#ifdef ARM_HAVE_MULL
		"	smull U1,A1,AL,AR\n", },
#else
		"	mul A1,AL,AR\n"
		"	mul U1,AL,AR\n", },
#endif

{ MUL,	INBREG,
	SAREG,		TSWORD|TSHORT|TCHAR,
	SAREG,		TSWORD|TSHORT|TCHAR,
		NBREG,	RESC1,
#ifdef ARM_HAVE_MULL
		"	umull U1,A1,AL,AR\n", },
#else
		"	mul A1,AL,AR\n"
		"	mul U1,AL,AR\n", },
#endif

{ MUL,	INBREG,
	SBREG,		TLONGLONG|TULONGLONG,
	SBREG,		TLONGLONG|TULONGLONG,
		NBREG,	RESC1,
#ifdef ARM_HAVE_MULL
		"	umull U1,A1,AL,AR\n", },
#else
		"	mul A1,AL,AR\n"
		"	mul U1,AL,AR\n", },
#endif

/*
 * Indirection operators.
 */

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TWORD|TPOINT,
		NAREG,	RESC1,
		"	ldr A1,AL" COM "word load\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TCHAR,
		NAREG,	RESC1,
		"	XXX lbz A1,AL" COM "char load\n"
		"	XXX extsb A1,A1\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TUCHAR,
		NAREG,	RESC1,
		"	lbz A1,AL" COM "uchar load\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TSHORT,
		NAREG,	RESC1,
		"	lhz A1,AL" COM "short load\n"
		"	extsh A1,A1\n", },

{ UMUL,	INAREG,
	SANY,		TANY,
	SOREG,		TUSHORT,
		NAREG,	RESC1,
		"	lhz A1,AL" COM "ushort load\n", },

{ UMUL, INBREG,
	SANY,		TANY,
	SOREG,		TLONGLONG|TULONGLONG,
		NBREG, RESC1,
		"	lwz A1,AL" COM "64-bit load\n"
		"	lwz U1,UL\n", },

/*
 * Logical/branching operators
 */

/* compare with register */
{ OPLOG,	FORCC,
	SAREG,	TSWORD|TSHORT|TCHAR,
	SAREG,	TSWORD|TSHORT|TCHAR,
		0, 	RESCC,
		"	cmp AL,AR	# AR-AL (sets flags)\n", },

/* compare with register */
{ OPLOG,	FORCC,
	SAREG,	TUWORD|TPOINT|TUSHORT|TUCHAR,
	SAREG,	TUWORD|TPOINT|TUSHORT|TUCHAR,
		0, 	RESCC,
		"	cmp AL,AR	# AR-AL (sets flags)\n", },

/* compare with register */
{ OPLOG,	FORCC,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		0, 	0,
		"ZD", },

/* AND/OR/ER */
{ OPSIMP,	INAREG,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1|RESCC,
		"	O A1,AL,AR\n", },

{ OPSIMP,	INAREG|FORCC,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
		"	Os A1,AL,AR\n", },

{ AND,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1|RESCC,
		"	and A1,AL,AR" COM "64-bit and\n"
		"	and U1,UL,UR\n", },

{ OR,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	orr A1,AL,AR" COM "64-bit or\n"
		"	orr U1,UL,UR\n" },

{ ER,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SBREG,	TLONGLONG|TULONGLONG,
		NBREG|NBSL,	RESC1,
		"	eor A1,AL,AR" COM "64-bit xor\n"
		"	eor U1,UL,UR\n" },

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	b LL\n", },

#if 0
{ GOTO, 	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	mtctr AL\n"
		"	bctr\n", },
#endif

/*
 * Convert LTYPE to reg.
 */

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	ldr A1,AL" COM "load word from memory\n", },

{ OPLTYPE,      INBREG,
        SANY,   	TANY,
        SOREG|SNAME,	TLONGLONG|TULONGLONG,
                NBREG,  RESC1,
                "	ldr A1,AL" COM "load long long from memory\n"
		"	ldr U1,UL\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TCHAR,
		NAREG,	RESC1,
		"	ldrsb A1,AL" COM "load char from memory\n" },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TUCHAR,
		NAREG,	RESC1,
		"	ldrb A1,AL" COM "load uchar from memory\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TSHORT,
		NAREG,	RESC1,
		"	ldrsh A1,AL" COM "load short from memory\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TUSHORT,
		NAREG,	RESC1,
		"	ldrh A1,AL" COM "load ushort from memory\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SCON,		TANY,
		NAREG,	RESC1,
		"	ldr A1,ZI" COM "load constant\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SCON,	TANY,
		NBREG,	RESC1,
		"	ldr A1,ZI" COM "load constant\n"
		"	mov U1,#0\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG,	TANY,
		NAREG,	RESC1,
		"	mov A1,AL" COM "load AL into A1\n" },

{ OPLTYPE,      INBREG,
        SANY,   TANY,
        SBREG,	TLONGLONG|TULONGLONG,
                NBREG,  RESC1,
		"	mov A1,AL" COM "load UL:AL into U1:A1\n"
                "       mov U1,UL\n", },

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
		"	mvn A1,AL\n", },

{ COMPL,	INBREG,
	SBREG,	TLONGLONG|TULONGLONG,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	mvn A1,AL"
		"	mvn U1,UL\n", },

/*
 * Arguments to functions.
 */

{ FUNARG,       FOREFF,
        SAREG,  TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SANY,   TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
                0,      0,
		"	stmfd sp!,{AL}" COM "save function arg to stack\n", },

{ FUNARG,       FOREFF,
        SBREG,  TLONGLONG|TULONGLONG,
        SANY,	TLONGLONG|TULONGLONG,
                0,      0,
		"	stmfd sp!,{UL,AL}" COM "save function arg to stack (endianness problem here?)\n", },

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
