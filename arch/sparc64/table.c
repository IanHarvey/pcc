/*
 * Copyright (c) 2008 David Crawshaw <david@zentus.com>
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
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

#include "pass2.h"

#define TUWORD TUNSIGNED|TULONG
#define TSWORD TINT|TLONG
#define TWORD TUWORD|TSWORD

struct optab table[] = {

{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },      /* empty */

{ PCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	! convert between word and pointer", },

/* Conversions. TODO: check zeroing on down conversions and signed/unsigned */

{ SCONV,	INAREG,
	SOREG,  TCHAR,
	SAREG,	TSWORD|TSHORT,
		NAREG,	RESC1,
		"	ldsb [AL],A1	! int8->int16/int32\n"
		"	nop\n", },

{ SCONV, 	INAREG,
	SOREG,	TCHAR,
	SAREG,	TUWORD|TUSHORT|TUCHAR,
		NAREG,	RESC1,
		"	ldub [AL],A1	! int8 -> uint16/uint32\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,  TUCHAR,
	SAREG,	TWORD|TSHORT|TUSHORT,
		NAREG,	RESC1,
		"	ldub [AL],A1	! int8 -> (u)int16/(u)int32\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TCHAR,
	SAREG,	TLONGLONG,
		NAREG,	RESC1,
		"	ldsb [AL],A1	! int8 -> int64\n"
      		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TUCHAR,
	SAREG,	TLONGLONG|TULONGLONG,
		NAREG,	RESC1,
		"	ldub [AL],A1	! uint8 -> (u)int64\n"
      		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TSHORT|TUSHORT,
	SAREG,	TCHAR,
		NAREG,	RESC1,
		"	ldsh [AL],A1 	! (u)int16 -> int8\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TSHORT|TUSHORT,
	SAREG,  TUCHAR,
		NAREG,	RESC1,
		"	ldsh [AL],A1 	! (u)int16 -> uint8\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TSHORT,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	ldsh [AL],A1 	! int16 -> int32\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TSHORT,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	lduh [AL],A1 	! int16 -> uint32\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TUSHORT,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	lduh [AL],A1 	! uint16 -> int32\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TSHORT,
	SAREG,	TLONGLONG,
		NAREG,	RESC1,
		"	ldsh [AL],A1 	! int16 -> int64\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TSHORT,
	SAREG,	TULONGLONG,
		NAREG,	RESC1,
		"	lduh [AL],A1 	! int16 -> uint64\n"
      		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TUSHORT,
	SAREG,	TLONGLONG|TULONGLONG,
		NAREG,	RESC1,
		"	lduh [AL],A1 	! uint16 -> uint64\n"
      		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TCHAR,
		NAREG,	RESC1,
		"	ldsw [AL],A1 	! int32 -> int8\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TUCHAR,
		NAREG,	RESC1,
		"	lduw [AL],A1 	! int32 -> uint8\n"
		"	nop\n", },
    
{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TSHORT,
		NAREG,	RESC1,
		"	ldsw [AL],A1 	! int32 -> int16\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TWORD,
	SAREG,	TUSHORT,
		NAREG,	RESC1,
		"	lduw [AL],A1 	! int32 -> uint16\n"
		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TSWORD,
	SAREG,	TLONGLONG|TULONGLONG,
		NAREG,	RESC1,
		"	ldsw [AL],A1 	! int32 -> (u)int64\n"
      		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TUWORD,
	SAREG,	TLONGLONG|TULONGLONG,
		NAREG,	RESC1,
		"	lduw [AL],A1 	! int32 -> (u)int64\n"
      		"	nop\n", },

{ SCONV,	INAREG,
	SOREG,	TLONGLONG|TULONGLONG,
	SAREG,	TCHAR|TUCHAR|TSHORT|TUSHORT|TWORD,
		NAREG,	RESC1,
		"	ldx [AL],A1 	! int64 -> (u)int8/16/32\n"
		"	nop\n", },


/* Multiplication and division */

{ MUL,	INAREG,
	SAREG,	TANY,
	SAREG,	TANY,
		NAREG|NASR|NASL,	RESC1,
		"	mulx AL,AR,AR	! multiply\n", },

{ DIV,	INAREG,
	SAREG,	TUWORD|TUSHORT|TUCHAR|TULONGLONG,
	SAREG,	TUWORD|TUSHORT|TUCHAR|TULONGLONG,
		NAREG|NASR|NASL,	RESC1,
		"	udivx AL,AR,AR	! unsigned division\n", },

{ DIV,	INAREG,
	SAREG,	TWORD|TUSHORT|TSHORT|TUCHAR|TCHAR|TULONGLONG|TLONGLONG,
	SAREG,	TWORD|TUSHORT|TSHORT|TUCHAR|TCHAR|TULONGLONG|TLONGLONG,
		NAREG|NASR|NASL,	RESC1,
		"	sdivx AL,AR,AR	! signed division\n", },

		/* TODO MOD */

{ PLUS,	INAREG,
	SAREG,	TULONGLONG|TLONGLONG,
	SAREG,	TULONGLONG|TLONGLONG,
		NAREG,	RESC1,
		"	addc A1,AL,AR 	! add 64-bit, XXX does this work?\n", },

{ PLUS,	INAREG,
	SAREG,	TSWORD|TUWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SAREG,	TSWORD|TUWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG|NASL,	RESC1,
      		"	add A1,AL,AR\n", },

{ PLUS,	INAREG,
	SAREG,	TSWORD|TSHORT|TCHAR|TUWORD|TUSHORT|TUCHAR,
	SSCON,	TWORD,
		NAREG|NASL,	RESC1,
		"	add AL,AR,A1	! add a constant to a register\n", },

{ MINUS,	INAREG,
	SAREG,	TSWORD|TUWORD|TSHORT|TUSHORT|TCHAR|TUCHAR|TLONGLONG|TULONGLONG,
	SAREG,	TSWORD|TUWORD|TSHORT|TUSHORT|TCHAR|TUCHAR|TLONGLONG|TULONGLONG,
		NAREG|NASL,	RESC1,
      		"	sub AL,AR,AR\n", },

{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
	SSCON,	TANY,
		NAREG|NASL,	RESC1,
		"	sub A1,AR,AL	! substrct constant from register\n", },

{ UMINUS,	INAREG,
	SAREG,	TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR|TLONGLONG|TULONGLONG,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	sub A1,AL,A1\n", },

/* Assignments */

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TWORD|TPOINT,
	SAREG,		TWORD|TPOINT,
		0,	RDEST,
		"	st AR,[AL]		! store (u)int32\n"
		"	nop\n", },	

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TSHORT|TUSHORT,
	SAREG,		TSHORT|TUSHORT,
		0,	RDEST,
        	"	sth AR,[AL]		/* store (u)int16 */\n"
		"	nop\n", },	

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TCHAR|TUCHAR,
	SAREG,		TCHAR|TUCHAR,
		0,	RDEST,
        	"	stb AR,[AL]		! store (u)int8\n"
		"	nop\n", },	

{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TLONGLONG|TULONGLONG,
	SAREG,		TLONGLONG|TULONGLONG,
		0,	RDEST,
		"	stx AR,[AL] 		! store (u)int64\n"
		"	nop\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TANY,
	SAREG,	TANY,
		0,	RDEST,
		"	mov AL, AR		! register move\n", },

/* Comparisons. */

{ EQ,	FORCC,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
                0,      RESCC,
		"	cmp AL,AR\n"
		"	be LC\n"
		"	nop\n", },

{ NE,	FORCC,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
        SAREG,		TWORD|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR,
                0,      RESCC,
		"	cmp AL,AR,\n"
                "	bne LC\n"
		"	nop\n", },

/* Convert LTYPE to reg. */

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TCHAR,
		NAREG,	RESC1,
		"	ldsb [AL],A1		! load int8 to reg\n"
		"	nop\n", },
	
{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TUCHAR,
		NAREG,	RESC1,
		"	ldub [AL],A1		! load uint8 to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TSHORT,
		NAREG,	RESC1,
		"	ldsh [AL],A1		! load int16 to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TUSHORT,
		NAREG,	RESC1,
		"	lduh [AL],A1		! load uint16 to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	ldsw [AL],A1		! load int32 to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	lduw [AL],A1		! load uint32 to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,		TANY,
	SOREG|SNAME,	TLONGLONG|TULONGLONG,
		NAREG,	RESC1,
		"	ldx [AL],A1		! load int64/uint64 to reg\n"
		"	nop\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TPOINT,
		NAREG,	RESC1,
		"	la A1,AL		! load address to reg\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SZERO,	TANY,
		NAREG,	RESC1,
		"	mov %%g0,A1		! load 0 to reg\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TANY,
		NAREG,	RESC1,
		"	mov AL,A1		! load constant to reg\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SANY,	TANY,
		NAREG,	RESC1,
		"	mov AL,A1\n", },

/* Jumps. */

{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	!XXX jmp LL		! goto label\n"
		"	nop\n", }, /* XXX */

{ UCALL,	FOREFF,
	SCON,		TANY,
	SANY,		TANY,
		0,	0,
		"	call CL			! void CL()\n"
		"	nop\n", },

{ UCALL,         INAREG,
        SCON,		TANY,
        SAREG,          TANY,
                NAREG,     RESC1,
		"	call CL			! = CL()\n"
		" 	nop\n", },

{ CALL,		FOREFF,
	SCON,		TANY,
	SANY,		TANY,
		0,	0,
		"	call CL			! void CL(constant)\n"
		"	nop\n", },

{ CALL,		FOREFF,
	SCON,		TANY,
	SAREG,		TANY,
		NAREG,		RESC1,
		"	call CL			! = CL(constant)\n"
		"	nop\n", },

{ CALL,		FOREFF,
	SAREG,		TANY,
	SANY,		TANY,
		0,	0,
		"	call CL			! void CL(args)\n"
		"	nop\n", },

{ CALL,         INAREG,
        SAREG,		TANY,
        SAREG,		TANY,
                NAREG,     RESC1,
		"	call CL			! = CL(args)\n"
		"	nop\n", },

{ FREE,FREE,FREE,FREE,FREE,FREE,FREE,FREE, "ERR: printing free op\n" },

};

int tablesize = sizeof(table)/sizeof(table[0]);
