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
 * A bunch conversions of integral<->integral types
 */

/* convert pointers to int. */
{ SCONV,	INTAREG,
	STAREG,	TPOINT|TWORD,
	SANY,	TWORD,
		0,	RLEFT,
		"", },

/* convert int to short/char. This is done when register is loaded */
{ SCONV,	INTAREG,
	STAREG,	TWORD,
	SANY,	TSHORT|TUSHORT|TCHAR|TUCHAR|TWORD,
		0,	RLEFT,
		"", },

/*
 * Store constant initializers.
 */
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
		"	call AL\n", },

{ UNARY CALL,	INTAREG,
	SAREG|STAREG,	TANY,
	SANY,	TWORD|TCHAR|TUCHAR|TSHORT|TUSHORT|TFLOAT|TDOUBLE|TLL|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\n", },

/*
 * The next rules handle all "+="-style operators.
 */
{ ASG OPSIMP,	INAREG|FOREFF,
	SAREG|STAREG,		TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"	Ol AR,AL\n", },

/*
 * The next rules handle all shift operators.
 */
{ ASG LS,	INTAREG|INAREG,
	STAREG|SAREG,	TWORD,
	STBREG|SCON,	TWORD,
		0,	RLEFT,
		"	sall ZA,AL\n", },

{ ASG LS,	FOREFF,
	STAREG|SAREG|SNAME|SOREG,	TWORD,
	STBREG|SCON,	TWORD,
		0,	RLEFT,
		"	sall ZA,AL\n", },

{ ASG RS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG|SNAME|SOREG,	TSWORD,
	STBREG|SCON,	TSWORD,
		0,	RLEFT,
		"	sarl ZA,AL\n", },

{ ASG RS,	INTAREG|INAREG|FOREFF,
	STAREG|SAREG|SNAME|SOREG,	TUWORD,
	STBREG|SCON,	TUWORD,
		0,	RLEFT,
		"	shrl ZA,AL\n", },

/*
 * The next rules takes care of assignments. "=".
 */
{ ASSIGN,	FOREFF|INAREG|INTAREG,
	SAREG|STAREG,	TWORD|TPOINT,
	SZERO,		TANY,
		0,	RLEFT,
		"	xorl AL,AL\n", },

{ ASSIGN,	FOREFF,
	STBREG|STAREG|SNAME|SOREG,	TWORD|TPOINT,
	STBREG|STAREG|SCON,		TWORD|TPOINT,
		0,	RRIGHT,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF,
	STBREG|STAREG,		TWORD|TPOINT,
	STBREG|STAREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"	movl AR,AL\n", },

{ ASSIGN,	FOREFF,
	STBREG|STAREG|SNAME|SOREG,	TSHORT|TUSHORT,
	STBREG|STAREG|SCON,		TWORD,
		0,	RRIGHT,
		"	movw ZR,ZL\n", },

{ ASSIGN,	FOREFF,
	STBREG|STAREG|SNAME|SOREG,	TCHAR|TUCHAR,
	STBREG|STAREG,			TWORD,
		0,	RLEFT,
		"	movb ZR,ZL\n", },

/*
 * DIV/MUL 
 */
{ ASG DIV,	INTAREG,
	STAREG,		TWORD|TPOINT,
	SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	cltd\n	idivl AR\n", },

{ ASG DIV,	INTAREG,
	STAREG,		TWORD|TPOINT,
	SCON,		TWORD|TPOINT,
		0,	RLEFT,
		"	movl AR,%ecx\n	cltd\n	idivl %ecx\n", },

{ ASG MUL,	INTAREG,
	STAREG,		TWORD|TPOINT,
	STAREG|STBREG|SNAME|SOREG|SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"	imull AR,AL\n", },

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
	SAREG,	TANY,
		NTEMP,	RESC1,
		"	movl AR,A1\n", },

/*
 * Logical/branching operators
 */

/* Can check anything by just comparing if EQ/NE */
{ OPLOG,	FORCC,
	SAREG|STAREG,	TWORD|TPOINT,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
		0, 	RESCC,
		"	cmpl AR,AL\n", },

{ OPLOG,	FORCC,  
	SAREG|STAREG,	TLL|TDOUBLE,
	SAREG|STAREG|SOREG|SNAME,	TLL|TDOUBLE,
		0,	RESCC,
		"diediedie!", },

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jmp LL\n", },

/*
 * Convert LTYPE to reg.
 */
{ OPLTYPE,	INTAREG,
	SANY,	TANY,
	SAREG|STAREG|SOREG|SNAME|SCON,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	movl AL,A1\n", },

{ OPLTYPE,	INTAREG,
	SANY,	TANY,
	SOREG|SNAME|SCON,	TSHORT|TUSHORT|TCHAR|TUCHAR,
		NAREG,	RESC1,
		"	movZB AL,A1\n", },

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
		"	pushl AR\n", },

{ OREG,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD,
		0,	RNULL,
		"	pushl AR\n", },

{ NAME,	FORARG,
	SANY,	TANY,
	SAREG|SNAME|SOREG,	TWORD,
		0,	RNULL,
		"	pushl AR\n", },

{ ICON,	FORARG,
	SANY,	TANY,
	SCON,	TCHAR|TUCHAR|TPTRTO,
		0,	RNULL,
		"	pushl $AR\n", },

{ REG,	FORARG,
	SANY,		TANY,
	SAREG|STAREG,	TLL|TDOUBLE,
		0,	RNULL,
		"	pushl AR\n	pushl UR\n", },


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
