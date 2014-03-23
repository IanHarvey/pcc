/*	$Id$	*/
/*
 * Copyright (c) 2014 Anders Magnusson (ragge@ludd.ltu.se).
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

#define TLL	TLONGLONG|TULONGLONG
#define	TABREG	TINT|TSHORT|TCHAR|TUNSIGNED|TUSHORT|TUCHAR
#define	SABREG	SAREG|SBREG

# define ANYSIGNED TINT|TSHORT|TCHAR
# define ANYUSIGNED TUNSIGNED|TUSHORT|TUCHAR
# define ANYFIXED ANYSIGNED|ANYUSIGNED
# define TUWORD TUNSIGNED
# define TSWORD TINT
# define TWORD	TUWORD|TSWORD
#define	TANYINT	TLL|ANYFIXED
#define	 SHINT	SAREG	/* Any integer */
#define	 ININT	INAREG
#define	 SHFL	SCREG	/* shape for long double */
#define	 INFL	INCREG	/* shape for long double */

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/* begin with all these casts */

/* char -> short/ushort */
{ SCONV,	INAREG|INBREG,
	SABREG,		TCHAR,
	SABREG,		TSHORT|TUSHORT,
		0,	RLEFT,
		"	ext.w AL\n", },

/* uchar -> short/ushort */
{ SCONV,	INAREG|INBREG,
	SABREG,		TUCHAR,
	SABREG,		TSHORT|TUSHORT,
		0,	RLEFT,
		"	and.l #255,AL\n", },

/* char -> (u)int */
{ SCONV,	INAREG|INBREG,
	SABREG,		TCHAR,
	SABREG,		TINT|TUNSIGNED,
		0,	RLEFT,
		"	extb.l AL\n", },

/* uchar -> (u)int */
{ SCONV,	INAREG|INBREG,
	SABREG,		TUCHAR,
	SABREG,		TINT|TUNSIGNED,
		0,	RLEFT,
		"	and.l #255,AL\n", },

/* short -> (u)int */
{ SCONV,	INAREG|INBREG,
	SABREG,		TSHORT,
	SABREG,		TINT|TUNSIGNED,
		0,	RLEFT,
		"	ext.l AL\n", },

/* ushort -> (u)int */
{ SCONV,	INAREG|INBREG,
	SABREG,		TUSHORT,
	SABREG,		TINT|TUNSIGNED,
		0,	RLEFT,
		"	and.l #65535,AL\n", },

{ ASSIGN,	FOREFF,
	SCREG|SNAME|SOREG,	TLL,
	SCREG|SNAME|SOREG,	TLL,
		0,	0,
		"	move.l AR,AL\n"
		"	move.l UR,UL\n", },

{ ASSIGN,	FOREFF,
	SABREG|SNAME|SOREG,	TABREG,
	SABREG|SNAME|SOREG,	TABREG,
		0,	0,
		"	move.l AR,AL\n", },

/* m/r |= r */
{ OPSIMP,	FOREFF,
	SABREG|SNAME|SOREG,		TABREG,
	SABREG|SNAME|SOREG|SCON,	TABREG,
		0,	RLEFT|RESCC,
		"	Oz.ZA AR,AL\n", },

{ OPSIMP,	FOREFF|INAREG|INBREG,
	SABREG,				TABREG,
	SABREG|SNAME|SOREG|SCON,	TABREG,
		0,	RLEFT|RESCC,
		"	Oz.ZA AR,AL\n", },

/*
 * Convert LTYPE to reg.
 */
{ OPLTYPE,	INAREG,
	SANY,				TANY,
	SABREG|SOREG|SNAME|SCON,	TABREG,
		NAREG,	RESC1,
		"	move.ZA AL,A1\n", },

{ OPLTYPE,	INBREG,
	SANY,				TANY,
	SABREG|SOREG|SNAME|SCON,	TABREG,
		NBREG,	RESC1,
		"	move.ZA AL,A1\n", },

{ UCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	jsr CL\n", },

{ CALL,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	jsr CL\nZB", },

{ FUNARG,	FOREFF,
	SAREG|SBREG|SOREG|SNAME|SCON,	TINT|TUNSIGNED,
	SANY,				TANY,
		0,	RNULL,
		"	move.l AL,-(%sp)\n", },

{ FUNARG,	FOREFF,
	SCON,	TPOINT,
	SANY,	TANY,
		0,	RNULL,
		"	pea CL\n", },

/*
 * Jumps.
 */
{ GOTO, 	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"	jmp LL\n", },

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
