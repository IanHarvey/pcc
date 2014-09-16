/*	$Id$	*/
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
 * Copyright (c) 2014 Alan Cox (alan@lxorguk.ukuu.org.uk).
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

/*
 * TODO
 *  -	clean up TLL usage so we are ready for 64bit longlong (T32 and T64?)
 *  -	clean up TCHAR|TUCHAR as T8
 *  -	Clean up all the SHINT SHCH etc oddities for clarity
 *  -	Should we use ctype() to eliminate INT/UINT at that stage ?
 *  -	Better optimisation of constant multiply/divide etc on 8086
 *  -	inc/inc is faster than add 2 (dec/dec likewise)
 *  -	some how lose the final bogus sp adjust we generate
 *  -	write helper lib for long and longlong ops
 *  -	add an E class and virtual 4 register longlong model
 *  -   bcc fastcall format
 *
 *  -	How to do configurable code-gen
 *	For 80186 we want to use the better shifts, push constant, mul/div
 *	features and enter/leave
 *	For 80286 it's not clear we need to do anything for real mode. However
 *	for large data/huge models protected mode means minimising ds/es
 *	reloads with the same data
 *	With 80386 there is lots more - but who wants 80386/16bit optimised
 *	code !
 *
 *  -	FPU has not been tackled. FPU/FPU should be fine (barring the odd
 *	gnu style instruction forms), but FPU/other is quite different
 *	because the 4 byte conversions are now long not int and two register
 *	while 8 byte longlong ones are going to need some gymnastics once
 *	we support the virtual 64bit registers
 *
 *  -	FPU/noFPU option. Assuming FPU present on 8086 is a bit obnoxious
 *	as a default. Need to be able to generate entirely soft-float calls.
 *
 *  -   We can't currently do various conversions we'd like to have, notably
 *	things like  "and ax, $ff"  to convert AL into AX from uchar to uint
 *	(that needs core changes and also allocator changes to try and avoid
 *	high bits being occupied by other users)
 *
 *  -	No attempt is made to deal with larger models than tiny
 *
 *	small mode should work (just keep constant data in data segments)
 *
 *	large code/small data should be fine-ish. Really that implies 32bit
 *	void * but for most uses you want 32bit only to be function pointers
 *	even if its not to spec. Function entry/exit needs to allow for the
 *	extra 2 bytes, call becomes call far, call ptr becomes a bit trickier
 *	but not a lot. Not that ld86 can link large model yet!
 *
 *	large data is much harder because an address is 32bit, half of which
 *	needs to keep getting stuck into a segment register. However we often
 *	(usually) work with multiple pointers to the same object. Any given
 *	object has a single segment so we will badly need optimisations to
 *	"share" segment data and avoid lots of duplicate register uses and
 *	mov es, foo statements.
 *
 *	huge model makes all pointers 32bit and all objects 32bit sized. That
 *	probably makes the sharing es issue go away somewhat, but raises the
 *	ugly problems of pointer normalisation (00:10 is the same as 01:00
 *	which b*ggers up comparisons royally) and also protected mode where
 *	your segment jump as you go over 64K boundaries is different. On the
 *	bright side huge model performance will suck whatever the compiler
 *	does.
 *
 */

# define TLL TLONGLONG|TULONGLONG|TLONG|TULONG
# define T16 TSHORT|TUSHORT|TINT|TUNSIGNED
# define ANYSIGNED TINT|TSHORT|TCHAR
# define ANYUSIGNED TUNSIGNED|TUSHORT|TUCHAR
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
	SAREG,	T16|TPOINT,
	SAREG,	T16|TPOINT,
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
	SHINT,	TPOINT|T16,
	SANY,	T16,
		0,	RLEFT,
		"", },

/* convert (u)long and (u)longlong to (u)longlong. */
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

/* convert char to 16bit types. */

/* 8086: Only as op on AL,AX */
{ SCONV,	ININT,
	SBREG|SOREG|SNAME,	TCHAR,
	SAREG,	T16,
		NSPECIAL|NAREG|NASL,	RESC1,
		"	cbw\n", },

/* convert unsigned char to 16bit types.
   We can do this one with any register */
{ SCONV,	ININT,
	SHCH|SOREG|SNAME,	TUCHAR,
	SAREG,	T16,
		NSPECIAL|NAREG,	RESC1,
		"ZT", },

/* convert char to (u)long
   8086 can only do cbw/cwd on AX and AX:DX pair */
{ SCONV,	INLL,
	SHCH|SOREG|SNAME,	TCHAR,
	SANY,	TLL,
		NSPECIAL|NCREG|NCSL,	RESC1,
		"; using AL\n	cbw\n	cwd\n", },

/* convert unsigned char to (u)long
   we can do this with any register */
/* FIXME: need to make this as ZZcode to get the asm right */
{ SCONV,	INLL,
	SHCH|SOREG|SNAME,	TUCHAR,
	SANY,			TLL,
		NCREG|NCSL,	RESC1,
		"	movb A1,AL\n	and A1, 0xff\n	xor U1,U1\n", },

/* convert char (in register) to double XXX - use NTEMP */
/* FIXME : need NSPECIAL to force into AL 
	check AX:DX right way around ! */
{ SCONV,	INFL,
	SHCH|SOREG|SNAME,	TCHAR,
	SHFL,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NASL|NDREG,	RESC2,
		"	cbw\n	cwd\n	push ax\n	push dx\n"
		"	fildl [sp]\n	add sp, 4\n", },

/* convert (u)char (in register) to double XXX - use NTEMP */
/* FIXME : needs to use ZT to get sizes right, need a register
   from somewhere to put 0 on the stack for the high bits */
{ SCONV,	INFL,
	SHCH|SOREG|SNAME,	TUCHAR,
	SHFL,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NASL|NDREG,	RESC2,
		"	movb A1 AL\n	and A1, 0xff\n	push A1\npush 0\n"
		"	fildl [sp]\n	add sp,4\n", },

/* 16bit to something */

/* convert 16bit to 16bit. */
{ SCONV,	INAREG,
	SAREG,	T16,
	SAREG,	T16,
		0,	RLEFT,
		"", },

/* convert 16bit (in memory) to char */
{ SCONV,	INCH,
	SNAME|SOREG,	T16|TPOINT,
	SHCH,		TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"	movb A1,AL\n", },

/* convert 16bit (in reg) to char. */
{ SCONV,	INCH,
	SAREG|SNAME|SOREG,	T16|TPOINT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL|NBREG|NBSL,	RESC1,
		"ZM", },

/* convert short/int to (u)long
   This must go via AX so we can use cwd
   
   FIXME: force into ax with NSPECIAL then don't want
   mov here */
{ SCONV,	INLL,
	SAREG|SOREG|SNAME,	TSHORT|TINT,
	SHLL,			TLL,
		NSPECIAL|NCREG|NCSL,	RESC1,
		"	mov A1, AL\n	cwd\n", },

/* convert unsigned short/int to (u)long
   Any pair will do. Note currently the compiler can't optimise
   out the reg->reg move involved */
{ SCONV,	INLL,
	SAREG|SOREG|SNAME,	TUSHORT|TUNSIGNED|TPOINT,
	SHLL,			TLL,
		NCREG|NCSL,	RESC1,
		"	mov A1,AL\n	xor U1,U1\n", },

/* convert short/int (in memory) to float/double */
{ SCONV,	INFL,
	SOREG|SNAME,	TSHORT|TINT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NDREG,	RESC1,
		"	fild AL\n", },

/* convert short/int (in register) to float/double */
{ SCONV,	INFL,
	SAREG,	TSHORT|TINT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NDREG,	RESC1,
		"	push AL\n	fild [sp]\n"
		"	inc sp\n	inc sp\n", },

/* convert unsigned short/int/ptr to double XXX - use NTEMP */
/* FIXME: need to force this via AX so we can cwd, fix stack setup to
   push both ax.dx*/
{ SCONV,	INFL,
	SAREG|SOREG|SNAME,	TUSHORT|TUNSIGNED|TPOINT,
	SHFL,			TLDOUBLE|TDOUBLE|TFLOAT,
		NAREG|NASL|NDREG|NTEMP,	RESC2,
		"	cwd\n		push ax\n"
		"	fild [sp]\n	inc sp\n"
		"	inc sp\n", },

/* long (and thus long long for now) to something */

/* convert (u)long to (u)char (mem->reg) */
{ SCONV,	INCH,
	SOREG|SNAME,	TLL,
	SANY,	TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"	movb A1, AL\n", },

/* convert (u)long to (u)char (reg->reg, hopefully nothing) */
{ SCONV,	INCH,
	SHLL,	TLL,
	SANY,	TCHAR|TUCHAR,
		NBREG|NBSL|NTEMP,	RESC1,
		"ZS", },

/* convert (u)long to (u)short (mem->reg) */
{ SCONV,	INAREG,
	SOREG|SNAME,	TLL,
	SAREG,	T16|TPOINT,
		NAREG|NASL,	RESC1,
		"	mov A1,AL\n", },

/* convert (u)long to 16bit (reg->reg, hopefully nothing) */
{ SCONV,	INAREG,
	SHLL|SOREG|SNAME,	TLL,
	SAREG,	T16,
		NAREG|NASL|NTEMP,	RESC1,
		"ZS", },

/* FIXME: float stuff is all TODO */

/* convert long (in memory) to floating */
{ SCONV,	INFL,
	SOREG|SNAME,	TLONGLONG,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		NDREG,	RESC1,
		"	fild AL\n", },

/* convert long (in register) to floating */
{ SCONV,	INFL,
	SHLL,	TLONGLONG,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		NTEMP|NDREG,	RESC1,
		"	push UL\n	push AL\n"
		"	fild [sp]\n	add sp, 8\n", },

/* convert unsigned long to floating */
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
		"	subl $4,%sp\n	fistpl (%sp)\n	popl A1\n", },

/* convert float/double to (u) int/short/char. XXX should use NTEMP here */
{ SCONV,	INCH,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHCH,	TWORD|TSHORT|TUSHORT|TCHAR|TUCHAR,
		NCREG,	RESC1,
		"	subl $4,%sp\n	fistpl (%sp)\n	popl A1\n", },
#endif

/* convert float/double to int. XXX should use NTEMP here */
{ SCONV,	INAREG,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	sub sp, 12\n"
		"	fstcw sp\n"
		"	fstcw 4[sp]\n"
		"	movb 1[sp], 12\n"
		"	fldcw sp\n"
		"	fistp 8[sp]\n"
		"	movl A1, 8(p)\n"
		"	fldcw 4[sp]\n"
		"	add sp, 4\n", },

/* convert float/double to unsigned int. XXX should use NTEMP here */
{ SCONV,       INAREG,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	subl $16,%sp\n"
		"	fnstcw (%sp)\n"
		"	fnstcw 4(%sp)\n"
		"	movb $12,1(%sp)\n"
		"	fldcw (%sp)\n"
		"	fistpq 8(%sp)\n"
		"	movl 8(%sp),A1\n"
		"	fldcw 4(%sp)\n"
		"	addl $16,%sp\n", },

/* convert float/double (in register) to long long */
{ SCONV,	INLL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHLL,	TLONGLONG,
		NCREG,	RESC1,
		"	subl $16,%sp\n"
		"	fnstcw (%sp)\n"
		"	fnstcw 4(%sp)\n"
		"	movb $12,1(%sp)\n"
		"	fldcw (%sp)\n"
		"	fistpq 8(%sp)\n"
		"	movl 8(%sp),A1\n"
		"	movl 12(%sp),U1\n"
		"	fldcw 4(%sp)\n"
		"	addl $16,%sp\n", },

/* convert float/double (in register) to unsigned long long */
{ SCONV,	INLL,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHLL,	TULONGLONG,
		NCREG,	RESC1,
		"	subl $16,%sp\n"
		"	fnstcw (%sp)\n"
		"	fnstcw 4(%sp)\n"
		"	movb $15,1(%sp)\n"	/* 64-bit prec */
		"	fldcw (%sp)\n"
		"	movl $0x5f000000, 8(%sp)\n"	/* (float)(1<<63) */
		"	fsubs 8(%sp)\n"	/* keep in range of fistpq */
		"	fistpq 8(%sp)\n"
		"	xorb $0x80,15(%sp)\n"	/* addq $1>>63 to 8(%sp) */
		"	movl 8(%sp),A1\n"
		"	movl 12(%sp),U1\n"
		"	fldcw 4(%sp)\n"
		"	addl $16,%sp\n", },
 


/* slut sconv */

/*
 * Subroutine calls.
 */

{ UCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call CL\nZC", },

{ CALL,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call CL\nZC", },

//{ UCALL,	FOREFF,
//	SCON,	TANY,
//	SAREG,	TWORD|TPOINT,
//		0,	0,
//		"	call CL\nZC", },

{ CALL,	INAREG,
	SCON,	TANY,
	SAREG,	T16|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INAREG,
	SCON,	TANY,
	SAREG,	T16|TPOINT,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ CALL,	INBREG,
	SCON,	TANY,
	SBREG,	TCHAR|TUCHAR,
		NBREG,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INBREG,
	SCON,	TANY,
	SBREG,	TCHAR|TUCHAR,
		NBREG,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ CALL,		INCREG,
	SCON,	TANY,
	SCREG,	TANY,
		NCREG|NCSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ UCALL,	INCREG,
	SCON,	TANY,
	SCREG,	TANY,
		NCREG|NCSL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

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

{ STCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	0,
		"	call CL\nZC", },

{ STCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call CL\nZC", },

{ STCALL,	INAREG,
	SNAME|SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,	/* should be 0 */
		"	call *AL\nZC", },

/*
 * The next rules handle all binop-style operators.
 */
/* Special treatment for long */
{ PLUS,		INLL|FOREFF,
	SHLL,		TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	add AL,AR\n	adc UL,UR\n", },

{ PLUS,		INLL|FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SHLL|SCON,		TLL,
		0,	RLEFT,
		"	add AL,AR\n	adc UL,UR\n", },

/* Special treatment for long  XXX - fix commutative check */
{ PLUS,		INLL|FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SHLL,			TLL,
		0,	RRIGHT,
		"	add AL,AR\n	adc UL,UR\n", },

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
	SAREG|SNAME|SOREG,	T16|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	inc AL\n", },

{ PLUS,		INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SONE,	TANY,
		0,	RLEFT,
		"	inc AL\n", },

{ PLUS,		INAREG,
	SAREG,	T16,
	SAREG,	T16,
		NAREG|NASL|NASR,	RESC1,
		"	lea A1, [AL+AR]\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	T16|TPOINT,
	SONE,			TANY,
		0,	RLEFT,
		"	dec AL\n", },

{ MINUS,	INCH|FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SONE,	TANY,
		0,	RLEFT,
		"	dec AL\n", },

/* address as register offset, negative */
{ MINUS,	INLL|FOREFF,
	SHLL,	TLL,
	SHLL|SNAME|SOREG,	TLL,
		0,	RLEFT,
		"	sub AR,AL\n	sbb UR,UL\n", },

{ MINUS,	INLL|FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SHLL|SCON,	TLL,
		0,	RLEFT,
		"	sub AR,AL\n	sbb UR,UL\n", },

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
/* m/r |= r */

{ OPSIMP,	INAREG|FOREFF|FORCC,
	SHINT|SNAME|SOREG,	T16|TPOINT,
	SHINT,		T16|TPOINT,
		0,	RLEFT|RESCC,
		"	Ow AR,AL\n", },

/* r |= r/m */
{ OPSIMP,	INAREG|FOREFF|FORCC,
	SHINT,		T16,
	SHINT|SNAME|SOREG,	T16,
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
	SHINT|SNAME|SOREG,	T16|TPOINT,
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
		"	Ow AR,AL\n	Ow UR,UL\n", },

/* m/r |= r/const */
{ OPSIMP,	INLL|FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SHLL|SCON,	TLL,
		0,	RLEFT,
		"	Ow AR,AL\n	Ow UR,UL\n", },

/* Try use-reg instructions first */
{ PLUS,		INAREG,
	SAREG,	T16|TPOINT,
	SCON,	TANY,
		NAREG|NASL,	RESC1,
		"	lea A1, CR[AL]\n", },

{ MINUS,	INAREG,
	SAREG,	T16|TPOINT,
	SPCON,	TANY,
		NAREG|NASL,	RESC1,
		"	lea A1, -CR[AL]\n", },


/*
 * The next rules handle all shift operators.
 */
/* (u)long left shift is emulated */

/* FIXME: for 8086 we only have shift by 1 or shift by CL
   so we need to fix this up further. Also need to optimise
   shifts by 8 16 24 and 32 as moves between registers for
   the bigger types. (eg >> 16 on a long might be mov dx, ax, xor ax, ax) */
{ LS,	INCREG,
	SCREG,	TLL,
	SHCH,	TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"ZO", },

/* r/m <<= r */
{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	T16,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shlw AR,AL\n", },

/* r/m <<= const */
{ LS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	T16,
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

/* (u)long right shift is emulated */
{ RS,	INCREG,
	SCREG,	TLL,
	SHCH,	TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"ZO", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TINT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	sarw AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TSHORT|TINT,
	SCON,			TANY,
		0,		RLEFT,
		"	sarw AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT|TUNSIGNED|TPOINT,
	SHCH,			TCHAR|TUCHAR,
		NSPECIAL,	RLEFT,
		"	shrw AR,AL\n", },

{ RS,	INAREG|FOREFF,
	SAREG|SNAME|SOREG,	TUSHORT|TUNSIGNED,
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
{ ASSIGN,	FORCC|FOREFF|INLL,
	SHLL,		TLL,
	SMIXOR,		TANY,
		0,	RDEST,
		"	xor AL,AL\n	xor UL,UL\n", },

{ ASSIGN,	FORCC|FOREFF|INLL,
	SHLL,		TLL,
	SMILWXOR,	TANY,
		0,	RDEST,
		"	xor AL,AL\n	mov UR,UL\n", },

{ ASSIGN,	FORCC|FOREFF|INLL,
	SHLL,		TLL,
	SMIHWXOR,	TANY,
		0,	RDEST,
		"	mov AL,AR\n	xor UL,UL\n", },

{ ASSIGN,	FOREFF|INLL,
	SHLL,		TLL,
	SCON,		TANY,
		0,	RDEST,
		"	mov AL,AR\n	mov UR,UL\n", },

{ ASSIGN,	FOREFF,
	SHLL|SNAME|SOREG,	TLL,
	SCON,		TANY,
		0,	0,
		"	mov AL,AR\n	mov UR,UL\n", },

{ ASSIGN,	FOREFF,
	SAREG|SNAME|SOREG,	T16|TPOINT,
	SCON,		TANY,
		0,	0,
		"	mov AL, AR\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	T16|TPOINT,
	SCON,		TANY,
		0,	RDEST,
		"	mov AL, AR\n", },

{ ASSIGN,	FOREFF,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SCON,		TANY,
		0,	0,
		"	movb AL, AR\n", },

{ ASSIGN,	FOREFF|INCH,
	SHCH,		TCHAR|TUCHAR,
	SCON,		TANY,
		0,	RDEST,
		"	movb AL, AR\n", },

{ ASSIGN,	FOREFF|INLL,
	SNAME|SOREG,	TLL,
	SHLL,		TLL,
		0,	RDEST,
		"	mov AL,AR\n	mov UL,UR\n", },

{ ASSIGN,	FOREFF|INLL,
	SHLL,	TLL,
	SHLL,	TLL,
		0,	RDEST,
		"ZH", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG|SNAME|SOREG,	T16|TPOINT,
	SAREG,		T16|TPOINT,
		0,	RDEST,
		"	mov AL,AR\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,			T16|TPOINT,
	SAREG|SNAME|SOREG,	T16|TPOINT,
		0,	RDEST,
		"	mov AL,AR\n", },

{ ASSIGN,	FOREFF|INCH,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
	SHCH,		TCHAR|TUCHAR|T16,
		0,	RDEST,
		"	movb AL,AR\n", },

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
		"	fstpt AL\n	fldt AL\n", }, /* XXX */

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

{ STASG,	INAREG|FOREFF,
	SOREG|SNAME,	TANY,
	SAREG,		TPTRTO|TANY,
		NSPECIAL|NAREG,	RDEST,
		"F	mov A1,si\nZQF	mov si,A1\n", },

/*
 * DIV/MOD/MUL 
 */
/* long div is emulated */
{ DIV,	INCREG,
	SCREG|SNAME|SOREG|SCON, TLL,
	SCREG|SNAME|SOREG|SCON, TLL,
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
		"ZO", },

/* REVIEW We can only do (i)divb ax/byte  and (i)divw (dx:ax)/word
   and the results are always in ah/al (remainer/mod)
   or dx:ax (dx = remainer, ax = mod) */
   
   
{ DIV,	INAREG,
	SAREG,			TUSHORT|TUNSIGNED|TPOINT,
	SAREG|SNAME|SOREG,	TUSHORT|TUNSIGNED|TPOINT,
		NSPECIAL,	RDEST,
		"	xor dx,dx\n	divw AR\n", },

{ DIV,	INCH,
	SHCH,			TUCHAR,
	SHCH|SNAME|SOREG,	TUCHAR,
		NSPECIAL,	RDEST,
		"	xor ah,ah\n	divb AR\n", },

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

/* (u)long mod is emulated */
{ MOD,	INCREG,
	SCREG|SNAME|SOREG|SCON, TLL,
	SCREG|SNAME|SOREG|SCON, TLL,
		NSPECIAL|NCREG|NCSL|NCSR,	RESC1,
		"ZO", },

{ MOD,	INAREG,
	SAREG,			T16|TPOINT,
	SAREG|SNAME|SOREG,	T16|TPOINT,
		NAREG|NSPECIAL,	RESC1,
		"	xor dx,dx\n	divw AR\n", },

{ MOD,	INCH,
	SHCH,			TUCHAR,
	SHCH|SNAME|SOREG,	TUCHAR,
		NBREG|NSPECIAL,	RESC1,
		"	xor ah,ah\n	divb AR\n", },

/* (u)long mul is emulated */
/* On 8086 we can only do multiplies of al * value into ax (for 8bit)
   or ax * value into dx:ax for 16bit 
   
   80186 allows us to do a signed multiply of a register with a constant
   into a second register */
   
{ MUL,	INCREG,
	SCREG,	TLL,
	SCREG,	TLL,
		NSPECIAL,	RDEST,
		"ZO", },

/* FIMXE: need special rules */
{ MUL,	INAREG,
	SAREG,			T16|TPOINT,
	SAREG|SNAME|SOREG,	T16|TPOINT,
		NSPECIAL,	RDEST,
		"	mul AR\n", },

{ MUL,	INCH,
	SHCH,			TCHAR|TUCHAR,
	SHCH|SNAME|SOREG,	TCHAR|TUCHAR,
		NSPECIAL,	RDEST,
		"	mulb AR\n", },

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
		"	mov UL,U1\n	mov AL,A1\n", },

{ UMUL,	INAREG,
	SANY,	TPOINT|T16,
	SOREG,	TPOINT|T16,
		NAREG|NASL,	RESC1,
		"	mov AL,A1\n", },

{ UMUL,	INCH,
	SANY,	TANY,
	SOREG,	TCHAR|TUCHAR,
		NBREG|NBSL,	RESC1,
		"	movb AL,A1\n", },

{ UMUL,	INAREG,
	SANY,	TANY,
	SOREG,	T16,
		NAREG|NASL,	RESC1,
		"	mov AL,A1\n", },

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
	SHLL|SOREG|SNAME,	TLL,
	SHLL,			TLL,
		0,	0,
		"ZD", },

{ OPLOG,	FORCC,
	SAREG|SOREG|SNAME,	T16|TPOINT,
	SCON|SAREG,	T16|TPOINT,
		0, 	RESCC,
		"	cmp AL,AR\n", },


{ OPLOG,	FORCC,
	SCON|SAREG,	T16|TPOINT,
	SAREG|SOREG|SNAME,	T16|TPOINT,
		0, 	RESCC,
		"	cmp AL,AR\n", },

{ OPLOG,	FORCC,
	SBREG|SOREG|SNAME,	TCHAR|TUCHAR,
	SCON|SBREG,	TANY,
		0, 	RESCC,
		"	cmpb AL,AR\n", },

{ OPLOG,	FORCC,
	SCON|SBREG,	TCHAR|TUCHAR,
	SBREG|SOREG|SNAME,	TANY,
		0, 	RESCC,
		"	cmpb AL,AR\n", },

{ OPLOG,	FORCC,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
	SDREG,	TLDOUBLE|TDOUBLE|TFLOAT,
		0, 	RNOP,
		"ZG", },

{ OPLOG,	FORCC,
	SANY,	TANY,
	SANY,	TANY,
		REWRITE,	0,
		"diediedie!", },

/* AND/OR/ER/NOT */
{ AND,	INCREG|FOREFF,
	SCREG,			TLL,
	SCREG|SOREG|SNAME,	TLL,
		0,	RLEFT,
		"	andw AR,AL\n	andw UR,UL\n", },

{ AND,	INAREG|FOREFF,
	SAREG,			T16,
	SAREG|SOREG|SNAME,	T16,
		0,	RLEFT,
		"	andw AR,AL\n", },

{ AND,	INAREG|FOREFF,  
	SAREG|SOREG|SNAME,	T16,
	SCON|SAREG,		T16,
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
{ OPLTYPE,	FORCC|INLL,
	SCREG,	TLL,
	SMIXOR,	TANY,
		NCREG,	RESC1,
		"	xor U1,U1\n	xor A1,A1\n", },

{ OPLTYPE,	FORCC|INLL,
	SCREG,	TLL,
	SMILWXOR,	TANY,
		NCREG,	RESC1,
		"	mov U1,UL\n	xor A1,A1\n", },

{ OPLTYPE,	FORCC|INLL,
	SCREG,	TLL,
	SMIHWXOR,	TANY,
		NCREG,	RESC1,
		"	xor U1,U1\n	mov A1,AL\n", },

{ OPLTYPE,	INLL,
	SANY,	TANY,
	SCREG,	TLL,
		NCREG,	RESC1,
		"ZK", },

{ OPLTYPE,	INLL,
	SANY,	TANY,
	SCON|SOREG|SNAME,	TLL,
		NCREG,	RESC1,
		"	mov U1,UL\n	mov A1,AL\n", },

{ OPLTYPE,	FORCC|INAREG,
	SAREG,	T16|TPOINT,
	SMIXOR,	TANY,
		NAREG|NASL,	RESC1,
		"	xor A1,A1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG|SCON|SOREG|SNAME,	T16|TPOINT,
		NAREG|NASL,	RESC1,
		"	mov A1,AL\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG|SOREG|SNAME|SCON,	TCHAR|TUCHAR,
		NBREG,	RESC1,
		"	mov A1,AL\n", },

{ OPLTYPE,	FORCC|INAREG,
	SAREG,	TSHORT|TUSHORT,
	SMIXOR,	TANY,
		NAREG,	RESC1,
		"	xor A1,A1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG|SOREG|SNAME|SCON,	T16,
		NAREG,	RESC1,
		"	mov A1,AL\n", },

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

/*
 * Negate a word.
 */

{ UMINUS,	INCREG|FOREFF,
	SCREG,	TLL,
	SCREG,	TLL,
		0,	RLEFT,
		"	neg AL\n	adc UL,0\n	neg UL\n", },

{ UMINUS,	INAREG|FOREFF,
	SAREG,	T16|TPOINT,
	SAREG,	T16|TPOINT,
		0,	RLEFT,
		"	neg AL\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG,	TCHAR|TUCHAR,
	SBREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"	neg AL\n", },

{ UMINUS,	INFL|FOREFF,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
	SHFL,	TLDOUBLE|TDOUBLE|TFLOAT,
		0,	RLEFT,
		"	fchs\n", },

{ COMPL,	INCREG,
	SCREG,	TLL,
	SANY,	TANY,
		0,	RLEFT,
		"	not AL\n	not UL\n", },

{ COMPL,	INAREG,
	SAREG,	T16,
	SANY,	TANY,
		0,	RLEFT,
		"	not AL\n", },

{ COMPL,	INBREG,
	SBREG,	TCHAR|TUCHAR,
	SANY,	TANY,
		0,	RLEFT,
		"	not AL\n", },

/*
 * Arguments to functions.
 *
 * char has already been promoted to integer types
 */
 
/* Push immediate not 8086.. */
{ FUNARG,	FOREFF,
	/*SCON|*/SCREG|SNAME|SOREG,	TLL,
	SANY,	TLL,
		0,	RNULL,
		"	push UL\n	push AL\n", },

{ FUNARG,	FOREFF,
	/*SCON|*/SAREG|SNAME|SOREG,	T16|TPOINT,
	SANY,	T16|TPOINT,
		0,	RNULL,
		"	push AL\n", },

/* FIXME: FPU needs reworking into 4 regs or a memcpy */
{ FUNARG,	FOREFF,
	SNAME|SOREG,	TDOUBLE,
	SANY,	TDOUBLE,
		0,	0,
		"	pushl UL\n	pushl AL\n", },

{ FUNARG,	FOREFF,
	SDREG,	TDOUBLE,
	SANY,		TDOUBLE,
		0,	0,
		"	sub sp,8\n	fstpl [sp]\n", },

{ FUNARG,	FOREFF,
	SNAME|SOREG,	TFLOAT,
	SANY,		TFLOAT,
		0,	0,
		"	pushl AL\n", },

{ FUNARG,	FOREFF,
	SDREG,	TFLOAT,
	SANY,		TFLOAT,
		0,	0,
		"	sub sp,4\n	fstps [sp]\n", },

{ FUNARG,	FOREFF,
	SDREG,	TLDOUBLE,
	SANY,		TLDOUBLE,
		0,	0,
		"	sub sp,12\n	fstpt [sp]\n", },

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
