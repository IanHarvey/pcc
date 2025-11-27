/*	$Id$	*/
/*
 * Copyright (c) 2025 LLVM Backend Implementation
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

/*
 * Machine-dependent defines for LLVM IR backend.
 */

/*
 * Convert (multi-)character constant to integer.
 */
#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);

#define ARGINIT		0	/* arguments start at offset 0 */
#define AUTOINIT	0	/* automatics start at offset 0 */

/*
 * Storage space requirements (64-bit target)
 */
#define SZCHAR		8
#define SZBOOL		8
#define SZSHORT		16
#define SZINT		32
#define SZLONG		64
#define SZPOINT(t)	64
#define SZLONGLONG	64
#define SZFLOAT		32
#define SZDOUBLE	64
#define SZLDOUBLE	128

/*
 * Alignment constraints
 */
#define ALCHAR		8
#define ALBOOL		8
#define ALSHORT		16
#define ALINT		32
#define ALLONG		64
#define ALPOINT		64
#define ALLONGLONG	64
#define ALFLOAT		32
#define ALDOUBLE	64
#define ALLDOUBLE	128
#define ALSTACK		64
#define ALMAX		128

/*
 * Min/max values.
 */
#define	MIN_CHAR	-128
#define	MAX_CHAR	127
#define	MAX_UCHAR	255
#define	MIN_SHORT	-32768
#define	MAX_SHORT	32767
#define	MAX_USHORT	65535
#define	MIN_INT		(-0x7fffffff-1)
#define	MAX_INT		0x7fffffff
#define	MAX_UNSIGNED	0xffffffffU
#define	MIN_LONG	0x8000000000000000LL
#define	MAX_LONG	0x7fffffffffffffffLL
#define	MAX_ULONG	0xffffffffffffffffULL
#define	MIN_LONGLONG	0x8000000000000000LL
#define	MAX_LONGLONG	0x7fffffffffffffffLL
#define	MAX_ULONGLONG	0xffffffffffffffffULL

/* Default char is signed */
#undef	CHAR_UNSIGNED
#define	BOOL_TYPE	UCHAR	/* what used to store _Bool */

/*
 * Use large-enough types.
 */
typedef	long long CONSZ;
typedef	unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"%lld"		/* format for printing constants */
#define LABFMT	"L%d"		/* format for printing labels (LLVM style) */
#define	STABLBL	"LL%d"		/* format for stab (debugging) labels */

#define	TARGET_TIMODE		/* has TI/TF/TC types (128 bit) */

#define BACKAUTO 		/* stack grows negatively for automatics */
#define BACKTEMP 		/* stack grows negatively for temporaries */

#undef	FIELDOPS		/* no bit-field instructions */
#define	TARGET_ENDIAN TARGET_LE	/* little-endian */

#define	CC_DIV_0	/* division by zero is safe in the compiler */

/* Definitions mostly used in pass2 */

#define BYTEOFF(x)	((x)&07)
#define wdal(k)		(BYTEOFF(k)==0)

#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)
#define genfcall(a,b)	gencall(a,b)

/* How many integer registers are needed? (used for stack allocation) */
#define	szty(t)	(t < LONG || t == FLOAT ? 1 : t == LDOUBLE ? 4 : 2)

/*
 * LLVM uses infinite virtual registers, so we define a minimal set
 * for the register allocator. All registers are given a sequential number.
 *
 * The classes used for LLVM:
 *	A - general purpose (integer/pointer)
 *	B - floating point
 */
#define	R0	000
#define	R1	001
#define	R2	002
#define	R3	003
#define	R4	004
#define	R5	005
#define	R6	006
#define	R7	007
#define	R8	010
#define	R9	011
#define	R10	012
#define	R11	013
#define	R12	014
#define	R13	015
#define	R14	016
#define	R15	017

#define	F0	020
#define	F1	021
#define	F2	022
#define	F3	023
#define	F4	024
#define	F5	025
#define	F6	026
#define	F7	027

#define	MAXREGS	030	/* 24 registers */

#define	RSTATUS	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, 	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,

/* no overlapping registers */
#define	ROVERLAP \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },

/* Return a register class based on the type of the node */
#define PCLASS(p) (p->n_type == FLOAT || p->n_type == DOUBLE || \
		   p->n_type == LDOUBLE ? SBREG : SAREG)

#define	NUMCLASS 	2	/* highest number of reg classes used */

int COLORMAP(int c, int *r);
#define	GCLASS(x) (x < 16 ? CLASSA : CLASSB)
#define DECRA(x,y)	(((x) >> (y*8)) & 255)	/* decode encoded regs */
#define	ENCRD(x)	(x)		/* Encode dest reg in n_reg */
#define ENCRA1(x)	((x) << 8)	/* A1 */
#define ENCRA2(x)	((x) << 16)	/* A2 */
#define ENCRA(x,y)	((x) << (8+y*8))	/* encode regs in int */

#define	RETREG(x)	(x == FLOAT || x == DOUBLE || x == LDOUBLE ? F0 : R0)

#define FPREG	R14	/* frame pointer */
#define STKREG	R15	/* stack pointer */

#define	TARGET_FLT_EVAL_METHOD	0	/* all as their type */

/* floating point definitions */
#define USE_IEEEFP_32
#define FLT_PREFIX      IEEEFP_32
#define USE_IEEEFP_64
#define DBL_PREFIX      IEEEFP_64
#define USE_IEEEFP_X80
#define LDBL_PREFIX     IEEEFP_X80
