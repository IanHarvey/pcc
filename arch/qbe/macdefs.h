/*	$Id$	*/
/*
 * Copyright (c) 2025 QBE Backend Contributors
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
 * Machine-dependent defines for QBE IR backend.
 * QBE (Quick Backend) is a simple SSA-based compiler backend.
 */

/*
 * Convert (multi-)character constant to integer.
 */
#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);

#define ARGINIT		0	/* # bits above fp where arguments start */
#define AUTOINIT	0	/* # bits below fp where automatics start */

/*
 * Storage space requirements
 * QBE uses a 64-bit model similar to amd64
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
#define SZLDOUBLE	64	/* QBE doesn't support 128-bit long double */

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
#define ALLDOUBLE	64
#define ALSTACK		64
#define ALMAX		64

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
#define LABFMT	"@.L%d"		/* format for printing labels (QBE uses @) */
#define	STABLBL	"@.LL%d"	/* format for stab (debugging) labels */

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
#define	szty(t)	(t == LDOUBLE || t == DOUBLE ? 1 : \
		 t == LONGLONG || t == ULONGLONG ? 1 : 1)

/*
 * QBE uses virtual registers with unlimited count.
 * We define a simple register model with enough temporary registers.
 *
 * Register classes:
 *	A - word/long registers (w/l in QBE)
 *	B - float/double registers (s/d in QBE)
 */

/* Integer registers - QBE temps %t0 - %t30 (31 total) */
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
#define	R16	020
#define	R17	021
#define	R18	022
#define	R19	023
#define	R20	024
#define	R21	025
#define	R22	026
#define	R23	027
#define	R24	030
#define	R25	031
#define	R26	032
#define	R27	033
#define	R28	034
#define	R29	035
#define	R30	036

/* Float registers - QBE float temps %f0 - %f30 (31 total) */
#define	F0	037
#define	F1	040
#define	F2	041
#define	F3	042
#define	F4	043
#define	F5	044
#define	F6	045
#define	F7	046
#define	F8	047
#define	F9	050
#define	F10	051
#define	F11	052
#define	F12	053
#define	F13	054
#define	F14	055
#define	F15	056
#define	F16	057
#define	F17	060
#define	F18	061
#define	F19	062
#define	F20	063
#define	F21	064
#define	F22	065
#define	F23	066
#define	F24	067
#define	F25	070
#define	F26	071
#define	F27	072
#define	F28	073
#define	F29	074
#define	F30	075

#define	MAXREGS	076	/* 62 registers total (31 int + 31 float) */

/* All registers are temp registers in QBE */
#define	RSTATUS	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, 			\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,

/* no overlapping registers */
#define	ROVERLAP \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, 	\
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },

/* Return a register class based on the type of the node */
#define PCLASS(p) (p->n_type == FLOAT || p->n_type == DOUBLE || \
		   p->n_type == LDOUBLE ? SBREG : SAREG)

#define	NUMCLASS 	2	/* highest number of reg classes used */

int COLORMAP(int c, int *r);
#define	GCLASS(x) (x < 31 ? CLASSA : CLASSB)
#define DECRA(x,y)	(((x) >> (y*8)) & 255)	/* decode encoded regs */
#define	ENCRD(x)	(x)		/* Encode dest reg in n_reg */
#define ENCRA1(x)	((x) << 8)	/* A1 */
#define ENCRA2(x)	((x) << 16)	/* A2 */
#define ENCRA(x,y)	((x) << (8+y*8))	/* encode regs in int */

#define	RETREG(x)	(x == FLOAT || x == DOUBLE || x == LDOUBLE ? F0 : R0)

/* Frame and stack pointers - not really used in QBE but required */
#define FPREG	R30	/* frame pointer */
#define STKREG	R31	/* stack pointer */

/*
 * QBE type suffixes for instructions
 */
#define QBE_BYTE	"b"
#define QBE_HALF	"h"
#define QBE_WORD	"w"
#define QBE_LONG	"l"
#define QBE_SINGLE	"s"
#define QBE_DOUBLE	"d"

/* QBE doesn't have special specials like other backends */
