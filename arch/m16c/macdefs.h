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
 * Machine-dependent defines for both passes.
 */
#define makecc(val,i)   lastcon = (lastcon<<8)|((val<<8)>>8);

#define ARGINIT		40	/* # bits above fp where arguments start */
#define AUTOINIT	8	/* # bits below fp where automatics start */

/*
 * Convert (multi-)character constant to integer.
 * Assume: If only one value; store at left side (char size), otherwise 
 * treat it as an integer.
 */

/*
 * Storage space requirements
 */
#define SZCHAR		8
#define SZINT		16
#define SZFLOAT         16
#define SZDOUBLE        16
#define SZLDOUBLE       16
#define SZLONG		32
#define SZSHORT		16
#define SZPOINT		16
#define SZLONGLONG      32

/*
 * Alignment constraints
 */
#define ALCHAR		8
#define ALINT		16
#define ALFLOAT		16
#define ALDOUBLE	16
#define ALLDOUBLE	16
#define ALLONG		16
#define ALLONGLONG	16
#define ALSHORT		16
#define ALPOINT		16
#define ALSTRUCT	16
#define ALSTACK		16

/*
 * Min/max values.
 */
#define MIN_CHAR	-128
#define MAX_CHAR	127
#define MAX_UCHAR	255
#define MIN_SHORT	-32768
#define MAX_SHORT	32767
#define MAX_USHORT	65535
#define MIN_INT		-32768
#define MAX_INT		32767
#define MAX_UNSIGNED	65535
#define MIN_LONG	-2147483648
#define MAX_LONG	2147483647
#define MAX_ULONG	4294967295UL
#define MIN_LONGLONG	-2147483648
#define MAX_LONGLONG	2147483647
#define MAX_ULONGLONG	4294967295UL

/* Default char is unsigned */
#undef	CHAR_UNSIGNED

/*
 * Use large-enough types.
 */
typedef long long CONSZ;
typedef unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"%lld"		/* format for printing constants */
#define LABFMT	"L%d"		/* format for printing labels */

#define BACKAUTO		/* stack grows negatively for automatics */
#define BACKTEMP		/* stack grows negatively for temporaries */

//#define	MYP2TREE(p) myp2tree(p);

#undef	FIELDOPS		/* no bit-field instructions */
#define RTOLBYTES		/* bytes are numbered right to left */

/* Definitions mostly used in pass2 */

#define BYTEOFF(x)	1
#define BITOOR(x)	((x)/SZCHAR)	/* bit offset to oreg offset */

#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)
#define genfcall(a,b)	gencall(a,b)

#define szty(t) ((t) == LONG ? 2 : 1)

/*
 * Register names.  These must match rnames[] and rstatus[] in local2.c.
 * The crazy order of the registers are due to the current register
 * allocations strategy and should be fixed.
 */
#define R0	0
#define R2	1
#define R1	2
#define R3	3
#define A0	4
#define A1	5
#define FB	6
#define SP	7

#define RETREG	R0	/* Return (and switch) register */
#define REGSZ	8	/* Number of registers */
#define FPREG	FB	/* frame pointer */
#define STKREG	SP	/* stack pointer */
#define MINRVAR R3	/* first register variable */
#define MAXRVAR R3	/* last register variable */

/*
 * Register types are described by bitmasks.
 */
#define	AREGS	(REGBIT(R0)|REGBIT(R1)|REGBIT(R2)|REGBIT(R3))
#define	TAREGS	(REGBIT(R0)|REGBIT(R2))
#define	BREGS	(REGBIT(A0)|REGBIT(A1))
#define	TBREGS	(REGBIT(A0))

#if 0
#define MYREADER(p) myreader(p)
#define MYCANON(p) mycanon(p)
#define MYOPTIM
#endif

#define special(a, b)	SRNOPE
