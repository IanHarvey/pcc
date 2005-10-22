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

/*
 * Convert (multi-)character constant to integer.
 * Assume: If only one value; store at left side (char size), otherwise 
 * treat it as an integer.
 */
#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);

#define ARGINIT		64	/* # bits above fp where arguments start */
#define AUTOINIT	0	/* # bits below fp where automatics start */

/*
 * Storage space requirements
 */
#define SZCHAR		8
#define SZINT		32
#define SZFLOAT		32
#define SZDOUBLE	64
#define SZLDOUBLE	96
#define SZLONG		32
#define SZSHORT		16
#define SZLONGLONG	64
#define SZPOINT(t)	32

/*
 * Alignment constraints
 */
#define ALCHAR		8
#define ALINT		32
#define ALFLOAT		32
#define ALDOUBLE	32
#define ALLDOUBLE	32
#define ALLONG		32
#define ALLONGLONG	32
#define ALSHORT		16
#define ALPOINT		32
#define ALSTRUCT	32
#define ALSTACK		32 

/*
 * Min/max values.
 */
#define	MIN_CHAR	-128
#define	MAX_CHAR	127
#define	MAX_UCHAR	255
#define	MIN_SHORT	-32768
#define	MAX_SHORT	32767
#define	MAX_USHORT	65535
#define	MIN_INT		-1
#define	MAX_INT		0x7fffffff
#define	MAX_UNSIGNED	0xffffffff
#define	MIN_LONG	MIN_INT
#define	MAX_LONG	MAX_INT
#define	MAX_ULONG	MAX_UNSIGNED
#define	MIN_LONGLONG	0x8000000000000000LL
#define	MAX_LONGLONG	0x7fffffffffffffffLL
#define	MAX_ULONGLONG	0xffffffffffffffffULL

/* Default char is unsigned */
#undef	CHAR_UNSIGNED

/*
 * Use large-enough types.
 */
typedef	long long CONSZ;
typedef	unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"%lld"		/* format for printing constants */
#define LABFMT	".L%d"		/* format for printing labels */
#define	STABLBL	".LL%d"		/* format for stab (debugging) labels */
#ifdef FORTRAN
#define XL 8
#define	FLABELFMT "%s:\n"
#define USETEXT ".text"
#define USECONST ".data\t0" 	/* XXX - fix */
#define USEBSS  ".data\t1" 	/* XXX - fix */
#define USEINIT ".data\t2" 	/* XXX - fix */
#define MAXREGVAR 3             /* XXX - fix */
#define BLANKCOMMON "_BLNK_"
#define MSKIREG  (M(TYSHORT)|M(TYLONG))
#define TYIREG TYLONG
#define FSZLENG  FSZLONG
#define FUDGEOFFSET 1
#define	AUTOREG	EBP
#define	ARGREG	EBP
#define ARGOFFSET 4
#endif

#define BACKAUTO 		/* stack grows negatively for automatics */
#define BACKTEMP 		/* stack grows negatively for temporaries */

#define	MYP2TREE(p) myp2tree(p);

#undef	FIELDOPS		/* no bit-field instructions */
#define	RTOLBYTES		/* bytes are numbered right to left */

#define ENUMSIZE(high,low) INT	/* enums are always stored in full int */

/* Definitions mostly used in pass2 */

#define BYTEOFF(x)	((x)&03)
#define wdal(k)		(BYTEOFF(k)==0)
#define BITOOR(x)	((x)/SZCHAR)	/* bit offset to oreg offset */

#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)
#define genfcall(a,b)	gencall(a,b)

#define	szty(t)	(((t) == DOUBLE || (t) == FLOAT || \
	(t) == LONGLONG || (t) == ULONGLONG) ? 2 : 1)

#ifdef MULTICLASS
/*
 * The x86 has a bunch of register classes, most of them interfering
 * with each other.
 * Each class contains a number of registers, represented by bits in
 * a bitmask.
 * The classes used on x86 are:
 *	A - short and int regs
 *	B - char regs
 *	C - long long regs
 *	D - floating point
 */
#define	EAX	0	/* Scratch and return register */
#define	EDX	1	/* Scratch and secondary return register */
#define	ECX	2	/* Scratch (and shift count) register */
#define	EBX	3	/* GDT pointer or callee-saved temporary register */
#define	ESI	4	/* Callee-saved temporary register */
#define	EDI	5	/* Callee-saved temporary register */
#define	EBP	6	/* Frame pointer */
#define	ESP	7	/* Stack pointer */
#define AREGS   (REGBIT(EAX)|REGBIT(EDX)|REGBIT(ECX)|REGBIT(ESI)| \
	REGBIT(EDI)|REGBIT(EBX))
#define	TAREGS	(REGBIT(EAX)|REGBIT(EDX)|REGBIT(ECX))
#define	NUMAREG	6

#define	AL	0
#define	AH	1
#define	DL	2
#define	DH	3
#define	CL	4
#define	CH	5
#define	BL	6
#define	BH	7
#define	BREGS	(REGBIT(AL)|REGBIT(AH)|REGBIT(DL)|REGBIT(DH)| \
	REGBIT(CL)|REGBIT(CH)|REGBIT(BL)|REGBIT(BH))
#define	TBREGS	(REGBIT(AL)|REGBIT(AH)|REGBIT(DL)|REGBIT(DH)| \
	REGBIT(CL)|REGBIT(CH))
#define	NUMBREG	8

#define	EAXEDX	0
#define	EAXECX	1
#define	EAXEBX	2
#define	EAXESI	3
#define	EAXEDI	4
#define	EDXECX	5
#define	EDXEBX	6
#define	EDXESI	7
#define	EDXEDI	8
#define	ECXEBX	9
#define	ECXESI	10
#define	ECXEDI	11
#define	EBXESI	12
#define	EBXEDI	13
#define	ESIEDI	14
#define	CREGS	(REGBIT(EAXEDX)|REGBIT(EAXECX)|REGBIT(EAXEBX)|REGBIT(EAXESI)| \
	REGBIT(EAXEDI)|REGBIT(EDXECX)|REGBIT(EDXEBX)|REGBIT(EDXESI)| \
	REGBIT(EDXEDI)|REGBIT(ECXEBX)|REGBIT(ECXESI)|REGBIT(ECXEDI)| \
	REGBIT(EBXESI)|REGBIT(EBXEDI)|REGBIT(ESIEDI))
#define	TCREGS	(REGBIT(EAXEDX)|REGBIT(EAXECX)|REGBIT(EAXEBX)|REGBIT(EAXESI)| \
	REGBIT(EAXEDI)|REGBIT(EDXECX)|REGBIT(EDXEBX)|REGBIT(EDXESI)| \
	REGBIT(EDXEDI)|REGBIT(ECXEBX)|REGBIT(ECXESI)|REGBIT(ECXEDI))
#define	NUMCREG	15

#define	DREGS	0xff	/* float regs (currently not used) */
#define	TDREGS	0
#define	NUMDREG	8

#define	PCLASS(p) (p->n_type <= UCHAR ? SBREG : \
		  (p->n_type == LONGLONG || p->n_type == ULONGLONG ? SCREG : \
		  (p->n_type >= FLOAT && p->n_type <= LDOUBLE ? SDREG : SAREG)))

#define	NUMCLASS 	4	/* highest number of reg classes used */
#define	MAXREGNUM	16	/* first non-precolored register */

extern char colormap[NUMCLASS][NUMAREG][NUMBREG][NUMCREG][NUMDREG];
#define COLORMAP(i, r) colormap[i][r[0]][r[1]][r[2]][r[3]]
int type2class(int); /* XXX */

/* XXX - to die */
#define MINRVAR	ESI	/* first register variable */
#define MAXRVAR	EBX	/* last register variable */
#define REGSZ	16	/* 8 "general" and 8 floating point regs */
#define FPREG	EBP	/* frame pointer */
#define STKREG	ESP	/* stack pointer */
#define	RETREG	EAX	/* Return (and switch) register */
#define	NREGREG	(MAXRVAR-MINRVAR+1)
#else
/*
 * Register names.  These must match rnames[] and rstatus[] in local2.c.
 * The crazy order of the registers are due to the current register
 * allocations strategy and should be fixed.
 */
#define	EAX	0	/* Scratch and return register */
#define	EDX	1	/* Scratch and secondary return register */
#define	ECX	2	/* Scratch (and shift count) register */
#define	ESI	3	/* Callee-saved temporary register */
#define	EDI	4	/* Callee-saved temporary register */
#define	EBX	5	/* GDT pointer or callee-saved temporary register */
#define	EBP	6	/* Frame pointer */
#define	ESP	7	/* Stack pointer */

#define	RETREG	EAX	/* Return (and switch) register */
#define REGSZ	16	/* 8 "general" and 8 floating point regs */
#define FPREG	EBP	/* frame pointer */
#define STKREG	ESP	/* stack pointer */
#define MINRVAR	ESI	/* first register variable */
#define MAXRVAR	EBX	/* last register variable */

#define	NREGREG	(MAXRVAR-MINRVAR+1)
/*
 * Register types are described by bitmasks.
 */
#define AREGS   (REGBIT(EAX)|REGBIT(EDX)|REGBIT(ECX)|REGBIT(ESI)| \
	REGBIT(EDI)|REGBIT(EBX))
#define TAREGS  (REGBIT(EAX)|REGBIT(EDX)|REGBIT(ECX))
#define	BREGS	0xff00	/* 8-15 are just floating point regs */
#define	BREGS_STACK 8	/* bregs is a register stack */
#if 0
#define	TBREGS	BREGS	/* 8-15 are just floating point regs */
#else
#define TBREGS	0
#endif
#endif /* MULTICLASS */

#define	MYADDEDGE(x, t) if (t < INT) { AddEdge(x, ESI); AddEdge(x, EDI); }
#define MYREADER(p) myreader(p)
#define MYCANON(p) mycanon(p)
#define	MYOPTIM

#define special(a, b)	SRNOPE
