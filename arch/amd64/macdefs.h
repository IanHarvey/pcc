/*	$Id$	*/
/*
 * Copyright (c) 2008 Michael Shalayeff
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
 */
#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);

#define ARGINIT		64	/* # bits above fp where arguments start */
#define AUTOINIT	0	/* # bits below fp where automatics start */

/*
 * Storage space requirements
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
#define ALSTRUCT	128
#define ALSTACK		64 

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
#define	MAX_UNSIGNED	0xffffffff
#define	MIN_LONG	MIN_LONGLONG
#define	MAX_LONG	MAX_LONGLONG
#define	MAX_ULONG	MAX_ULONGLONG
#define	MIN_LONGLONG	0x8000000000000000LL
#define	MAX_LONGLONG	0x7fffffffffffffffLL
#define	MAX_ULONGLONG	0xffffffffffffffffULL

/* Default char is signed */
#undef	CHAR_UNSIGNED
#define	BOOL_TYPE	CHAR	/* what used to store _Bool */
#if defined(os_mirbsd) || defined(os_win32)
#define WCHAR_TYPE	USHORT	/* ISO 10646 16-bit Unicode */
#else
#define	WCHAR_TYPE	INT	/* what used to store wchar_t */
#endif

/*
 * Use large-enough types.
 */
typedef	long long CONSZ;
typedef	unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"%lld"		/* format for printing constants */
#define LABFMT	".L%d"		/* format for printing labels */
#define	STABLBL	".LL%d"		/* format for stab (debugging) labels */
#ifdef LANG_F77
#define BLANKCOMMON "_BLNK_"
#define MSKIREG  (M(TYSHORT)|M(TYLONG))
#define TYIREG TYLONG
#define FSZLENG  FSZLONG
#define	AUTOREG	EBP
#define	ARGREG	EBP
#define ARGOFFSET 8
#endif

#define BACKAUTO 		/* stack grows negatively for automatics */
#define BACKTEMP 		/* stack grows negatively for temporaries */

#undef	FIELDOPS		/* no bit-field instructions */
#define	RTOLBYTES		/* bytes are numbered right to left */

#define ENUMSIZE(high,low) INT	/* enums are always stored in full int */

#define FINDMOPS	/* i386 has instructions that modifies memory */

/* Definitions mostly used in pass2 */

#define BYTEOFF(x)	((x)&03)
#define wdal(k)		(BYTEOFF(k)==0)
#define BITOOR(x)	(x)	/* bit offset to oreg offset XXX die! */

#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)
#define genfcall(a,b)	gencall(a,b)

#define	szty(t)	(((t) == DOUBLE || (t) == FLOAT || (t) == LONG || \
	(t) == ULONG || (t) == LONGLONG || (t) == ULONGLONG) ? 2 : \
	(t) == LDOUBLE ? 4 : 1)

/*
 * The x86 has a bunch of register classes, most of them interfering
 * with each other.  All registers are given a sequential number to
 * identify it which must match rnames[] in local2.c.
 * Class membership and overlaps are defined in the macros RSTATUS
 * and ROVERLAP below.
 *
 * The classes used on x86 are:
 *	A - short and int regs
 *	B - char regs
 *	C - long long regs
 *	D - floating point
 */
#define	EAX	000	/* Scratch and return register */
#define	EDX	001	/* Scratch and secondary return register */
#define	ECX	002	/* Scratch (and shift count) register */
#define	EBX	003	/* GDT pointer or callee-saved temporary register */
#define	ESI	004	/* Callee-saved temporary register */
#define	EDI	005	/* Callee-saved temporary register */
#define	EBP	006	/* Frame pointer */
#define	ESP	007	/* Stack pointer */

#define	AL	010
#define	AH	011
#define	DL	012
#define	DH	013
#define	CL	014
#define	CH	015
#define	BL	016
#define	BH	017

#define	RAX	020
#define	RDX	021
#define	RCX	022
#define	RBX	023
#define	RSI	024
#define	RDI	025
#define	RBP	026
#define	RSP	027
#define	R08	030
#define	R09	031
#define	R10	032
#define	R11	033
#define	R12	034
#define	R13	035
#define	R14	036
#define	R15	037

/* The 8 math registers in class D lacks names */

#define	MAXREGS	050	/* 40 registers */

#define	RSTATUS	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|PERMREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, 0, 0,	 			\
	SBREG, SBREG, SBREG, SBREG, SBREG, SBREG, SBREG, SBREG,		\
	SCREG, SCREG, SCREG, SCREG, SCREG, SCREG, 0, 0,			\
	SCREG|TEMPREG, SCREG|TEMPREG, SCREG|TEMPREG, SCREG|TEMPREG,	\
	SCREG|PERMREG, SCREG|PERMREG, SCREG|PERMREG, SCREG|PERMREG, 	\
	SDREG, SDREG, SDREG, SDREG,  SDREG, SDREG, SDREG, SDREG,

#define	ROVERLAP \
	/* 8 basic registers */\
	{ AL, AH, RAX, -1 },\
	{ DL, DH, RDX, -1 },\
	{ CL, CH, RCX, -1 },\
	{ BL, BH, RBX, -1 },\
	{ RSI, -1 },\
	{ RDI, -1 },\
	{ RBP, -1 },\
	{ RSP, -1 },\
\
	/* 8 char registers */\
	{ EAX, RAX, -1 },\
	{ EAX, RAX, -1 },\
	{ EDX, RDX, -1 },\
	{ EDX, RDX, -1 },\
	{ ECX, RCX, -1 },\
	{ ECX, RCX, -1 },\
	{ EBX, RBX, -1 },\
	{ EBX, RBX, -1 },\
\
	/* 16 long-long-emulating registers */\
	{ EAX, AL, AH, -1 },\
	{ EDX, DL, DH, -1 },\
	{ ECX, CL, CH, -1 },\
	{ EBX, BL, BH, -1 },\
	{ ESI, -1 },\
	{ EDI, -1 },\
	{ EBP, -1 },\
	{ ESP, -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
\
	/* The fp registers do not overlap with anything */\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },\
	{ -1 },


/* Return a register class based on the type of the node */
#define PCLASS(p) (p->n_type <= UCHAR ? SBREG : \
		  (p->n_type == LONG || p->n_type == ULONG || \
		   p->n_type == LONGLONG || p->n_type == ULONGLONG ? SCREG : \
		  (p->n_type >= FLOAT && p->n_type <= LDOUBLE ? SDREG : SAREG)))

#define	NUMCLASS 	4	/* highest number of reg classes used */

int COLORMAP(int c, int *r);
#define	GCLASS(x) (x < 8 ? CLASSA : x < 16 ? CLASSB : x < 32 ? CLASSC : CLASSD)
#define DECRA(x,y)	(((x) >> (y*8)) & 255)	/* decode encoded regs */
#define	ENCRD(x)	(x)		/* Encode dest reg in n_reg */
#define ENCRA1(x)	((x) << 8)	/* A1 */
#define ENCRA2(x)	((x) << 16)	/* A2 */
#define ENCRA(x,y)	((x) << (8+y*8))	/* encode regs in int */
/* XXX - return char in al? */
#define	RETREG(x)	(x == CHAR || x == UCHAR ? AL : \
			 x == LONG || x == ULONG || \
			 x == LONGLONG || x == ULONGLONG ? RAX : \
			 x == FLOAT || x == DOUBLE || x == LDOUBLE ? 32 : EAX)

//#define R2REGS	1	/* permit double indexing */

/* XXX - to die */
#define FPREG	RBP	/* frame pointer */
#define STKREG	RSP	/* stack pointer */

#define	SHSTR		(MAXSPECIAL+1)	/* short struct */
#define	SFUNCALL	(MAXSPECIAL+2)	/* struct assign after function call */
#define	SPCON		(MAXSPECIAL+3)	/* positive nonnamed constant */

/*
 * Specials that indicate the applicability of machine idioms.
 */
#define SMIXOR		(MAXSPECIAL+4)
#define SMILWXOR	(MAXSPECIAL+5)
#define SMIHWXOR	(MAXSPECIAL+6)

/*
 * i386-specific symbol table flags.
 */
#define	SSECTION	SLOCAL1
#define	STLS		SLOCAL2
#define	SNOUNDERSCORE	SLOCAL3
#define SSTDCALL	SLOCAL2	
#define SDLLINDIRECT	SLOCAL3

/*
 * i386-specific node flags.
 */
#define FSTDCALL	0x01

/*
 * i386-specific interpass stuff.
 */

#define TARGET_IPP_MEMBERS			\
	int ipp_argstacksize;

/*
 * Extended assembler macros.
 */
void targarg(char *w, void *arg);
#define	XASM_TARGARG(w, ary)	\
	(w[1] == 'b' || w[1] == 'h' || w[1] == 'w' || w[1] == 'k' ? \
	w++, targarg(w, ary), 1 : 0)
int numconv(void *ip, void *p, void *q);
#define	XASM_NUMCONV(ip, p, q)	numconv(ip, p, q)

/*
 * builtins.
 */
#define TARGET_BUILTINS							\
	{ "__builtin_frame_address", i386_builtin_frame_address },	\
	{ "__builtin_return_address", i386_builtin_return_address },

#define NODE struct node
struct node;
NODE *i386_builtin_frame_address(NODE *f, NODE *a);
NODE *i386_builtin_return_address(NODE *f, NODE *a);
#undef NODE
