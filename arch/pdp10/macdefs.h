/*	macdefs.h	4.6	88/05/31	*/

#ifndef _MACDEFS_
#define	_MACDEFS_

/*
 * Convert (multi-)character constant to integer.
 */
#define makecc(val,i)	lastcon |= (val << (27 - (i * 9)))

#define ARGINIT		36
#define AUTOINIT	36	/* # words above fp where automatics start */

/*
 * Storage space requirements
 */
#define SZCHAR		9
#define SZINT		36
#define SZFLOAT		36
#define SZDOUBLE	72
#define SZLONG		36
#define SZSHORT		18
#define SZPOINT		36
#define SZLONGLONG	72

/*
 * Alignment constraints
 */
#define ALCHAR		9
#define ALINT		36
#define ALFLOAT		36
#define ALDOUBLE	36
#define ALLONG		36
#define ALLONGLONG	36
#define ALSHORT		18
#define ALPOINT		36
#define ALSTRUCT	36
#define ALSTACK		36 

#ifdef __pdp10__
/*
 * Native compilation.
 */
typedef long CONSZ;		/* size in which constants are converted */
typedef	unsigned long	U_CONSZ;/* unsigned version of the above */
typedef	long	OFFSZ;		/* size in which offsets are kept */
#else
/*
 * Cross compilation, use large-enough types.
 */
typedef	int64_t CONSZ;
typedef	u_int64_t U_CONSZ;
typedef int64_t OFFSZ;
#endif

#ifdef __pdp10__
#define CONFMT	"%lo"		/* format for printing constants */
#else
#define CONFMT	"%llo"		/* format for printing constants */
#endif
#define LABFMT	"L%d"		/* format for printing labels */

#if 0
#define CCTRANS(x) x		/* character set macro */

/*
 * Register cookies for stack pointer and argument pointer
 */
#endif
#define STKREG	016		/* stack pointer */

/*
 * Maximum and minimum register variables
 */
#define MINRVAR	010		/* use 10 thru ... */
#define MAXRVAR	015		/* ... 15 */

#if 0
#define BACKAUTO		/* stack grows negatively for automatics */
#define BACKTEMP		/* stack grows negatively for temporaries */
#define FIELDOPS		/* show field hardware support on VAX */
#define RTOLBYTES		/* bytes are numbered from right to left */
#define ADDROREG		/* can unwind &o, where o is OREG */

#define ASSTRINGS		/* assembler handles string initializations */
#define STABDOT			/* assembler understands .stabd */
#define LCOMM			/* assembler supports .lcomm */

#define ENUMSIZE(high,low) INT	/* enums are always stored in full int */

#define FIXDEF(p) if (!nerrors) outstab(p); else
#define FIXARG(p) if (!nerrors) fixarg(p); else
#define FIXSTRUCT(p,q) if (!nerrors) outstruct(p,q); else

#ifndef ncopy
#define	ncopy(q, p)	memcpy(q, p, sizeof(*p))
#endif
#endif

#define aobeg()
#define aocode(p)
#define aoend()

#if 0
#define	PRTDCON			/* use machine-specific prtdcon routine */
#endif
#endif
