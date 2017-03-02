/*	$Id$	*/

#ifndef NATIVE_FLOATING_POINT
#define STRTODG_FOR_PCC
#ifdef STRTODG_FOR_PCC
/*
 * This is a derivation of (part of) the package gdtoa published by
 * David M. Gay and generally available as gdtoa.tgz at
 * http://www.netlib.org/fp/
 * This package is a generalization of the strtod routine described in
 * 	David M. Gay, "Correctly Rounded Binary-Decimal and
 *	Decimal-Binary Conversions", Numerical Analysis Manuscript
 *	No. 90-10, Bell Labs, Murray Hill, 1990;
 *	http://cm.bell-labs.com/cm/cs/what/ampl/REFS/rounding.ps.gz
 *
 * We used only the decimal->binary "generic" conversion, which is
 * exposed through the strtodg() function, along with all the support
 * routines it needs. We have concatenated the files
 *	gdtoaimp.h
 *	misc.c
 *	gmisc.c
 *	smisc.c
 *	sum.c
 *	hd_init.c
 *	gethex.c
 *	strtodg.c
 * removing the duplicated notices (first 32 lines.)
 * Then we have introduced small changes, mostly to silence compiler
 * warnings or to disable features we do not use.
 * Our functional changes are surrounded with STRTODG_FOR_PCC.
 * We stick to the naming and style conventions used in the original
 * package, to improve the "diffability" with it.
 * The reference version could thus be retrieved with
sed -e "32q" gdtoaimp.h > strtodg.ref
for f in gdtoaimp.h misc.c gmisc.c smisc.c sum.c hd_init.c gethex.c strtodg.c
do sed -e "1,32d" $f >>strtodg.ref
done
 *
 * Notes:
 * . An important distinction to do is between the target architecture,
 * which is generally described through the FPI structure; and the host
 * double floating-point, which is highly relied upon; variances
 * between the host implementations (IEEE or VAX, little- or big-endian)
 * should be detected at compile time, and are discrimated through the
 * source with the use of the macros IEEE_xxx or VAX (or IBM, untested.)
 *
 * Known issue: some compilers are reporting warnings about "shadowing
 * variables", because of the use of 'exp' as a variable name.
 */

/* Tailoring for PCC */
#define NO_INFNAN_CHECK
#undef NO_HEX_FP
#define No_Hex_NaN
#define NO_ERRNO
#undef USE_LOCALE

#ifndef __MATH_H__
#define __MATH_H__ /* skip unnecessary math.h, and avoid troubles */
#endif

#include "config.h"
#if defined(IEEE_8087) + defined(IEEE_MC68k) + defined(VAX) + defined(IBM) == 0
/* Try to guess the host floating-point implementation */
# if defined(vax) || defined(__vax__)
#define VAX
# elif defined(HOST_LITTLE_ENDIAN)
#define IEEE_8087
# elif defined(HOST_BIG_ENDIAN)
#define IEEE_MC68k
# endif
#endif
#endif
/* gdtoaimp.h */
/****************************************************************

The author of this software is David M. Gay.

Copyright (C) 1998-2000 by Lucent Technologies
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of Lucent or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.

****************************************************************/

/* This is a variation on dtoa.c that converts arbitary binary
   floating-point formats to and from decimal notation.  It uses
   double-precision arithmetic internally, so there are still
   various #ifdefs that adapt the calculations to the native
   double-precision arithmetic (any of IEEE, VAX D_floating,
   or IBM mainframe arithmetic).

   Please send bug reports to David M. Gay (dmg at acm dot org,
   with " at " changed at "@" and " dot " changed to ".").
 */

#ifdef STRTODG_FOR_PCC
/* (Irrelevant comment removed, to avoid being misled.) */
#endif

/* strtod for IEEE-, VAX-, and IBM-arithmetic machines.
 *
 * This strtod returns a nearest machine number to the input decimal
 * string (or sets errno to ERANGE).  With IEEE arithmetic, ties are
 * broken by the IEEE round-even rule.  Otherwise ties are broken by
 * biased rounding (add half and chop).
 *
 * Inspired loosely by William D. Clinger's paper "How to Read Floating
 * Point Numbers Accurately" [Proc. ACM SIGPLAN '90, pp. 112-126].
 *
 * Modifications:
 *
 *	1. We only require IEEE, IBM, or VAX double-precision
 *		arithmetic (not IEEE double-extended).
 *	2. We get by with floating-point arithmetic in a case that
 *		Clinger missed -- when we're computing d * 10^n
 *		for a small integer d and the integer n is not too
 *		much larger than 22 (the maximum integer k for which
 *		we can represent 10^k exactly), we may be able to
 *		compute (d*10^k) * 10^(e-k) with just one roundoff.
 *	3. Rather than a bit-at-a-time adjustment of the binary
 *		result in the hard case, we use floating-point
 *		arithmetic to determine the adjustment to within
 *		one bit; only in really hard cases do we need to
 *		compute a second residual.
 *	4. Because of 3., we don't need a large table of powers of 10
 *		for ten-to-e (just some small tables, e.g. of 10^k
 *		for 0 <= k <= 22).
 */

/*
 * #define IEEE_8087 for IEEE-arithmetic machines where the least
 *	significant byte has the lowest address.
 * #define IEEE_MC68k for IEEE-arithmetic machines where the most
 *	significant byte has the lowest address.
 * #define Long int on machines with 32-bit ints and 64-bit longs.
 * #define Sudden_Underflow for IEEE-format machines without gradual
 *	underflow (i.e., that flush to zero on underflow).
 * #define IBM for IBM mainframe-style floating-point arithmetic.
 * #define VAX for VAX-style floating-point arithmetic (D_floating).
 * #define No_leftright to omit left-right logic in fast floating-point
 *	computation of dtoa and gdtoa.  This will cause modes 4 and 5 to be
 *	treated the same as modes 2 and 3 for some inputs.
 * #define Check_FLT_ROUNDS if FLT_ROUNDS can assume the values 2 or 3.
 * #define RND_PRODQUOT to use rnd_prod and rnd_quot (assembly routines
 *	that use extended-precision instructions to compute rounded
 *	products and quotients) with IBM.
 * #define ROUND_BIASED for IEEE-format with biased rounding and arithmetic
 *	that rounds toward +Infinity.
 * #define ROUND_BIASED_without_Round_Up for IEEE-format with biased
 *	rounding when the underlying floating-point arithmetic uses
 *	unbiased rounding.  This prevent using ordinary floating-point
 *	arithmetic when the result could be computed with one rounding error.
 * #define Inaccurate_Divide for IEEE-format with correctly rounded
 *	products but inaccurate quotients, e.g., for Intel i860.
 * #define NO_LONG_LONG on machines that do not have a "long long"
 *	integer type (of >= 64 bits).  On such machines, you can
 *	#define Just_16 to store 16 bits per 32-bit Long when doing
 *	high-precision integer arithmetic.  Whether this speeds things
 *	up or slows things down depends on the machine and the number
 *	being converted.  If long long is available and the name is
 *	something other than "long long", #define Llong to be the name,
 *	and if "unsigned Llong" does not work as an unsigned version of
 *	Llong, #define #ULLong to be the corresponding unsigned type.
 * #define KR_headers for old-style C function headers.
 * #define Bad_float_h if your system lacks a float.h or if it does not
 *	define some or all of DBL_DIG, DBL_MAX_10_EXP, DBL_MAX_EXP,
 *	FLT_RADIX, FLT_ROUNDS, and DBL_MAX.
 * #define MALLOC your_malloc, where your_malloc(n) acts like malloc(n)
 *	if memory is available and otherwise does something you deem
 *	appropriate.  If MALLOC is undefined, malloc will be invoked
 *	directly -- and assumed always to succeed.  Similarly, if you
 *	want something other than the system's free() to be called to
 *	recycle memory acquired from MALLOC, #define FREE to be the
 *	name of the alternate routine.  (FREE or free is only called in
 *	pathological cases, e.g., in a gdtoa call after a gdtoa return in
 *	mode 3 with thousands of digits requested.)
 * #define Omit_Private_Memory to omit logic (added Jan. 1998) for making
 *	memory allocations from a private pool of memory when possible.
 *	When used, the private pool is PRIVATE_MEM bytes long:  2304 bytes,
 *	unless #defined to be a different length.  This default length
 *	suffices to get rid of MALLOC calls except for unusual cases,
 *	such as decimal-to-binary conversion of a very long string of
 *	digits.  When converting IEEE double precision values, the
 *	longest string gdtoa can return is about 751 bytes long.  For
 *	conversions by strtod of strings of 800 digits and all gdtoa
 *	conversions of IEEE doubles in single-threaded executions with
 *	8-byte pointers, PRIVATE_MEM >= 7400 appears to suffice; with
 *	4-byte pointers, PRIVATE_MEM >= 7112 appears adequate.
 * #define NO_INFNAN_CHECK if you do not wish to have INFNAN_CHECK
 *	#defined automatically on IEEE systems.  On such systems,
 *	when INFNAN_CHECK is #defined, strtod checks
 *	for Infinity and NaN (case insensitively).
 *	When INFNAN_CHECK is #defined and No_Hex_NaN is not #defined,
 *	strtodg also accepts (case insensitively) strings of the form
 *	NaN(x), where x is a string of hexadecimal digits (optionally
 *	preceded by 0x or 0X) and spaces; if there is only one string
 *	of hexadecimal digits, it is taken for the fraction bits of the
 *	resulting NaN; if there are two or more strings of hexadecimal
 *	digits, each string is assigned to the next available sequence
 *	of 32-bit words of fractions bits (starting with the most
 *	significant), right-aligned in each sequence.
 *	Unless GDTOA_NON_PEDANTIC_NANCHECK is #defined, input "NaN(...)"
 *	is consumed even when ... has the wrong form (in which case the
 *	"(...)" is consumed but ignored).
 * #define MULTIPLE_THREADS if the system offers preemptively scheduled
 *	multiple threads.  In this case, you must provide (or suitably
 *	#define) two locks, acquired by ACQUIRE_DTOA_LOCK(n) and freed
 *	by FREE_DTOA_LOCK(n) for n = 0 or 1.  (The second lock, accessed
 *	in pow5mult, ensures lazy evaluation of only one copy of high
 *	powers of 5; omitting this lock would introduce a small
 *	probability of wasting memory, but would otherwise be harmless.)
 *	You must also invoke freedtoa(s) to free the value s returned by
 *	dtoa.  You may do so whether or not MULTIPLE_THREADS is #defined.
 * #define IMPRECISE_INEXACT if you do not care about the setting of
 *	the STRTOG_Inexact bits in the special case of doing IEEE double
 *	precision conversions (which could also be done by the strtod in
 *	dtoa.c).
 * #define NO_HEX_FP to disable recognition of C9x's hexadecimal
 *	floating-point constants.
 * #define -DNO_ERRNO to suppress setting errno (in strtod.c and
 *	strtodg.c).
 * #define NO_STRING_H to use private versions of memcpy.
 *	On some K&R systems, it may also be necessary to
 *	#define DECLARE_SIZE_T in this case.
 * #define USE_LOCALE to use the current locale's decimal_point value.
 */

#ifndef GDTOAIMP_H_INCLUDED
#define GDTOAIMP_H_INCLUDED
#ifndef STRTODG_FOR_PCC
#include "gdtoa.h"
#include "gd_qnan.h"
#else
/* The public part has been moved to PCC headers */
#include "pass1.h"

#undef FREE	/* Do not mix apples with oranges... */
#define MALLOC(sz) tmpalloc(sz)
#define FREE(p) /*nothing to do*/

/* Just provide the "ANSI C" wrappers, to improve "diffability" */
#define ANSI(x) x
#define Void void
#define CONST const

#ifndef Long
#define Long int /* Better support than int32_t or int_fast32_t. */
#endif
#ifndef ULong
typedef unsigned Long ULong;
#endif
#ifndef UShort
typedef unsigned short UShort;
#endif

enum {	/* return values from strtodg */
	STRTOG_Zero	= SF_Zero,	/* required to be =0 */
	STRTOG_Normal	= SF_Normal,
	STRTOG_Denormal	= SF_Denormal,
	STRTOG_Infinite	= SF_Infinite,
	STRTOG_NaN	= SF_NaN,
	STRTOG_NaNbits	= SF_NaNbits,
	STRTOG_NoNumber	= SF_NoNumber,
	STRTOG_Retmask	= SF_kmask,
	/* The following may be or-ed into one of the above values. */
	STRTOG_Neg	= SF_Neg, /* does not affect STRTOG_Inexlo or STRTOG_Inexhi */
	STRTOG_Inexlo	= SFEXCP_Inexlo, /* returned result rounded toward zero */
	STRTOG_Inexhi	= SFEXCP_Inexhi, /* returned result rounded away from zero */
	STRTOG_Inexact	= SFEXCP_Inexact,
	STRTOG_Underflow= SFEXCP_Underflow,
	STRTOG_Overflow	= SFEXCP_Overflow
};
int strtodg (const char*, char**, FPI*, Long*, ULong*);
#endif /* STRTODG_FOR_PCC */
#ifdef Honor_FLT_ROUNDS
#include <fenv.h>
#endif

#ifdef DEBUG
#include "stdio.h"
#define Bug(x) {fprintf(stderr, "%s\n", x); exit(1);}
#endif

#include "stdlib.h"
#include "string.h"

#ifdef KR_headers
#define Char char
#else
#define Char void
#endif

#ifdef MALLOC
extern Char *MALLOC ANSI((size_t));
#else
#define MALLOC malloc
#endif

#undef IEEE_Arith
#undef Avoid_Underflow
#ifdef IEEE_MC68k
#define IEEE_Arith
#endif
#ifdef IEEE_8087
#define IEEE_Arith
#endif

#include "errno.h"
#ifdef Bad_float_h

#ifdef IEEE_Arith
#define DBL_DIG 15
#define DBL_MAX_10_EXP 308
#define DBL_MAX_EXP 1024
#define FLT_RADIX 2
#define DBL_MAX 1.7976931348623157e+308
#endif

#ifdef IBM
#define DBL_DIG 16
#define DBL_MAX_10_EXP 75
#define DBL_MAX_EXP 63
#define FLT_RADIX 16
#define DBL_MAX 7.2370055773322621e+75
#endif

#ifdef VAX
#define DBL_DIG 16
#define DBL_MAX_10_EXP 38
#define DBL_MAX_EXP 127
#define FLT_RADIX 2
#define DBL_MAX 1.7014118346046923e+38
#define n_bigtens 2
#endif

#ifndef LONG_MAX
#define LONG_MAX 2147483647
#endif

#else /* ifndef Bad_float_h */
#include "float.h"
#endif /* Bad_float_h */

#ifdef IEEE_Arith
#define Scale_Bit 0x10
#define n_bigtens 5
#endif

#ifdef IBM
#define n_bigtens 3
#endif

#ifdef VAX
#define n_bigtens 2
#endif

#ifndef __MATH_H__
#include "math.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(IEEE_8087) + defined(IEEE_MC68k) + defined(VAX) + defined(IBM) != 1
Exactly one of IEEE_8087, IEEE_MC68k, VAX, or IBM should be defined.
#endif

typedef union { double d; ULong L[2]; } U;

#ifdef IEEE_8087
#define word0(x) (x)->L[1]
#define word1(x) (x)->L[0]
#else
#define word0(x) (x)->L[0]
#define word1(x) (x)->L[1]
#endif
#define dval(x) (x)->d

/* The following definition of Storeinc is appropriate for MIPS processors.
 * An alternative that might be better on some machines is
 * #define Storeinc(a,b,c) (*a++ = b << 16 | c & 0xffff)
 */
#if defined(IEEE_8087) + defined(VAX)
#define Storeinc(a,b,c) (((unsigned short *)a)[1] = (unsigned short)b, \
((unsigned short *)a)[0] = (unsigned short)c, a++)
#else
#define Storeinc(a,b,c) (((unsigned short *)a)[0] = (unsigned short)b, \
((unsigned short *)a)[1] = (unsigned short)c, a++)
#endif

/* #define P DBL_MANT_DIG */
/* Ten_pmax = floor(P*log(2)/log(5)) */
/* Bletch = (highest power of 2 < DBL_MAX_10_EXP) / 16 */
/* Quick_max = floor((P-1)*log(FLT_RADIX)/log(10) - 1) */
/* Int_max = floor(P*log(FLT_RADIX)/log(10) - 1) */

#ifdef IEEE_Arith
#define Exp_shift  20
#define Exp_shift1 20
#define Exp_msk1    0x100000
#define Exp_msk11   0x100000
#define Exp_mask  0x7ff00000
#define P 53
#define Bias 1023
#define Emin (-1022)
#define Exp_1  0x3ff00000
#define Exp_11 0x3ff00000
#define Ebits 11
#define Frac_mask  0xfffff
#define Frac_mask1 0xfffff
#define Ten_pmax 22
#define Bletch 0x10
#define Bndry_mask  0xfffff
#define Bndry_mask1 0xfffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 1
#define Tiny0 0
#define Tiny1 1
#define Quick_max 14
#define Int_max 14

#ifndef Flt_Rounds
#ifdef FLT_ROUNDS
#define Flt_Rounds FLT_ROUNDS
#else
#define Flt_Rounds 1
#endif
#endif /*Flt_Rounds*/

#else /* ifndef IEEE_Arith */
#undef  Sudden_Underflow
#define Sudden_Underflow
#ifdef IBM
#undef Flt_Rounds
#define Flt_Rounds 0
#define Exp_shift  24
#define Exp_shift1 24
#define Exp_msk1   0x1000000
#define Exp_msk11  0x1000000
#define Exp_mask  0x7f000000
#define P 14
#define Bias 65
#define Exp_1  0x41000000
#define Exp_11 0x41000000
#define Ebits 8	/* exponent has 7 bits, but 8 is the right value in b2d */
#define Frac_mask  0xffffff
#define Frac_mask1 0xffffff
#define Bletch 4
#define Ten_pmax 22
#define Bndry_mask  0xefffff
#define Bndry_mask1 0xffffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 4
#define Tiny0 0x100000
#define Tiny1 0
#define Quick_max 14
#define Int_max 15
#else /* VAX */
#undef Flt_Rounds
#define Flt_Rounds 1
#define Exp_shift  23
#define Exp_shift1 7
#define Exp_msk1    0x80
#define Exp_msk11   0x800000
#define Exp_mask  0x7f80
#define P 56
#define Bias 129
#define Exp_1  0x40800000
#define Exp_11 0x4080
#define Ebits 8
#define Frac_mask  0x7fffff
#define Frac_mask1 0xffff007f
#define Ten_pmax 24
#define Bletch 2
#define Bndry_mask  0xffff007f
#define Bndry_mask1 0xffff007f
#define LSB 0x10000
#define Sign_bit 0x8000
#define Log2P 1
#define Tiny0 0x80
#define Tiny1 0
#define Quick_max 15
#define Int_max 15
#endif /* IBM, VAX */
#endif /* IEEE_Arith */

#ifndef IEEE_Arith
#define ROUND_BIASED
#else
#ifdef ROUND_BIASED_without_Round_Up
#undef  ROUND_BIASED
#define ROUND_BIASED
#endif
#endif

#ifdef RND_PRODQUOT
#define rounded_product(a,b) a = rnd_prod(a, b)
#define rounded_quotient(a,b) a = rnd_quot(a, b)
#ifdef KR_headers
extern double rnd_prod(), rnd_quot();
#else
extern double rnd_prod(double, double), rnd_quot(double, double);
#endif
#else
#define rounded_product(a,b) a *= b
#define rounded_quotient(a,b) a /= b
#endif

#define Big0 (Frac_mask1 | Exp_msk1*(DBL_MAX_EXP+Bias-1))
#define Big1 0xffffffff

#undef  Pack_16
#ifndef Pack_32
#define Pack_32
#endif

#ifdef NO_LONG_LONG
#undef ULLong
#ifdef Just_16
#undef Pack_32
#define Pack_16
/* When Pack_32 is not defined, we store 16 bits per 32-bit Long.
 * This makes some inner loops simpler and sometimes saves work
 * during multiplications, but it often seems to make things slightly
 * slower.  Hence the default is now to store 32 bits per Long.
 */
#endif
#else	/* long long available */
#ifndef Llong
#define Llong long long
#endif
#ifndef ULLong
#define ULLong unsigned Llong
#endif
#endif /* NO_LONG_LONG */

#ifdef Pack_32
#define ULbits 32
#define kshift 5
#define kmask 31
#define ALL_ON 0xffffffff
#else
#define ULbits 16
#define kshift 4
#define kmask 15
#define ALL_ON 0xffff
#endif

#ifndef MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n)	/*nothing*/
#define FREE_DTOA_LOCK(n)	/*nothing*/
#endif

#define Kmax 9

 struct
Bigint {
	struct Bigint *next;
	int k, maxwds, sign, wds;
	ULong x[1];
	};

 typedef struct Bigint Bigint;

#ifdef NO_STRING_H
#ifdef DECLARE_SIZE_T
typedef unsigned int size_t;
#endif
extern void memcpy_D2A ANSI((void*, const void*, size_t));
#define Bcopy(x,y) memcpy_D2A(&x->sign,&y->sign,y->wds*sizeof(ULong) + 2*sizeof(int))
#else /* !NO_STRING_H */
#define Bcopy(x,y) memcpy(&x->sign,&y->sign,y->wds*sizeof(ULong) + 2*sizeof(int))
#endif /* NO_STRING_H */

#define Balloc Balloc_D2A
#define Bfree Bfree_D2A
#define InfName InfName_D2A
#define NanName NanName_D2A
#define ULtoQ ULtoQ_D2A
#define ULtof ULtof_D2A
#define ULtod ULtod_D2A
#define ULtodd ULtodd_D2A
#define ULtox ULtox_D2A
#define ULtoxL ULtoxL_D2A
#define add_nanbits add_nanbits_D2A
#define any_on any_on_D2A
#define b2d b2d_D2A
#define bigtens bigtens_D2A
#define cmp cmp_D2A
#define copybits copybits_D2A
#define d2b d2b_D2A
#define decrement decrement_D2A
#define diff diff_D2A
#define dtoa_result dtoa_result_D2A
#define g__fmt g__fmt_D2A
#define gethex gethex_D2A
#define hexdig hexdig_D2A
#define hexnan hexnan_D2A
#define hi0bits(x) hi0bits_D2A((ULong)(x))
#define i2b i2b_D2A
#define increment increment_D2A
#define lo0bits lo0bits_D2A
#define lshift lshift_D2A
#define match match_D2A
#define mult mult_D2A
#define multadd multadd_D2A
#define nrv_alloc nrv_alloc_D2A
#define pow5mult pow5mult_D2A
#define quorem quorem_D2A
#define ratio ratio_D2A
#define rshift rshift_D2A
#define rv_alloc rv_alloc_D2A
#define s2b s2b_D2A
#define set_ones set_ones_D2A
#define strcp strcp_D2A
#define strtoIg strtoIg_D2A
#define sum sum_D2A
#define tens tens_D2A
#define tinytens tinytens_D2A
#define tinytens tinytens_D2A
#define trailz trailz_D2A
#define ulp ulp_D2A

 extern char *add_nanbits ANSI((char*, size_t, ULong*, int));
 extern char *dtoa_result;
#ifndef STRTODG_FOR_PCC
 extern CONST double bigtens[], tens[], tinytens[];
 extern unsigned char hexdig[];
#endif
 extern const char *InfName[6], *NanName[3];

 extern Bigint *Balloc ANSI((int));
 extern void Bfree ANSI((Bigint*));
 extern void ULtof ANSI((ULong*, ULong*, Long, int));
 extern void ULtod ANSI((ULong*, ULong*, Long, int));
 extern void ULtodd ANSI((ULong*, ULong*, Long, int));
 extern void ULtoQ ANSI((ULong*, ULong*, Long, int));
 extern void ULtox ANSI((UShort*, ULong*, Long, int));
 extern void ULtoxL ANSI((ULong*, ULong*, Long, int));
 extern ULong any_on ANSI((Bigint*, int));
 extern double b2d ANSI((Bigint*, int*));
 extern int cmp ANSI((Bigint*, Bigint*));
 extern void copybits ANSI((ULong*, int, Bigint*));
 extern Bigint *d2b ANSI((double, int*, int*));
 extern void decrement ANSI((Bigint*));
 extern Bigint *diff ANSI((Bigint*, Bigint*));
 extern char *dtoa ANSI((double d, int mode, int ndigits,
			int *decpt, int *sign, char **rve));
 extern char *g__fmt ANSI((char*, char*, char*, int, ULong, size_t));
 extern int gethex ANSI((CONST char**, FPI*, Long*, Bigint**, int));
 extern void hexdig_init_D2A(Void);
 extern int hexnan ANSI((CONST char**, FPI*, ULong*));
 extern int hi0bits_D2A ANSI((ULong));
 extern Bigint *i2b ANSI((int));
 extern Bigint *increment ANSI((Bigint*));
 extern int lo0bits ANSI((ULong*));
 extern Bigint *lshift ANSI((Bigint*, int));
 extern int match ANSI((CONST char**, char*));
 extern Bigint *mult ANSI((Bigint*, Bigint*));
 extern Bigint *multadd ANSI((Bigint*, int, int));
 extern char *nrv_alloc ANSI((char*, char **, int));
 extern Bigint *pow5mult ANSI((Bigint*, int));
 extern int quorem ANSI((Bigint*, Bigint*));
 extern double ratio ANSI((Bigint*, Bigint*));
 extern void rshift ANSI((Bigint*, int));
 extern char *rv_alloc ANSI((int));
 extern Bigint *s2b ANSI((CONST char*, int, int, ULong, int));
 extern Bigint *set_ones ANSI((Bigint*, int));
 extern char *strcp ANSI((char*, const char*));
#ifndef STRTODG_FOR_PCC
 extern int strtoIg ANSI((CONST char*, char**, FPI*, Long*, Bigint**, int*));
 extern double strtod ANSI((const char *s00, char **se));
#endif
 extern Bigint *sum ANSI((Bigint*, Bigint*));
 extern int trailz ANSI((Bigint*));
 extern double ulp ANSI((U*));

#ifdef __cplusplus
}
#endif
#ifndef STRTODG_FOR_PCC /* No NaN stuff here */
/*
 * NAN_WORD0 and NAN_WORD1 are only referenced in strtod.c.  Prior to
 * 20050115, they used to be hard-wired here (to 0x7ff80000 and 0,
 * respectively), but now are determined by compiling and running
 * qnan.c to generate gd_qnan.h, which specifies d_QNAN0 and d_QNAN1.
 * Formerly gdtoaimp.h recommended supplying suitable -DNAN_WORD0=...
 * and -DNAN_WORD1=...  values if necessary.  This should still work.
 * (On HP Series 700/800 machines, -DNAN_WORD0=0x7ff40000 works.)
 */
#ifdef IEEE_Arith
#ifndef NO_INFNAN_CHECK
#undef INFNAN_CHECK
#define INFNAN_CHECK
#endif
#ifdef IEEE_MC68k
#define _0 0
#define _1 1
#ifndef NAN_WORD0
#define NAN_WORD0 d_QNAN0
#endif
#ifndef NAN_WORD1
#define NAN_WORD1 d_QNAN1
#endif
#else
#define _0 1
#define _1 0
#ifndef NAN_WORD0
#define NAN_WORD0 d_QNAN1
#endif
#ifndef NAN_WORD1
#define NAN_WORD1 d_QNAN0
#endif
#endif
#else
#undef INFNAN_CHECK
#endif
#endif

#undef SI
#ifdef Sudden_Underflow
#define SI 1
#else
#define SI 0
#endif

#endif /* GDTOAIMP_H_INCLUDED */

/* misc.c */
 static Bigint *freelist[Kmax+1];
#ifndef Omit_Private_Memory
#ifndef PRIVATE_MEM
#define PRIVATE_MEM 2304
#endif
#define PRIVATE_mem ((PRIVATE_MEM+sizeof(double)-1)/sizeof(double))
static double private_mem[PRIVATE_mem], *pmem_next = private_mem;
#endif

 Bigint *
Balloc
#ifdef KR_headers
	(k) int k;
#else
	(int k)
#endif
{
	int x;
	Bigint *rv;
#ifndef Omit_Private_Memory
	unsigned int len;
#endif

	ACQUIRE_DTOA_LOCK(0);
	/* The k > Kmax case does not need ACQUIRE_DTOA_LOCK(0), */
	/* but this case seems very unlikely. */
	if (k <= Kmax && (rv = freelist[k]) !=0) {
		freelist[k] = rv->next;
		}
	else {
		x = 1 << k;
#ifdef Omit_Private_Memory
		rv = (Bigint *)MALLOC(sizeof(Bigint) + (x-1)*sizeof(ULong));
#else
		len = (sizeof(Bigint) + (x-1)*sizeof(ULong) + sizeof(double) - 1)
			/sizeof(double);
		if (k <= Kmax && pmem_next - private_mem + len <= PRIVATE_mem) {
			rv = (Bigint*)pmem_next;
			pmem_next += len;
			}
		else
			rv = (Bigint*)MALLOC(len*sizeof(double));
#endif
		rv->k = k;
		rv->maxwds = x;
		}
	FREE_DTOA_LOCK(0);
	rv->sign = rv->wds = 0;
	return rv;
	}

 void
Bfree
#ifdef KR_headers
	(v) Bigint *v;
#else
	(Bigint *v)
#endif
{
	if (v) {
		if (v->k > Kmax)
#ifdef FREE
			FREE((void*)v);
#else
			free((void*)v);
#endif
		else {
			ACQUIRE_DTOA_LOCK(0);
			v->next = freelist[v->k];
			freelist[v->k] = v;
			FREE_DTOA_LOCK(0);
			}
		}
	}

 int
lo0bits
#ifdef KR_headers
	(y) ULong *y;
#else
	(ULong *y)
#endif
{
	int k;
	ULong x = *y;

	if (x & 7) {
		if (x & 1)
			return 0;
		if (x & 2) {
			*y = x >> 1;
			return 1;
			}
		*y = x >> 2;
		return 2;
		}
	k = 0;
	if (!(x & 0xffff)) {
		k = 16;
		x >>= 16;
		}
	if (!(x & 0xff)) {
		k += 8;
		x >>= 8;
		}
	if (!(x & 0xf)) {
		k += 4;
		x >>= 4;
		}
	if (!(x & 0x3)) {
		k += 2;
		x >>= 2;
		}
	if (!(x & 1)) {
		k++;
		x >>= 1;
		if (!x)
			return 32;
		}
	*y = x;
	return k;
	}

 Bigint *
multadd
#ifdef KR_headers
	(b, m, a) Bigint *b; int m, a;
#else
	(Bigint *b, int m, int a)	/* multiply by m and add a */
#endif
{
	int i, wds;
#ifdef ULLong
	ULong *x;
	ULLong carry, y;
#else
	ULong carry, *x, y;
#ifdef Pack_32
	ULong xi, z;
#endif
#endif
	Bigint *b1;

	wds = b->wds;
	x = b->x;
	i = 0;
	carry = a;
	do {
#ifdef ULLong
		y = *x * (ULLong)m + carry;
		carry = y >> 32;
		*x++ = y & 0xffffffffUL;
#else
#ifdef Pack_32
		xi = *x;
		y = (xi & 0xffff) * m + carry;
		z = (xi >> 16) * m + (y >> 16);
		carry = z >> 16;
		*x++ = (z << 16) + (y & 0xffff);
#else
		y = *x * m + carry;
		carry = y >> 16;
		*x++ = y & 0xffff;
#endif
#endif
		}
		while(++i < wds);
	if (carry) {
		if (wds >= b->maxwds) {
			b1 = Balloc(b->k+1);
			Bcopy(b1, b);
			Bfree(b);
			b = b1;
			}
		b->x[wds++] = carry;
		b->wds = wds;
		}
	return b;
	}

 int
hi0bits_D2A
#ifdef KR_headers
	(x) ULong x;
#else
	(ULong x)
#endif
{
	int k = 0;

	if (!(x & 0xffff0000)) {
		k = 16;
		x <<= 16;
		}
	if (!(x & 0xff000000)) {
		k += 8;
		x <<= 8;
		}
	if (!(x & 0xf0000000)) {
		k += 4;
		x <<= 4;
		}
	if (!(x & 0xc0000000)) {
		k += 2;
		x <<= 2;
		}
	if (!(x & 0x80000000)) {
		k++;
		if (!(x & 0x40000000))
			return 32;
		}
	return k;
	}

 Bigint *
i2b
#ifdef KR_headers
	(i) int i;
#else
	(int i)
#endif
{
	Bigint *b;

	b = Balloc(1);
	b->x[0] = i;
	b->wds = 1;
	return b;
	}

 Bigint *
mult
#ifdef KR_headers
	(a, b) Bigint *a, *b;
#else
	(Bigint *a, Bigint *b)
#endif
{
	Bigint *c;
	int k, wa, wb, wc;
	ULong *x, *xa, *xae, *xb, *xbe, *xc, *xc0;
	ULong y;
#ifdef ULLong
	ULLong carry, z;
#else
	ULong carry, z;
#ifdef Pack_32
	ULong z2;
#endif
#endif

	if (a->wds < b->wds) {
		c = a;
		a = b;
		b = c;
		}
	k = a->k;
	wa = a->wds;
	wb = b->wds;
	wc = wa + wb;
	if (wc > a->maxwds)
		k++;
	c = Balloc(k);
	for(x = c->x, xa = x + wc; x < xa; x++)
		*x = 0;
	xa = a->x;
	xae = xa + wa;
	xb = b->x;
	xbe = xb + wb;
	xc0 = c->x;
#ifdef ULLong
	for(; xb < xbe; xc0++) {
		if ( (y = *xb++) !=0) {
			x = xa;
			xc = xc0;
			carry = 0;
			do {
				z = *x++ * (ULLong)y + *xc + carry;
				carry = z >> 32;
				*xc++ = z & 0xffffffffUL;
				}
				while(x < xae);
			*xc = carry;
			}
		}
#else
#ifdef Pack_32
	for(; xb < xbe; xb++, xc0++) {
		if ( (y = *xb & 0xffff) !=0) {
			x = xa;
			xc = xc0;
			carry = 0;
			do {
				z = (*x & 0xffff) * y + (*xc & 0xffff) + carry;
				carry = z >> 16;
				z2 = (*x++ >> 16) * y + (*xc >> 16) + carry;
				carry = z2 >> 16;
				Storeinc(xc, z2, z);
				}
				while(x < xae);
			*xc = carry;
			}
		if ( (y = *xb >> 16) !=0) {
			x = xa;
			xc = xc0;
			carry = 0;
			z2 = *xc;
			do {
				z = (*x & 0xffff) * y + (*xc >> 16) + carry;
				carry = z >> 16;
				Storeinc(xc, z, z2);
				z2 = (*x++ >> 16) * y + (*xc & 0xffff) + carry;
				carry = z2 >> 16;
				}
				while(x < xae);
			*xc = z2;
			}
		}
#else
	for(; xb < xbe; xc0++) {
		if ( (y = *xb++) !=0) {
			x = xa;
			xc = xc0;
			carry = 0;
			do {
				z = *x++ * y + *xc + carry;
				carry = z >> 16;
				*xc++ = z & 0xffff;
				}
				while(x < xae);
			*xc = carry;
			}
		}
#endif
#endif
	for(xc0 = c->x, xc = xc0 + wc; wc > 0 && !*--xc; --wc) ;
	c->wds = wc;
	return c;
	}

 static Bigint *p5s;

 Bigint *
pow5mult
#ifdef KR_headers
	(b, k) Bigint *b; int k;
#else
	(Bigint *b, int k)
#endif
{
	Bigint *b1, *p5, *p51;
	int i;
	static int p05[3] = { 5, 25, 125 };

	if ( (i = k & 3) !=0)
		b = multadd(b, p05[i-1], 0);

	if (!(k >>= 2))
		return b;
	if ((p5 = p5s) == 0) {
		/* first time */
#ifdef MULTIPLE_THREADS
		ACQUIRE_DTOA_LOCK(1);
		if (!(p5 = p5s)) {
			p5 = p5s = i2b(625);
			p5->next = 0;
			}
		FREE_DTOA_LOCK(1);
#else
		p5 = p5s = i2b(625);
		p5->next = 0;
#endif
		}
	for(;;) {
		if (k & 1) {
			b1 = mult(b, p5);
			Bfree(b);
			b = b1;
			}
		if (!(k >>= 1))
			break;
		if ((p51 = p5->next) == 0) {
#ifdef MULTIPLE_THREADS
			ACQUIRE_DTOA_LOCK(1);
			if (!(p51 = p5->next)) {
				p51 = p5->next = mult(p5,p5);
				p51->next = 0;
				}
			FREE_DTOA_LOCK(1);
#else
			p51 = p5->next = mult(p5,p5);
			p51->next = 0;
#endif
			}
		p5 = p51;
		}
	return b;
	}

 Bigint *
lshift
#ifdef KR_headers
	(b, k) Bigint *b; int k;
#else
	(Bigint *b, int k)
#endif
{
	int i, k1, n, n1;
	Bigint *b1;
	ULong *x, *x1, *xe, z;

	n = k >> kshift;
	k1 = b->k;
	n1 = n + b->wds + 1;
	for(i = b->maxwds; n1 > i; i <<= 1)
		k1++;
	b1 = Balloc(k1);
	x1 = b1->x;
	for(i = 0; i < n; i++)
		*x1++ = 0;
	x = b->x;
	xe = x + b->wds;
	if (k &= kmask) {
#ifdef Pack_32
		k1 = 32 - k;
		z = 0;
		do {
			*x1++ = *x << k | z;
			z = *x++ >> k1;
			}
			while(x < xe);
		if ((*x1 = z) !=0)
			++n1;
#else
		k1 = 16 - k;
		z = 0;
		do {
			*x1++ = *x << k  & 0xffff | z;
			z = *x++ >> k1;
			}
			while(x < xe);
		if (*x1 = z)
			++n1;
#endif
		}
	else do
		*x1++ = *x++;
		while(x < xe);
	b1->wds = n1 - 1;
	Bfree(b);
	return b1;
	}

 int
cmp
#ifdef KR_headers
	(a, b) Bigint *a, *b;
#else
	(Bigint *a, Bigint *b)
#endif
{
	ULong *xa, *xa0, *xb, *xb0;
	int i, j;

	i = a->wds;
	j = b->wds;
#ifdef DEBUG
	if (i > 1 && !a->x[i-1])
		Bug("cmp called with a->x[a->wds-1] == 0");
	if (j > 1 && !b->x[j-1])
		Bug("cmp called with b->x[b->wds-1] == 0");
#endif
	if (i -= j)
		return i;
	xa0 = a->x;
	xa = xa0 + j;
	xb0 = b->x;
	xb = xb0 + j;
	for(;;) {
		if (*--xa != *--xb)
			return *xa < *xb ? -1 : 1;
		if (xa <= xa0)
			break;
		}
	return 0;
	}

 Bigint *
diff
#ifdef KR_headers
	(a, b) Bigint *a, *b;
#else
	(Bigint *a, Bigint *b)
#endif
{
	Bigint *c;
	int i, wa, wb;
	ULong *xa, *xae, *xb, *xbe, *xc;
#ifdef ULLong
	ULLong borrow, y;
#else
	ULong borrow, y;
#ifdef Pack_32
	ULong z;
#endif
#endif

	i = cmp(a,b);
	if (!i) {
		c = Balloc(0);
		c->wds = 1;
		c->x[0] = 0;
		return c;
		}
	if (i < 0) {
		c = a;
		a = b;
		b = c;
		i = 1;
		}
	else
		i = 0;
	c = Balloc(a->k);
	c->sign = i;
	wa = a->wds;
	xa = a->x;
	xae = xa + wa;
	wb = b->wds;
	xb = b->x;
	xbe = xb + wb;
	xc = c->x;
	borrow = 0;
#ifdef ULLong
	do {
		y = (ULLong)*xa++ - *xb++ - borrow;
		borrow = y >> 32 & 1UL;
		*xc++ = y & 0xffffffffUL;
		}
		while(xb < xbe);
	while(xa < xae) {
		y = *xa++ - borrow;
		borrow = y >> 32 & 1UL;
		*xc++ = y & 0xffffffffUL;
		}
#else
#ifdef Pack_32
	do {
		y = (*xa & 0xffff) - (*xb & 0xffff) - borrow;
		borrow = (y & 0x10000) >> 16;
		z = (*xa++ >> 16) - (*xb++ >> 16) - borrow;
		borrow = (z & 0x10000) >> 16;
		Storeinc(xc, z, y);
		}
		while(xb < xbe);
	while(xa < xae) {
		y = (*xa & 0xffff) - borrow;
		borrow = (y & 0x10000) >> 16;
		z = (*xa++ >> 16) - borrow;
		borrow = (z & 0x10000) >> 16;
		Storeinc(xc, z, y);
		}
#else
	do {
		y = *xa++ - *xb++ - borrow;
		borrow = (y & 0x10000) >> 16;
		*xc++ = y & 0xffff;
		}
		while(xb < xbe);
	while(xa < xae) {
		y = *xa++ - borrow;
		borrow = (y & 0x10000) >> 16;
		*xc++ = y & 0xffff;
		}
#endif
#endif
	while(!*--xc)
		wa--;
	c->wds = wa;
	return c;
	}

 double
b2d
#ifdef KR_headers
	(a, e) Bigint *a; int *e;
#else
	(Bigint *a, int *e)
#endif
{
	ULong *xa, *xa0, w, y, z;
	int k;
	U d;
#ifdef VAX
	ULong d0, d1;
#else
#define d0 word0(&d)
#define d1 word1(&d)
#endif

	xa0 = a->x;
	xa = xa0 + a->wds;
	y = *--xa;
#ifdef DEBUG
	if (!y) Bug("zero y in b2d");
#endif
	k = hi0bits(y);
	*e = 32 - k;
#ifdef Pack_32
	if (k < Ebits) {
		d0 = Exp_1 | y >> (Ebits - k);
		w = xa > xa0 ? *--xa : 0;
		d1 = y << ((32-Ebits) + k) | w >> (Ebits - k);
		goto ret_d;
		}
	z = xa > xa0 ? *--xa : 0;
	if (k -= Ebits) {
		d0 = Exp_1 | y << k | z >> (32 - k);
		y = xa > xa0 ? *--xa : 0;
		d1 = z << k | y >> (32 - k);
		}
	else {
		d0 = Exp_1 | y;
		d1 = z;
		}
#else
	if (k < Ebits + 16) {
		z = xa > xa0 ? *--xa : 0;
		d0 = Exp_1 | y << k - Ebits | z >> Ebits + 16 - k;
		w = xa > xa0 ? *--xa : 0;
		y = xa > xa0 ? *--xa : 0;
		d1 = z << k + 16 - Ebits | w << k - Ebits | y >> 16 + Ebits - k;
		goto ret_d;
		}
	z = xa > xa0 ? *--xa : 0;
	w = xa > xa0 ? *--xa : 0;
	k -= Ebits + 16;
	d0 = Exp_1 | y << k + 16 | z << k | w >> 16 - k;
	y = xa > xa0 ? *--xa : 0;
	d1 = w << k + 16 | y << k;
#endif
 ret_d:
#ifdef VAX
	word0(&d) = d0 >> 16 | d0 << 16;
	word1(&d) = d1 >> 16 | d1 << 16;
#endif
	return dval(&d);
	}
#undef d0
#undef d1

 Bigint *
d2b
#ifdef KR_headers
	(dd, e, bits) double dd; int *e, *bits;
#else
	(double dd, int *e, int *bits)
#endif
{
	Bigint *b;
	U d;
#ifndef Sudden_Underflow
	int i;
#endif
	int de, k;
	ULong *x, y, z;
#ifdef VAX
	ULong d0, d1;
#else
#define d0 word0(&d)
#define d1 word1(&d)
#endif
	d.d = dd;
#ifdef VAX
	d0 = word0(&d) >> 16 | word0(&d) << 16;
	d1 = word1(&d) >> 16 | word1(&d) << 16;
#endif

#ifdef Pack_32
	b = Balloc(1);
#else
	b = Balloc(2);
#endif
	x = b->x;

	z = d0 & Frac_mask;
	d0 &= 0x7fffffff;	/* clear sign bit, which we ignore */
#ifdef Sudden_Underflow
	de = (int)(d0 >> Exp_shift);
#ifndef IBM
	z |= Exp_msk11;
#endif
#else
	if ( (de = (int)(d0 >> Exp_shift)) !=0)
		z |= Exp_msk1;
#endif
#ifdef Pack_32
	if ( (y = d1) !=0) {
		if ( (k = lo0bits(&y)) !=0) {
			x[0] = y | z << (32 - k);
			z >>= k;
			}
		else
			x[0] = y;
#ifndef Sudden_Underflow
		i =
#endif
		     b->wds = (x[1] = z) !=0 ? 2 : 1;
		}
	else {
		k = lo0bits(&z);
		x[0] = z;
#ifndef Sudden_Underflow
		i =
#endif
		    b->wds = 1;
		k += 32;
		}
#else
	if ( (y = d1) !=0) {
		if ( (k = lo0bits(&y)) !=0)
			if (k >= 16) {
				x[0] = y | z << 32 - k & 0xffff;
				x[1] = z >> k - 16 & 0xffff;
				x[2] = z >> k;
				i = 2;
				}
			else {
				x[0] = y & 0xffff;
				x[1] = y >> 16 | z << 16 - k & 0xffff;
				x[2] = z >> k & 0xffff;
				x[3] = z >> k+16;
				i = 3;
				}
		else {
			x[0] = y & 0xffff;
			x[1] = y >> 16;
			x[2] = z & 0xffff;
			x[3] = z >> 16;
			i = 3;
			}
		}
	else {
#ifdef DEBUG
		if (!z)
			Bug("Zero passed to d2b");
#endif
		k = lo0bits(&z);
		if (k >= 16) {
			x[0] = z;
			i = 0;
			}
		else {
			x[0] = z & 0xffff;
			x[1] = z >> 16;
			i = 1;
			}
		k += 32;
		}
	while(!x[i])
		--i;
	b->wds = i + 1;
#endif
#ifndef Sudden_Underflow
	if (de) {
#endif
#ifdef IBM
		*e = (de - Bias - (P-1) << 2) + k;
		*bits = 4*P + 8 - k - hi0bits(word0(&d) & Frac_mask);
#else
		*e = de - Bias - (P-1) + k;
		*bits = P - k;
#endif
#ifndef Sudden_Underflow
		}
	else {
		*e = de - Bias - (P-1) + 1 + k;
#ifdef Pack_32
		*bits = 32*i - hi0bits(x[i-1]);
#else
		*bits = (i+2)*16 - hi0bits(x[i]);
#endif
		}
#endif
	return b;
	}
#undef d0
#undef d1

 CONST double
#ifdef IEEE_Arith
bigtens[] = { 1e16, 1e32, 1e64, 1e128, 1e256 };
CONST double tinytens[] = { 1e-16, 1e-32, 1e-64, 1e-128, 1e-256
		};
#else
#ifdef IBM
bigtens[] = { 1e16, 1e32, 1e64 };
CONST double tinytens[] = { 1e-16, 1e-32, 1e-64 };
#else
bigtens[] = { 1e16, 1e32 };
CONST double tinytens[] = { 1e-16, 1e-32 };
#endif
#endif

 CONST double
tens[] = {
		1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
		1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
		1e20, 1e21, 1e22
#ifdef VAX
		, 1e23, 1e24
#endif
		};

 char *
#ifdef KR_headers
strcp_D2A(a, b) char *a; char *b;
#else
strcp_D2A(char *a, CONST char *b)
#endif
{
	while((*a = *b++))
		a++;
	return a;
	}

#ifdef NO_STRING_H

 Char *
#ifdef KR_headers
memcpy_D2A(a, b, len) Char *a; Char *b; size_t len;
#else
memcpy_D2A(void *a1, void *b1, size_t len)
#endif
{
	char *a = (char*)a1, *ae = a + len;
	char *b = (char*)b1, *a0 = a;
	while(a < ae)
		*a++ = *b++;
	return a0;
	}

#endif /* NO_STRING_H */

/* gmisc.c */
 void
#ifdef KR_headers
rshift(b, k) Bigint *b; int k;
#else
rshift(Bigint *b, int k)
#endif
{
	ULong *x, *x1, *xe, y;
	int n;

	x = x1 = b->x;
	n = k >> kshift;
	if (n < b->wds) {
		xe = x + b->wds;
		x += n;
		if (k &= kmask) {
			n = ULbits - k;
			y = *x++ >> k;
			while(x < xe) {
				*x1++ = (y | (*x << n)) & ALL_ON;
				y = *x++ >> k;
				}
			if ((*x1 = y) !=0)
				x1++;
			}
		else
			while(x < xe)
				*x1++ = *x++;
		}
	if ((b->wds = x1 - b->x) == 0)
		b->x[0] = 0;
	}

 int
#ifdef KR_headers
trailz(b) Bigint *b;
#else
trailz(Bigint *b)
#endif
{
	ULong L, *x, *xe;
	int n = 0;

	x = b->x;
	xe = x + b->wds;
	for(n = 0; x < xe && !*x; x++)
		n += ULbits;
	if (x < xe) {
		L = *x;
		n += lo0bits(&L);
		}
	return n;
	}

/* smisc.c */
 Bigint *
s2b
#ifdef KR_headers
	(s, nd0, nd, y9, dplen) CONST char *s; int dplen, nd0, nd; ULong y9;
#else
	(CONST char *s, int nd0, int nd, ULong y9, int dplen)
#endif
{
	Bigint *b;
	int i, k;
	Long x, y;

	x = (nd + 8) / 9;
	for(k = 0, y = 1; x > y; y <<= 1, k++) ;
#ifdef Pack_32
	b = Balloc(k);
	b->x[0] = y9;
	b->wds = 1;
#else
	b = Balloc(k+1);
	b->x[0] = y9 & 0xffff;
	b->wds = (b->x[1] = y9 >> 16) ? 2 : 1;
#endif

	i = 9;
	if (9 < nd0) {
		s += 9;
		do b = multadd(b, 10, *s++ - '0');
			while(++i < nd0);
		s += dplen;
		}
	else
		s += dplen + 9;
	for(; i < nd; i++)
		b = multadd(b, 10, *s++ - '0');
	return b;
	}

 double
ratio
#ifdef KR_headers
	(a, b) Bigint *a, *b;
#else
	(Bigint *a, Bigint *b)
#endif
{
	U da, db;
	int k, ka, kb;

	dval(&da) = b2d(a, &ka);
	dval(&db) = b2d(b, &kb);
	k = ka - kb + ULbits*(a->wds - b->wds);
#ifdef IBM
	if (k > 0) {
		word0(&da) += (k >> 2)*Exp_msk1;
		if (k &= 3)
			dval(&da) *= 1 << k;
		}
	else {
		k = -k;
		word0(&db) += (k >> 2)*Exp_msk1;
		if (k &= 3)
			dval(&db) *= 1 << k;
		}
#else
	if (k > 0)
		word0(&da) += k*Exp_msk1;
	else {
		k = -k;
		word0(&db) += k*Exp_msk1;
		}
#endif
	return dval(&da) / dval(&db);
	}

#ifdef INFNAN_CHECK

 int
match
#ifdef KR_headers
	(sp, t) char **sp, *t;
#else
	(CONST char **sp, char *t)
#endif
{
	int c, d;
	CONST char *s = *sp;

	while( (d = *t++) !=0) {
		if ((c = *++s) >= 'A' && c <= 'Z')
			c += 'a' - 'A';
		if (c != d)
			return 0;
		}
	*sp = s + 1;
	return 1;
	}
#endif /* INFNAN_CHECK */

 void
#ifdef KR_headers
copybits(c, n, b) ULong *c; int n; Bigint *b;
#else
copybits(ULong *c, int n, Bigint *b)
#endif
{
	ULong *ce, *x, *xe;
#ifdef Pack_16
	int nw, nw1;
#endif

	ce = c + ((n-1) >> kshift) + 1;
	x = b->x;
#ifdef Pack_32
	xe = x + b->wds;
	while(x < xe)
		*c++ = *x++;
#else
	nw = b->wds;
	nw1 = nw & 1;
	for(xe = x + (nw - nw1); x < xe; x += 2)
		Storeinc(c, x[1], x[0]);
	if (nw1)
		*c++ = *x;
#endif
	while(c < ce)
		*c++ = 0;
	}

 ULong
#ifdef KR_headers
any_on(b, k) Bigint *b; int k;
#else
any_on(Bigint *b, int k)
#endif
{
	int n, nwds;
	ULong *x, *x0, x1, x2;

	x = b->x;
	nwds = b->wds;
	n = k >> kshift;
	if (n > nwds)
		n = nwds;
	else if (n < nwds && (k &= kmask)) {
		x1 = x2 = x[n];
		x1 >>= k;
		x1 <<= k;
		if (x1 != x2)
			return 1;
		}
	x0 = x;
	x += n;
	while(x > x0)
		if (*--x)
			return 1;
	return 0;
	}

/* sum.c */
 Bigint *
#ifdef KR_headers
sum(a, b) Bigint *a; Bigint *b;
#else
sum(Bigint *a, Bigint *b)
#endif
{
	Bigint *c;
	ULong carry, *xc, *xa, *xb, *xe, y;
#ifdef Pack_32
	ULong z;
#endif

	if (a->wds < b->wds) {
		c = b; b = a; a = c;
		}
	c = Balloc(a->k);
	c->wds = a->wds;
	carry = 0;
	xa = a->x;
	xb = b->x;
	xc = c->x;
	xe = xc + b->wds;
#ifdef Pack_32
	do {
		y = (*xa & 0xffff) + (*xb & 0xffff) + carry;
		carry = (y & 0x10000) >> 16;
		z = (*xa++ >> 16) + (*xb++ >> 16) + carry;
		carry = (z & 0x10000) >> 16;
		Storeinc(xc, z, y);
		}
		while(xc < xe);
	xe += a->wds - b->wds;
	while(xc < xe) {
		y = (*xa & 0xffff) + carry;
		carry = (y & 0x10000) >> 16;
		z = (*xa++ >> 16) + carry;
		carry = (z & 0x10000) >> 16;
		Storeinc(xc, z, y);
		}
#else
	do {
		y = *xa++ + *xb++ + carry;
		carry = (y & 0x10000) >> 16;
		*xc++ = y & 0xffff;
		}
		while(xc < xe);
	xe += a->wds - b->wds;
	while(xc < xe) {
		y = *xa++ + carry;
		carry = (y & 0x10000) >> 16;
		*xc++ = y & 0xffff;
		}
#endif
	if (carry) {
		if (c->wds == c->maxwds) {
			b = Balloc(c->k + 1);
			Bcopy(b, c);
			Bfree(c);
			c = b;
			}
		c->x[c->wds++] = 1;
		}
	return c;
	}

/* hd_init.c */
#if 0
 unsigned char hexdig[256];

 static void
#ifdef KR_headers
htinit(h, s, inc) unsigned char *h; unsigned char *s; int inc;
#else
htinit(unsigned char *h, unsigned char *s, int inc)
#endif
{
	int i, j;
	for(i = 0; (j = s[i]) !=0; i++)
		h[j] = i + inc;
	}

 void
hexdig_init_D2A(Void)	/* Use of hexdig_init omitted 20121220 to avoid a */
			/* race condition when multiple threads are used. */
{
#define USC (unsigned char *)
	htinit(hexdig, USC "0123456789", 0x10);
	htinit(hexdig, USC "abcdef", 0x10 + 10);
	htinit(hexdig, USC "ABCDEF", 0x10 + 10);
	}
#else
 unsigned char hexdig[256] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	16,17,18,19,20,21,22,23,24,25,0,0,0,0,0,0,
	0,26,27,28,29,30,31,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,26,27,28,29,30,31,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	};
#endif

/* gethex.c */
#ifdef USE_LOCALE
#include "locale.h"
#endif

 int
#ifdef KR_headers
gethex(sp, fpi, exp, bp, sign)
	CONST char **sp; FPI *fpi; Long *exp; Bigint **bp; int sign;
#else
gethex( CONST char **sp, FPI *fpi, Long *exp, Bigint **bp, int sign)
#endif
{
	Bigint *b;
	CONST unsigned char *decpt, *s0, *s, *s1;
	int big, esign, havedig, irv, j, k, n, n0, nbits, up, zret;
	ULong L, lostbits, *x;
	Long e, e1;
#ifdef USE_LOCALE
	int i;
#ifdef NO_LOCALE_CACHE
	const unsigned char *decimalpoint = (unsigned char*)localeconv()->decimal_point;
#else
	const unsigned char *decimalpoint;
	static unsigned char *decimalpoint_cache;
	if (!(s0 = decimalpoint_cache)) {
		s0 = (unsigned char*)localeconv()->decimal_point;
		if ((decimalpoint_cache = (char*)MALLOC(strlen(s0) + 1))) {
			strcpy(decimalpoint_cache, s0);
			s0 = decimalpoint_cache;
			}
		}
	decimalpoint = s0;
#endif
#endif

	/**** if (!hexdig['0']) hexdig_init_D2A(); ****/
	*bp = 0;
	havedig = 0;
	s0 = *(CONST unsigned char **)sp + 2;
	while(s0[havedig] == '0')
		havedig++;
	s0 += havedig;
	s = s0;
	decpt = 0;
	zret = 0;
	e = 0;
	if (hexdig[*s])
		havedig++;
	else {
		zret = 1;
#ifdef USE_LOCALE
		for(i = 0; decimalpoint[i]; ++i) {
			if (s[i] != decimalpoint[i])
				goto pcheck;
			}
		decpt = s += i;
#else
		if (*s != '.')
			goto pcheck;
		decpt = ++s;
#endif
		if (!hexdig[*s])
			goto pcheck;
		while(*s == '0')
			s++;
		if (hexdig[*s])
			zret = 0;
		havedig = 1;
		s0 = s;
		}
	while(hexdig[*s])
		s++;
#ifdef USE_LOCALE
	if (*s == *decimalpoint && !decpt) {
		for(i = 1; decimalpoint[i]; ++i) {
			if (s[i] != decimalpoint[i])
				goto pcheck;
			}
		decpt = s += i;
#else
	if (*s == '.' && !decpt) {
		decpt = ++s;
#endif
		while(hexdig[*s])
			s++;
		}/*}*/
	if (decpt)
		e = -(((Long)(s-decpt)) << 2);
 pcheck:
	s1 = s;
	big = esign = 0;
	switch(*s) {
	  case 'p':
	  case 'P':
		switch(*++s) {
		  case '-':
			esign = 1;
			/* no break */
		  case '+':
			s++;
		  }
		if ((n = hexdig[*s]) == 0 || n > 0x19) {
			s = s1;
			break;
			}
		e1 = n - 0x10;
		while((n = hexdig[*++s]) !=0 && n <= 0x19) {
			if (e1 & 0xf8000000)
				big = 1;
			e1 = 10*e1 + n - 0x10;
			}
		if (esign)
			e1 = -e1;
		e += e1;
	  }
	*sp = (char*)s;
	if (!havedig)
		*sp = (char*)s0 - 1;
	if (zret)
		return STRTOG_Zero;
	if (big) {
		if (esign) {
#ifdef STRTODG_FOR_PCC
		  if (! fpi->sudden_underflow)
#endif
			switch(fpi->rounding) {
			  case FPI_Round_up:
				if (sign)
					break;
				goto ret_tiny;
			  case FPI_Round_down:
				if (!sign)
					break;
				goto ret_tiny;
			  }
			goto retz;
 ret_tiny:
			b = Balloc(0);
			b->wds = 1;
			b->x[0] = 1;
			goto dret;
			}
		switch(fpi->rounding) {
		  case FPI_Round_near:
			goto ovfl1;
		  case FPI_Round_up:
			if (!sign)
				goto ovfl1;
			goto ret_big;
		  case FPI_Round_down:
			if (sign)
				goto ovfl1;
			goto ret_big;
		  }
 ret_big:
		nbits = fpi->nbits;
		n0 = n = nbits >> kshift;
		if (nbits & kmask)
			++n;
		for(j = n, k = 0; j >>= 1; ++k);
		*bp = b = Balloc(k);
		b->wds = n;
		for(j = 0; j < n0; ++j)
			b->x[j] = ALL_ON;
		if (n > n0)
			b->x[j] = ULbits >> (ULbits - (nbits & kmask));
		*exp = fpi->emin;
		return STRTOG_Normal | STRTOG_Inexlo;
		}
	n = s1 - s0 - 1;
	for(k = 0; n > (1 << (kshift-2)) - 1; n >>= 1)
		k++;
	b = Balloc(k);
	x = b->x;
	n = 0;
	L = 0;
#ifdef USE_LOCALE
	for(i = 0; decimalpoint[i+1]; ++i);
#endif
	while(s1 > s0) {
#ifdef USE_LOCALE
		if (*--s1 == decimalpoint[i]) {
			s1 -= i;
			continue;
			}
#else
		if (*--s1 == '.')
			continue;
#endif
		if (n == ULbits) {
			*x++ = L;
			L = 0;
			n = 0;
			}
		L |= (hexdig[*s1] & 0x0f) << n;
		n += 4;
		}
	*x++ = L;
	b->wds = n = x - b->x;
	n = ULbits*n - hi0bits(L);
	nbits = fpi->nbits;
	lostbits = 0;
	x = b->x;
	if (n > nbits) {
		n -= nbits;
		if (any_on(b,n)) {
			lostbits = 1;
			k = n - 1;
			if (x[k>>kshift] & 1 << (k & kmask)) {
				lostbits = 2;
				if (k > 0 && any_on(b,k))
					lostbits = 3;
				}
			}
		rshift(b, n);
		e += n;
		}
	else if (n < nbits) {
		n = nbits - n;
		b = lshift(b, n);
		e -= n;
		x = b->x;
		}
	if (e > fpi->emax) {
 ovfl:
		Bfree(b);
 ovfl1:
#ifndef NO_ERRNO
		errno = ERANGE;
#endif
#ifdef STRTODG_FOR_PCC
		*exp = fpi->emax + 1;
#endif
		return STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
		}
	irv = STRTOG_Normal;
	if (e < fpi->emin) {
#ifdef STRTODG_FOR_PCC
		if (fpi->sudden_underflow) {
			Bfree(b);
			goto retz;
			}
#endif
		irv = STRTOG_Denormal;
		n = fpi->emin - e;
		if (n >= nbits) {
			switch (fpi->rounding) {
			  case FPI_Round_near:
				if (n == nbits && (n < 2 || any_on(b,n-1)))
					goto one_bit;
				break;
			  case FPI_Round_up:
				if (!sign)
					goto one_bit;
				break;
			  case FPI_Round_down:
				if (sign) {
 one_bit:
					x[0] = b->wds = 1;
 dret:
					*bp = b;
					*exp = fpi->emin;
#ifndef NO_ERRNO
					errno = ERANGE;
#endif
					return STRTOG_Denormal | STRTOG_Inexhi
						| STRTOG_Underflow;
					}
			  }
			Bfree(b);
 retz:
#ifndef NO_ERRNO
			errno = ERANGE;
#endif
			return STRTOG_Zero | STRTOG_Inexlo | STRTOG_Underflow;
			}
		k = n - 1;
		if (lostbits)
			lostbits = 1;
		else if (k > 0)
			lostbits = any_on(b,k);
		if (x[k>>kshift] & 1 << (k & kmask))
			lostbits |= 2;
		nbits -= n;
		rshift(b,n);
		e = fpi->emin;
		}
	if (lostbits) {
		up = 0;
		switch(fpi->rounding) {
		  case FPI_Round_zero:
			break;
		  case FPI_Round_near:
			if (lostbits & 2
			 && (lostbits | x[0]) & 1)
				up = 1;
			break;
		  case FPI_Round_up:
			up = 1 - sign;
			break;
		  case FPI_Round_down:
			up = sign;
		  }
		if (up) {
			k = b->wds;
			b = increment(b);
			x = b->x;
			if (irv == STRTOG_Denormal) {
				if (nbits == fpi->nbits - 1
				 && x[nbits >> kshift] & 1 << (nbits & kmask))
					irv =  STRTOG_Normal;
				}
			else if (b->wds > k
			 || ((n = nbits & kmask) !=0
			      && hi0bits(x[k-1]) < 32-n)) {
				rshift(b,1);
				if (++e > fpi->emax)
					goto ovfl;
				}
			irv |= STRTOG_Inexhi;
			}
		else
			irv |= STRTOG_Inexlo;
		}
	*bp = b;
	*exp = e;
	return irv;
	}

/* strtodg.c */
#ifdef USE_LOCALE
#include "locale.h"
#endif

 static CONST int
fivesbits[] = {	 0,  3,  5,  7, 10, 12, 14, 17, 19, 21,
		24, 26, 28, 31, 33, 35, 38, 40, 42, 45,
		47, 49, 52
#ifdef VAX
		, 54, 56
#endif
		};

 Bigint *
#ifdef KR_headers
increment(b) Bigint *b;
#else
increment(Bigint *b)
#endif
{
	ULong *x, *xe;
	Bigint *b1;
#ifdef Pack_16
	ULong carry = 1, y;
#endif

	x = b->x;
	xe = x + b->wds;
#ifdef Pack_32
	do {
		if (*x < (ULong)0xffffffffL) {
			++*x;
			return b;
			}
		*x++ = 0;
		} while(x < xe);
#else
	do {
		y = *x + carry;
		carry = y >> 16;
		*x++ = y & 0xffff;
		if (!carry)
			return b;
		} while(x < xe);
	if (carry)
#endif
	{
		if (b->wds >= b->maxwds) {
			b1 = Balloc(b->k+1);
			Bcopy(b1,b);
			Bfree(b);
			b = b1;
			}
		b->x[b->wds++] = 1;
		}
	return b;
	}

 void
#ifdef KR_headers
decrement(b) Bigint *b;
#else
decrement(Bigint *b)
#endif
{
	ULong *x, *xe;
#ifdef Pack_16
	ULong borrow = 1, y;
#endif

	x = b->x;
	xe = x + b->wds;
#ifdef Pack_32
	do {
		if (*x) {
			--*x;
			break;
			}
		*x++ = 0xffffffffL;
		}
		while(x < xe);
#else
	do {
		y = *x - borrow;
		borrow = (y & 0x10000) >> 16;
		*x++ = y & 0xffff;
		} while(borrow && x < xe);
#endif
	}

 static int
#ifdef KR_headers
all_on(b, n) Bigint *b; int n;
#else
all_on(Bigint *b, int n)
#endif
{
	ULong *x, *xe;

	x = b->x;
	xe = x + (n >> kshift);
	while(x < xe)
		if ((*x++ & ALL_ON) != ALL_ON)
			return 0;
	if (n &= kmask)
		return ((*x | (ALL_ON << n)) & ALL_ON) == ALL_ON;
	return 1;
	}

 Bigint *
#ifdef KR_headers
set_ones(b, n) Bigint *b; int n;
#else
set_ones(Bigint *b, int n)
#endif
{
	int k;
	ULong *x, *xe;

	k = (n + ((1 << kshift) - 1)) >> kshift;
	if (b->k < k) {
		Bfree(b);
		b = Balloc(k);
		}
	k = n >> kshift;
	if (n &= kmask)
		k++;
	b->wds = k;
	x = b->x;
	xe = x + k;
	while(x < xe)
		*x++ = ALL_ON;
	if (n)
		x[-1] >>= ULbits - n;
	return b;
	}

 static int
rvOK
#ifdef KR_headers
 (d, fpi, exp, bits, exact, rd, irv)
 U *d; FPI *fpi; Long *exp; ULong *bits; int exact, rd, *irv;
#else
 (U *d, FPI *fpi, Long *exp, ULong *bits, int exact, int rd, int *irv)
#endif
{
	Bigint *b;
	ULong carry, inex, lostbits;
	int bdif, e, j, k, k1, nb, rv;

	carry = rv = 0;
	b = d2b(dval(d), &e, &bdif);
	bdif -= nb = fpi->nbits;
	e += bdif;
	if (bdif <= 0) {
		if (exact)
			goto trunc;
		goto ret;
		}
	if (P == nb) {
		if (
#ifndef IMPRECISE_INEXACT
			exact &&
#endif
			fpi->rounding ==
#ifdef RND_PRODQUOT
					FPI_Round_near
#else
					Flt_Rounds
#endif
			) goto trunc;
		goto ret;
		}
	switch(rd) {
	  case 1: /* round down (toward -Infinity) */
		goto trunc;
	  case 2: /* round up (toward +Infinity) */
		break;
	  default: /* round near */
		k = bdif - 1;
		if (k < 0)
			goto trunc;
		if (!k) {
			if (!exact)
				goto ret;
			if (b->x[0] & 2)
				break;
			goto trunc;
			}
		if (b->x[k>>kshift] & ((ULong)1 << (k & kmask)))
			break;
		goto trunc;
	  }
	/* "break" cases: round up 1 bit, then truncate; bdif > 0 */
	carry = 1;
 trunc:
	inex = lostbits = 0;
	if (bdif > 0) {
		if ( (lostbits = any_on(b, bdif)) !=0)
			inex = STRTOG_Inexlo;
		rshift(b, bdif);
		if (carry) {
			inex = STRTOG_Inexhi;
			b = increment(b);
			if ( (j = nb & kmask) !=0)
				j = ULbits - j;
			if (hi0bits(b->x[b->wds - 1]) != j) {
				if (!lostbits)
					lostbits = b->x[0] & 1;
				rshift(b, 1);
				e++;
				}
			}
		}
	else if (bdif < 0)
		b = lshift(b, -bdif);
	if (e < fpi->emin) {
		k = fpi->emin - e;
		e = fpi->emin;
		if (k > nb || fpi->sudden_underflow) {
			b->wds = inex = 0;
			*irv = STRTOG_Underflow | STRTOG_Inexlo;
			}
		else {
			k1 = k - 1;
			if (k1 > 0 && !lostbits)
				lostbits = any_on(b, k1);
			if (!lostbits && !exact)
				goto ret;
			lostbits |=
			  carry = b->x[k1>>kshift] & (1 << (k1 & kmask));
			rshift(b, k);
			*irv = STRTOG_Denormal;
			if (carry) {
				b = increment(b);
				inex = STRTOG_Inexhi | STRTOG_Underflow;
				}
			else if (lostbits)
				inex = STRTOG_Inexlo | STRTOG_Underflow;
			}
		}
	else if (e > fpi->emax) {
		e = fpi->emax + 1;
		*irv = STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
#ifndef NO_ERRNO
		errno = ERANGE;
#endif
		b->wds = inex = 0;
		}
	*exp = e;
	copybits(bits, nb, b);
	*irv |= inex;
	rv = 1;
 ret:
	Bfree(b);
	return rv;
	}

 static int
#ifdef KR_headers
mantbits(d) U *d;
#else
mantbits(U *d)
#endif
{
	ULong L;
#ifdef VAX
	L = word1(d) << 16 | word1(d) >> 16;
	if (L)
#else
	if ( (L = word1(d)) !=0)
#endif
		return P - lo0bits(&L);
#ifdef VAX
	L = word0(d) << 16 | word0(d) >> 16 | Exp_msk11;
#else
	L = word0(d) | Exp_msk1;
#endif
	return P - 32 - lo0bits(&L);
	}

 int
strtodg
#ifdef KR_headers
	(s00, se, fpi, exp, bits)
	CONST char *s00; char **se; FPI *fpi; Long *exp; ULong *bits;
#else
	(CONST char *s00, char **se, FPI *fpi, Long *exp, ULong *bits)
#endif
{
	int abe, abits, asub;
	int bb0, bb2, bb5, bbe, bd2, bd5, bbbits, bs2, c, decpt, denorm;
	int dsign, e, e1, e2, emin, esign, finished, i, inex, irv;
	int j, k, nbits, nd, nd0, nf, nz, nz0, rd, rvbits, rve, rve1, sign;
	int sudden_underflow = 0; /* silence compilers */
	CONST char *s, *s0, *s1;
	double adj0, tol;
	Long L;
	U adj, rv;
	ULong *b, *be, y, z;
	Bigint *ab, *bb, *bb1, *bd, *bd0, *bs, *delta, *rvb, *rvb0;
#ifdef USE_LOCALE /*{{*/
#ifdef NO_LOCALE_CACHE
	char *decimalpoint = localeconv()->decimal_point;
	int dplen = strlen(decimalpoint);
#else
	char *decimalpoint;
	static char *decimalpoint_cache;
	static int dplen;
	if (!(s0 = decimalpoint_cache)) {
		s0 = localeconv()->decimal_point;
		if ((decimalpoint_cache = (char*)MALLOC(strlen(s0) + 1))) {
			strcpy(decimalpoint_cache, s0);
			s0 = decimalpoint_cache;
			}
		dplen = strlen(s0);
		}
	decimalpoint = (char*)s0;
#endif /*NO_LOCALE_CACHE*/
#else  /*USE_LOCALE}{*/
#define dplen 1
#endif /*USE_LOCALE}}*/

	irv = STRTOG_Zero;
	denorm = sign = nz0 = nz = 0;
	dval(&rv) = 0.;
	rvb = 0;
	nbits = fpi->nbits;
	for(s = s00;;s++) switch(*s) {
		case '-':
			sign = 1;
			/* no break */
		case '+':
			if (*++s)
				goto break2;
			/* no break */
		case 0:
			sign = 0;
			irv = STRTOG_NoNumber;
			s = s00;
			goto ret;
		case '\t':
		case '\n':
		case '\v':
		case '\f':
		case '\r':
		case ' ':
			continue;
		default:
			goto break2;
		}
 break2:
	if (*s == '0') {
#ifndef NO_HEX_FP
		switch(s[1]) {
		  case 'x':
		  case 'X':
			irv = gethex(&s, fpi, exp, &rvb, sign);
			if (irv == STRTOG_NoNumber) {
				s = s00;
				sign = 0;
				}
			goto ret;
		  }
#endif
		nz0 = 1;
		while(*++s == '0') ;
		if (!*s)
			goto ret;
		}
	sudden_underflow = fpi->sudden_underflow;
	s0 = s;
	y = z = 0;
	for(decpt = nd = nf = 0; (c = *s) >= '0' && c <= '9'; nd++, s++)
		if (nd < 9)
			y = 10*y + c - '0';
		else if (nd < DBL_DIG + 2)
			z = 10*z + c - '0';
	nd0 = nd;
#ifdef USE_LOCALE
	if (c == *decimalpoint) {
		for(i = 1; decimalpoint[i]; ++i)
			if (s[i] != decimalpoint[i])
				goto dig_done;
		s += i;
		c = *s;
#else
	if (c == '.') {
		c = *++s;
#endif
		decpt = 1;
		if (!nd) {
			for(; c == '0'; c = *++s)
				nz++;
			if (c > '0' && c <= '9') {
				s0 = s;
				nf += nz;
				nz = 0;
				goto have_dig;
				}
			goto dig_done;
			}
		for(; c >= '0' && c <= '9'; c = *++s) {
 have_dig:
			nz++;
			if (c -= '0') {
				nf += nz;
				for(i = 1; i < nz; i++)
					if (nd++ < 9)
						y *= 10;
					else if (nd <= DBL_DIG + 2)
						z *= 10;
				if (nd++ < 9)
					y = 10*y + c;
				else if (nd <= DBL_DIG + 2)
					z = 10*z + c;
				nz = 0;
				}
			}
		}/*}*/
 dig_done:
	e = 0;
	if (c == 'e' || c == 'E') {
		if (!nd && !nz && !nz0) {
			irv = STRTOG_NoNumber;
			s = s00;
			goto ret;
			}
		s00 = s;
		esign = 0;
		switch(c = *++s) {
			case '-':
				esign = 1;
			case '+':
				c = *++s;
			}
		if (c >= '0' && c <= '9') {
			while(c == '0')
				c = *++s;
			if (c > '0' && c <= '9') {
				L = c - '0';
				s1 = s;
				while((c = *++s) >= '0' && c <= '9')
					L = 10*L + c - '0';
				if (s - s1 > 8 || L > 19999)
					/* Avoid confusion from exponents
					 * so large that e might overflow.
					 */
					e = 19999; /* safe for 16 bit ints */
				else
					e = (int)L;
				if (esign)
					e = -e;
				}
			else
				e = 0;
			}
		else
			s = s00;
		}
	if (!nd) {
		if (!nz && !nz0) {
#ifdef INFNAN_CHECK
			/* Check for Nan and Infinity */
			if (!decpt)
			 switch(c) {
			  case 'i':
			  case 'I':
				if (match(&s,"nf")) {
					--s;
					if (!match(&s,"inity"))
						++s;
					irv = STRTOG_Infinite;
					goto infnanexp;
					}
				break;
			  case 'n':
			  case 'N':
				if (match(&s, "an")) {
					irv = STRTOG_NaN;
					*exp = fpi->emax + 1;
#ifndef No_Hex_NaN
					if (*s == '(') /*)*/
						irv = hexnan(&s, fpi, bits);
#endif
					goto infnanexp;
					}
			  }
#else
			(void)decpt; /* shut up compiler! */
#endif /* INFNAN_CHECK */
			irv = STRTOG_NoNumber;
			s = s00;
			}
		goto ret;
		}

	irv = STRTOG_Normal;
	e1 = e -= nf;
	rd = 0;
	switch(fpi->rounding & 3) {
	  case FPI_Round_up:
		rd = 2 - sign;
		break;
	  case FPI_Round_zero:
		rd = 1;
		break;
	  case FPI_Round_down:
		rd = 1 + sign;
	  }

	/* Now we have nd0 digits, starting at s0, followed by a
	 * decimal point, followed by nd-nd0 digits.  The number we're
	 * after is the integer represented by those digits times
	 * 10**e */

	if (!nd0)
		nd0 = nd;
	k = nd < DBL_DIG + 2 ? nd : DBL_DIG + 2;
	dval(&rv) = y;
	if (k > 9)
		dval(&rv) = tens[k - 9] * dval(&rv) + z;
	bd0 = 0;
	if (nbits <= P && nd <= DBL_DIG) {
		if (!e) {
			if (rvOK(&rv, fpi, exp, bits, 1, rd, &irv))
				goto ret;
			}
		else if (e > 0) {
			if (e <= Ten_pmax) {
#ifdef VAX
				goto vax_ovfl_check;
#else
				i = fivesbits[e] + mantbits(&rv) <= P;
				/* rv = */ rounded_product(dval(&rv), tens[e]);
				if (rvOK(&rv, fpi, exp, bits, i, rd, &irv))
					goto ret;
				e1 -= e;
				goto rv_notOK;
#endif
				}
			i = DBL_DIG - nd;
			if (e <= Ten_pmax + i) {
				/* A fancier test would sometimes let us do
				 * this for larger i values.
				 */
				e2 = e - i;
				e1 -= i;
				dval(&rv) *= tens[i];
#ifdef VAX
				/* VAX exponent range is so narrow we must
				 * worry about overflow here...
				 */
 vax_ovfl_check:
				dval(&adj) = dval(&rv);
				word0(&adj) -= P*Exp_msk1;
				/* adj = */ rounded_product(dval(&adj), tens[e2]);
				if ((word0(&adj) & Exp_mask)
				 > Exp_msk1*(DBL_MAX_EXP+Bias-1-P))
					goto rv_notOK;
				word0(&adj) += P*Exp_msk1;
				dval(&rv) = dval(&adj);
#else
				/* rv = */ rounded_product(dval(&rv), tens[e2]);
#endif
				if (rvOK(&rv, fpi, exp, bits, 0, rd, &irv))
					goto ret;
				e1 -= e2;
				}
			}
#ifndef Inaccurate_Divide
		else if (e >= -Ten_pmax) {
			/* rv = */ rounded_quotient(dval(&rv), tens[-e]);
			if (rvOK(&rv, fpi, exp, bits, 0, rd, &irv))
				goto ret;
			e1 -= e;
			}
#endif
		}
 rv_notOK:
	e1 += nd - k;

	/* Get starting approximation = rv * 10**e1 */

	e2 = 0;
	if (e1 > 0) {
		if ( (i = e1 & 15) !=0)
			dval(&rv) *= tens[i];
		if (e1 &= ~15) {
			e1 >>= 4;
			while(e1 >= (1 << (n_bigtens-1))) {
				e2 += ((word0(&rv) & Exp_mask)
					>> Exp_shift1) - Bias;
				word0(&rv) &= ~Exp_mask;
				word0(&rv) |= Bias << Exp_shift1;
				dval(&rv) *= bigtens[n_bigtens-1];
				e1 -= 1 << (n_bigtens-1);
				}
			e2 += ((word0(&rv) & Exp_mask) >> Exp_shift1) - Bias;
			word0(&rv) &= ~Exp_mask;
			word0(&rv) |= Bias << Exp_shift1;
			for(j = 0; e1 > 0; j++, e1 >>= 1)
				if (e1 & 1)
					dval(&rv) *= bigtens[j];
			}
		}
	else if (e1 < 0) {
		e1 = -e1;
		if ( (i = e1 & 15) !=0)
			dval(&rv) /= tens[i];
		if (e1 &= ~15) {
			e1 >>= 4;
			while(e1 >= (1 << (n_bigtens-1))) {
				e2 += ((word0(&rv) & Exp_mask)
					>> Exp_shift1) - Bias;
				word0(&rv) &= ~Exp_mask;
				word0(&rv) |= Bias << Exp_shift1;
				dval(&rv) *= tinytens[n_bigtens-1];
				e1 -= 1 << (n_bigtens-1);
				}
			e2 += ((word0(&rv) & Exp_mask) >> Exp_shift1) - Bias;
			word0(&rv) &= ~Exp_mask;
			word0(&rv) |= Bias << Exp_shift1;
			for(j = 0; e1 > 0; j++, e1 >>= 1)
				if (e1 & 1)
					dval(&rv) *= tinytens[j];
			}
		}
#ifdef IBM
	/* e2 is a correction to the (base 2) exponent of the return
	 * value, reflecting adjustments above to avoid overflow in the
	 * native arithmetic.  For native IBM (base 16) arithmetic, we
	 * must multiply e2 by 4 to change from base 16 to 2.
	 */
	e2 <<= 2;
#endif
	rvb = d2b(dval(&rv), &rve, &rvbits);	/* rv = rvb * 2^rve */
	rve += e2;
	if ((j = rvbits - nbits) > 0) {
		rshift(rvb, j);
		rvbits = nbits;
		rve += j;
		}
	bb0 = 0;	/* trailing zero bits in rvb */
	e2 = rve + rvbits - nbits;
	if (e2 > fpi->emax + 1)
		goto huge;
	rve1 = rve + rvbits - nbits;
	if (e2 < (emin = fpi->emin)) {
		denorm = 1;
		j = rve - emin;
		if (j > 0) {
			rvb = lshift(rvb, j);
			rvbits += j;
			}
		else if (j < 0) {
			rvbits += j;
			if (rvbits <= 0) {
				if (rvbits < -1) {
 ufl:
					rvb->wds = 0;
					rvb->x[0] = 0;
					*exp = emin;
					irv = STRTOG_Underflow | STRTOG_Inexlo;
					goto ret;
					}
				rvb->x[0] = rvb->wds = rvbits = 1;
				}
			else
				rshift(rvb, -j);
			}
		rve = rve1 = emin;
		if (sudden_underflow && e2 + 1 < emin)
			goto ufl;
		}

	/* Now the hard part -- adjusting rv to the correct value.*/

	/* Put digits into bd: true value = bd * 10^e */

	bd0 = s2b(s0, nd0, nd, y, dplen);

	for(;;) {
		bd = Balloc(bd0->k);
		Bcopy(bd, bd0);
		bb = Balloc(rvb->k);
		Bcopy(bb, rvb);
		bbbits = rvbits - bb0;
		bbe = rve + bb0;
		bs = i2b(1);

		if (e >= 0) {
			bb2 = bb5 = 0;
			bd2 = bd5 = e;
			}
		else {
			bb2 = bb5 = -e;
			bd2 = bd5 = 0;
			}
		if (bbe >= 0)
			bb2 += bbe;
		else
			bd2 -= bbe;
		bs2 = bb2;
		j = nbits + 1 - bbbits;
		i = bbe + bbbits - nbits;
		if (i < emin)	/* denormal */
			j += i - emin;
		bb2 += j;
		bd2 += j;
		i = bb2 < bd2 ? bb2 : bd2;
		if (i > bs2)
			i = bs2;
		if (i > 0) {
			bb2 -= i;
			bd2 -= i;
			bs2 -= i;
			}
		if (bb5 > 0) {
			bs = pow5mult(bs, bb5);
			bb1 = mult(bs, bb);
			Bfree(bb);
			bb = bb1;
			}
		bb2 -= bb0;
		if (bb2 > 0)
			bb = lshift(bb, bb2);
		else if (bb2 < 0)
			rshift(bb, -bb2);
		if (bd5 > 0)
			bd = pow5mult(bd, bd5);
		if (bd2 > 0)
			bd = lshift(bd, bd2);
		if (bs2 > 0)
			bs = lshift(bs, bs2);
		asub = 1;
		inex = STRTOG_Inexhi;
		delta = diff(bb, bd);
		if (delta->wds <= 1 && !delta->x[0])
			break;
		dsign = delta->sign;
		delta->sign = finished = 0;
		L = 0;
		i = cmp(delta, bs);
		if (rd && i <= 0) {
			irv = STRTOG_Normal;
			if ( (finished = dsign ^ (rd&1)) !=0) {
				if (dsign != 0) {
					irv |= STRTOG_Inexhi;
					goto adj1;
					}
				irv |= STRTOG_Inexlo;
				if (rve1 == emin)
					goto adj1;
				for(i = 0, j = nbits; j >= ULbits;
						i++, j -= ULbits) {
					if (rvb->x[i] & ALL_ON)
						goto adj1;
					}
				if (j > 1 && lo0bits(rvb->x + i) < j - 1)
					goto adj1;
				rve = rve1 - 1;
				rvb = set_ones(rvb, rvbits = nbits);
				break;
				}
			irv |= dsign ? STRTOG_Inexlo : STRTOG_Inexhi;
			break;
			}
		if (i < 0) {
			/* Error is less than half an ulp -- check for
			 * special case of mantissa a power of two.
			 */
			irv = dsign
				? STRTOG_Normal | STRTOG_Inexlo
				: STRTOG_Normal | STRTOG_Inexhi;
			if (dsign || bbbits > 1 || denorm || rve1 == emin)
				break;
			delta = lshift(delta,1);
			if (cmp(delta, bs) > 0) {
				irv = STRTOG_Normal | STRTOG_Inexlo;
				goto drop_down;
				}
			break;
			}
		if (i == 0) {
			/* exactly half-way between */
			if (dsign) {
				if (denorm && all_on(rvb, rvbits)) {
					/*boundary case -- increment exponent*/
					rvb->wds = 1;
					rvb->x[0] = 1;
					rve = emin + nbits - (rvbits = 1);
					irv = STRTOG_Normal | STRTOG_Inexhi;
					denorm = 0;
					break;
					}
				irv = STRTOG_Normal | STRTOG_Inexlo;
				}
			else if (bbbits == 1) {
				irv = STRTOG_Normal;
 drop_down:
				/* boundary case -- decrement exponent */
				if (rve1 == emin) {
					irv = STRTOG_Normal | STRTOG_Inexhi;
					if (rvb->wds == 1 && rvb->x[0] == 1)
						sudden_underflow = 1;
					break;
					}
				rve -= nbits;
				rvb = set_ones(rvb, rvbits = nbits);
				break;
				}
			else
				irv = STRTOG_Normal | STRTOG_Inexhi;
			if ((bbbits < nbits && !denorm) || !(rvb->x[0] & 1))
				break;
			if (dsign) {
				rvb = increment(rvb);
				j = kmask & (ULbits - (rvbits & kmask));
				if (hi0bits(rvb->x[rvb->wds - 1]) != j)
					rvbits++;
				irv = STRTOG_Normal | STRTOG_Inexhi;
				}
			else {
				if (bbbits == 1)
					goto undfl;
				decrement(rvb);
				irv = STRTOG_Normal | STRTOG_Inexlo;
				}
			break;
			}
		if ((dval(&adj) = ratio(delta, bs)) <= 2.) {
 adj1:
			inex = STRTOG_Inexlo;
			if (dsign) {
				asub = 0;
				inex = STRTOG_Inexhi;
				}
			else if (denorm && bbbits <= 1) {
 undfl:
				rvb->wds = 0;
				rve = emin;
				irv = STRTOG_Underflow | STRTOG_Inexlo;
				break;
				}
			adj0 = dval(&adj) = 1.;
			}
		else {
			adj0 = dval(&adj) *= 0.5;
			if (dsign) {
				asub = 0;
				inex = STRTOG_Inexlo;
				}
			if (dval(&adj) < 2147483647.) {
				L = adj0;
				adj0 -= L;
				switch(rd) {
				  case 0:
					if (adj0 >= .5)
						goto inc_L;
					break;
				  case 1:
					if (asub && adj0 > 0.)
						goto inc_L;
					break;
				  case 2:
					if (!asub && adj0 > 0.) {
 inc_L:
						L++;
						inex = STRTOG_Inexact - inex;
						}
				  }
				dval(&adj) = L;
				}
			}
		y = rve + rvbits;

		/* adj *= ulp(dval(&rv)); */
		/* if (asub) rv -= adj; else rv += adj; */

		if (!denorm && rvbits < nbits) {
			rvb = lshift(rvb, j = nbits - rvbits);
			rve -= j;
			rvbits = nbits;
			}
		ab = d2b(dval(&adj), &abe, &abits);
		if (abe < 0)
			rshift(ab, -abe);
		else if (abe > 0)
			ab = lshift(ab, abe);
		rvb0 = rvb;
		if (asub) {
			/* rv -= adj; */
			j = hi0bits(rvb->x[rvb->wds-1]);
			rvb = diff(rvb, ab);
			k = rvb0->wds - 1;
			if (denorm)
				/* do nothing */;
			else if (rvb->wds <= k
				|| hi0bits( rvb->x[k]) >
				   hi0bits(rvb0->x[k])) {
				/* unlikely; can only have lost 1 high bit */
				if (rve1 == emin) {
					--rvbits;
					denorm = 1;
					}
				else {
					rvb = lshift(rvb, 1);
					--rve;
					--rve1;
					L = finished = 0;
					}
				}
			}
		else {
			rvb = sum(rvb, ab);
			k = rvb->wds - 1;
			if (k >= rvb0->wds
			 || hi0bits(rvb->x[k]) < hi0bits(rvb0->x[k])) {
				if (denorm) {
					if (++rvbits == nbits)
						denorm = 0;
					}
				else {
					rshift(rvb, 1);
					rve++;
					rve1++;
					L = 0;
					}
				}
			}
		Bfree(ab);
		Bfree(rvb0);
		if (finished)
			break;

		z = rve + rvbits;
		if (y == z && L) {
			/* Can we stop now? */
			tol = dval(&adj) * 5e-16; /* > max rel error */
			dval(&adj) = adj0 - .5;
			if (dval(&adj) < -tol) {
				if (adj0 > tol) {
					irv |= inex;
					break;
					}
				}
			else if (dval(&adj) > tol && adj0 < 1. - tol) {
				irv |= inex;
				break;
				}
			}
		bb0 = denorm ? 0 : trailz(rvb);
		Bfree(bb);
		Bfree(bd);
		Bfree(bs);
		Bfree(delta);
		}
	if (!denorm && (j = nbits - rvbits) != 0) {
		if (j > 0)
			rvb = lshift(rvb, j);
		else
			rshift(rvb, -j);
		rve -= j;
		}
	*exp = rve;
	Bfree(bb);
	Bfree(bd);
	Bfree(bs);
	Bfree(bd0);
	Bfree(delta);
	if (rve > fpi->emax) {
		switch(fpi->rounding & 3) {
		  case FPI_Round_near:
			goto huge;
		  case FPI_Round_up:
			if (!sign)
				goto huge;
			break;
		  case FPI_Round_down:
			if (sign)
				goto huge;
		  }
		/* Round to largest representable magnitude */
		Bfree(rvb);
		rvb = 0;
		irv = STRTOG_Normal | STRTOG_Inexlo;
		*exp = fpi->emax;
		b = bits;
		be = b + ((fpi->nbits + 31) >> 5);
		while(b < be)
			*b++ = (ULong)(-1);
		if ((j = fpi->nbits & 0x1f) != 0)
			*--be >>= (32 - j);
		goto ret;
 huge:
		rvb->wds = 0;
		irv = STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
#ifndef NO_ERRNO
		errno = ERANGE;
#endif
#ifdef INFNAN_CHECK
 infnanexp:
#endif
		*exp = fpi->emax + 1;
		}
 ret:
	if (denorm) {
		if (sudden_underflow) {
			rvb->wds = 0;
			irv = STRTOG_Underflow | STRTOG_Inexlo;
#ifndef NO_ERRNO
			errno = ERANGE;
#endif
			}
		else  {
			irv = (irv & ~STRTOG_Retmask) |
				(rvb->wds > 0 ? STRTOG_Denormal : STRTOG_Zero);
			if (irv & STRTOG_Inexact) {
				irv |= STRTOG_Underflow;
#ifndef NO_ERRNO
				errno = ERANGE;
#endif
				}
			}
		}
	if (se)
		*se = (char *)s;
	if (sign)
		irv |= STRTOG_Neg;
	if (rvb) {
		copybits(bits, nbits, rvb);
		Bfree(rvb);
		}
	return irv;
	}
#endif /*SOFTFLOAT*/
