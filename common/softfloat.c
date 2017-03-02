/*	$Id$	*/

/*
 * Copyright (c) 2008 Anders Magnusson. All rights reserved.
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

#ifndef NATIVE_FLOATING_POINT

#include "manifest.h"
#include "softfloat.h"

#ifndef PCC_DEBUG
#define assert(e) ((void)0)
#else
#define assert(e) (!(e)?cerror("assertion failed " #e " at softfloat:%d",__LINE__):(void)0)
#endif
/*
 * Floating point emulation, to not depend on the characteristics (and bugs)
 * of the host floating-point implementation when compiling.
 *
 * XXX - assumes that:
 *	- long long is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */

#ifdef FDFLOAT

/*
 * Supports F- and D-float, used in DEC machines.
 *
 * XXX - assumes that:
 *	- long long is (at least) 64 bits
 *	- int is at least 32 bits.
 *	- short is 16 bits.
 */

/*
 * Useful macros to manipulate the float.
 */
#define DSIGN(w)	(((w).fd1 >> 15) & 1)
#define DSIGNSET(w,s)	((w).fd1 = (s << 15) | ((w).fd1 & 077777))
#define DEXP(w)		(((w).fd1 >> 7) & 0377)
#define DEXPSET(w,e)	((w).fd1 = (((e) & 0377) << 7) | ((w).fd1 & 0100177))
#define DMANTH(w)	((w).fd1 & 0177)
#define DMANTHSET(w,m)	((w).fd1 = ((m) & 0177) | ((w).fd1 & 0177600))

typedef unsigned int lword;
typedef unsigned long long dword;

#define MAXMANT 0x100000000000000LL

/*
 * Returns a zero dfloat.
 */
static SF
nulldf(void)
{
	SF rv;

	rv.fd1 = rv.fd2 = rv.fd3 = rv.fd4 = 0;
	return rv;
}

/*
 * Convert a (u)longlong to dfloat.
 * XXX - fails on too large (> 55 bits) numbers.
 */
SF
soft_cast(CONSZ ll, TWORD t)
{
	int i;
	SF rv;

	rv = nulldf();
	if (ll == 0)
		return rv;  /* fp is zero */
	if (ll < 0)
		DSIGNSET(rv,1), ll = -ll;
	for (i = 0; ll > 0; i++, ll <<= 1)
		;
	DEXPSET(rv, 192-i);
	DMANTHSET(rv, ll >> 56);
	rv.fd2 = ll >> 40;
	rv.fd3 = ll >> 24;
	rv.fd4 = ll >> 8;
	return rv;
}

/*
 * multiply two dfloat. Use chop, not round.
 */
SF
soft_mul(SF p1, SF p2)
{
	SF rv;
	lword a1[2], a2[2], res[4];
	dword sum;

	res[0] = res[1] = res[2] = res[3] = 0;

	/* move mantissa into lwords */
	a1[0] = p1.fd4 | (p1.fd3 << 16);
	a1[1] = p1.fd2 | DMANTH(p1) << 16 | 0x800000;

	a2[0] = p2.fd4 | (p2.fd3 << 16);
	a2[1] = p2.fd2 | DMANTH(p2) << 16 | 0x800000;

#define MULONE(x,y,r) sum += (dword)a1[x] * (dword)a2[y]; sum += res[r]; \
	res[r] = sum; sum >>= 32;

	sum = 0;
	MULONE(0, 0, 0);
	MULONE(1, 0, 1);
	res[2] = sum;
	sum = 0;
	MULONE(0, 1, 1);
	MULONE(1, 1, 2);
	res[3] = sum;

	rv.fd1 = 0;
	DSIGNSET(rv, DSIGN(p1) ^ DSIGN(p2));
	DEXPSET(rv, DEXP(p1) + DEXP(p2) - 128);
	if (res[3] & 0x8000) {
		res[3] = (res[3] << 8) | (res[2] >> 24);
		res[2] = (res[2] << 8) | (res[1] >> 24);
	} else {
		DEXPSET(rv, DEXP(rv) - 1);
		res[3] = (res[3] << 9) | (res[2] >> 23);
		res[2] = (res[2] << 9) | (res[1] >> 23);
	}
	DMANTHSET(rv, res[3] >> 16);
	rv.fd2 = res[3];
	rv.fd3 = res[2] >> 16;
	rv.fd4 = res[2];
	return rv;
}

SF
soft_div(SF t, SF n)
{
	SF rv;
	dword T, N, K;
	int c;

#define SHL(x,b) ((dword)(x) << b)
	T = SHL(1,55) | SHL(DMANTH(t), 48) |
	    SHL(t.fd2, 32) | SHL(t.fd3, 16) | t.fd4;
	N = SHL(1,55) | SHL(DMANTH(n), 48) |
	    SHL(n.fd2, 32) | SHL(n.fd3, 16) | n.fd4;

	c = T > N;
	for (K = 0; (K & 0x80000000000000ULL) == 0; ) {
		if (T >= N) {
			T -= N;
			K |= 1;
		}
		T <<= 1;
		K <<= 1;
	}
	rv.fd1 = 0;
	DSIGNSET(rv, DSIGN(t) ^ DSIGN(n));
	DEXPSET(rv, DEXP(t) - DEXP(n) + 128 + c);
	DMANTHSET(rv, K >> 48);
	rv.fd2 = K >> 32;
	rv.fd3 = K >> 16;
	rv.fd4 = K;
	return rv;
}

/*
 * Negate a float number. Easy.
 */
SF
soft_neg(SF sf)
{
	int sign = DSIGN(sf) == 0;
	DSIGNSET(sf, sign);
	return sf;
}

/*
 * Return true if fp number is zero.
 */
int
soft_isz(SF sf)
{
	return (DEXP(sf) == 0);
}

int
soft_cmp_eq(SF x1, SF x2)
{
	cerror("soft_cmp_eq");
	return 0;
}

int
soft_cmp_ne(SF x1, SF x2)
{
	cerror("soft_cmp_ne");
	return 0;
}

int
soft_cmp_le(SF x1, SF x2)
{
	cerror("soft_cmp_le");
	return 0;
}

int
soft_cmp_lt(SF x1, SF x2)
{
	cerror("soft_cmp_lt");
	return 0;
}

int
soft_cmp_ge(SF x1, SF x2)
{
	cerror("soft_cmp_ge");
	return 0;
}

int
soft_cmp_gt(SF x1, SF x2)
{
	cerror("soft_cmp_gt");
	return 0;
}

/*
 * Convert a fp number to a CONSZ.
 */
CONSZ
soft_val(SF sf)
{
	CONSZ mant;
	int exp = DEXP(sf) - 128;

	mant = SHL(1,55) | SHL(DMANTH(sf), 48) |
            SHL(sf.fd2, 32) | SHL(sf.fd3, 16) | sf.fd4;

	while (exp < 0)
		mant >>= 1, exp++;
	while (exp > 0)
		mant <<= 1, exp--;
	return mant;
}

SF
soft_plus(SF x1, SF x2)
{
	cerror("soft_plus");
	return x1;
}

SF
soft_minus(SF x1, SF x2)
{
	cerror("soft_minus");
	return x1;
}

/*
 * Convert a hex constant to floating point number.
 */
NODE *
fhexcon(char *s)
{
	cerror("fhexcon");
	return NULL;
}

/*
 * Convert a floating-point constant to D-float and store it in a NODE.
 */
NODE *
floatcon(char *s)
{
	NODE *p;
	dword mant;
	SF fl, flexp, exp5;
	int exp, negexp, bexp;

	exp = 0;
	mant = 0;
#define ADDTO(sum, val) sum = sum * 10 + val - '0'
	for (; *s >= '0' && *s <= '9'; s++) {
		if (mant<MAXMANT)
			ADDTO(mant, *s);
		else
			exp++;
	}
	if (*s == '.') {
		for (s++; *s >= '0' && *s <= '9'; s++) {
			if (mant<MAXMANT) {
				ADDTO(mant, *s);
				exp--;
			}
		}
	}

	if ((*s == 'E') || (*s == 'e')) {
		int eexp = 0, sign = 0;
		s++;
		if (*s == '+')
			s++;
		else if (*s=='-')
			sign = 1, s++;

		for (; *s >= '0' && *s <= '9'; s++)
			ADDTO(eexp, *s);
		if (sign)
			eexp = -eexp;
		exp = exp + eexp;
	}

	negexp = 1;
	if (exp<0) {
		negexp = -1;
		exp = -exp;
	}


	flexp = soft_cast(1, INT);
	exp5 = soft_cast(5, INT);
	bexp = exp;
	fl = soft_cast(mant, INT);

	for (; exp; exp >>= 1) {
		if (exp&01)
			flexp = soft_mul(flexp, exp5);
		exp5 = soft_mul(exp5, exp5);
	}
	if (negexp<0)
		fl = soft_div(fl, flexp);
	else
		fl = soft_mul(fl, flexp);

	DEXPSET(fl, DEXP(fl) + negexp*bexp);
	p = block(FCON, NIL, NIL, DOUBLE, 0, 0); /* XXX type */
	p->n_dcon = fl;
	return p;
}
#else

/*
 * Use parametric floating-point representation, as used in the package gdtoa
 * published by David M. Gay and generally available as gdtoa.tgz at
 * http://www.netlib.org/fp/ ; see also strtodg.c introduction.
 *
 * Arithmetic characteristics are described in struct FPI (explained below);
 * the actual numbers are represented (stored) in struct SF.
 * Floating-point numbers have fpi->nbits bits.
 * These numbers are regarded as integers multiplied by 2^e
 * (i.e., 2 to the power of the exponent e), where e is stored in
 * *exp by strtodg.  The minimum and maximum exponent values fpi->emin
 * and fpi->emax for normalized floating-point numbers reflect this
 * arrangement.  For example, the IEEE 754 standard for binary arithmetic
 * specifies doubles (also known as binary64) as having 53 bits, with
 * normalized values of the form 1.xxxxx... times 2^(b-1023), with 52 bits
 * (the x's) and the biased exponent b represented explicitly;
 * b is an unsigned integer in the range 1 <= b <= 2046 for normalized
 * finite doubles, b = 0 for denormals, and b = 2047 for Infinities and NaNs.
 * To turn an IEEE double into the representation used here, we multiply
 * 1.xxxx... by 2^52 (to make it an integer) and reduce the exponent
 * e = (b-1023) by 52:
 *	fpi->emin = 1 - 1023 - 52
 *	fpi->emax = 1046 - 1023 - 52
 * For fpi_binary64 initialization, we actually write -53+1 rather than -52,
 * to emphasize that there are 53 bits including one implicit bit at the
 * left of the binary point.
 * Field fpi->rounding indicates the desired rounding direction, with
 * possible values
 *	FPI_Round_zero = toward 0,
 *	FPI_Round_near = unbiased rounding -- the IEEE default,
 *	FPI_Round_up = toward +Infinity, and
 *	FPI_Round_down = toward -Infinity
 *	FPI_Round_near_from0 = to nearest, ties always away from 0
 * given in pass1.h.
 *
 * Field fpi->sudden_underflow indicates whether computations should return
 * denormals or flush them to zero.  Normal floating-point numbers have
 * bit fpi->nbits in the significand on.  Denormals have it off, with
 * exponent = fpi->emin.
 *
 * Fields fpi->explicit_one, fpi->storage, and fpi->exp_bias are only
 * relevant when the numbers are finally packed into interchange format.
 * If bit 1 is the lowest in the significand, bit fpi->storage has the sign.
 *
 * Some architectures do not use IEEE arithmetic but can nevertheless use
 * the same parametrization. They should provide their own FPI objects.
 * Fields fpi->has_inf_nan and fpi->has_neg_zero cover the non-IEEE cases
 * of lacking respectively the use of infinities and NaN, and negative zero.
 * Field fpi->has_radix_16 is for architectures (IBM, DG Nova) with base 16.
 *
 * In this implementation, the bits are stored in one large integer
 * (unsigned long long); this limits the number of bits to 64.
 * Because of the storage representation (but not the implementation),
 * exponents are restricted to 15 bits.
 *
 * Furthermore, this implementation assumes that:
 *	- integers are (obviously) stored as 2's complement
 *	- long long is (at least) 64 bits
 *	- CONSZ is (at least) 64 bits
 * There are possible issues if int is 16 bits, with divisions.
 *
 * As a result, this is also a slightly restricted software implementation of
 * IEEE 754:1985 standard. Missing parts, useless in a compiler, are:
 * - no soft_unpack(), to convert from external binary{32,64} formats
 * - no precision control (not required, dropped in :2008)
 * - no soft_sqrt() operation
 * - no soft_remainder() operation
 * - no soft_rint(), _ceil, or _floor rounding operations
 * - no binary-to-decimal conversion (can use D. Gay's dgtoa.c)
 * - signaling NaNs (SF_NoNumber; should cause SFEXCP_Invalid on every op)
 * - XXX fenv-support is pending
 * - no soft_scalb(), _logb, or _nextafter optional functions
 * It should be easy to expand it into a complete implementation.
 *
 * The operations are rounded according to the indicated type.
 * If because of FLT_EVAL_METHOD, the precision has to be greater,
 * this should be handled by the calling code.
 */

/*
 * API restrictions:
 *	- type information should be between FLOAT and LDOUBLE
 */

#ifndef Long
#define Long int
#endif
#ifndef ULong
typedef unsigned Long ULong;
#endif
#ifndef ULLong
typedef unsigned long long ULLong;
#endif

#define ONEZEROES(n)	(1ull << (n))
#define ONES(n) 	(ONEZEROES(n) | (ONEZEROES(n)-1))

#define WORKBITS	64
#define NORMALMANT	ONEZEROES(WORKBITS-1)

#define SFNORMALIZE(sf)					\
	while ((sf).significand < NORMALMANT)		\
		(sf).significand <<= 1, (sf).exponent--;\
	if (((sf).kind & SF_kmask) == SF_Denormal)	\
		(sf).kind -= SF_Denormal - SF_Normal;

#define SFNEG(sf)	((sf).kind ^= SF_Neg, sf)
#define SFCOPYSIGN(sf, src)	\
	((sf).kind ^= ((sf).kind & SF_Neg) ^ ((src).kind & SF_Neg), (sf))
#define SFISZ(sf)	(((sf).kind & SF_kmask) == SF_Zero)
#define SFISINF(sf)	(((sf).kind & SF_kmask) == SF_Infinite)
#define SFISNAN(sf)	(((sf).kind & SF_kmask) >= SF_NaN)

typedef struct DULLong {
	ULLong hi, lo;
} DULLong;

static DULLong rshiftdro(ULLong, ULLong, int);
static DULLong lshiftd(ULLong, ULLong, int);
static SF sfround(SF, ULLong, TWORD);
#define SFROUND(sf,t)	(sfround(sf, 0, t))
static SF sfadd(SF, SF, TWORD);
static SF sfsub(SF, SF, TWORD);

#ifndef NO_COMPLEX
static SF finitemma(SF, SF, int, SF, SF, ULLong *);
static SF sfddiv(SF, ULLong, SF, ULLong, TWORD);
#endif

extern int strtodg (const char*, char**, FPI*, Long*, ULong*);

/* IEEE binary formats, and their interchange format encodings */
FPI fpi_binary16 = { 11, 1-15-11+1,
                        30-15-11+1, 1, 0,
        0, 1, 1, 0,  16,   15+11-1 };
FPI fpi_binary32 = { 24,  1-127-24+1,
                        254-127-24+1, 1, 0,
        0, 1, 1, 0,  32,    127+24-1 };
FPI fpi_binary64 = { 53,   1-1023-53+1,
                        2046-1023-53+1, 1, 0,
        0, 1, 1, 0,  64,     1023+53-1 };
#ifndef notyet
FPI fpi_binary128 = { 113,   1-16383-113+1,
                         32766-16383-113+1, 1, 0,
        0, 1, 1, 0,   128,     16383+113-1 };
#endif
/* IEEE double extended in its usual form, for example Intel 387 */
FPI fpi_binaryx80 = { 64,   1-16383-64+1,
                        32766-16383-64+1, 1, 0,
        1, 1, 1, 0,   80,     16383+64-1 };

#ifndef FPI_FLOAT
#define FPI_FLOAT	fpi_binary32
#endif
#ifndef FPI_DOUBLE
#define FPI_DOUBLE	fpi_binary64
#endif
#ifndef FPI_LDOUBLE
#define FPI_LDOUBLE	FPI_DOUBLE
#endif

FPI * fpis[3] = {
	&FPI_FLOAT,
	&FPI_DOUBLE,
	&FPI_LDOUBLE
};

/*
 * Constant rounding control mode (cf. TS 18661 clause 11).
 * Default is to have no effect. Set through #pragma STDC FENV_ROUND
 */
int sf_constrounding = FPI_RoundNotSet;
/*
 * Exceptions raised by functions which do not return a SF; behave like errno
 */
int sf_exceptions = 0;

/*
 * Returns a (signed) zero softfloat.
 * "kind" (sign and exceptions) is merged into.
 */
SF
zerosf(int kind)
{
	SF rv;

	/*static_*/assert(SF_Zero == 0);
#if 0
	rv.kind &= ~SF_kmask;
#else
	rv.kind = SF_Zero | (kind & ~SF_kmask);
#endif
	rv.significand = rv.exponent = rv.kind = 0;
	return rv;
}

/*
 * Returns a (signed) infinite softfloat.
 * If the target does not support infinites, the "infinite" will propagate
 * until sfround() below, where it will be transformed to DBL_MAX and exception
 */
SF
infsf(int kind)
{
	SF rv;

	rv.kind = SF_Infinite | (kind & ~SF_kmask);
	rv.significand = NORMALMANT;
	rv.exponent = fpis[LDOUBLE-FLOAT]->emax + 1;
	return rv;
}

/*
 * Returns a NaN softfloat.
 */
SF
nansf(int kind)
{
	SF rv;

	rv.kind = SF_NaN | (kind & ~SF_kmask);
	rv.significand = 3 * ONEZEROES(WORKBITS-2);
	rv.exponent = fpis[LDOUBLE-FLOAT]->emax + 1;
	return rv;
}

/*
 * Returns a (signed) huge, perhaps infinite, softfloat.
 */
SF
hugesf(int kind, TWORD t)
{
	FPI *fpi = fpis[t-FLOAT];
	SF rv;

	if (fpi->has_inf_nan)
		return infsf(kind);
	rv.kind = (kind & ~SF_kmask) | SF_Normal;
	rv.significand = ONES(fpi->nbits - 1);
	rv.exponent = fpi->emax;
	return rv;
}

/*
 * The algorithms for the operations were derived from John Hauser's 
 * SoftFloat package (which is also used in libpcc/libsoftfloat.) 
 *
 * Results are rounded to odd ("jamming" in John Hauser's code)
 * in order to avoid double rounding errors. See
 *	Sylvie Boldo, Guillaume Melquiond, "When double rounding is odd",
 *	Research report No 2004-48, Normale Sup', Lyon, Nov 2004;
 *	http://www.ens-lyon.fr/LIP/Pub/Rapports/RR/RR2004/RR2004-48.pdf
 *	17th IMACS World Congress, Paris, Jul 2005; pp.11;
 *	http://hal.inria.fr/inria-00070603/document
 */

/*
 * Shift right double-sized, rounding to odd.
 */
static DULLong
rshiftdro(ULLong a, ULLong b, int count)
{
	struct DULLong z;
	assert(count >= 0);
	if ((unsigned)count >= 2*WORKBITS) {
		z.hi = 0;
		z.lo = (a != 0) | (b != 0);
	}
	else if (count >= WORKBITS) {
		z.hi = 0;
		z.lo = (a >> (count - WORKBITS)) | (b != 0);
	}
	else {
		z.hi = a >> count;
		z.lo = (a << (WORKBITS - count)) | (b >> count) | (b != 0);
	}
	return z;
}

/*
 * Shift left double-sized.
 */
static DULLong
lshiftd(ULLong a, ULLong b, int count)
{
	struct DULLong z;
	assert((unsigned)count < WORKBITS);
	z.hi = a << count;
	z.lo = (a >> (WORKBITS-count)) | (b << count);
	return z;
}

/*
 * Full-range multiply, result to double-sized.
 */
static DULLong
muld(ULLong a, ULLong b)
{
	struct DULLong z;
	ULong ahi, bhi;
	ULLong mid;

#define HALFWORKBITS	(WORKBITS/2)
#define LOWHALFMASK	ONES(WORKBITS-HALFWORKBITS)
	ahi = a >> HALFWORKBITS;
	a &= LOWHALFMASK;
	bhi = b >> HALFWORKBITS;
	b &= LOWHALFMASK;
	z.lo = a * b;
	a *= bhi;
	b *= ahi;
	mid = (z.lo >> HALFWORKBITS) + (a & LOWHALFMASK) + (b & LOWHALFMASK);
	z.lo &= LOWHALFMASK;
	z.lo |= (mid & LOWHALFMASK) << HALFWORKBITS;
	z.hi = (ULLong) ahi * bhi + (a >> HALFWORKBITS) +
		(b >> HALFWORKBITS) + (mid >> HALFWORKBITS);
	return z;
}

/*
 * Explicit cast into some floating-point format, and assigments.
 * Drop precision (rounding correctly) and clamp exponent in range.
 */
typedef int bool;

static SF
sfround(SF sf, ULLong extra, TWORD t)
{
	FPI *fpi;
	ULLong minmant, mant, maxmant;
	DULLong z;
	int exp = sf.exponent, excess, doinc, rd;

	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
	switch(sf.kind & SF_kmask) {
	  case SF_Zero:
		if (! fpi->has_neg_zero)
			sf.kind &= ~SF_Neg;
		/* FALLTHROUGH */
	  default:
/* XXX revise: SF_NaN escapes here, but what to do if !fpi->has_inf_nan */
		return sf;
	  case SF_Infinite:
		return fpi->has_inf_nan ? sf :
/* XXX revise: IEEE says overflow and invalid cannot be both, but this is for VAX... */
			hugesf(sf.kind | SFEXCP_Overflow | SFEXCP_Invalid, t);
	  case SF_NaNbits:
		cerror("Unexpected softfloat NaNbits");
		sf.kind -= SF_NaNbits - SF_NaN;
		return sf;
	  case SF_Denormal:
		if (exp != fpi->emin || extra) {
			/*static_*/assert(SF_Denormal > SF_Normal);
			sf.kind -= SF_Denormal - SF_Normal;
		}
		break;
	  case SF_Normal:
		break;
	}

	maxmant = ONES(fpi->nbits - 1);
	if (fpi->has_radix_16)
		minmant = ONEZEROES(fpi->nbits - 4);
	else
		minmant = ONEZEROES(fpi->nbits - 1);
	assert(sf.significand);
	excess = 0;
	if (sf.significand < minmant) {
		/* Not normalized */
		if ((sf.kind & SF_kmask) != SF_Denormal) {
			mant = sf.significand;
			for (; mant < minmant; --excess)
				mant <<= 1;
			if (fpi->has_radix_16 && (exp + excess) & 3)
				excess -= (exp + excess) & 3;
		}
	}
	else {
		if ((sf.kind & SF_kmask) == SF_Denormal)
			sf.kind -= SF_Denormal - SF_Normal;
		if ((sf.significand & ~maxmant) != 0) {
			/* More bits than allowed in significand */
			if (sf.significand & NORMALMANT)
				excess = WORKBITS - fpi->nbits;
			else {
				mant = sf.significand;
				for (; mant > maxmant; ++excess)
					mant >>= 1;
			}
		}
	}
	if (fpi->has_radix_16 && (exp + excess) & 3)
		excess += 4 - ((exp + excess) & 3);
	if (excess) {
		if (excess < 0)
			z = lshiftd(sf.significand, extra, -excess);
		else
			z = rshiftdro(sf.significand, extra, excess);
		sf.significand = z.hi;
		extra = z.lo;
		exp += excess;
	}
	assert((sf.kind & SF_kmask) == SF_Denormal ? sf.significand < minmant
	    : (sf.significand & ~(minmant-1)) == minmant || fpi->has_radix_16);

/* XXX check limit cases (emin, emin-1, emin-2) to avoid double rounding... */

	rd = fpi->rounding;
	if (sf_constrounding != FPI_RoundNotSet)
		rd = sf_constrounding;
	if (sf.kind & SF_Neg && rd == FPI_Round_down)
		rd = FPI_Round_up;
	else if (sf.kind & SF_Neg && rd == FPI_Round_up)
		rd = FPI_Round_down;
	if (extra != 0) {
		doinc = rd == FPI_Round_up;
		if (rd == FPI_Round_near && extra == NORMALMANT)
			doinc = sf.significand & 1;
		else if ((rd & 3) == FPI_Round_near && extra >= NORMALMANT)
			doinc = 1;
/* XXX set SFEXCP_Inex(hi|lo) ? later ? */
		if (doinc) {
			if (sf.significand == maxmant) {
				sf.significand = minmant;
				++exp;
			}
			else
				sf.significand++;
		}
	}
	else doinc = 0;

	if (exp < fpi->emin) {
/* XXX NO! IEEE754 says that if result is less than DBL_MIN but can be
 * represented exactly (as denormal), Underflow exception is NOT signaled.
 */
		sf.kind |= SFEXCP_Underflow;
		if (fpi->sudden_underflow || exp < fpi->emin - fpi->nbits)
			return zerosf(sf.kind | SFEXCP_Inexlo);
		if (doinc) {
			if (sf.significand == minmant) {
				sf.significand = maxmant;
				--exp;
			}
			else
				sf.significand--;
		}
		excess = fpi->emin - exp;
		z = rshiftdro(sf.significand, extra, excess);
/* XXX need to consider extra here... */
		doinc = rd == FPI_Round_up;
		if ((rd & 3) == FPI_Round_near) {
			if (z.lo > NORMALMANT)
				doinc = 1;
			else if (rd == FPI_Round_near && z.lo == NORMALMANT)
				doinc = z.hi & 1;
			else if (rd == FPI_Round_near_from0 && z.lo == NORMALMANT)
				doinc = 1;
		}
		if (doinc) z.hi++;
/* XXX revise: it seems there should be a missed case where the delivered result
 * was DBL_MIN-DBL_EPSILON, thus just below the bar, but when rounded to the lower
 * precision of denormals it should be rounded up to normal...
 * (in the exact half case, note extra==0)
 */
		if (z.hi) {
			sf.significand = z.hi;
			sf.kind += SF_Denormal - SF_Normal;
			sf.kind |= doinc ? SFEXCP_Inexhi : SFEXCP_Inexlo;
			exp = fpi->emin;
		}
		else {
			sf = zerosf(sf.kind | SFEXCP_Inexlo);
			exp = 0;
		}
	}
	else if (exp > fpi->emax) {
		sf.kind |= SFEXCP_Overflow | SFEXCP_Inexhi;
		if (! fpi->has_inf_nan || rd == FPI_Round_zero || 
		    rd == FPI_Round_down) {
			sf.significand = maxmant;
			exp = fpi->emax;
		}
		else {
			sf.kind = SF_Infinite | (sf.kind & ~SF_kmask);
			sf.significand = minmant;
			exp = fpi->emax+1;
		}
	}
	else if (extra)
		sf.kind = doinc ? SFEXCP_Inexhi : SFEXCP_Inexlo;
	sf.exponent = exp;
	return sf;
}

/*
 * Conversions.
 */

/*
 * Convert from integer type f to floating-point type t.
 * Rounds correctly to the target type.
 */
SF
soft_int2fp(CONSZ ll, TWORD f, TWORD t)
{
	SF rv;

	rv = zerosf(0);
	if (ll == 0)
		return rv;  /* fp is zero */
	rv.kind = SF_Normal;
	if (!ISUNSIGNED(f) && ll < 0)
		rv.kind |= SF_Neg, ll = -ll;
	rv.significand = ll; /* rv.exponent already 0 */
	return SFROUND(rv, t);
}

/*
 * Explicit cast into some floating-point format, and assigments.
 * Drop precision (rounding correctly) and clamp exponent in range.
 */
SF
soft_fp2fp(SF sf, TWORD t)
{
	return SFROUND(sf, t);
}

/*
 * Convert a fp number to a CONSZ. Always chop toward zero.
 * XXX Should warns somehow if out-of-range: in sf_exceptions
 */
CONSZ
soft_fp2int(SF sf, TWORD t)
{
	ULLong mant;
	int exp = sf.exponent;

	switch(sf.kind & SF_kmask) {
	  case SF_Zero:
	  case SF_Denormal:
		return 0;

	  case SF_Normal:
		if (exp < - WORKBITS - 1)
			return 0;
		if (exp < WORKBITS)
			break;
		/* FALLTHROUGH */
	  case SF_Infinite:
/* XXX revise */
		/* Officially entering undefined behaviour! */
		uerror("Conversion of huge FP constant into integer");
		/* FALLTHROUGH */

	  case SF_NoNumber:
	  default:
		/* Can it happen? Debug_Warns? ICE? */
		/* FALLTHROUGH */
	  case SF_NaN:
	  case SF_NaNbits:
		uerror("Undefined FP conversion into an integer, replaced with 0");
		return 0;
	}
	mant = sf.significand;
	while (exp < 0)
		mant >>= 1, exp++;
/* XXX if (exp > WORKBITS) useless (really SZ of CONSZ) */
	while (exp > 0)
/* XXX check overflow */
		mant <<= 1, exp--;
	if (sf.kind & SF_Neg)
		mant = -(long long)mant;
/* XXX sf_exceptions */
/* XXX check overflow for target type */
	return mant;
}

/*
 * Operations.
 */

/*
 * Negate a softfloat. Easy.
 * Do not work correctly for SF_Zero when negative zero are not supported
 * (need to have access to fpi-> (i.e. TWORD t) to solve this.) Not-a-problem
 */
SF
soft_neg(SF sf)
{
	sf.kind ^= SF_Neg;
	return sf;
}

/*
 * IEEE754 operations on sign bit. Always well defined, even with NaN.
 */
CONSZ
soft_signbit(SF sf)
{
	return sf.kind & SF_Neg;
}

SF
soft_copysign(SF sf, SF src)
{
	SFCOPYSIGN(sf, src);
	return sf;
}

/*
 * Add two numbers of same sign.
 * The devil is in the details, like those negative zeroes...
 */
static SF
sfadd(SF x1, SF x2, TWORD t)
{
	SF rv;
	DULLong z;
	int diff;

	if (SFISINF(x1))
		return x1;
	if (SFISINF(x2))
		return x2;
	if (SFISZ(x2)) /* catches all signed zeroes, delivering x1 */
		return x1;
	if (SFISZ(x1))
		return x2;
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	if (x1.exponent - WORKBITS > x2.exponent) {
		x1.kind |= SFEXCP_Inexlo;
		return x1;
	}
	if (x2.exponent - WORKBITS > x1.exponent) {
		x2.kind |= SFEXCP_Inexlo;
		return x2;
	}
	diff = x1.exponent - x2.exponent;
	if (diff < 0) {
		rv = x2;
		z = rshiftdro(x1.significand, 0, -diff );
	}
	else {
		rv = x1;
		z = rshiftdro(x2.significand, 0, diff );
	}
	rv.significand += z.hi;
	if (rv.significand < NORMALMANT) {
		/* target mantissa overflows */
		z = rshiftdro(rv.significand, z.lo, 1);
		rv.significand = z.hi | NORMALMANT;
		++rv.exponent;
	}
	return sfround(rv, z.lo, t);
}

/*
 * Substract two positive numbers.
 * Special hack when the type number t being negative, to indicate
 * that rounding rules should be reversed (because the operands were.)
 */
static SF
sfsub(SF x1, SF x2, TWORD t)
{
	SF rv;
	DULLong z;
	int diff;

	if (SFISINF(x1) && SFISINF(x2))
		return nansf(x1.kind | SFEXCP_Invalid);
	if (SFISINF(x1))
		return x1;
	if (SFISINF(x2))
		return SFNEG(x2);
	if (SFISZ(x2)) /* catches 0 - 0, delivering +0 */
		return x1;
	if (SFISZ(x1))
		return SFNEG(x2);
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	if (x1.exponent - WORKBITS > x2.exponent) {
		x1.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return x1;
	}
	if (x2.exponent - WORKBITS > x1.exponent) {
		x2.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return SFNEG(x2);
	}
	diff = x1.exponent - x2.exponent;
	if (diff == 0 && x1.significand == x2.significand) {
		if ((int)t < 0)
			t = -(int)t;
/* XXX sf_constrounding */
		if ((fpis[t-FLOAT]->rounding & 3) == FPI_Round_down)
			return zerosf(SF_Neg | (x1.kind & ~SF_Neg));
		return zerosf(x1.kind & ~SF_Neg); /* x1==x2==-0 done elsewhere */
	}
	if (diff < 0 || (diff == 0 && x1.significand < x2.significand)) {
		rv = SFNEG(x2);
		z = rshiftdro(x1.significand, 0, -diff );
	}
	else {
		rv = x1;
		z = rshiftdro(x2.significand, 0, diff );
	}
	if ((rv.kind & SF_kmask) == SF_Denormal)
		rv.kind -= SF_Denormal - SF_Normal;
	rv.significand -= z.hi;
	if ((int)t < 0) {
		int rd;

		t = -(int)t;
		rd = fpis[t-FLOAT]->rounding;
/* XXX sf_constrounding */
		if ((rd & 3) == FPI_Round_up || (rd & 3) == FPI_Round_down) {
			rv = sfround(SFNEG(rv), z.lo, t);
			return SFNEG(rv);
		}
	}
	return sfround(rv, z.lo, t);
}

SF
soft_plus(SF x1, SF x2, TWORD t)
{
	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return sfsub(x2, SFNEG(x1), - (int)t);
	  case 0 - SF_Neg:
		return sfsub(x1, SFNEG(x2), t);
	}
	return sfadd(x1, x2, t);
}

SF
soft_minus(SF x1, SF x2, TWORD t)
{
	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	if ((x1.kind & SF_Neg) != (x2.kind & SF_Neg))
		return sfadd(x1, SFNEG(x2), t);
	else if (SFISZ(x1) && SFISZ(x2))
		return x1; /* special case -0 - -0, should return -0 */
	else if (x1.kind & SF_Neg)
		return sfsub(SFNEG(x2), SFNEG(x1), - (int)t);
	else
		return sfsub(x1, x2, t);
}

/*
 * Multiply two softfloats.
 */
SF
soft_mul(SF x1, SF x2, TWORD t)
{
#if 0
	ULong x1hi, x2hi;
	ULLong mid1, mid, extra;
#else
	DULLong z;
#endif

	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	x1.kind ^= x2.kind & SF_Neg;
	SFCOPYSIGN(x2, x1);
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	if ((SFISINF(x1) && SFISZ(x2)) || (SFISZ(x1) && SFISINF(x2)))
		return nansf(x1.kind | SFEXCP_Invalid);
	if (SFISINF(x1) || SFISZ(x1))
		return x1;
	if (SFISINF(x2) || SFISZ(x2))
		return x2;
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	x1.exponent += x2.exponent + WORKBITS;
#if 0
	x1hi = x1.significand >> 32;
	x1.significand &= ONES(32);
	x2hi = x2.significand >> 32;
	x2.significand &= ONES(32);
	extra = x1.significand * x2.significand;
	mid1 = x1hi * x2.significand;
	mid = mid1 + x1.significand * x2hi;
	x1.significand = (ULLong) x1hi * x2hi;
	x1.significand += ((ULLong)(mid < mid1) << 32) | (mid >> 32);
	mid <<= 32;
	extra += mid;
#ifdef LONGLONG_WIDER_THAN_WORKBITS
	if (extra < mid || (extra>>WORKBITS)) {
		x1.significand++;
		extra &= ONES(WORKBITS);
	}
#else
	x1.significand += (extra < mid);
#endif
	if (x1.significand < NORMALMANT) {
		x1.exponent--;
		x1.significand <<= 1;
		if (extra >= NORMALMANT) {
			x1.significand++;
			extra -= NORMALMANT;
		}
		extra <<= 1;
	}
	return sfround(x1, extra, t);
#else
	z = muld(x1.significand, x2.significand);
	if (z.hi < NORMALMANT) {
		x1.exponent--;
		z = lshiftd(z.hi, z.lo, 1);
	}
	x1.significand = z.hi;
	return sfround(x1, z.lo, t);
#endif
}

SF
soft_div(SF x1, SF x2, TWORD t)
{
	ULLong q, r, oppx2;
	int exp;

	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	x1.kind ^= x2.kind & SF_Neg;
	SFCOPYSIGN(x2, x1);
	if (SFISNAN(x1))
		return x1;
	else if (SFISNAN(x2))
		return x2;
	if ((SFISINF(x1) && SFISINF(x2)) || (SFISZ(x1) && SFISZ(x2)))
		return nansf(x1.kind | SFEXCP_Invalid);
	if (SFISINF(x1) || SFISZ(x1))
		return x1;
	else if (SFISINF(x2))
		return zerosf(x1.kind);
	else if (SFISZ(x2))
		return infsf(x2.kind | SFEXCP_DivByZero);
	assert(x1.significand && x2.significand);
	SFNORMALIZE(x1);
	SFNORMALIZE(x2);
	exp = x1.exponent - x2.exponent - WORKBITS;
	if (exp < -32767)
		/* huge underflow, flush to 0 to avoid issues */
		return zerosf(x1.kind | SFEXCP_Inexlo | SFEXCP_Underflow);
	q = 0;
	if (x1.significand >= x2.significand) {
		++exp;
		++q;
		x1.significand -= x2.significand;
	}
	r = x1.significand;
	oppx2 = (ONES(WORKBITS-1) - x2.significand) + 1;
	do {
		q <<= 1;
		if (r & NORMALMANT) {
			r &= ~NORMALMANT;
			r <<= 1;
			r += oppx2;
			++q;
		}
		else {
			r <<= 1;
			if (r >= x2.significand) {
				r -= x2.significand;		
				++q;
			}
		}
	} while((q & NORMALMANT) == 0);
	x1.significand = q;
	x1.exponent = exp;
	if (r) {
		/* be sure to set correctly highest bit of extra */
		r += oppx2 / 2;
		r |= 1; /* rounds to odd */
/* XXX can remainder be power-of-2? doesn't seem it may happen... */
	}
	return sfround(x1, r, t);
}

#ifndef NO_COMPLEX
/*
 * Perform the addition or subtraction of two products of finite numbers.
 * Keep the extra bits (do not round).
 * Note that maximum return exponent is 2*emax+WORKBITS.
 */
#define FMMPLUS 	0
#define FMMMINUS	SF_Neg

static SF
finitemma(SF a, SF b, int op, SF c, SF d, ULLong * extrap)
{
	SF rv;
	DULLong z, z1, z2;
	int diff, z1k, z1exp, z2k, z2exp, excess;
	ULLong x;

	z1k = z2k = SF_Normal |
		(a.kind & SFEXCP_ALLmask) | (b.kind & SFEXCP_ALLmask) |
		(c.kind & SFEXCP_ALLmask) | (d.kind & SFEXCP_ALLmask);
	z1k |= (a.kind & SF_Neg) ^ (b.kind & SF_Neg);
	z2k |= (c.kind & SF_Neg) ^ (d.kind & SF_Neg) ^ op;
	assert(a.significand && b.significand && c.significand && d.significand);
	SFNORMALIZE(a);
	SFNORMALIZE(b);
	SFNORMALIZE(c);
	SFNORMALIZE(d);
	z1exp = a.exponent + b.exponent + WORKBITS;
	z2exp = c.exponent + d.exponent + WORKBITS;
#if 0
	if (z1exp - WORKBITS > z2exp) {
		x1.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return x1;
	}
	if (z2exp - WORKBITS > z1exp) {
		x2.kind |= (int)t < 0 ? SFEXCP_Inexlo : SFEXCP_Inexhi;
		return SFNEG(x2);
	}
#endif
	z1 = muld(a.significand, b.significand);
	if (z1.hi < NORMALMANT) {
		z1exp--;
		z1 = lshiftd(z1.hi, z1.lo, 1);
	}
	z2 = muld(c.significand, d.significand);
	if (z2.hi < NORMALMANT) {
		z2exp--;
		z2 = lshiftd(z2.hi, z2.lo, 1);
	}
	diff = z1exp - z2exp;
	if (z1k == z2k) { /* same sign, add them; easier */
		rv.kind = z1k;
		if (diff < 0) {
			z = z2, z2 = z1, z1 = z;
			rv.exponent = z2exp;
		}
		else
			rv.exponent = z1exp;
		z2 = rshiftdro(z2.hi, z2.lo, diff );
		rv.significand = z1.hi + z2.hi;
		z1.lo += z2.lo;
		if (z1.lo < z2.lo)
			++rv.significand;
		if (rv.significand < NORMALMANT) {
			/* target mantissa overflows */
			z = rshiftdro(rv.significand, z1.lo, 1);
			rv.significand = z.hi | NORMALMANT;
			++rv.exponent;
		}
		else
			z.lo = z1.lo;
		*extrap = z.lo;
	}
	else { /* opposite sign, substraction, and possible cancellation */
		if (diff == 0 && z1.hi == z2.hi && z1.lo == z2.lo) {
			*extrap = 0;
/* XXX compute sign of 0 if rounding, merge exceptions... */
			return zerosf(z1k & ~SF_Neg);
		}
		else if (diff == 0 && z1.hi == z2.hi) {
			if (z1.lo > z2.lo) {
				rv.kind = z1k;
				rv.significand = z1.lo - z2.lo;
			}
			else {
				rv.kind = z2k;
				rv.significand = z2.lo - z1.lo;
			}
			rv.exponent = z1exp - WORKBITS;
			z.lo = 0;
		}
		else {
			if (diff < 0 || (diff == 0 && z1.hi < z2.hi)) {
				rv.kind = z2k ^ SF_Neg;
				rv.exponent = z2exp;
				if (diff != 0) {
					z = z2;
					z2 = rshiftdro(z1.hi, z1.lo, -diff );
					z1 = z;
				}
				else {
					z = z2, z2 = z1, z1 = z;
				}
			}
			else {
				rv.kind = z1k;
				rv.exponent = z1exp;
				if (diff != 0)
					z2 = rshiftdro(z2.hi, z2.lo, diff );
			}
			z.hi = z1.hi - z2.hi;
			if (z2.lo > z1.lo)
				--z.hi;
			z.lo = z1.lo - z2.lo;
			x = z.hi;
			for (excess = 0; x < NORMALMANT; ++excess)
				x <<= 1;
			z = lshiftd(z.hi, z.lo, excess);
			rv.exponent -= excess;
			rv.significand = z.hi;
		}
		*extrap = z.lo;
	}
	return rv;
}

#define CXSFISZ(r,i)	(SFISZ(r)   && SFISZ(i))
#define CXSFISINF(r,i)	(SFISINF(r) || SFISINF(i))
#define CXSFISNAN(r,i)	(!CXSFISINF(r,i) && (SFISNAN(r) || SFISNAN(i)))

/*
 * Multiply two complexes (pairs of softfloats)
 */
void
soft_cxmul(SF r1, SF i1, SF r2, SF i2, SF *rrv, SF *irv, TWORD t)
{
	SF sf;
	ULLong extra;

# if 0
	x1.kind |= x2.kind & SFEXCP_ALLmask;
	x2.kind |= x1.kind & SFEXCP_ALLmask;
	x1.kind ^= x2.kind & SF_Neg;
	SFCOPYSIGN(x2, x1);
#endif
	if (CXSFISINF(r1, i1)) {
		if (CXSFISZ(r2, i2)) {
			return;
		}
		else if (SFISNAN(r2) && SFISNAN(i2)) {
			return;
		}
		/* result is an infinity */
	}
	else if (CXSFISINF(r2, i2)) {
		if (CXSFISZ(r1, i1)) {
			return;
		}
		else if (SFISNAN(r1) && SFISNAN(i1)) {
			return;
		}
		/* result is an infinity */
	}

/* XXX missing many special cases with NaN or zeroes... */

	sf = finitemma(r1, r2, FMMMINUS, i1, i2, &extra);
	*rrv = sfround(sf, extra, t);
	sf = finitemma(r1, i2, FMMPLUS,  i1, r2, &extra);
	*irv = sfround(sf, extra, t);
}

/*
 * Divide two complexes (pairs of softfloats.) Indeed complex.
 */
/*
 * Helper function: division double by double
 * Result is correctly rounded.
 */
static SF
sfddiv(SF num, ULLong extran, SF den, ULLong extrad, TWORD t)
{
	DULLong zn, zd, r, oppden;
	ULLong q;
	int exp, excess;

#if 0
	num.kind |= den.kind & SFEXCP_ALLmask;
	den.kind |= num.kind & SFEXCP_ALLmask;
	num.kind ^= den.kind & SF_Neg;
	SFCOPYSIGN(den, num);
#endif
	assert(num.significand && den.significand);
	q = num.significand;
	for (excess = 0; q < NORMALMANT; ++excess)
		q <<= 1;
	zn = lshiftd(num.significand, extran, excess);
	num.exponent -= excess;
	q = den.significand;
	for (excess = 0; q < NORMALMANT; ++excess)
		q <<= 1;
	zd = lshiftd(den.significand, extrad, excess);
	den.exponent -= excess;
	exp = num.exponent - den.exponent - WORKBITS;
	if (exp < -32767) {
		/* huge underflow, flush to 0 to avoid issues */
		return zerosf(num.kind | SFEXCP_Inexlo | SFEXCP_Underflow);
	}
	q = 0;
	if (zn.hi >= zd.hi) {
		++exp;
		++q;
		zn.hi -= zd.hi;
	}
	r = zn;
	if (zd.lo) {
		oppden.hi = (ONES(WORKBITS-1) - zd.hi);
		oppden.lo = (ONES(WORKBITS-1) - zd.lo) + 1;
	}
	else {
		oppden.hi = (ONES(WORKBITS-1) - zd.hi) + 1;
		oppden.lo = 0;
	}
	do {
		q <<= 1;
		if (r.hi & NORMALMANT) {
			r.hi &= ~NORMALMANT;
			r = lshiftd(r.hi, r.lo, 1);
			r.hi += oppden.hi;
			r.lo += oppden.lo;
			if (r.lo < oppden.lo)
				++r.hi;
			++q;
		}
		else {
			r = lshiftd(r.hi, r.lo, 1);
			if (r.hi > zd.hi || (r.hi == zd.hi && r.lo >= zd.lo)) {
				r.hi -= zd.hi;
				if (zd.lo > r.lo)
					--r.hi;
				r.lo -= zd.lo;
				++q;
			}
		}
	} while((q & NORMALMANT) == 0);
	num.significand = q;
	num.exponent = exp;
	if (r.hi) {
		/* be sure to set correctly highest bit of extra */
		r.hi += oppden.hi / 2;
		r.hi |= 1; /* rounds to odd */
/* XXX is there special case if remainder is power-of-2? can it happen? */
	}
	return sfround(num, r.hi, t);
}

void
soft_cxdiv(SF r1, SF i1, SF r2, SF i2, SF *rrv, SF *irv, TWORD t)
{
	SF den, sf;
	ULLong extrad, extra;

	if (CXSFISINF(r1, i1)) {
		if (CXSFISINF(r2, i2)) {
			return;
		}
		else if (SFISNAN(r2) && SFISNAN(i2)) {
			return;
		}
		/* result is an infinity */
	}
	else if (CXSFISINF(r2, i2)) {
		if (SFISNAN(r1) && SFISNAN(i1)) {
			return;
		}
		/* result is zero */
	}
	else if (CXSFISZ(r2, i2)) {
		if (SFISNAN(r2) && SFISNAN(i2)) {
			return;
		}
		/* result is an infinity */
	}

/* XXX missing many special cases with NaN or zeroes... */

	den= finitemma(r2, r2, FMMPLUS,  i2, i2, &extrad);
	sf = finitemma(r1, r2, FMMPLUS,  i1, i2, &extra);
	*rrv = sfddiv(sf, extra, den, extrad, t);
	sf = finitemma(i1, r2, FMMMINUS, r1, i2, &extra);
	*irv = sfddiv(sf, extra, den, extrad, t);
}
#endif

/*
 * Classifications and comparisons.
 */

/*
 * Return true if fp number is zero. Easy.
 *
 * Ignores FLT_EVAL_METHOD, compares the "internal value" of the fp number.
 */
int
soft_isz(SF sf)
{
	return (sf.kind & SF_kmask) == SF_Zero;
}

int
soft_isnan(SF sf)
{
	return (sf.kind & SF_kmask) >= SF_NaN;
}

/*
 * Return IEEE754 class of fp number, as SF_xxx enum member.
 * First clamp the number into its original (semantic) type, cf. 7.12.3.
 * Can be used to implement isinf, isfinite, isnormal.
 */
int
soft_fpclassify(SF sf, TWORD t)
{
	return SFROUND(sf, t).kind & SF_kmask;
}


/*
 * Return true if either operand is NaN.
 */
static int
soft_cmp_unord(SF x1, SF x2)
{
	return SFISNAN(x1) || SFISNAN(x2);
}

static int
soft_cmp_eq(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
		return 0; /* IEEE says "quiet" */
	if (SFISZ(x1))
		/* special case: +0 == -0 (discard sign) */
		return SFISZ(x2);
	if ((x1.kind & SF_Neg) != (x2.kind & SF_Neg))
		return 0;
	if (SFISINF(x1))
		return SFISINF(x2);
	/* Both operands are finite, nonzero, same sign. */
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 == exp2 && x1.significand == x2.significand;
}

/*
 * IEEE754 states that between any two floating-point numbers,
 * four mutually exclusive relations are possible:
 * less than, equal, greater than, or unordered.
 */

static int
soft_cmp_ne(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_eq(x1, x2);
}

static int
soft_cmp_lt(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
/* XXX "invalid"; warns? use sf_exceptions */
		return 0;
	if (SFISZ(x1) && SFISZ(x2))
		return 0; /* special case: -0 !> +0 */
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return 1; /* x1 < 0 < x2 */
	  case 0 - SF_Neg:
		return 0; /* x1 > 0 > x2 */
	  case 0:
		break;
	}
	if (x1.kind & SF_Neg) {
		SF tmp = x1;
		x1 = x2;
		x2 = tmp;
	}
	if (SFISINF(x2))
		return ! SFISINF(x1);
	if (SFISZ(x1))
		return 1;
	if (SFISZ(x2) || SFISINF(x1))
		return 0;
	/* Both operands are finite, nonzero, same sign. Test |x1| < |x2|*/
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 < exp2 || x1.significand < x2.significand;
}

static int
soft_cmp_gt(SF x1, SF x2)
{
	int exp1, exp2;

	if (soft_cmp_unord(x1, x2))
/* XXX "invalid"; warns? use sf_exceptions */
		return 0;
	if (SFISZ(x1) && SFISZ(x2))
		return 0; /* special case: -0 !< +0 */
	switch ((x1.kind & SF_Neg) - (x2.kind & SF_Neg)) {
	  case SF_Neg - 0:
		return 1; /* x1 > 0 > x2 */
	  case 0 - SF_Neg:
		return 0; /* x1 < 0 < x2 */
	  case 0:
		break;
	}
	if (x1.kind & SF_Neg) {
		SF tmp = x1;
		x1 = x2;
		x2 = tmp;
	}
	if (SFISINF(x1))
		return ! SFISINF(x2);
	if (SFISZ(x1) || SFISINF(x2))
		return 0;
	if (SFISZ(x2))
		return 1;
	/* Both operands are finite, nonzero, same sign. Test |x1| > |x2|*/
	exp1 = x1.exponent, exp2 = x2.exponent;
	assert(x1.significand && x2.significand);
	while (x1.significand < NORMALMANT)
		x1.significand <<= 1, exp1--;
	while (x2.significand < NORMALMANT)
		x2.significand <<= 1, exp2--;
	return exp1 > exp2 || x1.significand > x2.significand;
}

/*
 * Note: for _le and _ge, having NaN operand is "invalid" in IEEE754;
 * but we cannot return SFEXCP_Invalid as done for the operations.
 */
static int
soft_cmp_le(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_gt(x1, x2);
}

static int
soft_cmp_ge(SF x1, SF x2)
{
	return soft_cmp_unord(x1, x2) ? 0 : ! soft_cmp_lt(x1, x2);
}

int
soft_cmp(SF v1, SF v2, int v)
{
	int rv = 0;

	switch (v) {
	case GT:
		rv = soft_cmp_gt(v1, v2);
		break;
	case LT:
		rv = soft_cmp_lt(v1, v2);
		break;
	case GE:
		rv = soft_cmp_ge(v1, v2);
		break;
	case LE:
		rv = soft_cmp_le(v1, v2);
		break;
	case EQ:
		rv = soft_cmp_eq(v1, v2);
		break;
	case NE:
		rv = soft_cmp_ne(v1, v2);
		break;
	}
	return rv;
}

#if 0
/*
 * Prepare a SF value for use in packed (interchange) format.
 * Correctly rounds for the target type representation.
 * Returns the biased exponent, ready to be shifted.
 *
 * The .significand bits are left in place, ready to be used by
 * the endian-aware code. The MSB one is still there.
 * Sign is indicated with psf->kind & SF_neg, as usual.
 *
 * SF_NaN is expanded into the IEEE754:2008 default representation
 * (11 as most significant bits, rest all zeroes); if not appropriate
 * for the target, the calling code should replace it with SF_NaNbits
 * with the adequate bits into the .significand member.
 */
/* XXX TODO: implement the NaNbits stuff in sfround() above... */
static int
soft_pack(SF *psf, TWORD t)
{
	FPI *fpi;
	int biasedexp;

	assert(t>=FLOAT && t<=LDOUBLE);
	fpi = fpis[t-FLOAT];
	*psf = SFROUND(*psf, t);
	switch(psf->kind & SF_kmask) {
	  case SF_Zero:
		psf->significand = 0;
		biasedexp = 0;
		if (! fpi->has_neg_zero)
			psf->kind &= ~SF_Neg;
		break;

	  case SF_Normal:
		if (fpi->has_radix_16) {
			assert((psf->significand >> (fpi->nbits-4)) >= 1);
			assert((psf->significand >> (fpi->nbits-4)) <= 15);
		}
		else
			assert((psf->significand >> (fpi->nbits-1)) == 1);
		assert(psf->exponent >= fpi->emin);
		assert(psf->exponent <= fpi->emax);
		biasedexp = psf->exponent + fpi->exp_bias;
		break;

	  case SF_Denormal:
		assert(! fpi->sudden_underflow);
		assert(! fpi->has_radix_16);
		assert((psf->significand >> (fpi->nbits-1)) == 0);
		assert(psf->exponent == fpi->emin);
		biasedexp = 0;
		break;

	  case SF_Infinite:
		assert(fpi->has_inf_nan);
		psf->significand = ONEZEROES(fpi->nbits-1);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;

	  case SF_NoNumber:
	  default:
		/* Can it happen? Debug_Warns? ICE? */
		/* Let decay as quiet NaN */
	  case SF_NaN:
		psf->significand = 3 * ONEZEROES(fpi->nbits-2);
		/* FALLTHROUGH */
	  case SF_NaNbits:
/* XXX 0./0. on a Vax is likely to end here... as an ICE :-( */
		assert(fpi->has_inf_nan);
		biasedexp = fpi->emax - fpi->exp_bias + 1;
		break;
	}
	if (fpi->has_radix_16) {
		assert((biasedexp & 3) == 0);
		biasedexp >>= 2;
	}
	return biasedexp;
}

/*
 * Convert an internal floating-point constant into its external representation.
 * isf is floating point number, dt is resulting type and 
 */
CONSZ
soft_fp2ext(SF isf, TWORD dt)
{
#ifdef FLT_IS_NOT_SIGN_EXP_MAGNITUDE
	return NULL;
#else
	SF sf;
	FPI *fpi;
	int exp, fracbits, t, ti;

#ifdef notyet
	fpi = fpis[t - FLOAT];
	if (fpi->storage == 0)
		return NULL; /* not stored as sign+exponent+significand */
#endif
	if (~(U_CONSZ)0 >> (fpi->storage-1) == 0)
		return NULL; /* too large */
	sf = p->n_dcon;
	exp = soft_pack(&sf, t);
	fracbits = fpi->nbits - 1;
	if (fpi->explicit_one) ++fracbits;
	q = ccopy(p);
	q->n_lval = sf.significand & (((U_CONSZ)1 << fracbits) - 1);
	q->n_lval |= ((U_CONSZ)exp) << fracbits;
	if (sf.kind & SF_Neg)
		q->n_lval |= ((U_CONSZ)1 << (fpi->storage-1));
	q->n_op = ICON;
	q->n_type = ti;
	q->n_sp = NULL;
	return q;
#endif
}
#endif

/*
 * Conversions from decimal and hexadecimal strings.
 * Rounds correctly to the target type (subject to FLT_EVAL_METHOD.)
 * dt is resulting type.
 */
SF
strtosf(char *str, TWORD tw)
{
	SF sf;
	char *eptr;
	FPI *fpi, const_fpi;
	ULong bits[2] = { 0, 0 };
	Long expt;
	int k;

#ifdef TARGET_FLT_EVAL_METHOD
	fpi = fpis[tw > TARGET_FLT_EVAL_METHOD + FLOAT ? tw - FLOAT :
			TARGET_FLT_EVAL_METHOD];
#else
	fpi = fpis[tw - FLOAT];
#endif
	if (sf_constrounding != FPI_RoundNotSet) {
		const_fpi = *fpi;
		const_fpi.rounding = sf_constrounding;
		fpi = &const_fpi;
	}
	k = strtodg(str, &eptr, fpi, &expt, bits);
	if (k & SFEXCP_Overflow)
		werror("Overflow in floating-point constant");
/* XXX F.7.2 recommends (indirectly) diagnostic for underflow; might be verbose though */
	if (k & SFEXCP_Inexact && (str[1] == 'x' || str[1] == 'X'))
		werror("Hexadecimal floating-point constant not represented exactly");
	sf.kind = k;
	sf.significand = bits[0];
	if (fpi->nbits > 32)
		sf.significand |= ((ULLong)bits[1] << 32);
	sf.exponent = expt;
	return sf;
}
#endif
#endif
