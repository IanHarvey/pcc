/*	$Id$	*/
/*
 * Copyright(C) Caldera International Inc. 2001-2002. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code and documentation must retain the above
 * copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditionsand the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 * 	This product includes software developed or owned by Caldera
 *	International, Inc.
 * Neither the name of Caldera International, Inc. nor the names of other
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * USE OF THE SOFTWARE PROVIDED FOR UNDER THIS LICENSE BY CALDERA
 * INTERNATIONAL, INC. AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL CALDERA INTERNATIONAL, INC. BE LIABLE
 * FOR ANY DIRECT, INDIRECT INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OFLIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef MANIFEST
#define	MANIFEST

#include <stdio.h>
#include "macdefs.h"
#include "node.h"
#include "main.h"

/*
 * Node types
 */
#define LTYPE	02		/* leaf */
#define UTYPE	04		/* unary */
#define BITYPE	010		/* binary */

/*
 * DSIZE is the size of the dope array
 */
#define DSIZE	(MAXOP+1)

/*
 * Type names, used in symbol table building.
 * The order of the integer types are important.
 */
#define	UNDEF		0	/* free symbol table entry */
#define	FARG		1 	/* function argument */
#define	CHAR		2
#define	UCHAR		3
#define	SHORT		4
#define	USHORT		5
#define	INT		6
#define	UNSIGNED	7
#define	LONG		8
#define	ULONG		9      
#define	LONGLONG	10
#define	ULONGLONG	11
#define	FLOAT		12
#define	DOUBLE		13
#define	LDOUBLE		14
#define	STRTY		15
#define	UNIONTY		16
#define	ENUMTY		17
#define	MOETY		18	/* member of enum */
#define	VOID		19
/*
 * The following is only used in pass1, for simplicity.
 */
#define	SIGNED		20

/*
 * Various flags
 */
#define NOLAB	(-1)

/* 
 * Type modifiers.
 */
#define	PTR		0x20
#define	FTN		0x40
#define	ARY		0x60
#define	CON		0x20
#define	VOL		0x40

/*
 * Type packing constants
 */
#define TMASK	0x060
#define TMASK1	0x180
#define TMASK2	0x1e0
#define BTMASK	0x1f
#define BTSHIFT	5
#define TSHIFT	2

/*
 * Macros
 */
#define MODTYPE(x,y)	x = ((x)&(~BTMASK))|(y)	/* set basic type of x to y */
#define BTYPE(x)	((x)&BTMASK)		/* basic type of x */
#define	ISLONGLONG(x)	((x) == LONGLONG || (x) == ULONGLONG)
#define ISUNSIGNED(x)	(((x) <= ULONGLONG) && (((x) & 1) == (UNSIGNED & 1)))
#define UNSIGNABLE(x)	(((x)<=ULONGLONG&&(x)>=CHAR) && !ISUNSIGNED(x))
#define ENUNSIGN(x)	((x)+1)
#define DEUNSIGN(x)	((x)-1)
#define ISPTR(x)	(((x)&TMASK)==PTR)
#define ISFTN(x)	(((x)&TMASK)==FTN)	/* is x a function type? */
#define ISARY(x)	(((x)&TMASK)==ARY)	/* is x an array type? */
#define	ISCON(x)	(((x)&CON)==CON)	/* is x const? */
#define	ISVOL(x)	(((x)&VOL)==VOL)	/* is x volatile? */
#define INCREF(x)	((((x)&~BTMASK)<<TSHIFT)|PTR|((x)&BTMASK))
#define INCQAL(x)	((((x)&~BTMASK)<<TSHIFT)|((x)&BTMASK))
#define DECREF(x)	((((x)>>TSHIFT)&~BTMASK)|((x)&BTMASK))
#define DECQAL(x)	((((x)>>TSHIFT)&~BTMASK)|((x)&BTMASK))
#define SETOFF(x,y)	{ if ((x)%(y) != 0) (x) = (((x)/(y) + 1) * (y)); }
		/* advance x to a multiple of y */
#define NOFIT(x,y,z)	(((x)%(z) + (y)) > (z))
		/* can y bits be added to x without overflowing z */

#ifndef SPECIAL_INTEGERS
#define	ASGLVAL(lval, val)
#endif

/*
 * Pack and unpack field descriptors (size and offset)
 */
#define PKFIELD(s,o)	(((o)<<6)| (s))
#define UPKFSZ(v)	((v)&077)
#define UPKFOFF(v)	((v)>>6)

/*
 * Operator information
 */
#define TYFLG	016
#define ASGFLG	01
#define LOGFLG	020

#define SIMPFLG	040
#define COMMFLG	0100
#define DIVFLG	0200
#define FLOFLG	0400
#define LTYFLG	01000
#define CALLFLG	02000
#define MULFLG	04000
#define SHFFLG	010000
#define ASGOPFLG 020000

#define SPFLG	040000

/*
 * Table sizes.
 */
#define TREESZ	20000		/* space for building parse tree */

/*
 * Location counters
 */
#define PROG		0		/* program segment */
#define DATA		1		/* data segment */
#define ADATA		2		/* array data segment */
#define STRNG		3		/* string data segment */
#define ISTRNG		4		/* initialized string segment */
#define STAB		5		/* symbol table segment */


/*
 * External declarations, typedefs and the like
 */
char	*hash(char *s);
char	*savestr(char *cp);
char	*tstr(char *cp);

/* memory management stuff */
void *permalloc(int size);
void *tmpalloc(int size);
void tmpfree(void);
char *newstring(char *, int len);

void tprint(FILE *, TWORD, TWORD);

/* pass t communication subroutines */
void topt_compile(struct interpass *);

/* pass 2 communication subroutines */
void pass2_compile(struct interpass *);

/* node routines */
void nfree(NODE *);

extern	int nerrors;		/* number of errors seen so far */
#endif
