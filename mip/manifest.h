/*	manifest.h	4.2	87/12/09	*/

#ifndef _MANIFEST_
#define	_MANIFEST_

#include <stdio.h>
#include "config.h"
#include "node.h"
#include "main.h"

#define DSIZE	(MAXOP+1)	/* DSIZE is the size of the dope array */

#define NOLAB	(-1)		/* no label with constant */

/*
 * Node types
 */
#define LTYPE	02		/* leaf */
#define UTYPE	04		/* unary */
#define BITYPE	010		/* binary */

/*
 * Bogus type values
 */
#define TNULL	INCREF(MOETY)	/* pointer to MOETY -- impossible type */
#define TVOID	FTN		/* function returning UNDEF (for void) */

/*
 * Type packing constants
 */
#define TMASK	0x60		/* mask for 1st component of compound type */
#define TMASK1	0x180		/* mask for 2nd component of compound type */
#define TMASK2	0x1e0		/* mask for 3rd component of compound type */
#define BTMASK	0x1f		/* basic type mask */
#define BTSHIFT	5		/* basic type shift */
#define TSHIFT	2		/* shift count to get next type component */

/*
 * Type manipulation macros
 */
#define MODTYPE(x,y)	x = ((x)&(~BTMASK))|(y)	/* set basic type of x to y */
#define BTYPE(x)	((x)&BTMASK)		/* basic type of x */
#define	ISLONGLONG(x)	((x) == LONGLONG || (x) == ULONGLONG)
#define ISUNSIGNED(x)	(((x) & 1) == (UNSIGNED & 1))
#define UNSIGNABLE(x)	(((x)<=ULONGLONG&&(x)>=CHAR) && !ISUNSIGNED(x))
#define ENUNSIGN(x)	((x)+1)
#define DEUNSIGN(x)	((x)-1)
#define ISPTR(x)	(((x)&TMASK)==PTR)
#define ISFTN(x)	(((x)&TMASK)==FTN)	/* is x a function type */
#define ISARY(x)	(((x)&TMASK)==ARY)	/* is x an array type */
#define INCREF(x)	((((x)&~BTMASK)<<TSHIFT)|PTR|((x)&BTMASK))
#define DECREF(x)	((((x)>>TSHIFT)&~BTMASK)|( (x)&BTMASK))
/* advance x to a multiple of y */
#define SETOFF(x,y)	if ((x)%(y) != 0) (x) = (((x)/(y) + 1) * (y))
/* can y bits be added to x without overflowing z */
#define NOFIT(x,y,z)	(((x)%(z) + (y)) > (z))

/*
 * Pack and unpack field descriptors (size and offset)
 */
#define PKFIELD(s,o)	(((o)<<6)| (s))
#define UPKFSZ(v)	((v) &077)
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

#define optype(o)	(dope[o]&TYFLG)
#define asgop(o)	(dope[o]&ASGFLG)
#define logop(o)	(dope[o]&LOGFLG)
#define callop(o)	(dope[o]&CALLFLG)

/*
 * Types, as encoded in intermediate file cookies.
 * The order of the integer types are important.
 */
#define	UNDEF		0
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
/*
 * The following are only used in pass1, for simplicity.
 */
#define	SIGNED		19
#define	CONST		20
#define	VOLATILE	21
#define	VOID		22

/* 
 * Type modifiers.
 */
#define	PTR		0x20
#define	FTN		0x40
#define	ARY		0x60
#define	BASETYPE	0x1f
#define	TYPESHIFT	2

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

/* memory management */
void *permalloc(int size);
void *tmpalloc(int size);
void tmpfree(void);

/* pass t communication subroutines */
void topt_compile(struct interpass *);

/* pass 2 communication subroutines */
void pass2_compile(struct interpass *);

/* node routines */
void nfree(NODE *);

extern	int nerrors;		/* number of errors seen so far */
extern	int dope[];		/* a vector containing operator information */
extern	char *opst[];		/* a vector containing names for ops */
#endif
