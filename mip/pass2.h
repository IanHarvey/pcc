/*	pass2.h	4.4	87/12/10	*/

#ifndef _PASS2_
#define	_PASS2_

#include <sys/types.h>

#include "macdefs.h"
#include "mac2defs.h"
#include "manifest.h"
#include "protos.h"

/* cookies, used as arguments to codgen */
#define FOREFF	01		/* compute for effects only */
#define INAREG	02		/* compute into a register */
#define INTAREG	04		/* compute into a scratch register */
#define INBREG	010		/* compute into a lvalue register */
#define INTBREG 020		/* compute into a scratch lvalue register */
#define FORCC	040		/* compute for condition codes only */
#define INTEMP	010000		/* compute into a temporary location */
#define FORARG	020000		/* compute for an argument of a function */
#define FORREW	040000		/* search the table for a rewrite rule */

/*
 * OP descriptors,
 * the ASG operator may be used on some of these
 */
#define OPSIMP	010000		/* +, -, &, |, ^ */
#define OPCOMM	010002		/* +, &, |, ^ */
#define OPMUL	010004		/* *, / */
#define OPDIV	010006		/* /, % */
#define OPUNARY	010010		/* unary ops */
#define OPLEAF	010012		/* leaves */
#define OPANY	010014		/* any op... */
#define OPLOG	010016		/* logical ops */
#define OPFLOAT	010020		/* +, -, *, or / (for floats) */
#define OPSHFT	010022		/* <<, >> */
#define OPLTYPE	010024		/* leaf type nodes (e.g, NAME, ICON, etc.) */

/* match returns */
#define MNOPE	010000		/* no match generated */
#define MDONE	010001		/* done evalution */

/* shapes */
#define SANY	01		/* same as FOREFF */
#define SAREG	02		/* same as INAREG */
#define STAREG	04		/* same as INTAREG */
#define SBREG	010		/* same as INBREG */
#define STBREG	020		/* same as INTBREG */
#define SCC	040		/* same as FORCC */
#define SNAME	0100
#define SCON	0200
#define SFLD	0400
#define SOREG	01000
#define STARNM	02000
#define STARREG	04000
#define SWADD	040000
#define SPECIAL	0100000
#define SZERO	SPECIAL
#define SONE	(SPECIAL|1)
#define SMONE	(SPECIAL|2)
#define SCCON	(SPECIAL|3)	/* -256 <= constant < 256 */
#define SSCON	(SPECIAL|4)	/* -32768 <= constant < 32768 */
#define SSOREG	(SPECIAL|5)	/* non-indexed OREG */

/* FORARG and INTEMP are carefully not conflicting with shapes */

/* types */
#define TCHAR		01	/* char */
#define TSHORT		02	/* short */
#define TINT		04	/* int */
#define TLONG		010	/* long */
#define TFLOAT		020	/* float */
#define TDOUBLE		040	/* double */
#define TPOINT		0100	/* pointer to something */
#define TUCHAR		0200	/* unsigned char */
#define TUSHORT		0400	/* unsigned short */
#define TUNSIGNED	01000	/* unsigned int */
#define TULONG		02000	/* unsigned long */
#define TPTRTO		04000	/* pointer to one of the above */
#define TANY		010000	/* matches anything within reason */
#define TSTRUCT		020000	/* structure or union */
#define	TLONGLONG	040000	/* long long */
#define	TULONGLONG	0100000	/* unsigned long long */

/* reclamation cookies */
#define RNULL		0	/* clobber result */
#define RLEFT		01
#define RRIGHT		02
#define RESC1		04
#define RESC2		010
#define RESC3		020
#define RESCC		04000
#define RNOP		010000	/* DANGER: can cause loops.. */

/* needs */
#define NAREG		01
#define NACOUNT		03
#define NAMASK		017
#define NASL		04  /* share left register */
#define NASR		010 /* share right register */
#define NBREG		020
#define NBCOUNT		060
#define NBMASK		0360
#define NBSL		0100
#define NBSR		0200
#define NTEMP		0400
#define NTMASK		07400
#define REWRITE		010000

#define MUSTDO		010000	/* force register requirements */
#define NOPREF		020000	/* no preference for register assignment */

/* register allocation */
extern	int rstatus[];		/* register status info */
extern	int busy[];		/* register use info */
extern	struct respref {
	int	cform;
	int	mform;
} respref[];

#define isbreg(r)	(rstatus[r]&SBREG)
#define istreg(r)	(rstatus[r]&(STBREG|STAREG))
#define istnode(p)	(p->n_op==REG && istreg(p->n_rval))

#define TBUSY		01000
#define REGLOOP(i)	for (i = 0; i < REGSZ; ++i)

#define SETSTO(x,y)	(stotree = (x), stocook = (y))
extern	int stocook;

extern	NODE *stotree;
extern	int callflag;

extern	int fregs;

/* code tables */
extern	struct optab {
	int	op;
	int	visit;
	int	lshape;
	int	ltype;
	int	rshape;
	int	rtype;
	int	needs;
	int	rewrite;
	char	*cstring;
} table[];

extern	NODE resc[];

extern	OFFSZ tmpoff;
extern	OFFSZ maxoff;
extern	OFFSZ baseoff;
extern	OFFSZ maxtemp;
extern	int maxtreg;
extern	int ftnno;

extern	int nrecur;		/* flag to keep track of recursions */

#define NRECUR  (10*TREESZ)

extern	NODE
	*talloc(void),
	*eread(void),
	*tcopy(NODE *),
	*getlr(NODE *p, int);

void eoftn(int regs, int autos, int retlab);
void prologue(int regs, int autos);
void setlocc(int locctr);
void defname(char *name, int visib);
int e2print(NODE *p, int down, int *a, int *b);
int canaddr(NODE *);

extern	char *rnames[];

extern	int lineno;
extern	int fldshf, fldsz;
extern	int lflag, x2debug, udebug, e2debug, odebug, mdebug;
extern	int rdebug, radebug, t2debug, s2debug;
#ifdef FORT
extern	int Oflag;
#endif

#ifndef callchk
#define callchk(x) allchk()
#endif

#ifndef PUTCHAR
#define PUTCHAR(x) putchar(x)
#endif

#define optype(o)	(dope[o]&TYFLG)
#define asgop(o)	(dope[o]&ASGFLG) 
#define logop(o)	(dope[o]&LOGFLG)
#define callop(o)	(dope[o]&CALLFLG)
extern	int dope[];	/* a vector containing operator information */
extern	char *opst[];	/* a vector containing names for ops */

	/* macros for doing double indexing */
#define R2PACK(x,y,z)	(0200*((x)+1)+y+040000*z)
#define R2UPK1(x)	((((x)>>7)-1)&0177)
#define R2UPK2(x)	((x)&0177)
#define R2UPK3(x)	(x>>14)
#define R2TEST(x)	((x)>=0200)

#endif

