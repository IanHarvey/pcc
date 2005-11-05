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
#include <sys/types.h>

#include "manifest.h"
#include "protos.h"
#ifndef MKEXT
#include "external.h"
#else
typedef int bittype; /* XXX - for basicblock */
#endif

/* cookies, used as arguments to codgen */
#define FOREFF	01		/* compute for effects only */
#define INAREG	02		/* compute into a register */
#define INBREG	04		/* compute into a register */
#ifdef MULTICLASS
#define INCREG	010		/* compute into a register */
#define INDREG	020		/* compute into a register */
#define	INREGS	(INAREG|INBREG|INCREG|INDREG)
#else
#define INTAREG	010		/* compute into a scratch register */
#define INTBREG 020		/* compute into a scratch lvalue register */
#endif
#define FORCC	040		/* compute for condition codes only */
#define INTEMP	010000		/* compute into a temporary location */
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

/* shapes */
#define SANY	01		/* same as FOREFF */
#define SAREG	02		/* same as INAREG */
#define SBREG	04		/* same as INBREG */
#ifdef MULTICLASS
#define SCREG	010		/* same as INCREG */
#define SDREG	020		/* same as INDREG */
#endif
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

/* tshape() return values */
#define	SRNOPE	0		/* Cannot match any shape */
#define	SRDIR	1		/* Direct match */
#define	SROREG	2		/* Can convert into OREG */
#define	SRREG	3		/* Must put into REG */

#ifdef MULTICLASS
/* find*() return values */
#define	FRETRY	0
#define	FFAIL	-1
#endif

/* INTEMP is carefully not conflicting with shapes */

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
#define	TLDOUBLE	0200000	/* long double; exceeds 16 bit */
#define	TFTN		0400000	/* function pointer; exceeds 16 bit */

/* reclamation cookies */
#define RNULL		0	/* clobber result */
#define RLEFT		01
#define RRIGHT		02
#define RESC1		04
#define RESC2		010
#define RESC3		020
#define RDEST		040
#define RESCC		04000
#define RNOP		010000	/* DANGER: can cause loops.. */

/* needs */
#define NAREG		000001
#define NACOUNT		000003
#define NAMASK		000017
#define NASL		000004	/* may share left register */
#define NASR		000010	/* may share right register */
#define NBREG		000020
#define NBCOUNT		000060
#define NBMASK		000360
#define NBSL		000100
#define NBSR		000200
#define NTEMP		000400
#define NTMASK		001400
#define NSPECIAL	002000	/* need special register treatment */
#define REWRITE		010000

#define MUSTDO		010000	/* force register requirements */
#define NOPREF		020000	/* no preference for register assignment */

#define	isbreg(r)	(REGBIT(r) & BREGS)

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

/* Special needs for register allocations */
struct rspecial {
	int *left;
	int *right;
	int *rmask;
	int *res;
};

extern	NODE resc[];

extern	int p2autooff, p2maxautooff;

extern	NODE
	*talloc(void),
	*eread(void),
	*tcopy(NODE *),
	*mklnode(int, CONSZ, int, TWORD),
	*mkbinode(int, NODE *, NODE *, TWORD),
	*mkunode(int, NODE *, int, TWORD),
	*getlr(NODE *p, int);

void eoftn(struct interpass_prolog *);
void prologue(struct interpass_prolog *);
void setlocc(int locctr);
int e2print(NODE *p, int down, int *a, int *b);
void myoptim(struct interpass *);
void cbgen(int op, int label);
struct optab *nxtmatch(struct optab *);
int chkmatch(NODE *, int, int, int);
int match(NODE *p, int cookie);
int nmatch(NODE *p, int what);
#ifndef special
int special(NODE *, int);
#endif
int setasg(NODE *, int);
int setuni(NODE *, int);
int sucomp(NODE *);
int nsucomp(NODE *);
#ifdef MULTICLASS
int geninsn(NODE *, int cookie);
#else
void geninsn(NODE *, int cookie);
#endif
void adrput(FILE *, NODE *);
void comperr(char *str, ...);
void genregs(NODE *p);
void ngenregs(struct interpass *);
NODE *store(NODE *);
void mygenregs(NODE *);
void gencall(NODE *, NODE *prev);
struct interpass *ipnode(NODE *);
void deflab(int);
void rmove(int, int, TWORD);
struct rspecial *nspecial(struct optab *);
void printip(struct interpass *pole);
int findops(NODE *p, int);
int findasg(NODE *p, int);
int finduni(NODE *p, int);
int findleaf(NODE *p, int);
int relops(NODE *p);
#ifdef MULTICLASS
int offstar(NODE *p, int shape);
#else
int offstar(NODE *p);
#endif

char *prcook(int);

void conput(FILE *, NODE *);

extern	char *rnames[];

#ifdef MULTICLASS
extern int classmask(int), tclassmask(int);
extern void cmapinit(void);
extern int aliasmap(int thisclass, int adjnum, int adjclass);
extern int regK[];
#define	CLASSA	1
#define	CLASSB	2
#define	CLASSC	3
#define	CLASSD	4
#define	CLASSE	5
#endif

extern	int lineno;
extern	int fldshf, fldsz;
extern	int lflag, x2debug, udebug, e2debug, odebug, mdebug;
extern	int rdebug, radebug, t2debug, s2debug, b2debug;
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

/*
 * Layout of findops() return value:
 *      bit 0-1 where to store left node.
 *      bit 2-3 where to store right node.
 *      bit 4   set if right leg should be evaluated first
 *      bit 5-  table index
 */
#define LREG		001
#define LOREG		002
#define LTEMP		003
#define LMASK		003
#define RREG		004
#define ROREG		010
#define RTEMP		014
#define RMASK		014
#define DORIGHT		020
#ifdef MULTICLASS
#define	SCLASS(v,x)	((v) |= ((x) << 5))
#define	TCLASS(x)	(((x) >> 5) & 7)
#define	TBSH		8
#else
#define	LBREG		040	/* XXX - left in breg */
#define TBSH		6
#endif
#define TBLIDX(idx)	((idx) >> TBSH)
#define MKIDX(tbl,mod)	(((tbl) << TBSH) | (mod))

#ifndef	BREGS
#define	BREGS	0
#define	TBREGS	0
#endif
#define	REGBIT(x) (1 << (x))

/* hardops definitions */
struct hardops {
	int op;		/* op to search for */
	TWORD type;	/* type not handled */
	char *fun;	/* function to call */
};
extern struct hardops hardops[];

void emit(struct interpass *);
void optimize(struct interpass *);

struct basicblock {
	DLIST_ENTRY(basicblock) bbelem;
	SLIST_HEAD(, cfgnode) children; /* CFG - children to this node */
	SLIST_HEAD(, cfgnode) parents; /* CFG - parents to this node */
	int bbnum;	/* this basic block number */
	unsigned int dfnum; /* DFS-number */
	unsigned int dfparent; /* Parent in DFS */
	unsigned int semi;
	unsigned int ancestor;
	unsigned int idom;
	unsigned int samedom;
	bittype *bucket;
	bittype *df;
	bittype *dfchildren;
	bittype *Aorig;
	bittype *Aphi;
	struct interpass *first; /* first element of basic block */
	struct interpass *last;  /* last element of basic block */
};

struct labelinfo {
	struct basicblock **arr;
	unsigned int size;
	unsigned int low;
};

struct bblockinfo {
	unsigned int size;
	struct basicblock **arr;
};

struct varinfo {
	struct pvarinfo **arr;
	int size;
	int low;
};

struct pvarinfo {
	struct pvarinfo *next;
	struct basicblock *bb;
	NODE *top, *n;
};

struct cfgnode {
	SLIST_ENTRY(cfgnode) cfgelem;
	struct basicblock *bblock;
};

/*
 * C compiler second pass extra defines.
 */
#define PHI (MAXOP + 1)		/* Used in SSA trees */
