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
#include "ccconfig.h"

/*
 * Storage classes
 */
#define SNULL		0
#define AUTO		1
#define EXTERN		2
#define STATIC		3
#define REGISTER	4
#define EXTDEF		5
/* #define LABEL	6*/
/* #define ULABEL	7*/
#define MOS		8
#define PARAM		9
#define STNAME		10
#define MOU		11
#define UNAME		12
#define TYPEDEF		13
#define FORTRAN		14
#define ENAME		15
#define MOE		16
#define UFORTRAN 	17
#define USTATIC		18
#define ILABEL		19

	/* field size is ORed in */
#define FIELD		0100
#define FLDSIZ		077
extern	char *scnames(int);

/*
 * Symbol table flags
 */
#define	SNORMAL		0
#define	STAGNAME	01
#define	SLBLNAME	02
#define	SMOSNAME	03
#define	SSTRING		04
#define	NSTYPES		05
#define	SMASK		07

#define SSET		00010
#define SREF		00020
#define SNOCREAT	00040
#define STEMP		00100
#define	SDYNARRAY	00200
#define	STNODE		01000
#ifdef GCC_COMPAT
#define	SRENAME		02000	/* Node is renamed */
#endif

#ifndef FIXDEF
#define FIXDEF(p)
#endif

	/* alignment of initialized quantities */
#ifndef AL_INIT
#define	AL_INIT ALINT
#endif

struct rstack;
struct symtab;
union arglist;
/*
 * Dimension/prototype information.
 */
union dimfun {
	int	ddim;		/* Dimension of an array */
	union arglist *dfun;	/* Prototype index */
};

/*
 * Struct/union/enum definition.
 * The first element (size) is used for other types as well.
 */
struct suedef {
	int	suesize;	/* Size of the struct */
	struct	symtab **suelem;/* points to the list of elements */
	int	suealign;	/* Alignment of this struct */
};

/*
 * Symbol table definition.
 *
 * The symtab_hdr struct is used to save label info in NAME and ICON nodes.
 */
struct symtab_hdr {
	struct	symtab *h_next;	/* link to other symbols in the same scope */
	int	h_offset;	/* offset or value */
	char	h_sclass;	/* storage class */
	char	h_slevel;	/* scope level */
	short	h_sflags;		/* flags, see below */
};

struct	symtab {
	struct	symtab_hdr hdr;
	char	*sname;
	TWORD	stype;		/* type word */
	TWORD	squal;		/* qualifier word */
	union	dimfun *sdf;	/* ptr to the dimension/prototype array */
	struct	suedef *ssue;	/* ptr to the definition table */
	int	suse;		/* line number of last use of the variable */
};

#define	snext	hdr.h_next
#define	soffset	hdr.h_offset
#define	sclass	hdr.h_sclass
#define	slevel	hdr.h_slevel
#define	sflags	hdr.h_sflags

#define	MKSUE(type)  (struct suedef *)&btdim[type]

/*
 * External definitions
 */
struct swents {			/* switch table */
	struct swents *next;	/* Next struct in linked list */
	CONSZ	sval;		/* case value */
	int	slab;		/* associated label */
};
void genswitch(struct swents **, int);

extern	int ftnno;
extern	int blevel;
extern	int instruct, got_type;
extern	int oldstyle;

extern	int lineno, nerrors;

extern	char *ftitle;
extern	struct symtab *cftnsp;
extern	int btdim[];
extern	int autooff, argoff, strucoff;
extern	int regvar;
extern	int minrvar;
extern	int brkflag;
extern	int lastloc;
#if 0
typedef union {
	int intval;
	NODE *nodep;
	struct symtab *symp;
	struct rstack *rp;
	char *strp;
	struct stri {
		char *str;
		int len;
	} stri;
} YYSTYPE;
extern	YYSTYPE yylval;
#endif

struct stri {
	char *str;
	int len;
} stri;

extern	OFFSZ inoff;

extern	int reached;
extern	int isinlining;

/* 	tunnel to buildtree for name id's */

extern	struct symtab *spname;

extern	int sdebug;

/* various labels */
extern	int brklab;
extern	int contlab;
extern	int flostat;
extern	int retlab;

/*
 * Flags used in structures/unions
 */
#define INSTRUCT	02
#define INUNION		04

/*
 * Flags used in the (elementary) flow analysis ...
 */
#define FBRK		02
#define FCONT		04
#define FDEF		010
#define FLOOP		020

/*	mark an offset which is undefined */

#define NOOFFSET	(-10201)

/* declarations of various functions */
extern	NODE
	*buildtree(int, NODE *l, NODE *r),
	*mkty(unsigned, union dimfun *, struct suedef *),
	*rstruct(char *, int),
	*dclstruct(struct rstack *),
	*strend(struct stri *),
	*tymerge(NODE *typ, NODE *idp),
	*stref(NODE *),
	*offcon(OFFSZ, TWORD, union dimfun *, struct suedef *),
	*bcon(int),
	*bpsize(NODE *),
	*convert(NODE *, int),
	*pconvert(NODE *),
	*oconvert(NODE *),
	*ptmatch(NODE *),
	*tymatch(NODE *),
	*makety(NODE *, TWORD, TWORD, union dimfun *, struct suedef *),
	*block(int, NODE *, NODE *r, TWORD, union dimfun *, struct suedef *),
	*doszof(NODE *),
	*talloc(void),
	*optim(NODE *),
	*clocal(NODE *),
	*ccopy(NODE *),
	*btsize(TWORD, union dimfun *, struct suedef *),
	*tempnode(int *, TWORD type, union dimfun *df, struct suedef *sue),
	*doacall(NODE *f, NODE *a);
OFFSZ	tsize(TWORD, union dimfun *, struct suedef *),
	psize(NODE *);
NODE *	typenode(NODE *new);
void	spalloc(NODE *, NODE *, OFFSZ);
char	*exname(char *);

int oalloc(struct symtab *p, int *poff);
void deflabel(char *);
void deflab1(int);
void setloc1(int);
void gotolabel(char *);
int esccon(char **sptr);
void inline_start(char *name);
void inline_end(void);
void inline_addarg(struct interpass *);
void inline_ref(char *);
void inline_prtout(void);
void ftnarg(NODE *);
struct rstack *bstruct(char *, int);
void moedef(char *);
void beginit(struct symtab *, int);
struct symtab *lookup(char *name, int s);
struct symtab *getsymtab(char *name, int flags);
char *addstring(char *);
char *addname(char *);
char *newstring(char *, int len);
void symclear(int level);
void schedremove(struct symtab *p);
struct symtab *hide(struct symtab *p);
int talign(unsigned int, struct suedef *);
void bfcode(struct symtab **, int);
int chkftn(union arglist *, union arglist *);
void branch(int);
void cbranch(NODE *p, NODE *q);
void commdec(struct symtab *);
void lcommdec(struct symtab *);
void finval(NODE *);
int falloc(struct symtab *p, int w, int new, NODE *pty);
TWORD ctype(TWORD);  
void inval(CONSZ);
void ninval(NODE *);
void defnam(struct symtab *);
void plabel(int lab);
void bjobcode(void);
void ejobcode(int);

void p1print(char *fmt, ...);
char *copst(int);
int cdope(int);
void myp2tree(NODE *);

#ifdef GCC_COMPAT
void gcc_init(void);
int gcc_keyword(char *, NODE **);
void gcc_rename(struct symtab *sp, char *newname);
char *gcc_findname(struct symtab *sp);
#endif

#ifdef STABS
void stabs_init(void);
void stabs_file(char *);
void stabs_line(int);
void stabs_rbrac(int);
void stabs_lbrac(int);
void stabs_func(struct symtab *);
void stabs_newsym(struct symtab *);
void stabs_chgsym(struct symtab *);
void stabs_struct(struct symtab *p, struct suedef *sue);
#endif

#ifndef CHARCAST
/* to make character constants into character connstants */
/* this is a macro to defend against cross-compilers, etc. */
#define CHARCAST(x) (char)(x)
#endif

/*
 * C compiler first pass extra defines.
 */
#define	QUALIFIER	(MAXOP+1)
#define	CLASS		(MAXOP+2)
#define	RB		(MAXOP+3)
#define	DOT		(MAXOP+4)
#define	ELLIPSIS	(MAXOP+5)
#define	TYPE		(MAXOP+6)
#define	LB		(MAXOP+7)
#define	COMOP		(MAXOP+8)
#define	QUEST		(MAXOP+9)
#define	COLON		(MAXOP+10)
#define	ANDAND		(MAXOP+11)
#define	OROR		(MAXOP+12)
#define	NOT		(MAXOP+13)
#define	CAST		(MAXOP+14)
/* #define	STRING		(MAXOP+15) */

/* The following must be in the same order as their NOASG counterparts */
#define	PLUSEQ		(MAXOP+16)
#define	MINUSEQ		(MAXOP+17)
#define	DIVEQ		(MAXOP+18)
#define	MODEQ		(MAXOP+19)
#define	MULEQ		(MAXOP+20)
#define	ANDEQ		(MAXOP+21)
#define	OREQ		(MAXOP+22)
#define	EREQ		(MAXOP+23)
#define	LSEQ		(MAXOP+24)
#define	RSEQ		(MAXOP+25)

#define	UNASG		(-(PLUSEQ-PLUS))+

#define coptype(o)	(cdope(o)&TYFLG)
#define clogop(o)	(cdope(o)&LOGFLG)
#define casgop(o)	(cdope(o)&ASGFLG)

