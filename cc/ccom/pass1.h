/*	pass1.h	4.2	85/08/22	*/

#ifndef _PASS1_
#define	_PASS1_

#include <sys/types.h>

#include "macdefs.h"
#include "manifest.h"
#include "cgram.h"

#include "protos.h"

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
 * Colliding entries are moved down with a standard
 * probe (no quadratic rehash here) and moved back when
 * entries are cleared.
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
	union	dimfun *sdf;	/* ptr to the dimension/prototype array */
	struct	suedef *ssue;	/* ptr to the definition table */
	int	suse;		/* line number of last use of the variable */
	int	s_argn;		/* Index to prototype nodes */
};
extern struct symtab *stab;

#define	snext	hdr.h_next
#define	soffset	hdr.h_offset
#define	sclass	hdr.h_sclass
#define	slevel	hdr.h_slevel
#define	sflags	hdr.h_sflags

/*
 * Storage classes
 */
#define SNULL		0		/* initial value */
#define AUTO		1		/* automatic (on stack) */
#define EXTERN		2		/* external reference */
#define STATIC		3		/* static scope */
#define REGISTER	4		/* register requested */
#define EXTDEF		5		/* external definition */
/* #define LABEL	6*/		/* label definition */
/* #define ULABEL	7*/		/* undefined label reference */
#define MOS		8		/* member of structure */
#define PARAM		9		/* parameter */
#define STNAME		10		/* structure name */
#define MOU		11		/* member of union */
#define UNAME		12		/* union name */
#define TYPEDEF		13		/* typedef name */
#define FORTRAN		14		/* fortran function */
#define ENAME		15		/* enumeration name */
#define MOE		16		/* member of enumeration */
#define UFORTRAN 	17		/* undefined fortran reference */
#define USTATIC		18		/* undefined static reference */
#define ILABEL		19		/* internal label */

/* field size is ORed in */
#define FIELD		0100
#define FLDSIZ		077
#ifndef BUG1
extern	char *scnames(int);
#endif

/*
 * Symbol table flags
 */
#define	SNORMAL		0		/* Symbol can be hidden */
#define	STAGNAME	01		/* Struct tag name */
#define	SLBLNAME	02		/* Goto labels */
#define	SMOSNAME	03		/* Member of struct */
#define	SSTRING		04		/* Read-only string */
#define	NSTYPES		05
#define	SMASK		07

#define SSET		010		/* symbol assigned to */
#define SREF		020		/* symbol referenced */
#define SNOCREAT	040		/* do not create symbol */
#define STEMP		0100		/* allocate in temp memory */
#define	SDYNARRAY	0200		/* dynamic allocated struct */
#define	SLABEL		0400		/* is an internal label */

/*
 * Location counters
 */
#define PROG		0		/* program segment */
#define DATA		1		/* data segment */
#define ADATA		2		/* array data segment */
#define STRNG		3		/* string data segment */
#define ISTRNG		4		/* initialized string segment */
#define STAB		5		/* symbol table segment */
#define	BSS		6		/* Shared uninitialized segment */

#ifndef FIXDEF
#define FIXDEF(p)
#endif
#ifndef FIXARG
#define FIXARG(p)
#endif
#ifndef FIXSTRUCT
#define FIXSTRUCT(a,b)
#endif

	/* alignment of initialized quantities */
#ifndef AL_INIT
#define	AL_INIT ALINT
#endif

#define	MKSUE(type)  (struct suedef *)&btdim[type]

/*
 * External definitions
 */
struct sw {		/* switch table */
	CONSZ	sval;	/* case value */
	int	slab;	/* associated label */
};
extern	struct sw swtab[];
extern	struct sw *swp;
extern	int swx;

extern	int ftnno;
extern	int blevel;
extern	int instruct, got_type;
extern	int oldstyle;

extern	int lineno, nerrors;

extern	CONSZ lastcon;
extern	float fcon;
extern	double dcon;

extern	char ftitle[];
extern	char ititle[];
extern	struct symtab *cftnsp;
extern	int btdim[];
extern	int autooff, argoff, strucoff;
extern	int regvar;
extern	int minrvar;
extern	int brkflag;
typedef union {
	int intval;
	NODE *nodep;
	struct symtab *symp;
	char *strp;
	struct rstack *rp;
} YYSTYPE;
extern	YYSTYPE yylval;
extern	char *yytext;

extern	OFFSZ inoff;

extern	int reached;
extern	int isinlining;

/* tunnel to buildtree for name id's */
extern	struct symtab *spname;

extern	NODE node[];
extern	NODE *lastfree;

extern	int cflag, pflag, sdebug;

/* various labels */
extern	int brklab;
extern	int contlab;
extern	int flostat;
extern	int retlab;
extern	int retstat;

/* declarations of various functions */
extern	NODE
	*buildtree(int, NODE *l, NODE *r),
	*mkty(unsigned, union dimfun *, struct suedef *),
	*rstruct(char *, int),
	*dclstruct(struct rstack *),
	*strend(char *),
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
	*makety(NODE *p, TWORD t, union dimfun *, struct suedef *),
	*block(int, NODE *, NODE *r, TWORD, union dimfun *, struct suedef *),
	*doszof(NODE *),
	*talloc(void),
	*optim(NODE *),
	*fixargs(NODE *),
	*clocal(NODE *),
	*doacall(NODE *f, NODE *a);

OFFSZ	tsize(TWORD, union dimfun *, struct suedef *),
	psize(NODE *);
NODE *	typenode(NODE *new);
void	spalloc(NODE *, NODE *, OFFSZ);
char	*exname(char *);

void deflabel(char *);
void gotolabel(char *);
int esccon(char **sptr);
void inline_start(char *name);
void inline_end(void);
void inline_savenode(NODE *);
void inline_savestring(char *);
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

void p1print(char *fmt, ...);

/* Function calls for argument type checking */
void proto_enter(struct symtab *, NODE **);/* Enter prototype for a function */
void proto_adapt(struct symtab *, NODE *); /* Match and convert parameters */

#ifdef PCC_DEBUG
void checkst(int);
#else
#define checkst(x)
#endif

#ifndef CHARCAST
/* to make character constants into character connstants */
/* this is a macro to defend against cross-compilers, etc. */
#define CHARCAST(x) (char)(x)
#endif
#endif

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

/*
 * Flags used for return status
 */
#define RETVAL		1
#define NRETVAL		2

#define NOOFFSET	(-10201)	/* mark an offset which is undefined */

/*
 * C compiler first pass extra defines.
 */
#define	QUALIFIER	(MAXOP+1)
#define	CLASS		(MAXOP+2)
#define	RB		(MAXOP+3)
#define	DOT		(MAXOP+4)
#define	ELLIPSIS	(MAXOP+5)
#define	ARGNODE		(MAXOP+6)
