/*	pass1.h	4.2	85/08/22	*/

#ifndef _PASS1_
#define	_PASS1_

#include <sys/types.h>

#include "macdefs.h"
#include "manifest.h"

#include "protos.h"
/*
 * Symbol table definition.
 *
 * Colliding entries are moved down with a standard
 * probe (no quadratic rehash here) and moved back when
 * entries are cleared.
 */
struct svinfo;

struct	symtab {
	char	*sname;
	struct	symtab *snext;	/* link to other symbols in the same scope */
	TWORD	stype;		/* type word */
	char	sclass;		/* storage class */
	char	slevel;		/* scope level */
	short	sflags;		/* flags, see below */
	int	offset;		/* offset or value */
	short	dimoff;		/* offset into the dimension table */
	short	sizoff;		/* offset into the size table */
	int	suse;		/* line number of last use of the variable */
	int	s_argn;		/* Index to prototype nodes */
};

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

/* field size is ORed in */
#define FIELD		0100
#define FLDSIZ		077
#ifndef BUG1
extern	char *scnames(int);
#endif

/*
 * Symbol table flags
 */
#define SMOS		01		/* member of structure */
#define SHIDDEN		02		/* hidden in current scope */
#define SHIDES		04		/* hides symbol in outer scope */
#define SSET		010		/* symbol assigned to */
#define SREF		020		/* symbol referenced */
#define SNONUNIQ	040		/* non-unique structure member */
#define STAG		0100		/* structure tag name */

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
extern	int instruct, stwart;

extern	int lineno, nerrors;

extern	CONSZ lastcon;
extern	float fcon;
extern	double dcon;

extern	char ftitle[];
extern	char ititle[];
extern	struct symtab stab[];
extern	int curftn;
extern	int curclass;
extern	int curdim;
extern	int dimtab[];
extern	int paramstk[];
extern	int paramno;
extern	int autooff, argoff, strucoff;
extern	int regvar;
extern	int minrvar;
extern	int brkflag;
typedef union {
	int intval;
	NODE *nodep;
	struct symtab *symp;
	char *strp;
} YYSTYPE;
extern	YYSTYPE yylval;
extern	char *yytext;

extern	OFFSZ inoff;

extern	int reached;
extern	int isinlining;

/* tunnel to buildtree for name id's */
extern	int idname;

extern	NODE node[];
extern	NODE *lastfree;

extern	int cflag, pflag, sdebug;

/* various labels */
extern	int brklab;
extern	int contlab;
extern	int flostat;
extern	int retlab;
extern	int retstat;
extern	int asavbc[], *psavbc;
extern	struct symtab *schain[];

/* declarations of various functions */
extern	NODE
	*buildtree(int, NODE *l, NODE *r),
	*bdty(int, NODE *, int),
	*mkty(unsigned, int, int),
	*rstruct(int, int),
	*dclstruct(int),
	*strend(char *),
	*tymerge(NODE *typ, NODE *idp),
	*stref(NODE *),
	*offcon(OFFSZ, TWORD, int, int),
	*bcon(int),
	*bpsize(NODE *),
	*convert(NODE *, int),
	*pconvert(NODE *),
	*oconvert(NODE *),
	*ptmatch(NODE *),
	*tymatch(NODE *),
	*makety(NODE *p, TWORD t, int, int),
	*block(int, NODE *, NODE *r, TWORD, int, int),
	*doszof(NODE *),
	*talloc(void),
	*optim(NODE *),
	*fixargs(NODE *),
	*clocal(NODE *);
OFFSZ	tsize(TWORD, int, int),
	psize(NODE *);
NODE *	typenode(NODE *new);
char	*exname(char *);

void deflabel(int id);
void gotolabel(int id);
int esccon(char **sptr);
void inline_start(char *name);
void inline_end(void);
void inline_savenode(NODE *);
void inline_savestring(char *);
void inline_ref(char *);
void inline_prtout(void);

void p1print(char *fmt, ...);

/* Function calls for argument type checking */
void proto_enter(int, NODE **); /* Enter a prototype for a function */
void proto_adapt(struct symtab *, NODE *); /* match and convert parameters */

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
#define SEENAME		01
#define INSTRUCT	02
#define INUNION		04
#define FUNNYNAME	010
#define TAGNAME		020

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

#define NONAME		040000		/* marks constant w/o name field */
#define NOOFFSET	(-10201)	/* mark an offset which is undefined */
