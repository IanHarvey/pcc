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
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "ftypes.h"
#include "defines.h"

#include "macdefs.h"

#define VL 6

#define MAXINCLUDES 10
#define MAXLITERALS 20
#define MAXCTL 20
#define MAXHASH 401
#define MAXSTNO 201
#define MAXEXT 200
#define MAXEQUIV 150
#define MAXLABLIST 100

#ifdef NEWSTR
typedef struct bigblock *bigptr;
#else
typedef union expression *expptr;
typedef union taggedblock *tagptr;
#endif
typedef union chainedblock *chainp;

extern FILEP infile;
extern FILEP diagfile;
extern FILEP textfile;
extern FILEP asmfile;
extern FILEP initfile;
extern long int headoffset;

extern char token [ ];
extern int toklen;
extern int lineno;
extern char *infname;
extern int needkwd;
extern struct labelblock *thislabel;

extern flag profileflag;
extern flag optimflag;
extern flag nowarnflag;
extern flag ftn66flag;
extern flag shiftcase;
extern flag undeftype;
extern flag shortsubs;
extern flag onetripflag;
extern flag checksubs;
extern flag debugflag;
extern int nerr;
extern int nwarn;
extern int ndata;

extern int parstate;
extern flag headerdone;
extern int blklevel;
extern flag saveall;
extern flag substars;
extern int impltype[ ];
extern int implleng[ ];
extern int implstg[ ];

extern int tyint;
extern int tylogical;
extern ftnint typesize[];
extern int typealign[];
extern int procno;
extern int proctype;
extern char * procname;
extern int rtvlabel[ ];
extern int fudgelabel;	/* to confuse the pdp11 optimizer */
extern struct addrblock *typeaddr;
extern struct addrblock *retslot;
extern int cxslot;
extern int chslot;
extern int chlgslot;
extern int procclass;
extern ftnint procleng;
extern int nentry;
extern flag multitype;
extern int blklevel;
extern int lastlabno;
extern int lastvarno;
extern int lastargslot;
extern int argloc;
extern ftnint autoleng;
extern ftnint bssleng;
extern int retlabel;
extern int ret0label;
extern int dorange;
extern int regnum[ ];
extern struct nameblock *regnamep[ ];
extern int maxregvar;
extern int highregvar;
extern int nregvar;

extern chainp templist;
extern chainp holdtemps;
extern struct entrypoint *entries;
extern struct rplblock *rpllist;
extern chainp curdtp;
extern ftnint curdtelt;
extern flag toomanyinit;

extern flag inioctl;
extern int iostmt;
extern struct bigblock *ioblkp;
extern int nioctl;
extern int nequiv;
extern int nintnames;
extern int nextnames;

struct chain
	{
	chainp nextp;
#ifdef NEWSTR
	bigptr datap;
#else
	tagptr datap;
#endif
	};

extern chainp chains;

struct ctlframe
	{
	unsigned ctltype:8;
	unsigned dostepsign:8;
	int ctlabels[4];
	int dolabel;
	struct nameblock *donamep;
#ifdef NEWSTR
	bigptr domax;
	bigptr dostep;
#else
	expptr domax;
	expptr dostep;
#endif
	};
#define endlabel ctlabels[0]
#define elselabel ctlabels[1]
#define dobodylabel ctlabels[1]
#define doposlabel ctlabels[2]
#define doneglabel ctlabels[3]
extern struct ctlframe ctls[ ];
extern struct ctlframe *ctlstack;
extern struct ctlframe *lastctl;

struct extsym
	{
	char extname[XL];
	unsigned extstg:4;
	unsigned extsave:1;
	unsigned extinit:1;
	ptr extp;
	ftnint extleng;
	ftnint maxleng;
	};

extern struct extsym extsymtab[ ];
extern struct extsym *nextext;
extern struct extsym *lastext;

struct labelblock
	{
	int labelno;
	unsigned blklevel:8;
	unsigned labused:1;
	unsigned labinacc:1;
	unsigned labdefined:1;
	unsigned labtype:2;
	ftnint stateno;
	};

extern struct labelblock labeltab[ ];
extern struct labelblock *labtabend;
extern struct labelblock *highlabtab;

struct entrypoint
	{
	chainp nextp;
	struct extsym *entryname;
	chainp arglist;
	int entrylabel;
	int typelabel;
	ptr enamep;
	};

struct primblock
	{
	unsigned tag:4;
	unsigned vtype:4;
#ifdef NEWSTR
	struct bigblock *namep;
	struct bigblock *argsp;
	bigptr fcharp;
	bigptr lcharp;
#else
	struct nameblock *namep;
	struct listblock *argsp;
	expptr fcharp;
	expptr lcharp;
#endif
	};


struct hashentry
	{
	int hashval;
#ifdef NEWSTR
	struct bigblock *varp;
#else
	struct nameblock *varp;
#endif
	};
extern struct hashentry hashtab[ ];
extern struct hashentry *lasthash;

struct intrpacked	/* bits for intrinsic function description */
	{
	unsigned f1:3;
	unsigned f2:4;
	unsigned f3:7;
	};

struct nameblock
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	unsigned vstg:4;
	expptr vleng;
#endif
	char varname[VL];
	unsigned vdovar:1;
	unsigned vdcldone:1;
	unsigned vadjdim:1;
	unsigned vsave:1;
	unsigned vprocclass:3;
	unsigned vregno:4;
	union	{
		int varno;
		chainp vstfdesc;	/* points to (formals, expr) pair */
		struct intrpacked intrdesc;	/* bits for intrinsic function */
		} vardesc;
	struct dimblock *vdim;
	int voffset;
	};


struct paramblock
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	expptr vleng;
#endif
	char varname[VL];
	bigptr paramval;
	} ;


struct exprblock
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	expptr vleng;
#endif
	unsigned opcode:6;
#ifdef NEWSTR
	bigptr leftp;
	bigptr rightp;
#else
	expptr leftp;
	expptr rightp;
#endif
	};

struct dcomplex {
	double dreal, dimag;
};

union constant
	{
	char *ccp;
	ftnint ci;
	double cd[2];
	struct dcomplex dc;
	};

struct constblock
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned vtype:4;
#ifdef NEWSTR
	struct constblock *cb_ptr;
#else
	expptr vleng;
#endif
#endif
	union constant fconst;
	};


struct listblock
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned vtype:4;
#endif
	chainp listp;
	};



struct addrblock
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	unsigned vstg:4;
	expptr vleng;
#endif
	int memno;
#ifdef NEWSTR
	bigptr memoffset;
#else
	expptr memoffset;
#endif
	unsigned istemp:1;
	unsigned ntempelt:10;
	};



struct errorblock
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned vtype:4;
#else
	int pad;
#endif
	};


#ifndef NEWSTR
union expression
	{
	struct exprblock exprblock;
	struct addrblock addrblock;
	struct constblock constblock;
	struct errorblock errorblock;
	struct listblock listblock;
	struct primblock primblock;
	} ;
#endif


struct dimblock
	{
	int ndim;
#ifdef NEWSTR
	bigptr nelt;
	bigptr baseoffset;
	bigptr basexpr;
#else
	expptr nelt;
	expptr baseoffset;
	expptr basexpr;
#endif
	struct
		{
#ifdef NEWSTR
		bigptr dimsize;
		bigptr dimexpr;
#else
		expptr dimsize;
		expptr dimexpr;
#endif
		} dims[1];
	};


struct impldoblock  /* XXXX */
	{
#ifndef NEWSTR
	unsigned tag:4;
	unsigned isactive:1;
	unsigned isbusy:1;
#endif
#define	isactive vtype
#define isbusy vclass
	struct nameblock *varnp;
	struct constblock *varvp;
#ifdef NEWSTR
	bigptr implb;
	bigptr impub;
	bigptr impstep;
#else
	expptr implb;
	expptr impub;
	expptr impstep;
#endif
	ftnint impdiff;
	ftnint implim;
	chainp datalist;
	};


struct rplblock	/* name replacement block */
	{
	chainp nextp;
	struct bigblock *rplnp;
	ptr rplvp;
	struct bigblock *rplxp;
	int rpltag;
	};



struct equivblock
	{
	ptr equivs;
	unsigned eqvinit:1;
	long int eqvtop;
	long int eqvbottom;
	} ;
#define eqvleng eqvtop

extern struct equivblock eqvclass[ ];


struct eqvchain
	{
	chainp nextp;
	ptr eqvitem;
	long int eqvoffset;
	} ;

union chainedblock
	{
	struct chain chain;
	struct entrypoint entrypoint;
	struct rplblock rplblock;
	struct eqvchain eqvchain;
	};


#ifndef NEWSTR
union taggedblock
	{
	struct nameblock nameblock;
	struct paramblock paramblock;
	struct exprblock exprblock;
	struct constblock constblock;
	struct listblock listblock;
	struct addrblock addrblock;
	struct errorblock errorblock;
	struct primblock primblock;
	struct impldoblock impldoblock;
	} ;
#endif

#ifdef NEWSTR
struct bigblock {
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	unsigned vstg:4;
	bigptr vleng;
	union {
		struct exprblock _expr;
		struct addrblock _addr;
		struct constblock _const;
		struct errorblock _error;
		struct listblock _list;
		struct primblock _prim;
		struct nameblock _name;
		struct paramblock _param;
		struct impldoblock _impldo;
	} _u;
#define	b_expr		_u._expr
#define	b_addr		_u._addr
#define	b_const		_u._const
#define	b_error		_u._error
#define	b_list		_u._list
#define	b_prim		_u._prim
#define	b_name		_u._name
#define	b_param		_u._param
#define	b_impldo	_u._impldo
};
#endif

struct literal
	{
	short littype;
	short litnum;
	union	{
		ftnint litival;
		double litdval;
		struct	{
			char litclen;	/* small integer */
			char litcstr[XL];
			} litcval;
		} litval;
	};

extern struct literal litpool[ ];
extern int nliterals;





/* popular functions with non integer return values */
#define	expptr bigptr
#define	tagptr bigptr

ptr cpblock(int ,char *);

int *ckalloc(int);
char *varstr(int, char *), *nounder(int, char *), *varunder(int, char *);
char *copyn(int, char *), *copys(char *);
chainp hookup(chainp, chainp), mkchain(int, int);
ftnint convci(int, char *), iarrlen(struct bigblock *q);
ftnint lmin(ftnint, ftnint), lmax(ftnint, ftnint);
ftnint simoffset(expptr *);
char *memname(int, int), *convic(ftnint), *setdoto(char *);
double convcd(int, char *);
struct extsym *mkext(char *), 
#ifdef NEWSTR
	*newentry(struct bigblock *),
#else
	*newentry(struct nameblock *),
#endif
	*comblock(int, char *s);
#ifdef NEWSTR
struct bigblock *mkname(int, char *);
#else
struct nameblock *mkname(int, char *);
#endif
struct labelblock *mklabel(ftnint);
struct bigblock *addrof(expptr), *call1(int, char *, expptr),
	*call2(int, char *, expptr, expptr),
	*call3(int, char *, expptr, expptr, expptr),
	*call4(int, char *, expptr, expptr, expptr, expptr);
struct bigblock *call0(int, char *), *mkexpr(int, bigptr, bigptr);
struct bigblock *callk(int, char *, chainp);

struct bigblock *builtin(int, char *), *fmktemp(int, bigptr),
	*mktmpn(int, int, bigptr), *nextdata(ftnint *, ftnint *),
	*autovar(int, int, bigptr), *mklhs(struct bigblock *),
	*mkaddr(struct bigblock *), *putconst(struct bigblock *),
	*memversion(struct bigblock *);
struct bigblock *mkscalar(struct bigblock *np);
struct bigblock *realpart(struct bigblock *p);
struct bigblock *imagpart(struct bigblock *p);

struct bigblock *mkintcon(ftnint), *mkbitcon(int, int, char *),
	*mklogcon(int), *mkaddcon(int), *mkrealcon(int, double),
	*mkstrcon(int, char *), *mkcxcon(bigptr,bigptr);
struct bigblock *mkconst(int t);

struct bigblock *mklist(chainp p);
struct bigblock *mkiodo(chainp, chainp);


bigptr mkconv(int, bigptr),
	mkfunct(struct bigblock *), fixexpr(struct bigblock *),
	fixtype(bigptr);


union uuu { struct bigblock paramblock; struct bigblock nameblock; };
#ifdef NEWSTR
bigptr cpexpr(bigptr), mkprim(union uuu *, struct bigblock *, bigptr, bigptr);
struct bigblock *mkarg(int, int);
#else
tagptr cpexpr(tagptr), mkprim(union uuu *, struct listblock *, expptr, expptr);
struct addrblock *mkarg(int, int);
#endif
struct bigblock *errnode(void);
void initkey(void), prtail(void), puteof(void), done(int);
void fileinit(void), procinit(void), endproc(void), doext(void), preven(int);
int inilex(char *), yyparse(void), newlabel(void), lengtype(int, int);
void err(char *, ...), warn(char *, ...), fatal(char *, ...), enddcl(void);
void clf(FILEP *p), p2pass(char *s), frexpr(tagptr p), execerr(char *, ...);
void setimpl(int, ftnint, int, int), setlog(void), newproc(void);
void prdbginfo(void), impldcl(struct bigblock *p);
void putbracket(void), enddcl(void), doequiv(void);
void puthead(char *), startproc(struct extsym *, int);
#ifdef NEWSTR
void dclerr(char *s, struct bigblock *v), putforce(int, bigptr);
#else
void dclerr(char *s, struct nameblock *v), putforce(int, expptr);
#endif
void entrypt(int, int, ftnint, struct extsym *, chainp);
void settype(struct bigblock *, int, int), putlabel(int);
void putbranch(struct bigblock *p), goret(int), putrbrack(int);
void prolog(struct entrypoint *, struct bigblock *), prendproc(void);
void prlocvar(char *, ftnint), prext(char *, ftnint, int);
void vardcl(struct bigblock *v), frchain(chainp *p); 
void frtemp(struct bigblock *p), incomm(struct extsym *, struct bigblock *);
void setintr(struct bigblock * v), setext(struct bigblock * v);
struct uux { expptr lb, ub; };
void setbound(struct bigblock *, int, struct uux []);
void setfmt(struct labelblock *lp), frdata(chainp), frrpl(void),
	dataval(struct bigblock *, struct bigblock *),
	consnegop(struct bigblock *p), exdo(int, chainp), exelse(void),
	exendif(void), exif(bigptr), exelif(bigptr),
	exequals(struct bigblock *, bigptr),
	exassign(struct bigblock *, struct labelblock *),
	exarif(bigptr, struct labelblock *, struct labelblock *,
	    struct labelblock *);



int intrfunct(char s[VL]), eqn(int, char *, char *);
int fmtstmt(struct labelblock *lp);
int cktype(int, int, int);
int yylex(void), inregister(struct bigblock *);
int inilex(char *), iocname(void);
int maxtype(int, int), log2(ftnint), hextoi(int);
int cmpstr(char *, char *, ftnint, ftnint);
int enregister(struct bigblock *np);
int conssgn(bigptr p);
int fixargs(int, struct bigblock *);
int addressable(bigptr p);

void prlabel(FILEP, int);
void prconi(FILEP, int, ftnint);
void mvarg(int, int, int);
void prsave(void);
void prcona(FILEP, ftnint);
void prconr(FILEP, int, float);
void prarif(bigptr, int, int, int);
void putstr(FILEP, char *, ftnint);
void putex1(bigptr p);
void putassign(bigptr, bigptr);
void puteq(bigptr, bigptr);
void putsteq(bigptr, bigptr);
void popstack(chainp *p); 
void consconv(int, union constant *, int, union constant *);
void yyerror(char *s);
void enddo(int);
void doinclude(char *);
void flline(void);
void startioctl(void);
void endioctl(void), endio(void), ioclause(int, bigptr), doio(chainp);
void excall(struct hashentry *, struct bigblock *, int, struct labelblock *[]);
void exreturn(expptr p);
void exstop(int, expptr);
void exgoto(struct labelblock *);
void exasgoto(struct hashentry *);
void putcmgo(expptr, int, struct labelblock *[]);
void putexpr(expptr p);
void putif(expptr, int);
void startrw(void);
void putgoto(int);
void mkstfunct(struct bigblock *, bigptr);
void deregister(struct bigblock *np);
void p2flush(void);
void p2word(long int w);
void p2name(char *s);
void p2triple(int, int, int);
void p2str(char *s);
void p2reg(int, int);
void p2oreg(ftnint, int, int);
void p2icon(ftnint, int);
void p2op(int, int);
void putx(expptr p);
void cpn(int, char *, char *);
void prhead(FILEP fp);
void prcmgoto(expptr, int, int, int);
void putstmt(void);
char *lexline(ftnint *n);
bigptr subcheck(struct bigblock *, bigptr), suboffset(struct bigblock *p);


#undef expptr
#undef tagptr

#define	err1 err
#define err2 err
#define	warn1 warn
#define	fatal1 fatal
