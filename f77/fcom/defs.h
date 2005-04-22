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

typedef union expression *expptr;
typedef union taggedblock *tagptr;
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
extern struct addrblock *ioblkp;
extern int nioctl;
extern int nequiv;
extern int nintnames;
extern int nextnames;

struct chain
	{
	chainp nextp;
	tagptr datap;
	};

extern chainp chains;

struct ctlframe
	{
	unsigned ctltype:8;
	unsigned dostepsign:8;
	int ctlabels[4];
	int dolabel;
	struct nameblock *donamep;
	expptr domax;
	expptr dostep;
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
	struct nameblock *namep;
	struct listblock *argsp;
	expptr fcharp;
	expptr lcharp;
	};


struct hashentry
	{
	int hashval;
	struct nameblock *varp;
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
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	unsigned vstg:4;
	expptr vleng;
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
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	expptr vleng;
	char varname[VL];
	ptr paramval;
	} ;


struct exprblock
	{
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	expptr vleng;
	unsigned opcode:6;
	expptr leftp;
	expptr rightp;
	};


union constant
	{
	char *ccp;
	ftnint ci;
	double cd[2];
	};

struct constblock
	{
	unsigned tag:4;
	unsigned vtype:4;
	expptr vleng;
	union constant fconst;
	};


struct listblock
	{
	unsigned tag:4;
	unsigned vtype:4;
	chainp listp;
	};



struct addrblock
	{
	unsigned tag:4;
	unsigned vtype:4;
	unsigned vclass:4;
	unsigned vstg:4;
	expptr vleng;
	int memno;
	expptr memoffset;
	unsigned istemp:1;
	unsigned ntempelt:10;
	};



struct errorblock
	{
	unsigned tag:4;
	unsigned vtype:4;
	};


union expression
	{
	struct exprblock exprblock;
	struct addrblock addrblock;
	struct constblock constblock;
	struct errorblock errorblock;
	struct listblock listblock;
	struct primblock primblock;
	} ;



struct dimblock
	{
	int ndim;
	expptr nelt;
	expptr baseoffset;
	expptr basexpr;
	struct
		{
		expptr dimsize;
		expptr dimexpr;
		} dims[1];
	};


struct impldoblock
	{
	unsigned tag:4;
	unsigned isactive:1;
	unsigned isbusy:1;
	struct nameblock *varnp;
	struct constblock *varvp;
	expptr implb;
	expptr impub;
	expptr impstep;
	ftnint impdiff;
	ftnint implim;
	chainp datalist;
	};


struct rplblock	/* name replacement block */
	{
	chainp nextp;
	struct nameblock *rplnp;
	ptr rplvp;
	struct exprblock *rplxp;
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


int *ckalloc(int);
char *varstr(int, char *), *nounder(int, char *), *varunder(int, char *);
char *copyn(int, char *), *copys(char *);
chainp hookup(chainp, chainp), mkchain(int, int);
ftnint convci(int, char *), iarrlen(struct nameblock *q);
char *memname(int, int), *convic(ftnint), *setdoto(char *);
double convcd(int, char *);
struct extsym *mkext(char *), *newentry(struct nameblock *),
	*comblock(int, char *s);
struct nameblock *mkname(int, char *);
struct labelblock *mklabel(ftnint);
struct exprblock *addrof(expptr), *call1(int, char *, expptr),
	*call2(int, char *, expptr, expptr),
	*call3(int, char *, expptr, expptr, expptr),
	*call4(int, char *, expptr, expptr, expptr, expptr);
struct addrblock *builtin(int, char *), *fmktemp(int, expptr),
	*mktmpn(int, int, expptr), *nextdata(ftnint *, ftnint *),
	*autovar(int, int, expptr), *mklhs(struct primblock *),
	*mkaddr(struct nameblock *), *putconst(struct constblock *),
	*memversion(struct nameblock *);
struct constblock *mkintcon(ftnint), *mkbitcon(int, int, char *),
	*mklogcon(int), *mkaddcon(int), *mkrealcon(int, double),
	*mkstrcon(int, char *), *mkcxcon(expptr,expptr);
struct listblock *mklist(chainp p);
struct impldoblock *mkiodo(chainp, chainp);


expptr mkexpr(int, expptr, expptr), mkconv(int, expptr),
	mkfunct(struct primblock *), fixexpr(struct exprblock *),
	fixtype(tagptr);


union uuu { struct paramblock paramblock; struct nameblock nameblock; };
tagptr cpexpr(tagptr), mkprim(union uuu *, struct listblock *, expptr, expptr);
struct errorblock *errnode(void);
struct addrblock *mkarg(int, int);
void initkey(void), prtail(void), puteof(void), done(int);
void fileinit(void), procinit(void), endproc(void), doext(void), preven(int);
int inilex(char *), yyparse(void), newlabel(void), lengtype(int, int);
void err(char *, ...), warn(char *, ...), fatal(char *, ...), enddcl(void);
void clf(FILEP *p), p2pass(char *s), frexpr(tagptr p), execerr(char *, ...);
void setimpl(int, ftnint, int, int), setlog(void), newproc(void);
void prdbginfo(void), impldcl(struct nameblock *p);
void putbracket(void), enddcl(void), doequiv(void);
void puthead(char *), startproc(struct extsym *, int);
void dclerr(char *s, struct nameblock *v), putforce(int, expptr);
void entrypt(int, int, ftnint, struct extsym *, chainp);
void settype(struct nameblock *, int, int), putlabel(int);
void putbranch(struct addrblock *p), goret(int), putrbrack(int);
void prolog(struct entrypoint *, struct addrblock *), prendproc(void);
void prlocvar(char *, ftnint), prext(char *, ftnint, int);
void vardcl(struct nameblock *v), frchain(chainp *p); 
void frtemp(struct addrblock *p), incomm(struct extsym *, struct nameblock *);
void setintr(struct nameblock * v), setext(struct nameblock * v);
struct uux { expptr lb, ub; };
void setbound(struct nameblock *, int, struct uux []);
void setfmt(struct labelblock *lp), frdata(chainp), frrpl(void),
	dataval(struct constblock *, struct constblock *),
	consnegop(struct constblock *p), exdo(int, chainp), exelse(void),
	exendif(void), exif(expptr), exelif(expptr),
	exequals(struct primblock *, expptr),
	exassign(struct nameblock *, struct labelblock *),
	exarif(expptr, struct labelblock *, struct labelblock *,
	    struct labelblock *);



int intrfunct(char s[VL]), eqn(int, char *, char *);
int fmtstmt(struct labelblock *lp);



#define	err1 err
#define err2 err
#define	warn1 warn
#define	fatal1 fatal
