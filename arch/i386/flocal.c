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

#include "macdefs.h"

#include "ftypes.h"
#include "defines.h"
#include "defs.h"

void
prchars(fp, s)
FILEP fp;
int *s;
{

fprintf(fp, ".byte 0%o,0%o\n", s[0], s[1]);
}


void
pruse(fp, s)
FILEP fp;
char *s;
{
fprintf(fp, "\t%s\n", s);
}


void
prskip(fp, k)
FILEP fp;
ftnint k;
{
fprintf(fp, "\t.space\t%ld\n", k);
}




void
prcomblock(fp, name)
FILEP fp;
char *name;
{
fprintf(fp, FLABELFMT, name);
}

#ifdef FCOM

#if OUTPUT==BINARY
#	include "scjdefs.h"
#endif

/*
	PDP11-780/VAX - SPECIFIC PRINTING ROUTINES
*/

static char textline[50];
int maxregvar = MAXREGVAR;
int regnum[] =  { 11, 10, 9, 8, 7, 6 } ;
static int regmask[] = { 0, 0x800, 0xc00, 0xe00, 0xf00, 0xf80, 0xfc0 };



void
prsave()
{
int proflab;
sprintf(textline, "\t.word\t0x%x", regmask[highregvar]);	/*  register variable mask */
p2pass( textline);
if(profileflag)
	{
	proflab = newlabel();
	fprintf(asmfile, "L%d:\t.space\t4\n", proflab);
	sprintf(textline, "\tmovab\tL%d,r0", proflab);
	p2pass( textline);
	sprintf(textline, "\tjsb\tmcount");
	p2pass( textline);
	}
sprintf(textline, "\tsubl2\t$.F%d,sp", procno);
p2pass( textline);
}


void
goret(type)
int type;
{
sprintf(textline, "\tret");
p2pass( textline);
}




/*
 * move argument slot arg1 (relative to ap)
 * to slot arg2 (relative to ARGREG)
 */
void
mvarg(type, arg1, arg2)
int type, arg1, arg2;
{
sprintf(textline, "\tmovl\t%d(ap),%d(fp)", arg1+ARGOFFSET, arg2+argloc) ;
p2pass( textline);
}



void
prlabel(fp, k)
FILEP fp;
int k;
{
fprintf(fp, "L%d:\n", k);
}


void
prconi(fp, type, n)
FILEP fp;
int type;
ftnint n;
{
fprintf(fp, "\t%s\t%ld\n", (type==TYSHORT ? ".word" : ".long"), n);
}


void
prcona(fp, a)
FILEP fp;
ftnint a;
{
fprintf(fp, "\t.long\tL%ld\n", a);
}



#ifndef vax
void
prconr(fp, type, x)
FILEP fp;
int type;
float x;
{
fprintf(fp, "\t%s\t0f%e\n", (type==TYREAL ? ".float" : ".double"), x);
}
#endif

#ifdef vax
prconr(fp, type, x)
FILEP fp;
int type;
double x;
{
long int *n;
n = &x;	/* nonportable cheat */
if(type == TYREAL)
	fprintf(fp, "\t.long\t0x%X\n", n[0]);
else
	fprintf(fp, "\t.long\t0x%X,0x%X\n", n[0], n[1]);
}
#endif






void
preven(k)
int k;
{
register int lg;

if(k > 4)
	lg = 3;
else if(k > 2)
	lg = 2;
else if(k > 1)
	lg = 1;
else
	return;
fprintf(asmfile, "\t.align\t%d\n", lg);
}


#if 0
vaxgoto(index, nlab, labs)
expptr index;
register int nlab;
struct labelblock *labs[];
{
register int i;
register int arrlab;

putforce(TYINT, index);
sprintf(textline, "\tcasel\tr0,$1,$%d", nlab-1) ;
p2pass( textline);
sprintf(textline, "L%d:", arrlab = newlabel() ) ;
p2pass( textline);
for(i = 0; i< nlab ; ++i) {
	sprintf(textline, "\t.word\tL%d-L%d", labs[i]->labelno, arrlab) ;
	p2pass( textline);
}
}
#endif

void
prarif(p, neg, zer, pos)
bigptr p;
int neg, zer, pos;
{
putforce(p->vtype, p);
if( ISINT(p->vtype) ) {
	sprintf(textline, "\ttstl\tr0") ;p2pass( textline);
} else {
	sprintf(textline, "\ttstd\tr0") ;p2pass( textline);
}
sprintf(textline, "\tjlss\tL%d", neg) ;p2pass( textline);
sprintf(textline, "\tjeql\tL%d", zer) ;p2pass( textline);
sprintf(textline, "\tjbr\tL%d", pos) ;p2pass( textline);
}




char *memname(stg, mem)
int stg, mem;
{
static char s[20];

switch(stg)
	{
	case STGCOMMON:
	case STGEXT:
		sprintf(s, "_%s", varstr(XL, extsymtab[mem].extname) );
		break;

	case STGBSS:
	case STGINIT:
		sprintf(s, "v.%d", mem);
		break;

	case STGCONST:
		sprintf(s, "L%d", mem);
		break;

	case STGEQUIV:
		sprintf(s, "q.%d", mem);
		break;

	default:
		fatal1("memname: invalid vstg %d", stg);
	}
return(s);
}



void
prlocvar(s, len)
char *s;
ftnint len;
{
fprintf(asmfile, "\t.lcomm\t%s,%ld\n", s, len);
}


void
prext(name, leng, init)
char *name;
ftnint leng;
int init;
{
if(leng == 0)
	fprintf(asmfile, "\t.globl\t_%s\n", name);
else
	fprintf(asmfile, "\t.comm\t_%s,%ld\n", name, leng);
}




void
prendproc()
{
}



void
prtail()
{
}




void
prolog(ep, argvec)
struct entrypoint *ep;
struct bigblock *argvec;
{
int i, argslot;
int size;
register chainp p;
register struct bigblock *q;
register struct dimblock *dp;

if(procclass == CLMAIN)
	p2pass( "_MAIN__:" );
if(ep->entryname) {
	sprintf(textline, "_%s:",  varstr(XL, ep->entryname->extname)) ;
	p2pass( textline);
}
if(procclass == CLBLOCK)
	return;
prsave();
if(argvec)
	{
	argloc = argvec->b_addr.memoffset->b_const.fconst.ci;
	if(proctype == TYCHAR)
		{
		mvarg(TYADDR, 0, chslot);
		mvarg(TYLENG, FSZADDR, chlgslot);
		argslot = FSZADDR + FSZLENG;
		}
	else if( ISCOMPLEX(proctype) )
		{
		mvarg(TYADDR, 0, cxslot);
		argslot = FSZADDR;
		}
	else
		argslot = 0;

	for(p = ep->arglist ; p ; p =p->chain.nextp)
		{
		q = p->chain.datap;
		mvarg(TYADDR, argslot, q->b_name.vardesc.varno);
		argslot += FSZADDR;
		}
	for(p = ep->arglist ; p ; p = p->chain.nextp)
		{
		q = p->chain.datap;
		if(q->vtype==TYCHAR || q->vclass==CLPROC)
			{
			if(q->vleng && q->vleng->tag!=TCONST)
				mvarg(TYLENG, argslot, q->vleng->b_name.vardesc.varno);
			argslot += FSZLENG;
			}
		}
	sprintf(textline, "\taddl3\t$%d,fp,ap", argloc-ARGOFFSET);
	p2pass( textline);
	}

for(p = ep->arglist ; p ; p = p->chain.nextp)
	{
	q = p->chain.datap;
	if((dp = q->b_name.vdim))
		{
		for(i = 0 ; i < dp->ndim ; ++i)
			if(dp->dims[i].dimexpr)
				puteq( fixtype(cpexpr(dp->dims[i].dimsize)),
					fixtype(cpexpr(dp->dims[i].dimexpr)));
		size = typesize[ q->vtype ];
		/* on VAX, get more efficient subscripting if subscripts
		   have zero-base, so fudge the argument pointers for arrays.
		   Not done if array bounds are being checked.
		*/
		if(dp->basexpr)
			{
			puteq( 	cpexpr(fixtype(dp->baseoffset)),
				cpexpr(fixtype(dp->basexpr)));
			if(! checksubs)
				{
				putforce(TYINT,
					fixtype( mkexpr(OPSTAR, ICON(size),
						cpexpr(dp->baseoffset)) ));
				sprintf(textline, "\tsubl2\tr0,%d(ap)",
					p->chain.datap->b_name.vardesc.varno + ARGOFFSET);
				p2pass(textline);
				}
			}
		else if(!checksubs && dp->baseoffset->b_const.fconst.ci!=0) {
			sprintf(textline, "\tsubl2\t$%ld,%d(ap)",
				dp->baseoffset->b_const.fconst.ci * size,
				p->chain.datap->b_name.vardesc.varno + ARGOFFSET) ;
			p2pass(textline);
			}
		}
	}

if(typeaddr)
	puteq( cpexpr(typeaddr), mkaddcon(ep->typelabel) );
putgoto(ep->entrylabel);
}



void
prhead(fp)
FILEP fp;
{
#if FAMILY==SCJ
#	if OUTPUT == BINARY
		p2triple(P2LBRACKET, ARGREG-highregvar, procno);
		p2word( (long) (SZCHAR*autoleng) );
		p2flush();
#	else
		fprintf(fp, "[%02d\t%06ld\t%02d\t\n", procno,
			SZCHAR*autoleng, ARGREG-highregvar);
#	endif
#endif
}


void
prdbginfo()
{
}

void
prcmgoto(bigptr a, int b, int c, int d)
{
}
#endif /* FCOM */
