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
 * notice, this list of conditions and the following disclaimer in the
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
#include "defs.h"
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




prsave()
{
int proflab;
p2pass( sprintf(textline, "\t.word\t0x%x", regmask[highregvar]) );	/*  register variable mask */
if(profileflag)
	{
	proflab = newlabel();
	fprintf(asmfile, "L%d:\t.space\t4\n", proflab);
	p2pass( sprintf(textline, "\tmovab\tL%d,r0", proflab) );
	p2pass( sprintf(textline, "\tjsb\tmcount") );
	}
p2pass( sprintf(textline, "\tsubl2\t$.F%d,sp", procno) );
}



goret(type)
int type;
{
p2pass( sprintf(textline, "\tret") );
}




/*
 * move argument slot arg1 (relative to ap)
 * to slot arg2 (relative to ARGREG)
 */

mvarg(type, arg1, arg2)
int type, arg1, arg2;
{
p2pass( sprintf(textline, "\tmovl\t%d(ap),%d(fp)", arg1+ARGOFFSET, arg2+argloc) );
}




prlabel(fp, k)
FILEP fp;
int k;
{
fprintf(fp, "L%d:\n", k);
}



prconi(fp, type, n)
FILEP fp;
int type;
ftnint n;
{
fprintf(fp, "\t%s\t%ld\n", (type==TYSHORT ? ".word" : ".long"), n);
}



prcona(fp, a)
FILEP fp;
ftnint a;
{
fprintf(fp, "\t.long\tL%ld\n", a);
}



#ifndef vax
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



vaxgoto(index, nlab, labs)
expptr index;
register int nlab;
struct labelblock *labs[];
{
register int i;
register int arrlab;

putforce(TYINT, index);
p2pass( sprintf(textline, "\tcasel\tr0,$1,$%d", nlab-1) );
p2pass( sprintf(textline, "L%d:", arrlab = newlabel() ) );
for(i = 0; i< nlab ; ++i)
	p2pass( sprintf(textline, "\t.word\tL%d-L%d", labs[i]->labelno, arrlab) );
}


prarif(p, neg, zer, pos)
ptr p;
int neg, zer, pos;
{
putforce(((struct exprblock *)p)->vtype, p);
if( ISINT(((struct exprblock *)p)->vtype) )
	p2pass( sprintf(textline, "\ttstl\tr0") );
else
	p2pass( sprintf(textline, "\ttstd\tr0") );
p2pass( sprintf(textline, "\tjlss\tL%d", neg) );
p2pass( sprintf(textline, "\tjeql\tL%d", zer) );
p2pass( sprintf(textline, "\tjbr\tL%d", pos) );
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




prlocvar(s, len)
char *s;
ftnint len;
{
fprintf(asmfile, "\t.lcomm\t%s,%ld\n", s, len);
}



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





prendproc()
{
}




prtail()
{
}





prolog(ep, argvec)
struct entrypoint *ep;
struct addrblock *argvec;
{
int i, argslot, proflab;
int size;
register chainp p;
register struct nameblock *q;
register struct dimblock *dp;
expptr tp;

if(procclass == CLMAIN)
	p2pass( "_MAIN__:" );
if(ep->entryname)
	p2pass( sprintf(textline, "_%s:",  varstr(XL, ep->entryname->extname)) );
if(procclass == CLBLOCK)
	return;
prsave();
if(argvec)
	{
	argloc = argvec->memoffset->constblock.fconst.ci;
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
		mvarg(TYADDR, argslot, q->vardesc.varno);
		argslot += FSZADDR;
		}
	for(p = ep->arglist ; p ; p = p->chain.nextp)
		{
		q = p->chain.datap;
		if(q->vtype==TYCHAR || q->vclass==CLPROC)
			{
			if(q->vleng && q->vleng->exprblock.tag!=TCONST)
				mvarg(TYLENG, argslot, ((union taggedblock *)q->vleng)->nameblock.vardesc.varno);
			argslot += FSZLENG;
			}
		}
	p2pass( sprintf(textline, "\taddl3\t$%d,fp,ap", argloc-ARGOFFSET) );
	}

for(p = ep->arglist ; p ; p = p->chain.nextp)
	{
	q = p->chain.datap;
	if(dp = q->vdim)
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
				p2pass( sprintf(textline, "\tsubl2\tr0,%d(ap)",
					p->chain.datap->nameblock.vardesc.varno + ARGOFFSET) );
				}
			}
		else if(!checksubs && dp->baseoffset->constblock.fconst.ci!=0)
			p2pass( sprintf(textline, "\tsubl2\t$%ld,%d(ap)",
				dp->baseoffset->constblock.fconst.ci * size,
				p->chain.datap->nameblock.vardesc.varno + ARGOFFSET) );
		}
	}

if(typeaddr)
	puteq( cpexpr(typeaddr), mkaddcon(ep->typelabel) );
putgoto(ep->entrylabel);
}




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



prdbginfo()
{
}

prcmgoto()
{
}
