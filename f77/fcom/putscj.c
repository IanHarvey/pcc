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
/* INTERMEDIATE CODE GENERATION FOR S C JOHNSON C COMPILERS */
/* NEW VERSION USING BINARY POLISH POSTFIX INTERMEDIATE */

#include <unistd.h>
#include <string.h>

#include "macdefs.h"

#include "ftypes.h"
#include "defines.h"
#include "defs.h"

#include "scjdefs.h"

LOCAL struct bigblock *putcall(struct bigblock *p);
LOCAL void putmnmx(struct bigblock *p);
LOCAL void putcomma(int, int, int);
LOCAL void putmem(bigptr, int, ftnint);
LOCAL void putaddr(struct bigblock *, int);
LOCAL void putct1(bigptr, struct bigblock *, struct bigblock *, int *, int *);
LOCAL int ncat(bigptr p);
LOCAL void putcat(struct bigblock *, bigptr);
LOCAL void putchcmp(struct bigblock *p);
LOCAL void putcheq(struct bigblock *p);
LOCAL void putcxcmp(struct bigblock *p);
LOCAL struct bigblock *putcx1(bigptr, int *);
LOCAL void putcxop(bigptr p);
LOCAL struct bigblock *putcxeq(struct bigblock *p);
LOCAL void putpower(bigptr p);
LOCAL void putop(bigptr p);
LOCAL void putchop(bigptr p);
LOCAL struct bigblock *putch1(bigptr, int *);
LOCAL struct bigblock *intdouble(struct bigblock *, int *);


struct bigblock *p;
#define FOUR 4
extern int ops2[];
extern int types2[];

void
puthead(s)
char *s;
{
#if TARGET == VAX
	if(s) {
		char buff[100];
		sprintf(buff, "\t.globl\t_%s", s);
		p2pass(buff);
	}
#endif
/* put out fake copy of left bracket line, to be redone later */
if( ! headerdone )
	{
	headoffset = ftell(textfile);
	prhead(textfile);
	headerdone = YES;
	p2triple(P2STMT, (strlen(infname)+FOUR-1)/FOUR, 0);
	p2str(infname);
	}
}





/* It is necessary to precede each procedure with a "left bracket"
 * line that tells pass 2 how many register variables and how
 * much automatic space is required for the function.  This compiler
 * does not know how much automatic space is needed until the
 * entire procedure has been processed.  Therefore, "puthead"
 * is called at the begining to record the current location in textfile,
 * then to put out a placeholder left bracket line.  This procedure
 * repositions the file and rewrites that line, then puts the
 * file pointer back to the end of the file.
 */

void
putbracket()
{
long int hereoffset;

hereoffset = ftell(textfile);
if(fseek(textfile, headoffset, 0))
	fatal("fseek failed");
prhead(textfile);
if(fseek(textfile, hereoffset, 0))
	fatal("fseek failed 2");
}



void
putrbrack(k)
int k;
{
p2op(P2RBRACKET, k);
}


void
putnreg()
{
}





void
puteof()
{
p2op(P2EOF, 0);
}


void
putstmt()
{
p2triple(P2STMT, 0, lineno);
}




/* put out code for if( ! p) goto l  */
void
putif(p,l)
register bigptr p;
int l;
{
register int k;

if( ( k = (p = fixtype(p))->vtype) != TYLOGICAL)
	{
	if(k != TYERROR)
		err("non-logical expression in IF statement");
	frexpr(p);
	}
else
	{
	putex1(p);
	p2icon( (long int) l , P2INT);
	p2op(P2CBRANCH, 0);
	putstmt();
	}
}





/* put out code for  goto l   */
void
putgoto(label)
int label;
{
p2triple(P2GOTO, 1, label);
putstmt();
}


/* branch to address constant or integer variable */
void
putbranch(p)
register struct bigblock *p;
{
putex1(p);
p2op(P2GOTO, P2INT);
putstmt();
}



/* put out label  l:     */
void
putlabel(label)
int label;
{
p2op(P2LABEL, label);
}



void
putexpr(p)
bigptr p;
{
putex1(p);
putstmt();
}



void
putcmgo(index, nlab, labs)
bigptr index;
int nlab;
struct labelblock *labs[];
{
int i, labarray, skiplabel;

if(! ISINT(index->vtype) )
	{
	execerr("computed goto index must be integer", NULL);
	return;
	}

#if TARGET == VAX
	/* use special case instruction */
	vaxgoto(index, nlab, labs);
#else
	labarray = newlabel();
	preven(ALIADDR);
	prlabel(asmfile, labarray);
	prcona(asmfile, (ftnint) (skiplabel = newlabel()) );
	for(i = 0 ; i < nlab ; ++i)
		prcona(asmfile, (ftnint)(labs[i]->labelno) );
	prcmgoto(index, nlab, skiplabel, labarray);
	putlabel(skiplabel);
#endif
}

void
putx(p)
bigptr p;
{
int opc;
int ncomma;
int type, k;

switch(p->tag)
	{
	case TERROR:
		free(p);
		break;

	case TCONST:
		switch(type = p->vtype)
			{
			case TYLOGICAL:
				type = tyint;
			case TYLONG:
			case TYSHORT:
				p2icon(p->b_const.fconst.ci, types2[type]);
				free(p);
				break;

			case TYADDR:
				p2triple(P2ICON, 1, P2INT|P2PTR);
				fprintf(textfile, "    tyaddr=0\n");
				p2name(memname(STGCONST, (int) p->b_const.fconst.ci) );
				free(p);
				break;

			default:
				putx( putconst(p) );
				break;
			}
		break;

	case TEXPR:
		switch(opc = p->b_expr.opcode)
			{
			case OPCALL:
			case OPCCALL:
				if( ISCOMPLEX(p->vtype) )
					putcxop(p);
				else	putcall(p);
				break;

			case OPMIN:
			case OPMAX:
				putmnmx(p);
				break;


			case OPASSIGN:
				if( ISCOMPLEX(p->b_expr.leftp->vtype) || ISCOMPLEX(p->b_expr.rightp->vtype) ) {
					frexpr( putcxeq(p) );
				} else if( ISCHAR(p) )
					putcheq(p);
				else
					goto putopp;
				break;

			case OPEQ:
			case OPNE:
				if( ISCOMPLEX(p->b_expr.leftp->vtype) || ISCOMPLEX(p->b_expr.rightp->vtype) )
					{
					putcxcmp(p);
					break;
					}
			case OPLT:
			case OPLE:
			case OPGT:
			case OPGE:
				if(ISCHAR(p->b_expr.leftp))
					putchcmp(p);
				else
					goto putopp;
				break;

			case OPPOWER:
				putpower(p);
				break;

			case OPSTAR:
				/*   m * (2**k) -> m<<k   */
				if(INT(p->b_expr.leftp->vtype) && ISICON(p->b_expr.rightp) &&
				   ( (k = flog2(p->b_expr.rightp->b_const.fconst.ci))>0) )
					{
					p->b_expr.opcode = OPLSHIFT;
					frexpr(p->b_expr.rightp);
					p->b_expr.rightp = MKICON(k);
					goto putopp;
					}

			case OPMOD:
				goto putopp;
			case OPPLUS:
			case OPMINUS:
			case OPSLASH:
			case OPNEG:
				if( ISCOMPLEX(p->vtype) )
					putcxop(p);
				else	goto putopp;
				break;

			case OPCONV:
				if( ISCOMPLEX(p->vtype) )
					putcxop(p);
				else if( ISCOMPLEX(p->b_expr.leftp->vtype) )
					{
					ncomma = 0;
					putx( mkconv(p->vtype,
						realpart(putcx1(p->b_expr.leftp, &ncomma))));
					putcomma(ncomma, p->vtype, NO);
					free(p);
					}
				else	goto putopp;
				break;

			case OPNOT:
			case OPOR:
			case OPAND:
			case OPEQV:
			case OPNEQV:
			case OPADDR:
			case OPPLUSEQ:
			case OPSTAREQ:
			case OPCOMMA:
			case OPQUEST:
			case OPCOLON:
			case OPBITOR:
			case OPBITAND:
			case OPBITXOR:
			case OPBITNOT:
			case OPLSHIFT:
			case OPRSHIFT:
		putopp:
				putop(p);
				break;

			default:
				fatal1("putx: invalid opcode %d", opc);
			}
		break;

	case TADDR:
		putaddr(p, YES);
		break;

	default:
		fatal1("putx: impossible tag %d", p->tag);
	}
}



LOCAL void
putop(p)
bigptr p;
{
int k;
bigptr lp, tp;
int pt, lt;
int comma;

switch(p->b_expr.opcode)	/* check for special cases and rewrite */
	{
	case OPCONV:
		pt = p->vtype;
		lp = p->b_expr.leftp;
		lt = lp->vtype;
		while(p->tag==TEXPR && p->b_expr.opcode==OPCONV &&
		     ( (ISREAL(pt)&&ISREAL(lt)) ||
			(INT(pt)&&(ONEOF(lt,MSKINT|MSKADDR|MSKCHAR))) ))
			{
#if FSZINT < FSZLONG
			if(lp->tag != TEXPR)
				{
				if(pt==TYINT && lt==TYLONG)
					break;
				if(lt==TYINT && pt==TYLONG)
					break;
				}
#endif
			free(p);
			p = lp;
			pt = lt;
			lp = p->b_expr.leftp;
			lt = lp->vtype;
			}
		if(p->tag==TEXPR && p->b_expr.opcode==OPCONV)
			break;
		putx(p);
		return;

	case OPADDR:
		comma = NO;
		lp = p->b_expr.leftp;
		if(lp->tag != TADDR)
			{
			tp = fmktemp(lp->vtype, lp->vleng);
			putx( mkexpr(OPASSIGN, cpexpr(tp), lp) );
			lp = tp;
			comma = YES;
			}
		putaddr(lp, NO);
		if(comma)
			putcomma(1, TYINT, NO);
		free(p);
		return;
	}

if( (k = ops2[p->b_expr.opcode]) <= 0)
	fatal1("putop: invalid opcode %d", p->b_expr.opcode);
putx(p->b_expr.leftp);
if(p->b_expr.rightp)
	putx(p->b_expr.rightp);
p2op(k, types2[p->vtype]);

if(p->vleng)
	frexpr(p->vleng);
free(p);
}

void
putforce(t, p)
int t;
bigptr p;
{
p = mkconv(t, fixtype(p));
putx(p);
p2op(P2FORCE,
	(t==TYSHORT ? P2SHORT : (t==TYLONG ? P2LONG : P2DREAL)) );
putstmt();
}



LOCAL void
putpower(p)
bigptr p;
{
bigptr base;
struct bigblock *t1, *t2;
ftnint k = 0; /* XXX gcc */
int type;
int ncomma;

if(!ISICON(p->b_expr.rightp) || (k = p->b_expr.rightp->b_const.fconst.ci)<2)
	fatal("putpower: bad call");
base = p->b_expr.leftp;
type = base->vtype;
t1 = fmktemp(type, NULL);
t2 = NULL;
ncomma = 1;
putassign(cpexpr(t1), cpexpr(base) );

for( ; (k&1)==0 && k>2 ; k>>=1 )
	{
	++ncomma;
	putsteq(t1, t1);
	}

if(k == 2)
	putx( mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)) );
else
	{
	t2 = fmktemp(type, NULL);
	++ncomma;
	putassign(cpexpr(t2), cpexpr(t1));
	
	for(k>>=1 ; k>1 ; k>>=1)
		{
		++ncomma;
		putsteq(t1, t1);
		if(k & 1)
			{
			++ncomma;
			putsteq(t2, t1);
			}
		}
	putx( mkexpr(OPSTAR, cpexpr(t2),
		mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)) ));
	}
putcomma(ncomma, type, NO);
frexpr(t1);
if(t2)
	frexpr(t2);
frexpr(p);
}




LOCAL struct bigblock *intdouble(p, ncommap)
struct bigblock *p;
int *ncommap;
{
register struct bigblock *t;

t = fmktemp(TYDREAL, NULL);
++*ncommap;
putassign(cpexpr(t), p);
return(t);
}





LOCAL struct bigblock *
putcxeq(p)
register struct bigblock *p;
{
register struct bigblock *lp, *rp;
int ncomma;

ncomma = 0;
lp = putcx1(p->b_expr.leftp, &ncomma);
rp = putcx1(p->b_expr.rightp, &ncomma);
putassign(realpart(lp), realpart(rp));
if( ISCOMPLEX(p->vtype) )
	{
	++ncomma;
	putassign(imagpart(lp), imagpart(rp));
	}
putcomma(ncomma, TYREAL, NO);
frexpr(rp);
free(p);
return(lp);
}



LOCAL void
putcxop(p)
bigptr p;
{
int ncomma;

ncomma = 0;
putaddr( putcx1(p, &ncomma), NO);
putcomma(ncomma, TYINT, NO);
}



LOCAL struct bigblock *putcx1(p, ncommap)
register bigptr p;
int *ncommap;
{
struct bigblock *q, *lp, *rp;
register struct bigblock *resp;
int opcode;
int ltype, rtype;

ltype = rtype = 0; /* XXX gcc */
if(p == NULL)
	return(NULL);

switch(p->tag)
	{
	case TCONST:
		if( ISCOMPLEX(p->vtype) )
			p = putconst(p);
		return( p );

	case TADDR:
		if( ! addressable(p) )
			{
			++*ncommap;
			resp = fmktemp(tyint, NULL);
			putassign( cpexpr(resp), p->b_addr.memoffset );
			p->b_addr.memoffset = resp;
			}
		return( p );

	case TEXPR:
		if( ISCOMPLEX(p->vtype) )
			break;
		++*ncommap;
		resp = fmktemp(TYDREAL, NO);
		putassign( cpexpr(resp), p);
		return(resp);

	default:
		fatal1("putcx1: bad tag %d", p->tag);
	}

opcode = p->b_expr.opcode;
if(opcode==OPCALL || opcode==OPCCALL)
	{
	++*ncommap;
	return( putcall(p) );
	}
else if(opcode == OPASSIGN)
	{
	++*ncommap;
	return( putcxeq(p) );
	}
resp = fmktemp(p->vtype, NULL);
if((lp = putcx1(p->b_expr.leftp, ncommap) ))
	ltype = lp->vtype;
if((rp = putcx1(p->b_expr.rightp, ncommap) ))
	rtype = rp->vtype;

switch(opcode)
	{
	case OPCOMMA:
		frexpr(resp);
		resp = rp;
		rp = NULL;
		break;

	case OPNEG:
		putassign( realpart(resp), mkexpr(OPNEG, realpart(lp), NULL) );
		putassign( imagpart(resp), mkexpr(OPNEG, imagpart(lp), NULL) );
		*ncommap += 2;
		break;

	case OPPLUS:
	case OPMINUS:
		putassign( realpart(resp), mkexpr(opcode, realpart(lp), realpart(rp) ));
		if(rtype < TYCOMPLEX)
			putassign( imagpart(resp), imagpart(lp) );
		else if(ltype < TYCOMPLEX)
			{
			if(opcode == OPPLUS)
				putassign( imagpart(resp), imagpart(rp) );
			else	putassign( imagpart(resp), mkexpr(OPNEG, imagpart(rp), NULL) );
			}
		else
			putassign( imagpart(resp), mkexpr(opcode, imagpart(lp), imagpart(rp) ));

		*ncommap += 2;
		break;

	case OPSTAR:
		if(ltype < TYCOMPLEX)
			{
			if( ISINT(ltype) )
				lp = intdouble(lp, ncommap);
			putassign( realpart(resp), mkexpr(OPSTAR, cpexpr(lp), realpart(rp) ));
			putassign( imagpart(resp), mkexpr(OPSTAR, cpexpr(lp), imagpart(rp) ));
			}
		else if(rtype < TYCOMPLEX)
			{
			if( ISINT(rtype) )
				rp = intdouble(rp, ncommap);
			putassign( realpart(resp), mkexpr(OPSTAR, cpexpr(rp), realpart(lp) ));
			putassign( imagpart(resp), mkexpr(OPSTAR, cpexpr(rp), imagpart(lp) ));
			}
		else	{
			putassign( realpart(resp), mkexpr(OPMINUS,
				mkexpr(OPSTAR, realpart(lp), realpart(rp)),
				mkexpr(OPSTAR, imagpart(lp), imagpart(rp)) ));
			putassign( imagpart(resp), mkexpr(OPPLUS,
				mkexpr(OPSTAR, realpart(lp), imagpart(rp)),
				mkexpr(OPSTAR, imagpart(lp), realpart(rp)) ));
			}
		*ncommap += 2;
		break;

	case OPSLASH:
		/* fixexpr has already replaced all divisions
		 * by a complex by a function call
		 */
		if( ISINT(rtype) )
			rp = intdouble(rp, ncommap);
		putassign( realpart(resp), mkexpr(OPSLASH, realpart(lp), cpexpr(rp)) );
		putassign( imagpart(resp), mkexpr(OPSLASH, imagpart(lp), cpexpr(rp)) );
		*ncommap += 2;
		break;

	case OPCONV:
		putassign( realpart(resp), realpart(lp) );
		if( ISCOMPLEX(lp->vtype) )
			q = imagpart(lp);
		else if(rp != NULL)
			q = realpart(rp);
		else
			q = mkrealcon(TYDREAL, 0.0);
		putassign( imagpart(resp), q);
		*ncommap += 2;
		break;

	default:
		fatal1("putcx1 of invalid opcode %d", opcode);
	}

frexpr(lp);
frexpr(rp);
free(p);
return(resp);
}




LOCAL void
putcxcmp(p)
register struct bigblock *p;
{
int opcode;
int ncomma;
register struct bigblock *lp, *rp;
struct bigblock *q;

ncomma = 0;
opcode = p->b_expr.opcode;
lp = putcx1(p->b_expr.leftp, &ncomma);
rp = putcx1(p->b_expr.rightp, &ncomma);

q = mkexpr( opcode==OPEQ ? OPAND : OPOR ,
	mkexpr(opcode, realpart(lp), realpart(rp)),
	mkexpr(opcode, imagpart(lp), imagpart(rp)) );
putx( fixexpr(q) );
putcomma(ncomma, TYINT, NO);

free(lp);
free(rp);
free(p);
}

LOCAL struct bigblock *putch1(p, ncommap)
register bigptr p;
int * ncommap;
{
register struct bigblock *t;

switch(p->tag)
	{
	case TCONST:
		return( putconst(p) );

	case TADDR:
		return(p);

	case TEXPR:
		++*ncommap;

		switch(p->b_expr.opcode)
			{
			case OPCALL:
			case OPCCALL:
				t = putcall(p);
				break;

			case OPCONCAT:
				t = fmktemp(TYCHAR, cpexpr(p->vleng) );
				putcat( cpexpr(t), p );
				break;

			case OPCONV:
				if(!ISICON(p->vleng) || p->vleng->b_const.fconst.ci!=1
				   || ! INT(p->b_expr.leftp->vtype) )
					fatal("putch1: bad character conversion");
				t = fmktemp(TYCHAR, MKICON(1) );
				putassign( cpexpr(t), p);
				break;
			default:
				fatal1("putch1: invalid opcode %d", p->b_expr.opcode);
				t = NULL; /* XXX gcc */
			}
		return(t);

	default:
		fatal1("putch1: bad tag %d", p->tag);
	}
/* NOTREACHED */
return NULL; /* XXX gcc */
}




LOCAL void
putchop(p)
bigptr p;
{
int ncomma;

ncomma = 0;
putaddr( putch1(p, &ncomma) , NO );
putcomma(ncomma, TYCHAR, YES);
}




LOCAL void
putcheq(p)
register struct bigblock *p;
{
int ncomma;

ncomma = 0;
if( p->b_expr.rightp->tag==TEXPR && p->b_expr.rightp->b_expr.opcode==OPCONCAT )
	putcat(p->b_expr.leftp, p->b_expr.rightp);
else if( ISONE(p->b_expr.leftp->vleng) && ISONE(p->b_expr.rightp->vleng) )
	{
	putaddr( putch1(p->b_expr.leftp, &ncomma) , YES );
	putaddr( putch1(p->b_expr.rightp, &ncomma) , YES );
	p2op(P2ASSIGN, P2CHAR);
	}
else	putx( call2(TYINT, "s_copy", p->b_expr.leftp, p->b_expr.rightp) );

putcomma(ncomma, TYINT, NO);
frexpr(p->vleng);
free(p);
}




LOCAL void
putchcmp(p)
register struct bigblock *p;
{
int ncomma;

ncomma = 0;
if(ISONE(p->b_expr.leftp->vleng) && ISONE(p->b_expr.rightp->vleng) )
	{
	putaddr( putch1(p->b_expr.leftp, &ncomma) , YES );
	putaddr( putch1(p->b_expr.rightp, &ncomma) , YES );
	p2op(ops2[p->b_expr.opcode], P2CHAR);
	free(p);
	putcomma(ncomma, TYINT, NO);
	}
else
	{
	p->b_expr.leftp = call2(TYINT,"s_cmp", p->b_expr.leftp, p->b_expr.rightp);
	p->b_expr.rightp = MKICON(0);
	putop(p);
	}
}





LOCAL void
putcat(lhs, rhs)
register struct bigblock *lhs;
register bigptr rhs;
{
int n, ncomma;
struct bigblock *lp, *cp;

ncomma = 0;
n = ncat(rhs);
lp = mktmpn(n, TYLENG, NULL);
cp = mktmpn(n, TYADDR, NULL);

n = 0;
putct1(rhs, lp, cp, &n, &ncomma);

putx( call4(TYSUBR, "s_cat", lhs, cp, lp, MKICON(n) ) );
putcomma(ncomma, TYINT, NO);
}





LOCAL int
ncat(p)
register bigptr p;
{
if(p->tag==TEXPR && p->b_expr.opcode==OPCONCAT)
	return( ncat(p->b_expr.leftp) + ncat(p->b_expr.rightp) );
else	return(1);
}




LOCAL void
putct1(q, lp, cp, ip, ncommap)
register bigptr q;
register struct bigblock *lp, *cp;
int *ip, *ncommap;
{
int i;
struct bigblock *lp1, *cp1;

if(q->tag==TEXPR && q->b_expr.opcode==OPCONCAT)
	{
	putct1(q->b_expr.leftp, lp, cp, ip, ncommap);
	putct1(q->b_expr.rightp, lp, cp , ip, ncommap);
	frexpr(q->vleng);
	free(q);
	}
else
	{
	i = (*ip)++;
	lp1 = cpexpr(lp);
	lp1->b_addr.memoffset = mkexpr(OPPLUS, lp1->b_addr.memoffset, MKICON(i*FSZLENG));
	cp1 = cpexpr(cp);
	cp1->b_addr.memoffset = mkexpr(OPPLUS, cp1->b_addr.memoffset, MKICON(i*FSZADDR));
	putassign( lp1, cpexpr(q->vleng) );
	putassign( cp1, addrof(putch1(q,ncommap)) );
	*ncommap += 2;
	}
}

LOCAL void
putaddr(p, indir)
register struct bigblock *p;
int indir;
{
int type, type2, funct;
ftnint offset;
bigptr offp;

type = p->vtype;
type2 = types2[type];
funct = (p->vclass==CLPROC ? P2FUNCT<<2 : 0);

offp = (p->b_addr.memoffset ? cpexpr(p->b_addr.memoffset) : NULL);


#if (FUDGEOFFSET != 1)
if(offp)
	offp = mkexpr(OPSTAR, MKICON(FUDGEOFFSET), offp);
#endif

offset = simoffset( &offp );
#if FSZINT < FSZLONG
	if(offp)
		if(shortsubs)
			offp = shorten(offp);
		else
			offp = mkconv(TYINT, offp);
#else
	if(offp)
		offp = mkconv(TYINT, offp);
#endif

switch(p->vstg)
	{
	case STGAUTO:
		if(indir && !offp)
			{
			p2oreg(offset, AUTOREG, type2);
			break;
			}

		if(!indir && !offp && !offset)
			{
			p2reg(AUTOREG, type2 | P2PTR);
			break;
			}

		p2reg(AUTOREG, type2 | P2PTR);
		if(offp)
			{
			putx(offp);
			if(offset)
				p2icon(offset, P2INT);
			}
		else
			p2icon(offset, P2INT);
		if(offp && offset)
			p2op(P2PLUS, type2 | P2PTR);
		p2op(P2PLUS, type2 | P2PTR);
		if(offp && offset)
		if(indir)
			p2op(P2INDIRECT, type2);
		break;

	case STGARG:
		p2oreg(
#ifdef ARGOFFSET
			ARGOFFSET +
#endif
			(ftnint) (FUDGEOFFSET*p->b_addr.memno),
			ARGREG,   type2 | P2PTR | funct );

		if(offp)
			putx(offp);
		if(offset)
			p2icon(offset, P2INT);
		if(offp && offset)
			p2op(P2PLUS, type2 | P2PTR);
		if(offp || offset)
			p2op(P2PLUS, type2 | P2PTR);
		if(indir)
			p2op(P2INDIRECT, type2);
		break;

	case STGLENG:
		if(indir)
			{
			p2oreg(
#ifdef ARGOFFSET
				ARGOFFSET +
#endif
				(ftnint) (FUDGEOFFSET*p->b_addr.memno),
				ARGREG,   type2 | P2PTR | funct);
			}
		else	{
			p2op(P2PLUS, types2[TYLENG] | P2PTR );
			p2reg(ARGREG, types2[TYLENG] | P2PTR );
			p2icon(
#ifdef ARGOFFSET
				ARGOFFSET +
#endif
				(ftnint) (FUDGEOFFSET*p->b_addr.memno), P2INT);
			}
		break;


	case STGBSS:
	case STGINIT:
	case STGEXT:
	case STGCOMMON:
	case STGEQUIV:
	case STGCONST:
		if(offp)
			{
			putx(offp);
			putmem(p, P2ICON, offset);
			p2op(P2PLUS, type2 | P2PTR);
			if(indir)
				p2op(P2INDIRECT, type2);
			}
		else
			putmem(p, (indir ? P2NAME : P2ICON), offset);

		break;

	case STGREG:
		if(indir)
			p2reg(p->b_addr.memno, type2);
		else
			fatal("attempt to take address of a register");
		break;

	default:
		fatal1("putaddr: invalid vstg %d", p->vstg);
	}
frexpr(p);
}




LOCAL void
putmem(p, class, offset)
bigptr p;
int class;
ftnint offset;
{
int type2;
int funct;
char *name;

funct = (p->vclass==CLPROC ? P2FUNCT<<2 : 0);
type2 = types2[p->vtype];
if(p->vclass == CLPROC)
	type2 |= (P2FUNCT<<2);
name = memname(p->vstg, p->b_addr.memno);
if(class == P2ICON)
	{
	p2triple(P2ICON, name[0]!='\0', type2|P2PTR);
	fprintf(textfile, "    icon=%ld\n", offset);
	if(name[0])
		p2name(name);
	}
else
	{
	p2triple(P2NAME, offset!=0, type2);
	if(offset != 0)
		fprintf(textfile, "    icon=%ld\n", offset);
	p2name(name);
	}
}



LOCAL struct bigblock *putcall(p)
struct bigblock *p;
{
chainp arglist, charsp, cp;
int n, first;
struct bigblock *t;
struct bigblock *q;
struct bigblock *fval;
int type, type2, ctype, indir;

type2 = types2[type = p->vtype];
charsp = NULL;
indir =  (p->b_expr.opcode == OPCCALL);
n = 0;
first = YES;

if(p->b_expr.rightp)
	{
	arglist = p->b_expr.rightp->b_list.listp;
	free(p->b_expr.rightp);
	}
else
	arglist = NULL;

for(cp = arglist ; cp ; cp = cp->chain.nextp)
	if(indir)
		++n;
	else	{
		q = cp->chain.datap;
		if(q->tag == TCONST)
			cp->chain.datap = q = putconst(q);
		if( ISCHAR(q) )
			{
			charsp = hookup(charsp, mkchain(cpexpr(q->vleng), 0) );
			n += 2;
			}
		else if(q->vclass == CLPROC)
			{
			charsp = hookup(charsp, mkchain( MKICON(0) , 0));
			n += 2;
			}
		else
			n += 1;
		}

if(type == TYCHAR)
	{
	if( ISICON(p->vleng) )
		{
		fval = fmktemp(TYCHAR, p->vleng);
		n += 2;
		}
	else	{
		err("adjustable character function");
		return NULL;
		}
	}
else if( ISCOMPLEX(type) )
	{
	fval = fmktemp(type, NULL);
	n += 1;
	}
else
	fval = NULL;

ctype = (fval ? P2INT : type2);
putaddr(p->b_expr.leftp, NO);

if(fval)
	{
	first = NO;
	putaddr( cpexpr(fval), NO);
	if(type==TYCHAR)
		{
		putx( cpexpr(p->vleng) );
		p2op(P2LISTOP, type2);
		}
	}

for(cp = arglist ; cp ; cp = cp->chain.nextp)
	{
	q = cp->chain.datap;
	if(q->tag==TADDR && (indir || q->vstg!=STGREG) )
		putaddr(q, indir && q->vtype!=TYCHAR);
	else if( ISCOMPLEX(q->vtype) )
		putcxop(q);
	else if (ISCHAR(q) )
		putchop(q);
	else if( ! ISERROR(q) )
		{
		if(indir)
			putx(q);
		else	{
			t = fmktemp(q->vtype, q->vleng);
			putassign( cpexpr(t), q );
			putaddr(t, NO);
			putcomma(1, q->vtype, YES);
			}
		}
	if(first)
		first = NO;
	else
		p2op(P2LISTOP, type2);
	}

if(arglist)
	frchain(&arglist);
for(cp = charsp ; cp ; cp = cp->chain.nextp)
	{
	putx( mkconv(TYLENG,cp->chain.datap) );
	p2op(P2LISTOP, type2);
	}
frchain(&charsp);
p2op(n>0 ? P2CALL : P2CALL0 , ctype);
free(p);
return(fval);
}



LOCAL void
putmnmx(p)
register struct bigblock *p;
{
int op, type;
int ncomma;
struct bigblock *qp;
chainp p0, p1;
struct bigblock *sp, *tp;

type = p->vtype;
op = (p->b_expr.opcode==OPMIN ? OPLT : OPGT );
p0 = p->b_expr.leftp->b_list.listp;
free(p->b_expr.leftp);
free(p);

sp = fmktemp(type, NULL);
tp = fmktemp(type, NULL);
qp = mkexpr(OPCOLON, cpexpr(tp), cpexpr(sp));
qp = mkexpr(OPQUEST, mkexpr(op, cpexpr(tp),cpexpr(sp)), qp);
qp = fixexpr(qp);

ncomma = 1;
putassign( cpexpr(sp), p0->chain.datap );

for(p1 = p0->chain.nextp ; p1 ; p1 = p1->chain.nextp)
	{
	++ncomma;
	putassign( cpexpr(tp), p1->chain.datap );
	if(p1->chain.nextp)
		{
		++ncomma;
		putassign( cpexpr(sp), cpexpr(qp) );
		}
	else
		putx(qp);
	}

putcomma(ncomma, type, NO);
frtemp(sp);
frtemp(tp);
frchain( &p0 );
}




LOCAL void
putcomma(n, type, indir)
int n, type, indir;
{
type = types2[type];
if(indir)
	type |= P2PTR;
while(--n >= 0)
	p2op(P2COMOP, type);
}




ftnint simoffset(p0)
bigptr *p0;
{
ftnint offset, prod;
register bigptr p, lp, rp;

offset = 0;
p = *p0;
if(p == NULL)
	return(0);

if( ! ISINT(p->vtype) )
	return(0);

if(p->tag==TEXPR && p->b_expr.opcode==OPSTAR)
	{
	lp = p->b_expr.leftp;
	rp = p->b_expr.rightp;
	if(ISICON(rp) && lp->tag==TEXPR && lp->b_expr.opcode==OPPLUS && ISICON(lp->b_expr.rightp))
		{
		p->b_expr.opcode = OPPLUS;
		lp->b_expr.opcode = OPSTAR;
		prod = rp->b_const.fconst.ci * lp->b_expr.rightp->b_const.fconst.ci;
		lp->b_expr.rightp->b_const.fconst.ci = rp->b_const.fconst.ci;
		rp->b_const.fconst.ci = prod;
		}
	}

if(p->tag==TEXPR && p->b_expr.opcode==OPPLUS && ISICON(p->b_expr.rightp))
	{
	rp = p->b_expr.rightp;
	lp = p->b_expr.leftp;
	offset += rp->b_const.fconst.ci;
	frexpr(rp);
	free(p);
	*p0 = lp;
	}

if(p->tag == TCONST)
	{
	offset += p->b_const.fconst.ci;
	frexpr(p);
	*p0 = NULL;
	}

return(offset);
}




void
p2op(op, type)
int op, type;
{
p2triple(op, 0, type);
}

void
p2icon(offset, type)
ftnint offset;
int type;
{
p2triple(P2ICON, 0, type);
fprintf(textfile, "    num=%ld\n", offset);
}



void
p2oreg(offset, reg, type)
ftnint offset;
int reg, type;
{
p2triple(P2OREG, reg, type);
fprintf(textfile, "    num=%ld\n", offset);
p2name("");
}



void
p2reg(reg, type)
int reg, type;
{
p2triple(P2REG, reg, type);
}


void
p2pass(s)
char *s;
{
p2triple(P2PASS, (strlen(s) + FOUR-1)/FOUR, 0);
p2str(s);
}



void
p2str(s)
register char *s;
{
	while (*s)
		fputc(*s++, textfile);
	fputc('\n', textfile);
}



void
p2triple(int op, int var, int type)
{
	fprintf(textfile, "TRIPLE op=%d var=%d type=%d\n", op, var, type);
}



void
p2name(s)
char *s;
{
	while (*s)
		fputc(*s++, textfile);
	fputc('\n', textfile);
}

