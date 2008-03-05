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

#include "ftypes.h"
#include "defines.h"
#include "defs.h"

#include "scjdefs.h"

LOCAL struct bigblock *putcall(struct bigblock *p);
LOCAL NODE *putmnmx(struct bigblock *p);
LOCAL void putcomma(int, int, int);
LOCAL NODE *putmem(bigptr, int, ftnint);
LOCAL NODE *putaddr(struct bigblock *, int);
LOCAL void putct1(bigptr, struct bigblock *, struct bigblock *, int *, int *);
LOCAL int ncat(bigptr p);
LOCAL NODE *putcat(struct bigblock *, bigptr);
LOCAL NODE *putchcmp(struct bigblock *p);
LOCAL NODE *putcheq(struct bigblock *p);
LOCAL NODE *putcxcmp(struct bigblock *p);
LOCAL struct bigblock *putcx1(bigptr, int *);
LOCAL NODE *putcxop(bigptr p);
LOCAL struct bigblock *putcxeq(struct bigblock *p);
LOCAL NODE *putpower(bigptr p);
LOCAL NODE *putop(bigptr p);
LOCAL NODE *putchop(bigptr p);
LOCAL struct bigblock *putch1(bigptr, int *);
LOCAL struct bigblock *intdouble(struct bigblock *, int *);

#define FOUR 4
extern int ops2[];
extern int types2[];
static int inproc;

#define XINT(z) 	ONEOF(z, MSKINT|MSKCHAR)
#define	P2TYPE(x)	(types2[(x)->vtype])
#define	P2OP(x)		(ops2[(x)->b_expr.opcode])

static void
sendp2(NODE *p)
{
	p2tree(p);
	pass2_compile(ipnode(p));
}

static NODE *
putassign(bigptr lp, bigptr rp)
{
	return putx(fixexpr(mkexpr(OPASSIGN, lp, rp)));
}


void
puthead(char *s)
{
	struct interpass_prolog *ipp = malloc(sizeof(struct interpass_prolog));

	if (inproc)
		fatal1("puthead %s in procedure", s);
	inproc = 1;
	ipp->ipp_regs = 0;
	ipp->ipp_autos = 0;
	ipp->ipp_name = s;
	ipp->ipp_type = 0; /* XXX */
	ipp->ipp_vis = 0; /* XXX */
	ipp->ip_tmpnum = 0; /* XXX */
	ipp->ip_lblnum = 0; /* XXX */
	ipp->ipp_ip.ip_lbl = 0; /* XXX */
	ipp->ipp_ip.type = IP_PROLOG;
	pass2_compile((struct interpass *)ipp);
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
	struct interpass_prolog *ipp = malloc(sizeof(struct interpass_prolog));

	if (inproc == 0)
		fatal1("puteof outside procedure");
	inproc = 1;
	ipp->ipp_regs = 0;
	ipp->ipp_autos = 0;
	ipp->ipp_name = NULL;
	ipp->ipp_type = INT; /* XXX */
	ipp->ipp_vis = 1;
	ipp->ip_tmpnum = 0; /* XXX */
	ipp->ip_lblnum = 0; /* XXX */
	ipp->ipp_ip.ip_lbl = retlabel;
	ipp->ipp_ip.type = IP_EPILOG;
	pass2_compile((struct interpass *)ipp);
}



void
putrbrack(int k)
{
}


void
putnreg()
{
}

void
puteof()
{
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
	}
}





/* put out code for  goto l   */
void
putgoto(int label)
{
	NODE *p;

	p = mkunode(GOTO, mklnode(ICON, label, 0, INT), 0, INT);
	sendp2(p);
}


/* branch to address constant or integer variable */
void
putbranch(struct bigblock *q)
{
	NODE *p;

	p = mkunode(GOTO, putex1(q), 0, INT);
	sendp2(p);
}



/* put out label  l:     */
void
putlabel(int label)
{
	prlabel(0, label);
}


/*
 * Called from inner routines.  Generates a NODE tree and writes it out.
 */
void
putexpr(bigptr q)
{
	NODE *p;
	p = putex1(q);
	sendp2(p);
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

/*
 * Convert a f77 tree statement to something that looks like a
 * pcc expression tree.
 */
NODE *
putx(bigptr q)
{
	NODE *p;
	int opc;
	int ncomma;
	int type, k;

	switch(q->tag) {
	case TERROR:
		free(q);
		break;

	case TCONST:
		switch(type = q->vtype) {
			case TYLOGICAL:
				type = tyint;
			case TYLONG:
			case TYSHORT:
				p = mklnode(ICON, q->b_const.fconst.ci,
				    0, types2[type]);
				free(q);
				break;

			case TYADDR:
				p = mklnode(ICON, 0, 0, types2[type]);
				p->n_name = memname(STGCONST,
				    (int)q->b_const.fconst.ci);
				free(q);
				break;

			default:
				p = putx(putconst(q));
				break;
			}
		break;

	case TEXPR:
		switch(opc = q->b_expr.opcode) {
			case OPCALL:
			case OPCCALL:
				if( ISCOMPLEX(q->vtype) )
					p = putcxop(q);
				else {
					putcall(q);
					p = mklnode(ICON, 0, 0, INT);
				}
				break;

			case OPMIN:
			case OPMAX:
				p = putmnmx(q);
				break;

			case OPASSIGN:
				if (ISCOMPLEX(q->b_expr.leftp->vtype) ||
				    ISCOMPLEX(q->b_expr.rightp->vtype)) {
					frexpr(putcxeq(q));
				} else if (ISCHAR(q))
					p = putcheq(q);
				else
					goto putopp;
				break;

			case OPEQ:
			case OPNE:
				if (ISCOMPLEX(q->b_expr.leftp->vtype) ||
				    ISCOMPLEX(q->b_expr.rightp->vtype) ) {
					p = putcxcmp(q);
					break;
				}
			case OPLT:
			case OPLE:
			case OPGT:
			case OPGE:
				if(ISCHAR(q->b_expr.leftp))
					p = putchcmp(q);
				else
					goto putopp;
				break;

			case OPPOWER:
				p = putpower(q);
				break;

			case OPSTAR:
				/*   m * (2**k) -> m<<k   */
				if (XINT(q->b_expr.leftp->vtype) &&
				    ISICON(q->b_expr.rightp) &&
				    ((k = flog2(q->b_expr.rightp->b_const.fconst.ci))>0) ) {
					q->b_expr.opcode = OPLSHIFT;
					frexpr(q->b_expr.rightp);
					q->b_expr.rightp = MKICON(k);
					goto putopp;
				}

			case OPMOD:
				goto putopp;
			case OPPLUS:
			case OPMINUS:
			case OPSLASH:
			case OPNEG:
				if( ISCOMPLEX(q->vtype) )
					p = putcxop(q);
				else	
					goto putopp;
				break;

			case OPCONV:
				if( ISCOMPLEX(q->vtype) )
					p = putcxop(q);
				else if (ISCOMPLEX(q->b_expr.leftp->vtype)) {
					ncomma = 0;
					p = putx(mkconv(q->vtype,
						realpart(putcx1(q->b_expr.leftp, &ncomma))));
					putcomma(ncomma, q->vtype, NO);
					free(q);
				} else
					goto putopp;
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
				p = putop(q);
				break;

			default:
				fatal1("putx: invalid opcode %d", opc);
			}
		break;

	case TADDR:
		p = putaddr(q, YES);
		break;

	default:
		fatal1("putx: impossible tag %d", q->tag);
	}
	return p;
}

LOCAL NODE *
putop(bigptr q)
{
	NODE *p;
	int k;
	bigptr lp, tp;
	int pt, lt;

	switch(q->b_expr.opcode) { /* check for special cases and rewrite */
	case OPCONV:
		pt = q->vtype;
		lp = q->b_expr.leftp;
		lt = lp->vtype;
		while(q->tag==TEXPR && q->b_expr.opcode==OPCONV &&
		     ((ISREAL(pt)&&ISREAL(lt)) ||
			(XINT(pt)&&(ONEOF(lt,MSKINT|MSKADDR|MSKCHAR))) )) {
			if(lp->tag != TEXPR) {
				if(pt==TYINT && lt==TYLONG)
					break;
				if(lt==TYINT && pt==TYLONG)
					break;
			}
			free(q);
			q = lp;
			pt = lt;
			lp = q->b_expr.leftp;
			lt = lp->vtype;
		}
		if(q->tag==TEXPR && q->b_expr.opcode==OPCONV)
			break;
		p = putx(q);
		return p;

	case OPADDR:
		lp = q->b_expr.leftp;
		if(lp->tag != TADDR) {
			tp = fmktemp(lp->vtype, lp->vleng);
			p = putx(mkexpr(OPASSIGN, cpexpr(tp), lp));
			sendp2(p);
			lp = tp;
		}
		p = putaddr(lp, NO);
		free(q);
		return p;
	}

	if ((k = ops2[q->b_expr.opcode]) <= 0)
		fatal1("putop: invalid opcode %d", q->b_expr.opcode);
	p = putx(q->b_expr.leftp);
	if(q->b_expr.rightp)
		p = mkbinode(k, p, putx(q->b_expr.rightp), types2[q->vtype]);
	else
		p = mkunode(k, p, 0, types2[q->vtype]);

	if(q->vleng)
		frexpr(q->vleng);
	free(q);
	return p;
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
}

LOCAL NODE *
putpower(bigptr p)
{
	NODE *p3;
	bigptr base;
	struct bigblock *t1, *t2;
	ftnint k = 0; /* XXX gcc */
	int type;

	if(!ISICON(p->b_expr.rightp) ||
	    (k = p->b_expr.rightp->b_const.fconst.ci)<2)
		fatal("putpower: bad call");
	base = p->b_expr.leftp;
	type = base->vtype;
	t1 = fmktemp(type, NULL);
	t2 = NULL;
	p3 = putassign(cpexpr(t1), cpexpr(base) );
	sendp2(p3);

	for( ; (k&1)==0 && k>2 ; k>>=1 ) {
		p3 = putassign(cpexpr(t1),
		    mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)));
		sendp2(p3);
	}

	if(k == 2)
		p3 = putx(mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)));
	else {
		t2 = fmktemp(type, NULL);
		p3 = putassign(cpexpr(t2), cpexpr(t1));
		sendp2(p3);
	
		for(k>>=1 ; k>1 ; k>>=1) {
			p3 = putassign(cpexpr(t1),
			    mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)));
			sendp2(p3);
			if(k & 1) {
				p3 = putassign(cpexpr(t2),
				    mkexpr(OPSTAR, cpexpr(t2), cpexpr(t1)));
				sendp2(p3);
			}
		}
		p3 = putx( mkexpr(OPSTAR, cpexpr(t2),
		mkexpr(OPSTAR, cpexpr(t1), cpexpr(t1)) ));
	}
	frexpr(t1);
	if(t2)
		frexpr(t2);
	frexpr(p);
	return p3;
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
putcxeq(struct bigblock *q)
{
	struct bigblock *lp, *rp;
	int ncomma;

	ncomma = 0;
	lp = putcx1(q->b_expr.leftp, &ncomma);
	rp = putcx1(q->b_expr.rightp, &ncomma);
	putassign(realpart(lp), realpart(rp));
	if( ISCOMPLEX(q->vtype) ) {
		++ncomma;
		putassign(imagpart(lp), imagpart(rp));
	}
	putcomma(ncomma, TYREAL, NO);
	frexpr(rp);
	free(q);
	return(lp);
}



LOCAL NODE *
putcxop(bigptr q)
{
	NODE *p;
	int ncomma;

	ncomma = 0;
	p = putaddr(putcx1(q, &ncomma), NO);
	putcomma(ncomma, TYINT, NO);
	return p;
}

LOCAL struct bigblock *
putcx1(bigptr qq, int *ncommap)
{
	struct bigblock *q, *lp, *rp;
	register struct bigblock *resp;
	NODE *p;
	int opcode;
	int ltype, rtype;

	ltype = rtype = 0; /* XXX gcc */
	if(qq == NULL)
		return(NULL);

	switch(qq->tag) {
	case TCONST:
		if( ISCOMPLEX(qq->vtype) )
			qq = putconst(qq);
		return( qq );

	case TADDR:
		if( ! addressable(qq) ) {
			resp = fmktemp(tyint, NULL);
			p = putassign( cpexpr(resp), qq->b_addr.memoffset );
			sendp2(p);
			qq->b_addr.memoffset = resp;
		}
		return( qq );

	case TEXPR:
		if( ISCOMPLEX(qq->vtype) )
			break;
		resp = fmktemp(TYDREAL, NO);
		p = putassign( cpexpr(resp), qq);
		sendp2(p);
		return(resp);

	default:
		fatal1("putcx1: bad tag %d", qq->tag);
	}

	opcode = qq->b_expr.opcode;
	if(opcode==OPCALL || opcode==OPCCALL) {
		++*ncommap;
		return( putcall(qq) );
	} else if(opcode == OPASSIGN) {
		++*ncommap;
		return( putcxeq(qq) );
	}

	resp = fmktemp(qq->vtype, NULL);
	if((lp = putcx1(qq->b_expr.leftp, ncommap) ))
		ltype = lp->vtype;
	if((rp = putcx1(qq->b_expr.rightp, ncommap) ))
		rtype = rp->vtype;

	switch(opcode) {
	case OPCOMMA:
		frexpr(resp);
		resp = rp;
		rp = NULL;
		break;

	case OPNEG:
		p = putassign(realpart(resp),
		    mkexpr(OPNEG, realpart(lp), NULL));
		sendp2(p);
		p = putassign(imagpart(resp),
		    mkexpr(OPNEG, imagpart(lp), NULL));
		sendp2(p);
		break;

	case OPPLUS:
	case OPMINUS:
		p = putassign( realpart(resp),
		    mkexpr(opcode, realpart(lp), realpart(rp) ));
		sendp2(p);
		if(rtype < TYCOMPLEX) {
			p = putassign(imagpart(resp), imagpart(lp) );
		} else if(ltype < TYCOMPLEX) {
			if(opcode == OPPLUS)
				p = putassign( imagpart(resp), imagpart(rp) );
			else
				p = putassign( imagpart(resp),
				    mkexpr(OPNEG, imagpart(rp), NULL) );
		} else
			p = putassign( imagpart(resp),
			    mkexpr(opcode, imagpart(lp), imagpart(rp) ));
		sendp2(p);
		break;

	case OPSTAR:
		if(ltype < TYCOMPLEX) {
			if( ISINT(ltype) )
				lp = intdouble(lp, ncommap);
			p = putassign( realpart(resp),
			    mkexpr(OPSTAR, cpexpr(lp), realpart(rp) ));
			sendp2(p);
			p = putassign( imagpart(resp),
			    mkexpr(OPSTAR, cpexpr(lp), imagpart(rp) ));
		} else if(rtype < TYCOMPLEX) {
			if( ISINT(rtype) )
				rp = intdouble(rp, ncommap);
			p = putassign( realpart(resp),
			    mkexpr(OPSTAR, cpexpr(rp), realpart(lp) ));
			sendp2(p);
			p = putassign( imagpart(resp),
			    mkexpr(OPSTAR, cpexpr(rp), imagpart(lp) ));
		} else {
			p = putassign( realpart(resp), mkexpr(OPMINUS,
				mkexpr(OPSTAR, realpart(lp), realpart(rp)),
				mkexpr(OPSTAR, imagpart(lp), imagpart(rp)) ));
			sendp2(p);
			p = putassign( imagpart(resp), mkexpr(OPPLUS,
				mkexpr(OPSTAR, realpart(lp), imagpart(rp)),
				mkexpr(OPSTAR, imagpart(lp), realpart(rp)) ));
		}
		sendp2(p);
		break;

	case OPSLASH:
		/* fixexpr has already replaced all divisions
		 * by a complex by a function call
		 */
		if( ISINT(rtype) )
			rp = intdouble(rp, ncommap);
		p = putassign( realpart(resp),
		    mkexpr(OPSLASH, realpart(lp), cpexpr(rp)) );
		sendp2(p);
		p = putassign( imagpart(resp),
		    mkexpr(OPSLASH, imagpart(lp), cpexpr(rp)) );
		sendp2(p);
		break;

	case OPCONV:
		p = putassign( realpart(resp), realpart(lp) );
		if( ISCOMPLEX(lp->vtype) )
			q = imagpart(lp);
		else if(rp != NULL)
			q = realpart(rp);
		else
			q = mkrealcon(TYDREAL, 0.0);
		sendp2(p);
		p = putassign( imagpart(resp), q);
		sendp2(p);
		break;

	default:
		fatal1("putcx1 of invalid opcode %d", opcode);
	}

	frexpr(lp);
	frexpr(rp);
	free(qq);
	return(resp);
}


LOCAL NODE *
putcxcmp(struct bigblock *p)
{
	NODE *p1;
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
	p1 = putx( fixexpr(q) );
	putcomma(ncomma, TYINT, NO);

	free(lp);
	free(rp);
	free(p);
	return p1;
}

LOCAL struct bigblock *
putch1(bigptr p, int *ncommap)
{
	struct bigblock *t;

	switch(p->tag) {
	case TCONST:
		return( putconst(p) );

	case TADDR:
		return(p);

	case TEXPR:
		++*ncommap;

		switch(p->b_expr.opcode) {
			case OPCALL:
			case OPCCALL:
				t = putcall(p);
				break;

			case OPCONCAT:
				t = fmktemp(TYCHAR, cpexpr(p->vleng) );
				putcat( cpexpr(t), p );
				break;

			case OPCONV:
				if(!ISICON(p->vleng) ||
				    p->vleng->b_const.fconst.ci!=1
				   || ! XINT(p->b_expr.leftp->vtype) )
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




LOCAL NODE *
putchop(bigptr p)
{
	int ncomma;
	NODE *p1;

	ncomma = 0;
	p1 = putaddr( putch1(p, &ncomma) , NO );
	putcomma(ncomma, TYCHAR, YES);
	return p1;
}


/*
 * Assign a character to another.
 */
LOCAL NODE *
putcheq(struct bigblock *p)
{
	NODE *p1, *p2, *p3;
	int ncomma;

	ncomma = 0;
	if( p->b_expr.rightp->tag==TEXPR &&
	    p->b_expr.rightp->b_expr.opcode==OPCONCAT )
		p3 = putcat(p->b_expr.leftp, p->b_expr.rightp);
	else if( ISONE(p->b_expr.leftp->vleng) &&
	    ISONE(p->b_expr.rightp->vleng) ) {
		p1 = putaddr( putch1(p->b_expr.leftp, &ncomma) , YES );
		p2 = putaddr( putch1(p->b_expr.rightp, &ncomma) , YES );
		p3 = mkbinode(ASSIGN, p1, p2, CHAR);
	} else
		p3 = putx(call2(TYINT, "s_copy",
		    p->b_expr.leftp, p->b_expr.rightp));

	putcomma(ncomma, TYINT, NO);
	frexpr(p->vleng);
	free(p);
	return p3;
}



/*
 * Compare character(s) code.
 */
LOCAL NODE *
putchcmp(struct bigblock *p)
{
	NODE *p1, *p2, *p3;
	int ncomma;

	ncomma = 0;
	if(ISONE(p->b_expr.leftp->vleng) && ISONE(p->b_expr.rightp->vleng) ) {
		p1 = putaddr( putch1(p->b_expr.leftp, &ncomma) , YES );
		p2 = putaddr( putch1(p->b_expr.rightp, &ncomma) , YES );
		p3 = mkbinode(ops2[p->b_expr.opcode], p1, p2, CHAR);
		free(p);
		putcomma(ncomma, TYINT, NO);
	} else {
		p->b_expr.leftp = call2(TYINT,"s_cmp",
		    p->b_expr.leftp, p->b_expr.rightp);
		p->b_expr.rightp = MKICON(0);
		p3 = putop(p);
	}
	return p3;
}

LOCAL NODE *
putcat(bigptr lhs, bigptr rhs)
{
	NODE *p3;
	int n, ncomma;
	struct bigblock *lp, *cp;

	ncomma = 0;
	n = ncat(rhs);
	lp = mktmpn(n, TYLENG, NULL);
	cp = mktmpn(n, TYADDR, NULL);

	n = 0;
	putct1(rhs, lp, cp, &n, &ncomma);

	p3 = putx( call4(TYSUBR, "s_cat", lhs, cp, lp, MKICON(n) ) );
	putcomma(ncomma, TYINT, NO);
	return p3;
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
putct1(bigptr q, bigptr lp, bigptr cp, int *ip, int *ncommap)
{
	NODE *p;
	int i;
	struct bigblock *lp1, *cp1;

	if(q->tag==TEXPR && q->b_expr.opcode==OPCONCAT) {
		putct1(q->b_expr.leftp, lp, cp, ip, ncommap);
		putct1(q->b_expr.rightp, lp, cp , ip, ncommap);
		frexpr(q->vleng);
		free(q);
	} else {
		i = (*ip)++;
		lp1 = cpexpr(lp);
		lp1->b_addr.memoffset =
		    mkexpr(OPPLUS, lp1->b_addr.memoffset, MKICON(i*FSZLENG));
		cp1 = cpexpr(cp);
		cp1->b_addr.memoffset =
		    mkexpr(OPPLUS, cp1->b_addr.memoffset, MKICON(i*FSZADDR));
		p = putassign( lp1, cpexpr(q->vleng) );
		sendp2(p);
		p = putassign( cp1, addrof(putch1(q,ncommap)) );
		sendp2(p);
	}
}

static NODE *
putaddr(bigptr q, int indir)
{
	int type, type2, funct;
	NODE *p, *p1, *p2;
	ftnint offset;
	bigptr offp;

	type = q->vtype;
	type2 = types2[type];
	funct = (q->vclass==CLPROC ? FTN<<TSHIFT : 0);

	offp = (q->b_addr.memoffset ? cpexpr(q->b_addr.memoffset) : NULL);

	offset = simoffset(&offp);
	if(offp)
		offp = mkconv(TYINT, offp);

	switch(q->vstg) {
	case STGAUTO:
		if(indir && !offp) {
			p = mklnode(OREG, offset, AUTOREG, type2);
			break;
		}

		if(!indir && !offp && !offset) {
			p = mklnode(REG, 0, AUTOREG, INCREF(type2));
			break;
		}

		p = mklnode(REG, 0, AUTOREG, INCREF(type2));
		if(offp) {
			p1 = putx(offp);
			if(offset)
				p2 = mklnode(ICON, offset, 0, INT);
		} else
			p1 = mklnode(ICON, offset, 0, INT);
		if (offp && offset)
			p1 = mkbinode(PLUS, p1, p2, INCREF(type2));
		p = mkbinode(PLUS, p, p1, INCREF(type2));
		if (indir)
			p = mkunode(UMUL, p, 0, type2);
		break;

	case STGARG:
		p = mklnode(OREG, ARGOFFSET + (ftnint)(q->b_addr.memno),
		    ARGREG, INCREF(type2)|funct);

		if (offp)
			p1 = putx(offp);
		if (offset)
			p2 = mklnode(ICON, offset, 0, INT);
		if (offp && offset)
			p1 = mkbinode(PLUS, p1, p2, INCREF(type2));
		else if (offset)
			p1 = p2;
		if (offp || offset)
			p = mkbinode(PLUS, p, p1, INCREF(type2));
		if (indir)
			p = mkunode(UMUL, p, 0, type2);
		break;

	case STGLENG:
		if(indir) {
			p = mklnode(OREG, ARGOFFSET + (ftnint)(q->b_addr.memno),
			    ARGREG, INCREF(type2)|funct);
		} else	{
			fatal1("faddrnode: STGLENG: fixme!");
#if 0
			p2op(P2PLUS, types2[TYLENG] | P2PTR );
			p2reg(ARGREG, types2[TYLENG] | P2PTR );
			p2icon( ARGOFFSET +
				(ftnint) (FUDGEOFFSET*p->b_addr.memno), P2INT);
#endif
		}
		break;


	case STGBSS:
	case STGINIT:
	case STGEXT:
	case STGCOMMON:
	case STGEQUIV:
	case STGCONST:
		if(offp) {
			p1 = putx(offp);
			p2 = putmem(q, ICON, offset);
			p = mkbinode(PLUS, p1, p2, INCREF(type2));
			if(indir)
				p = mkunode(UMUL, p, 0, type2);
		} else
			p = putmem(q, (indir ? NAME : ICON), offset);

		break;

	case STGREG:
		if(indir)
			p = mklnode(REG, 0, q->b_addr.memno, type2);
		else
			fatal("attempt to take address of a register");
		break;

	default:
		fatal1("putaddr: invalid vstg %d", q->vstg);
	}
	frexpr(q);
	return p;
}

NODE *
putmem(bigptr q, int class, ftnint offset)
{
	NODE *p;
	int type2;
	int funct;

	funct = (q->vclass==CLPROC ? FTN<<TSHIFT : 0);
	type2 = types2[q->vtype];
	if(q->vclass == CLPROC)
		type2 |= (FTN<<TSHIFT);
	p = mklnode(class, offset, 0, INCREF(type2));
	p->n_name = memname(q->vstg, q->b_addr.memno);
	return p;
}



LOCAL struct bigblock *
putcall(struct bigblock *qq)
{
	chainp arglist, charsp, cp;
	int n, first;
	struct bigblock *t;
	struct bigblock *q;
	struct bigblock *fval;
	int type, type2, ctype, indir;
	NODE *lp, *p1, *p2;

	type2 = types2[type = qq->vtype];
	charsp = NULL;
	indir =  (qq->b_expr.opcode == OPCCALL);
	n = 0;
	first = YES;

	if(qq->b_expr.rightp) {
		arglist = qq->b_expr.rightp->b_list.listp;
		free(qq->b_expr.rightp);
	} else
		arglist = NULL;

	for(cp = arglist ; cp ; cp = cp->chain.nextp)
		if(indir) {
			++n;
		} else {
			q = cp->chain.datap;
			if(q->tag == TCONST)
				cp->chain.datap = q = putconst(q);
			if( ISCHAR(q) ) {
				charsp = hookup(charsp,
				    mkchain(cpexpr(q->vleng), 0) );
				n += 2;
			} else if(q->vclass == CLPROC) {
				charsp = hookup(charsp,
				    mkchain( MKICON(0) , 0));
				n += 2;
			} else
				n += 1;
		}

	if(type == TYCHAR) {
		if( ISICON(qq->vleng) ) {
			fval = fmktemp(TYCHAR, qq->vleng);
			n += 2;
		} else {
			err("adjustable character function");
			return NULL;
		}
	} else if(ISCOMPLEX(type)) {
		fval = fmktemp(type, NULL);
		n += 1;
	} else
		fval = NULL;

	ctype = (fval ? P2INT : type2);
	p1 = putaddr(qq->b_expr.leftp, NO);

	if(fval) {
		first = NO;
		lp = putaddr( cpexpr(fval), NO);
		if(type==TYCHAR)
			lp = mkbinode(CM, lp, putx(cpexpr(qq->vleng)), INT);
	}

	for(cp = arglist ; cp ; cp = cp->chain.nextp) {
		q = cp->chain.datap;
		if(q->tag==TADDR && (indir || q->vstg!=STGREG) )
			p2 = putaddr(q, indir && q->vtype!=TYCHAR);
		else if( ISCOMPLEX(q->vtype) )
			p2 = putcxop(q);
		else if (ISCHAR(q) ) {
			p2 = putchop(q);
		} else if( ! ISERROR(q) ) {
			if(indir)
				p2 = putx(q);
			else	{
				t = fmktemp(q->vtype, q->vleng);
				p2 = putassign( cpexpr(t), q );
				sendp2(p2);
				p2 = putaddr(t, NO);
			}
		}
		if(first) {
			first = NO;
			lp = p2;
		} else
			lp = mkbinode(CM, lp, p2, INT);
	}

	if(arglist)
		frchain(&arglist);
	for(cp = charsp ; cp ; cp = cp->chain.nextp) {
		p2 = putx( mkconv(TYLENG,cp->chain.datap) );
		lp = mkbinode(CM, lp, p2, INT);
	}
	frchain(&charsp);
	if (n > 0)
		p1 = mkbinode(CALL, p1, lp, ctype);
	else
		p1 = mkunode(UCALL, p1, 0, ctype);
	sendp2(p1);
	free(qq);
	return(fval);
}

/*
 * Write out code to do min/max calculations.
 */
LOCAL NODE *
putmnmx(struct bigblock *p)
{
fatal1("putmnmx");
#if 0
	NODE *p1, *p2;
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

	for(p1 = p0->chain.nextp ; p1 ; p1 = p1->chain.nextp) {
		++ncomma;
		putassign( cpexpr(tp), p1->chain.datap );
		if(p1->chain.nextp) {
			++ncomma;
			putassign( cpexpr(sp), cpexpr(qp) );
		} else
			putx(qp);
	}

	putcomma(ncomma, type, NO);
	frtemp(sp);
	frtemp(tp);
	frchain( &p0 );
#endif
	return NIL;
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

/*
 * F77 uses malloc() for NODEs.
 */
NODE *
talloc()
{
	NODE *p = ckalloc(sizeof(NODE));
	memset(p, 0, sizeof(NODE));
	p->n_name = "";
	return p;
}
