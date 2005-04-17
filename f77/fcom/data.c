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
#include "defs.h"

/* ROUTINES CALLED DURING DATA STATEMENT PROCESSING */

static char datafmt[] = "%s\t%05ld\t%05ld\t%d" ;

/* another initializer, called from parser */
dataval(repp, valp)
register struct constblock *repp, *valp;
{
int i, nrep;
ftnint elen, vlen;
register struct addrblock *p;
struct addrblock *nextdata();

if(repp == NULL)
	nrep = 1;
else if (ISICON(((union expression *)repp)) && repp->fconst.ci >= 0)
	nrep = repp->fconst.ci;
else
	{
	err("invalid repetition count in DATA statement");
	frexpr(repp);
	goto ret;
	}
frexpr(repp);

if( ! ISCONST(((union expression *)valp)) )
	{
	err("non-constant initializer");
	goto ret;
	}

if(toomanyinit) goto ret;
for(i = 0 ; i < nrep ; ++i)
	{
	p = nextdata(&elen, &vlen);
	if(p == NULL)
		{
		err("too many initializers");
		toomanyinit = YES;
		goto ret;
		}
	setdata(p, valp, elen, vlen);
	frexpr(p);
	}

ret:
	frexpr(valp);
}


struct addrblock *nextdata(elenp, vlenp)
ftnint *elenp, *vlenp;
{
register struct impldoblock *ip;
struct primblock *pp;
register struct nameblock *np;
register struct rplblock *rp;
tagptr p;
expptr neltp;
register expptr q;
int skip;
ftnint off;
struct constblock *mkintcon();

while(curdtp)
	{
	p = curdtp->chain.datap;
	if(p->nameblock.tag == TIMPLDO)
		{
		ip = p;
		if(ip->implb==NULL || ip->impub==NULL || ip->varnp==NULL)
			fatal1("bad impldoblock 0%o", ip);
		if(ip->isactive)
			ip->varvp->fconst.ci += ip->impdiff;
		else
			{
			q = fixtype(cpexpr(ip->implb));
			if( ! ISICON(q) )
				goto doerr;
			ip->varvp = q;

			if(ip->impstep)
				{
				q = fixtype(cpexpr(ip->impstep));
				if( ! ISICON(q) )
					goto doerr;
				ip->impdiff = q->constblock.fconst.ci;
				frexpr(q);
				}
			else
				ip->impdiff = 1;

			q = fixtype(cpexpr(ip->impub));
			if(! ISICON(q))
				goto doerr;
			ip->implim = q->constblock.fconst.ci;
			frexpr(q);

			ip->isactive = YES;
			rp = ALLOC(rplblock);
			rp->nextp = rpllist;
			rpllist = rp;
			rp->rplnp = ip->varnp;
			rp->rplvp = ip->varvp;
			rp->rpltag = TCONST;
			}

		if( (ip->impdiff>0 && (ip->varvp->fconst.ci <= ip->implim))
		 || (ip->impdiff<0 && (ip->varvp->fconst.ci >= ip->implim)) )
			{ /* start new loop */
			curdtp = ip->datalist;
			goto next;
			}

		/* clean up loop */

		popstack(&rpllist);

		frexpr(ip->varvp);
		ip->isactive = NO;
		curdtp = curdtp->chain.nextp;
		goto next;
		}

	pp = p;
	np = pp->namep;
	skip = YES;

	if(p->primblock.argsp==NULL && np->vdim!=NULL)
		{   /* array initialization */
		q = mkaddr(np);
		off = typesize[np->vtype] * curdtelt;
		if(np->vtype == TYCHAR)
			off *= np->vleng->constblock.fconst.ci;
		q->addrblock.memoffset = mkexpr(OPPLUS, q->addrblock.memoffset, mkintcon(off) );
		if( (neltp = np->vdim->nelt) && ISCONST(neltp))
			{
			if(++curdtelt < neltp->constblock.fconst.ci)
				skip = NO;
			}
		else
			err("attempt to initialize adjustable array");
		}
	else
		q = mklhs( cpexpr(pp) );
	if(skip)
		{
		curdtp = curdtp->chain.nextp;
		curdtelt = 0;
		}
	if(q->addrblock.vtype == TYCHAR)
		if(ISICON(q->exprblock.vleng))
			*elenp = q->exprblock.vleng->constblock.fconst.ci;
		else	{
			err("initialization of string of nonconstant length");
			continue;
			}
	else	*elenp = typesize[q->exprblock.vtype];

	if(np->vstg == STGCOMMON)
		*vlenp = extsymtab[np->vardesc.varno].maxleng;
	else if(np->vstg == STGEQUIV)
		*vlenp = eqvclass[np->vardesc.varno].eqvleng;
	else	{
		*vlenp =  (np->vtype==TYCHAR ?
				np->vleng->constblock.fconst.ci : typesize[np->vtype]);
		if(np->vdim)
			*vlenp *= np->vdim->nelt->constblock.fconst.ci;
		}
	return(q);

doerr:
		err("nonconstant implied DO parameter");
		frexpr(q);
		curdtp = curdtp->chain.nextp;

next:	curdtelt = 0;
	}

return(NULL);
}






LOCAL setdata(varp, valp, elen, vlen)
struct addrblock *varp;
ftnint elen, vlen;
struct constblock *valp;
{
union constant con;
int i, k;
int stg, type, valtype;
ftnint offset;
register char *s, *t;
char *memname();
static char varname[XL+2];

/* output form of name is padded with blanks and preceded
   with a storage class digit
*/

stg = varp->vstg;
varname[0] = (stg==STGCOMMON ? '2' : (stg==STGEQUIV ? '1' : '0') );
s = memname(stg, varp->memno);
for(t = varname+1 ; *s ; )
	*t++ = *s++;
while(t < varname+XL+1)
	*t++ = ' ';
varname[XL+1] = '\0';

offset = varp->memoffset->constblock.fconst.ci;
type = varp->vtype;
valtype = valp->vtype;
if(type!=TYCHAR && valtype==TYCHAR)
	{
	if(! ftn66flag)
		warn("non-character datum initialized with character string");
	varp->vleng = ICON(typesize[type]);
	varp->vtype = type = TYCHAR;
	}
else if( (type==TYCHAR && valtype!=TYCHAR) ||
	 (cktype(OPASSIGN,type,valtype) == TYERROR) )
	{
	err("incompatible types in initialization");
	return;
	}
if(type != TYCHAR)
	if(valtype == TYUNKNOWN)
		con.ci = valp->fconst.ci;
	else	consconv(type, &con, valtype, &valp->fconst);

k = 1;
switch(type)
	{
	case TYLOGICAL:
		type = tylogical;
	case TYSHORT:
	case TYLONG:
		fprintf(initfile, datafmt, varname, offset, vlen, type);
		prconi(initfile, type, con.ci);
		break;

	case TYCOMPLEX:
		k = 2;
		type = TYREAL;
	case TYREAL:
		goto flpt;

	case TYDCOMPLEX:
		k = 2;
		type = TYDREAL;
	case TYDREAL:
	flpt:

		for(i = 0 ; i < k ; ++i)
			{
			fprintf(initfile, datafmt, varname, offset, vlen, type);
			prconr(initfile, type, con.cd[i]);
			offset += typesize[type];
			}
		break;

	case TYCHAR:
		k = valp->vleng->constblock.fconst.ci;
		if(elen < k)
			k = elen;

		for(i = 0 ; i < k ; ++i)
			{
			fprintf(initfile, datafmt, varname, offset++, vlen, TYCHAR);
			fprintf(initfile, "\t%d\n", valp->fconst.ccp[i]);
			}
		k = elen - valp->vleng->constblock.fconst.ci;
		while( k-- > 0)
			{
			fprintf(initfile, datafmt, varname, offset++, vlen, TYCHAR);
			fprintf(initfile, "\t%d\n", ' ');
			}
		break;

	default:
		fatal1("setdata: impossible type %d", type);
	}

}



frdata(p0)
chainp p0;
{
register chainp p;
register tagptr q;

for(p = p0 ; p ; p = p->chain.nextp)
	{
	q = p->chain.datap;
	if(q->nameblock.tag == TIMPLDO)
		{
		if(q->impldoblock.isbusy)
			return;	/* circular chain completed */
		q->impldoblock.isbusy = YES;
		frdata(q->impldoblock.datalist);
		free(q);
		}
	else
		frexpr(q);
	}

frchain( &p0);
}
