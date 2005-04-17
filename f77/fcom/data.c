#include "defs"

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
else if (ISICON(repp) && repp->const.ci >= 0)
	nrep = repp->const.ci;
else
	{
	err("invalid repetition count in DATA statement");
	frexpr(repp);
	goto ret;
	}
frexpr(repp);

if( ! ISCONST(valp) )
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
	p = curdtp->datap;
	if(p->tag == TIMPLDO)
		{
		ip = p;
		if(ip->implb==NULL || ip->impub==NULL || ip->varnp==NULL)
			fatal1("bad impldoblock 0%o", ip);
		if(ip->isactive)
			ip->varvp->const.ci += ip->impdiff;
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
				ip->impdiff = q->const.ci;
				frexpr(q);
				}
			else
				ip->impdiff = 1;

			q = fixtype(cpexpr(ip->impub));
			if(! ISICON(q))
				goto doerr;
			ip->implim = q->const.ci;
			frexpr(q);

			ip->isactive = YES;
			rp = ALLOC(rplblock);
			rp->nextp = rpllist;
			rpllist = rp;
			rp->rplnp = ip->varnp;
			rp->rplvp = ip->varvp;
			rp->rpltag = TCONST;
			}

		if( (ip->impdiff>0 && (ip->varvp->const.ci <= ip->implim))
		 || (ip->impdiff<0 && (ip->varvp->const.ci >= ip->implim)) )
			{ /* start new loop */
			curdtp = ip->datalist;
			goto next;
			}

		/* clean up loop */

		popstack(&rpllist);

		frexpr(ip->varvp);
		ip->isactive = NO;
		curdtp = curdtp->nextp;
		goto next;
		}

	pp = p;
	np = pp->namep;
	skip = YES;

	if(p->argsp==NULL && np->vdim!=NULL)
		{   /* array initialization */
		q = mkaddr(np);
		off = typesize[np->vtype] * curdtelt;
		if(np->vtype == TYCHAR)
			off *= np->vleng->const.ci;
		q->memoffset = mkexpr(OPPLUS, q->memoffset, mkintcon(off) );
		if( (neltp = np->vdim->nelt) && ISCONST(neltp))
			{
			if(++curdtelt < neltp->const.ci)
				skip = NO;
			}
		else
			err("attempt to initialize adjustable array");
		}
	else
		q = mklhs( cpexpr(pp) );
	if(skip)
		{
		curdtp = curdtp->nextp;
		curdtelt = 0;
		}
	if(q->vtype == TYCHAR)
		if(ISICON(q->vleng))
			*elenp = q->vleng->const.ci;
		else	{
			err("initialization of string of nonconstant length");
			continue;
			}
	else	*elenp = typesize[q->vtype];

	if(np->vstg == STGCOMMON)
		*vlenp = extsymtab[np->vardesc.varno].maxleng;
	else if(np->vstg == STGEQUIV)
		*vlenp = eqvclass[np->vardesc.varno].eqvleng;
	else	{
		*vlenp =  (np->vtype==TYCHAR ?
				np->vleng->const.ci : typesize[np->vtype]);
		if(np->vdim)
			*vlenp *= np->vdim->nelt->const.ci;
		}
	return(q);

doerr:
		err("nonconstant implied DO parameter");
		frexpr(q);
		curdtp = curdtp->nextp;

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

offset = varp->memoffset->const.ci;
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
		con.ci = valp->const.ci;
	else	consconv(type, &con, valtype, &valp->const);

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
		k = valp->vleng->const.ci;
		if(elen < k)
			k = elen;

		for(i = 0 ; i < k ; ++i)
			{
			fprintf(initfile, datafmt, varname, offset++, vlen, TYCHAR);
			fprintf(initfile, "\t%d\n", valp->const.ccp[i]);
			}
		k = elen - valp->vleng->const.ci;
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

for(p = p0 ; p ; p = p->nextp)
	{
	q = p->datap;
	if(q->tag == TIMPLDO)
		{
		if(q->isbusy)
			return;	/* circular chain completed */
		q->isbusy = YES;
		frdata(q->datalist);
		free(q);
		}
	else
		frexpr(q);
	}

frchain( &p0);
}
