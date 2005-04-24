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
/*
 * INTERMEDIATE CODE GENERATION PROCEDURES COMMON TO BOTH
 * JOHNSON AND RITCHIE FAMILIES OF SECOND PASSES
*/

#include "defs.h"

#if FAMILY == SCJ
#	include "scjdefs.h"
#else
#	include "dmrdefs"
#endif

/*
char *ops [ ] =
	{
	"??", "+", "-", "*", "/", "**", "-",
	"OR", "AND", "EQV", "NEQV", "NOT",
	"CONCAT",
	"<", "==", ">", "<=", "!=", ">=",
	" of ", " ofC ", " = ", " += ", " *= ", " CONV ", " << ", " % ",
	" , ", " ? ", " : "
	" abs ", " min ", " max ", " addr ", " indirect ",
	" bitor ", " bitand ", " bitxor ", " bitnot ", " >> ",
	};
*/

int ops2 [ ] =
	{
	P2BAD, P2PLUS, P2MINUS, P2STAR, P2SLASH, P2BAD, P2NEG,
	P2OROR, P2ANDAND, P2EQ, P2NE, P2NOT,
	P2BAD,
	P2LT, P2EQ, P2GT, P2LE, P2NE, P2GE,
	P2CALL, P2CALL, P2ASSIGN, P2PLUSEQ, P2STAREQ, P2CONV, P2LSHIFT, P2MOD,
	P2COMOP, P2QUEST, P2COLON,
	P2BAD, P2BAD, P2BAD, P2BAD, P2BAD,
	P2BITOR, P2BITAND, P2BITXOR, P2BITNOT, P2RSHIFT
	};


int types2 [ ] =
	{
	P2BAD, P2INT|P2PTR, P2SHORT, P2LONG, P2REAL, P2DREAL,
#if TARGET == INTERDATA
	P2BAD, P2BAD, P2LONG, P2CHAR, P2INT, P2BAD
#else
	P2REAL, P2DREAL, P2LONG, P2CHAR, P2INT, P2BAD
#endif
	};

void
setlog()
{
types2[TYLOGICAL] = types2[tylogical];
}

void
putex1(p)
expptr p;
{
putx( fixtype(p) );
templist = hookup(templist, holdtemps);
holdtemps = NULL;
}




void
putassign(lp, rp)
expptr lp, rp;
{
putx( fixexpr( mkexpr(OPASSIGN, lp, rp) ));
}



void
puteq(lp, rp)
expptr lp, rp;
{
putexpr( mkexpr(OPASSIGN, lp, rp) );
}




/* put code for  a *= b */
void
putsteq(a, b)
expptr a, b;
{
putx( fixexpr( mkexpr(OPSTAREQ, cpexpr(a), cpexpr(b)) ));
}





struct addrblock *realpart(p)
register struct addrblock *p;
{
register struct addrblock *q;

q = cpexpr(p);
if( ISCOMPLEX(p->vtype) )
	q->vtype += (TYREAL-TYCOMPLEX);
return(q);
}




struct addrblock *imagpart(p)
register struct addrblock *p;
{
register struct addrblock *q;
struct constblock *mkrealcon();

if( ISCOMPLEX(p->vtype) )
	{
	q = cpexpr(p);
	q->vtype += (TYREAL-TYCOMPLEX);
	q->memoffset = mkexpr(OPPLUS, q->memoffset, ICON(typesize[q->vtype]));
	}
else
	q = mkrealcon( ISINT(p->vtype) ? TYDREAL : p->vtype , 0.0);
return(q);
}

struct addrblock *putconst(p)
register struct constblock *p;
{
register struct addrblock *q;
struct literal *litp, *lastlit;
int i, k, type;
int litflavor;

if( ! ISCONST(((expptr)p)) )
	fatal1("putconst: bad tag %d", p->tag);

q = ALLOC(addrblock);
q->tag = TADDR;
type = p->vtype;
q->vtype = ( type==TYADDR ? TYINT : type );
q->vleng = cpexpr(p->vleng);
q->vstg = STGCONST;
q->memno = newlabel();
q->memoffset = ICON(0);

/* check for value in literal pool, and update pool if necessary */

switch(type = p->vtype)
	{
	case TYCHAR:
		if(p->vleng->constblock.fconst.ci > XL)
			break;	/* too long for literal table */
		litflavor = 1;
		goto loop;

	case TYREAL:
	case TYDREAL:
		litflavor = 2;
		goto loop;

	case TYLOGICAL:
		type = tylogical;
	case TYSHORT:
	case TYLONG:
		litflavor = 3;

	loop:
		lastlit = litpool + nliterals;
		for(litp = litpool ; litp<lastlit ; ++litp)
			if(type == litp->littype) switch(litflavor)
				{
			case 1:
				if(p->vleng->constblock.fconst.ci != litp->litval.litcval.litclen)
					break;
				if(! eqn( (int) p->vleng->constblock.fconst.ci, ((expptr)p)->constblock.fconst.ccp,
					litp->litval.litcval.litcstr) )
						break;

			ret:
				q->memno = litp->litnum;
				frexpr(p);
				return(q);

			case 2:
				if(((expptr)p)->constblock.fconst.cd[0] == litp->litval.litdval)
					goto ret;
				break;

			case 3:
				if(((expptr)p)->constblock.fconst.ci == litp->litval.litival)
					goto ret;
				break;
				}
		if(nliterals < MAXLITERALS)
			{
			++nliterals;
			litp->littype = type;
			litp->litnum = q->memno;
			switch(litflavor)
				{
				case 1:
					litp->litval.litcval.litclen = p->vleng->constblock.fconst.ci;
					cpn( (int) litp->litval.litcval.litclen,
						((expptr)p)->constblock.fconst.ccp,
						litp->litval.litcval.litcstr);
					break;

				case 2:
					litp->litval.litdval = ((expptr)p)->constblock.fconst.cd[0];
					break;

				case 3:
					litp->litval.litival = ((expptr)p)->constblock.fconst.ci;
					break;
				}
			}
	default:
		break;
	}

preven(typealign[ type==TYCHAR ? TYLONG : type ]);
prlabel(asmfile, q->memno);

k = 1;
switch(type)
	{
	case TYLOGICAL:
	case TYSHORT:
	case TYLONG:
		prconi(asmfile, type, ((expptr)p)->constblock.fconst.ci);
		break;

	case TYCOMPLEX:
		k = 2;
	case TYREAL:
		type = TYREAL;
		goto flpt;

	case TYDCOMPLEX:
		k = 2;
	case TYDREAL:
		type = TYDREAL;

	flpt:
		for(i = 0 ; i < k ; ++i)
			prconr(asmfile, type, ((expptr)p)->constblock.fconst.cd[i]);
		break;

	case TYCHAR:
		putstr(asmfile, ((expptr)p)->constblock.fconst.ccp, p->vleng->constblock.fconst.ci);
		break;

	case TYADDR:
		prcona(asmfile, ((expptr)p)->constblock.fconst.ci);
		break;

	default:
		fatal1("putconst: bad type %d", p->vtype);
	}

frexpr(p);
return( q );
}

/*
 * put out a character string constant.  begin every one on
 * a long integer boundary, and pad with nulls
 */
void
putstr(fp, s, n)
FILEP fp;
char *s;
ftnint n;
{
int b[SZSHORT];
int i;

i = 0;
while(--n >= 0)
	{
	b[i++] = *s++;
	if(i == SZSHORT)
		{
		prchars(fp, b);
		i = 0;
		}
	}

while(i < SZSHORT)
	b[i++] = '\0';
prchars(fp, b);
}
