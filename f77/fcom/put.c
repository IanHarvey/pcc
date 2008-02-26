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

#include "macdefs.h"

#include "ftypes.h"
#include "defines.h"
#include "defs.h"

#include "scjdefs.h"

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
bigptr p;
{
putx( fixtype(p) );
templist = hookup(templist, holdtemps);
holdtemps = NULL;
}




void
putassign(lp, rp)
bigptr lp, rp;
{
putx( fixexpr( mkexpr(OPASSIGN, lp, rp) ));
}



void
puteq(lp, rp)
bigptr lp, rp;
{
putexpr( mkexpr(OPASSIGN, lp, rp) );
}




/* put code for  a *= b */
void
putsteq(a, b)
bigptr a, b;
{
putx( fixexpr( mkexpr(OPSTAREQ, cpexpr(a), cpexpr(b)) ));
}





struct bigblock *realpart(p)
register struct bigblock *p;
{
register struct bigblock *q;

q = cpexpr(p);
if( ISCOMPLEX(p->vtype) )
	q->vtype += (TYREAL-TYCOMPLEX);
return(q);
}




struct bigblock *imagpart(p)
register struct bigblock *p;
{
register struct bigblock *q;

if( ISCOMPLEX(p->vtype) )
	{
	q = cpexpr(p);
	q->vtype += (TYREAL-TYCOMPLEX);
	q->b_addr.memoffset = mkexpr(OPPLUS, q->b_addr.memoffset, ICON(typesize[q->vtype]));
	}
else
	q = mkrealcon( ISINT(p->vtype) ? TYDREAL : p->vtype , 0.0);
return(q);
}

struct bigblock *putconst(p)
register struct bigblock *p;
{
register struct bigblock *q;
struct literal *litp, *lastlit;
int i, k, type;
int litflavor;

if( ! ISCONST(p) )
	fatal1("putconst: bad tag %d", p->tag);

q = BALLO();
q->tag = TADDR;
type = p->vtype;
q->vtype = ( type==TYADDR ? TYINT : type );
q->vleng = cpexpr(p->vleng);
q->vstg = STGCONST;
q->b_addr.memno = newlabel();
q->b_addr.memoffset = ICON(0);

/* check for value in literal pool, and update pool if necessary */

switch(type = p->vtype)
	{
	case TYCHAR:
		if(p->vleng->b_const.fconst.ci > XL)
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
				if(p->vleng->b_const.fconst.ci != litp->litval.litcval.litclen)
					break;
				if(! eqn( (int) p->vleng->b_const.fconst.ci, p->b_const.fconst.ccp,
					litp->litval.litcval.litcstr) )
						break;
			ret:
				q->b_addr.memno = litp->litnum;
				frexpr(p);
				return(q);

			case 2:
				if(p->b_const.fconst.cd[0] == litp->litval.litdval)
					goto ret;
				break;

			case 3:
				if(p->b_const.fconst.ci == litp->litval.litival)
					goto ret;
				break;
				}
		if(nliterals < MAXLITERALS)
			{
			++nliterals;
			litp->littype = type;
			litp->litnum = q->b_addr.memno;
			switch(litflavor)
				{
				case 1:
					litp->litval.litcval.litclen = p->vleng->b_const.fconst.ci;
					cpn( (int) litp->litval.litcval.litclen,
						p->b_const.fconst.ccp,
						litp->litval.litcval.litcstr);
					break;

				case 2:
					litp->litval.litdval = p->b_const.fconst.cd[0];
					break;

				case 3:
					litp->litval.litival = p->b_const.fconst.ci;
					break;
				}
			}
	default:
		break;
	}

preven(typealign[ type==TYCHAR ? TYLONG : type ]);
prlabel(asmfile, q->b_addr.memno);

k = 1;
switch(type)
	{
	case TYLOGICAL:
	case TYSHORT:
	case TYLONG:
		prconi(asmfile, type, p->b_const.fconst.ci);
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
			prconr(asmfile, type, p->b_const.fconst.cd[i]);
		break;

	case TYCHAR:
		putstr(asmfile, p->b_const.fconst.ccp, p->vleng->b_const.fconst.ci);
		break;

	case TYADDR:
		prcona(asmfile, p->b_const.fconst.ci);
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
