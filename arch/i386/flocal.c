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

#include "ftypes.h"
#include "defines.h"
#include "defs.h"

void
prchars(int *s)
{
	printf("\t.byte 0%o,0%o\n", s[0], s[1]);
}


void
pruse(FILEP fp, char *s)
{
	printf("\t%s\n", s);
}


void
prskip(FILEP fp, ftnint k)
{
	printf("\t.space\t%ld\n", k);
}

void
setloc(int l)
{
	static char *loctbl[] =
	    { "text", "data", "section .rodata", "section .rodata", "bss" };
	printf("\t.%s\n", loctbl[l]);
}

void
prcomblock(fp, name)
FILEP fp;
char *name;
{
fprintf(fp, FLABELFMT, name);
}

#ifdef FCOM


/*
	PDP11-780/VAX - SPECIFIC PRINTING ROUTINES
*/

int maxregvar;
int regnum[] =  { 11, 10, 9, 8, 7, 6 } ;
static int regmask[] = { 0, 0x800, 0xc00, 0xe00, 0xf00, 0xf80, 0xfc0 };



void
prsave()
{
	int proflab;
	printf("\t.word\t0x%x\n", regmask[highregvar]);	/*  register variable mask */
	if(profileflag) {
		proflab = newlabel();
		printf("L%d:\t.space\t4\n", proflab);
		printf("\tmovab\tL%d,r0\n", proflab);
		printf("\tjsb\tmcount\n");
	}
	printf("\tsubl2\t$.F%d,sp\n", procno);
}

/*
 * Called just before return from a subroutine.
 */
void
goret(int type)
{
}




/*
 * move argument slot arg1 (relative to ap)
 * to slot arg2 (relative to ARGREG)
 */
void
mvarg(type, arg1, arg2)
int type, arg1, arg2;
{
	printf("\tmovl\t%d(ap),%d(fp)\n", arg1+ARGOFFSET, arg2+argloc) ;
}

/*
 * Print out a label.
 */
void
prlabel(int k)
{
	printf(LABFMT ":\n", k);
}


/*
 * Print integer constant.
 */
void
prconi(int type, ftnint n)
{
	printf("\t%s\t%ld\n", (type==TYSHORT ? ".word" : ".long"), n);
}

/*
 * Print address constant, given as a label number.
 */
void
prcona(ftnint a)
{
	printf("\t.long\t" LABFMT "\n", (int)a);
}

/*
 * Print out a floating constant.
 */
void
prconr(int type, double x)
{
	printf("\t%s\t0f%e\n", (type==TYREAL ? ".float" : ".double"), x);
}

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
preven(int k)
{
	int lg;

	if(k > 4)
		lg = 3;
	else if(k > 2)
		lg = 2;
	else if(k > 1)
		lg = 1;
	else
		return;
	printf("\t.align\t%d\n", lg);
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
printf("\tcasel\tr0,$1,$%d", nlab-1) ;
printf("L%d:", arrlab = newlabel() ) ;
for(i = 0; i< nlab ; ++i) {
	printf("\t.word\tL%d-L%d", labs[i]->labelno, arrlab) ;
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
	printf("\ttstl\tr0\n") ;
} else {
	printf("\ttstd\tr0\n") ;
}
printf("\tjlss\tL%d\n", neg) ;
printf("\tjeql\tL%d\n", zer) ;
printf("\tjbr\tL%d\n", pos) ;
}

/*
 * Convert a tag and offset into the symtab table to a string.
 * An external string is never longer than XL bytes.
 */
char *
memname(int stg, int mem)
{
#define	MLEN	(XL + 10)
	char *s = malloc(MLEN);

	switch(stg) {
	case STGCOMMON:
	case STGEXT:
		snprintf(s, MLEN, "%s", varstr(XL, extsymtab[mem].extname));
		break;

	case STGBSS:
	case STGINIT:
		snprintf(s, MLEN, "v.%d", mem);
		break;

	case STGCONST:
		snprintf(s, MLEN, ".L%d", mem);
		break;

	case STGEQUIV:
		snprintf(s, MLEN, "q.%d", mem);
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
	printf("\t.lcomm\t%s,%ld\n", s, len);
}


void
prext(char *name, ftnint leng, int init)
{
	if(leng == 0)
		printf("\t.globl\t_%s\n", name);
	else
		printf("\t.comm\t_%s,%ld\n", name, leng);
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
prolog(struct entrypoint *ep, struct bigblock *argvec)
{
	int i, argslot;
	int size;
	chainp p;
	struct bigblock *q;
	struct dimblock *dp;

//printf("prolog! entryname %s\n", ep->entryname->extname);
return;
if(procclass == CLMAIN)
	printf( "_MAIN__:\n" );
if(ep->entryname) {
	printf("_%s:\n",  varstr(XL, ep->entryname->extname)) ;
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
	printf("\taddl3\t$%d,fp,ap\n", argloc-ARGOFFSET);
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
					fixtype( mkexpr(OPSTAR, MKICON(size),
						cpexpr(dp->baseoffset)) ));
				printf("\tsubl2\tr0,%d(ap)\n",
					p->chain.datap->b_name.vardesc.varno + ARGOFFSET);
				}
			}
		else if(!checksubs && dp->baseoffset->b_const.fconst.ci!=0) {
			printf("\tsubl2\t$%ld,%d(ap)\n",
				dp->baseoffset->b_const.fconst.ci * size,
				p->chain.datap->b_name.vardesc.varno + ARGOFFSET) ;
			}
		}
	}

if(typeaddr)
	puteq( cpexpr(typeaddr), mkaddcon(ep->typelabel) );
putgoto(ep->entrylabel);
}



void
prhead(FILEP fp)
{
	printf("[%02d\t%06ld\t%02d\t\n", procno,
		SZCHAR*autoleng, ARGREG-highregvar);
}


void
prdbginfo()
{
}

void
prcmgoto(bigptr a, int b, int c, int d)
{
	fatal1("Fix computed goto\n");
}

static void
fcheck(NODE *p)
{
	NODE *r, *l;

	switch (p->n_op) {
	case CALL: /* fix arguments */
		for (r = p->n_right; r->n_op == CM; r = r->n_left) {
			r->n_right = mkunode(FUNARG, r->n_right, 0,
			    r->n_right->n_type);
		}
		l = talloc();
		*l = *r;
		r->n_op = FUNARG;
		r->n_left = l;
		r->n_type = l->n_type;
		break;
	}
}

/*
 * Called just before the tree is written out to pass2.
 */
void p2tree(NODE *p);
void
p2tree(NODE *p)
{
	walkf(p, fcheck);
}
#endif /* FCOM */
