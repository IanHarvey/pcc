/*      $Id$   */
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

# include "pass2.h"

int e2print(NODE *p, int down, int *a, int *b);
void prttype(int t);

int fldsz, fldshf;

int s2debug = 0;

extern char *ltyp[], *rtyp[];

/*
 * return true if shape is appropriate for the node p
 * side effect for SFLD is to set up fldsz, etc
 */
int
tshape(NODE *p, int shape)
{
	int o, mask;

	o = p->n_op;

#ifdef PCC_DEBUG
	if (s2debug)
		printf("tshape(%p, %s) op = %s\n", p, prcook(shape), opst[o]);
#endif

	if (shape & SPECIAL) {

		switch (shape) {
		case SZERO:
		case SONE:
		case SMONE:
		case SSCON:
		case SCCON:
			if (o != ICON || p->n_name[0])
				return SRNOPE;
			if (p->n_lval == 0 && shape == SZERO)
				return SRDIR;
			if (p->n_lval == 1 && shape == SONE)
				return SRDIR;
			if (p->n_lval == -1 && shape == SMONE)
				return SRDIR;
			if (p->n_lval > -257 && p->n_lval < 256 &&
			    shape == SCCON)
				return SRDIR;
			if (p->n_lval > -32769 && p->n_lval < 32768 &&
			    shape == SSCON)
				return SRDIR;
			return SRNOPE;

		case SSOREG:	/* non-indexed OREG */
			if (o == OREG && !R2TEST(p->n_rval))
				return SRDIR;
			return SRNOPE;

		default:
			return (special(p, shape));
		}
	}

	if (shape & SANY)
		return SRDIR;

	if ((shape&INTEMP) && shtemp(p))
		return SRDIR;

	if ((shape&SWADD) && (o==NAME||o==OREG))
		if (BYTEOFF(p->n_lval))
			return SRNOPE;

	switch (o) {

	case NAME:
		if (shape & SNAME)
			return SRDIR;
		break;

	case ICON:
		if (shape & SCON)
			return SRDIR;
		break;

	case FLD:
		if (shape & SFLD) {
			int sh;

			if ((sh = flshape(p->n_left)) == SRNOPE)
				return sh;
			/* it is a FIELD shape; make side-effects */
			/* XXX - this will not work for multi-matches */
			o = p->n_rval;
			fldsz = UPKFSZ(o);
# ifdef RTOLBYTES
			fldshf = UPKFOFF(o);
# else
			fldshf = SZINT - fldsz - UPKFOFF(o);
# endif
			return sh;
		}
		break;

	case CCODES:
		if (shape & SCC)
			return SRDIR;
		break;

	case TEMP: /* temporaries are handled as registers */
#if 0
		mask = PCLASS(p);
		if (shape & mask)
			return SRREG; /* let register allocator coalesce */
#endif
		break;

	case REG:
		mask = PCLASS(p);
		if (shape & mask)
			return SRDIR;
		break;

	case OREG:
		if (shape & SOREG)
			return SRDIR;
		break;

	case UMUL:
		if (shumul(p->n_left) & shape)
			return SROREG;	/* Call offstar to do an OREG */
		break;

	}
#ifdef SNH_REG
	if (shape & PCLASS(p))
		return SRREG;	/* Can put in register */
#else
	if (shape & (SAREG|SBREG))
		return SRREG;	/* Can put in register */
#endif

	return SRNOPE;
}

/*
 * does the type t match tword
 */
int
ttype(TWORD t, int tword)
{
	if (tword & TANY)
		return(1);

#ifdef PCC_DEBUG
	if (t2debug)
		printf("ttype(%o, %o)\n", t, tword);
#endif
	if (ISPTR(t) && ISFTN(DECREF(t)) && (tword & TFTN)) {
		/* For funny function pointers */
		return 1;
	}
	if (ISPTR(t) && (tword&TPTRTO)) {
		do {
			t = DECREF(t);
		} while (ISARY(t));
			/* arrays that are left are usually only
			 * in structure references...
			 */
		return (ttype(t, tword&(~TPTRTO)));
	}
	if (t != BTYPE(t))
		return (tword & TPOINT); /* TPOINT means not simple! */
	if (tword & TPTRTO)
		return(0);

	switch (t) {
	case CHAR:
		return( tword & TCHAR );
	case SHORT:
		return( tword & TSHORT );
	case STRTY:
	case UNIONTY:
		return( tword & TSTRUCT );
	case INT:
		return( tword & TINT );
	case UNSIGNED:
		return( tword & TUNSIGNED );
	case USHORT:
		return( tword & TUSHORT );
	case UCHAR:
		return( tword & TUCHAR );
	case ULONG:
		return( tword & TULONG );
	case LONG:
		return( tword & TLONG );
	case LONGLONG:
		return( tword & TLONGLONG );
	case ULONGLONG:
		return( tword & TULONGLONG );
	case FLOAT:
		return( tword & TFLOAT );
	case DOUBLE:
		return( tword & TDOUBLE );
	case LDOUBLE:
		return( tword & TLDOUBLE );
	}

	return(0);
}

/*
 * generate code by interpreting table entry
 */
void
expand(NODE *p, int cookie, char *cp)
{
	CONSZ val;

	for( ; *cp; ++cp ){
		switch( *cp ){

		default:
			PUTCHAR( *cp );
			continue;  /* this is the usual case... */

		case 'Z':  /* special machine dependent operations */
			zzzcode( p, *++cp );
			continue;

		case 'F':  /* this line deleted if FOREFF is active */
			if( cookie & FOREFF ) while( *++cp != '\n' ) ; /* VOID */
			continue;

		case 'S':  /* field size */
			printf( "%d", fldsz );
			continue;

		case 'H':  /* field shift */
			printf( "%d", fldshf );
			continue;

		case 'M':  /* field mask */
		case 'N':  /* complement of field mask */
			val = 1;
			val <<= fldsz;
			--val;
			val <<= fldshf;
			adrcon( *cp=='M' ? val : ~val );
			continue;

		case 'L':  /* output special label field */
			if (*++cp == 'C')
				printf(LABFMT, p->n_label);
			else
				printf(LABFMT, (int)getlr(p,*cp)->n_lval);
			continue;

		case 'O':  /* opcode string */
			hopcode( *++cp, p->n_op );
			continue;

		case 'B':  /* byte offset in word */
			val = getlr(p,*++cp)->n_lval;
			val = BYTEOFF(val);
			printf( CONFMT, val );
			continue;

		case 'C': /* for constant value only */
			conput(stdout, getlr( p, *++cp ) );
			continue;

		case 'I': /* in instruction */
			insput( getlr( p, *++cp ) );
			continue;

		case 'A': /* address of */
			adrput(stdout, getlr( p, *++cp ) );
			continue;

		case 'U': /* for upper half of address, only */
			upput(getlr(p, *++cp), SZLONG);
			continue;

			}

		}

	}

NODE resc[4];

NODE *
getlr(NODE *p, int c)
{
	NODE *q;

	/* return the pointer to the left or right side of p, or p itself,
	   depending on the optype of p */

	switch (c) {

	case '1':
	case '2':
	case '3':
		c -= '1';
		q = &resc[c];
		q->n_op = REG;
		q->n_type = p->n_type; /* ???? */
		q->n_rval = p->n_rall; /* Should be assigned by genregs() */
		q->n_rval += szty(q->n_type) * c;
		return q;

	case 'L':
		return( optype( p->n_op ) == LTYPE ? p : p->n_left );

	case 'R':
		return( optype( p->n_op ) != BITYPE ? p : p->n_right );

	}
	cerror( "bad getlr: %c", c );
	/* NOTREACHED */
	return NULL;
}

static char *tarr[] = {
	"CHAR", "SHORT", "INT", "LONG", "FLOAT", "DOUBLE", "POINT", "UCHAR",
	"USHORT", "UINT", "ULONG", "PTRTO", "ANY", "STRUCT", "LONGLONG",
	"ULONGLONG",
};

void
prttype(int t)
{
	int i, gone = 0;

	for (i = 0; i < 16; i++)
		if ((t >> i) & 1) {
			if (gone) putchar('|');
			gone++;
			printf("%s", tarr[i]);
		}
}


static int shltab[] = { 0, 0, LOREG, LREG };
static int shrtab[] = { 0, 0, ROREG, RREG };

/*
 * Find the best ops for a given tree. 
 * Different instruction sequences are graded as:
  	add2 reg,reg	 = 0
	add2 mem,reg	 = 1
	add3 mem,reg,reg = 2
	add3 reg,mem,reg = 2
	add3 mem,mem,reg = 3
	move mem,reg ; add2 mem,reg 	= 4
	move mem,reg ; add3 mem,reg,reg = 5
	move mem,reg ; move mem,reg ; add2 reg,reg = 6
	move mem,reg ; move mem,reg ; add3 reg,reg,reg = 7
 * The instruction with the lowest grading is emitted.
 */
int
findops(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl, shr, tl, tr, is3;
	NODE *l, *r;
	int *ixp;
	int rv = -1, mtchno = 10;

if (f2debug) printf("findops tree:\n");
if (f2debug) fwalk(p, e2print, 0);

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("findop: ixp %d\n", ixp[i]);
		l = getlr(p, 'L');
		r = getlr(p, 'R');
		if (ttype(l->n_type, q->ltype) == 0 ||
		    ttype(r->n_type, q->rtype) == 0)
			continue; /* Types must be correct */

if (f2debug) printf("findop got types\n");
		if ((shl = tshape(l, q->lshape)) == SRNOPE)
			continue; /* useless */
if (f2debug) printf("findop lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);
		if ((shr = tshape(r, q->rshape)) == SRNOPE)
			continue; /* useless */
if (f2debug) printf("findop rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		tl = tr = 1;  /* XXX remove later */
		is3 = ((q->rewrite & (RLEFT|RRIGHT)) == 0);

		if (shl == SRDIR && shr== SRDIR ) {
			int got = 10;
			/*
			 * Both shapes matches direct. If one of them is
			 * in a temp register and there is a corresponding
			 * 2-op instruction, be very happy. If not, but
			 * there is a 3-op instruction that ends in a reg,
			 * be quite happy. If neither, cannot do anything.
			 */
			if (tl && (q->rewrite & RLEFT)) {
				got = 1;
			} else if (tr && (q->rewrite & RRIGHT)) {
				got = 1;
			} else if ((q->rewrite & (RLEFT|RRIGHT)) == 0) {
				got = 3;
			}
			if (got < mtchno) {
				mtchno = got;
				rv = MKIDX(ixp[i], 0);
			}
			if (got != 10)
				continue;
		}
if (f2debug) printf("second\n");
		if (shr == SRDIR) {
			/*
			 * Right shape matched. If left node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if (q->rewrite & RLEFT) {
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], LREG);
				}
				continue; /* Can't do anything else */
			} else if (is3) {
				if (5 < mtchno) {
					mtchno = 5;
					rv = MKIDX(ixp[i], shltab[shl]);
				}
				continue; /* Can't do anything else */
			}
		}
if (f2debug) printf("third\n");
		if (shl == SRDIR) {
			/*
			 * Left shape matched. If right node can be put into
			 * a temporary register, and the current op matches,
			 * be happy.
			 */
			if (q->rewrite & RRIGHT) {
				if (4 < mtchno) {
					mtchno = 4;
					rv = MKIDX(ixp[i], RREG);
				}
				continue; /* Can't do anything */
			} else if (is3) {
				if (5 < mtchno) {
					mtchno = 5;
					rv = MKIDX(ixp[i], shrtab[shr]);
				}
				continue; /* Can't do anything */
			}
		}
		/*
		 * Neither of the shapes matched. Put both args in 
		 * regs and be done with it.
		 */
		if (is3) {
			if (7 < mtchno) {
				mtchno = 7;
				rv = MKIDX(ixp[i], shltab[shl]|shrtab[shr]);
			}
		} else {
			if (6 < mtchno) {
				mtchno = 6;
				if (q->rewrite & RLEFT)
					rv = MKIDX(ixp[i], shrtab[shr]|LREG);
				else
					rv = MKIDX(ixp[i], shltab[shl]|RREG);
			}
		}
	}
#ifdef PCC_DEBUG
	if (f2debug) {
		if (rv == -1)
			printf("findops failed\n");
		else
			printf("findops entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}

/*
 * Find the best relation op for matching the two trees it has.
 * This is a sub-version of the function findops() above.
 * The instruction with the lowest grading is emitted.
 *
 * Level assignment for priority:
 *	left	right	prio
 *	-	-	-
 *	direct	direct	1
 *	direct	OREG	2	# make oreg
 *	OREG	direct	2	# make oreg
 *	OREG	OREG	2	# make both oreg
 *	direct	REG	3	# put in reg
 *	OREG	REG	3	# put in reg, make oreg
 *	REG	direct	3	# put in reg
 *	REG	OREG	3	# put in reg, make oreg
 *	REG	REG	4	# put both in reg
 */
int
relops(NODE *p)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl, shr;
	NODE *l, *r;
	int *ixp;
	int rv = -1, mtchno = 10;

if (f2debug) printf("relops tree:\n");
if (f2debug) fwalk(p, e2print, 0);

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("relops: ixp %d\n", ixp[i]);
		l = getlr(p, 'L');
		r = getlr(p, 'R');
		if (ttype(l->n_type, q->ltype) == 0 ||
		    ttype(r->n_type, q->rtype) == 0)
			continue; /* Types must be correct */

if (f2debug) printf("relops got types\n");
		shl = tshape(l, q->lshape);
		if (shl == 0)
			continue; /* useless */
if (f2debug) printf("relops lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);
		shr = tshape(r, q->rshape);
		if (shr == 0)
			continue; /* useless */
if (f2debug) printf("relops rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (shl+shr < mtchno) {
			mtchno = shl+shr;
			rv = MKIDX(ixp[i], shltab[shl]|shrtab[shr]);
		}
	}
#ifdef PCC_DEBUG
	if (f2debug) {
		if (rv == -1)
			printf("relops failed\n");
		else
			printf("relops entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}

/*
 * Find a matching assign op.
 *
 * Level assignment for priority:
 * 	left	right	prio
 *	-	-	-
 *	direct	direct	1
 *	direct	REG	2
 *	direct	OREG	3
 *	OREG	direct	4
 *	OREG	REG	5
 *	OREG	OREG	6
 */
int
findasg(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl, shr, lvl = 10;
	NODE *l, *r;
	int *ixp;
	int rv = -1;

#ifdef PCC_DEBUG
	if (f2debug) {
		printf("findasg tree: %s\n", prcook(cookie));
		fwalk(p, e2print, 0);
	}
#endif

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("asgop: ixp %d\n", ixp[i]);
		l = getlr(p, 'L');
		r = getlr(p, 'R');
		if (ttype(l->n_type, q->ltype) == 0 ||
		    ttype(r->n_type, q->rtype) == 0)
			continue; /* Types must be correct */

		if ((cookie & (INTAREG|INTBREG)) &&
		    (q->rewrite & (RLEFT|RRIGHT)) == 0)
			continue; /* must get a result somehere */

if (f2debug) printf("asgop got types\n");
		if ((shl = tshape(l, q->lshape)) == SRNOPE)
			continue;

		if (p->n_left->n_op == TEMP)
			shl = SRDIR;
		else if (shl == SRREG)
			continue;

if (f2debug) printf("asgop lshape %d\n", shl);
if (f2debug) fwalk(l, e2print, 0);

		if ((shr = tshape(r, q->rshape)) == SRNOPE)
			continue; /* useless */

if (f2debug) printf("asgop rshape %d\n", shr);
if (f2debug) fwalk(r, e2print, 0);
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (lvl <= (shl + shr))
			continue;
		lvl = shl + shr;
		
		rv = MKIDX(ixp[i], shltab[shl]|shrtab[shr]);
	}
#ifdef PCC_DEBUG
	if (f2debug) {
		if (rv == -1)
			printf("findasg failed\n");
		else
			printf("findasg entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}

/*
 * Find an ASSIGN node that puts the value into a register.
 */
int
findleaf(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	int i, shl;
	int *ixp;
	int rv = -1;

#ifdef PCC_DEBUG
	if (f2debug) {
		printf("findleaf tree: %s\n", prcook(cookie));
		fwalk(p, e2print, 0);
	}
#endif

	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("findleaf: ixp %d\n", ixp[i]);
		if (ttype(p->n_type, q->rtype) == 0)
			continue; /* Type must be correct */

if (f2debug) printf("findleaf got types\n");
		if ((shl = tshape(p, q->rshape)) != SRDIR && p->n_op != TEMP)
			continue; /* shape must match */

		if ((q->visit & cookie) == 0)
			continue; /* wrong registers */

if (f2debug) printf("findleaf got shapes %d\n", shl);

		if (q->needs & REWRITE)
			break;	/* Done here */

		rv = MKIDX(ixp[i], 0);
		break;
	}
	if (f2debug) { 
		if (rv == -1)
			printf("findleaf failed\n");
		else
			printf("findleaf entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
	return rv;
}

/*
 * Find a UNARY op that satisfy the needs.
 * For now, the destination is always a register.
 * Both source and dest types must match, but only source (left)
 * shape is of interest.
 */
int
finduni(NODE *p, int cookie)
{
	extern int *qtable[];
	struct optab *q;
	NODE *l, *r;
	int i, shl, num = 4;
	int *ixp;
	int rv = -1;

#ifdef PCC_DEBUG
	if (f2debug) {
		printf("finduni tree: %s\n", prcook(cookie));
		fwalk(p, e2print, 0);
	}
#endif

	l = getlr(p, 'L');
	r = getlr(p, 'R');
	ixp = qtable[p->n_op];
	for (i = 0; ixp[i] >= 0; i++) {
		q = &table[ixp[i]];

if (f2debug) printf("finduni: ixp %d\n", ixp[i]);
		if (ttype(l->n_type, q->ltype) == 0)
			continue; /* Type must be correct */

if (f2debug) printf("finduni got left type\n");
		if (ttype(r->n_type, q->rtype) == 0)
			continue; /* Type must be correct */

if (f2debug) printf("finduni got types\n");
		if ((shl = tshape(l, q->lshape)) == SRNOPE)
			continue; /* shape must match */

if (f2debug) printf("finduni got shapes %d\n", shl);

		if ((cookie & q->visit) == 0)	/* check correct return value */
			continue;		/* XXX - should check needs */

if (f2debug) printf("finduni got cookie\n");
		if (q->needs & REWRITE)
			break;	/* Done here */

		if (shl >= num)
			continue;
		num = shl;
		rv = MKIDX(ixp[i], shltab[shl]);
		if ((q->lshape & (SBREG)) && !(q->lshape & (SAREG)))
			rv |= LBREG;
		if (shl == SRDIR)
			break;
	}
#ifdef PCC_DEBUG
	if (f2debug) { 
		if (rv == -1)
			printf("finduni failed\n");
		else
			printf("finduni entry %d(%s %s)\n",
			    TBLIDX(rv), ltyp[rv & LMASK], rtyp[(rv&RMASK)>>2]);
	}
#endif
	return rv;
}
