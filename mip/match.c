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

# include "pass2.h"

int e2print(NODE *p, int down, int *a, int *b);
void prttype(int t);

int fldsz, fldshf;

int s2debug = 0;

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
		/* XXX - register classes? */
		mask = SAREG|STAREG;
		if (shape & mask)
			return SRREG; /* let register allocator coalesce */
		break;

	case REG:
		/* distinctions:
		 * SAREG	any scalar register
		 * STAREG	any temporary scalar register
		 * SBREG	any lvalue (index) register
		 * STBREG	any temporary lvalue register
		*/
		mask = isbreg(p->n_rval) ? SBREG : SAREG;
		if (istreg(p->n_rval))
			mask |= mask==SAREG ? STAREG : STBREG;
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
	if (shape & (SAREG|STAREG|SBREG|STBREG))
		return SRREG;	/* Can put in register */

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
