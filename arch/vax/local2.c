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

# include "mfile2"
# include "ctype.h"
/* a lot of the machine dependent parts of the second pass */

# define BITMASK(n) ((1L<<n)-1)

where(c){
	fprintf( stderr, "%s, line %d: ", filename, lineno );
	}

lineid( l, fn ) char *fn; {
	/* identify line l and file fn */
	printf( "#	line %d, file %s\n", l, fn );
	}

eobl2(){
	OFFSZ spoff;	/* offset from stack pointer */

	spoff = maxoff;
	if( spoff >= AUTOINIT ) spoff -= AUTOINIT;
	spoff /= SZCHAR;
	SETOFF(spoff,4);
	printf( "	.set	.F%d,%Ld\n", ftnno, spoff );
	maxargs = -1;
	}

struct hoptab { int opmask; char * opstring; } ioptab[] = {

	ASG PLUS, "add",
	ASG MINUS, "sub",
	ASG MUL, "mul",
	ASG DIV, "div",
	ASG OR, "bis",
	ASG ER,	"xor",
	ASG AND, "bic",
	PLUS,	"add",
	MINUS,	"sub",
	MUL,	"mul",
	DIV,	"div",
	OR,	"bis",
	ER,	"xor",
	AND,	"bic",
	-1, ""    };

hopcode( f, o ){
	/* output the appropriate string from the above table */

	register struct hoptab *q;

	for( q = ioptab;  q->opmask>=0; ++q ){
		if( q->opmask == o ){
			printf( "%s", q->opstring );
/* tbl
			if( f == 'F' ) printf( "e" );
			else if( f == 'D' ) printf( "d" );
   tbl */
/* tbl */
			switch( f ) {
				case 'L':
				case 'W':
				case 'B':
				case 'D':
				case 'F':
					printf("%c", tolower(f));
					break;

				}
/* tbl */
			return;
			}
		}
	cerror( "no hoptab for %s", opst[o] );
	}

char *
rnames[] = {  /* keyed to register number tokens */

	"r0", "r1",
	"r2", "r3", "r4", "r5",
	"r6", "r7", "r8", "r9", "r10", "r11",
	"ap", "fp", "sp", "pc",

	};

int rstatus[] = {
	SAREG|STAREG, SAREG|STAREG,
	SAREG|STAREG, SAREG|STAREG, SAREG|STAREG, SAREG|STAREG,
	SAREG, SAREG, SAREG, SAREG, SAREG, SAREG,
	SAREG, SAREG, SAREG, SAREG,

	};

tlen(p) NODE *p;
{
	switch(p->type) {
		case CHAR:
		case UCHAR:
			return(1);

		case SHORT:
		case USHORT:
			return(2);

		case DOUBLE:
			return(8);

		default:
			return(4);
		}
}

mixtypes(p, q) NODE *p, *q;
{
	register tp, tq;

	tp = p->type;
	tq = q->type;

	return( (tp==FLOAT || tp==DOUBLE) !=
		(tq==FLOAT || tq==DOUBLE) );
}

prtype(n) NODE *n;
{
	switch (n->type)
		{
		case DOUBLE:
			printf("d");
			return;

		case FLOAT:
			printf("f");
			return;

		case LONG:
		case ULONG:
		case INT:
		case UNSIGNED:
			printf("l");
			return;

		case SHORT:
		case USHORT:
			printf("w");
			return;

		case CHAR:
		case UCHAR:
			printf("b");
			return;

		default:
			if ( !ISPTR( n->type ) ) cerror("zzzcode- bad type");
			else {
				printf("l");
				return;
				}
		}
}

zzzcode( p, c ) register NODE *p; {
	register m;
	CONSZ val;
	switch( c ){

	case 'N':  /* logical ops, turned into 0-1 */
		/* use register given by register 1 */
		cbgen( 0, m=getlab(), 'I' );
		deflab( p->label );
		printf( "	clrl	%s\n", rnames[getlr( p, '1' )->rval] );
		deflab( m );
		return;

	case 'I':
	case 'P':
		cbgen( p->op, p->label, c );
		return;

	case 'A':
		{
		register NODE *l, *r;

		if (xdebug) eprint(p, 0, &val, &val);
		r = getlr(p, 'R');
		if (optype(p->op) == LTYPE || p->op == UNARY MUL)
			{
			l = resc;
			l->type = (r->type==FLOAT || r->type==DOUBLE ? DOUBLE : INT);
			}
		else
			l = getlr(p, 'L');
		if (r->op == ICON  && r->name[0] == '\0')
			{
			if (r->lval == 0)
				{
				printf("clr");
				prtype(l);
				printf("	");
				adrput(l);
				return;
				}
			if (r->lval < 0 && r->lval >= -63)
				{
				printf("mneg");
				prtype(l);
				r->lval = -r->lval;
				goto ops;
				}
			r->type = (r->lval < 0 ?
					(r->lval >= -128 ? CHAR
					: (r->lval >= -32768 ? SHORT
					: INT )) : r->type);
			r->type = (r->lval >= 0 ?
					(r->lval <= 63 ? INT
					: ( r->lval <= 127 ? CHAR
					: (r->lval <= 255 ? UCHAR
					: (r->lval <= 32767 ? SHORT
					: (r->lval <= 65535 ? USHORT
					: INT ))))) : r->type );
			}
		if (l->op == REG && l->type != FLOAT && l->type != DOUBLE)
			l->type = INT;
		if (!mixtypes(l,r))
			{
			if (tlen(l) == tlen(r))
				{
				printf("mov");
				prtype(l);
				goto ops;
				}
			else if (tlen(l) > tlen(r) && ISUNSIGNED(r->type))
				{
				printf("movz");
				}
			else
				{
				printf("cvt");
				}
			}
		else
			{
			printf("cvt");
			}
		prtype(r);
		prtype(l);
	ops:
		printf("	");
		adrput(r);
		printf(",");
		adrput(l);
		return;
		}

	case 'C':	/* num words pushed on arg stack */
		{
		extern int gc_numbytes;
		extern int xdebug;

		if (xdebug) printf("->%d<-",gc_numbytes);

		printf("$%d", gc_numbytes/(SZLONG/SZCHAR) );
		return;
		}

	case 'D':	/* INCR and DECR */
		zzzcode(p->left, 'A');
		printf("\n	");

	case 'E':	/* INCR and DECR, FOREFF */
		if (p->right->lval == 1)
			{
			printf("%s", (p->op == INCR ? "inc" : "dec") );
			prtype(p->left);
			printf("	");
			adrput(p->left);
			return;
			}
		printf("%s", (p->op == INCR ? "add" : "sub") );
		prtype(p->left);
		printf("2	");
		adrput(p->right);
		printf(",");
		adrput(p->left);
		return;

	case 'F':	/* register type of right operand */
		{
		register NODE *n;
		extern int xdebug;
		register int ty;

		n = getlr( p, 'R' );
		ty = n->type;

		if (xdebug) printf("->%d<-", ty);

		if ( ty==DOUBLE) printf("d");
		else if ( ty==FLOAT ) printf("f");
		else printf("l");
		return;
		}

	case 'L':	/* type of left operand */
	case 'R':	/* type of right operand */
		{
		register NODE *n;
		extern int xdebug;

		n = getlr ( p, c);
		if (xdebug) printf("->%d<-", n->type);

		prtype(n);
		return;
		}

	case 'Z':	/* complement mask for bit instr */
		printf("$%Ld", ~p->right->lval);
		return;

	case 'U':	/* 32 - n, for unsigned right shifts */
		printf("$%d", 32 - p->right->lval );
		return;

	case 'T':	/* rounded structure length for arguments */
		{
		int size;

		size = p->stsize;
		SETOFF( size, 4);
		printf("$%d", size);
		return;
		}

	case 'S':  /* structure assignment */
		{
			register NODE *l, *r;
			register size;

			if( p->op == STASG ){
				l = p->left;
				r = p->right;

				}
			else if( p->op == STARG ){  /* store an arg into a temporary */
				l = getlr( p, '3' );
				r = p->left;
				}
			else cerror( "STASG bad" );

			if( r->op == ICON ) r->op = NAME;
			else if( r->op == REG ) r->op = OREG;
			else if( r->op != OREG ) cerror( "STASG-r" );

			size = p->stsize;

			if( size <= 0 || size > 65535 )
				cerror("structure size <0=0 or >65535");

			switch(size) {
				case 1:
					printf("	movb	");
					break;
				case 2:
					printf("	movw	");
					break;
				case 4:
					printf("	movl	");
					break;
				case 8:
					printf("	movq	");
					break;
				default:
					printf("	movc3	$%d,", size);
					break;
			}
			adrput(r);
			printf(",");
			adrput(l);
			printf("\n");

			if( r->op == NAME ) r->op = ICON;
			else if( r->op == OREG ) r->op = REG;

			}
		break;

	default:
		cerror( "illegal zzzcode" );
		}
	}

rmove( rt, rs, t ){
	printf( "	%s	%s,%s\n",
		(t==FLOAT ? "movf" : (t==DOUBLE ? "movd" : "movl")),
		rnames[rs], rnames[rt] );
	}

struct respref
respref[] = {
	INTAREG|INTBREG,	INTAREG|INTBREG,
	INAREG|INBREG,	INAREG|INBREG|SOREG|STARREG|STARNM|SNAME|SCON,
	INTEMP,	INTEMP,
	FORARG,	FORARG,
	INTEMP,	INTAREG|INAREG|INTBREG|INBREG|SOREG|STARREG|STARNM,
	0,	0 };

setregs(){ /* set up temporary registers */
	fregs = 6;	/* tbl- 6 free regs on VAX (0-5) */
	;
	}

szty(t){ /* size, in registers, needed to hold thing of type t */
	return( (t==DOUBLE||t==FLOAT) ? 2 : 1 );
	}

rewfld( p ) NODE *p; {
	return(1);
	}

callreg(p) NODE *p; {
	return( R0 );
	}

base( p ) register NODE *p; {
	register int o = p->op;

	if( (o==ICON && p->name[0] != '\0')) return( 100 ); /* ie no base reg */
	if( o==REG ) return( p->rval );
    if( (o==PLUS || o==MINUS) && p->left->op == REG && p->right->op==ICON)
		return( p->left->rval );
    if( o==OREG && !R2TEST(p->rval) && (p->type==INT || p->type==UNSIGNED || ISPTR(p->type)) )
		return( p->rval + 0200*1 );
	if( o==INCR && p->left->op==REG ) return( p->left->rval + 0200*2 );
	if( o==ASG MINUS && p->left->op==REG) return( p->left->rval + 0200*4 );
	if( o==UNARY MUL && p->left->op==INCR && p->left->left->op==REG
	  && (p->type==INT || p->type==UNSIGNED || ISPTR(p->type)) )
		return( p->left->left->rval + 0200*(1+2) );
	return( -1 );
	}

offset( p, tyl ) register NODE *p; int tyl; {

	if( tyl==1 && p->op==REG && (p->type==INT || p->type==UNSIGNED) ) return( p->rval );
	if( (p->op==LS && p->left->op==REG && (p->left->type==INT || p->left->type==UNSIGNED) &&
	      (p->right->op==ICON && p->right->name[0]=='\0')
	      && (1<<p->right->lval)==tyl))
		return( p->left->rval );
	return( -1 );
	}

makeor2( p, q, b, o) register NODE *p, *q; register int b, o; {
	register NODE *t;
	register int i;
	NODE *f;

	p->op = OREG;
	f = p->left; 	/* have to free this subtree later */

	/* init base */
	switch (q->op) {
		case ICON:
		case REG:
		case OREG:
			t = q;
			break;

		case MINUS:
			q->right->lval = -q->right->lval;
		case PLUS:
			t = q->right;
			break;

		case INCR:
		case ASG MINUS:
			t = q->left;
			break;

		case UNARY MUL:
			t = q->left->left;
			break;

		default:
			cerror("illegal makeor2");
	}

	p->lval = t->lval;
	for(i=0; i<NCHNAM; ++i)
		p->name[i] = t->name[i];

	/* init offset */
	p->rval = R2PACK( (b & 0177), o, (b>>7) );

	tfree(f);
	return;
	}

canaddr( p ) NODE *p; {
	register int o = p->op;

	if( o==NAME || o==REG || o==ICON || o==OREG || (o==UNARY MUL && shumul(p->left)) ) return(1);
	return(0);
	}

shltype( o, p ) register NODE *p; {
	return( o== REG || o == NAME || o == ICON || o == OREG || ( o==UNARY MUL && shumul(p->left)) );
	}

flshape( p ) register NODE *p; {
	return( p->op == REG || p->op == NAME || p->op == ICON ||
		(p->op == OREG && (!R2TEST(p->rval) || tlen(p) == 1)) );
	}

shtemp( p ) register NODE *p; {
	if( p->op == STARG ) p = p->left;
	return( p->op==NAME || p->op ==ICON || p->op == OREG || (p->op==UNARY MUL && shumul(p->left)) );
	}

shumul( p ) register NODE *p; {
	register o;
	extern int xdebug;

	if (xdebug) {
		 printf("\nshumul:op=%d,lop=%d,rop=%d", p->op, p->left->op, p->right->op);
		printf(" prname=%s,plty=%d, prlval=%D\n", p->right->name, p->left->type, p->right->lval);
		}


	o = p->op;
	if( o == NAME || (o == OREG && !R2TEST(p->rval)) || o == ICON ) return( STARNM );

	if( ( o == INCR || o == ASG MINUS ) &&
	    ( p->left->op == REG && p->right->op == ICON ) &&
	    p->right->name[0] == '\0' )
		{
		switch (p->left->type)
			{
			case CHAR|PTR:
			case UCHAR|PTR:
				o = 1;
				break;

			case SHORT|PTR:
			case USHORT|PTR:
				o = 2;
				break;

			case INT|PTR:
			case UNSIGNED|PTR:
			case LONG|PTR:
			case ULONG|PTR:
			case FLOAT|PTR:
				o = 4;
				break;

			case DOUBLE|PTR:
				o = 8;
				break;

			default:
				if ( ISPTR(p->left->type) ) {
					o = 4;
					break;
					}
				else return(0);
			}
		return( p->right->lval == o ? STARREG : 0);
		}

	return( 0 );
	}

adrcon( val ) CONSZ val; {
	printf( "$" );
	printf( CONFMT, val );
	}

conput( p ) register NODE *p; {
	switch( p->op ){

	case ICON:
		acon( p );
		return;

	case REG:
		printf( "%s", rnames[p->rval] );
		return;

	default:
		cerror( "illegal conput" );
		}
	}

insput( p ) register NODE *p; {
	cerror( "insput" );
	}

upput( p ) register NODE *p; {
	cerror( "upput" );
	}

adrput( p ) register NODE *p; {
	register int r;
	/* output an address, with offsets, from p */

	if( p->op == FLD ){
		p = p->left;
		}
	switch( p->op ){

	case NAME:
		acon( p );
		return;

	case ICON:
		/* addressable value of the constant */
		printf( "$" );
		acon( p );
		return;

	case REG:
		printf( "%s", rnames[p->rval] );
		return;

	case OREG:
		r = p->rval;
		if( R2TEST(r) ){ /* double indexing */
			register int flags;

			flags = R2UPK3(r);
			if( flags & 1 ) printf("*");
			if( flags & 4 ) printf("-");
			if( p->lval != 0 || p->name[0] != '\0' ) acon(p);
			if( R2UPK1(r) != 100) printf( "(%s)", rnames[R2UPK1(r)] );
			if( flags & 2 ) printf("+");
			printf( "[%s]", rnames[R2UPK2(r)] );
			return;
			}
		if( r == AP ){  /* in the argument region */
			if( p->lval <= 0 || p->name[0] != '\0' ) werror( "bad arg temp" );
			printf( CONFMT, p->lval );
			printf( "(ap)" );
			return;
			}
		if( p->lval != 0 || p->name[0] != '\0') acon( p );
		printf( "(%s)", rnames[p->rval] );
		return;

	case UNARY MUL:
		/* STARNM or STARREG found */
		if( tshape(p, STARNM) ) {
			printf( "*" );
			adrput( p->left);
			}
		else {	/* STARREG - really auto inc or dec */
			register NODE *q;

/* tbl
			p = p->left;
			p->left->op = OREG;
			if( p->op == INCR ) {
				adrput( p->left );
				printf( "+" );
				}
			else {
				printf( "-" );
				adrput( p->left );
				}
   tbl */
			printf("%c(%s)%c", (p->left->op==INCR ? '\0' : '-'),
				rnames[p->left->left->rval], 
				(p->left->op==INCR ? '+' : '\0') );
			p->op = OREG;
			p->rval = p->left->left->rval;
			q = p->left;
			p->lval = (p->left->op == INCR ? -p->left->right->lval : 0);
			p->name[0] = '\0';
			tfree(q);
		}
		return;

	default:
		cerror( "illegal address" );
		return;

		}

	}

acon( p ) register NODE *p; { /* print out a constant */

	if( p->name[0] == '\0' ){
		printf( CONFMT, p->lval);
		}
	else if( p->lval == 0 ) {
		printf( "%.8s", p->name );
		}
	else {
		printf( "%.8s+", p->name );
		printf( CONFMT, p->lval );
		}
	}

/*
aacon( p ) register NODE *p; { /* print out a constant */
/*

	if( p->name[0] == '\0' ){
		printf( CONFMT, p->lval);
		return( 0 );
		}
	else if( p->lval == 0 ) {
		printf( "$%.8s", p->name );
		return( 1 );
		}
	else {
		printf( "$(" );
		printf( CONFMT, p->lval );
		printf( "+" );
		printf( "%.8s)", p->name );
		return(1);
		}
	}
 */

genscall( p, cookie ) register NODE *p; {
	/* structure valued call */
	return( gencall( p, cookie ) );
	}

/* tbl */
int gc_numbytes;
/* tbl */

gencall( p, cookie ) register NODE *p; {
	/* generate the call given by p */
	register NODE *p1, *ptemp;
	register temp, temp1;
	register m;

	if( p->right ) temp = argsize( p->right );
	else temp = 0;

	if( p->op == STCALL || p->op == UNARY STCALL ){
		/* set aside room for structure return */

		if( p->stsize > temp ) temp1 = p->stsize;
		else temp1 = temp;
		}

	if( temp > maxargs ) maxargs = temp;
	SETOFF(temp1,4);

	if( p->right ){ /* make temp node, put offset in, and generate args */
		ptemp = talloc();
		ptemp->op = OREG;
		ptemp->lval = -1;
		ptemp->rval = SP;
		ptemp->name[0] = '\0';
		ptemp->rall = NOPREF;
		ptemp->su = 0;
		genargs( p->right, ptemp );
		ptemp->op = FREE;
		}

	p1 = p->left;
	if( p1->op != ICON ){
		if( p1->op != REG ){
			if( p1->op != OREG || R2TEST(p1->rval) ){
				if( p1->op != NAME ){
					order( p1, INAREG );
					}
				}
			}
		}

/*
	if( p1->op == REG && p->rval == R5 ){
		cerror( "call register overwrite" );
		}
 */
/* tbl
	setup gc_numbytes so reference to ZC works */

	gc_numbytes = temp;
/* tbl */

	p->op = UNARY CALL;
	m = match( p, INTAREG|INTBREG );
/* tbl
	switch( temp ) {
	case 0:
		break;
	case 2:
		printf( "	tst	(sp)+\n" );
		break;
	case 4:
		printf( "	cmp	(sp)+,(sp)+\n" );
		break;
	default:
		printf( "	add	$%d,sp\n", temp);
		}
   tbl */
	return(m != MDONE);
	}

/* tbl */
char *
ccbranches[] = {
	"	jeql	L%d\n",
	"	jneq	L%d\n",
	"	jleq	L%d\n",
	"	jlss	L%d\n",
	"	jgeq	L%d\n",
	"	jgtr	L%d\n",
	"	jlequ	L%d\n",
	"	jlssu	L%d\n",
	"	jgequ	L%d\n",
	"	jgtru	L%d\n",
	};
/* tbl */

cbgen( o, lab, mode ) { /*   printf conditional and unconditional branches */

/* tbl */
	if( o == 0 ) printf( "	jbr     L%d\n", lab );
/* tbl */
	else {
		if( o > UGT ) cerror( "bad conditional branch: %s", opst[o] );
		printf( ccbranches[o-EQ], lab );
		}
	}

nextcook( p, cookie ) NODE *p; {
	/* we have failed to match p with cookie; try another */
	if( cookie == FORREW ) return( 0 );  /* hopeless! */
	if( !(cookie&(INTAREG|INTBREG)) ) return( INTAREG|INTBREG );
	if( !(cookie&INTEMP) && asgop(p->op) ) return( INTEMP|INAREG|INTAREG|INTBREG|INBREG );
	return( FORREW );
	}

lastchance( p, cook ) NODE *p; {
	/* forget it! */
	return(0);
	}

optim2( p ) register NODE *p; {
	/* do local tree transformations and optimizations */

	register NODE *r;

	switch( p->op ) {

	case AND:
		/* commute L and R to eliminate compliments and constants */
		if( (p->left->op==ICON&&p->left->name[0]==0) || p->left->op==COMPL ) {
			r = p->left;
			p->left = p->right;
			p->right = r;
			}
	case ASG AND:
		/* change meaning of AND to ~R&L - bic on pdp11 */
		r = p->right;
		if( r->op==ICON && r->name[0]==0 ) { /* compliment constant */
			r->lval = ~r->lval;
			}
		else if( r->op==COMPL ) { /* ~~A => A */
			r->op = FREE;
			p->right = r->left;
			}
		else { /* insert complement node */
			p->right = talloc();
			p->right->op = COMPL;
			p->right->rall = NOPREF;
			p->right->type = r->type;
			p->right->left = r;
			p->right->right = NULL;
			}
		break;

		}
	}


# ifndef ONEPASS
main( argc, argv ) char *argv[]; {
	return( mainp2( argc, argv ) );
	}
# endif
