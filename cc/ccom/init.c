



# include "pass1.h"


/*
 * Struct used in array initialisation.
 *
 * Initialization needs some extra explanation:
 * If it's a simple assignment, only init_val() is called.
 * If the assignment is protected by '{', then init_begin(),
 * init_elem() and init_end() are called in order.  init_elem()
 * may be called multiple times for many elements.
 */
static struct instk {
	struct	instk *in_prev; /* linked list */
	int	in_sz;		/* size of array element */
	struct	symtab **in_xp;	 /* member in structure initializations */
	int	in_n;		/* number of initializations seen */
	struct	suedef *in_sue;
	union	dimfun *in_df;	/* dimoff/protos */
	TWORD	in_t;		/* type */
	TWORD	in_q;		/* qualifier */
	struct	symtab *in_sym; /* stab index */
	int	in_fl;	/* flag which says if this level is controlled by {} */
	OFFSZ	in_off;		/* offset of the beginning of this level */
} *pstk;

int ibseen;	/* the number of } constructions which have been filled */
int ilocctr;	/* location counter for current initialization */
int ilabel;	/* label for automatic initialization */

#define ISSOU(ty) (ty == STRTY || ty == UNIONTY)

static int howinit;	/* store in read-only or read-write segment.
#define	ROINIT	0	/* Data is read-only (pseudo) */
#define	RWINIT	1	/* Data is read-write */
#define	DOCOPY	2	/* must copy (initialized on stack) */

static void instk(struct symtab *p, TWORD t, TWORD q, union dimfun *d,
    struct suedef *, OFFSZ off);
void vfdalign(int n);
void inforce(OFFSZ n);
void gotscal(void);		

/*
 * beginning of initialization; allocate space to store initialized data.
 * remember storage class for writeout in endinit().
 */
void
beginit(struct symtab *p, int class)
{
#ifdef PCC_DEBUG
	if (idebug >= 3)
		printf("beginit(), symtab = %p\n", p);
#endif

	if (class == EXTERN)
		uerror("cannot initialize external variables");

	howinit = ROINIT;
	if (class == EXTDEF
	
	iclass = p->sclass;
	if (class == EXTERN || class == FORTRAN)
		iclass = EXTERN;
	switch (iclass) {

	case UNAME:
	case EXTERN:
		return;
	case AUTO:
		if (ISSOU(p->stype) || ISARY(p->stype)) {
			/*
			 * For objects that must be copied onto
			 * the stack to init variables, store
			 * the data in the same way as static data.
			 */
			ilocctr = ISARY(p->stype)?ADATA:DATA;
			send_passt(IP_LOCCTR, ilocctr);
			defalign(talign(p->stype, p->ssue));
			send_passt(IP_DEFLAB, ilabel = getlab());
			iclass = STATIC;
		}
		break;

	case REGISTER:
		break;

	case EXTDEF:
	case STATIC:
		ilocctr = ISARY(p->stype)?ADATA:DATA;
		send_passt(IP_LOCCTR, ilocctr);
		defalign(talign(p->stype, p->ssue));
		defnam(p);
	}

	inoff = 0;
	ibseen = 0;
	pstk = 0;

	instk(p, p->stype, p->squal, p->sdf, p->ssue, inoff);

}



/*
 * make a new entry on the parameter stack to initialize p
 */
void
instk(struct symtab *p, TWORD t, TWORD q,
    union dimfun *d, struct suedef *sue, OFFSZ off)
{   
	struct instk *sp;

	for (;;) {
#ifdef PCC_DEBUG
		if (idebug)
			printf("instk((%p, %x, %x, %p,%p, %lld)\n",
			    p, t, q, d, sue, (long long)off);
#endif

		/* save information on the stack */
		sp = tmpalloc(sizeof(struct instk));
		sp->in_prev = pstk;
		pstk = sp;

		pstk->in_fl = 0;	/* { flag */
		pstk->in_sym = p;
		pstk->in_t = t;
		pstk->in_q = q;
		pstk->in_df = d;
		pstk->in_sue = sue;
		pstk->in_n = 0;	 /* number seen */
		pstk->in_xp = (t == STRTY || t == UNIONTY) ? sue->suelem : NULL;
		pstk->in_off = off;/* offset at the beginning of this element */

		/* if t is an array, DECREF(t) can't be a field */
		/* in_sz has size of array elements, and -size for fields */
		if (ISARY(t)) {
			pstk->in_sz = tsize(DECREF(t), d+1, sue);
		} else if (p->sclass & FIELD){
			pstk->in_sz = - (p->sclass & FLDSIZ);
		} else {
			pstk->in_sz = 0;
		}

		if (iclass == REGISTER &&
		    (t == STRTY || t == UNIONTY || ISARY(t)))
				uerror("erroneous register initialization");

		/* now, if this is not a scalar, put on another element */

		if (ISARY(t)) {
			t = DECREF(t);
			q = DECREF(q);
			++d;
		} else if (t == STRTY || t == UNIONTY) {
			if (pstk->in_sue == 0) {
				uerror("can't initialize undefined %s",
				    t == STRTY ? "structure" : "union");
				iclass = -1;
				return;
			}
			p = *pstk->in_xp;
			if (((p->sclass != MOS && t == STRTY) ||
			    (p->sclass != MOU && t == UNIONTY)) &&
			    !(p->sclass&FIELD))
				cerror("insane %s member list",
				    t == STRTY ? "structure" : "union");
			t = p->stype;
			q = p->squal;
			d = p->sdf;
			sue = p->ssue;
			off += p->soffset;
		} else
			return;
	}
}

/*
 * take care of generating a value for the initializer p
 * inoff has the current offset (last bit written)
 * in the current word being generated
 */
void
doinit(NODE *p)
{
	union dimfun *d;
	NODE *u;
	struct suedef *sue;
	int sz;
	TWORD t;
	int o;

	/* note: size of an individual initializer is assumed to fit into an int
 */

	if( iclass < 0 ) return;
	if( iclass == EXTERN || iclass == UNAME ){
		uerror( "cannot initialize extern or union" );
		iclass = -1;
		return;
		}

	if ((iclass == AUTO || iclass == REGISTER) && !ilabel) {
		/*
		 * do the initialization and get out, without regard
		 * for filing out the variable with zeros, etc.
		 */
		bccode(); /* XXX ??? */
		send_passt(IP_NEWBLK, regvar, autooff);/* Wrong place */
		spname = pstk->in_sym;

#if 0
		/* Allow for array init */
		if (ISARY(spname->stype)) {
			if (spname->stype != p->n_type || p->n_type != ARY+CHAR)
				uerror("illegal types in assignment");
			/*
			 * Check and construct the assign tree here.
			 * Don't wanna go through buildtree(), too messy.
			 */
			if (spname->sdf->ddim == 0) {
				/* Get size from string */
				spname->sdf->ddim = p->n_df->ddim;
				/* Adjust stack accordingly */
				spname->soffset = NOOFFSET;
				oalloc(spname, &autooff);
				if (autooff > maxautooff)
					maxautooff = autooff;
			} else if (spname->sdf->ddim < p->n_df->ddim-1)
				werror("excess element in assignment");
			p = buildtree(ADDROF, p, NIL);
			u = buildtree(NAME, NIL, NIL); /* Get variable node */
			p = block(STASG, u, p, u->n_type, u->n_df, u->n_sue);
			p = block(UMUL, p, NIL, u->n_type, u->n_df, u->n_sue);
		} else {
			u = buildtree(NAME, NIL, NIL); /* Get variable node */
			p = buildtree( ASSIGN, u, p );
		}
#endif
		p = buildtree( ASSIGN, buildtree(NAME, NIL, NIL), p);
		ecomp(p);
		return;
	}

	if (p->n_op == NAME && DEUNSIGN(p->n_type) == ARY+CHAR &&
	    DEUNSIGN(pstk->in_t) == CHAR &&
	    pstk->in_prev != NULL && ISARY(pstk->in_prev->in_t)) {
		/* String is an array of char, store it so */
	}

#if 0
	if( p == NIL ) return;	/* for throwing away strings that have been turn
ed into lists */
#endif

	if( ibseen ){
		uerror( "} expected");
		return;
		}

#ifdef PCC_DEBUG
	if (idebug > 1)
		printf("doinit(%p)\n", p);
#endif
     
	t = pstk->in_t;	 /* type required */
	d = pstk->in_df;
	sue = pstk->in_sue;
	if (pstk->in_sz < 0) {	/* bit field */
		sz = -pstk->in_sz;
	} else {
		sz = tsize( t, d, sue );
	}

	inforce( pstk->in_off );

	u = block(NAME, NIL,NIL, t, d, sue);
	u->n_qual = pstk->in_q;
	p = buildtree( ASSIGN, u, p );
	nfree(p->n_left);
	p->n_left = p->n_right;
	p->n_right = NIL;
	p->n_left = optim( p->n_left );
	o = p->n_left->n_op;
	if( o == ADDROF ){
		NODE *l = p->n_left->n_left;
		nfree(p->n_left);
		p->n_left = l;
	}
	p->n_op = INIT;

	if( sz < SZINT ){ /* special case: bit fields, etc. */
		if (o != ICON)
			uerror( "illegal initialization" );
		else {
			incode( p->n_left, sz );
			tfree(p);
		}
	} else if( o == FCON ){
		fincode(p->n_left, sz );
		tfree(p);
	} else {
		cinit( optim(p), sz );
	}

	gotscal();
}


void
endinit(void)
{
	NODE *p, *u;
	struct suedef *sue;
	TWORD t;
	union dimfun *d, *d1;
	int n;

#ifdef PCC_DEBUG
	if (idebug)
		printf("endinit(), inoff = %lld\n", (long long)inoff);
#endif

	switch( iclass ){

	case EXTERN:
	case AUTO:
		if (ilabel) {
#if 0
			if (spname->sdf->ddim == 0) {
				/* Get size from string */
				spname->sdf->ddim = p->n_df->ddim;
				/* Adjust stack accordingly */
				spname->soffset = NOOFFSET;
				oalloc(spname, &autooff);
				if (autooff > maxautooff)
					maxautooff = autooff;
			} else if (spname->sdf->ddim < p->n_df->ddim-1)
				werror("excess element in assignment");
#endif
			p = buildtree(ADDROF, p, NIL);
			u = buildtree(NAME, NIL, NIL); /* Get variable node */
			p = block(STASG, u, p, u->n_type, u->n_df, u->n_sue);
			p = block(UMUL, p, NIL, u->n_type, u->n_df, u->n_sue);
			ecomp(p);
			ilabel = 0;
		}
		return;

	case REGISTER:
		return;
		}

	while (pstk->in_prev)
		pstk = pstk->in_prev;

	t = pstk->in_t;
	d = pstk->in_df;
	sue = pstk->in_sue;
	n = pstk->in_n;

	if( ISARY(t) ){
		d1 = d;

		vfdalign(pstk->in_sz);	/* fill out part of the last element, if
 needed */
		n = inoff/pstk->in_sz;	/* real number of initializers */
		if (d1->ddim >= n) {
			/* once again, t is an array, so no fields */
			inforce(tsize(t, d, sue));
			n = d1->ddim;
		}
		if (d1->ddim != 0 && d1->ddim != n)
			uerror("too many initializers");
		if (n == 0)
			werror("empty array declaration");
		d->ddim = n;
	}

	else if (t == STRTY || t == UNIONTY) {
		/* clearly not fields either */
		inforce( tsize( t, d, sue ) );
	} else if (n > 1)
		uerror("bad scalar initialization");
	else
	/* this will never be called with a field element... */
		inforce(tsize(t, d, sue));

	vfdalign( AL_INIT );
	inoff = 0;
	iclass = SNULL;
	ilabel = 0;
}

void			
gotscal(void)		
{	
	int t, n;	    
	struct symtab *p;
	OFFSZ temp;
			
	for( ; pstk->in_prev != NULL; ) {
	
		if( pstk->in_fl ) ++ibseen;

		pstk = pstk->in_prev;
 
		t = pstk->in_t;

		if( t == STRTY || t == UNIONTY){
			++pstk->in_xp;
			if ((p = *pstk->in_xp) == NULL)
				continue;
		
			/* otherwise, put next element on the stack */
			instk(p, p->stype, p->squal, p->sdf, p->ssue,
			    p->soffset + pstk->in_off);
			return;
		} else if( ISARY(t) ){
			n = ++pstk->in_n;
			if (n >= pstk->in_df->ddim && pstk->in_prev != NULL)
				continue;

			/* put the new element onto the stack */

			temp = pstk->in_sz;
			instk(pstk->in_sym, DECREF(pstk->in_t), DECREF(pstk->in_q),
			    pstk->in_df+1, pstk->in_sue, pstk->in_off+n*temp);
			return;
		} else if (ISFTN(t))
			cerror("gotscal");

	}
}

/*
 * process an initializer's left brace
 */
void
ilbrace()
{
	int t;
	struct instk *temp;

	temp = pstk;	

	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {

		t = pstk->in_t;
		if (t != UNIONTY && t != STRTY && !ISARY(t))
			continue; /* not an aggregate */
		if (pstk->in_fl) { /* already associated with a { */
			if (pstk->in_n)
				uerror( "illegal {");
			continue;
		}	

		/* we have one ... */
		pstk->in_fl = 1;
		break;
	}	
 
	/* cannot find one */
	/* ignore such right braces */

	pstk = temp;
}

/*
 * called when a '}' is seen
 */
void
irbrace()
{
#ifdef PCC_DEBUG
//	 if (idebug)
//		  printf( "irbrace(): lparam = %p on entry\n", lparam);
#endif

	if (ibseen) {
		--ibseen;
		return;
	}

	for (; pstk->in_prev != NULL; pstk = pstk->in_prev) {
		if(!pstk->in_fl)
			continue;

		/* we have one now */

		pstk->in_fl = 0;  /* cancel { */
		gotscal();  /* take it away... */
		return;
	}

	/* these right braces match ignored left braces: throw out */
}

/*
 * make inoff have the offset the next alignment of n
 */
void
vfdalign(int n)
{
	OFFSZ m;

	m = inoff;
	SETOFF( m, n );
	inforce( m );
}

/*	
 * force inoff to have the value n
 */
void	
inforce(OFFSZ n)
{	
	/* inoff is updated to have the value n */
	OFFSZ wb;
	int rest;
	/* rest is used to do a lot of conversion to ints... */

	if( inoff == n ) return;
	if (inoff > n)
		cerror("initialization alignment error: inoff %lld n %lld",
		    inoff, n);

	wb = inoff;
	SETOFF( wb, SZINT );

	/* wb now has the next higher word boundary */

	if( wb >= n ){ /* in the same word */
		rest = n - inoff;
		if (rest > 0)
			vfdzero( rest );
		return;
		}

	/* otherwise, extend inoff to be word aligned */

	rest = wb - inoff;
	if (rest > 0)
		vfdzero( rest );

	/* now, skip full words until near to n */

	rest = (n-inoff)/SZINT;
	if (rest > 0)
		zecode(rest);

	/* now, the remainder of the last word */

	rest = n-inoff;
	if (rest > 0)
		vfdzero( rest );
	if( inoff != n ) cerror( "inoff error");

	}

