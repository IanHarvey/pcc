



# include "pass1.h"

/*
 * Three machine-dependent routines may be called during initialization:
 * 
 * inval(CONSZ)		- writes an integer constant.
 * zecode(CONSZ)	- sets a CONSZ sized block to zeros.
 * finval(NODE *)	- outputs a floating-point constant.
 *
 * bitfields are merged into an integer in this file.
 */

/*
 * Struct used in array initialisation.
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
int maystr;	/* do not store a string if found */

static int inwd;		/* current bit offsed in word */
static CONSZ word;		/* word being built from fields */
static struct symtab *csym;

#define ISSOU(ty) (ty == STRTY || ty == UNIONTY)

static int howinit;	/* store in read-only or read-write segment */
#define ROINIT	0	/* Data is read-only (pseudo) */
#define RWINIT	1	/* Data is read-write */
#define DOCOPY	2	/* must copy (initialized on stack) */
#define SIMPLE	4	/* simple assignment for automatics */

int oalloc(struct symtab *p, int *poff);

static void instk(struct symtab *p, TWORD t, TWORD q, union dimfun *d,
    struct suedef *, OFFSZ off);
static void vfdalign(int n);
static void inforce(OFFSZ n);
static void gotscal(void);		
static void infld(CONSZ, int sz);
static void kinit(NODE *p, int sz);

/*
 * Initializations of a given object will be stored in a
 * linked list until all initializations are done.  This is
 * needed to get C99 element initializations to work.
 */
struct ilist {
	struct ilist *next;
	int type;
#define TNOD	1
#define TVAL	2
#define TZERO	3	/* val used */
#define TFP	4	/* nod used */
	union {
		NODE *nod;
		CONSZ val;
	} u;
} *ilist, **iend;

#define ADDENT(t, v, f) { \
	struct ilist *i = tmpalloc(sizeof(struct ilist)); \
	i->type = t; \
	i->u.f = v; \
	i->next = NULL; \
	*iend = i; \
	iend = &i->next; \
}

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

	switch (p->sclass) {
	case STATIC:
	case EXTDEF:
		howinit = (p->squal >> TSHIFT) & CON ? ROINIT : RWINIT;
		break;

	case AUTO:
	case REGISTER:
		if (ISARY(p->stype) || ISSOU(p->stype))
			howinit = ROINIT|DOCOPY;
		else
			howinit = RWINIT|SIMPLE;
		break;

	default:
		uerror("illegal initialization");
		return;
	}

	csym = p;
	inoff = 0;
	ibseen = 0;
	pstk = 0;
	ilist = NULL;
	iend = &ilist;

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
		pstk->in_xp = ISSOU(t) ? sue->suelem : NULL;
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

#if 0
		if (iclass == REGISTER &&
		    (ISSOU(t) || ISARY(t)))
				uerror("erroneous register initialization");
#endif

		/* now, if this is not a scalar, put on another element */

		if (ISARY(t)) {
			t = DECREF(t);
			q = DECREF(q);
			++d;
		} else if (ISSOU(t)) {
			if (pstk->in_sue == 0) {
				uerror("can't initialize undefined %s",
				    t == STRTY ? "structure" : "union");
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
		} else {
			if (DEUNSIGN(pstk->in_t) == CHAR &&
			    pstk->in_prev != NULL &&
			    ISARY(pstk->in_prev->in_t))
				maystr = 1;
			else
				maystr = 0;
			return;
		}
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
	struct suedef *sue;
	int sz;
	TWORD t;
	int o;

#ifdef PCC_DEBUG
	if (idebug > 2)
		printf("doinit(%p)\n", p);
#endif

	if (nerrors)
		return;

	if (howinit & SIMPLE) {
		/* generate tree and get out */
		spname = pstk->in_sym;
		ecomp(buildtree(ASSIGN, buildtree(NAME, NIL, NIL), p));
		return;
	}

	if (p->n_op == NAME && DEUNSIGN(p->n_type) == ARY+CHAR &&
	    DEUNSIGN(pstk->in_t) == CHAR &&
	    pstk->in_prev != NULL && ISARY(pstk->in_prev->in_t)) {
		char *c;
		int len, alen;

		/* String is an array of char, store it so */
		ilbrace();  /* simulate { */
		inforce(pstk->in_off);
		c = p->n_sp->sname;
		len = p->n_df->ddim;
		alen = pstk->in_prev->in_df->ddim;
		if (alen && alen < len)
			len = alen;
		while (len-- > 0)
			infld(*c++, SZCHAR), gotscal();
		if (alen && *c)
			werror("too many chars in string");
		irbrace();
		tfree(p);
		return;
	}
	if( ibseen ){
		uerror( "} expected");
		return;
	}
     
	t = pstk->in_t;	 /* type required */
	d = pstk->in_df;
	sue = pstk->in_sue;
	if (pstk->in_sz < 0) {	/* bit field */
		sz = -pstk->in_sz;
	} else {
		sz = tsize( t, d, sue );
	}

	inforce( pstk->in_off );

	/* let buildtree do typechecking */
	p = buildtree( ASSIGN, block(NAME, NIL,NIL, t, d, sue), p );
	nfree(p->n_left);

	/* Make an INIT node of it all */
	p->n_left = optim( p->n_right );
	o = p->n_left->n_op;
	if( o == ADDROF ){
		NODE *l = p->n_left->n_left;
		nfree(p->n_left);
		p->n_left = l;
	}
	p->n_op = INIT;

	/* print it out */
	if( sz < SZINT ){ /* special case: bit fields, etc. */
		if (o != ICON)
			uerror( "illegal initialization" );
		else
			infld( p->n_left->n_lval, sz );
		tfree(p);
	} else if( o == FCON ){
		if (sz == SZFLOAT)
			p->n_left->n_type = FLOAT;
		else if (sz == SZDOUBLE)
			p->n_left->n_type = DOUBLE;
		else if (sz == SZLDOUBLE)
			p->n_left->n_type = LDOUBLE;
		else
			cerror("bad float size");
		ADDENT(TFP, p->n_left, nod);
		inoff += sz;
		nfree(p);
	} else {
		kinit( optim(p), sz );
	}

	gotscal();
}

void
endinit(void)
{
	struct ilist *p;
	struct suedef *sue;
	union dimfun *d;
	TWORD t;
	int on, n, lbl;

#ifdef PCC_DEBUG
	if (idebug)
		printf("endinit(), inoff = %lld\n", (long long)inoff);
#endif

	maystr = 0;

	if (howinit & SIMPLE)
		return;

	while (pstk->in_prev)
		pstk = pstk->in_prev;

	t = pstk->in_t;
	d = pstk->in_df;
	sue = pstk->in_sue;
	n = pstk->in_n;

	if( ISARY(t) ){
		vfdalign(pstk->in_sz);	/* fill out part of the last element, if
 needed */
		n = inoff/pstk->in_sz;	/* real number of initializers */
		if (d->ddim >= n) {
			/* once again, t is an array, so no fields */
			inforce(tsize(t, d, sue));
			n = d->ddim;
		}
		if (d->ddim != 0 && d->ddim != n)
			uerror("too many initializers");
#if 0
		if (n == 0)
			werror("empty array declaration");
#endif
		on = d->ddim;
		d->ddim = n;
	} else if (ISSOU(t)) {
		/* clearly not fields either */
		inforce( tsize( t, d, sue ) );
	} else if (n > 1)
		uerror("bad scalar initialization");
	else
	/* this will never be called with a field element... */
		inforce(tsize(t, d, sue));

	send_passt(IP_LOCCTR, howinit & RWINIT ? DATA : RDATA);
	vfdalign( AL_INIT );
	defalign(talign(csym->stype, csym->ssue));
	lbl = 0;
	if (csym->sclass == EXTDEF || csym->sclass == STATIC)
		defnam(csym);
	else
		send_passt(IP_DEFLAB, lbl = getlab());

	for (p = ilist; p ; p = p->next) {
		/* print it out */
		switch (p->type) {
		case TNOD:
			ecode(p->u.nod);
			break;
		case TVAL:
			inval(p->u.val);
			break;
		case TZERO:
			zecode(p->u.val);
			break;
		case TFP:
			finval(p->u.nod);
			nfree(p->u.nod);
			break;
		}
	}
	if (lbl) {
		struct symtab *sp;
		NODE *r, *l;

		/*
		 * generate copy code for initializations on stack.
		 */
		/* Fixa: kopieringslängd char c[12] = "hhhh"; */
		if (ISARY(t) && on == 0) {
			extern  int autooff, maxautooff;
			
			/*
			 * adjust stack in case of open arrays.
			 */
			csym->soffset = NOOFFSET;
			oalloc(csym, &autooff);
			if (autooff > maxautooff)
				maxautooff = autooff;
		}
		spname = csym;
		sp = isinlining ? permalloc(sizeof *sp) : tmpalloc(sizeof *sp);
		l = buildtree(NAME, NIL, NIL);
		r = block(ICON, NIL, NIL, INCREF(l->n_type), l->n_df, l->n_sue);
		sp->sclass = ILABEL;
		sp->soffset = lbl;
		r->n_sp = sp;
		l = block(STASG, l, r, l->n_type, l->n_df, l->n_sue);
		l = block(UMUL, l, NIL, l->n_type, l->n_df, l->n_sue);
		ecomp(l);
	}
	inoff = 0;
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

		if( ISSOU(t)){
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
		if (!ISSOU(t) && !ISARY(t))
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
	if (rest > 0) {
		ADDENT(TZERO, rest, val);
		inoff += rest * SZINT;
	}

	/* now, the remainder of the last word */

	rest = n-inoff;
	if (rest > 0)
		vfdzero( rest );
	if( inoff != n ) cerror( "inoff error");

	}

/*
 * collect 
 */
void
infld(CONSZ con, int sz)
{  
	inoff += sz;
	if ((sz + inwd) > SZINT)
		cerror("infld: field > int");

#ifdef RTOLBYTES
	word |= ((unsigned)(con<<(SZINT-sz))) >> (SZINT-sz-inwd);
#else
#error fix big endian support
#endif

	inwd += sz;
	if (inoff % SZINT == 0) {
		ADDENT(TVAL, word, val);
		word = inwd = 0;
	}
}

void
kinit(NODE *p, int sz)
{
	NODE *l;

	/*
	 * as a favor (?) to people who want to write
	 *     int i = 9600/134.5;
	 * we will, under the proper circumstances, do
	 * a coercion here.
	 */
	switch (p->n_type) {
	case INT:
	case UNSIGNED:
		l = p->n_left;
		if (l->n_op != SCONV || l->n_left->n_op != FCON)
			break;
		nfree(l);
		l = l->n_left;
		l->n_lval = (long)(l->n_dcon);
		l->n_sp = NULL;
		l->n_op = ICON;
		l->n_type = INT;
		p->n_left = l;
		break;
	}
	/* arrange for the initialization of p into a space of size sz */
	/* the proper alignment has been opbtained */
	/* inoff is updated to have the proper final value */
	ADDENT(TNOD, p, nod);
	word = inwd = 0;
	inoff += sz;
}

/*    
 * define n bits of zeros in a vfd
 */   
void
vfdzero(int n)
{
	inoff += n;
	inwd += n;
	if (inoff%ALINT ==0) {
		ADDENT(TVAL, word, val);
		word = inwd = 0; 
	} 
}

