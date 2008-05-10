spec:	  dcl
	| common
	| external
	| intrinsic
	| equivalence
	| data
	| implicit
	| SSAVE
		{ saveall = YES; }
	| SSAVE savelist
	| SFORMAT
		{ fmtstmt(thislabel); setfmt(thislabel); }
	| SPARAM in_dcl SLPAR paramlist SRPAR
	;

dcl:	  type name in_dcl dims lengspec
		{ settype($2, $1, $5);
		  if(ndim>0) setbound($2,ndim,dims);
		}
	| dcl SCOMMA name dims lengspec
		{ settype($3, $1, $5);
		  if(ndim>0) setbound($3,ndim,dims);
		}
	;

type:	  typespec lengspec
		{ varleng = $2; }
	;

typespec:  typename
		{ varleng = ($1<0 || $1==TYLONG ? 0 : typesize[$1]); }
	;

typename:    SINTEGER	{ $$ = TYLONG; }
	| SREAL		{ $$ = TYREAL; }
	| SCOMPLEX	{ $$ = TYCOMPLEX; }
	| SDOUBLE	{ $$ = TYDREAL; }
	| SDCOMPLEX	{ $$ = TYDCOMPLEX; }
	| SLOGICAL	{ $$ = TYLOGICAL; }
	| SCHARACTER	{ $$ = TYCHAR; }
	| SUNDEFINED	{ $$ = TYUNKNOWN; }
	| SDIMENSION	{ $$ = TYUNKNOWN; }
	| SAUTOMATIC	{ $$ = - STGAUTO; }
	| SSTATIC	{ $$ = - STGBSS; }
	;

lengspec:
		{ $$ = varleng; }
	| SSTAR expr
		{
		  if( ! ISICON($2) )
			{
			$$ = 0;
			dclerr("length must be an integer constant", 0);
			}
		  else $$ = $2->b_const.fconst.ci;
		}
	| SSTAR SLPAR SSTAR SRPAR
		{ $$ = 0; }
	;

common:	  SCOMMON in_dcl var
		{ incomm( $$ = comblock(0, 0) , $3 ); }
	| SCOMMON in_dcl comblock var
		{ $$ = $3;  incomm($3, $4); }
	| common opt_comma comblock opt_comma var
		{ $$ = $3;  incomm($3, $5); }
	| common SCOMMA var
		{ incomm($1, $3); }
	;

comblock:  SCONCAT
		{ $$ = comblock(0, 0); }
	| SSLASH SFNAME SSLASH
		{ $$ = comblock(toklen, token); }
	;

external: SEXTERNAL in_dcl name
		{ setext($3); }
	| external SCOMMA name
		{ setext($3); }
	;

intrinsic:  SINTRINSIC in_dcl name
		{ setintr($3); }
	| intrinsic SCOMMA name
		{ setintr($3); }
	;

equivalence:  SEQUIV in_dcl equivset
	| equivalence SCOMMA equivset
	;

equivset:  SLPAR equivlist SRPAR
		{
		struct equivblock *p;
		if(nequiv >= MAXEQUIV)
			fatal("too many equivalences");
		p  =  & eqvclass[nequiv++];
		p->eqvinit = 0;
		p->eqvbottom = 0;
		p->eqvtop = 0;
		p->equivs = $2;
		}
	;

equivlist:  lhs
		{ $$ = ALLOC(eqvchain); $$->eqvchain.eqvitem = $1; }
	| equivlist SCOMMA lhs
		{ $$ = ALLOC(eqvchain); $$->eqvchain.eqvitem = $3; $$->eqvchain.nextp = $1; }
	;

data:	  SDATA in_data datalist
	| data opt_comma datalist
	;

in_data:
		{ if(parstate == OUTSIDE)
			{
			newproc();
			startproc(0, CLMAIN);
			}
		  if(parstate < INDATA)
			{
			enddcl();
			parstate = INDATA;
			}
		}
	;

datalist:  datavarlist SSLASH vallist SSLASH
		{ ftnint junk;
		  if(nextdata(&junk,&junk) != NULL)
			{
			err("too few initializers");
			curdtp = NULL;
			}
		  frdata($1);
		  frrpl();
		}
	;

vallist:  { toomanyinit = NO; }  val
	| vallist SCOMMA val
	;

val:	  value
		{ dataval(NULL, $1); }
	| simple SSTAR value
		{ dataval($1, $3); }
	;

value:	  simple
	| addop simple
		{ if( $1==OPMINUS && ISCONST($2) )
			consnegop($2);
		  $$ = $2;
		}
	| complex_const
	| bit_const
	;

savelist: saveitem
	| savelist SCOMMA saveitem
	;

saveitem: name
		{ int k;
		  $1->b_name.vsave = 1;
		  k = $1->vstg;
		if( ! ONEOF(k, M(STGUNKNOWN)|M(STGBSS)|M(STGINIT)) )
			dclerr("can only save static variables", $1);
		}
	| comblock
		{ $1->extsave = 1; }
	;

paramlist:  paramitem
	| paramlist SCOMMA paramitem
	;

paramitem:  name SEQUALS expr
		{ if($1->vclass == CLUNKNOWN)
			{ $1->vclass = CLPARAM;
			  $1->b_param.paramval = $3;
			}
		  else dclerr("cannot make %s parameter", $1);
		}
	;

var:	  name dims
		{ if(ndim>0) setbound($1, ndim, dims); }
	;

datavar:	  lhs
		{ struct bigblock *np;
		  vardcl(np = $1->b_prim.namep);
		  if(np->vstg == STGBSS)
			np->vstg = STGINIT;
		  else if(np->vstg == STGCOMMON)
			extsymtab[np->b_name.vardesc.varno].extinit = YES;
		  else if(np->vstg==STGEQUIV)
			eqvclass[np->b_name.vardesc.varno].eqvinit = YES;
		  else if(np->vstg != STGINIT)
			dclerr("inconsistent storage classes", np);
		  $$ = mkchain($1, 0);
		}
	| SLPAR datavarlist SCOMMA dospec SRPAR
		{ chainp p; struct bigblock *q;
		q = BALLO();
		q->tag = TIMPLDO;
		q->b_impldo.varnp = $4->chain.datap;
		p = $4->chain.nextp;
		if(p)  { q->b_impldo.implb = p->chain.datap; p = p->chain.nextp; }
		if(p)  { q->b_impldo.impub = p->chain.datap; p = p->chain.nextp; }
		if(p)  { q->b_impldo.impstep = p->chain.datap; p = p->chain.nextp; }
		frchain( & ($4) );
		$$ = mkchain(q, 0);
		q->b_impldo.datalist = hookup($2, $$);
		}
	;

datavarlist: datavar
		{ curdtp = $1; curdtelt = 0; }
	| datavarlist SCOMMA datavar
		{ $$ = hookup($1, $3); }
	;

dims:
		{ ndim = 0; }
	| SLPAR dimlist SRPAR
	;

dimlist:   { ndim = 0; }   dim
	| dimlist SCOMMA dim
	;

dim:	  ubound
		{ dims[ndim].lb = 0;
		  dims[ndim].ub = $1;
		  ++ndim;
		}
	| expr SCOLON ubound
		{ dims[ndim].lb = $1;
		  dims[ndim].ub = $3;
		  ++ndim;
		}
	;

ubound:	  SSTAR
		{ $$ = 0; }
	| expr
	;

labellist: label
		{ nstars = 1; labarray[0] = $1; }
	| labellist SCOMMA label
		{ labarray[nstars++] = $3; }
	;

label:	  labelval
		{ if($1->labinacc)
			warn1("illegal branch to inner block, statement %s",
				convic( (ftnint) ($1->stateno) ));
		  else if($1->labdefined == NO)
			$1->blklevel = blklevel;
		  $1->labused = YES;
		}
	;

labelval:   SICON
		{ $$ = mklabel( convci(toklen, token) ); }
	;

implicit:  SIMPLICIT in_dcl implist
	| implicit SCOMMA implist
	;

implist:  imptype SLPAR letgroups SRPAR
	;

imptype:   { needkwd = 1; } type
		{ vartype = $2; }
	;

letgroups: letgroup
	| letgroups SCOMMA letgroup
	;

letgroup:  letter
		{ setimpl(vartype, varleng, $1, $1); }
	| letter SMINUS letter
		{ setimpl(vartype, varleng, $1, $3); }
	;

letter:  SFNAME
		{ if(toklen!=1 || token[0]<'a' || token[0]>'z')
			{
			dclerr("implicit item must be single letter", 0);
			$$ = 0;
			}
		  else $$ = token[0];
		}
	;

in_dcl:
		{ switch(parstate)	
			{
			case OUTSIDE:	newproc();
					startproc(0, CLMAIN);
			case INSIDE:	parstate = INDCL;
			case INDCL:	break;

			default:
				dclerr("declaration among executables", 0);
			}
		}
	;
