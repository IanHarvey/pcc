/*	cgram.y	4.22	87/12/09	*/

/*
 * Grammar for the C compiler.
 *
 * This grammar requires the definitions of terminals in the file 'pcctokens'.
 * (YACC doesn't have an 'include' mechanism, unfortunately.)
 */


/* at last count, there were 7 shift/reduce, 1 reduce/reduce conflicts
/* these involved:
	if/else
	recognizing functions in various contexts, including declarations
	error recovery
	*/

%left CM
%right ASSIGN ASOP
%right QUEST COLON
%left OROR
%left ANDAND
%left OR
%left ER
%left AND
%left EQUOP
%left RELOP
%left SHIFTOP
%left PLUS MINUS
%left MUL DIVOP
%right UNOP
%right INCOP SIZEOF
%left LB LP STROP
%{
# include "pass1.h"
%}

	/* define types */
%start ext_def_list

%type <intval> con_e ifelprefix ifprefix whprefix forprefix doprefix switchpart
		enum_head str_head name_lp ulmerdecl
%type <nodep> e .e term attributes oattributes type enum_dcl struct_dcl
		cast_type null_decl funct_idn declarator fdeclarator nfdeclarator
		elist

%token <intval> CLASS NAME STRUCT RELOP CM DIVOP PLUS MINUS SHIFTOP MUL AND
		OR ER ANDAND OROR ASSIGN STROP INCOP UNOP ICON ASOP EQUOP
%token <nodep> TYPE

%%

%{
	static int fake = 0;
	static char fakename[24];
	static int nsizeof = 0;
	static int ansifunc;	/* Current function is ansi declared */
	static int ansiparams;	/* Number of ansi parameters gotten so far */
	static int isproto;	/* Currently reading in a prototype */
%}

ext_def_list:	   ext_def_list external_def
		| { ftnend(); }
		;

external_def:	   data_def { curclass = SNULL;  blevel = 0; }
		|  error { curclass = SNULL;  blevel = 0; }
		;

data_def:	   oattributes init_dcl_list SM {  $1->in.op = FREE; }
		|  oattributes SM {  $1->in.op = FREE; }

		|  oattributes fdeclarator {
			if (isproto)
				uerror("argument missing parameters");
			defid(tymerge($1,$2), curclass==STATIC?STATIC:EXTDEF);
				if (nerrors == 0)
					pfstab(stab[$2->tn.rval].sname);
			proto_chkfun($2, ansifunc);

			/* Remove ELLIPSIS if there */
			if (schain[1] && schain[1]->stype == -1) {
				schain[1]->stype = TNULL;
				schain[1] = schain[1]->snext;
			}
				
		}  function_body {  
			if (blevel)
				cerror("function level error");
			if (reached)
				retstat |= NRETVAL; 
			$1->in.op = FREE;
			ftnend();
			ansifunc = ansiparams = isproto = 0;
		}
		;

function_body:	   arg_dcl_list compoundstmt
		;
arg_dcl_list:	   arg_dcl_list declaration {
			if (ansifunc)
				uerror("K&R parameters in ANSI style function");
		}
		| 	{  blevel = 1; }
		;

stmt_list:	   stmt_list statement
		|  /* empty */ {  bccode(); (void) locctr(PROG); }
		;

r_dcl_stat_list	:  dcl_stat_list attributes SM {
			$2->in.op = FREE; 
			if( nerrors == 0 )
				plcstab(blevel);
		}
		|  dcl_stat_list attributes init_dcl_list SM {
			$2->in.op = FREE; 
			if( nerrors == 0 )
				plcstab(blevel);
		}
		;

dcl_stat_list	:  dcl_stat_list attributes SM {  $2->in.op = FREE; }
		|  dcl_stat_list attributes init_dcl_list SM
			{  $2->in.op = FREE; }
		|  /* empty */
		;
declaration:	   attributes declarator_list  SM
			{ curclass = SNULL;  $1->in.op = FREE; }
		|  attributes SM { curclass = SNULL;  $1->in.op = FREE; }
		|  error  SM {  curclass = SNULL; }
		;

oattributes:	  attributes
		|  /* VOID */ { 
			if (Wimplicit_int)
				werror("type defaults to `int'");
			$$ = mkty(INT,0,INT);  curclass = SNULL;
		}
		;

attributes:	   class type { $$ = $2; }
		|  type class
		|  class { $$ = mkty(INT,0,INT); }
		|  type { curclass = SNULL ; }
		|  type class type {
			$1->in.type = types( $1->in.type, $3->in.type,
			    UNDEF, UNDEF);
			$3->in.op = FREE;
		}
		;

class:		  CLASS {  curclass = $1; }
		;

type:		   TYPE
		|  TYPE TYPE {
			$1->in.type = types($1->in.type, $2->in.type,
			    UNDEF, UNDEF);
			$2->in.op = FREE;
		}
		|  TYPE TYPE TYPE {
			$1->in.type = types($1->in.type, $2->in.type,
			    $3->in.type, UNDEF);
			$2->in.op = $3->in.op = FREE;
		}
		|  TYPE TYPE TYPE TYPE {
			$1->in.type = types($1->in.type, $2->in.type,
			    $3->in.type, $4->in.type);
			$2->in.op = $3->in.op = $4->in.op = FREE;
		}
		|  struct_dcl
		|  enum_dcl
		;

enum_dcl:	   enum_head LC moe_list optcomma RC { $$ = dclstruct($1); }
		|  ENUM NAME {  $$ = rstruct($2,0);  stwart = instruct; }
		;

enum_head:	   ENUM {  $$ = bstruct(-1,0); stwart = SEENAME; }
		|  ENUM NAME {  $$ = bstruct($2,0); stwart = SEENAME; }
		;

moe_list:	   moe
		|  moe_list CM moe
		;

moe:		   NAME {  moedef( $1 ); }
		|  NAME ASSIGN con_e {  strucoff = $3;  moedef( $1 ); }
		;

struct_dcl:	   str_head LC type_dcl_list optsemi RC { $$ = dclstruct($1);  }
		|  STRUCT NAME {  $$ = rstruct($2,$1); }
		;

str_head:	   STRUCT {  $$ = bstruct(-1,$1);  stwart=0; }
		|  STRUCT NAME {  $$ = bstruct($2,$1);  stwart=0;  }
		;

type_dcl_list:	   type_declaration
		|  type_dcl_list SM type_declaration
		;

type_declaration:  type declarator_list {
			curclass = SNULL;
			stwart=0; $1->in.op = FREE;
		}
		|  type {
			if( curclass != MOU ) {
				curclass = SNULL;
			} else {
				sprintf( fakename, "$%dFAKE", fake++ );
				/* No need to hash this, we won't look it up */
				defid(tymerge($1, bdty(NAME,NIL,
				    lookup(savestr(fakename), SMOS ))),
				    curclass );
				werror("structure typed union member must be named");
			}
			stwart = 0;
			$1->in.op = FREE;
		}
		;


declarator_list:   declarator {
			defid(tymerge($<nodep>0,$1), curclass);
			stwart = instruct;
		}
		|  declarator_list  CM { $<nodep>$=$<nodep>0; }  declarator {
			defid( tymerge($<nodep>0,$4), curclass);
			stwart = instruct;
		}
		;

declarator:	   fdeclarator
		|  nfdeclarator
		|  nfdeclarator COLON con_e %prec CM {
			if (!(instruct&INSTRUCT))
				uerror( "field outside of structure" );
			if( $3<0 || $3 >= FIELD ){
				uerror( "illegal field size" );
				$3 = 1;
			}
			defid( tymerge($<nodep>0,$1), FIELD|$3 );
			$$ = NIL;
		}
		|  COLON con_e %prec CM {
			if (!(instruct&INSTRUCT))
				uerror( "field outside of structure" );
			(void)falloc( stab, $2, -1, $<nodep>0 );
					/* alignment or hole */
			$$ = NIL;
		}
		|  error { $$ = NIL; }
		;

		/* int (a)();   is not a function --- sorry! */
nfdeclarator:	   MUL nfdeclarator { $$ = bdty( UNARY MUL, $2, 0 ); }
		|  nfdeclarator LP gurka RP {
			proto_enter($1);
			ansifunc = ansiparams = isproto = 0;
			$$ = bdty( UNARY CALL, $1, 0 );
		}
		|  nfdeclarator LB RB { $$ = bdty( LB, $1, 0 ); }
		|  nfdeclarator LB con_e RB {
			bary:
			if( (int)$3 <= 0 )
				werror( "zero or negative subscript" );
			$$ = bdty( LB, $1, $3 );
		}
		|  NAME { 
			if (ansifunc)
				ftnarg($1);
			$$ = bdty( NAME, NIL, $1 );
		}
		|   LP  nfdeclarator  RP { $$=$2; }
		;

gurka:		   ansi_args { }
		|  /* VOID */ { 
			if (Wstrict_prototypes)
			      werror("function declaration isn't a prototype");
		}
		;

fdeclarator:	   MUL fdeclarator {  $$ = bdty(UNARY MUL, $2, 0); }
		|  fdeclarator  LP RP { 
			if (Wstrict_prototypes)
			      werror("function declaration isn't a prototype");
			  $$ = bdty(UNARY CALL, $1, 0); }
		|  fdeclarator LB RB {  $$ = bdty(LB, $1, 0); }
		|  fdeclarator LB con_e RB {  
			if ((int)$3 <= 0)
				werror( "zero or negative subscript" );
			$$ = bdty(LB, $1, $3);
		}
		|   LP  fdeclarator  RP { $$ = $2; }
		|  name_lp  name_list  RP {
			if (Wstrict_prototypes && stab[$1].s_args == NULL)
			      werror("function declaration isn't a prototype");
			if( blevel!=0 )
				uerror("function declaration in bad context");
			$$ = bdty( UNARY CALL, bdty(NAME,NIL,$1), 0 );
			stwart = 0;
		}
		|  name_lp { ansifunc=1; proto_setfun($1); } ansi_args RP {
			$$ = bdty( UNARY CALL, bdty(NAME,NIL,$1), 0 );
			printf("ansi_args1: fun %s\n", stab[$1].sname);
		}
		|  name_lp RP {
			if (Wstrict_prototypes && stab[$1].s_args == NULL)
			      werror("function declaration isn't a prototype");
			$$ = bdty( UNARY CALL, bdty(NAME,NIL,$1), 0 );
			stwart = 0;
		}
		;

name_lp:	  NAME LP {
			/* turn off typedefs for argument names */
			/* stwart = SEENAME; */
			if( stab[$1].sclass == SNULL )
				stab[$1].stype = FTN;
		}
		;







ansi_args:	   ansi_list { proto_endarg(0); printf("ansi_args\n"); }
		|  ansi_list CM ELLIPSIS { 
			struct symtab *sym = getsym();
			sym->stype = -1;
			sym->sizoff = 0;
			sym->snext = schain[1];
			schain[1] = sym;
		}
		;

ansi_list:	   ansi_declaration { printf("ansi_list\n"); }
		|  ansi_list CM ansi_declaration { printf("ansi_list1\n"); }
		;

ansi_declaration:  type nfdeclarator {
			blevel++;
			defid(tymerge($1,$2), curclass);
			blevel--;
			ansiparams++;
			stwart = instruct;
			$1->in.op = FREE;
			proto_addarg($2);
			printf("ansi_declaration %s type %x op %d\n",
			    stab[$2->tn.rval].sname, $2->tn.type, 
			    $2->tn.op);
		}
		|  type ulmerdecl {
			struct symtab *sym;

			isproto++;
			if (ansiparams != 0 && $1->in.type == UNDEF)
				uerror("bad declaration");
			sym = getsym();
			sym->stype = $1->fn.type | $2;
			sym->sizoff = $1->fn.csiz;
			sym->snext = schain[1];
			schain[1] = sym;
			printf("ansi_declaration1: type %x\n", $1->tn.type);
			ansiparams++;
			$1->in.op = FREE;
			stwart = 0;
		}
		|  NAME { printf("NAMEulmerdecl2 %s\n", stab[$1].sname); }
		;

	/* XXX - only pointers for now */
ulmerdecl:	   MUL ulmerdecl { printf("ulmerdecl1\n"); $$ = INCREF($2); }
		|  { printf("ulmerdecl2\n"); $$ = 0; }
		;





name_list:	   NAME	{ ftnarg( $1 );  stwart = SEENAME; }
		|  name_list  CM  NAME { ftnarg( $3 );  stwart = SEENAME; }
		|  error
		;

		/* always preceeded by attributes: thus the $<nodep>0's */
init_dcl_list:	   init_declarator
			%prec CM
		|  init_dcl_list  CM {$<nodep>$=$<nodep>0;}  init_declarator
		;

		/* always preceeded by attributes */
xnfdeclarator:	   nfdeclarator {
			int id;

			defid( $1 = tymerge($<nodep>0,$1), curclass);
			id = $1->tn.rval;
			beginit(id);
			if (stab[id].sclass == AUTO ||
				stab[id].sclass == REGISTER ||
				stab[id].sclass == STATIC)
				stab[id].suse = -lineno;
		}
		|  error
		;

		/* always preceeded by attributes */
init_declarator:   nfdeclarator {  nidcl( tymerge($<nodep>0,$1) ); }
		|  fdeclarator {
			defid(tymerge($<nodep>0,$1), uclass(curclass));
			if (ansifunc)
				proto_enter($1);
			else if (paramno > 0)
				uerror("illegal argument");
			isproto = ansiparams = ansifunc = paramno = 0;
			while (schain[1] != NULL) {
				schain[1]->stype = TNULL;
				schain[1] = schain[1]->snext;
			}
		}
		|  xnfdeclarator ASSIGN e %prec CM {
			doinit( $3 );
			endinit();
		}
		|  xnfdeclarator ASSIGN LC init_list optcomma RC { endinit(); }
		| error {  fixinit(); }
		;

init_list:	   initializer %prec CM
		|  init_list CM  initializer
		;

initializer:	   e %prec CM {  doinit( $1 ); }
		|  ibrace init_list optcomma RC { irbrace(); }
		;

optcomma	:	/* VOID */
		|  CM
		;

optsemi		:	/* VOID */
		|  SM
		;

ibrace:		   LC {  ilbrace(); }
		;

/*	STATEMENTS	*/

compoundstmt:	   dcmpstmt
		|  cmpstmt
		;

dcmpstmt:	   begin r_dcl_stat_list stmt_list RC {  
			if( nerrors == 0 )
				prcstab(blevel);
			--blevel;
			if( blevel == 1 )
				blevel = 0;
			clearst( blevel );
			checkst( blevel );
			autooff = *--psavbc;
			regvar = *--psavbc;
		}
		;

cmpstmt:	   begin stmt_list RC {
			--blevel;
			if( blevel == 1 )
				blevel = 0;
			clearst( blevel );
			checkst( blevel );
			autooff = *--psavbc;
			regvar = *--psavbc;
		}
		;

begin:		  LC {
			if( blevel == 1 )
				dclargs();
			++blevel;
			if( psavbc > &asavbc[BCSZ-2] )
				cerror( "nesting too deep" );
			*psavbc++ = regvar;
			*psavbc++ = autooff;
		}
		;

statement:	   e SM { ecomp( $1 ); }
		|  compoundstmt
		|  ifprefix statement { deflab($1); reached = 1; }
		|  ifelprefix statement {
			if( $1 != NOLAB ){
				deflab( $1 );
				reached = 1;
			}
		}
		|  whprefix statement {
			branch(  contlab );
			deflab( brklab );
			if( (flostat&FBRK) || !(flostat&FLOOP))
				reached = 1;
			else
				reached = 0;
			resetbc(0);
		}
		|  doprefix statement WHILE  LP  e  RP   SM {
			deflab( contlab );
			if( flostat & FCONT )
				reached = 1;
			ecomp( buildtree( CBRANCH,
			    buildtree( NOT, $5, NIL ), bcon( $1 ) ) );
			deflab( brklab );
			reached = 1;
			resetbc(0);
		}
		|  forprefix .e RP statement
			={  deflab( contlab );
			    if( flostat&FCONT ) reached = 1;
			    if( $2 ) ecomp( $2 );
			    branch( $1 );
			    deflab( brklab );
			    if( (flostat&FBRK) || !(flostat&FLOOP) ) reached = 1;
			    else reached = 0;
			    resetbc(0);
			    }
		| switchpart statement
			={  if( reached ) branch( brklab );
			    deflab( $1 );
			   swend();
			    deflab(brklab);
			    if( (flostat&FBRK) || !(flostat&FDEF) ) reached = 1;
			    resetbc(FCONT);
			    }
		|  BREAK  SM
			={  if( brklab == NOLAB ) uerror( "illegal break");
			    else if(reached) branch( brklab );
			    flostat |= FBRK;
			    if( brkflag ) goto rch;
			    reached = 0;
			    }
		|  CONTINUE  SM
			={  if( contlab == NOLAB ) uerror( "illegal continue");
			    else branch( contlab );
			    flostat |= FCONT;
			    goto rch;
			    }
		|  RETURN  SM
			={  retstat |= NRETVAL;
			    branch( retlab );
			rch:
			    if( !reached ) werror( "statement not reached");
			    reached = 0;
			    }
		|  RETURN e  SM
			={  register NODE *temp;
			    idname = curftn;
			    temp = buildtree( NAME, NIL, NIL );
			    if(temp->in.type == TVOID)
				uerror("void function %s cannot return value",
					stab[idname].sname);
			    temp->in.type = DECREF( temp->in.type );
			    temp = buildtree( RETURN, temp, $2 );
			    /* now, we have the type of the RHS correct */
			    temp->in.left->in.op = FREE;
			    temp->in.op = FREE;
			    ecomp( buildtree( FORCE, temp->in.right, NIL ) );
			    retstat |= RETVAL;
			    branch( retlab );
			    reached = 0;
			    }
		|  GOTO NAME SM
			={  register NODE *q;
			    q = block( FREE, NIL, NIL, INT|ARY, 0, INT );
			    q->tn.rval = idname = $2;
			    defid( q, ULABEL );
			    stab[idname].suse = -lineno;
			    branch( stab[idname].offset );
			    goto rch;
			    }
		|   SM
		|  error  SM
		|  error RC
		|  label statement
		;
label:		   NAME COLON
			={  register NODE *q;
			    q = block( FREE, NIL, NIL, INT|ARY, 0, LABEL );
			    q->tn.rval = $1;
			    defid( q, LABEL );
			    reached = 1;
			    }
		|  CASE e COLON { addcase($2); reached = 1; }
		|  DEFAULT COLON { reached = 1; adddef(); flostat |= FDEF; }
		;

doprefix:	DO
			={  savebc();
			    if( !reached ) werror( "loop not entered at top");
			    brklab = getlab();
			    contlab = getlab();
			    deflab( $$ = getlab() );
			    reached = 1;
			    }
		;
ifprefix:	IF LP e RP
			={  ecomp( buildtree( CBRANCH, $3, bcon( $$=getlab()) ) ) ;
			    reached = 1;
			    }
		;
ifelprefix:	  ifprefix statement ELSE
			={  if( reached ) branch( $$ = getlab() );
			    else $$ = NOLAB;
			    deflab( $1 );
			    reached = 1;
			    }
		;

whprefix:	  WHILE  LP  e  RP
			={  savebc();
			    if( !reached ) werror( "loop not entered at top");
			    if( $3->in.op == ICON && $3->tn.lval != 0 ) flostat = FLOOP;
			    deflab( contlab = getlab() );
			    reached = 1;
			    brklab = getlab();
			    if( flostat == FLOOP ) tfree( $3 );
			    else ecomp( buildtree( CBRANCH, $3, bcon( brklab) ) );
			    }
		;
forprefix:	  FOR  LP  .e  SM .e  SM 
			={  if( $3 ) ecomp( $3 );
			    else if( !reached ) werror( "loop not entered at top");
			    savebc();
			    contlab = getlab();
			    brklab = getlab();
			    deflab( $$ = getlab() );
			    reached = 1;
			    if( $5 ) ecomp( buildtree( CBRANCH, $5, bcon( brklab) ) );
			    else flostat |= FLOOP;
			    }
		;
switchpart:	   SWITCH  LP  e  RP
			={  register NODE *q;
			
			    savebc();
			    brklab = getlab();
			    q = $3;
			    switch( q->in.type ) {
			    case CHAR:	case UCHAR:
			    case SHORT:	case USHORT:
			    case INT:	case UNSIGNED:
			    case MOE:	case ENUMTY:
				    break;
			    default:
				werror("switch expression not type int");
				q = makety( q, INT, q->fn.cdim, q->fn.csiz );
				}
#ifdef LINT
			    if( hflag && q->in.op == ICON )
				werror( "constant switch expression" );
#endif
			    ecomp( buildtree( FORCE, q, NIL ) );
			    branch( $$ = getlab() );
			    swstart();
			    reached = 0;
			    }
		;
/*	EXPRESSIONS	*/
con_e:		{ $$=instruct; stwart=instruct=0; } e %prec CM {
			$$ = icons( $2 );
			instruct=$1;
		}
		;

.e:		   e
		| 	{ $$=0; }
		;

elist:		   e %prec CM
		|  elist  CM  e { $$ = buildtree($2, $1, $3); }
		;

/*
 * Precedence order of operators.
 */
e:		   e CM e { $$ = buildtree(COMOP, $1, $3); }
		|  e ASSIGN e {  $$ = buildtree($2, $1, $3); }
		|  e ASOP e {  $$ = buildtree($2, $1, $3); }
		|  e QUEST e COLON e {
			$$=buildtree(QUEST, $1, buildtree(COLON, $3, $5));
		}
		|  e OROR e { $$ = buildtree($2, $1, $3); }
		|  e ANDAND e { $$ = buildtree($2, $1, $3); }
		|  e OR e { $$ = buildtree($2, $1, $3); }
		|  e ER e { $$ = buildtree($2, $1, $3); }
		|  e AND e { $$ = buildtree($2, $1, $3); }
		|  e EQUOP  e { $$ = buildtree($2, $1, $3); }
		|  e RELOP e { $$ = buildtree($2, $1, $3); }
		|  e SHIFTOP e { $$ = buildtree($2, $1, $3); }
		|  e PLUS e { $$ = buildtree($2, $1, $3); }
		|  e MINUS e { $$ = buildtree($2, $1, $3); }
		|  e DIVOP e { $$ = buildtree($2, $1, $3); }
		|  e MUL e { $$ = buildtree($2, $1, $3); }

		|  term
		;

term:		   term INCOP {  $$ = buildtree( $2, $1, bcon(1) ); }
		|  MUL term { $$ = buildtree( UNARY $1, $2, NIL ); }
		|  AND term {
			if( ISFTN($2->in.type) || ISARY($2->in.type) ){
				werror( "& before array or function: ignored" );
				$$ = $2;
			} else if( $2->in.op == UNARY MUL &&
			    ($2->in.left->in.op == STASG ||
			    $2->in.left->in.op == STCALL ||
			    $2->in.left->in.op == UNARY STCALL) ){
				/* legal trees but not available to users */
				uerror( "unacceptable operand of &" );
				$$ = buildtree( UNARY $1, $2, NIL );
			} else
				$$ = buildtree( UNARY $1, $2, NIL );
		}
		|  MINUS term ={  $$ = buildtree( UNARY $1, $2, NIL ); }
		|  UNOP term ={ $$ = buildtree( $1, $2, NIL ); }
		|  INCOP term {
			$$ = buildtree( $1==INCR ? ASG PLUS : ASG MINUS,
			    $2, bcon(1)  );
		}
		|  pushsizeof term %prec SIZEOF { $$ = doszof($2); --nsizeof; }
		|  LP cast_type RP term  %prec INCOP
			={  $$ = buildtree( CAST, $2, $4 );
			    $$->in.left->in.op = FREE;
			    $$->in.op = FREE;
			    $$ = $$->in.right;
			    }
		|  pushsizeof LP cast_type RP  %prec SIZEOF
			={  $$ = doszof( $3 ); --nsizeof; }
		|  term LB e RB {
			$$ = buildtree( UNARY MUL,
			    buildtree( PLUS, $1, $3 ), NIL );
		}
		|  funct_idn  RP {  $$=buildtree(UNARY CALL,$1,NIL); }
		|  funct_idn elist RP { $$=buildtree(CALL,$1,$2); }
		|  term STROP NAME
			={  if( $2 == DOT ){
				if( notlval( $1 ) &&
				    !($1->in.op == UNARY MUL &&
				      ($1->in.left->in.op == STASG ||
				       $1->in.left->in.op == STCALL ||
				       $1->in.left->in.op == UNARY STCALL)) )
				    uerror("structure reference must be addressable");
				$1 = buildtree( UNARY AND, $1, NIL );
				}
			    idname = $3;
			    $$ = buildtree( STREF, $1, buildtree( NAME, NIL, NIL ) );
			    }
		|  NAME
			={  idname = $1;
			    /* recognize identifiers in initializations */
			    if( blevel==0 && stab[idname].stype == UNDEF ) {
				register NODE *q;
				werror( "undeclared initializer name %s", stab[idname].sname );
				q = block( FREE, NIL, NIL, INT, 0, INT );
				q->tn.rval = idname;
				defid( q, EXTERN );
				}
			    $$=buildtree(NAME,NIL,NIL);
			    if( nsizeof == 0 )
				stab[$1].suse = -lineno;
			}
		|  ICON
			={  $$=bcon(0);
			    $$->tn.lval = lastcon;
			    $$->tn.rval = NONAME;
			    if( $1 ) $$->fn.csiz = $$->in.type = ctype(LONG);
			    }
		|  FCON ={  $$=buildtree(FCON,NIL,NIL); $$->fpn.fval = fcon; }
		|  DCON ={  $$=buildtree(DCON,NIL,NIL); $$->dpn.dval = dcon; }
		|  STRING ={  $$ = getstr(); /* get string contents */ }
		|   LP  e  RP ={ $$=$2; }
		;

cast_type:	  type null_decl ={
			$$ = tymerge( $1, $2 );
			$$->in.op = NAME;
			$1->in.op = FREE;
			}
		;

pushsizeof:	  SIZEOF ={ ++nsizeof; }
		;

null_decl:	   /* empty */ ={ $$ = bdty( NAME, NIL, -1 ); }
		|  LP RP ={ $$ = bdty( UNARY CALL, bdty(NAME,NIL,-1),0); }
		|  LP null_decl RP LP RP ={  $$ = bdty( UNARY CALL, $2, 0 ); }
		|  MUL null_decl ={  $$ = bdty( UNARY MUL, $2, 0 ); }
		|  null_decl LB RB ={  $$ = bdty( LB, $1, 0 ); }
		|  null_decl LB con_e RB ={  goto bary;  }
		|  LP null_decl RP { $$ = $2; }
		;

funct_idn:	   NAME  LP 
			={  if( stab[$1].stype == UNDEF ){
				register NODE *q;
				q = block( FREE, NIL, NIL, FTN|INT, 0, INT );
				q->tn.rval = $1;
				defid( q, EXTERN );
				}
			    idname = $1;
			    $$=buildtree(NAME,NIL,NIL);
			    stab[idname].suse = -lineno;
			}
		|  term  LP 
		;
%%

NODE *
mkty( t, d, s ) unsigned t; {
	return( block( TYPE, NIL, NIL, t, d, s ) );
	}

NODE *
bdty( op, p, v ) NODE *p; {
	register NODE *q;

	q = block( op, p, NIL, INT, 0, INT );

	switch( op ){

	case UNARY MUL:
	case UNARY CALL:
		break;

	case LB:
		q->in.right = bcon(v);
		break;

	case NAME:
		q->tn.rval = v;
		break;

	default:
		cerror( "bad bdty" );
		}

	return( q );
	}

/*
 * put n into the dimension table
 */
void
dstash(int n)
{
	if( curdim >= DIMTABSZ-1 ){
		cerror( "dimension table overflow");
		}
	dimtab[ curdim++ ] = n;
}

static void
savebc(void)
{
	if( psavbc > & asavbc[BCSZ-4 ] ){
		cerror( "whiles, fors, etc. too deeply nested");
		}
	*psavbc++ = brklab;
	*psavbc++ = contlab;
	*psavbc++ = flostat;
	*psavbc++ = swx;
	flostat = 0;
}

static void
resetbc(int mask)
{
	swx = *--psavbc;
	flostat = *--psavbc | (flostat&mask);
	contlab = *--psavbc;
	brklab = *--psavbc;
}

static void
addcase(NODE *p)
{ /* add case to switch */

	p = optim( p );  /* change enum to ints */
	if( p->in.op != ICON || p->tn.rval != NONAME ){
		uerror( "non-constant case expression");
		return;
		}
	if( swp == swtab ){
		uerror( "case not in switch");
		return;
		}
	if( swp >= &swtab[SWITSZ] ){
		cerror( "switch table overflow");
		}
	swp->sval = p->tn.lval;
	deflab( swp->slab = getlab() );
	++swp;
	tfree(p);
}

static void
adddef(void)
{ /* add default case to switch */
	if( swtab[swx].slab >= 0 ){
		uerror( "duplicate default in switch");
		return;
		}
	if( swp == swtab ){
		uerror( "default not inside switch");
		return;
		}
	deflab( swtab[swx].slab = getlab() );
}

static void
swstart(void)
{
	/* begin a switch block */
	if( swp >= &swtab[SWITSZ] ){
		cerror( "switch table overflow");
		}
	swx = swp - swtab;
	swp->slab = -1;
	++swp;
}

static void
swend(void)
{ /* end a switch block */

	struct sw *swbeg, *p, *q, *r, *r1;
	CONSZ temp;
	int tempi;

	swbeg = &swtab[swx+1];

	/* sort */

	r1 = swbeg;
	r = swp-1;

	while( swbeg < r ){
		/* bubble largest to end */
		for( q=swbeg; q<r; ++q ){
			if( q->sval > (q+1)->sval ){
				/* swap */
				r1 = q+1;
				temp = q->sval;
				q->sval = r1->sval;
				r1->sval = temp;
				tempi = q->slab;
				q->slab = r1->slab;
				r1->slab = tempi;
				}
			}
		r = r1;
		r1 = swbeg;
		}

	/* it is now sorted */

	for( p = swbeg+1; p<swp; ++p ){
		if( p->sval == (p-1)->sval ){
			uerror( "duplicate case in switch, %d", p->sval );
			return;
			}
		}

	genswitch( swbeg-1, swp-swbeg );
	swp = swbeg-1;
}

/*
 * Only used in prototypes.
 */
static struct symtab *
getsym(void)
{
	int i;

	for (i = 0; i < SYMTSZ; i++)
		if (stab[i].stype == TNULL)
			return &stab[i];
	cerror("symbol table full");
	return NULL;	/* XXX */
}
