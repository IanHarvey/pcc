/*	cgram.y	4.22	87/12/09	*/

/*
 * Comments for this grammar file. Ragge 021123
 *
 * ANSI support required rewrite of the function header and declaration
 * rules almost totally. The grammar used for this is based on a usenet
 * posting in comp.lang.c many years ago.
 *
 * Because things like typedef'd types causes problem when recognizing
 * the correctness of parameters to old versus new-style declarations
 * all trees are kept around until it is clear which style of function
 * (or prototype) declaration it is currently matching. This is done by
 * just returning tree nodes further up while traversing the tree, and
 * do the complete verification and declaration when it is discovered
 * what is actually matched. To do this a bunch of new node types are
 * created; these node types are never seen outside this file.
 *
 */

/*
 * Below is some old comments left for curious people.
 */
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
/*
 * This file contains definitions for all the constants and structures
 *      needed to use the intermediate code files generated and read by
 *      the Portable C Compiler and related compilers.
 *
 * Rules for changing this code:
 *   1) All op values must be integer constants -- this permits us to run
 *      a 'sed' script on this file to create %term declarations for yacc.
 *   2) Because the PCC uses fancy ASG and UNARY macros, assignment
 *      operators must have values 1 greater than corresponding normal
 *      operators, and unary operators must have values 2 greater ditto.
 *   3) Ops used only by f1 must have values >= 150 (PCCF_FORTOPS).
 *   4) Other language-dependent ops must have values >= 200.
 */

/*
 * Constants.
 */
%term	STRING	3       /* a string constant */
%term	ICON	4       /* an integer constant */
%term	FCON	5       /* a floating point constant */
%term	DCON	6       /* a double precision f.p. constant */

/*
 * Leaf types.  
 */
%term	NAME		7	/* an identifier */
%term	REG		8	/* a register */
%term	OREG		9	/* register and offset */
%term	CCODES		10	/* condition codes */
%term	FLD		11	/* a bit field */

/*
 * Arithmetic operators.
 */
%term	PLUS		12	/* + */
%term	PLUSEQ		13	/* += */
%term	MINUS		15	/* - */
%term	MINUSEQ		16	/* -= */
%term	MUL		18	/* * */
%term	MULEQ		19	/* *= */
%term	DIV		21	/* / */
%term	DIVEQ		22	/* /= */
%term	MOD		23	/* % */
%term	MODEQ		24	/* %= */
%term	INCR		25	/* ++ */
%term	DECR		26	/* -- */
%term	ASSIGN		27	/* = (these last 3 are stretching it) */

/*
 * Bit operators.
 */
%term	AND		28	/* & */
%term	ANDEQ		29	/* &= */
/* Reserve a slot for 'unary &', jargon for PCC_ADDROF */
%term	OR		31	/* | */
%term	OREQ		32	/* |= */
%term	ER		33	/* ^ */
%term	EREQ		34	/* ^= */
%term	LS		35	/* << */
%term	LSEQ		36	/* <<= */
%term	RS		37	/* >> */
%term	RSEQ		38	/* >>= */
%term	COMPL		39	/* ~ */

/*
 * Booleans.    
 */
%term	EQ		40	/* == */
%term	NE		41	/* != */
%term	LE		42	/* <= */
%term	LT		43	/* < */
%term	GE		44	/* >= */
%term	GT		45	/* > */
%term	ULE		46	/* unsigned <= */
%term	ULT		47	/* unsigned < */
%term	UGE		48	/* unsigned >= */
%term	UGT		49	/* unsigned > */
%term	QUEST		50	/* ? (for conditional expressions) */
%term	COLON		51	/* : (for conditional expressions) */
%term	ANDAND		52	/* && */
%term	OROR		53	/* || */
%term	NOT		54	/* ! */

/*
 * Function calls.
 */
%term	CALL		55	/* call by value */
/* no ASG */
%term	UCALL		57	/* call with no arguments */
%term	FORTCALL	58	/* call by reference? */
/* no ASG */

/*
 * Special structure operators.
 */
%term	DOT		64	/* . */
%term	STREF		65	/* -> */
%term	STASG		66	/* structure assignment */
%term	STARG		67	/* an argument of type structure */
%term	STCALL		68	/* a function of type structure */
/* no ASG */
%term	USTCALL		70	/* unary structure function */

/*
 * Conversions.
 */
%term	SCONV		71	/* scalar conversion */
%term	PCONV		72	/* pointer conversion */
%term	PMCONV		73	/* pointer multiply conversion */
%term	PVCONV		74	/* pointer divide conversion */
%term	CAST		75	/* redundant? */

/*
 * Bracket types.
 */
%term	LB		76	/* [ */
%term	RB		77	/* ] */

/*
 * Comma nodes.
 */
%term	COMOP		78	/* , (in expressions) */
%term	CM		79	/* , (in argument lists) */

/*
 * Miscellaneous.
 */
%term	FORCE		80	/* result of last expression goes in r0 */
%term	GOTO		81	/* unconditional goto */
%term	CBRANCH		82	/* goto label if !test */
%term	RETURN		83	/* return from function */
%term	INIT		84	/* initialized data */
%term	TYPE		85	/* a type */
%term	CLASS		86	/* a storage class */
%term	ELLIPSIS	87	/* "..." */
%term	QUALIFIER	88

%term	MAXOP		88	/* highest numbered PCC op */

/*
 * Leftover operators.
 */
%term	ASOP		100	/* assignment ops */
%term	RELOP		101	/* <=, <, >=, > */
%term	EQUOP		102	/* ==, != */
%term	DIVOP		103	/* /, % */
%term	SHIFTOP		104	/* <<, >> */
%term	INCOP		105	/* ++, -- */
%term	UNOP		106	/* !, ~ */
%term	STROP		107	/* ., -> */

%term	LP		108	/* ( */
%term	RP		109	/* ) */
%term	LC		110     /* { */
%term	RC		111     /* } */

/*
 * C keywords.
 */
%term	STRUCT		112
%term	IF		113
%term	ELSE		114
%term	SWITCH		115
%term	BREAK		116
%term	CONTINUE	117
%term	WHILE		118
%term	DO		119
%term	FOR		120
%term	DEFAULT		121
%term	CASE		122
%term	SIZEOF		123
%term	ENUM		124
%term	SM		125

/*
 * Pseudos used while parsing (as node op)
 */
%term	TYPELIST	126	/* Linked list for arguments */
%term	ARGNODE		127	/* Type node on left, declarator on right */

/*
 * Precedence
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
		enum_head str_head
%type <nodep> e .e term type enum_dcl struct_dcl
		cast_type null_decl funct_idn declarator
		direct_declarator elist type_specifier merge_attribs
		declarator parameter_declaration abstract_declarator
		parameter_type_list parameter_list declarator
		declaration_specifiers pointer direct_abstract_declarator
		specifier_qualifier_list merge_specifiers

%token <intval> CLASS NAME STRUCT RELOP CM DIVOP PLUS MINUS SHIFTOP MUL AND
		OR ER ANDAND OROR ASSIGN STROP INCOP UNOP ICON ASOP EQUOP
%token <nodep>  TYPE QUALIFIER

%%

%{
	static int nsizeof = 0;
	static int oldstyle;	/* Current function being defined */
%}

ext_def_list:	   ext_def_list external_def
		| { ftnend(); }
		;

external_def:	   function_definition { blevel = 0; }
		|  declaration 
		|  SM
		|  error { blevel = 0; }
		;

function_definition:
	/* Ansi (or K&R header without parameter types) */
		   declaration_specifiers declarator {
			fundef($1, $2);
		} compoundstmt { fend(); }
	/* Same as above but without declaring function type */
		|  declarator { fundef(mkty(INT, 0, INT), $1); } compoundstmt {
			fend();
		}
	/* K&R function without type declaration */
		|  declarator {
			if (oldstyle == 0)
				uerror("bad declaration in ansi function");
			fundef(mkty(INT, 0, INT), $1);
		} arg_dcl_list compoundstmt { fend(); oldstyle = 0; }
	/* K&R function with type declaration */
		|  declaration_specifiers declarator {
			if (oldstyle == 0)
				uerror("bad declaration in ansi function");
			fundef($1, $2);
		} arg_dcl_list compoundstmt { fend(); oldstyle = 0; }
		;

/*
 * Returns a node pointer or NULL, if no types at all given.
 * Type trees are checked for correctness and merged into one
 * type node in typenode().
 */
declaration_specifiers:
		   merge_attribs { $$ = typenode($1); }
		;

merge_attribs:	   CLASS { $$ = block(CLASS, NIL, NIL, $1, 0, 0); }
		|  CLASS merge_attribs { $$ = block(CLASS, $2, NIL, $1,0,0);}
		|  type_specifier { $$ = $1; }
		|  type_specifier merge_attribs { $1->in.left = $2; $$ = $1; }
		|  QUALIFIER { $$ = $1; }
		|  QUALIFIER merge_attribs { $1->in.left = $2; $$ = $1; }
		;

type_specifier:	   TYPE { $$ = $1; }
		|  struct_dcl { $$ = $1; }
		|  enum_dcl { $$ = $1; }
		;

/*
 * Adds a pointer list to front of the declarators.
 * Note the UNARY MUL right node pointer usage.
 */
declarator:	   pointer direct_declarator {
			$$ = $1; $1->in.right->in.left = $2;
		}
		|  direct_declarator { $$ = $1; }
		;

/*
 * Return an UNARY MUL node type linked list of indirections.
 * XXX - must handle qualifiers correctly.
 */
pointer:	   MUL { $$ = bdty(UNARY MUL, NIL, 0); $$->in.right = $$; }
		|  MUL type_qualifier_list {
			$$ = bdty(UNARY MUL, NIL, 0); $$->in.right = $$;
		}
		|  MUL pointer {
			$$ = bdty(UNARY MUL, $2, 0);
			$$->in.right = $2->in.right;
		}
		|  MUL type_qualifier_list pointer {
			$$ = bdty(UNARY MUL, $3, 0);
			$$->in.right = $3->in.right;
		}
		;

type_qualifier_list:
		   QUALIFIER { $1->in.op = FREE; }
		|  type_qualifier_list QUALIFIER { $2->in.op = FREE; }
		;

/*
 * Sets up a function declarator. The call node will have its parameters
 * connected to its right node pointer.
 */
direct_declarator: NAME { $$ = bdty(NAME, NIL, $1); }
		|  LP declarator RP { $$ = $2; }
		|  direct_declarator LB con_e RB { 
			if ($3 < 0)
				werror("negative array subscript");
			$$ = bdty(LB, $1, $3);
		}
		|  direct_declarator LB RB { $$ = bdty(LB, $1, 0); }
		|  direct_declarator LP parameter_type_list RP {
			$$ = bdty(UNARY CALL, $1, 0);
			$$->in.right = $3;
		}
		|  direct_declarator LP identifier_list RP { 
			$$ = bdty(UNARY CALL, $1, 0);
			if (blevel != 0)
				uerror("function declaration in bad context");
			oldstyle = 1;
			stwart = 0;
		}
		|  direct_declarator LP RP { $$ = bdty(UNARY CALL, $1, 0); }
		;

identifier_list:   NAME { ftnarg($1); }
		|  identifier_list CM NAME { ftnarg($3); }
		;

/*
 * Returns as parameter_list, but can add an additional ELLIPSIS node.
 * Calls revert() to get the parameter list in the forward order.
 */
parameter_type_list:
		   parameter_list { $$ = revert($1); }
		|  parameter_list CM ELLIPSIS {
			$$ = block(TYPELIST, block(ELLIPSIS, NIL, NIL, 0, 0, 0),
			    $1, 0, 0, 0);
			$$ = revert($$);
		}
		;

/*
 * Returns a linked lists of nodes of op TYPELIST with parameters on
 * its left and additional TYPELIST nodes of its right pointer.
 */
parameter_list:	   parameter_declaration {
			$$ = block(TYPELIST, $1, NIL, 0, 0, 0);
		}
		|  parameter_list CM parameter_declaration {
			$$ = block(TYPELIST, $3, $1, 0, 0, 0);
		}
		;

/*
 * Returns a node pointer to the declaration.
 */
parameter_declaration:
		   declaration_specifiers declarator {
			$$ = block(ARGNODE, $1, $2, 0, 0, 0);
			stwart = 0;
		}
		|  declaration_specifiers abstract_declarator { 
			$$ = block(ARGNODE, $1, $2, 0, 0, 0);
			stwart = 0;
		}
		|  declaration_specifiers {
			$$ = block(ARGNODE, $1, NIL, 0, 0, 0);
			stwart = 0;
		}
		;

abstract_declarator:
		   pointer { $$ = $1; }
		|  direct_abstract_declarator { $$ = $1; }
		|  pointer direct_abstract_declarator { cerror("abstract_declarator3"); }
		;

direct_abstract_declarator:
		   LP abstract_declarator RP { $$ = $2; }
		|  LB RB { cerror("direct_abstract_declarator2"); }
		|  LB con_e RB { $$ = bdty(LB, NIL, $2); }
		|  direct_abstract_declarator LB RB { cerror("direct_abstract_declarator4"); }
		|  direct_abstract_declarator LB con_e RB { cerror("direct_abstract_declarator5"); }
		|  LP RP { cerror("direct_abstract_declarator6"); }
		|  LP parameter_type_list RP { cerror("direct_abstract_declarator7"); }
		|  direct_abstract_declarator LP RP { cerror("direct_abstract_declarator8"); }
		|  direct_abstract_declarator LP parameter_type_list RP {
			$$ = bdty(UNARY CALL, $1, 0);
			$$->in.right = $3;
		}
		;

/*
 * K&R arg declaration, between ) and {
 */
arg_dcl_list:	   arg_declaration
		|  arg_dcl_list arg_declaration
		;


arg_declaration:   declaration_specifiers arg_param_list SM { $1->in.op=FREE; }
		;

arg_param_list:	   declarator {
			defid(tymerge($<nodep>0, $1), $<nodep>0->in.su);
			stwart = instruct;
		}
		|  arg_param_list CM { $<nodep>$ = $<nodep>0; } declarator {
			defid(tymerge($<nodep>0, $4), $<nodep>0->in.su);
			stwart = instruct;
		}
		;

/*
 * Declarations in beginning of blocks.
 */
declaration_list:  declaration
		|  declaration_list declaration
		;

/*
 * Here starts the old YACC code.
 */

stmt_list:	   stmt_list statement
		|  /* empty */ {  bccode(); (void) locctr(PROG); }
		;

/*
 * Variables are declared in init_declarator.
 */
declaration:	   declaration_specifiers SM { $1->in.op = FREE; }
		|  declaration_specifiers init_declarator_list SM {
			$1->in.op = FREE;
		}
		;

/*
 * Normal declaration of variables. curtype contains the current type node.
 * Returns nothing, variables are declared in init_declarator.
 */
init_declarator_list:
		   init_declarator
		|  init_declarator_list CM { $<nodep>$ = $<nodep>0; } init_declarator
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

struct_dcl:	   str_head LC struct_dcl_list RC { $$ = dclstruct($1);  }
		|  STRUCT NAME {  $$ = rstruct($2,$1); }
		;

str_head:	   STRUCT {  $$ = bstruct(-1,$1);  stwart=0; }
		|  STRUCT NAME {  $$ = bstruct($2,$1);  stwart=0;  }
		;

struct_dcl_list:   struct_declaration
		|  struct_dcl_list struct_declaration
		;

struct_declaration:
		   specifier_qualifier_list struct_declarator_list SM {
			$1->in.op = FREE;
		}
		;

specifier_qualifier_list:
		   merge_specifiers { $$ = typenode($1); }
		;

merge_specifiers:  type_specifier merge_specifiers { $1->in.left = $2;$$ = $1; }
		|  type_specifier { $$ = $1; }
		|  QUALIFIER merge_specifiers { $1->in.left = $2; $$ = $1; }
		|  QUALIFIER { $$ = $1; }
		;

struct_declarator_list:
		   struct_declarator { stwart = 0; }
		|  struct_declarator_list CM { $<nodep>$=$<nodep>0; } 
			struct_declarator { stwart = 0; }
		;

struct_declarator: declarator {
			NODE *w = $1;

			/* Remove function args */
			while (w && w->in.op != NAME) {
				if (w->in.op == UNARY CALL)
					cleanargs(w->in.right);
				w = w->in.left;
			}
			defid(tymerge($<nodep>0,$1), $<nodep>0->in.su);
			stwart = instruct;
		}
		|  COLON con_e {
			if (!(instruct&INSTRUCT))
				uerror( "field outside of structure" );
			(void)falloc( stab, $2, -1, $<nodep>0 );
		}
		|  declarator COLON con_e {
			if (!(instruct&INSTRUCT))
				uerror( "field outside of structure" );
			if( $3<0 || $3 >= FIELD ){
				uerror( "illegal field size" );
				$3 = 1;
			}
			defid( tymerge($<nodep>0,$1), FIELD|$3 );
		}
		;

		/* always preceeded by attributes */
xnfdeclarator:	   declarator { init_declarator($1, $<nodep>0, 1); }
		;

/*
 * Handles declarations and assignments.
 * Returns nothing.
 */
init_declarator:   declarator { init_declarator($1, $<nodep>0, 0); }
		|  xnfdeclarator ASSIGN e { doinit($3); endinit(); }
		|  xnfdeclarator ASSIGN LC init_list optcomma RC { endinit(); }
		;

init_list:	   initializer %prec CM { }
		|  init_list CM  initializer { }
		;

initializer:	   e %prec CM {  doinit( $1 ); }
		|  ibrace init_list optcomma RC { irbrace(); }
		;

optcomma	:	/* VOID */
		|  CM
		;

ibrace:		   LC {  ilbrace(); }
		;

/*	STATEMENTS	*/

compoundstmt:	   begin declaration_list stmt_list RC {  
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
		|  begin stmt_list RC {
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
		|  null_decl LB con_e RB ={  
  			if( (int)$3 <= 0 )
  				werror( "zero or negative subscript" );
  			$$ = bdty( LB, $1, $3 );

		}
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
 * Reverse the arguments so that they ends up in the correct order.
 */
static NODE *
revert(NODE *p)
{
	NODE *t, *r = NIL;

	while (p != NIL) {
		t = p;
		p = p->in.right;
		t->in.right = r;
		r = t;
	}
	return r;
}

static void cleanargs(NODE *args);
/*
 * Get the symbol table index for the name in the tree.
 */
static int
findname(NODE *p)
{
	if (p->in.op != ARGNODE)
		cerror("findname != ARGNODE");
	p = p->in.right;
	if (p == NULL) {
		uerror("missing argument name");
		return 0;
	}
	switch (p->in.op) {
	case NAME:
		return p->tn.rval;
	case UNARY CALL:
		cleanargs(p->in.right);
		/* FALLTHROUGH */
	case UNARY MUL:
		do 
			p = p->in.left;
		while (p && p->in.op != NAME);
		if (p->in.op == NAME)
			return p->tn.rval;
		uerror("missing parameter name");
		break;
	default:
		cerror("findname op %d", p->in.op);
	}
	return 0;
}

/*
 * Declare the actual arguments when the function is declared.
 */
static void
doargs(NODE *link)
{
	NODE *pp, *p = link;
	int num;

	/* Check void (or nothing) first */
	if (p && p->in.right == NIL && p->in.left->in.right == NIL &&
	    p->in.left->in.left->in.op == TYPE &&
	    p->in.left->in.left->in.type == 0) {
		p->in.left->in.left->in.op = FREE;
		p->in.left->in.op = FREE;
		p->in.op = FREE;
		return;
	}
		
	while (p != NIL) {
		if (p->in.op != TYPELIST)
			cerror("doargs != TYPELIST");
		num = findname(p->in.left);
		if (num == 0)
			return; /* failed anyway, forget this */
		ftnarg(num);
		p = p->in.right;
	}
	blevel = 1;
	p = link;
	while (p != NIL) {
		pp = p->in.left;
		if (pp->in.op != ARGNODE)
			cerror("doargs!= ARGNODE");

		defid(tymerge(pp->in.left, pp->in.right), SNULL);
		pp->in.left->in.op = FREE;
		p->in.op = FREE;
		pp->in.op = FREE;
		p = p->in.right;
	}
}

/*
 * Clean the prototype parameters and ignore them for now.
 */
static void
cleanargs(NODE *args)
{
	if (args == NIL)
		return;
	switch (args->in.op) {
	case TYPELIST:
	case ARGNODE:
	case UNARY CALL:
	case LB:
		cleanargs(args->in.left);
		cleanargs(args->in.right);
		break;
	case UNARY MUL:
		cleanargs(args->in.left);
		break;
	case NAME:
		if (stab[args->tn.rval].stype == UNDEF)
			stab[args->tn.rval].stype = TNULL;
		/* FALLTHROUGH */
	case TYPE:
	case ICON:
	case ELLIPSIS:
		break;
	default:
		cerror("cleanargs op %d", args->in.op);
	}
	args->in.op = FREE;
}

/*
 * Declare a variable or prototype.
 */
static void
init_declarator(NODE *p, NODE *tn, int assign)
{
	NODE *typ, *w = p;
	int id, class = tn->in.su;
	int isfun = 0;

	/*
	 * Traverse down to see if this is a function declaration.
	 * In that case, only call defid(), otherwise nidcl().
	 * While traversing, discard function parameters.
	 */
	while (w->in.op != NAME) {
		if (w->in.op == UNARY CALL) {
			cleanargs(w->in.right); /* Remove args */
			if (w->in.left->in.op == NAME)
				isfun++;
		}
		w = w->in.left;
	}

	typ = tymerge(tn, p);
	if (isfun == 0) {
		if (assign) {
			defid(typ, class);
			id = typ->tn.rval;
			beginit(id, class);
			if (stab[id].sclass == AUTO ||
			    stab[id].sclass == REGISTER ||
			    stab[id].sclass == STATIC)
				stab[id].suse = -lineno;
		} else {
			nidcl(typ, class);
		}
	} else {
		if (assign)
			uerror("cannot initialise function");
		defid(typ, uclass(class));
		if (paramno > 0)
			cerror("illegal argument"); /* XXX */
		paramno = 0;
		while (schain[1] != NULL) {
			schain[1]->stype = TNULL;
			schain[1] = schain[1]->snext;
		}
	}
	p->in.op = FREE;
}

/*
 * Declare a function.
 */
static void
fundef(NODE *tp, NODE *p)
{
	NODE *alst, *w = p;
	int class = tp->in.su;

	/*
	 * Traverse down to find the function arguments.
	 */
	while (w->in.op != NAME) {
		if (w->in.op == UNARY CALL) {
			if (w->in.left->in.op == NAME) {
				alst = w->in.right;
			} else
				cleanargs(w->in.right);
		}
		w = w->in.left;
	}

	defid(tymerge(tp, p), class);
	if (nerrors == 0)
		pfstab(stab[p->tn.rval].sname);
	doargs(alst);
	tp->in.op = FREE;
}

static void
fend(void)
{
	if (blevel)
		cerror("function level error");
	if (reached)
		retstat |= NRETVAL;
	ftnend();
}
