/*	$Id$	*/

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
 * Comments for this grammar file. Ragge 021123
 *
 * ANSI support required rewrite of the function header and declaration
 * rules almost totally.
 *
 * The lex/yacc shared keywords are now split from the keywords used
 * in the rest of the compiler, to simplify use of other frontends.
 */

/*
 * At last count, there were 3 shift/reduce and no reduce/reduce conflicts
 * Two was funct_idn and the third was "dangling else".
 */

/*
 * Token used in C lex/yacc communications.
 */
%token	C_STRING	/* a string constant */
%token	C_ICON		/* an integer constant */
%token	C_FCON		/* a floating point constant */
%token	C_NAME		/* an identifier */
%token	C_TYPENAME	/* a typedef'd name */
%token	C_ANDAND	/* && */
%token	C_OROR		/* || */
%token	C_GOTO		/* unconditional goto */
%token	C_RETURN	/* return from function */
%token	C_TYPE		/* a type */
%token	C_CLASS		/* a storage class */
%token	C_ASOP		/* assignment ops */
%token	C_RELOP		/* <=, <, >=, > */
%token	C_EQUOP		/* ==, != */
%token	C_DIVOP		/* /, % */
%token	C_SHIFTOP	/* <<, >> */
%token	C_INCOP		/* ++, -- */
%token	C_UNOP		/* !, ~ */
%token	C_STROP		/* ., -> */
%token	C_STRUCT
%token	C_IF
%token	C_ELSE
%token	C_SWITCH
%token	C_BREAK
%token	C_CONTINUE
%token	C_WHILE	
%token	C_DO
%token	C_FOR
%token	C_DEFAULT
%token	C_CASE
%token	C_SIZEOF
%token	C_ENUM
%token	C_ELLIPSIS
%token	C_QUALIFIER
%token	C_FUNSPEC
%token	C_ASM

/*
 * Precedence
 */
%left ','
%right '=' C_ASOP
%right '?' ':'
%left C_OROR
%left C_ANDAND
%left '|'
%left '^'
%left '&'
%left C_EQUOP
%left C_RELOP
%left C_SHIFTOP
%left '+' '-'
%left '*' C_DIVOP
%right C_UNOP
%right C_INCOP C_SIZEOF
%left '[' '(' C_STROP
%{
# include "pass1.h"
# include <stdarg.h>
%}

	/* define types */
%start ext_def_list

%type <intval> con_e ifelprefix ifprefix whprefix forprefix doprefix switchpart
		type_qualifier_list
%type <nodep> e .e term enum_dcl struct_dcl cast_type funct_idn declarator
		direct_declarator elist type_specifier merge_attribs
		declarator parameter_declaration abstract_declarator
		parameter_type_list parameter_list declarator
		declaration_specifiers pointer direct_abstract_declarator
		specifier_qualifier_list merge_specifiers nocon_e
		identifier_list arg_param_list arg_declaration arg_dcl_list
%type <stri>	string strget C_STRING
%type <rp>	enum_head str_head

%token <intval> C_CLASS C_STRUCT C_RELOP C_DIVOP C_SHIFTOP
		C_ANDAND C_OROR C_STROP C_INCOP C_UNOP C_ASOP C_EQUOP
%token <nodep>  C_TYPE C_QUALIFIER C_ICON C_FCON
%token <strp>	C_NAME C_TYPENAME

%%

%{
	static int fun_inline;	/* Reading an inline function */
	int oldstyle;	/* Current function being defined */
	int got_type;
%}

ext_def_list:	   ext_def_list external_def
		| { ftnend(); }
		;

external_def:	   function_definition { blevel = 0; }
		|  declaration  { blevel = 0; symclear(0); }
		|  ';'
		|  error { blevel = 0; }
		;

function_definition:
	/* Ansi (or K&R header without parameter types) */
		   declaration_specifiers declarator {
			fundef($1, $2);
		} compoundstmt { fend(); }
	/* Same as above but without declaring function type */
		|  declarator { fundef(mkty(INT, 0, MKSUE(INT)), $1); } compoundstmt {
			fend();
		}
	/* K&R function without type declaration */
		|  declarator {
			if (oldstyle == 0)
				uerror("bad declaration in ansi function");
			fundef(mkty(INT, 0, MKSUE(INT)), $1);
		} arg_dcl_list compoundstmt { fend(); }
	/* K&R function with type declaration */
		|  declaration_specifiers declarator {
			if (oldstyle == 0)
				uerror("bad declaration in ansi function");
			fundef($1, $2);
		} arg_dcl_list compoundstmt { fend(); }
		;

/*
 * Returns a node pointer or NULL, if no types at all given.
 * Type trees are checked for correctness and merged into one
 * type node in typenode().
 */
declaration_specifiers:
		   merge_attribs { $$ = typenode($1); }
		;

merge_attribs:	   C_CLASS { $$ = block(CLASS, NIL, NIL, $1, 0, 0); }
		|  C_CLASS merge_attribs { $$ = block(CLASS, $2, NIL, $1,0,0);}
		|  type_specifier { $$ = $1; }
		|  type_specifier merge_attribs { $1->n_left = $2; $$ = $1; }
		|  C_QUALIFIER { $$ = $1; }
		|  C_QUALIFIER merge_attribs { $1->n_left = $2; $$ = $1; }
		|  function_specifiers { $$ = NIL; }
		|  function_specifiers merge_attribs { $$ = $2; }
		;

function_specifiers:
		   C_FUNSPEC {
			if (fun_inline)
				uerror("too many inline");
			fun_inline = 1;
		}
		;

type_specifier:	   C_TYPE { $$ = $1; }
		|  C_TYPENAME { 
			struct symtab *sp = lookup($1, 0);
			$$ = mkty(sp->stype, sp->sdf, sp->ssue);
			$$->n_sp = sp;
		}
		|  struct_dcl { $$ = $1; }
		|  enum_dcl { $$ = $1; }
		;

/*
 * Adds a pointer list to front of the declarators.
 * Note the UNARY MUL right node pointer usage.
 */
declarator:	   pointer direct_declarator {
			$$ = $1; $1->n_right->n_left = $2;
		}
		|  direct_declarator { $$ = $1; }
		;

/*
 * Return an UNARY MUL node type linked list of indirections.
 */
pointer:	   '*' { $$ = bdty(UNARY MUL, NIL); $$->n_right = $$; }
		|  '*' type_qualifier_list {
			$$ = bdty(UNARY MUL, NIL);
			$$->n_qual = $2;
			$$->n_right = $$;
		}
		|  '*' pointer {
			$$ = bdty(UNARY MUL, $2);
			$$->n_right = $2->n_right;
		}
		|  '*' type_qualifier_list pointer {
			$$ = bdty(UNARY MUL, $3);
			$$->n_qual = $2;
			$$->n_right = $3->n_right;
		}
		;

type_qualifier_list:
		   C_QUALIFIER { $$ = $1->n_type; nfree($1); }
		|  type_qualifier_list C_QUALIFIER {
			$$ = $1 | $2->n_type; nfree($2);
		}
		;

/*
 * Sets up a function declarator. The call node will have its parameters
 * connected to its right node pointer.
 */
direct_declarator: C_NAME { $$ = bdty(NAME, $1); }
		|  '(' declarator ')' { $$ = $2; }
		|  direct_declarator '[' nocon_e ']' { 
			$$ = block(LB, $1, $3, INT, 0, MKSUE(INT));
		}
		|  direct_declarator '[' ']' { $$ = bdty(LB, $1, 0); }
		|  direct_declarator '(' parameter_type_list ')' {
			$$ = bdty(CALL, $1, $3);
		}
		|  direct_declarator '(' identifier_list ')' { 
			$$ = bdty(CALL, $1, $3);
			if (blevel != 0)
				uerror("function declaration in bad context");
			oldstyle = 1;
		}
		|  direct_declarator '(' ')' { $$ = bdty(UNARY CALL, $1); }
		;

identifier_list:   C_NAME { $$ = bdty(NAME, $1); $$->n_type = FARG; }
		|  identifier_list ',' C_NAME { 
			$$ = bdty(NAME, $3);
			$$->n_type = FARG;
			$$ = block(CM, $1, $$, 0, 0, 0);
		}
		;

/*
 * Returns as parameter_list, but can add an additional ELLIPSIS node.
 * Calls revert() to get the parameter list in the forward order.
 */
parameter_type_list:
		   parameter_list { $$ = $1; }
		|  parameter_list ',' C_ELLIPSIS {
			$$ = block(CM, $1, block(ELLIPSIS, NIL, NIL, 0, 0, 0),
			    0, 0, 0);
		}
		;

/*
 * Returns a linked lists of nodes of op CM with parameters on
 * its right and additional CM nodes of its left pointer.
 * No CM nodes if only one parameter.
 */
parameter_list:	   parameter_declaration { $$ = $1; }
		|  parameter_list ',' parameter_declaration {
			$$ = block(CM, $1, $3, 0, 0, 0);
		}
		;

/*
 * Returns a node pointer to the declaration.
 */
parameter_declaration:
		   declaration_specifiers declarator {
			$$ = tymerge($1, $2);
			nfree($1);
			got_type = 0;
		}
		|  declaration_specifiers abstract_declarator { 
			$$ = tymerge($1, $2);
			nfree($1);
			got_type = 0;
		}
		|  declaration_specifiers {
			$$ = tymerge($1, bdty(NAME, NULL));
			nfree($1);
			got_type = 0;
		}
		;

abstract_declarator:
		   pointer {
			$$ = $1; $1->n_right->n_left = bdty(NAME, NULL); 
			got_type = 0;
		}
		|  direct_abstract_declarator { $$ = $1; }
		|  pointer direct_abstract_declarator { 
			$$ = $1; $1->n_right->n_left = $2;
		}
		;

direct_abstract_declarator:
		   '(' abstract_declarator ')' { $$ = $2; }
		|  '[' ']' { $$ = bdty(LB, bdty(NAME, NULL), 0); }
		|  '[' con_e ']' { $$ = bdty(LB, bdty(NAME, NULL), $2); }
		|  direct_abstract_declarator '[' ']' { $$ = bdty(LB, $1, 0); }
		|  direct_abstract_declarator '[' con_e ']' {
			$$ = bdty(LB, $1, $3);
		}
		|  '(' ')' { $$ = bdty(UNARY CALL, bdty(NAME, NULL)); }
		|  '(' parameter_type_list ')' {
			$$ = bdty(CALL, bdty(NAME, NULL), $2);
		}
		|  direct_abstract_declarator '(' ')' {
			$$ = bdty(UNARY CALL, $1);
		}
		|  direct_abstract_declarator '(' parameter_type_list ')' {
			$$ = bdty(CALL, $1, $3);
		}
		;

/*
 * K&R arg declaration, between ) and {
 */
arg_dcl_list:	   arg_declaration
		|  arg_dcl_list arg_declaration
		;


arg_declaration:   declaration_specifiers arg_param_list ';' {
			nfree($1);
		}
		;

arg_param_list:	   declarator { olddecl(tymerge($<nodep>0, $1)); }
		|  arg_param_list ',' declarator {
			olddecl(tymerge($<nodep>0, $3));
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
		|  {
			bccode();
			send_passt(IP_NEWBLK, regvar, autooff);
			send_passt(IP_LOCCTR, PROG);
		}
		;

/*
 * Variables are declared in init_declarator.
 */
declaration:	   declaration_specifiers ';' { nfree($1); goto inl; }
		|  declaration_specifiers init_declarator_list ';' {
			nfree($1);
			inl:
			fun_inline = 0;
		}
		;

/*
 * Normal declaration of variables. curtype contains the current type node.
 * Returns nothing, variables are declared in init_declarator.
 */
init_declarator_list:
		   init_declarator
		|  init_declarator_list ',' { $<nodep>$ = $<nodep>0; } init_declarator
		;

enum_dcl:	   enum_head '{' moe_list optcomma '}' { $$ = dclstruct($1); }
		|  C_ENUM C_NAME {  $$ = rstruct($2,0);  }
		|  C_ENUM C_TYPENAME {  $$ = rstruct($2,0);  }
		;

enum_head:	   C_ENUM {  $$ = bstruct(NULL,0); }
		|  C_ENUM C_NAME {  $$ = bstruct($2,0); }
		|  C_ENUM C_TYPENAME {  $$ = bstruct($2,0); }
		;

moe_list:	   moe
		|  moe_list ',' moe
		;

moe:		   C_NAME {  moedef( $1 ); }
		|  C_NAME '=' con_e {  strucoff = $3;  moedef( $1 ); }
		;

struct_dcl:	   str_head '{' struct_dcl_list '}' { $$ = dclstruct($1);  }
		|  C_STRUCT C_NAME {  $$ = rstruct($2,$1); }
		|  C_STRUCT C_TYPENAME {  $$ = rstruct($2,$1); }
		;

str_head:	   C_STRUCT {  $$ = bstruct(NULL, $1);  }
		|  C_STRUCT C_NAME {  $$ = bstruct($2,$1);  }
		|  C_STRUCT C_TYPENAME {  $$ = bstruct($2,$1);  }
		;

struct_dcl_list:   struct_declaration
		|  struct_dcl_list struct_declaration
		;

struct_declaration:
		   specifier_qualifier_list struct_declarator_list ';' {
			nfree($1);
		}
		;

specifier_qualifier_list:
		   merge_specifiers { $$ = typenode($1); }
		;

merge_specifiers:  type_specifier merge_specifiers { $1->n_left = $2;$$ = $1; }
		|  type_specifier { $$ = $1; }
		|  C_QUALIFIER merge_specifiers { $1->n_left = $2; $$ = $1; }
		|  C_QUALIFIER { $$ = $1; }
		;

struct_declarator_list:
		   struct_declarator { }
		|  struct_declarator_list ',' { $<nodep>$=$<nodep>0; } 
			struct_declarator { }
		;

struct_declarator: declarator {
			tymerge($<nodep>0, $1);
			$1->n_sp = getsymtab((char *)$1->n_sp, SMOSNAME); /* XXX */
			defid($1, $<nodep>0->n_lval); 
			nfree($1);
		}
		|  ':' con_e {
			if (!(instruct&INSTRUCT))
				uerror( "field outside of structure" );
			falloc(NULL, $2, -1, $<nodep>0);
		}
		|  declarator ':' con_e {
			if (!(instruct&INSTRUCT))
				uerror( "field outside of structure" );
			if( $3<0 || $3 >= FIELD ){
				uerror( "illegal field size" );
				$3 = 1;
			}
			if ($1->n_op == NAME) {
				$1->n_sp = getsymtab($1->n_name, SMOSNAME);
				defid( tymerge($<nodep>0,$1), FIELD|$3 );
				nfree($1);
			} else
				uerror("illegal declarator");
		}
		;

		/* always preceeded by attributes */
xnfdeclarator:	   declarator { init_declarator($<nodep>0, $1, 1); }
		;

/*
 * Handles declarations and assignments.
 * Returns nothing.
 */
init_declarator:   declarator { init_declarator($<nodep>0, $1, 0); }
		|  xnfdeclarator '=' e { doinit($3); endinit(); }
		|  xnfdeclarator '=' '{' init_list optcomma '}' { endinit(); }
		;

init_list:	   initializer %prec ',' { }
		|  init_list ','  initializer { }
		;

initializer:	   e %prec ',' {  doinit( $1 ); }
		|  ibrace init_list optcomma '}' { irbrace(); }
		;

optcomma	:	/* VOID */
		|  ','
		;

ibrace:		   '{' {  ilbrace(); }
		;

/*	STATEMENTS	*/

compoundstmt:	   begin declaration_list stmt_list '}' {  
			prcstab(blevel);
			--blevel;
			if( blevel == 1 )
				blevel = 0;
			symclear(blevel); /* Clean ut the symbol table */
			autooff = savctx->contlab;
			regvar = savctx->brklab;
			savctx = savctx->next;
		}
		|  begin stmt_list '}' {
			--blevel;
			if( blevel == 1 )
				blevel = 0;
			symclear(blevel); /* Clean ut the symbol table */
			autooff = savctx->contlab;
			regvar = savctx->brklab;
			savctx = savctx->next;
		}
		;

begin:		  '{' {
			struct savbc *bc = tmpalloc(sizeof(struct savbc));

			if (blevel == 1)
				dclargs();
			++blevel;
			oldstyle = 0;
			bc->brklab = regvar;
			bc->contlab = autooff;
			bc->next = savctx;
			savctx = bc;
		}
		;

statement:	   e ';' { ecomp( $1 ); }
		|  compoundstmt
		|  ifprefix statement { send_passt(IP_DEFLAB, $1); reached = 1; }
		|  ifelprefix statement {
			if ($1 != NOLAB) {
				send_passt(IP_DEFLAB, $1);
				reached = 1;
			}
		}
		|  whprefix statement {
			branch(contlab);
			send_passt(IP_DEFLAB, brklab );
			if( (flostat&FBRK) || !(flostat&FLOOP))
				reached = 1;
			else
				reached = 0;
			resetbc(0);
		}
		|  doprefix statement C_WHILE '(' e ')' ';' {
			send_passt(IP_DEFLAB,contlab);
			if (flostat & FCONT)
				reached = 1;
			/* Keep quiet if do { goto foo; } while (0); */
			if ($5->n_op == ICON && $5->n_lval == 0)
				reached = 1;
			ecomp(buildtree(CBRANCH, $5, bcon($1)));
			send_passt(IP_DEFLAB, brklab);
			reached = 1;
			resetbc(0);
		}
		|  forprefix .e ')' statement
			={  send_passt(IP_DEFLAB, contlab );
			    if( flostat&FCONT ) reached = 1;
			    if( $2 ) ecomp( $2 );
			    branch($1);
			    send_passt(IP_DEFLAB, brklab );
			    if( (flostat&FBRK) || !(flostat&FLOOP) ) reached = 1;
			    else reached = 0;
			    resetbc(0);
			    }
		| switchpart statement
			={  if( reached ) branch( brklab );
			    send_passt(IP_DEFLAB, $1 );
			   swend();
			    send_passt(IP_DEFLAB, brklab);
			    if( (flostat&FBRK) || !(flostat&FDEF) ) reached = 1;
			    resetbc(FCONT);
			    }
		|  C_BREAK  ';' {
			if (brklab == NOLAB)
				uerror("illegal break");
			else if (reached)
				branch(brklab);
			flostat |= FBRK;
			reached = 0;
		}
		|  C_CONTINUE  ';' {
			if (contlab == NOLAB)
				uerror("illegal continue");
			else
				branch(contlab);
			flostat |= FCONT;
			goto rch;
		}
		|  C_RETURN  ';' {
			branch(retlab);
			if (cftnsp->stype != VOID && cftnsp->stype != VOID+FTN)
				uerror("return value required");
			rch:
			if (!reached)
				werror( "statement is not reached");
			reached = 0;
		}
		|  C_RETURN e  ';' {
			register NODE *temp;

			spname = cftnsp;
			temp = buildtree( NAME, NIL, NIL );
			temp->n_type = DECREF(temp->n_type);
			temp = buildtree(RETURN, temp, $2);

			if (temp->n_type == VOID)
				ecomp(temp->n_right);
			else
				ecomp(buildtree(FORCE, temp->n_right, NIL));
			nfree(temp->n_left);
			nfree(temp);
			branch(retlab);
			reached = 0;
		}
		|  C_GOTO C_NAME ';' { gotolabel($2); goto rch; }
		|  asmstatement ';';
		|   ';'
		|  error  ';'
		|  error '}'
		|  label statement
		;

asmstatement:	   C_ASM '(' string ')' {
			char *ch = permalloc($3.len+1);
			memcpy(ch, $3.str, $3.len+1);
			send_passt(IP_ASM, ch);
		}
		;

label:		   C_NAME ':' { deflabel($1); reached = 1; }
		|  C_CASE e ':' { addcase($2); reached = 1; }
		|  C_DEFAULT ':' { reached = 1; adddef(); flostat |= FDEF; }
		;

doprefix:	C_DO {
			savebc();
			if (!reached)
				werror("loop not entered at top");
			brklab = getlab();
			contlab = getlab();
			send_passt(IP_DEFLAB,  $$ = getlab());
			reached = 1;
		}
		;
ifprefix:	C_IF '(' e ')' {
			ecomp(buildtree(CBRANCH, buildtree(NOT, $3, NIL),
			    bcon($$ = getlab())));
			reached = 1;
		}
		;
ifelprefix:	  ifprefix statement C_ELSE {
			if (reached)
				branch($$ = getlab());
			else
				$$ = NOLAB;
			send_passt(IP_DEFLAB, $1);
			reached = 1;
		}
		;

whprefix:	  C_WHILE  '('  e  ')' {
			savebc();
			if (!reached)
				werror("loop not entered at top");
			if ($3->n_op == ICON && $3->n_lval != 0)
				flostat = FLOOP;
			send_passt(IP_DEFLAB, contlab = getlab());
			reached = 1;
			brklab = getlab();
			if (flostat == FLOOP)
				tfree($3);
			else
				ecomp(buildtree(CBRANCH,
				    buildtree(NOT, $3, NIL), bcon(brklab)));
		}
		;
forprefix:	  C_FOR  '('  .e  ';' .e  ';' {
			if ($3)
				ecomp($3);
			else if (!reached)
				werror("loop not entered at top");
			savebc();
			contlab = getlab();
			brklab = getlab();
			send_passt(IP_DEFLAB, $$ = getlab());
			reached = 1;
			if ($5)
				ecomp(buildtree(CBRANCH,
				    buildtree(NOT, $5, NIL), bcon(brklab)));
			else
				flostat |= FLOOP;
		}
		;
switchpart:	   C_SWITCH  '('  e  ')' {
			    savebc();
			    brklab = getlab();
			    ecomp( buildtree( FORCE, $3, NIL ) );
			    branch( $$ = getlab());
			    swstart();
			    reached = 0;
			    }
		;
/*	EXPRESSIONS	*/
con_e:		{ $$=instruct; instruct=0; } e %prec ',' {
			$$ = icons( $2 );
			instruct=$1;
		}
		;

nocon_e:	{ $<intval>$=instruct; instruct=0; } e %prec ',' {
			instruct=$<intval>1;
			$$ = $2;
		}
		;

.e:		   e
		| 	{ $$=0; }
		;

elist:		   e %prec ','
		|  elist  ','  e { $$ = buildtree(CM, $1, $3); }
		;

/*
 * Precedence order of operators.
 */
e:		   e ',' e { $$ = buildtree(COMOP, $1, $3); }
		|  e '=' e {  $$ = buildtree(ASSIGN, $1, $3); }
		|  e C_ASOP e {  $$ = buildtree($2, $1, $3); }
		|  e '?' e ':' e {
			$$=buildtree(QUEST, $1, buildtree(COLON, $3, $5));
		}
		|  e C_OROR e { $$ = buildtree($2, $1, $3); }
		|  e C_ANDAND e { $$ = buildtree($2, $1, $3); }
		|  e '|' e { $$ = buildtree(OR, $1, $3); }
		|  e '^' e { $$ = buildtree(ER, $1, $3); }
		|  e '&' e { $$ = buildtree(AND, $1, $3); }
		|  e C_EQUOP  e { $$ = buildtree($2, $1, $3); }
		|  e C_RELOP e { $$ = buildtree($2, $1, $3); }
		|  e C_SHIFTOP e { $$ = buildtree($2, $1, $3); }
		|  e '+' e { $$ = buildtree(PLUS, $1, $3); }
		|  e '-' e { $$ = buildtree(MINUS, $1, $3); }
		|  e C_DIVOP e { $$ = buildtree($2, $1, $3); }
		|  e '*' e { $$ = buildtree(MUL, $1, $3); }

		|  term
		;

term:		   term C_INCOP {  $$ = buildtree( $2, $1, bcon(1) ); }
		|  '*' term { $$ = buildtree(UNARY MUL, $2, NIL); }
		|  '&' term {
			if( ISFTN($2->n_type) || ISARY($2->n_type) ){
				werror( "& before array or function: ignored" );
				$$ = $2;
			} else
				$$ = buildtree(UNARY AND, $2, NIL);
		}
		|  '-' term { $$ = buildtree(UNARY MINUS, $2, NIL ); }
		|  C_UNOP term ={ $$ = buildtree( $1, $2, NIL ); }
		|  C_INCOP term {
			$$ = buildtree( $1==INCR ? ASG PLUS : ASG MINUS,
			    $2, bcon(1)  );
		}
		|  C_SIZEOF term { $$ = doszof($2); got_type = 0; }
		|  '(' cast_type ')' term  %prec C_INCOP {
			$$ = buildtree(CAST, $2, $4);
			nfree($$->n_left);
			nfree($$);
			$$ = $$->n_right;
		}
		|  C_SIZEOF '(' cast_type ')'  %prec C_SIZEOF {
			$$ = doszof($3);
		}
		|  term '[' e ']' {
			$$ = buildtree( UNARY MUL,
			    buildtree( PLUS, $1, $3 ), NIL );
		}
		|  funct_idn  ')' { $$ = doacall($1, NIL); }
		|  funct_idn elist ')' { $$ = doacall($1, $2); }
		|  term C_STROP C_NAME { $$ = structref($1, $2, $3); }
		|  term C_STROP C_TYPENAME { $$ = structref($1, $2, $3); }
		|  C_NAME {
			spname = lookup($1, 0);
			/* recognize identifiers in initializations */
			if (blevel==0 && spname->stype == UNDEF) {
				register NODE *q;
				werror("undeclared initializer name %s",
				    spname->sname);
				q = block(NAME, NIL, NIL, INT, 0, MKSUE(INT));
				q->n_sp = spname;
				defid(q, EXTERN);
				nfree(q);
			}
			$$ = buildtree(NAME, NIL, NIL);
			spname->suse = -lineno;
			if (spname->sflags & SDYNARRAY)
				$$ = buildtree(UNARY MUL, $$, NIL);
		}
		|  C_ICON { $$ = $1; }
		|  C_FCON { $$ = $1; }
		|  string {  $$ = strend(&$1); /* get string contents */ }
		|   '('  e  ')' ={ $$=$2; }
		;

string:		   strget {
			$$.str = tmpalloc($1.len + 1);
			memcpy($$.str, $1.str, $1.len+1);
			$$.len = $1.len;
		}
		|  string strget { 
			$$.str = tmpalloc($1.len + $2.len + 1);
			memcpy($$.str, $1.str, $1.len);
			memcpy($$.str+$1.len, $2.str, $2.len+1);
			$$.len = $1.len + $2.len;
		}
		;

strget:		C_STRING { $$ = $1; }
		;

cast_type:	   specifier_qualifier_list {
			$$ = tymerge($1, bdty(NAME, NULL));
			nfree($1);
			got_type = 0;
		}
		|  specifier_qualifier_list abstract_declarator {
			$$ = tymerge($1, $2);
			nfree($1);
			got_type = 0;
		}
		;

funct_idn:	   C_NAME  '(' {
			struct symtab *s = lookup($1, 0);
			if (s->stype == UNDEF) {
				register NODE *q;
				q = block(NAME, NIL, NIL, FTN|INT, 0, MKSUE(INT));
				q->n_sp = s;
				defid(q, EXTERN);
				nfree(q);
			}
			if (s->sclass == STATIC)
				inline_ref($1);
			spname = s;
			$$ = buildtree(NAME, NIL, NIL);
			s->suse = -lineno;
		}
		|  term  '(' 
		;
%%

NODE *
mkty(TWORD t, union dimfun *d, struct suedef *sue)
{
	return block(TYPE, NIL, NIL, t, d, sue);
}

static NODE *
bdty(int op, ...)
{
	va_list ap;
	register NODE *q;

	va_start(ap, op);
	q = block(op, NIL, NIL, INT, 0, MKSUE(INT));

	switch (op) {
	case UNARY MUL:
	case UNARY CALL:
		q->n_left = va_arg(ap, NODE *);
		break;

	case CALL:
		q->n_left = va_arg(ap, NODE *);
		q->n_right = va_arg(ap, NODE *);
		break;

	case LB:
		q->n_left = va_arg(ap, NODE *);
		q->n_right = bcon(va_arg(ap, int));
		break;

	case NAME:
		q->n_sp = va_arg(ap, struct symtab *); /* XXX survive tymerge */
		break;

	default:
		cerror("bad bdty");
	}
	va_end(ap);

	return q;
}

/*
 * State for saving current switch state (when nested switches).
 */
struct savbc {
	struct savbc *next;
	int brklab;
	int contlab;
	int flostat;
	int swx;
} *savbc, *savctx;

static void
savebc(void)
{
	struct savbc *bc = tmpalloc(sizeof(struct savbc));

	bc->brklab = brklab;
	bc->contlab = contlab;
	bc->flostat = flostat;
	bc->next = savbc;
	savbc = bc;
	flostat = 0;
}

static void
resetbc(int mask)
{
	flostat = savbc->flostat | (flostat&mask);
	contlab = savbc->contlab;
	brklab = savbc->brklab;
	savbc = savbc->next;
}

struct swdef {
	struct swdef *next;	/* Next in list */
	int deflbl;		/* Label for "default" */
	struct swents *ents;	/* Linked sorted list of case entries */
	int nents;		/* # of entries in list */
} *swpole;

/*
 * add case to switch
 */
static void
addcase(NODE *p)
{
	struct swents *w, *sw = tmpalloc(sizeof(struct swents));

	p = optim(p);  /* change enum to ints */
	if (p->n_op != ICON || p->n_sp != NULL) {
		uerror( "non-constant case expression");
		return;
	}
	if (swpole == NULL) {
		uerror("case not in switch");
		return;
	}

	sw->sval = p->n_lval;
	send_passt(IP_DEFLAB, sw->slab = getlab());
	w = swpole->ents;
	if (swpole->ents == NULL) {
		sw->next = NULL;
		swpole->ents = sw;
	} else if (swpole->ents->next == NULL) {
		if (swpole->ents->sval == sw->sval) {
			uerror("duplicate case in switch");
		} else if (swpole->ents->sval < sw->sval) {
			sw->next = NULL;
			swpole->ents->next = sw;
		} else {
			sw->next = swpole->ents;
			swpole->ents = sw;
		}
	} else {
		while (w->next->next != NULL && w->next->sval < sw->sval) {
			w = w->next;
		}
		if (w->next->sval == sw->sval) {
			uerror("duplicate case in switch");
		} else if (w->next->sval > sw->sval) {
			sw->next = w->next;
			w->next = sw;
		} else {
			sw->next = NULL;
			w->next->next = sw;
		}
	}
	swpole->nents++;
	tfree(p);
}

/*
 * add default case to switch
 */
static void
adddef(void)
{
	if (swpole == NULL)
		uerror("default not inside switch");
	else if (swpole->deflbl != 0)
		uerror("duplicate default in switch");
	else
		send_passt(IP_DEFLAB, swpole->deflbl = getlab());
}

static void
swstart(void)
{
	struct swdef *sw = tmpalloc(sizeof(struct swdef));

	sw->deflbl = sw->nents = 0;
	sw->ents = NULL;
	sw->next = swpole;
	swpole = sw;
}

/*
 * end a switch block
 */
static void
swend(void)
{
	struct swents *sw, **swp;
	int i;

	sw = tmpalloc(sizeof(struct swents));
	swp = tmpalloc(sizeof(struct swents *) * (swpole->nents+1));

	sw->slab = swpole->deflbl;
	swp[0] = sw;

	for (i = 1; i <= swpole->nents; i++) {
		swp[i] = swpole->ents;
		swpole->ents = swpole->ents->next;
	}
	genswitch(swp, swpole->nents);

	swpole = swpole->next;
}

/*
 * Declare a variable or prototype.
 */
static void
init_declarator(NODE *tn, NODE *p, int assign)
{
	struct symtab *s;
	int class = tn->n_lval;
	NODE *typ;

	typ = tymerge(tn, p);
	typ->n_sp = lookup((char *)typ->n_sp, 0); /* XXX */

	if (ISFTN(typ->n_type) == 0) {
		if (assign) {
			defid(typ, class);
			s = typ->n_sp;
			beginit(s, class);
		} else {
			nidcl(typ, class);
		}
	} else {
		if (assign)
			uerror("cannot initialise function");
		defid(typ, uclass(class));
	}
	nfree(p);
}

/*
 * Declare a function.
 */
static void
fundef(NODE *tp, NODE *p)
{
	struct symtab *s;
	int class = tp->n_lval, oclass;

	/* Enter function args before they are clobbered in tymerge() */
	/* Typecheck against prototype will be done in defid(). */
	ftnarg(p);

	tymerge(tp, p);
	s = p->n_sp = lookup((char *)p->n_sp, 0); /* XXX */

	oclass = s->sclass;
	if (class == STATIC && oclass == EXTERN)
		werror("%s was first declared extern, then static", s->sname);

	if ((oclass == SNULL || oclass == USTATIC) &&
	    class == STATIC && fun_inline) {
		/* Unreferenced, store it for (eventual) later use */
		/* Ignore it if it not declared static */
		inline_start(s->sname);
	}

	cftnsp = s;
	defid(p, class);
	pfstab(s->sname);
	nfree(tp);
	nfree(p);

	blevel = 1;
}

static void
fend(void)
{
	if (blevel)
		cerror("function level error");
	ftnend();
	fun_inline = 0;
	cftnsp = NULL;
}

static NODE *
structref(NODE *p, int f, char *name)
{
	NODE *r;

	if (f == DOT)
		p = buildtree(UNARY AND, p, NIL);
	r = block(NAME, NIL, NIL, INT, 0, MKSUE(INT));
	r->n_name = name;
	return buildtree(STREF, p, r);
}

static void
olddecl(NODE *p)
{
	struct symtab *s;

	s = lookup((char *)p->n_sp, 0);
	if (s->slevel != 1 || s->stype == UNDEF)
		uerror("parameter '%s' not defined", s->sname);
	else if (s->stype != FARG)
		uerror("parameter '%s' redefined", s->sname);
	s->stype = p->n_type;
	s->sdf = p->n_df;
	s->ssue = p->n_sue;
	nfree(p);
}

void
branch(int lbl)
{
	int r = reached++;
	ecomp(block(GOTO, bcon(lbl), NIL, INT, 0, 0));
	reached = r;
}
