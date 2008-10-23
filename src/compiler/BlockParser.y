%{
  /*
   * Copyright (C) 2008, The EROS Group, LLC.
   * All rights reserved.
   *
   * Redistribution and use in source and binary forms, with or
   * without modification, are permitted provided that the following
   * conditions are met:
   *
   *   - Redistributions of source code must contain the above
   *     copyright notice, this list of conditions, and the following
   *     disclaimer.
   *
   *   - Redistributions in binary form must reproduce the above
   *     copyright notice, this list of conditions, and the following
   *     disclaimer in the documentation and/or other materials
   *     provided with the distribution.
   *
   *   - Neither the names of the copyright holders nor the names of any
   *     of any contributors may be used to endorse or promote products
   *     derived from this software without specific prior written
   *     permission.
   *
   * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   */

/// @file
///
/// @brief Partial implementation of a parser for a provisional
/// block-structured BitC syntax.
/// 
/// This is very incomplete, and very much in flux at the moment.

#include <sys/fcntl.h>
#include <sys/stat.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <dirent.h>
#include <iostream>

#include <string>

#include "Version.hxx"
#include "AST.hxx"
#include "ParseType.hxx"
#include "UocInfo.hxx"
#include "Options.hxx"

using namespace boost;
using namespace sherpa;
using namespace std;

#define YYSTYPE ParseType
#define YYLEX_PARAM (BlockLexer *)lexer
#undef yyerror
#define yyerror(lexer, s) lexer->ReportParseError(s)

#include "BlockLexer.hxx"

#define SHOWPARSE(s) \
  do { \
    if (Options::showParse) \
      lexer->errStream << (s) << std::endl;		\
  } while (false);
#define SHOWPARSE1(s,x) \
  do { \
    if (Options::showParse) \
      lexer->errStream << (s) << " " << (x) << std::endl;	\
  } while (false);


inline int block_lex(YYSTYPE *lvalp, BlockLexer *lexer)
{
  return lexer->lex(lvalp);
}

// If the passed exprSeq has a documentation string at the front,
// remove it. Note that if the exprSeq has length 1 then the string is
// the expression value and not a documentation comment.
static shared_ptr<AST>
stripDocString(shared_ptr<AST> exprSeq)
{
  if (exprSeq->children.size() > 1 &&
      exprSeq->child(0)->astType == at_stringLiteral)
    exprSeq->disown(0);

  return exprSeq;
}

%}

%pure-parser
 // All of the expected errors are shift/reduce errors in which the
 // lookahead token is '(' or '[' and the correct action is to shift.
%expect 7
%parse-param {BlockLexer *lexer}

%token <tok> tk_Reserved	/* reserved words */

/* Categorical terminals: */
%token <tok> tk_Ident
%token <tok> tk_TypeVar
%token <tok> tk_EffectVar
%token <tok> tk_Int
%token <tok> tk_Float
%token <tok> tk_Char
%token <tok> tk_String

%token <tok> tk_SIZEOF
%token <tok> tk_EQUALS
%token <tok> tk_NOTEQUALS
%token <tok> tk_LE tk_GE '<' '>'
%token <tok> tk_FNARROW
%token <tok> tk_GETS

/* Primary types and associated hand-recognized literals: */
%token <tok> '(' ')' ','	/* unit */
%token <tok> '[' ']'	/* unit */
%token <tok> tk_AS
%token <tok> tk_UNIT
%token <tok> tk_BOOL
%token <tok> tk_TRUE   /* #t */
%token <tok> tk_FALSE  /* #f */
%token <tok> tk_CHAR
%token <tok> tk_STRING
%token <tok> tk_FLOAT
%token <tok> tk_DOUBLE
%token <tok> tk_DUP
%token <tok> tk_INT8
%token <tok> tk_INT16
%token <tok> tk_INT32
%token <tok> tk_INT64
%token <tok> tk_UINT8
%token <tok> tk_UINT16
%token <tok> tk_UINT32
%token <tok> tk_UINT64
%token <tok> tk_WORD

%token <tok> tk_BITFIELD
%token <tok> tk_FILL
%token <tok> tk_RESERVED // not to be confused with tk_Reserved
%token <tok> tk_WHERE

%token <tok> tk_BITC_VERSION

%token <tok> tk_PURE
%token <tok> tk_IMPURE
%token <tok> tk_CONST

%token <tok> tk_THE
%token <tok> tk_IF
%token <tok> tk_WHEN
%token <tok> tk_AND
%token <tok> tk_OR
%token <tok> tk_NOT
%token <tok> tk_COND
%token <tok> tk_SWITCH
%token <tok> tk_CASE
%token <tok> tk_OTHERWISE

%token <tok> tk_BLOCK tk_RETURN_FROM
%token <tok> tk_RETURN tk_CONTINUE

%token <tok> tk_PAIR
%token <tok> tk_VECTOR
%token <tok> tk_ARRAY
 //%token <tok> tk_MAKE_VECTOR
%token <tok> tk_MAKE_VECTORL
%token <tok> tk_ARRAY_LENGTH
%token <tok> tk_VECTOR_LENGTH
%token <tok> tk_ARRAY_NTH
%token <tok> tk_VECTOR_NTH

%token <tok> tk_BY_REF
%token <tok> tk_REF
%token <tok> tk_VAL

%token <tok> tk_SET
%token <tok> tk_TAG

%token <tok> tk_DEREF
%token <tok> tk_LAMBDA

%token <tok> tk_DEF
%token <tok> tk_STRUCT
%token <tok> tk_UNION
%token <tok> tk_REPR
%token <tok> tk_EXCEPTION
%token <tok> tk_TYPECLASS
%token <tok> tk_INSTANCE

%token <tok> tk_TRY
%token <tok> tk_CATCH
%token <tok> tk_THROW

%token <tok> tk_DO
%token <tok> tk_WHILE

%token <tok> tk_METHOD
%token <tok> tk_FORALL

%token <tok> tk_END
%token <tok> tk_IN

%token <tok> tk_LET
%token <tok> tk_LETREC
%token <tok> tk_OPAQUE
%token <tok> tk_DECLARE
%token <tok> tk_MUTABLE
%token <tok> tk_EXTERNAL
%token <tok> tk_INTERFACE
%token <tok> tk_MODULE
// token tk_USESEL historic significance only
// %token <tok> tk_USESEL
%token <tok> tk_PACKAGE
%token <tok> tk_IMPORT
%token <tok> tk_PROVIDE

%token <tok> tk_TYFN
//%token <tok> tk_SUPER
%token <tok> tk_SUSPEND

 //%type <ast> module
%type <ast> interface module
%type <ast> if_definitions if_definition
%type <ast> mod_definitions mod_definition
%type <ast> provide_definition provideList
%type <ast> common_definition type_val_definition value_declaration
%type <ast> constrained_definition
%type <ast> constraints constraint
%type <ast> tc_definition
%type <ast> tc_decls tc_decl
%type <ast> method_decls method_decl
%type <ast> ti_definition
%type <ast> method_bindings method_binding
%type <ast> import_definition importList
%type <ast> type_definition type_decl externals
%type <ast> value_definition
%type <ast> defpattern
%type <ast> lambdapatterns lambdapattern
%type <ast> types type qual_type
%type <ast> bool_type
%type <ast> fntype fneffect
 //%type <ast> bitfieldtype
%type <ast> literal
%type <ast> typevar
%type <ast> type_pl_byref types_pl_byref
%type <ast> type_or_bitfield bitfieldtype
%type <ast> type_cpair typeapp
%type <ast> int_type uint_type any_int_type float_type
%type <ast> declares // declare
 //%type <ast> decls decl
%type <ast> alias
%type <ast> ident defident useident
%type <ast> exident
%type <tok> ifident
%type <ast> intLit floatLit charlit strLit boolLit
%type <ast> docstring optdocstring
%type <ast> ptype_name val
%type <ast> tvlist
%type <ast> fields field
%type <ast> constructor constructors
%type <ast> repr_constructor repr_constructors
%type <ast> repr_reprs repr_repr
%type <ast> expr_seq arg_exprs
%type <ast> expr block
%type <ast> primary_expr postfix_expr prefix_expr mul_expr add_expr
%type <ast> shift_expr inequality_expr equality_expr
%type <ast> bitand_expr bitxor_expr bitor_expr and_expr or_expr
%type <ast> assign_expr

%%

// Parser built for version 0.9
// Section Numbers indicated within []

// COMPILATION UNITS [2.5]
// This definition od start mus be changed as it ignores
// junk after the body.

start: version uoc_body {
  SHOWPARSE("start -> version uoc_body");
  return 0;
};

start: uoc_body {
  SHOWPARSE("start -> uoc_body");
  return 0;
};

// VERSION [2.5]

// We cannot do optversion, because that would require two token look-ahead.
version: tk_BITC_VERSION strLit ';' {
  SHOWPARSE("version -> BITC-VERSION strLit ;");
  if (!CheckVersionCompatibility($2->s)) {
    std::string s = ": Warning: BitC version conflict " + $2->s + " vs " + Version();
    lexer->ReportParseWarning($2->loc, s);
  }
};

docstring: tk_String {
  SHOWPARSE("docstring -> STRING");
  $$ = AST::make(at_docString, $1.loc, AST::makeStringLit($1));
};
optdocstring: docstring {
  SHOWPARSE("optdocstring -> docstring");
  $$ = $1;
};
optdocstring: {
  SHOWPARSE("optdocstring -> ");
  $$ = AST::make(at_docString);
};

uoc_body: interface {
  SHOWPARSE("uocbody -> interface");
}

uoc_body: module_seq {
  SHOWPARSE("uocbody -> module_seq");
}

interface: tk_INTERFACE ifident '{' {
    if ($2.str.find("bitc.") == 0)
      lexer->isRuntimeUoc = true;
  }
  optdocstring if_definitions '}' {
  SHOWPARSE("interface -> INTERFACE ifident optdocstring if_definitions END");
  shared_ptr<AST> ifIdent = AST::make(at_ident, $2);
  $$ = AST::make(at_interface, $1.loc, ifIdent);
  $$->addChildrenFrom($6);

  if (lexer->isCommandLineInput) {
    const char *s =
      ": Warning: interface units of compilation should no longer\n"
      "    be given on the command line.\n";
    lexer->ReportParseWarning($$->loc, s);
  }

  std::string uocName = ifIdent->s;
  shared_ptr<UocInfo> uoc =
    UocInfo::make(uocName, lexer->here.origin, $$);

  if (uocName == "bitc.prelude")
    uoc->flags |= UOC_IS_PRELUDE;

  shared_ptr<UocInfo> existingUoc = UocInfo::findInterface(uocName);

  if (existingUoc) {
    std::string s = "Error: This interface has already been loaded from "
      + existingUoc->uocAst->loc.asString();
    lexer->ReportParseError($$->loc, s);

  }
  else  {
    /* Put the UoC onto the interface list so that we do not recurse on
       import. */
    UocInfo::ifList[uocName] = uoc;
  }

  // Regardless, compile the new interface to check for further
  // warnings and/or errors:
  uoc->Compile();
 }

ifident: {
    lexer->setIfIdentMode(true);
  } ident {
  lexer->setIfIdentMode(false);
  $$ = LToken($2->loc, $2->s);
};
ifident: ifident '.' {
    lexer->setIfIdentMode(true);
  } tk_Ident {
  lexer->setIfIdentMode(false);
  $$ = LToken($1.loc, $1.str + "." + $4.str);
};

module: tk_MODULE optdocstring '{' mod_definitions '}' {
 SHOWPARSE("module -> tk_MODULE optdocstring { mod_definitions }");
 $$ = $4;
 $$->astType = at_module;

 string uocName = 
   UocInfo::UocNameFromSrcName(lexer->here.origin, lexer->nModules);

 // Construct, compile, and admit the parsed UoC:
 shared_ptr<UocInfo> uoc = UocInfo::make(uocName, lexer->here.origin, $$);
 lexer->nModules++;
 uoc->Compile();
 UocInfo::srcList[uocName] = uoc;
};

module: tk_MODULE ifident optdocstring '{' mod_definitions '}' {
 SHOWPARSE("module -> ( tk_MODULE ifident optdocstring { mod_definitions }");
 $$ = $5;
 $$->astType = at_module;

 // Construct, compile, and admit the parsed UoC.
 // Note that we do not even consider the user-provided module name
 // for purposes of internal naming, because it is not significant.
 string uocName = 
   UocInfo::UocNameFromSrcName(lexer->here.origin, lexer->nModules);

 shared_ptr<UocInfo> uoc = UocInfo::make(uocName, lexer->here.origin, $$);
 lexer->nModules++;
 uoc->Compile();
 UocInfo::srcList[uocName] = uoc;
};

module_seq: module {
 SHOWPARSE("module_seq -> module");
}

module_seq: module_seq module {
 SHOWPARSE("module_seq -> module_seq module");
}

// INTERFACE TOP LEVEL DEFINITIONS
if_definitions: if_definition {
  SHOWPARSE("if_definitions -> if_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

if_definitions: if_definitions if_definition {
  SHOWPARSE("if_definitions -> if_definitions if_definition");
  $$ = $1;
  $$->addChild($2);
};

if_definition: common_definition {
  SHOWPARSE("if_definition -> common_definition");
  $$ = $1;
};

mod_definitions: mod_definition {
  SHOWPARSE("mod_definitions -> mod_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

mod_definitions: mod_definitions mod_definition {
  SHOWPARSE("mod_definitions -> mod_definitions mod_definition");
  $$ = $1;
  $$->addChild($2);   
};

mod_definition: provide_definition {
  SHOWPARSE("mod_definition -> provide_definition");
  $$ = $1;
};
mod_definition: common_definition {
  SHOWPARSE("mod_definition -> common_definition");
  $$ = $1;
};

// TOP LEVEL DEFINITIONS [2.5.1]
constrained_definition: tk_FORALL constraints type_val_definition {
  // HACK ALERT!!! For reasons of ancient history, there is no
  // at_forall AST. Instead, all of the type_val_definition ASTs have
  // their constraints tacked on at the end. This is rather badly
  // glitched and we need to fix it, but the immediate goal was to
  // move the (forall ...) syntax to the outside without doing major
  // surgery on the compiler internals.

  uint32_t nChildren = $3->children.size();

  shared_ptr<AST> tvConstraints= $3->child(nChildren-1);
  assert(tvConstraints->astType == at_constraints);
  tvConstraints->addChildrenFrom($2);
  $$ = $3;
};

common_definition: import_definition {
  SHOWPARSE("common_definition -> import_definition");
  $$ = $1;
};

common_definition: type_val_definition {
  SHOWPARSE("common_definition -> type_val_definition");
  $$ = $1;
};

common_definition: constrained_definition {
  SHOWPARSE("common_definition -> constrained_definition");
  $$ = $1;
};

type_val_definition: type_definition {
  SHOWPARSE("common_definition -> type_definition");
  $$ = $1;
};

type_val_definition: type_decl {
  SHOWPARSE("common_definition -> type_decl");
  $$ = $1;
};

type_val_definition: value_definition {
  SHOWPARSE("common_definition -> type_decl");
  $$ = $1;
};

type_val_definition: value_declaration {
  SHOWPARSE("type_val_definition -> value_declaration");
  $$ = $1;
};

type_val_definition: tc_definition {
  SHOWPARSE("type_val_definition -> tc_definition");
  $$ = $1;
};

type_val_definition: ti_definition {
  SHOWPARSE("type_val_definition -> ti_definition");
  $$ = $1;
};

import_definition: tk_IMPORT ifident tk_AS ident ';' {
  SHOWPARSE("import_definition -> IMPORT ident ifident ;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_importAs, $1.loc, ifIdent, $4);
};

import_definition: tk_IMPORT ifident ';' {
  SHOWPARSE("import_definition -> IMPORT ifident ';'");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_import, $1.loc, ifIdent);
};

import_definition: tk_IMPORT ifident importList ';' {
  SHOWPARSE("import_definition -> IMPORT ifident importList ';");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_import, $1.loc, ifIdent);
  $$->addChildrenFrom($3);
};

importList: alias {
  SHOWPARSE("importList -> alias");
  $$ = AST::make(at_Null, $1->loc, $1);
};
importList: importList ',' alias {
  SHOWPARSE("importList -> importList alias");
  $$ = $1;
  $$->addChild($3);
};

alias: ident {
  SHOWPARSE("alias -> ident");
  // The two identifiers in this case are textually the same, but the
  // need to end up with distinct AST nodes, thus getDCopy().
  $$ = AST::make(at_ifsel, $1->loc, $1, $1->getDeepCopy());
};
alias: ident tk_AS ident {
  SHOWPARSE("alias -> ( ident AS ident )");

  $$ = AST::make(at_ifsel, $1->loc, $3, $1);
};

// PROVIDE DEFINITION [8.3]
provide_definition: tk_PROVIDE ifident provideList ';' {
  SHOWPARSE("provide_definition -> PROVIDE ifident provideList)");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_provide, $1.loc, ifIdent); 
  $$->addChildrenFrom($3);
};

provideList: ident {
  SHOWPARSE("provideList -> ident");
  $$ = AST::make(at_Null, $1->loc, $1);
};

provideList: provideList ',' ident {
  SHOWPARSE("provideList -> provideList , ident");
  $$ = $1;
  $$->addChild($3);
};

/* Literals and Variables */
// INTEGER LITERALS [2.4.1]
literal: boolLit {
  SHOWPARSE("literal -> boolLit");
  $$ = $1;
};
literal: intLit {
  SHOWPARSE("literal -> intLit");
  $$ = $1;
};
// FLOATING POINT LITERALS [2.4.2]
literal: floatLit {
  SHOWPARSE("literal -> floatLit");
  $$ = $1;
};
// CHARACTER LITERALS [2.4.3]
literal: charlit {
  SHOWPARSE("literal -> Charlit");
  $$ = $1;
};
// STRING LITERALS [2.4.4]
literal: strLit {
  SHOWPARSE("literal -> strLit");
  $$ = $1;
};

// External identifiers are not subject to reserved word restrictions...
exident: tk_Ident {
  SHOWPARSE("exident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

exident: tk_Reserved {
  SHOWPARSE("exident -> <Reserved " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

// Typeclass Constraint Declarations

constraints: constraint {
 SHOWPARSE("constraints -> constraint");  
 $$ = AST::make(at_constraints, $1->loc, $1);
};

constraints: constraints ',' constraint {
 SHOWPARSE("constraints -> constraints , constraint");  
 $$ = $1;
 $$->addChild($3);
};

constraint: typeapp {
 SHOWPARSE("constraint -> typeapp");  
 $1->astType = at_tcapp;
 $$ = $1;
};

// Type classes can have zero type variables, and serve in that case
// as enable/disable controls
constraint: useident {
 SHOWPARSE("constraint -> useident");  
 $$ = AST::make(at_tcapp, $1->loc, $1); 
};

// FIX: This should probably get its own AST type.
ptype_name: defident {
  SHOWPARSE("ptype_name -> defident");
  shared_ptr<AST> tvlist = AST::make(at_tvlist, $1->loc);
  shared_ptr<AST> constraints = AST::make(at_constraints, $1->loc);
  $$ = AST::make(at_Null, $1->loc, $1, tvlist, constraints);
};

ptype_name: defident '(' tvlist ')' {
  SHOWPARSE("ptype_name -> defident '(' tvlist ')'");
  shared_ptr<AST> constraints = AST::make(at_constraints, $1->loc);
  $$ = AST::make(at_Null, $1->loc, $1, $3, constraints);
};

// STRUCTURE TYPES [3.6.1]
type_definition: tk_STRUCT ptype_name val optdocstring declares '{' fields '}' ';' {
  SHOWPARSE("type_definition -> STRUCT ptype_name val "
	    "optdocstring '{' declares fields '}' ';'");
  $$ = AST::make(at_defstruct, $1.loc, $2->child(0), $2->child(1), $3,
	       $5, $7);
  $$->child(0)->defForm = $$;
  $$->addChild($2->child(2));
};

// UNION TYPES [3.6.2]
type_definition: tk_UNION ptype_name val optdocstring declares '{' constructors '}' ';'  {
  SHOWPARSE("type_definition -> STRUCT ptype_name val "
	    "optdocstring '{' declares fields '}' ';'");
  $$ = AST::make(at_defunion, $1.loc, $2->child(0), $2->child(1), $3,
		 $5, $7);
  $$->child(0)->defForm = $$;
  $$->addChild($2->child(2));
};

/* defunion Constructors */
constructors: constructor {
  SHOWPARSE("constructors -> constructor");
  $$ = AST::make(at_constructors, $1->loc, $1);
};
constructors: constructors constructor {
  SHOWPARSE("constructors -> constructors constructor");
  $$ = $1;
  $$->addChild($2);
};
constructor: ident { 	       	  /* simple constructor */ 
  SHOWPARSE("constructor -> defident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
};
constructor: ident '{' fields '}' ';' {  /* compound constructor */ 
  SHOWPARSE("constructor ->  ident '{' fields '}' ';'");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
  $$->addChildrenFrom($3);
};

// REPR TYPES
type_definition: tk_REPR defident val optdocstring declares '{' repr_constructors '}' ';' {
  SHOWPARSE("type_definition -> REPR defident val optdocstring "
	    "'{' declares repr_constructors '}' ';'");
  $$ = AST::make(at_defrepr, $1.loc, $2, $3, $5, $7);
  $$->child(0)->defForm = $$;
};

/* defrepr Constructors */
repr_constructors: repr_constructor {
  SHOWPARSE("repr_constructors -> repr_constructor");
  $$ = AST::make(at_reprctrs, $1->loc, $1);
};
repr_constructors: repr_constructors repr_constructor {
  SHOWPARSE("repr_constructors -> repr_constructors repr_constructor");
  $$ = $1;
  $$->addChild($2);
};
/* repr_constructor: ident repr_reprs { 	       	  /\* simple constructor *\/  */
/*   SHOWPARSE("repr_constructor -> defident"); */
/*   $1->flags |= (ID_IS_GLOBAL); */
/*   $$ = AST::make(at_reprctr, $1->loc, $1); */
/* }; */
repr_constructor: ident '{' fields '}' tk_WHERE repr_reprs ';' {  /* compound constructor */ 
  SHOWPARSE("repr_constructor ->  ( ident fields ( WHERE repr_reprs ) )");
  $1->flags |= (ID_IS_GLOBAL);
  shared_ptr<AST> ctr = AST::make(at_constructor, $1->loc, $1);
  ctr->addChildrenFrom($3);
  $$ = AST::make(at_reprctr, $1->loc, ctr);
  $$->addChildrenFrom($6);
};

repr_reprs: repr_repr {
  SHOWPARSE("repr_reprs -> repr_repr");
  $$ = AST::make(at_Null, $1->loc, $1);
};
repr_reprs: repr_reprs ',' repr_repr {
  SHOWPARSE("repr_reprs -> repr_reprs , repr_repr");
  $$ = $1;
  $$->addChild($3);
};
repr_repr: ident tk_EQUALS intLit {
  SHOWPARSE("repr_repr ->  ident = intLit");

  $$ = AST::make(at_reprrepr, $1->loc, $1, $3);
};

// EXCEPTION DEFINITION [3.10]
type_definition: tk_EXCEPTION ident optdocstring '{' '}' ';'{
  SHOWPARSE("type_definition -> exception ident ';'");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $1.loc, $2);
  $$->child(0)->defForm = $$;
};

type_definition: tk_EXCEPTION ident optdocstring '{' fields '}' ';' {
  SHOWPARSE("type_definition -> exception ident '{' fields '}' ';'");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $1.loc, $2);
  $$->child(0)->defForm = $$;
  $$->addChildrenFrom($5);
};

declares: {
  $$ = GC_NULL;
};

// TYPE CLASSES [4]
// TYPE CLASS DEFINITION [4.1]

tc_definition: tk_TYPECLASS ptype_name optdocstring tc_decls '{' method_decls '}' ';' {
  SHOWPARSE("tc_definition -> ( TYPECLASS ptype_name optdocstring tc_decls { method_decls } ;");
  $$ = AST::make(at_deftypeclass, $1.loc, $2->child(0), 
	       $2->child(1), $4, $6, $2->child(2));  
  $$->child(0)->defForm = $$;
};

tc_decls: {
  SHOWPARSE("tcdecls -> <empty>");
  $$ = AST::make(at_tcdecls);
};

tc_decls: tc_decls ',' tc_decl {
  SHOWPARSE("tcdecls -> tcdelcs , tcdecl");
  $$ = $1;
  $$->addChild($3);
};

tc_decl: '(' tvlist ')' tk_FNARROW typevar {
  //         ^^^^^^
  // I really mean tvlist here, arbitraty types
  // are not accepptable.
  SHOWPARSE("tc_decl -> ( tvlist ) -> typevar");
  $2->astType = at_fnargVec;
  $$ = AST::make(at_tyfn, $1.loc, $2, $5);  
};

method_decls: /* Nothing */ {
  SHOWPARSE("method_decls -> ");
  LexLoc loc;
  $$ = AST::make(at_method_decls, loc);
};

method_decls: method_decls method_decl {
  SHOWPARSE("method_decls -> method_decls method_decl");
  $$ = $1;
  $$->addChild($2);
};

method_decl: fntype ident ';' {
  SHOWPARSE("method_decl -> fntype ident ;");
  $2->flags |= ID_IS_GLOBAL;
  $2->identType = id_tcmethod;
  $$ = AST::make(at_method_decl, $2->loc, $2, $1);
};

// TYPE CLASS INSTANTIATIONS [4.2]
ti_definition: tk_INSTANCE constraint optdocstring ';' {
  SHOWPARSE("ti_definition -> INSTANCE constraint [docstring] ;");

  // Constraints will be inserted under the empty constraints node (if
  // appropriate) from above in the constrained_definition case.
  shared_ptr<AST> constrs = AST::make(at_constraints, $2->loc);
  $$ = AST::make(at_definstance, $1.loc, $2,
		 AST::make(at_tcmethods, $1.loc),
		 constrs);  
};
ti_definition: tk_INSTANCE constraint optdocstring '{' method_bindings '}' ';' {
  SHOWPARSE("ti_definition -> INSTANCE constraint [docstring] method_seq ;");

  // Constraints will be inserted under the empty constraints node (if
  // appropriate) from above in the constrained_definition case.
  shared_ptr<AST> constrs = AST::make(at_constraints, $2->loc);
  $$ = AST::make(at_definstance, $1.loc, $2, $5, 
		 constrs);  
};

method_bindings: method_binding {
  SHOWPARSE("method_bindings -> method_binding");
  $$ = AST::make(at_tcmethods, $1->loc, $1);
};

method_bindings: method_bindings ',' method_binding {
  SHOWPARSE("method_bindings -> method_bindings , method_binding");
  $$ = $1;
  $$->addChild($3);
};

method_binding: ident '=' expr {
  SHOWPARSE("method_binding -> ident = expr");
  
  $$ = AST::make(at_tcmethod_binding, $1->loc, $1, $3);
};

// Type Declarations
// External declarations
externals: /* nothing */ {
  SHOWPARSE("externals -> ");
  $$ = AST::make(at_Null);
  $$->flags = NO_FLAGS;
};

externals: tk_EXTERNAL {
  SHOWPARSE("externals -> EXTERNAL");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;
};

externals: tk_EXTERNAL exident {
  SHOWPARSE("externals -> EXTERNAL exident");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;  
  $$->externalName = $2->s;
};

// STRUCTURE DECLARATIONS
type_decl: tk_STRUCT ptype_name val externals ';' {
  SHOWPARSE("type_decl -> STRUCT ptype_name val externals ;");
  $$ = AST::make(at_declstruct, $1.loc, $2->child(0), $2->child(1), $3,
	       $2->child(2));
  $$->child(0)->defForm = $$;
  $$->flags |= $4->flags;
  $$->getID()->flags |= $4->flags;
  $$->getID()->externalName = $4->externalName;
};

// UNION DECLARATIONS
type_decl: tk_UNION ptype_name val externals ';' {
  SHOWPARSE("type_decl -> UNION ptype_name val ;");
  $$ = AST::make(at_declunion, $1.loc, $2->child(0), $2->child(1), $3,
	       $2->child(2));
  $$->child(0)->defForm = $$;
  $$->flags |= $4->flags;
  $$->getID()->flags |= $4->flags;
  $$->getID()->externalName = $4->externalName;
};

// REPR DECLARATIONS
type_decl: tk_REPR defident val externals ';' {
  SHOWPARSE("type_decl -> REPR defident val externals ;");
  $$ = AST::make(at_declrepr, $1.loc, $2, $3);
  $$->child(0)->defForm = $$;
  $$->flags |= $4->flags;
  $$->getID()->flags |= $4->flags;
  $$->getID()->externalName = $4->externalName;
};

// FIELDS
/* defstruct / constructor / exception fields */
fields: field  {
  SHOWPARSE("fields -> field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

fields: fields field {
  SHOWPARSE("fields -> fields field ");
  $$ = $1;
  $$->addChild($2);
};

field: type_or_bitfield ident ';' {
  SHOWPARSE("field -> type_or_bitfield ident ;");
  $$ = AST::make(at_field, $1->loc, $2, $1);
}

field: bitfieldtype tk_FILL ';' {
  SHOWPARSE("field -> bitfieldtype FILL ;");
  $$ = AST::make(at_fill, $1->loc, $1);
};

//field: '(' tk_RESERVED bitfieldtype intLit ')'  {
//  SHOWPARSE("field -> '(' RESERVED bitfieldtype intLit ')'");
//  $$ = AST::make(at_fill, $1.loc, $3);
//};

// CATEGORIES

val: {
  SHOWPARSE("val -> <empty>");
  $$ = AST::make(at_refCat);
  $$->printVariant = pf_IMPLIED;
};

val: ':' tk_VAL {
  SHOWPARSE("val -> ':' VAL");
  $$ = AST::make(at_valCat, $2);
};
val: ':' tk_OPAQUE {
  SHOWPARSE("val -> ':' OPAQUE");
  $$ = AST::make(at_opaqueCat, $2);
};
val: ':' tk_REF {
  /* Same as :ref, since that is the default. */
  SHOWPARSE("val -> ':' REF");
  $$ = AST::make(at_refCat, $2);
};

// There are no defpattern sequences, because there is no top-level
// pattern application 
// DEFPATTERN
defpattern: defident {
  SHOWPARSE("defpattern -> defident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
defpattern: qual_type defident {
  SHOWPARSE("defpattern -> qual_type defident");
  $$ = AST::make(at_identPattern, $1->loc, $2, $1);
};

// DEFINE  [5.1]
value_definition: tk_DEF defpattern '=' expr ';'  {
  SHOWPARSE("value_definition -> DEF defpattern = expr ;");
  $$ = AST::make(at_define, $1.loc, $2, $4);
  $$->addChild(AST::make(at_constraints));
};

// Define convenience syntax case 1: no arguments
// FIX: Issue with function types
value_definition: tk_DEF defident '(' ')' optdocstring block {
  SHOWPARSE("value_definition -> DEF defident () optdocstring { expr_seq }");
  shared_ptr<AST> iRetBlock = 
    AST::make(at_block, $1.loc, AST::make(at_ident, LToken("__return")), $6);
  shared_ptr<AST> iLambda =
    AST::make(at_lambda, $1.loc, AST::make(at_argVec, $4.loc), iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $2->loc, $2);
  $$ = AST::make(at_recdef, $1.loc, iP, iLambda);
  $$->addChild(AST::make(at_constraints));
};

value_definition: tk_DEF defident '(' lambdapatterns ')' optdocstring
                  block {
  SHOWPARSE("value_definition -> DEF defident ( lambdapatterns ) optdocstring "
	    "{ expr_seq }");
  shared_ptr<AST> iRetBlock = 
    AST::make(at_block, $1.loc, AST::make(at_ident, LToken("__return")), $7);
  shared_ptr<AST> iLambda = AST::make(at_lambda, $1.loc, $4, iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $2->loc, $2);
  $$ = AST::make(at_recdef, $1.loc, iP, iLambda);
  $$->addChild(AST::make(at_constraints));
};

// PROCLAIM DEFINITION -- VALUES [6.2]
value_declaration: tk_DECLARE qual_type defident optdocstring externals ';' {
  SHOWPARSE("if_definition -> DECLARE qual_type ident optdocstring externals ;");
  $$ = AST::make(at_proclaim, $1.loc, $3, $2);
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
  $$->addChild(AST::make(at_constraints));
};

/* Lambda Patterns -- with an additional by-ref annotation */
lambdapatterns: lambdapattern {
  SHOWPARSE("lambdapatterns -> lambdapattern");
  $$ = AST::make(at_argVec, $1->loc);
  $$->addChild($1);
};
lambdapatterns: lambdapatterns ',' lambdapattern {
  SHOWPARSE("lambdapatterns -> lambdapatterns , lambdapattern");
  $$ = $1;
  $$->addChild($3);
};

lambdapattern: ident {
  SHOWPARSE("lambdapattern -> ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};

lambdapattern: type_pl_byref ident {
  SHOWPARSE("lambdapattern -> type_pl_byref ident");
  $$ = AST::make(at_identPattern, $1->loc, $2, $1);
  if ($1->astType == at_byrefType)
    $2->flags |= ARG_BYREF;
};

// EXPRESSIONS [7]
//
// In the block syntax, the expression sub-grammar must deal with
// operator precedence. This is a horrible mess, and I would really
// like to replace it with something more flexible and extensible. In
// Bison/Yacc, it appears that the only way to accomplish that would
// be to accept expressions as a linear sequence of tokens, and then
// do the precedence and associativity processing in the expression
// reduce action (which is moderately icky). At the moment, I am *not*
// actually doing this.
//
// Operator precedence rules for the C:
//    HIGHEST                TOKEN           ASSOCIATION
//    ----------------
//    funcall                ()              left-to-right
//    array ref              ([])
//    field select           (.)
//    [arrow field select]   (->)
//    [post inc/dec]         (++,--)
//    ----------------
//    [pre inc/dec]          (++,--)         right-to-left
//    unary +/-              (+,-)           
//    bool/bit negate        (!,~)
//    [cast]                 [parens]
//    [dereference]          *
//    ----------------
//    (binary) mul/div/mod   (*,/,%)         left-to-right
//    (binary) add/sub       (+, -)          left-to-right
//    [bit shift]            (<<, >>)        left-to-right
//    ne compare             (<, >, <=, >=)  left-to-right
//    eql compare            (==, !=)        left-to-right
//    bitwise AND            (&)             left-to-right
//    bitwise XOR            (^)             left-to-right    [*]
//    bitwise OR             (|)             left-to-right
//    logical AND            (&&)            right-to-left
//    logical OR             (||)            right-to-left
//    [ternary conditional]
//    ----------------
//    assignment             (=)             left-to-right
//    [updating ops]         (+=, friends)
//    ----------------
//    LOWEST
//
// Is that sufficiently horrible yet?
//
// In BitC, there is an issue that many of these operators are members
// of type classes. To avoid recognizing them specially in the
// applicable defining contexts, we treat the punctuation-style
// operators as short-hands for corresponding procedures that have
// conventional names.
//
// In BitC, the applicable operator precedence rules are:
//
//    HIGHEST                TOKEN           ASSOCIATION
//    ----------------
// postfix_expr:
//    funcall                ()              left-to-right    SYNTAX
//    array ref              ([])                             SYNTAX
//    field select           (.)                              SYNTAX
//    ----------------
// prefix_expr:
//    unary +/-              (+,-)           right-to-left    [neg]
//    bool negate            (!)                              [not]
//    bit negate             (~)                              [bit-negate]
//    type qualification     [parens]                         SYNTAX
//    [dereference]          *                                SYNTAX
//    ----------------
// mul_expr:
//    (binary) mul/div/mod   (*,/,%)         left-to-right    [mul, div, mod]
// add_expr:
//    (binary) add/sub       (+, -)          left-to-right    [add, sub]
// shift_expr:
//    [bit shift]            (<<, >>)        left-to-right    [lshift,rshift]
// inequality_expr:
//    ne compare             (<, >, <=, >=)  left-to-right    [lt, gt, le, ge]
// equality_expr:
//    eql compare            (==, !=)        left-to-right    [equal, not-equal]
// bitand_expr:
//    bitwise AND            (&)             left-to-right    [bit-and]
// bitxor_expr:
//    bitwise XOR            (^)             left-to-right    [bit-xor]
// bitor_expr:
//    bitwise OR             (|)             left-to-right    [bit-and]
// and_expr:
//    logical AND            (&&)            right-to-left    SYNTAX
// or_expr:
//    logical OR             (||)            right-to-left    SYNTAX
//    ----------------
// assign_expr:
//    assignment             (=)             left-to-right    SYNTAX
//    ----------------
//    LOWEST
//
// Rule: when dealing with precedence in a grammar, higher rules
// appear "inside" lower rules
//
// As a practical matter, every expression on the RHS of a production
// should be an expr

// QUASI-BLOCK
block: '{' expr_seq '}' {
  SHOWPARSE("block -> { expr_seq }");
  $$ = $2;
};
expr_seq: expr ';' {
  SHOWPARSE("expr_seq -> expr ;");
  $$ = AST::make(at_begin, $1->loc, $1);
}
expr_seq: value_definition {
  SHOWPARSE("expr_seq -> value_definition");
  $$ = AST::make(at_begin, $1->loc, $1);
  $$->printVariant;
};
expr_seq: expr_seq expr ';' {
  SHOWPARSE("expr_seq -> expr_seq expr ;");
  $$ = $1;
  $$->addChild($2);
}
expr_seq: expr_seq value_definition {
  SHOWPARSE("expr_seq -> expr_seq value_definition");
  $$ = $1;
  $$->addChild($2);
}

// PRIMARY EXPRESSIONS
primary_expr: ident {
  SHOWPARSE("primary_expr -> ident");
  $$ = $1;
};

// LITERALS  [7.1]
primary_expr: literal {
  SHOWPARSE("primary_expr -> Literal");
  $$ = $1;
};

primary_expr: '(' expr ')' {
  SHOWPARSE("primary_expr -> ( expr )");
  $$ = $2;
};

primary_expr: block {
  SHOWPARSE("primary_expr -> block");
  $$ = $1;
};

arg_exprs: expr {
  SHOWPARSE("arg_exprs -> expr");
  $$ = AST::make(at_Null, $1->loc, $1);
  $$->addChild($1);
}
arg_exprs: arg_exprs ',' expr {
  SHOWPARSE("arg_exprs -> arg_exprs , expr");
  $$ = $1;
  $$->addChild($3);
}

postfix_expr: primary_expr {
  SHOWPARSE("postfix_expr -> primary_expr");
  $$ = $1;
}
// APPLICATION [7.14]          
postfix_expr: postfix_expr '(' ')' {
  SHOWPARSE("postfix_expr -> postfix_expr ( )");
  $$ = AST::make(at_apply, $1->loc, $1);
}
postfix_expr: postfix_expr '(' arg_exprs ')' {
  SHOWPARSE("postfix_expr -> postfix_expr ( )");
  $$ = AST::make(at_apply, $1->loc, $1);
  $$->addChildrenFrom($3);
}
// NTH-REF [7.11.2]          
postfix_expr: postfix_expr '[' expr ']' {
  SHOWPARSE("postfix_expr -> postfix_expr [ expr ]");
  $$ = AST::make(at_vector_nth, $1->loc, $1, $3);
};
postfix_expr: tk_ARRAY_NTH '(' expr ',' expr ')' {
  SHOWPARSE("eform -> ARRAY-NTH ( expr , expr )");
  $$ = AST::make(at_array_nth, $1.loc, $3, $5);
};
postfix_expr: tk_VECTOR_NTH '(' expr ',' expr ')' {
  SHOWPARSE("eform -> VECTOR-NTH ( expr , expr )");
  $$ = AST::make(at_vector_nth, $1.loc, $3, $5);
};
// MEMBER [7.9]
postfix_expr: postfix_expr '.' ident {
  SHOWPARSE("postfix_expr -> postfix_expr . ident");
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

prefix_expr: postfix_expr {
  SHOWPARSE("prefix_expr -> postfix_expr");
  $$ = $1;
};

prefix_expr: '+' prefix_expr {
  SHOWPARSE("prefix_expr -> '+' prefix_expr");
  $$ = $2;
};
prefix_expr: '-' prefix_expr {
  SHOWPARSE("prefix_expr -> '-' prefix_expr");
  // FIX!
  $$ = $2;
};
prefix_expr: tk_SIZEOF '(' expr ')' {
  SHOWPARSE("prefix_expr -> sizeof ( expr )");
  // FIX!
  $$ = $3;
};
//prefix_expr: tk_SIZEOF '(' type ')' {
//  SHOWPARSE("prefix_expr -> sizeof ( type )");
//  // FIX!
//  $$ = $2;
//};

mul_expr: prefix_expr { 
  SHOWPARSE("mul_expr -> prefix_expr");
  $$ = $1;
};
mul_expr: mul_expr '*' prefix_expr { 
  SHOWPARSE("mul_expr -> mul_expr '*' prefix_expr");
  // FIX!
  $$ = $1;
};
mul_expr: mul_expr '/' prefix_expr { 
  SHOWPARSE("mul_expr -> mul_expr '/' prefix_expr");
  // FIX!
  $$ = $1;
};
mul_expr: mul_expr '%' prefix_expr { 
  SHOWPARSE("mul_expr -> mul_expr '%' prefix_expr");
  // FIX!
  $$ = $1;
};
add_expr: mul_expr { 
  SHOWPARSE("add_expr -> mul_expr");
  $$ = $1;
};
add_expr: add_expr '+' mul_expr { 
  SHOWPARSE("add_expr -> add_expr '+' mful_expr");
  // FIX!
  $$ = $1;
};
add_expr: add_expr '-' mul_expr { 
  SHOWPARSE("add_expr -> add_expr '-' mful_expr");
  // FIX!
  $$ = $1;
};

shift_expr: add_expr { 
  SHOWPARSE("shift_expr -> add_expr");
  $$ = $1;
};
inequality_expr: shift_expr { 
  SHOWPARSE("inequality_expr -> shift_expr");
  // FIX!
  $$ = $1;
};
inequality_expr: inequality_expr '<' shift_expr { 
  SHOWPARSE("inequality_expr -> inequality_expr < shift_expr");
  // FIX!
  $$ = $1;
};
inequality_expr: inequality_expr '>' shift_expr { 
  SHOWPARSE("inequality_expr -> inequality_expr > shift_expr");
  // FIX!
  $$ = $1;
};
inequality_expr: inequality_expr tk_LE shift_expr { 
  SHOWPARSE("inequality_expr -> inequality_expr <= shift_expr");
  // FIX!
  $$ = $1;
};
inequality_expr: inequality_expr tk_GE shift_expr { 
  SHOWPARSE("inequality_expr -> inequality_expr >= shift_expr");
  // FIX!
  $$ = $1;
};
equality_expr: inequality_expr { 
  SHOWPARSE("equality_expr -> inequality_expr");
  $$ = $1;
};
equality_expr: equality_expr tk_EQUALS inequality_expr { 
  SHOWPARSE("equality_expr -> equality_expr == inequality_expr");
  // FIX!
  $$ = $1;
};
equality_expr: equality_expr tk_NOTEQUALS inequality_expr { 
  SHOWPARSE("equality_expr -> equality_expr != inequality_expr");
  // FIX!
  $$ = $1;
};

bitand_expr: equality_expr { 
  SHOWPARSE("bitand_expr -> equality_expr");
  $$ = $1;
};
bitand_expr: bitand_expr '&' equality_expr { 
  SHOWPARSE("bitand_expr -> bitand_expr & equality_expr");
  // FIX!
  $$ = $1;
};

bitxor_expr: bitand_expr { 
  SHOWPARSE("bitxor_expr -> bitand_expr");
  $$ = $1;
};
bitxor_expr: bitxor_expr '^' bitand_expr { 
  SHOWPARSE("bitxor_expr -> bitxor_expr ^ bitand_expr");
  // FIX!
  $$ = $1;
};

bitor_expr: bitxor_expr { 
  SHOWPARSE("bitor_expr -> bitxor_expr");
  $$ = $1;
};
bitor_expr: bitor_expr '|' bitxor_expr { 
  SHOWPARSE("bitor_expr -> bitor_expr | bitxor_expr");
  // FIX!
  $$ = $1;
};

and_expr: bitor_expr { 
  SHOWPARSE("and_expr -> bitor_expr");
  $$ = $1;
};
and_expr: and_expr tk_AND bitor_expr { 
  SHOWPARSE("and_expr -> and_expr && bitor_expr");
  // FIX!
  $$ = $1;
};

or_expr: and_expr { 
  SHOWPARSE("or_expr -> and_expr");
  $$ = $1;
};
or_expr: or_expr tk_OR and_expr { 
  SHOWPARSE("or_expr -> or_expr || and_expr");
  // FIX!
  $$ = $1;
};

assign_expr: or_expr { 
  SHOWPARSE("assign_expr -> or_expr");
  $$ = $1;
};
assign_expr: prefix_expr tk_GETS assign_expr { 
  // In C, the LHS can be a unary expresion, primarily to support LHS cast
  SHOWPARSE("assign_expr -> or_expr = assign_expr");
  // FIX!
  $$ = $1;
};

expr: assign_expr {
  SHOWPARSE("expr -> assign_expr");
  $$ = $1;
}

// TYPE QUALIFIED EXPRESSIONS  [7.3]
//expr: eform {
//  SHOWPARSE("expr -> eform");
//  $$ = $1;
//};

// N.B. This is NOT cast! This is type-qualified expression!
//expr: '(' type ')' eform {
//  SHOWPARSE("expr -> ( type ) eform");
//  $$ = AST::make(at_tqexpr, $1.loc, $4, $2);
//};

tvlist: typevar  {
  SHOWPARSE("tvlist -> typevar");
  $$ = AST::make(at_tvlist, $1->loc, $1);
};
tvlist: tvlist ',' typevar {
  SHOWPARSE("tvlist -> tvlist typevar");
  $$ = $1;
  $1->addChild($3);
};

// TYPES [3]
types: type  {
  SHOWPARSE("types -> type");
  $$ = AST::make(at_Null);
  $$->addChild($1);
};
types: types type {
  SHOWPARSE("types -> types type");
  $$ = $1;
  $1->addChild($2);
};

type: useident  { 			/* previously defined type */
  SHOWPARSE("type -> useident");
  $$ = $1;
};

// PRIMARY TYPES [3.2]
type: tk_UNIT {
  SHOWPARSE("type -> UNIT");
  $$ = AST::make(at_primaryType, $1.loc);
  $$->s = "unit";		/* for lookup! */
};

bool_type: tk_BOOL {
  SHOWPARSE("bool_type -> BOOL");
  $$ = AST::make(at_primaryType, $1);
};

type: bool_type {
  SHOWPARSE("type -> bool_type");
  $$ = $1;
};

type: tk_CHAR {
  SHOWPARSE("type -> CHAR");
  $$ = AST::make(at_primaryType, $1);
};
type: tk_STRING {
  SHOWPARSE("type -> STRING");
  $$ = AST::make(at_primaryType, $1);
};

int_type: tk_INT8 {
  SHOWPARSE("int_type -> INT8");
  $$ = AST::make(at_primaryType, $1);
};
int_type: tk_INT16 {
  SHOWPARSE("int_type -> INT16");
  $$ = AST::make(at_primaryType, $1);
};
int_type: tk_INT32 {
  SHOWPARSE("int_type -> INT32");
  $$ = AST::make(at_primaryType, $1);
};
int_type: tk_INT64 {
  SHOWPARSE("int_type -> INT64");
  $$ = AST::make(at_primaryType, $1);
};
uint_type: tk_UINT8 {
  SHOWPARSE("uint_type -> UINT8");
  $$ = AST::make(at_primaryType, $1);
};
uint_type: tk_UINT16 {
  SHOWPARSE("uint_type -> UINT16");
  $$ = AST::make(at_primaryType, $1);
};
uint_type: tk_UINT32 {
  SHOWPARSE("uint_type -> UINT32");
  $$ = AST::make(at_primaryType, $1);
};
uint_type: tk_UINT64 {
  SHOWPARSE("type -> UINT64");
  $$ = AST::make(at_primaryType, $1);
};

any_int_type: int_type {
  SHOWPARSE("any_int_type -> int_type");
  $$ = $1;
};
any_int_type: uint_type {
  SHOWPARSE("any_int_type -> uint_type");
  $$ = $1;
};
any_int_type: tk_WORD {
  SHOWPARSE("any_int_type -> WORD");
  $$ = AST::make(at_primaryType, $1);
};

float_type: tk_FLOAT {
  SHOWPARSE("float_type -> FLOAT");
  $$ = AST::make(at_primaryType, $1);
};
float_type: tk_DOUBLE {
  SHOWPARSE("float_type -> DOUBLE");
  $$ = AST::make(at_primaryType, $1);
};

type: any_int_type {
  SHOWPARSE("type -> any_int_type");
  $$ = $1;
};
type: float_type {
  SHOWPARSE("type -> float_type");
  $$ = $1;
};

// EXCEPTION type
type: tk_EXCEPTION {
  SHOWPARSE("type -> EXCEPTION");
  $$ = AST::make(at_exceptionType, $1.loc);
};

// TYPE VARIABLES [3.3]
type: typevar  { 		
  SHOWPARSE("type -> typevar");
  $$ = $1;
};

// REF TYPES [3.4.1]
type: tk_REF '(' type ')' {
  SHOWPARSE("type -> REF ( type )");
  $$ = AST::make(at_refType, $1.loc, $3);
};

// VAL TYPES [3.4.2]
type: tk_VAL '(' type ')' {
  SHOWPARSE("type -> VAL ( type )");
  $$ = AST::make(at_valType, $1.loc, $3);
};

// FUNCTION TYPES [3.4.3]
type: fntype {
  SHOWPARSE("type -> fntype");
  $$ = $1;
}

fneffect: {
  SHOWPARSE("fneffect -> <empty>");
  $$ = AST::make(at_ident, LToken("impure"));
};

fneffect: tk_PURE {
  SHOWPARSE("fneffect -> PURE");
  $$ = AST::make(at_ident, $1);
};

fneffect: tk_IMPURE {
  SHOWPARSE("fneffect -> IMPURE");
  $$ = AST::make(at_ident, $1);
};

fneffect: tk_EffectVar {
  SHOWPARSE("fneffect -> <EffectVar=" + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

fntype: '(' ')' fneffect tk_FNARROW type {
  SHOWPARSE("fntype -> () fneffect -> type");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $1.loc);
  $$ = AST::make(at_fn, $1.loc, fnargVec, $5);
};

fntype: '(' types_pl_byref ')' fneffect tk_FNARROW type {
  SHOWPARSE("( types_pl_byref ) fneffect --> type");
  $$ = AST::make(at_fn, $1.loc, $2, $6);
};

type_cpair: type ',' type {
  SHOWPARSE("type_cpair -> type ',' type");
  $$ = AST::make(at_typeapp, $2.loc,
	       AST::make(at_ident, LToken($2.loc, "pair")),
	       $1, $3);
  $$->printVariant = pf_IMPLIED;
};
type_cpair: type ',' type_cpair {
  SHOWPARSE("type_cpair -> type ',' type_cpair");
  $$ = AST::make(at_typeapp, $2.loc,
	       AST::make(at_ident, LToken($2.loc, "pair")),
	       $1, $3);
  $$->printVariant = pf_IMPLIED;
};

type: '(' type_cpair ')' {
  SHOWPARSE("type -> type_cpair");
  $$ = $2;
};

// ARRAY TYPE [3.5.1]
type: tk_ARRAY type '[' intLit ']' {
  SHOWPARSE("type -> type '[' intLit ']'");
  $$ = AST::make(at_arrayType, $1.loc, $2, $4);
};
// VECTOR TYPE [3.5.2]
type: type '[' ']' {
  SHOWPARSE("type -> type []");
  $$ = AST::make(at_vectorType, $1->loc, $1);
};

// TYPE CONSTRUCTORS (typeapp)
type: typeapp {
  SHOWPARSE("type -> typeapp");
  $$ = $1;
};

typeapp: useident '(' types ')' {
  SHOWPARSE("typeapp -> useident ( types )");
  $$ = AST::make(at_typeapp, $1->loc, $1);
  $$->addChildrenFrom($3);
};

// MUTABLE TYPE [3.7]
type: tk_MUTABLE type {
  SHOWPARSE("type -> MUTABLE type");
  $$ = AST::make(at_mutableType, $1.loc, $2);
};

type: tk_CONST type {
  SHOWPARSE("type -> CONST type");
  // Should be:
  // $$ = AST::make(at_constType, $2.loc, $3);
  // but for now:
  $$ = $2;
};

// BITFIELD TYPE

// Note that in contrast to C the field length is part of
// the type!
bitfieldtype: any_int_type ':' intLit {
  SHOWPARSE("bitfieldtype -> any_int_type : intLit");
  $$ = AST::make(at_bitfield, $1->loc, $1, $3);
}

bitfieldtype: bool_type ':' intLit {
  SHOWPARSE("bitfieldtype -> bool_type : intLit");
  $$ = AST::make(at_bitfield, $1->loc, $1, $3);
}

// Legal field types, including bitfield type
type_or_bitfield: bitfieldtype {
  SHOWPARSE("type_or_bitfield -> bitfieldtype");
  $$ = $1;
};

type_or_bitfield: type {
  SHOWPARSE("type_or_bitfield -> type");
  $$ = $1;
};


// by-ref types are not a part of general `type' rule.
// They are gramatiocally restricted to apprae only on
// formal function arguments and function types.
type_pl_byref: type {
  SHOWPARSE("type_pl_byref -> type");
  $$ = $1;
};

type_pl_byref: tk_BY_REF type {
  SHOWPARSE("type_pl_byref -> BY-REF type");
  $$ = AST::make(at_byrefType, $1.loc, $2);
};

types_pl_byref: type_pl_byref {
  SHOWPARSE("types_pl_byref -> type_pl_byref");
  $$ = AST::make(at_fnargVec);
  $$->addChild($1);
};
types_pl_byref: types_pl_byref type_pl_byref {
  SHOWPARSE("types_pl_byref -> types_pl_byref type_pl_byref");
  $$ = $1;
  $1->addChild($2);
};

// Qualified types:

qual_type: type {
  SHOWPARSE("qual_type -> type");
  $$ = $1;  
};

// This syntax is just too horrible for words:
//
//  forall Eql('a) ('a)->bool
//
// so consider it a temporary solution. The problem is that there is
// no explicit bracketing for the eye to follow.
qual_type: tk_FORALL constraints type {
 SHOWPARSE("qual_type -> FORALL constraints type");
 $$ = AST::make(at_qualType, $1.loc, $2, $3);
};

// IDENTIFIERS [2.2]
ident: tk_Ident {
  SHOWPARSE("ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

ident: tk_Reserved {
  SHOWPARSE("ident -> <RESERVED=" + $1.str + ">");
  cerr << $1.loc.asString() << ": The token \"" << $1.str
       << "\" is reserved for future use.\n";
  lexer->num_errors++;
  $$ = AST::make(at_ident, $1);
};

useident: ident {
  SHOWPARSE("useident -> ident");
  $$ = $1;
};

useident: ident '.' ident {
  SHOWPARSE("useident -> ident . ident");
  $$ = AST::make(at_usesel, $1->loc, $1, $3);
};

//defident: ident {
//  SHOWPARSE("defident -> ident");
//  $1->flags |= (ID_IS_GLOBAL);
//  $$ = $1;
//};

defident: useident {
  SHOWPARSE("defident -> useident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = $1;
};

// TYPE VARIABLES [3.3]
typevar: tk_TypeVar {
  SHOWPARSE("typevar -> <TypeVar=" + $1.str + ">");
  $$ = AST::make(at_ident, $1);
  $$->identType = id_tvar;
};

// Literal Value Representations

boolLit: tk_TRUE {
  SHOWPARSE("boolLit -> <Bool=" + $1.str +">");
  $$ = AST::makeBoolLit($1);
};
boolLit: tk_FALSE {
  SHOWPARSE("boolLit -> <Bool=" + $1.str +">");
  $$ = AST::makeBoolLit($1);
};

charlit: tk_Char {
  SHOWPARSE("charlit -> <Char=" + $1.str +">");
  $$ = AST::makeCharLit($1);
};

intLit: tk_Int {
  SHOWPARSE("intLit -> <Int=" + $1.str +">");
  $$ = AST::makeIntLit($1);
};

floatLit: tk_Float {
  SHOWPARSE("floatLit -> <Float=" + $1.str +">");
  $$ = AST::makeFloatLit($1);
};

strLit: tk_String {
  SHOWPARSE("strLit -> <String=" + $1.str +">");
  $$ = AST::makeStringLit($1);
};

%%

