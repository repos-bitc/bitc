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

%token <tok> tk_REF
%token <tok> tk_VAL

%token <tok> tk_SET
%token <tok> tk_TAG

%token <tok> tk_DEREF
%token <tok> tk_LAMBDA

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
%token <tok> tk_PROCLAIM
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
%type <ast> interface
%type <ast> if_definitions if_definition
%type <ast> common_definition
%type <ast> import_definition importList
 //%type <ast> provide_definition provideList
%type <ast> type_definition
%type <ast> types type  bool_type
 //%type <ast> bitfieldtype
%type <ast> literal
%type <ast> typevar
%type <ast> type_or_bitfield type_pl_byref types_pl_byref
%type <ast> type_cpair typapp
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

// Documentation comments. These are added only in productions where
// they do NOT appear before expr_seq. If a string literal appears as
// the first form of a multiform expr_seq, it won't hurt anything. If
// it is the *only* form, then it is the value in any case, and that
// is fine. We can figure out which case is which in the documentation
// extractor.
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

//uoc_body: module {
//  SHOWPARSE("uocbody -> module");
//}

interface: tk_INTERFACE ifident {
    if ($2.str.find("bitc.") == 0)
      lexer->isRuntimeUoc = true;
  }
  optdocstring if_definitions tk_END {
  SHOWPARSE("interface -> INTERFACE ifident optdocstring if_definitions END");
  shared_ptr<AST> ifIdent = AST::make(at_ident, $2);
  $$ = AST::make(at_interface, $1.loc, ifIdent);
  $$->addChildrenFrom($5);

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

common_definition: import_definition {
  SHOWPARSE("common_definition -> import_definition");
  $$ = $1;
};

common_definition: type_definition {
  SHOWPARSE("common_definition -> import_definition");
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
alias: '(' ident tk_AS ident ')' {
  SHOWPARSE("alias -> ( ident AS ident )");

  $$ = AST::make(at_ifsel, $2->loc, $4, $2);
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
type_definition: tk_STRUCT ptype_name val optdocstring declares fields tk_END  {
  SHOWPARSE("type_definition -> STRUCT ptype_name val "
	    "optdocstring declares fields END");
  $$ = AST::make(at_defstruct, $1.loc, $2->child(0), $2->child(1), $3,
	       $5, $6);
  $$->child(0)->defForm = $$;
  $$->addChild($2->child(2));
};

declares: {
  $$ = GC_NULL;
};

// CATEGORIES

val: {
  SHOWPARSE("val -> <empty>");
  $$ = AST::make(at_refCat);
  $$->printVariant = 1;
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

field: ident ':' type_or_bitfield  {
  SHOWPARSE("field -> ident : type_or_bitfield");
  $$ = AST::make(at_field, $1->loc, $1, $3);
};

//field: '(' tk_THE type_pl_bf ident ')'  {
//  SHOWPARSE("field -> '(' THE type_pl_bf ident ')'");
//  $$ = AST::make(at_field, $1.loc, $4, $3);
//};

//field: '(' tk_FILL bitfieldtype ')'  {
//  SHOWPARSE("field -> '(' FILL type ')'");
//  $$ = AST::make(at_fill, $1.loc, $3);
//};

//field: '(' tk_RESERVED bitfieldtype intLit ')'  {
//  SHOWPARSE("field -> '(' RESERVED bitfieldtype intLit ')'");
//  $$ = AST::make(at_fill, $1.loc, $3);
//};

tvlist: typevar  {
  SHOWPARSE("tvlist -> typevar");
  $$ = AST::make(at_tvlist, $1->loc, $1);
};
tvlist: tvlist typevar {
  SHOWPARSE("tvlist -> tvlist typevar");
  $$ = $1;
  $1->addChild($2);
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
//type: fntype {
//  SHOWPARSE("type -> fntype");
//  $$ = $1;
//}
//
//fneffect: {
//  SHOWPARSE("fneffect -> <empty>");
//  $$ = AST::make(at_ident, LToken("impure"));
//};
//
//fneffect: tk_PURE {
//  SHOWPARSE("fneffect -> PURE");
//  $$ = AST::make(at_ident, $1);
//};
//
//fneffect: tk_IMPURE {
//  SHOWPARSE("fneffect -> IMPURE");
//  $$ = AST::make(at_ident, $1);
//};
//
//fneffect: tk_EffectVar {
//  SHOWPARSE("fneffect -> <EffectVar=" + $1.str + ">");
//  $$ = AST::make(at_ident, $1);
//};
//
//fntype: '(' fneffect tk_FN '(' ')' type ')' {
//  SHOWPARSE("fntype -> ( fneffect FN () type )");
//  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $4.loc);
//  $$ = AST::make(at_fn, $1.loc, fnargVec, $6);
//};
//
//fntype: '(' fneffect tk_FN '(' types_pl_byref ')' type ')'  {
//  SHOWPARSE("fntype -> ( fneffect FN ( types_pl_byref ) type )");
//  $$ = AST::make(at_fn, $1.loc, $5, $7);
//};

type_cpair: type ',' type {
  SHOWPARSE("type_cpair -> type ',' type");
  $$ = AST::make(at_typeapp, $2.loc,
	       AST::make(at_ident, LToken($2.loc, "pair")),
	       $1, $3);
  $$->printVariant = 1;
};
type_cpair: type ',' type_cpair {
  SHOWPARSE("type_cpair -> type ',' type_cpair");
  $$ = AST::make(at_typeapp, $2.loc,
	       AST::make(at_ident, LToken($2.loc, "pair")),
	       $1, $3);
  $$->printVariant = 1;
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

// TYPE CONSTRUCTORS (typapp)
type: typapp {
  SHOWPARSE("type -> typapp");
  $$ = $1;
};

typapp: useident '(' types ')' {
  SHOWPARSE("typeapp -> useident ( types )");
  $$ = AST::make(at_typeapp, $1->loc, $1);
  $$->addChildrenFrom($3);
};

// MUTABLE TYPE [3.7]
type: tk_MUTABLE '(' type ')' {
  SHOWPARSE("type -> MUTABLE ( type )");
  $$ = AST::make(at_mutableType, $1.loc, $3);
};

type: tk_CONST '(' type ')' {
  SHOWPARSE("type -> CONST ( type )");
  // Should be:
  // $$ = AST::make(at_constType, $2.loc, $3);
  // but for now:
  $$ = $3;
};

// BITFIELD TYPE
//bitfieldtype: '(' tk_BITFIELD any_int_type intLit ')' {
//  SHOWPARSE("bitfieldtype -> ( BITFIELD any_int_type intLit )");
//  $$ = AST::make(at_bitfield, $2.loc, $3, $4);
//};
//bitfieldtype: '(' tk_BITFIELD bool_type intLit ')' {
//  SHOWPARSE("bitfieldtype -> ( BITFIELD bool_type intLit )");
//  $$ = AST::make(at_bitfield, $2.loc, $3, $4);
//};

// Any-type, including bitfield type
//type_or_bitfield: bitfieldtype {
//  SHOWPARSE("type_or_bitfield -> bitfieldtype");
//  $$ = $1;
//};

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

//type_pl_byref: '(' tk_BY_REF type ')' {
//  SHOWPARSE("type_pl_byref -> ( BY-REF type )");
//  $$ = AST::make(at_byrefType, $2.loc, $3);
//};

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

