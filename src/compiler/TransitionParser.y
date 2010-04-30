%{
  /*
   * Copyright (C) 2010, Jonathan S. Shapiro
   * Portions Copyright (C) 2008, The EROS Group, LLC.
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
/// @brief The Transitional parser for BitC.
///
/// This is the reference parser for the language described in the
/// specification.

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
#define YYLEX_PARAM (TransitionLexer *)lexer
#undef yyerror
#define yyerror(lexer, s) lexer->ReportParseError(s)

#include "TransitionLexer.hxx"

#define SHOWPARSE(s) \
  do { \
    if (Options::showParse) \
      lexer->errStream << (s) << std::endl;                \
  } while (false);
#define SHOWPARSE1(s,x) \
  do { \
    if (Options::showParse) \
      lexer->errStream << (s) << " " << (x) << std::endl;        \
  } while (false);


inline int transition_lex(YYSTYPE *lvalp, TransitionLexer *lexer)
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

static unsigned VersionMajor(const std::string s)
{
  std::string::size_type dotPos = s.find('.');
  return strtoul(s.substr(0, dotPos).c_str(), 0, 10);
}

static unsigned VersionMinor(const std::string s)
{
  std::string::size_type dotPos = s.find('.');
  return strtoul(s.substr(dotPos+1, s.size()).c_str(), 0, 10);
}

%}

%pure-parser
%parse-param {TransitionLexer *lexer}

%token <tok> tk_ReservedWord        /* reserved words */

/* Categorical terminals: */
%token <tok> tk_Ident
%token <tok> tk_TypeVar
%token <tok> tk_EffectVar
%token <tok> tk_Int
%token <tok> tk_Float
%token <tok> tk_Char
%token <tok> tk_String
%token <tok> tk_VersionNumber

/* Primary sxp_types and associated hand-recognized literals: */
%token <tok> '(' ')' ','        /* unit */
%token <tok> '[' ']'        /* unit */
%token <tok> '.'
%token <tok> tk_AS
%token <tok> tk_BOOL
%token <tok> tk_TRUE   /* #t */
%token <tok> tk_FALSE  /* #f */
%token <tok> tk_CHAR
%token <tok> tk_STRING
%token <tok> tk_FLOAT
%token <tok> tk_DOUBLE
%token <tok> tk_DUP
%token <tok> tk_QUAD
%token <tok> tk_INT8
%token <tok> tk_INT16
%token <tok> tk_INT32
%token <tok> tk_INT64
%token <tok> tk_UINT8
%token <tok> tk_UINT16
%token <tok> tk_UINT32
%token <tok> tk_UINT64
%token <tok> tk_WORD

%token <tok> tk_SIZEOF
%token <tok> tk_BITSIZEOF

%token <tok> tk_BITFIELD
%token <tok> tk_FILL
%token <tok> tk_RESERVED
%token <tok> tk_WHERE

%token <tok> tk_BITC_VERSION
%token <tok> tk_BITC
%token <tok> tk_VERSION

%token <tok> tk_PURE
%token <tok> tk_IMPURE
%token <tok> tk_CONST

%token <tok> tk_THE
%token <tok> tk_IF
%token <tok> tk_WHEN
%token <tok> tk_AND
%token <tok> tk_OR
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
%token <tok> tk_ARRAY_REF_LENGTH
%token <tok> tk_VECTOR_LENGTH
%token <tok> tk_ARRAY_NTH
%token <tok> tk_ARRAY_REF_NTH
%token <tok> tk_VECTOR_NTH
%token <tok> tk_NTH

%token <tok> tk_DEFSTRUCT
%token <tok> tk_DEFOBJECT
%token <tok> tk_DEFUNION
%token <tok> tk_DEFREPR
%token <tok> tk_DEFTHM
%token <tok> tk_DEFINE
%token <tok> tk_DECLARE
%token <tok> tk_PROCLAIM
%token <tok> tk_EXTERNAL
%token <tok> tk_TAG
%token <tok> tk_DEFEXCEPTION

%token <tok> tk_MUTABLE
%token <tok> tk_SET
%token <tok> tk_DEREF
%token <tok> tk_REF
%token <tok> tk_INNER_REF
%token <tok> tk_VAL
%token <tok> tk_OPAQUE
%token <tok> tk_CLOSED
%token <tok> tk_MEMBER
%token <tok> tk_LAMBDA
%token <tok> tk_LET
%token <tok> tk_LETREC
%token <tok> tk_FN
%token <tok> tk_FNARROW
%token <tok> tk_BEGIN
%token <tok> tk_DO
%token <tok> tk_APPLY
%token <tok> tk_BY_REF
%token <tok> tk_ARRAY_REF

%token <tok> tk_EXCEPTION
%token <tok> tk_TRY
%token <tok> tk_CATCH
%token <tok> tk_THROW

%token <tok> tk_METHOD
%token <tok> tk_DEFTYPECLASS
%token <tok> tk_DEFINSTANCE
%token <tok> tk_FORALL

%token <tok> tk_INTERFACE
%token <tok> tk_MODULE
// token tk_USESEL historic significance only
// %token <tok> tk_USESEL
%token <tok> tk_IMPORT
%token <tok> tk_PROVIDE

%token <tok> tk_TYFN
//%token <tok> tk_SUPER
%token <tok> tk_SUSPEND
%type <tok>  ifident

//%token <tok> tk_EXPORT
 
%type <ast> sxp_module sxp_implicit_module sxp_module_seq
%type <ast> sxp_interface
%type <ast> sxp_defpattern
%type <ast> sxp_mod_definitions sxp_mod_definition
%type <ast> sxp_if_definitions sxp_if_definition
%type <ast> sxp_common_definition
%type <ast> sxp_value_declaration
%type <ast> sxp_ptype_name sxp_val sxp_openclosed
%type <ast> sxp_type_definition sxp_typeapp
%type <ast> sxp_type_decl sxp_externals sxp_alias
%type <ast> sxp_importList sxp_provideList
%type <ast> sxp_type_cpair sxp_eform_cpair
%type <ast> sxp_value_definition sxp_tc_definition sxp_ti_definition
%type <ast> sxp_import_definition sxp_provide_definition
%type <ast> sxp_tc_decls sxp_tc_decl sxp_ow
%type <ast> sxp_declares sxp_declare sxp_decls sxp_decl
%type <ast> sxp_constructors sxp_constructor
%type <ast> sxp_repr_constructors sxp_repr_constructor
%type <ast> sxp_repr_reprs sxp_repr_repr
%type <ast> sxp_bindingpattern sxp_lambdapatterns sxp_lambdapattern
%type <ast> sxp_expr_seq
%type <ast> sxp_expr sxp_the_expr sxp_eform sxp_method_decls sxp_method_decl
%type <ast> sxp_method_bindings sxp_method_binding
%type <ast> sxp_constraints sxp_constraint_seq sxp_constraint
%type <ast> sxp_types sxp_type sxp_bitfieldtype bool_type
%type <ast> sxp_field_type sxp_type_pl_byref sxp_types_pl_byref
%type <ast> sxp_int_type sxp_uint_type sxp_any_int_type sxp_float_type
%type <ast> sxp_tvlist
%type <ast> sxp_fields sxp_field
%type <ast> sxp_fields_and_methods sxp_methods_only sxp_methdecl
%type <ast> sxp_literal sxp_typevar //mod_ident
%type <ast> sxp_switch_matches sxp_switch_match
%type <ast> sxp_exident
%type <ast> sxp_docstring sxp_optdocstring
%type <ast> sxp_condcases sxp_condcase sxp_fntype sxp_method_type
%type <ast> sxp_fneffect
 //%type <ast> reprbody reprbodyitems reprbodyitem reprtags reprcase reprcaseleg
//%type <ast> typecase_leg typecase_legs
%type <ast> sxp_sw_legs sxp_sw_leg sxp_otherwise
//%type <ast> catchclauses catchclause
%type <ast> sxp_letbindings sxp_letbinding
%type <ast> sxp_dobindings ne_dobindings sxp_dobinding sxp_dotest
%type <ast> sxp_let_eform
%type <ast> sxp_type_val_definition
%type <ast> sxp_constrained_definition

%type <ast> ident defident useident
%type <ast> intLit floatLit charLit strLit boolLit
%%

// Parser built for version 10.0
// Section Numbers indicated within []

// COMPILATION UNITS [2.5]
// This definition od sxp_start mus be changed as it ignores
// junk after the body.

sxp_start: trn_version sxp_uoc_body {
  SHOWPARSE("sxp_start -> trn_version sxp_uoc_body");
  return 0;
};

sxp_start: sxp_uoc_body {
  SHOWPARSE("sxp_start -> sxp_uoc_body");
  return 0;
};

sxp_uoc_body: sxp_interface {
  SHOWPARSE("uocbody -> sxp_interface");
}

sxp_uoc_body: sxp_implicit_module {
  SHOWPARSE("uocbody -> sxp_implicit_module");
}

sxp_uoc_body: sxp_module_seq {
  SHOWPARSE("uocbody -> sxp_module_seq");
}

// VERSION [2.5]

trn_version: sxp_version;
trn_version: blk_version;

// We cannot do optversion, because that would require two token look-ahead.
sxp_version: '(' tk_BITC_VERSION strLit ')' {
  SHOWPARSE("sxp_version -> ( BITC-VERSION strLit )");
  shared_ptr<AST> version = $3;

  if ((VersionMajor(version->s) == 0) && (VersionMinor(version->s) < 10)) {
    std::string s = ": Error: input language version " + version->s + " is no longer accepted.";
    lexer->ReportParseError(version->loc, s);
  }

  if ((VersionMajor(version->s) == 0) && (VersionMinor(version->s) == 11)) {
    std::string s = ": Warning: bitc-version with stringified version number is deprecated.";
    lexer->ReportParseWarning(version->loc, s);
  }

  // Beginning in version 0.12, this entire form will be gone.

  if (!CheckVersionCompatibility(version->s)) {
    std::string s = ": Warning: BitC version conflict " 
      + version->s + " vs " + Version();
    lexer->ReportParseWarning(version->loc, s);
  }
};

sxp_version: '(' tk_BITC_VERSION tk_VersionNumber ')' {
  SHOWPARSE("sxp_version -> ( BITC-VERSION strLit )");
  shared_ptr<AST> version = AST::makeStringLit($3);

  // Beginning in version 0.12, this entire form will be gone.

  if ((VersionMajor(version->s) == 0) && (VersionMinor(version->s) < 10)) {
    std::string s = ": Error: input language version " + version->s + " is no longer accepted.";
    lexer->ReportParseError(version->loc, s);
  }
};

blk_version: tk_BITC {
    lexer->currentLang |= TransitionLexer::lf_version; 
    lexer->currentLang &= ~TransitionLexer::lf_LispComments; 
} tk_VERSION tk_VersionNumber ';' {
  SHOWPARSE("sxp_version -> ( BITC-VERSION strLit )");
  shared_ptr<AST> version = AST::makeStringLit($3);

  if ((VersionMajor(version->s) == 0) && (VersionMinor(version->s) < 11)) {
    std::string s = ": Error: block syntax is supported in versions 0.11 and above.";
    lexer->ReportParseError(version->loc, s);
  }

  lexer->currentLang |= TransitionLexer::lf_block; 
  lexer->currentLang &= ~TransitionLexer::lf_LispComments;
};

// It would be really nice to support this, but the tokenizer will
// report the sxp_literal as a floating point value. We could, I suppose,
// crack that back into its constituent parts, but I don't have the
// energy to implement that today.
// version: '(' tk_BITC_VERSION intLit '.' intLit ')' {
//   SHOWPARSE("sxp_version -> ( BITC-VERSION intLit '.' intLit )");
//   if (!CheckVersionCompatibility($3->litValue.i, $5->litValue.i)) {
//     std::string s = ": Warning: BitC sxp_version conflict " + $3->s + " vs " + Version();
//     lexer->ReportParseWarning($3->loc, s);
//   }
// };

// Documentation comments. These are added only in productions where
// they do NOT appear before expr_seq. If a string sxp_literal appears as
// the first form of a multiform expr_seq, it won't hurt anything. If
// it is the *only* form, then it is the value in any case, and that
// is fine. We can figure out which case is which in the documentation
// extractor.
sxp_docstring: tk_String {
  SHOWPARSE("sxp_docstring -> STRING");
  $$ = AST::make(at_docString, $1.loc, AST::makeStringLit($1));
};
sxp_optdocstring: sxp_docstring {
  SHOWPARSE("sxp_optdocstring -> sxp_docstring");
  $$ = $1;
};
sxp_optdocstring: {
  SHOWPARSE("sxp_optdocstring -> ");
  $$ = AST::make(at_docString);
};

// TODO: The ident in sxp_interface rule should be restricted to
// ({ALPHA} | [_\-]) (({ALPHA} | {DECDIGIT} | [_\-])*

// INTERFACES [8.1]
sxp_interface: '(' tk_INTERFACE ifident {
    if ($3.str.find("bitc.") == 0)
      lexer->isRuntimeUoc = true;
  }
  sxp_optdocstring sxp_if_definitions ')' {
  SHOWPARSE("sxp_interface -> INTERFACE ifident sxp_optdocstring sxp_if_definitions");
  shared_ptr<AST> ifIdent = AST::make(at_ident, $3);
  $$ = AST::make(at_interface, $2.loc, ifIdent);
  $$->addChildrenFrom($6);

  if (lexer->isCommandLineInput) {
    const char *s =
      ": Warning: sxp_interface units of compilation should no longer\n"
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
    std::string s = "Error: This sxp_interface has already been loaded from "
      + existingUoc->uocAst->loc.asString();
    lexer->ReportParseError($$->loc, s);

  }
  else  {
    /* Put the UoC onto the sxp_interface list so that we do not recurse on
       import. */
    UocInfo::ifList[uocName] = uoc;
  }
  
  // Regardless, compile the new sxp_interface to check for further
  // warnings and/or errors:
  uoc->Compile();
};

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

// MODULES [2.5]
sxp_implicit_module: sxp_mod_definitions  {
 SHOWPARSE("sxp_implicit_module -> sxp_mod_definitions");
 $$ = $1;
 $$->astType = at_module;
 $$->printVariant = pf_IMPLIED;

 // Construct, compile, and admit the parsed UoC:
 string uocName =
   UocInfo::UocNameFromSrcName(lexer->here.origin, lexer->nModules);

 shared_ptr<UocInfo> uoc = UocInfo::make(uocName, lexer->here.origin, $$);
 lexer->nModules++;

 uoc->Compile();
 UocInfo::srcList[uocName] = uoc;
};

sxp_module: '(' tk_MODULE sxp_optdocstring sxp_mod_definitions ')' {
 SHOWPARSE("sxp_module -> ( tk_MODULE sxp_optdocstring sxp_mod_definitions )");
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

sxp_module: '(' tk_MODULE ifident sxp_optdocstring sxp_mod_definitions ')' {
 SHOWPARSE("sxp_module -> ( tk_MODULE ifident sxp_optdocstring sxp_mod_definitions )");
 $$ = $5;
 $$->astType = at_module;

 // Construct, compile, and admit the parsed UoC.
 // Note that we do not even consider the user-provided sxp_module name
 // for purposes of internal naming, because it is not significant.
 string uocName =
   UocInfo::UocNameFromSrcName(lexer->here.origin, lexer->nModules);

 shared_ptr<UocInfo> uoc = UocInfo::make(uocName, lexer->here.origin, $$);
 lexer->nModules++;
 uoc->Compile();
 UocInfo::srcList[uocName] = uoc;
};

sxp_module_seq: sxp_module {
 SHOWPARSE("sxp_module_seq -> sxp_module");
}

sxp_module_seq: sxp_module_seq sxp_module {
 SHOWPARSE("sxp_module_seq -> sxp_module_seq sxp_module");
}

// INTERFACE TOP LEVEL DEFINITIONS
sxp_if_definitions: sxp_if_definition {
  SHOWPARSE("sxp_if_definitions -> sxp_if_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

sxp_if_definitions: sxp_if_definitions sxp_if_definition {
  SHOWPARSE("sxp_if_definitions -> sxp_if_definitions sxp_if_definition");
  $$ = $1;
  $$->addChild($2); 
};

sxp_if_definition: sxp_common_definition {
  SHOWPARSE("sxp_if_definition -> sxp_common_definition");
  $$ = $1;
};

// TOP LEVEL DEFINITIONS [2.5.1]
sxp_constrained_definition: '(' tk_FORALL sxp_constraints sxp_type_val_definition ')' {
  // HACK ALERT!!! For reasons of ancient history, there is no
  // at_forall AST. Instead, all of the sxp_type_val_definition ASTs have
  // their sxp_constraints tacked on at the end. This is rather badly
  // glitched and we need to fix it, but the immediate goal was to
  // move the (forall ...) syntax to the outside without doing major
  // surgery on the compiler internals.

  uint32_t nChildren = $4->children.size();

  shared_ptr<AST> tvConstraints= $4->child(nChildren-1);
  tvConstraints->addChildrenFrom($3);
  $$ = $4;
};

sxp_mod_definitions: sxp_mod_definition {
  SHOWPARSE("sxp_mod_definitions -> sxp_mod_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

sxp_mod_definitions: sxp_mod_definitions sxp_mod_definition {
  SHOWPARSE("sxp_mod_definitions -> sxp_mod_definitions sxp_mod_definition");
  $$ = $1;
  $$->addChild($2); 
};

sxp_mod_definition: sxp_provide_definition {
  SHOWPARSE("sxp_mod_definition -> sxp_provide_definition");
  $$ = $1;
};
sxp_mod_definition: sxp_common_definition {
  SHOWPARSE("sxp_mod_definition -> sxp_common_definition");
  $$ = $1;
};

sxp_common_definition: sxp_import_definition {
  SHOWPARSE("sxp_common_definition -> sxp_import_definition");
  $$ = $1;
};

sxp_common_definition: sxp_type_val_definition {
  SHOWPARSE("sxp_common_definition -> sxp_type_val_definition");
  $$ = $1;
};

sxp_common_definition: sxp_constrained_definition {
  SHOWPARSE("sxp_common_definition -> sxp_constrained_definition");
  $$ = $1;
};

sxp_type_val_definition: sxp_type_definition {
  SHOWPARSE("sxp_type_val_definition -> sxp_type_definition");
  $$ = $1;
};

sxp_type_val_definition: sxp_type_decl {
  SHOWPARSE("sxp_type_val_definition -> sxp_type_decl");
  $$ = $1;
};

sxp_type_val_definition: sxp_value_definition {
  SHOWPARSE("sxp_type_val_definition -> sxp_value_definition");
  $$ = $1;
};

sxp_type_val_definition: sxp_value_declaration {
  SHOWPARSE("sxp_type_val_definition -> sxp_value_declaration");
  $$ = $1;
};

sxp_type_val_definition: sxp_tc_definition {
  SHOWPARSE("sxp_type_val_definition -> sxp_tc_definition");
  $$ = $1;
};

sxp_type_val_definition: sxp_ti_definition {
  SHOWPARSE("sxp_type_val_definition -> sxp_ti_definition");
  $$ = $1;
};

// DECLARE [8.4.2]
//common_definition: sxp_declare {
//  SHOWPARSE("sxp_common_definition -> sxp_declare");
//  $$ = $1;
//};

//Typeclass sxp_constraint declarations

sxp_constraints: '(' sxp_constraint_seq ')' {
 SHOWPARSE("sxp_constraints -> ( sxp_constraint_seq )");
 $$ = $2;
};

sxp_constraint_seq: sxp_constraint_seq sxp_constraint {
 SHOWPARSE("sxp_constraint_seq -> sxp_constraint_seq sxp_constraint");
 $$ = $1;
 $$->addChild($2);
};

sxp_constraint_seq: sxp_constraint {
 SHOWPARSE("sxp_constraint_seq -> sxp_constraint");
 $$ = AST::make(at_constraints, $1->loc, $1);
};

sxp_constraint: sxp_typeapp {
 SHOWPARSE("sxp_constraint -> sxp_typeapp");
 $1->astType = at_tcapp;
 $$ = $1;
};

sxp_constraint: useident {
 SHOWPARSE("sxp_constraint -> useident");
 $$ = AST::make(at_tcapp, $1->loc, $1);
};


// FIX: This should probably get its own AST type.
sxp_ptype_name: defident {
  SHOWPARSE("sxp_ptype_name -> defident");
  shared_ptr<AST> tvlist = AST::make(at_tvlist, $1->loc);
  shared_ptr<AST> constraints = AST::make(at_constraints, $1->loc);
  $$ = AST::make(at_Null, $1->loc, $1, tvlist, constraints);
};

sxp_ptype_name: '(' defident sxp_tvlist ')' {
  SHOWPARSE("sxp_ptype_name -> '(' defident sxp_tvlist ')'");
  shared_ptr<AST> constraints = AST::make(at_constraints, $2->loc);
  $$ = AST::make(at_Null, $2->loc, $2, $3, constraints);
};

//ptype_name: '(' tk_FORALL sxp_constraints defident ')' {
//  SHOWPARSE("sxp_ptype_name -> '(' FORALL sxp_constraints '(' defident sxp_tvlist ')' ')' ");
//  shared_ptr<AST> tvlist = AST::make(at_tvlist, $4->loc);
//  $$ = AST::make(at_Null, $2.loc, $4, tvlist, $3);
//};
//
//ptype_name: '(' tk_FORALL sxp_constraints '(' defident sxp_tvlist ')' ')' {
//  SHOWPARSE("sxp_ptype_name -> '(' FORALL sxp_constraints '(' defident sxp_tvlist ')' ')' ");
//  $$ = AST::make(at_Null, $2.loc, $5, $6, $3);
//};

// STRUCTURE TYPES [3.6.1]         
sxp_type_definition: '(' tk_DEFSTRUCT sxp_ptype_name sxp_val sxp_optdocstring sxp_declares sxp_fields_and_methods ')'  {
  SHOWPARSE("sxp_type_definition -> ( DEFSTRUCT sxp_ptype_name sxp_val "
            "sxp_optdocstring sxp_declares sxp_fields )");
  $$ = AST::make(at_defstruct, $2.loc, $3->child(0), $3->child(1), $4,
               $6, $7);
  $$->child(0)->defForm = $$;
  $$->addChild($3->child(2));
};


// UNION TYPES [3.6.2]              
sxp_type_definition: '(' tk_DEFUNION sxp_ptype_name sxp_val sxp_optdocstring sxp_declares sxp_constructors  ')'  {
  SHOWPARSE("sxp_type_definition -> ( DEFUNION sxp_ptype_name sxp_val "
            "sxp_optdocstring sxp_declares sxp_constructors");
  $$ = AST::make(at_defunion, $2.loc, $3->child(0), $3->child(1), $4,
               $6, $7);
  $$->child(0)->defForm = $$;
  $$->addChild($3->child(2));
};

/* // REPR TYPES */
/* type_definition: '(' tk_DEFREPR defident sxp_val sxp_optdocstring sxp_declares reprbody  ')'  { */
/*   SHOWPARSE("sxp_type_definition -> ( DEFUNION sxp_ptype_name sxp_val " */
/*             "sxp_optdocstring sxp_declares reprbody"); */
/*   $$ = AST::make(at_defrepr, $2.loc, $3->child(0), $3->child(1), $4, */
/*                $6, $7); */
/*   $$->addChild($3->child(2));   */
/* }; */
/* reprbody: '(' reprbodyitems ')' { */
/*   SHOWPARSE("reprbody -> reprbodyitems"); */
/*   $$ = $2; */
/* }; */

/* reprbodyitems: reprbodyitem { */
/*   SHOWPARSE("reprbodyitems -> reprbodyitem"); */
/*   $$ = AST::make(at_reprbody, $1->loc, $1); */
/* }; */

/* reprbodyitems: reprbodyitems reprbodyitem { */
/*   SHOWPARSE("reprbodyitems -> reprbodyitems reprbodyitem"); */
/*   $$ = $1; */
/*   $$->addChild($2); */
/* }; */

/* reprbodyitem: sxp_field { */
/*   SHOWPARSE("reprbodyitem -> sxp_field"); */
/*   $$ = $1; */
/* }; */

/* reprbodyitem: '(' tk_TAG reprtags ')' { */
/*   SHOWPARSE("reprbodyitem -> '(' TAG reprtags ')' "); */
/*   $$ = $3; */
/*   $$->loc = $2.loc; */
/* }; */

/* reprbodyitem: '(' tk_THE sxp_field_type '(' tk_TAG reprtags ')' ')' { */
/*   SHOWPARSE("reprbodyitem -> '(' TAG reprtags ')' "); */
/*   $$ = $6; */
/*   $$->loc = $5.loc; */
/* }; */

/* reprbodyitem: '(' tk_CASE reprcase ')' { */
/*   SHOWPARSE("reprbodyitem -> '(' CASE reprcases ')' "); */
/*   $$ = $3; */
/*   $$->loc = $2.loc; */
/* }; */

/* reprcase: reprcaseleg { */
/*   SHOWPARSE("reprcase -> reprcaseleg"); */
/*   $$ = AST::make(at_reprcase, $1->loc, $1); */
/* }; */

/* reprcase: reprcase reprcaseleg { */
/*   SHOWPARSE("reprcase -> reprcase reprcaseleg"); */
/*   $$ = $1; */
/*   $$->addChild($2); */
/* }; */

/* reprcaseleg: '(' reprtags reprbody ')' { */
/*   SHOWPARSE("reprcaseleg -> reprtags reprbody"); */
/*   $$ = AST::make(at_reprcaselegR, $1.loc, $3); */
/*   $$->addChildrenFrom($2); */
/* }; */

/* reprtags: ident { */
/*   SHOWPARSE("reprtags -> ident"); */
/*   $$ = AST::make(at_reprtag, $1->loc, $1); /\* dummy AST sxp_type *\/ */
/* }; */

/* reprtags: reprtags ident { */
/*   SHOWPARSE("reprtags -> reprtags ident"); */
/*   $$ = $1; */
/*   $$->addChild($2); */
/* }; */

// REPR TYPES
sxp_type_definition: '(' tk_DEFREPR defident sxp_val sxp_optdocstring sxp_declares sxp_repr_constructors  ')'  {
  SHOWPARSE("sxp_type_definition -> ( DEFREPR defident sxp_val "
            "sxp_optdocstring sxp_declares sxp_repr_constructors");
  $$ = AST::make(at_defrepr, $2.loc, $3, $4, $6, $7);
  $$->child(0)->defForm = $$;
};

// Type Declarations
// External declarations
sxp_externals: /* nothing */ {
  SHOWPARSE("sxp_externals -> ");
  $$ = AST::make(at_Null);
  $$->flags = NO_FLAGS;
};

sxp_externals: tk_EXTERNAL {
  SHOWPARSE("sxp_externals -> EXTERNAL");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;
};

sxp_externals: tk_EXTERNAL sxp_exident {
  SHOWPARSE("sxp_externals -> EXTERNAL sxp_exident");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;
  $$->externalName = $2->s;
};


// OBJECT TYPES [3.6.1]         
sxp_type_definition: '(' tk_DEFOBJECT sxp_ptype_name sxp_optdocstring sxp_declares sxp_methods_only ')'  {
  SHOWPARSE("sxp_type_definition -> ( DEFSTRUCT sxp_ptype_name sxp_val "
            "sxp_optdocstring sxp_declares sxp_fields )");

  // For the moment, all objects are value types:
  shared_ptr<AST> valCat = AST::make(at_valCat, LToken($2.loc, "sxp_val"));

  $$ = AST::make(at_defobject, $2.loc, $3->child(0), $3->child(1),
                 valCat,
                 $5, $6);
  $$->child(0)->defForm = $$;
  $$->addChild($3->child(2));
};


// STRUCTURE DECLARATIONS
sxp_type_decl: '(' tk_DEFSTRUCT sxp_ptype_name sxp_val sxp_externals ')' {
  SHOWPARSE("sxp_type_decl -> ( DEFSTRUCT sxp_ptype_name sxp_val sxp_externals )");
  $$ = AST::make(at_declstruct, $2.loc, $3->child(0), $3->child(1), $4,
               $3->child(2));
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// UNION DECLARATIONS
sxp_type_decl: '(' tk_DEFUNION sxp_ptype_name sxp_val sxp_externals ')' {
  SHOWPARSE("sxp_type_decl -> ( DEFUNION sxp_ptype_name sxp_val )");
  $$ = AST::make(at_declunion, $2.loc, $3->child(0), $3->child(1), $4,
               $3->child(2));
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// REPR DECLARATIONS
sxp_type_decl: '(' tk_DEFREPR defident sxp_val sxp_externals ')' {
  SHOWPARSE("sxp_type_decl -> ( DEFREPR defident sxp_val sxp_externals )");
  $$ = AST::make(at_declrepr, $2.loc, $3, $4);
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// CATEGORIES

sxp_val: {
  SHOWPARSE("sxp_val -> <empty>");
  $$ = AST::make(at_refCat);
  $$->printVariant = pf_IMPLIED;
};

sxp_val: ':' tk_VAL {
  SHOWPARSE("sxp_val -> ':' VAL");
  $$ = AST::make(at_valCat, $2);
};
sxp_val: ':' tk_OPAQUE {
  SHOWPARSE("sxp_val -> ':' OPAQUE");
  $$ = AST::make(at_opaqueCat, $2);
};
sxp_val: ':' tk_REF {
  /* Same as :ref, since that is the default. */
  SHOWPARSE("sxp_val -> ':' REF");
  $$ = AST::make(at_refCat, $2);
};

sxp_openclosed: {
  SHOWPARSE("closed -> <empty>");
  $$ = AST::make(at_Null);
  $$->printVariant = pf_IMPLIED;
};

sxp_openclosed: ':' tk_CLOSED {
  SHOWPARSE("closed -> ':' CLOSED");
  $$ = AST::make(at_closed, $2);
};

// EXCEPTION DEFINITION [3.10]
sxp_type_definition: '(' tk_DEFEXCEPTION ident sxp_optdocstring ')' {
  SHOWPARSE("sxp_type_definition -> ( defexception ident )");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $2.loc, $3);
  $$->child(0)->defForm = $$;
};

sxp_type_definition: '(' tk_DEFEXCEPTION ident sxp_optdocstring sxp_fields ')' {
  SHOWPARSE("sxp_type_definition -> ( defexception ident sxp_fields )");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $2.loc, $3);
  $$->child(0)->defForm = $$;
  $$->addChildrenFrom($5);
};

// TYPE CLASSES [4]
// TYPE CLASS DEFINITION [4.1]

sxp_tc_definition: '(' tk_DEFTYPECLASS sxp_ptype_name sxp_optdocstring sxp_tc_decls sxp_openclosed sxp_method_decls ')' {
  SHOWPARSE("sxp_tc_definition -> ( DEFTYPECLASS sxp_ptype_name sxp_optdocstring sxp_tc_decls sxp_openclosed sxp_method_decls)");
  $$ = AST::make(at_deftypeclass, $2.loc, $3->child(0),
                 $3->child(1), $5, $6, $7);
  $$->addChild($3->child(2));
  $$->child(0)->defForm = $$;
};

//tc_definition: '(' tk_DEFTYPECLASS sxp_ptype_name sxp_optdocstring sxp_tc_decls sxp_method_decls ')' {
//  SHOWPARSE("sxp_tc_definition -> ( DEFTYPECLASS sxp_ptype_name sxp_optdocstring sxp_tc_decls sxp_openclosed sxp_method_decls)");
//  $$ = AST::make(at_deftypeclass, $2.loc, $3->child(0),
//                 $3->child(1), $5, $6, $3->child(2));
//  $$->child(0)->defForm = $$;
//};

sxp_tc_decls: {
  SHOWPARSE("tcdecls -> <empty>");
  $$ = AST::make(at_tcdecls);
};

sxp_tc_decls: sxp_tc_decls sxp_tc_decl {
  SHOWPARSE("tcdecls -> tcdelcs tcdecl");
  $$ = $1;
  $$->addChild($2);
};

sxp_tc_decl: '(' tk_TYFN '(' sxp_tvlist ')' sxp_typevar ')' {
  //                     ^^^^^^
  // I really mean sxp_tvlist here, arbitraty sxp_types
  // are not accepptable.
  SHOWPARSE("sxp_tc_decl -> ( TYFN ( sxp_tvlist ) sxp_typevar )");
  $4->astType = at_fnargVec;
  $$ = AST::make(at_tyfn, $2.loc, $4, $6);
};

sxp_method_decls: /* Nothing */ {
  SHOWPARSE("sxp_method_decls -> ");
  LexLoc loc;
  $$ = AST::make(at_method_decls, loc);
};

sxp_method_decls: sxp_method_decls sxp_method_decl {
  SHOWPARSE("sxp_method_decls -> sxp_method_decls sxp_method_decl");
  $$ = $1;
  $$->addChild($2);
};

sxp_method_decl: ident ':' sxp_fntype {
  SHOWPARSE("sxp_method_decl -> ident : sxp_fntype");
  $1->flags |= ID_IS_GLOBAL;
  $1->identType = id_tcmethod;
  $$ = AST::make(at_method_decl, $1->loc, $1, $3);
};

// TYPE CLASS INSTANTIATIONS [4.2]
// No sxp_docstring here because method_seq is really a potentially empty
// sxp_expr_seq
sxp_ti_definition: '(' tk_DEFINSTANCE sxp_constraint sxp_optdocstring ')' {
  SHOWPARSE("sxp_ti_definition -> ( DEFINSTANCE sxp_constraint [docstring])");
  $$ = AST::make(at_definstance, $2.loc, $3,
                 AST::make(at_tcmethods, $5.loc),
                 AST::make(at_constraints, $3->loc));
};
sxp_ti_definition: '(' tk_DEFINSTANCE sxp_constraint sxp_optdocstring sxp_method_bindings ')' {
  SHOWPARSE("sxp_ti_definition -> ( DEFINSTANCE sxp_constraint [docstring] sxp_method_bindings)");
  $$ = AST::make(at_definstance, $2.loc, $3, $5,
                 AST::make(at_constraints, $3->loc));
};

sxp_method_bindings: sxp_method_binding {
  SHOWPARSE("sxp_method_bindings -> sxp_method_binding");
  $$ = AST::make(at_tcmethods, $1->loc, $1);
};

sxp_method_bindings: sxp_method_bindings sxp_method_binding {
  SHOWPARSE("sxp_method_bindings -> sxp_method_bindings sxp_method_binding");
  $$ = $1;
  $$->addChild($2);
};

sxp_method_binding: '(' ident ident sxp_expr ')' {
  SHOWPARSE("sxp_method_binding -> ( ident = sxp_expr )");

  if ($3->s != "=") {
    cerr << $2->loc << ": Syntax error, expecting `='.\n";
    lexer->num_errors++;
  }

  $$ = AST::make(at_tcmethod_binding, $1.loc, $2, $4);
};

// DEFINE  [5.1]
sxp_value_definition: '(' tk_DEFINE sxp_defpattern sxp_expr ')'  {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  sxp_defpattern sxp_expr )");
  $$ = AST::make(at_define, $2.loc, $3, $4);
  $$->addChild(AST::make(at_constraints));
};
sxp_value_definition: '(' tk_DEFINE sxp_defpattern sxp_docstring sxp_expr ')'  {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  sxp_defpattern sxp_docstring sxp_expr )");
  $$ = AST::make(at_define, $2.loc, $3, $5);
  $$->addChild(AST::make(at_constraints));
};

// Define convenience syntax case 1: no arguments
// No sxp_docstring here because of sxp_expr_seq
sxp_value_definition: '(' tk_DEFINE '(' defident ')' sxp_expr_seq ')'  {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  ( defident ) [docstring] sxp_expr_seq )");
  $6 = stripDocString($6);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $2.loc, AST::make(at_ident, LToken("__return")), $6);
  shared_ptr<AST> iLambda =
    AST::make(at_lambda, $2.loc, AST::make(at_argVec, $5.loc), iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $4->loc, $4);
  $$ = AST::make(at_recdef, $2.loc, iP, iLambda);
  $$->addChild(AST::make(at_constraints));
};

// Define convenience syntax case 3: one or more arguments
// No sxp_docstring here because of sxp_expr_seq
sxp_value_definition: '(' tk_DEFINE '(' defident sxp_lambdapatterns ')'
                  sxp_expr_seq ')'  {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  ( defident sxp_lambdapatterns ) "
            "[docstring] sxp_expr_seq )");
  $7 = stripDocString($7);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $2.loc, AST::make(at_ident, LToken("__return")), $7);
  shared_ptr<AST> iLambda = AST::make(at_lambda, $2.loc, $5, iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $4->loc, $4);
  $$ = AST::make(at_recdef, $2.loc, iP, iLambda);
  $$->addChild(AST::make(at_constraints));
};

// PROCLAIM DEFINITION -- VALUES [6.2]
sxp_value_declaration: '(' tk_PROCLAIM defident ':' sxp_type sxp_optdocstring sxp_externals ')' {
  SHOWPARSE("sxp_if_definition -> ( PROCLAIM ident : sxp_type sxp_optdocstring sxp_externals )");
  $$ = AST::make(at_proclaim, $2.loc, $3, $5);
  $$->flags |= $7->flags;
  $$->getID()->flags |= $7->flags;
  $$->getID()->externalName = $7->externalName;
  $$->addChild(AST::make(at_constraints));
};

// TODO: The second ident in import rule, and the ident in the provide rule
//  should be restricted to
// ({ALPHA} | [_\-]) (({ALPHA} | {DECDIGIT} | [_\-])*
// IMPORT DEFINITIONS [8.2]

//import_definition: '(' tk_IMPORT ident ifident ')' {
//  SHOWPARSE("sxp_import_definition -> ( IMPORT ident ifident )");
//  shared_ptr<AST> ifIdent = AST::make(at_ifident, $4);
//  ifIdent->uoc = UocInfo::importInterface(lexer->errStream, $4.loc, $4.str);
//  $$ = AST::make(at_import, $2.loc, $3, ifIdent);
//};

sxp_import_definition: '(' tk_IMPORT ifident tk_AS ident ')' {
  SHOWPARSE("sxp_import_definition -> ( IMPORT ident ifident )");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_importAs, $2.loc, ifIdent, $5);
};

sxp_import_definition: '(' tk_IMPORT ifident ')' {
  SHOWPARSE("sxp_import_definition -> (IMPORT sxp_ifident)");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_import, $2.loc, ifIdent);
};

sxp_import_definition: '(' tk_IMPORT ifident sxp_importList ')' {
  SHOWPARSE("sxp_import_definition -> (IMPORT ifident sxp_importList)");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_import, $2.loc, ifIdent);
  $$->addChildrenFrom($4);
};

sxp_importList: sxp_alias {
  SHOWPARSE("sxp_importList -> sxp_alias");
  $$ = AST::make(at_Null, $1->loc, $1);
};
sxp_importList: sxp_importList sxp_alias {
  SHOWPARSE("sxp_importList -> sxp_importList sxp_alias");
  $$ = $1;
  $$->addChild($2);
};

sxp_alias: ident {
  SHOWPARSE("sxp_alias -> ident");
  // The two identifiers in this case are textually the same, but the
  // need to end up with distinct AST nodes, thus getDCopy().
  $$ = AST::make(at_ifsel, $1->loc, $1, $1->getDeepCopy());
};
sxp_alias: '(' ident tk_AS ident ')' {
  SHOWPARSE("sxp_alias -> ( ident AS ident )");

  $$ = AST::make(at_ifsel, $2->loc, $4, $2);
};


// PROVIDE DEFINITION [8.3]
sxp_provide_definition: '(' tk_PROVIDE ifident sxp_provideList ')' {
  SHOWPARSE("sxp_provide_definition -> (PROVIDE ifident sxp_provideList)");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_provide, $2.loc, ifIdent);
  $$->addChildrenFrom($4);
};

sxp_provideList: ident {
  SHOWPARSE("sxp_provideList -> ident");
  $$ = AST::make(at_Null, $1->loc, $1);
};

sxp_provideList: sxp_provideList ident {
  SHOWPARSE("sxp_provideList -> sxp_provideList ident");
  $$ = $1;
  $$->addChild($2);
};

// definition: '(' tk_DEFTHM ident sxp_expr ')'  {
//    SHOWPARSE("definition -> ( DEFTHM ident sxp_expr )");
//    $$ = AST::make(at_defthm, $2.loc, $3, $4);
// };

sxp_declares: {
  SHOWPARSE("sxp_declares -> <empty>");
  $$ = AST::make(at_declares);
};
sxp_declares: sxp_declares sxp_declare {
  SHOWPARSE("sxp_declares -> sxp_declares sxp_declare");
  $$ = $1;
  $$->addChildrenFrom($2);
};

sxp_declare: '(' tk_DECLARE sxp_decls ')' {
  SHOWPARSE("sxp_declare -> ( DECLARE sxp_decls )");
  $$ = $3;
};

sxp_decls: sxp_decl {
  SHOWPARSE("sxp_decls -> sxp_decl");
  $$ = AST::make(at_declares, $1->loc, $1);
};

sxp_decls: sxp_decls sxp_decl {
  SHOWPARSE("sxp_decls -> sxp_decls sxp_decl");
  $$ = $1;
  $$->addChild($2);
};

sxp_decl: '(' ident sxp_field_type ')' {
  SHOWPARSE("sxp_decl -> ( ident sxp_field_type )");
  $$ = AST::make(at_declare, $2->loc, $2, $3);
};
//decl: '(' ident ')' {
//  SHOWPARSE("sxp_decl -> ( ident )");
//  $$ = AST::make(at_declare, $2->loc, $2);
//};
sxp_decl: ident {
  SHOWPARSE("sxp_decl -> ident");
  $$ = AST::make(at_declare, $1->loc, $1);
};


/* defunion Constructors */
sxp_constructors: sxp_constructor {
  SHOWPARSE("sxp_constructors -> sxp_constructor");
  $$ = AST::make(at_constructors, $1->loc, $1);
};
sxp_constructors: sxp_constructors sxp_constructor {
  SHOWPARSE("sxp_constructors -> sxp_constructors sxp_constructor");
  $$ = $1;
  $$->addChild($2);
};
sxp_constructor: ident {                          /* simple sxp_constructor */
  SHOWPARSE("sxp_constructor -> defident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
};
sxp_constructor: '(' ident sxp_fields ')' {  /* compound sxp_constructor */
  SHOWPARSE("sxp_constructor ->  ( ident sxp_fields )");
  $2->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $2->loc, $2);
  $$->addChildrenFrom($3);
};

/* defrepr Constructors */
sxp_repr_constructors: sxp_repr_constructor {
  SHOWPARSE("sxp_repr_constructors -> sxp_repr_constructor");
  $$ = AST::make(at_reprctrs, $1->loc, $1);
};
sxp_repr_constructors: sxp_repr_constructors sxp_repr_constructor {
  SHOWPARSE("sxp_repr_constructors -> sxp_repr_constructors sxp_repr_constructor");
  $$ = $1;
  $$->addChild($2);
};
/* repr_constructor: ident sxp_repr_reprs {                          /\* simple sxp_constructor *\/  */
/*   SHOWPARSE("sxp_repr_constructor -> defident"); */
/*   $1->flags |= (ID_IS_GLOBAL); */
/*   $$ = AST::make(at_reprctr, $1->loc, $1); */
/* }; */
sxp_repr_constructor: '(' ident sxp_fields '(' tk_WHERE sxp_repr_reprs ')' ')' {  /* compound sxp_constructor */
  SHOWPARSE("sxp_repr_constructor ->  ( ident sxp_fields ( WHERE sxp_repr_reprs ) )");
  $2->flags |= (ID_IS_GLOBAL);
  shared_ptr<AST> ctr = AST::make(at_constructor, $2->loc, $2);
  ctr->addChildrenFrom($3);
  $$ = AST::make(at_reprctr, $2->loc, ctr);
  $$->addChildrenFrom($6);
};

sxp_repr_reprs: sxp_repr_repr {
  SHOWPARSE("sxp_repr_reprs -> sxp_repr_repr");
  $$ = AST::make(at_Null, $1->loc, $1);
};
sxp_repr_reprs: sxp_repr_reprs sxp_repr_repr {
  SHOWPARSE("sxp_repr_reprs -> sxp_repr_reprs sxp_repr_repr");
  $$ = $1;
  $$->addChild($2);
};
sxp_repr_repr: '(' ident ident intLit')' {
  SHOWPARSE("sxp_repr_repr ->  ( = ident intLit )");

  if ($2->s != "==") {
    cerr << $2->loc << ": Syntax error, expecting `=='.\n";
    lexer->num_errors++;
  }

  $$ = AST::make(at_reprrepr, $2->loc, $3, $4);
};


/* defstruct / sxp_constructor / exception sxp_fields */
sxp_fields: sxp_field  {
  SHOWPARSE("sxp_fields -> sxp_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

sxp_fields: sxp_fields sxp_field {
  SHOWPARSE("sxp_fields -> sxp_fields sxp_field ");
  $$ = $1;
  $$->addChild($2);
};

sxp_field: ident ':' sxp_field_type  {
  SHOWPARSE("sxp_field -> ident : sxp_field_type");
  $$ = AST::make(at_field, $1->loc, $1, $3);
};

sxp_field: '(' tk_THE sxp_field_type ident ')'  {
  SHOWPARSE("sxp_field -> '(' THE sxp_field_type ident ')'");
  $$ = AST::make(at_field, $1.loc, $4, $3);
};

sxp_field: '(' tk_FILL sxp_bitfieldtype ')'  {
  SHOWPARSE("sxp_field -> '(' FILL sxp_bitfieldtype ')'");
  $$ = AST::make(at_fill, $1.loc, $3);
};

// Some low level data structures have reserved bit positions that are
// required to hold designated values.
sxp_field: '(' tk_RESERVED sxp_bitfieldtype intLit ')'  {
  SHOWPARSE("sxp_field -> '(' RESERVED sxp_bitfieldtype intLit ')'");
  $$ = AST::make(at_fill, $1.loc, $3, $4);
};

sxp_methdecl: ident ':' sxp_method_type  {
  SHOWPARSE("sxp_methdecl -> ident : sxp_method_type");
  $$ = AST::make(at_methdecl, $1->loc, $1, $3);
};

sxp_methods_only: sxp_methdecl  {
  SHOWPARSE("sxp_methods_only -> sxp_methdecl");
  $$ = AST::make(at_fields, $1->loc, $1);
};

sxp_methods_only: sxp_methods_only sxp_methdecl  {
  SHOWPARSE("sxp_methods_only -> sxp_methods_only sxp_methdecl");
  $$ = $1;
  $$->addChild($2);
};

sxp_fields_and_methods: sxp_methdecl  {
  SHOWPARSE("sxp_fields_and_methods -> sxp_methdecl");
  $$ = AST::make(at_fields, $1->loc, $1);
};

sxp_fields_and_methods: sxp_field  {
  SHOWPARSE("sxp_fields_and_methods -> sxp_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

sxp_fields_and_methods: sxp_fields_and_methods sxp_methdecl {
  SHOWPARSE("sxp_fields_and_methods -> sxp_fields_and_methods sxp_methdecl ");
  $$ = $1;
  $$->addChild($2);
};

sxp_fields_and_methods: sxp_fields_and_methods sxp_field {
  SHOWPARSE("sxp_fields_and_methods -> sxp_fields_and_methods sxp_field ");
  $$ = $1;
  $$->addChild($2);
};

sxp_tvlist: sxp_typevar  {
  SHOWPARSE("sxp_tvlist -> sxp_typevar");
  $$ = AST::make(at_tvlist, $1->loc, $1);
};
sxp_tvlist: sxp_tvlist sxp_typevar {
  SHOWPARSE("sxp_tvlist -> sxp_tvlist sxp_typevar");
  $$ = $1;
  $1->addChild($2);
};

// TYPES [3]
sxp_types: sxp_type  {
  SHOWPARSE("sxp_types -> sxp_type");
  $$ = AST::make(at_Null);
  $$->addChild($1);
};
sxp_types: sxp_types sxp_type {
  SHOWPARSE("sxp_types -> sxp_types sxp_type");
  $$ = $1;
  $1->addChild($2);
};

sxp_type: useident  {                         /* previously defined sxp_type */
  SHOWPARSE("sxp_type -> useident");
  $$ = $1;
};

// PRIMARY TYPES [3.2]           
sxp_type: '(' ')' {
  SHOWPARSE("sxp_type -> ( )");
  $$ = AST::make(at_primaryType, $1.loc);
  $$->s = "unit";                /* for lookup! */
};

bool_type: tk_BOOL {
  SHOWPARSE("bool_type -> BOOL");
  $$ = AST::make(at_primaryType, $1);
};

sxp_type: bool_type {
  SHOWPARSE("sxp_type -> bool_type");
  $$ = $1;
};

sxp_type: tk_CHAR {
  SHOWPARSE("sxp_type -> CHAR");
  $$ = AST::make(at_primaryType, $1);
};
sxp_type: tk_STRING {
  SHOWPARSE("sxp_type -> STRING");
  $$ = AST::make(at_primaryType, $1);
};

sxp_int_type: tk_INT8 {
  SHOWPARSE("sxp_int_type -> INT8");
  $$ = AST::make(at_primaryType, $1);
};
sxp_int_type: tk_INT16 {
  SHOWPARSE("sxp_int_type -> INT16");
  $$ = AST::make(at_primaryType, $1);
};
sxp_int_type: tk_INT32 {
  SHOWPARSE("sxp_int_type -> INT32");
  $$ = AST::make(at_primaryType, $1);
};
sxp_int_type: tk_INT64 {
  SHOWPARSE("sxp_int_type -> INT64");
  $$ = AST::make(at_primaryType, $1);
};
sxp_uint_type: tk_UINT8 {
  SHOWPARSE("sxp_uint_type -> UINT8");
  $$ = AST::make(at_primaryType, $1);
};
sxp_uint_type: tk_UINT16 {
  SHOWPARSE("sxp_uint_type -> UINT16");
  $$ = AST::make(at_primaryType, $1);
};
sxp_uint_type: tk_UINT32 {
  SHOWPARSE("sxp_uint_type -> UINT32");
  $$ = AST::make(at_primaryType, $1);
};
sxp_uint_type: tk_UINT64 {
  SHOWPARSE("sxp_type -> UINT64");
  $$ = AST::make(at_primaryType, $1);
};

sxp_any_int_type: sxp_int_type {
  SHOWPARSE("sxp_any_int_type -> sxp_int_type");
  $$ = $1;
};
sxp_any_int_type: sxp_uint_type {
  SHOWPARSE("sxp_any_int_type -> sxp_uint_type");
  $$ = $1;
};
sxp_any_int_type: tk_WORD {
  SHOWPARSE("sxp_any_int_type -> WORD");
  $$ = AST::make(at_primaryType, $1);
};

sxp_float_type: tk_FLOAT {
  SHOWPARSE("sxp_float_type -> FLOAT");
  $$ = AST::make(at_primaryType, $1);
};
sxp_float_type: tk_DOUBLE {
  SHOWPARSE("sxp_float_type -> DOUBLE");
  $$ = AST::make(at_primaryType, $1);
};
sxp_float_type: tk_QUAD {
  SHOWPARSE("sxp_float_type -> QUAD");
  $$ = AST::make(at_primaryType, $1);
};

sxp_type: sxp_any_int_type {
  SHOWPARSE("sxp_type -> sxp_any_int_type");
  $$ = $1;
};
sxp_type: sxp_float_type {
  SHOWPARSE("sxp_type -> sxp_float_type");
  $$ = $1;
};

// EXCEPTION sxp_type
sxp_type: tk_EXCEPTION {
  SHOWPARSE("sxp_type -> EXCEPTION");
  $$ = AST::make(at_exceptionType, $1.loc);
};

// TYPE VARIABLES [3.3]          
sxp_type: sxp_typevar  {                 
  SHOWPARSE("sxp_type -> sxp_typevar");
  $$ = $1;
};

// REF TYPES [3.4.1]             
sxp_type: '(' tk_REF sxp_type ')' {
  SHOWPARSE("sxp_type -> ( REF sxp_type )");
  $$ = AST::make(at_refType, $2.loc, $3);
};

// VAL TYPES [3.4.2]
sxp_type: '(' tk_VAL sxp_type ')' {
  SHOWPARSE("sxp_type -> ( VAL sxp_type )");
  $$ = AST::make(at_valType, $2.loc, $3);
};

// FUNCTION TYPES [3.4.3]
sxp_type: sxp_fntype {
  SHOWPARSE("sxp_type -> sxp_fntype");
  $$ = $1;
}

sxp_fneffect: {
  SHOWPARSE("sxp_fneffect -> <empty>");
  $$ = AST::make(at_ident, LToken("impure"));
};

sxp_fneffect: tk_PURE {
  SHOWPARSE("sxp_fneffect -> PURE");
  $$ = AST::make(at_ident, $1);
};

sxp_fneffect: tk_IMPURE {
  SHOWPARSE("sxp_fneffect -> IMPURE");
  $$ = AST::make(at_ident, $1);
};

sxp_fneffect: tk_EffectVar {
  SHOWPARSE("sxp_fneffect -> <EffectVar=" + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

// Reworked by shap on 10/9/2008 to use arrow syntax
sxp_fntype: '(' sxp_fneffect tk_FN tk_FNARROW sxp_type ')' {
  SHOWPARSE("sxp_fntype -> ( sxp_fneffect FN -> sxp_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $4.loc);
  $$ = AST::make(at_fn, $1.loc, fnargVec, $5);
};
//fntype: '(' sxp_fneffect tk_FN '(' ')' sxp_type ')' {
//  SHOWPARSE("sxp_fntype -> ( sxp_fneffect FN () sxp_type )");
//  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $4.loc);
//  $$ = AST::make(at_fn, $1.loc, fnargVec, $6);
//};

// Reworked by shap on 10/9/2008 to use arrow syntax
sxp_fntype: '(' sxp_fneffect tk_FN sxp_types_pl_byref tk_FNARROW sxp_type ')'  {
  SHOWPARSE("sxp_fntype -> ( sxp_fneffect FN sxp_types_pl_byref -> sxp_type )");
  $$ = AST::make(at_fn, $1.loc, $4, $6);
};
//fntype: '(' sxp_fneffect tk_FN '(' sxp_types_pl_byref ')' sxp_type ')'  {
//  SHOWPARSE("sxp_fntype -> ( sxp_fneffect FN ( sxp_types_pl_byref ) sxp_type )");
//  $$ = AST::make(at_fn, $1.loc, $5, $7);
//};

// METHOD TYPES [3.9]
// Note: not complete; need methods taking no arguments.
sxp_method_type: '(' sxp_fneffect tk_METHOD tk_FNARROW sxp_type ')' {
  SHOWPARSE("methodtype -> ( sxp_fneffect METHOD -> sxp_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $4.loc);
  $$ = AST::make(at_methType, $1.loc, fnargVec, $5);
};

sxp_method_type: '(' sxp_fneffect tk_METHOD sxp_types_pl_byref tk_FNARROW sxp_type ')' {
  SHOWPARSE("methodtype -> ( sxp_fneffect METHOD sxp_types_pl_byref -> sxp_type )");
  $$ = AST::make(at_methType, $1.loc, $4, $6);
};

/* type: '(' tk_METHOD tvapp fntype')' { */
/*   SHOWPARSE("METHOD tcapp sxp_fntype )"); */
/*   shared_ptr<AST> tcreqs = AST::make(at_tcreqs, $3->loc, $3); */
/*   $$ = AST::make(at_methodType, $2.loc, tcreqs, $4); */
/* }; */

/* type: '(' tk_METHOD '(' tk_AND tcreqs ')' fntype')' { */
/*   SHOWPARSE("tcreq -> ( var sxp_tvlist )"); */
/*   $$ = AST::make(at_methodType, $2.loc, $5, $7); */
/* }; */

sxp_type_cpair: sxp_type ',' sxp_type {
  SHOWPARSE("sxp_type_cpair -> sxp_type ',' sxp_type");
  $$ = AST::make(at_typeapp, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};
sxp_type_cpair: sxp_type ',' sxp_type_cpair {
  SHOWPARSE("sxp_type_cpair -> sxp_type ',' sxp_type_cpair");
  $$ = AST::make(at_typeapp, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};

sxp_type: '(' sxp_type_cpair ')' {
  SHOWPARSE("sxp_type -> sxp_type_cpair");
  $$ = $2;
};

// ARRAY TYPE [3.5.1]               
sxp_type: '(' tk_ARRAY sxp_type intLit ')'  {
  SHOWPARSE("sxp_type -> ( ARRAY sxp_type intLit )");
  $$ = AST::make(at_arrayType, $2.loc, $3, $4);
};
// VECTOR TYPE [3.5.2]             
sxp_type: '(' tk_VECTOR sxp_type ')' {
  SHOWPARSE("sxp_type -> (VECTOR sxp_type )");
  $$ = AST::make(at_vectorType, $2.loc, $3);
};

// TYPE CONSTRUCTORS (typeapp)
sxp_type: sxp_typeapp {
  SHOWPARSE("sxp_type -> sxp_typeapp");
  $$ = $1;
};

sxp_typeapp: '(' useident sxp_types ')' {
  SHOWPARSE("sxp_typeapp -> ( useident sxp_types )");
  $$ = AST::make(at_typeapp, $2->loc, $2);
  $$->addChildrenFrom($3);
};

// MUTABLE TYPE [3.7]            
sxp_type: '(' tk_MUTABLE sxp_type ')' {
  SHOWPARSE("sxp_type -> ( MUTABLE sxp_type )");
  $$ = AST::make(at_mutableType, $2.loc, $3);
};

sxp_type: '(' tk_CONST sxp_type ')' {
  SHOWPARSE("sxp_type -> ( CONST sxp_type )");
  $$ = AST::make(at_constType, $2.loc, $3);
  };

// BITFIELD TYPE
sxp_bitfieldtype: '(' tk_BITFIELD sxp_any_int_type intLit ')' {
  SHOWPARSE("sxp_bitfieldtype -> ( BITFIELD sxp_any_int_type intLit )");
  $$ = AST::make(at_bitfield, $2.loc, $3, $4);
};
sxp_bitfieldtype: '(' tk_BITFIELD bool_type intLit ')' {
  SHOWPARSE("sxp_bitfieldtype -> ( BITFIELD bool_type intLit )");
  $$ = AST::make(at_bitfield, $2.loc, $3, $4);
};

// Any-type, including bitfield sxp_type
sxp_field_type: sxp_bitfieldtype {
  SHOWPARSE("sxp_field_type -> sxp_bitfieldtype");
  $$ = $1;
};

sxp_field_type: sxp_type {
  SHOWPARSE("sxp_field_type -> sxp_type");
  $$ = $1;
};

// by-ref sxp_types are not a part of general `type' rule.
// They are gramatiocally restricted to apprae only on
// formal function arguments and function types.
sxp_type_pl_byref: sxp_type {
  SHOWPARSE("sxp_type_pl_byref -> sxp_type");
  $$ = $1;
};

sxp_type_pl_byref: '(' tk_BY_REF sxp_type ')' {
  SHOWPARSE("sxp_type_pl_byref -> ( BY-REF sxp_type )");
  $$ = AST::make(at_byRefType, $2.loc, $3);
};

sxp_type_pl_byref: '(' tk_ARRAY_REF sxp_type ')' {
  SHOWPARSE("sxp_type_pl_byref -> ( ARRAY-BY-REF sxp_type )");
  $$ = AST::make(at_arrayRefType, $2.loc, $3);
};

sxp_types_pl_byref: sxp_type_pl_byref {
  SHOWPARSE("sxp_types_pl_byref -> sxp_type_pl_byref");
  $$ = AST::make(at_fnargVec);
  $$->addChild($1);
};
sxp_types_pl_byref: sxp_types_pl_byref sxp_type_pl_byref {
  SHOWPARSE("sxp_types_pl_byref -> sxp_types_pl_byref sxp_type_pl_byref");
  $$ = $1;
  $1->addChild($2);
};

// BINDING PATTERNS [5.1]
sxp_bindingpattern: ident {
  SHOWPARSE("sxp_bindingpattern -> ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
sxp_bindingpattern: ident ':' sxp_type {
  SHOWPARSE("sxp_bindingpattern -> ident : sxp_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
};
sxp_bindingpattern: '(' tk_THE sxp_type ident ')' {
  SHOWPARSE("sxp_bindingpattern -> ident : sxp_type");
  $$ = AST::make(at_identPattern, $1.loc, $4, $3);
};

// There are no sxp_defpattern sequences, because there is no top-level
// pattern application
// DEFPATTERN
sxp_defpattern: defident {
  SHOWPARSE("sxp_defpattern -> defident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
sxp_defpattern: defident ':' sxp_type {
  SHOWPARSE("sxp_defpattern -> defident : sxp_qual_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
};
sxp_defpattern: '(' tk_THE sxp_type defident ')' {
  SHOWPARSE("sxp_defpattern -> (THE sxp_qual_type sxp_defident)");
  $$ = AST::make(at_identPattern, $1.loc, $4, $3);
};


/* Lambda Patterns -- with an additional by-ref annotation */
sxp_lambdapatterns: sxp_lambdapattern {
  SHOWPARSE("sxp_lambdapatterns -> sxp_lambdapattern");
  $$ = AST::make(at_argVec, $1->loc);
  $$->addChild($1);
};
sxp_lambdapatterns: sxp_lambdapatterns sxp_lambdapattern {
  SHOWPARSE("sxp_lambdapatterns -> sxp_lambdapatterns sxp_lambdapattern");
  $$ = $1;
  $$->addChild($2);
};

sxp_lambdapattern: ident {
  SHOWPARSE("sxp_lambdapattern -> ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};

sxp_lambdapattern: ident ':' sxp_type_pl_byref {
  SHOWPARSE("sxp_lambdapattern -> ident : sxp_type_pl_byref");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
  if ($3->astType == at_byRefType)
    $1->flags |= ARG_BYREF;
};

sxp_lambdapattern: '(' tk_THE sxp_type ident ')' {
  SHOWPARSE("sxp_lambdapattern -> ( THE sxp_type ident ) ");
  $$ = AST::make(at_identPattern, $1.loc, $4, $3);
};

sxp_lambdapattern: '(' tk_THE '(' tk_BY_REF sxp_type ')' ident ')' {
  SHOWPARSE("sxp_lambdapattern -> ( THE ( BY-REF sxp_type ) ident )");
  $$ = AST::make(at_identPattern, $1.loc, $7, $5);
  $5->flags |= ARG_BYREF;
};

sxp_lambdapattern: '(' tk_THE '(' tk_ARRAY_REF sxp_type ')' ident ')' {
  SHOWPARSE("sxp_lambdapattern -> ( THE ( ARRAY-REF sxp_type ) ident )");
  $$ = AST::make(at_identPattern, $1.loc, $7, $5);
};

// EXPRESSIONS [7]
//
// sxp_expr   -- an expression form with an optional sxp_type qualifier
// sxp_eform  -- a naked expression form
//
// As a practical matter, every expression on the RHS of a production
// should be an sxp_expr

sxp_expr_seq: sxp_expr {
  SHOWPARSE("sxp_expr_seq -> sxp_expr");
  $$ = AST::make(at_begin, $1->loc, $1);
  $$->printVariant = pf_IMPLIED;
};
sxp_expr_seq: sxp_value_definition {
  SHOWPARSE("sxp_expr_seq -> sxp_value_definition");
  $$ = AST::make(at_begin, $1->loc, $1);
  $$->printVariant = pf_IMPLIED;
};
sxp_expr_seq: sxp_expr_seq sxp_expr {
  SHOWPARSE("sxp_expr_seq -> sxp_expr_seq sxp_expr");
  $$ = $1;
  $$->addChild($2);
};
sxp_expr_seq: sxp_expr_seq sxp_value_definition {
  SHOWPARSE("sxp_expr_seq -> sxp_expr_seq sxp_value_definition");
  $$ = $1;
  $$->addChild($2);
};
//expr_seq: sxp_value_definition sxp_expr_seq {
//  // AST define = identPattern sxp_expr constraints;
//  SHOWPARSE("sxp_expr_seq -> sxp_value_definition sxp_expr_seq");
//  shared_ptr<AST> letBinding = AST::make(at_letbinding, $1->loc,
//                            $1->child(0), $1->child(1));
//  $$ = AST::make(at_letrec, $1->loc,
//               AST::make(at_letbindings, $1->loc, letBinding),
//               $2,
//               $1->child(2));
//};


// TYPE QUALIFIED EXPRESSIONS  [7.3]
sxp_expr: sxp_eform {
  SHOWPARSE("sxp_expr -> sxp_eform");
  $$ = $1;
};
sxp_expr: sxp_eform ':' sxp_type {
  SHOWPARSE("sxp_expr -> sxp_eform : sxp_type");
  $$ = AST::make(at_tqexpr, $1->loc, $1, $3);
};
sxp_expr: sxp_the_expr {
  SHOWPARSE("sxp_expr -> sxp_the_expr");
  $$ = $1;
};

sxp_the_expr: '(' tk_THE sxp_type sxp_eform ')' {
  SHOWPARSE("sxp_the_expr -> ( THE sxp_type sxp_eform )");
  // Note: argument order swapped for historical reasons.
  $$ = AST::make(at_tqexpr, $2.loc, $4, $3);
};

// SUSPENDED EXPRESSIONS
sxp_expr: '(' tk_SUSPEND useident sxp_expr ')' {
  SHOWPARSE("sxp_expr -> ( SUSPEND useident sxp_expr )");
  $$ = AST::make(at_suspend, $2.loc, $3, $4);
};

// LITERALS  [7.1]
sxp_eform: sxp_literal {
  SHOWPARSE("sxp_eform -> Literal");
  $$ = $1;
};

sxp_eform: '(' tk_SIZEOF sxp_type ')' {
  SHOWPARSE("sxp_eform -> (SIZEOF sxp_type)");
  $$ = AST::make(at_sizeof, $2.loc, $3);
};

sxp_eform: '(' tk_BITSIZEOF sxp_type ')' {
  SHOWPARSE("sxp_eform -> (BITSIZEOF sxp_type)");
  $$ = AST::make(at_bitsizeof, $2.loc, $3);
};

// UNIT EXPRESSIONS   [7.4.1]
sxp_eform: '(' ')' {
  SHOWPARSE("sxp_eform -> ()");
  $$ = AST::make(at_unit, $1.loc);
};

// Expressions that involve locations:

// IDENTIFIERS [7.2]
/* This would actually have been
sxp_eform: useident {
  SHOWPARSE("sxp_eform -> useident");
  $$ = $1;
};
but for the ambiguity with record (field) selection.
So, the burden is now passed to further stages */

sxp_eform: ident {
  SHOWPARSE("sxp_eform -> ident");
  $$ = $1;
};

// MEMBER [7.9]
// In principle, we would like to accept expr.ident here, but that
// creates a parse conflict with expr:type, because the sequence
//
//    sxp_expr : sxp_type . ident
//
// can turn into:
//
//    sxp_expr : Id . Id . ident
//           ^^^^^^^
//             sxp_type
//
// which creates a shift-reduce conflict. It's all fine as long as the
// sxp_expr is fully bracketed, so there is no problem accepting:
//
//    (the sxp_type sxp_expr).ident
//
// We have adopted the solution of declaring that the ":" convenience
// syntax cannot be used inside a member selection. This is not likely
// to be burdensome. If the sxp_expr on the LHS is a locally computed
// result, the sxp_type will be inferred through chaining from the
// sxp_constructor expression. The only case where this is likely to be
// annoying is for arguments of structure type, but we do not infer
// structure sxp_types from sxp_field names in any case, so those will
// probably require argument declarations in any case.
sxp_eform: sxp_eform '.' ident {
  SHOWPARSE("sxp_eform -> sxp_eform . ident");
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

sxp_eform: sxp_the_expr '.' ident {
  SHOWPARSE("sxp_eform -> sxp_the_expr . ident");
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

sxp_eform: '(' tk_MEMBER sxp_expr ident ')' {
  SHOWPARSE("sxp_eform -> ( member sxp_expr ident )");
  $$ = AST::make(at_select, $2.loc, $3, $4);
};

// NTH-REF [7.11.2]         
sxp_eform: sxp_expr '[' sxp_expr ']' {
  SHOWPARSE("sxp_eform -> sxp_expr [ sxp_expr ]");
  $$ = AST::make(at_nth, $1->loc, $1, $3);
};

sxp_eform: '(' tk_ARRAY_NTH sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( ARRAY-NTH sxp_expr sxp_expr )");
  $$ = AST::make(at_array_nth, $2.loc, $3, $4);
};
sxp_eform: '(' tk_ARRAY_REF_NTH sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( ARRAY-REF-NTH sxp_expr sxp_expr )");
  $$ = AST::make(at_array_ref_nth, $2.loc, $3, $4);
};
sxp_eform: '(' tk_VECTOR_NTH sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( VECTOR-NTH sxp_expr sxp_expr )");
  $$ = AST::make(at_vector_nth, $2.loc, $3, $4);
};

// DUP [7.17.1]
sxp_eform: '(' tk_DUP sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( DUP sxp_expr )");
  $$ = AST::make(at_dup, $2.loc, $3);
};

// DEREF [7.17.2]               
sxp_eform: sxp_expr '^' {
  SHOWPARSE("sxp_eform -> sxp_expr ^");
  $$ = AST::make(at_deref, $1->loc, $1);
  $$->printVariant = pf_IMPLIED;
};
sxp_eform: '(' tk_DEREF sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( DEREF sxp_expr )");
  $$ = AST::make(at_deref, $2.loc, $3);
};

// INNER-REF
// In the case of structures, the second "sxp_expression"
// must be a label. This cannot be checked until
// type-checking phase.
/* eform: '(' tk_INNER_REF sxp_expr sxp_expr ')' { */
/*   SHOWPARSE("sxp_eform -> ( INNER_REF sxp_expr sxp_expr)"); */
/*   $$ = AST::make(at_inner_ref, $2.loc, $3, $4); */
/* }; */

// End of locations

// PAIR EXPRESSIONS
sxp_eform_cpair: sxp_expr ',' sxp_expr {
  SHOWPARSE("sxp_eform_cpair -> sxp_expr ',' sxp_expr");
  $$ = AST::make(at_apply, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};
sxp_eform_cpair: sxp_expr ',' sxp_eform_cpair {
  SHOWPARSE("sxp_eform_cpair -> sxp_expr ',' sxp_eform_cpair");
  $$ = AST::make(at_apply, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};
sxp_eform: '(' sxp_eform_cpair ')' {
  SHOWPARSE("sxp_eform -> ( sxp_eform_cpair )");
  $$ = $2;
};

sxp_eform: '(' tk_MAKE_VECTORL sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( MAKE-VECTOR sxp_expr sxp_expr )");
  $$ = AST::make(at_makevectorL, $2.loc, $3, $4);
};

// VECTORS [7.4.3]
sxp_eform: '(' tk_VECTOR sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> (VECTOR sxp_expr_seq)");
  $$ = $3;
  $$->astType = at_vector;
  $$->loc = $2.loc;
};

// ARRAYS [7.4.3]
sxp_eform: '(' tk_ARRAY sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> (ARRAY sxp_expr_seq)");
  $$ = $3;
  $$->astType = at_array;
  $$->loc = $2.loc;
};

// BEGIN [7.5]
sxp_eform: '(' tk_BEGIN sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> ( BEGIN sxp_expr_seq )");
  $$ = $3;
  $$->loc = $2.loc;
  $$->astType = at_begin;
};

// LABELS and LABELED EXIT [7.6]
sxp_eform: '(' tk_BLOCK ident sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> (BLOCK defident sxp_expr)");
  $$ = AST::make(at_block, $2.loc, $3, $4);
}

sxp_eform: '(' tk_RETURN_FROM ident sxp_expr ')' {
  SHOWPARSE("sxp_eform -> (RETURN-FROM ident sxp_expr)");
  $$ = AST::make(at_return_from, $2.loc, $3, $4);
}

// ARRAY-LENGTH [7.11.1]
sxp_eform: '(' tk_ARRAY_LENGTH sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( ARRAY-LENGTH sxp_expr )");
  $$ = AST::make(at_array_length, $2.loc, $3);
};
sxp_eform: '(' tk_ARRAY_REF_LENGTH sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( ARRAY-REF-LENGTH sxp_expr )");
  $$ = AST::make(at_array_ref_length, $2.loc, $3);
};
// VECTOR-LENGTH [7.11.1]
sxp_eform: '(' tk_VECTOR_LENGTH sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( VECTOR-LENGTH sxp_expr )");
  $$ = AST::make(at_vector_length, $2.loc, $3);
};

// LAMBDA [7.12]
// handles unit argument
//eform: '(' tk_LAMBDA sxp_lambdapattern sxp_expr_seq ')'  {
//  SHOWPARSE("lambda -> ( LAMBDA sxp_lambdapattern sxp_expr_seq )");
//  $4->astType = at_ibegin;
//  $$ = AST::make(at_xlambda, $2.loc, $3, $4);
//};
// convenience syntax: multiple arguments
sxp_eform: '(' tk_LAMBDA '(' ')' sxp_expr_seq ')'  {
  SHOWPARSE("lambda -> ( LAMBDA sxp_lambdapatterns sxp_expr_seq )");
  if ($5->children.size() == 1 && $5->child(0)->astType == at_begin)
    $5 = $5->child(0);
  shared_ptr<AST> argVec = AST::make(at_argVec, $3.loc);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $2.loc, AST::make(at_ident, LToken("__return")), $5);
  $$ = AST::make(at_lambda, $2.loc, argVec, iRetBlock);
};

sxp_eform: '(' tk_LAMBDA '(' sxp_lambdapatterns ')' sxp_expr_seq ')'  {
  SHOWPARSE("lambda -> ( LAMBDA sxp_lambdapatterns sxp_expr_seq )");
  if ($6->children.size() == 1 && $6->child(0)->astType == at_begin)
    $6 = $6->child(0);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $2.loc, AST::make(at_ident, LToken("__return")), $6);
  $$ = AST::make(at_lambda, $2.loc, $4, iRetBlock);
};

// RETURN [7.13]         
sxp_eform: '(' tk_RETURN sxp_expr ')' {
  SHOWPARSE("sxp_eform -> (RETURN sxp_expr)");
  $$ = AST::make(at_return_from, $2.loc,
                 AST::make(at_ident, LToken("__return")), $3);
}

// APPLICATION [7.14]         
sxp_eform: '(' sxp_expr ')' { /* apply to zero args */
  SHOWPARSE("sxp_eform -> ( sxp_expr )");
  $$ = AST::make(at_apply, $2->loc, $2);
};
sxp_eform: '(' sxp_expr sxp_expr_seq ')' { /* apply to one or more args */
  SHOWPARSE("sxp_eform -> ( sxp_expr sxp_expr_seq )");
  $$ = AST::make(at_apply, $2->loc, $2);
  $$->addChildrenFrom($3);
};

// IF [7.15.1]
sxp_eform: '(' tk_IF sxp_expr sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_eform -> (IF sxp_expr sxp_expr sxp_expr )");
  $$ = AST::make(at_if, $2.loc, $3, $4, $5);
};

// WHEN [7.15.2]
sxp_eform: '(' tk_WHEN sxp_expr sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> (WHEN sxp_expr_seq )");
  $$ = AST::make(at_when, $2.loc, $3, $4);
};

// NOT [7.15.3]

// AND [7.15.4]                 
sxp_eform: '(' tk_AND sxp_expr_seq ')'  {
  SHOWPARSE("sxp_eform -> ( AND sxp_expr_seq )");
  $$ = $3;
  $$->loc = $2.loc;
  $$->astType = at_and;
};

// OR [7.15.5]
sxp_eform: '(' tk_OR sxp_expr_seq ')'  {
  SHOWPARSE("sxp_eform -> ( OR sxp_expr_seq )");
  $$ = $3;
  $$->loc = $2.loc;
  $$->astType = at_or;
};

// COND [7.15.6]          
sxp_eform: '(' tk_COND sxp_condcases sxp_otherwise ')'  {
  SHOWPARSE("sxp_eform -> (COND  ( sxp_condcases sxp_otherwise ) ) ");
  $4->astType = at_condelse;
  $$ = AST::make(at_cond, $2.loc, $3, $4);
};

sxp_condcases: sxp_condcase {
  SHOWPARSE("sxp_condcases -> sxp_condcase");
  $$ = AST::make(at_cond_legs, $1->loc, $1);
};

sxp_condcases: sxp_condcases sxp_condcase {
  SHOWPARSE("sxp_condcases -> sxp_condcases sxp_condcase");
  $$ = $1;
  $$->addChild($2);
};

sxp_condcase: '(' sxp_expr sxp_expr_seq ')'  {
  SHOWPARSE("sxp_condcase -> ( sxp_expr sxp_expr_seq )");
  $$ = AST::make(at_cond_leg, $1.loc, $2, $3);
};

// SET! [7.16]                
sxp_eform: '(' tk_SET sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( SET! sxp_expr sxp_expr )");
  $$ = AST::make(at_setbang, $2.loc, $3, $4);
};

// SWITCH
sxp_eform: '(' tk_SWITCH ident sxp_expr sxp_sw_legs sxp_ow ')' {
  SHOWPARSE("sxp_eform -> ( SWITCH ident sxp_expr sxp_sw_legs sxp_ow)");
  $$ = AST::make(at_uswitch, $2.loc, $3, $4, $5, $6);
  for (size_t c =0; c < $5->children.size(); c++) {
    shared_ptr<AST> sw_leg = $5->child(c);
    sw_leg->children.insert(sw_leg->children.begin(),
                            $3->getDeepCopy());
  }
  if ($6->astType == at_otherwise) {
    shared_ptr<AST> ow = $6;
    ow->children.insert(ow->children.begin(),
                        $3->getDeepCopy());
  }
};

sxp_sw_legs: sxp_sw_leg {
  SHOWPARSE("sxp_sw_legs -> sxp_sw_leg");
  $$ = AST::make(at_usw_legs, $1->loc, $1);
};

sxp_sw_legs: sxp_sw_legs sxp_sw_leg {
  SHOWPARSE("sxp_sw_legs -> sxp_sw_legs sxp_sw_leg");
  $$ = $1;
  $$->addChild($2);
};

sxp_sw_leg: '(' sxp_switch_match sxp_expr_seq ')'  {
  SHOWPARSE("sxp_sw_leg -> ( sxp_switch_match sxp_expr_seq )");
  $$ = AST::make(at_usw_leg, $1.loc, $3, $2);
};

sxp_sw_leg: '(' '(' sxp_switch_matches ')' sxp_expr_seq ')'  {
  SHOWPARSE("sxp_sw_leg -> ( ( sxp_switch_matches ) sxp_expr_seq )");
  $$ = AST::make(at_usw_leg, $1.loc, $5);
  $$->addChildrenFrom($3);
};

sxp_switch_matches: sxp_switch_match {
  SHOWPARSE("sxp_switch_matches -> sxp_switch_match");
  $$ = AST::make(at_Null, $1->loc, $1);
};

sxp_switch_matches: sxp_switch_matches sxp_switch_match {
  SHOWPARSE("sxp_switch_matches -> sxp_switch_matches sxp_switch_match");
  $$ = $1;
  $$->addChild($2);
};

/* Constructors may be expressed as:
   i) cons
   ii) list.cons
   iii) bitc.list.cons

   If we find the double-dotted version, we are sure that we have
   found the useident.ctr version, otherwise, this is ambiguous, and
   leave the burden on the resolver to find out */
sxp_switch_match: ident {
  SHOWPARSE("sxp_switch_match -> ident");
  $$ = $1;
};

sxp_switch_match: ident '.' ident {
  SHOWPARSE("sxp_switch_match -> ident . ident"); 
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

sxp_switch_match: ident '.' ident '.' ident {
  SHOWPARSE("sxp_switch_match -> ident '.' ident '.' ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $1->loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = AST::make(at_select, $1->loc, usesel, $5);
};

sxp_ow: sxp_otherwise {
  SHOWPARSE("sxp_ow -> sxp_otherwise");
  $$ = $1;
};

sxp_ow: { //empty
  SHOWPARSE("sxp_ow -> Null");
  $$ = AST::make(at_Null);
};

sxp_otherwise: '(' tk_OTHERWISE sxp_expr_seq')' {
  SHOWPARSE("sxp_otherwise -> ( OTHERWISE sxp_expr_seq)");
  $$ = AST::make(at_otherwise, $2.loc, $3);
};

/* // TYPECASE  [11]           
sxp_eform: '(' tk_TYPECASE '(' typecase_legs ')' ')'  {
  SHOWPARSE("sxp_eform -> ( typecase ( typecase_legs ) )");
  $$ = $4;
  $$->loc = $2.loc;
};
typecase_legs: typecase_leg {
  SHOWPARSE("sxp_typecase_legs -> typecase_leg");
  $$ = AST::make(at_typecase, $1->loc);
  $$->addChild($1);
};
typecase_legs: typecase_legs typecase_leg {
  SHOWPARSE("sxp_typecase_legs -> typecase_legs typecase_leg");
  $$ = $1;
  $$->addChild($2);
};
typecase_leg: '(' sxp_bindingpattern sxp_expr ')'  {
  SHOWPARSE("sxp_typecase_leg -> ( Bindingpattern sxp_expr )");
  $$ = AST::make(at_typecase_leg, $1.loc, $2, $3);
  }; */

// TRY/CATCH [7.19.1]
sxp_eform: '(' tk_TRY sxp_expr '(' tk_CATCH  ident sxp_sw_legs sxp_ow ')' ')'  {
  SHOWPARSE("sxp_eform -> ( TRY sxp_expr ( CATCH ident sxp_sw_legs sxp_ow) )");
  $$ = AST::make(at_try, $2.loc, $3, $6, $7, $8);
  for (size_t c =0; c < $7->children.size(); c++) {
    shared_ptr<AST> sw_leg = $7->child(c);
    sw_leg->children.insert(sw_leg->children.begin(),
                            $6->getDeepCopy());
  }
  if ($8->astType == at_otherwise) {
    shared_ptr<AST> ow = $8;
    ow->children.insert(ow->children.begin(),
                        $6->getDeepCopy());
  }
};
// shap: empty switch legs permitted, but only if sxp_otherwise clause is present.
sxp_eform: '(' tk_TRY sxp_expr '(' tk_CATCH ident sxp_ow ')' ')'  {
  SHOWPARSE("sxp_eform -> ( TRY sxp_expr ( CATCH ident sxp_ow) )");
  $$ = AST::make(at_try, $2.loc, $3, $6,
                 AST::make(at_usw_legs, $7->loc), $7);

  if ($7->astType == at_otherwise) {
    shared_ptr<AST> ow = $7;
    ow->children.insert(ow->children.begin(),
                        $6->getDeepCopy());
  }
};

// THROW  [7.19.2]              
sxp_eform: '(' tk_THROW sxp_expr ')' {
  SHOWPARSE("sxp_eform -> ( THROW sxp_expr )");
  $$ = AST::make(at_throw, $2.loc, $3);
};

// let / letrec forms

sxp_eform: sxp_let_eform {
  SHOWPARSE("sxp_eform -> sxp_let_eform");
  $$ = $1;
};

// LET [5.3.1]                 
sxp_let_eform: '(' tk_LET '(' sxp_letbindings ')' sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> (LET (letbindings) sxp_expr_seq)");
  $6->astType = at_begin;
  $6->printVariant = pf_IMPLIED;
  $$ = AST::make(at_let, $2.loc, $4, $6);
  $$->addChild(AST::make(at_constraints));
};
sxp_letbindings: sxp_letbinding {
  SHOWPARSE("sxp_letbindings -> sxp_letbinding");
  $$ = AST::make(at_letbindings, $1->loc, $1);
};
sxp_letbindings: sxp_letbindings sxp_letbinding {
  SHOWPARSE("sxp_letbindings -> sxp_letbindings sxp_letbinding");
  $$ = $1;
  $$->addChild($2);
};
sxp_letbinding: '(' sxp_bindingpattern sxp_expr ')' {
  SHOWPARSE("sxp_letbinding -> ( sxp_bindingpattern sxp_expr )");
  $$ = AST::make(at_letbinding, $2->loc, $2, $3);
};

// LETREC [5.3.2]              
sxp_let_eform: '(' tk_LETREC '(' sxp_letbindings ')' sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> (LETREC (letbindings) sxp_expr_seq)");
  $6->astType = at_begin;
  $6->printVariant = pf_IMPLIED;
  shared_ptr<AST> lbs = $4;
  for (size_t c=0; c < lbs->children.size(); c++)
    lbs->child(c)->flags |= LB_REC_BIND;
 
  $$ = AST::make(at_letrec, $2.loc, $4, $6);
  $$->addChild(AST::make(at_constraints));
};

sxp_eform: '(' tk_DO '(' sxp_dobindings ')' sxp_dotest sxp_expr_seq ')' {
  SHOWPARSE("sxp_eform -> (DO (dobindings) sxp_dotest sxp_expr_seq)");

  // The body is executed for side effects. We need to know its result
  // sxp_type so that the CONTINUE block will be properly typed. Since we
  // are only running the body for side effects, force the result sxp_type
  // to be unit by appending a unit sxp_constructor at the end of the
  // expression sequence:
  $7->addChild(AST::make(at_unit, $7->loc));

  shared_ptr<AST> iContinueBlock =
    AST::make(at_block, $2.loc,
              AST::make(at_ident, LToken("__continue")),
              $7);
  $$ = AST::make(at_do, $2.loc, $4, $6, iContinueBlock);
};


sxp_dobindings: {
  SHOWPARSE("sxp_dobindings -> <empty>");
  $$ = AST::make(at_dobindings);
};
sxp_dobindings: ne_dobindings {
  SHOWPARSE("sxp_dobindings -> ne_dobindings");
  $$ = $1;
};
ne_dobindings: sxp_dobinding {
  SHOWPARSE("ne_dobindings -> sxp_dobinding");
  $$ = AST::make(at_dobindings, $1->loc, $1);
};
ne_dobindings: ne_dobindings sxp_dobinding {
  SHOWPARSE("ne_dobindings -> ne_dobindings sxp_dobinding");
  $$ = $1;
  $$->addChild($2);
};
sxp_dobinding: '(' sxp_bindingpattern sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_dobinding -> ( sxp_bindingpattern sxp_expr )");
  $$ = AST::make(at_dobinding, $2->loc, $2, $3, $4);
};
sxp_dotest: '(' sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_dobinding -> ( sxp_expr sxp_expr )");
  $$ = AST::make(at_dotest, $2->loc, $2, $3); 
};

sxp_eform: '(' tk_CONTINUE ')' {
  SHOWPARSE("sxp_eform -> (CONTINUE)");
  $$ = AST::make(at_return_from, $2.loc,
                 AST::make(at_ident, LToken("__continue")),
                 AST::make(at_unit, $2.loc));
}

/* Literals and Variables */
// INTEGER LITERALS [2.4.1]
sxp_literal: boolLit {
  SHOWPARSE("sxp_literal -> boolLit");
  $$ = $1;
};
sxp_literal: intLit {
  SHOWPARSE("sxp_literal -> intLit");
  $$ = $1;
};
// FLOATING POINT LITERALS [2.4.2]
sxp_literal: floatLit {
  SHOWPARSE("sxp_literal -> floatLit");
  $$ = $1;
};
// CHARACTER LITERALS [2.4.3]
sxp_literal: charLit {
  SHOWPARSE("sxp_literal -> CharLit");
  $$ = $1;
};
// STRING LITERALS [2.4.4]
sxp_literal: strLit {
  SHOWPARSE("sxp_literal -> strLit");
  $$ = $1;
};

// External identifiers are not subject to reserved word restrictions...
sxp_exident: tk_Ident {
  SHOWPARSE("sxp_exident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

sxp_exident: tk_ReservedWord {
  SHOWPARSE("sxp_exident -> <Reserved " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

// IDENTIFIERS [2.2]
ident: tk_Ident {
  SHOWPARSE("sxp_ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

ident: tk_ReservedWord {
  SHOWPARSE("sxp_ident -> <RESERVED=" + $1.str + ">");
  cerr << $1.loc.asString() << ": The token \"" << $1.str
       << "\" is reserved for future use.\n";
  lexer->num_errors++;
  $$ = AST::make(at_ident, $1);
};

useident: ident {
  SHOWPARSE("sxp_useident -> ident");
  $$ = $1;
};

useident: ident '.' ident {
  SHOWPARSE("sxp_useident -> ident . ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $2.loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = usesel;
};

useident: ident '.' ident '.' ident {
  SHOWPARSE("sxp_useident -> ident . ident . ident");

  shared_ptr<AST> lhs = AST::make(at_usesel, $2.loc, $1, $3);
  lhs->s = $1->s + "." + $3->s;

  shared_ptr<AST> usesel = AST::make(at_usesel, $4.loc, lhs, $5); 
  usesel->s = lhs->s + "." + $5->s;
  $$ = usesel;
};

//defident: ident {
//  SHOWPARSE("sxp_defident -> ident");
//  $1->flags |= (ID_IS_GLOBAL);
//  $$ = $1;
//};

defident: useident {
  SHOWPARSE("sxp_defident -> useident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = $1;
};

// TYPE VARIABLES [3.3]
sxp_typevar: tk_TypeVar {
  SHOWPARSE("sxp_typevar -> <TypeVar=" + $1.str + ">");
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

charLit: tk_Char {
  SHOWPARSE("charLit -> <Char=" + $1.str +">");
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

