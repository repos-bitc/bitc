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


inline int
transition_lex(YYSTYPE *lvalp, TransitionLexer *lexer)
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

// We currently have 3 shift/reduce ambiguities of the form
//   x -> a
//   x -> a ( {args} )
//
// that are resolved by explicit prioritization.
// 
// The first is type = identifier vs type = type application
// The second is parameterized type names (with or without type vars)
// The third is constraint = identifier vs constraint = type app
//
// We have one that is the classic dangling if/then/else problem. This
// one probably *should* be resolved by explicit prioritization.
//
// We have ten associated with opt_docstring in the s-expression
// grammar that will be going away when the s-expression syntax is
// dropped.
//
// We will undoubtedly see another when we introduce procedure
// application syntax.
//
// There are a large number of others, mainly due to optional
// semicolons.
//
// In C, we would resolve this by introducing associativity for '(',
// but I'm not convinced that we can do that safely until we are fully
// done with the s-expression syntax.
%pure-parser
%parse-param {TransitionLexer *lexer}

%token <tok> tk_ReservedWord        /* reserved words */
%token <tok> tk_SxpReservedWord     /* S-expression reserved words */

/* Categorical terminals: */
%token <tok> tk_SxpIdent
%nonassoc <tok> tk_BlkIdent tk_MixIdent
%nonassoc <tok> tk_NotApply

%token <tok> tk_TypeVar
%token <tok> tk_EffectVar
 /* Nat and NegativeInt are distinguished to ensure that only naturals
    are legal in types. The lexer returns tk_Nat for positive integer
    literals and tk_NegativeInt for negative integer literals. In the
    expression grammar these are re-merged immediately in the IntLit
    production. 

    Actually, this distinction reflects a bug in the grammar in the
    handling of unary negation. The lexer should really be returning
    tk_Nat at all times, and negation should be getting handled at
    the grammar level. */
%token <tok> tk_NegativeInt     /* S-expression only */
%token <tok> tk_Nat         /* in block: *postive* integer */
%token <tok> tk_Float       /* in block: *postive* float */
%token <tok> tk_Char
%token <tok> tk_String
%token <tok> tk_VersionNumber

%left <tok> '('
%left <tok> ':'
%token <tok> ')'                /* procedure call, unit */
%token <tok> ','                /* pair */
%token <tok> '[' ']'            /* array, vector */
%token <tok> '.'
%token <tok> '`'
%token <tok> tk_ASSIGN

/* Primary types and associated hand-recognized literals: */
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

%token <tok> tk_BITC
%token <tok> tk_VERSION

%token <tok> tk_PURE
%token <tok> tk_IMPURE
%token <tok> tk_CONST

%token <tok> tk_THE
%token <tok> tk_IF tk_THEN tk_ELSE
%token <tok> tk_WHEN
%token <tok> tk_NOT '!'
%token <tok> tk_COND
%token <tok> tk_SWITCH
%token <tok> tk_TYPECASE
%token <tok> tk_CASE
%token <tok> tk_OTHERWISE

/* Precedence relationships for infix operators. In Bison, lower
   precedence operators come first: */
%left <tok> tk_OR
%left <tok> tk_AND
%left <tok> tk_EQUALS tk_NOTEQUALS
%left <tok> '<' tk_LE '>' tk_GE
%left <tok> '|'
%left <tok> '^'
%left <tok> '&'
%left <tok> tk_LSHIFT tk_RSHIFT // eventually
%left <tok> '+' '-'
%left <tok> '*' '/' '%'

%left <tok> '='

%token <tok> tk_BLOCK
%token <tok> tk_RETURN tk_FROM tk_CONTINUE

%token <tok> tk_PAIR
%token <tok> tk_VECTOR
%token <tok> tk_ARRAY
%token <tok> tk_MAKE_VECTOR
%token <tok> tk_NTH

// Block syntax:
%token <tok> tk_STRUCT
%token <tok> tk_OBJECT
%token <tok> tk_UNION
%token <tok> tk_REPR
%token <tok> tk_EXCEPTION
%token <tok> tk_TRAIT
%token <tok> tk_INSTANCE
%token <tok> tk_DEF

// S-expr syntax:
%token <tok> tk_DEFSTRUCT
%token <tok> tk_DEFOBJECT
%token <tok> tk_DEFUNION
%token <tok> tk_DEFREPR
%token <tok> tk_DEFEXCEPTION
%token <tok> tk_DEFTYPECLASS
%token <tok> tk_DEFINSTANCE
%token <tok> tk_DEFTHM
%token <tok> tk_DEFINE
%token <tok> tk_DECLARE
%token <tok> tk_PROCLAIM
%token <tok> tk_EXTERNAL
%token <tok> tk_TAG

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
%token <tok> tk_IN
%token <tok> tk_FN
%token <tok> tk_FNARROW
%token <tok> tk_BEGIN
%token <tok> tk_DO
%token <tok> tk_APPLY
%token <tok> tk_BY_REF
%token <tok> tk_ARRAY_REF

%token <tok> tk_TRY
%token <tok> tk_CATCH
%token <tok> tk_HANDLE
%token <tok> tk_THROW

%token <tok> tk_METHOD
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
%type <tok>  sxp_ifident blk_ifident

//%token <tok> tk_EXPORT
 
%type <tok> LP RP

%type <ast> sxp_module sxp_implicit_module sxp_module_seq
%type <ast> trn_module trn_implicit_module trn_module_seq
%type <ast> sxp_interface
%type <ast> trn_interface
%type <ast> sxp_defpattern blk_defpattern
%type <ast> sxp_mod_definitions
%type <ast> sxp_mod_definition blk_mod_definition
%type <ast> trn_mod_definitions trn_mod_definition
%type <ast> sxp_if_definitions sxp_if_definition
%type <ast> trn_if_definitions trn_if_definition
%type <ast> sxp_common_definition
%type <ast> blk_common_definition
%type <ast> sxp_value_declaration
%type <ast> blk_value_declaration
%type <ast> sxp_ptype_name cmn_val sxp_openclosed
%type <ast> blk_ptype_name
%type <ast> sxp_type_definition blk_type_definition
%type <ast> sxp_typeapp blk_typeapp
%type <ast> sxp_type_decl blk_type_decl
%type <ast> sxp_externals blk_externals
%type <ast> sxp_alias blk_alias
%type <ast> sxp_importList sxp_provideList
%type <ast> blk_importList blk_provideList
%type <ast> sxp_type_cpair sxp_unqual_expr_cpair
%type <ast> blk_type_cpair blk_expr_cpair
%type <ast> sxp_value_definition blk_value_definition 
%type <ast> sxp_tc_definition sxp_ti_definition
%type <ast> blk_tc_definition blk_ti_definition
%type <ast> sxp_import_definition sxp_provide_definition
%type <ast> blk_import_definition blk_provide_definition
%type <ast> sxp_tc_decls sxp_tc_decl
%type <ast> blk_tc_decls blk_tc_decl
%type <ast> sxp_declares sxp_declare sxp_decls sxp_decl
%type <ast> blk_declares blk_declare blk_decls blk_decl
%type <ast> sxp_constructors sxp_constructor
%type <ast> blk_constructors blk_constructor
%type <ast> sxp_repr_constructors sxp_repr_constructor
%type <ast> sxp_repr_reprs sxp_repr_repr
%type <ast> sxp_bindingpattern sxp_lambdapatterns sxp_lambdapattern
%type <ast> blk_bindingpattern blk_lambdapatterns blk_lambdapattern
%type <ast> sxp_actual_params sxp_nonempty_params
%type <ast> blk_actual_params blk_nonempty_params
%type <ast> sxp_block sxp_block_exprs
%type <ast> blk_block blk_stmt_seq
%type <ast> sxp_expr sxp_the_expr sxp_unqual_expr
%type <ast> blk_primary_expr blk_postfix_expr blk_prefix_expr blk_infix_expr
%type <ast> blk_tqual_expr
%type <ast> blk_stmt
%type <ast> blk_expr
%type <ast> sxp_method_decls sxp_method_decl
%type <ast> blk_method_decls blk_method_decl
%type <ast> sxp_method_bindings sxp_method_binding
%type <ast> blk_method_bindings blk_method_binding
%type <ast> sxp_constraints sxp_constraint_seq sxp_constraint
%type <ast> blk_constraints blk_constraint_seq blk_constraint
%type <ast> sxp_type_args sxp_type sxp_bitfieldtype
%type <ast> blk_type blk_postfix_type blk_prefix_type
%type <ast> blk_type_args blk_bitfieldtype
%type <ast> sxp_field_type sxp_type_pl_byref sxp_type_args_pl_byref
%type <ast> blk_field_type blk_type_pl_byref blk_type_args_pl_byref
%type <ast> int_type uint_type any_int_type float_type primary_type bool_type
%type <ast> sxp_tvlist blk_tvlist
%type <ast> sxp_fields sxp_field
%type <ast> blk_fields blk_field
%type <ast> sxp_fields_and_methods sxp_methods_only sxp_methdecl
%type <ast> blk_fields_and_methods blk_methods_only blk_methdecl
%type <ast> trn_literal typevar //mod_ident
%type <ast> sxp_switch_matches sxp_switch_match
 // %type <ast> blk_switch_matches blk_switch_match
%type <ast> blk_switch_match
%type <ast> sxp_exident blk_exident
%type <ast> trn_docstring trn_optdocstring
%type <ast> sxp_condcases sxp_condcase
%type <ast> sxp_fntype sxp_method_type
%type <ast> blk_fntype blk_method_type
%type <ast> trn_fneffect
 //%type <ast> reprbody reprbodyitems reprbodyitem reprtags reprcase reprcaseleg
//%type <ast> typecase_leg typecase_legs
%type <ast> sxp_sw_legs sxp_sw_leg sxp_otherwise sxp_opt_otherwise
%type <ast> blk_catch_legs blk_catch_leg
%type <ast> blk_sw_legs blk_sw_leg
%type <ast> blk_otherwise blk_opt_otherwise
//%type <ast> catchclauses catchclause
%type <ast> sxp_letbindings sxp_letbinding
%type <ast> blk_letbindings blk_letbinding
%type <ast> sxp_dobindings ne_dobindings sxp_dobinding sxp_dotest
%type <ast> sxp_let_eform
%type <ast> sxp_type_val_definition blk_type_val_definition
%type <ast> sxp_constrained_definition

%type <ast> sxp_ident sxp_defident sxp_useident
%type <ast> blk_ident blk_defident blk_useident
%type <ast> intLit natLit floatLit charLit strLit boolLit

%%

// Parser built for version 10.0
// Section Numbers indicated within []

// COMPILATION UNITS [2.5]
// This definition of sxp_start mus be changed as it ignores
// junk after the body.

sxp_start: sxp_version sxp_uoc_body {
  SHOWPARSE("sxp_start -> sxp_version sxp_uoc_body");
  return 0;
};

sxp_start: sxp_uoc_body {
  SHOWPARSE("sxp_start -> sxp_uoc_body");
  return 0;
};

sxp_start: blk_version trn_uoc_body {
  SHOWPARSE("sxp_start -> blk_version trn_uoc_body");
  return 0;
};

sxp_uoc_body: sxp_interface {
  SHOWPARSE("sxp_uoc_body -> sxp_interface");
}

sxp_uoc_body: sxp_implicit_module {
  SHOWPARSE("sxp_uoc_body -> sxp_implicit_module");
}

sxp_uoc_body: sxp_module_seq {
  SHOWPARSE("sxp_uoc_body -> sxp_module_seq");
}

trn_uoc_body: trn_interface {
  SHOWPARSE("trn_uoc_body -> trn_interface");
}

trn_uoc_body: trn_implicit_module {
  SHOWPARSE("trn_uoc_body -> trn_implicit_module");
}

trn_uoc_body: trn_module_seq {
  SHOWPARSE("trn_uoc_body -> trn_module_seq");
}

// VERSION [2.5]

sxp_version: LP tk_BITC {
    lexer->currentLang |= TransitionLexer::lf_version; 
} tk_VERSION { 
    lexer->currentLang |= TransitionLexer::lf_version; 
} tk_VersionNumber RP {
  SHOWPARSE("sxp_version -> LP BITC-VERSION VersionNumber RP");
  shared_ptr<AST> version = AST::makeStringLit($6);

  // Beginning in version 0.12, this entire form will be gone.

  if ((VersionMajor(version->s) == 0) && (VersionMinor(version->s) < 10)) {
    std::string s = ": Error: input language version " + version->s + " is no longer accepted.";
    lexer->ReportParseError(version->loc, s);
  }
};

blk_version: tk_BITC {
    lexer->currentLang &= ~TransitionLexer::lf_LispComments; 
    lexer->currentLang |= TransitionLexer::lf_version; 
} tk_VERSION {
    lexer->currentLang |= TransitionLexer::lf_version; 
} tk_VersionNumber {
  SHOWPARSE("blk_version -> BITC VERSION VersionNumber");
  shared_ptr<AST> version = AST::makeStringLit($5);

  if ((VersionMajor(version->s) == 0) && (VersionMinor(version->s) < 11)) {
    std::string s = ": Error: block syntax is supported in versions 0.11 and above.";
    lexer->ReportParseError(version->loc, s);
  }

  lexer->currentLang |= TransitionLexer::lf_block; 
  lexer->currentLang &= ~TransitionLexer::lf_sexpr; 
  lexer->currentLang &= ~TransitionLexer::lf_LispComments;
};

// Documentation comments. These are added only in productions where
// they do NOT appear before expr_seq. If a string trn_literal appears as
// the first form of a multiform expr_seq, it won't hurt anything. If
// it is the *only* form, then it is the value in any case, and that
// is fine. We can figure out which case is which in the documentation
// extractor.
trn_docstring: tk_String {
  SHOWPARSE("trn_docstring -> STRING");
  $$ = AST::make(at_docString, $1.loc, AST::makeStringLit($1));
};
trn_optdocstring: trn_docstring {
  SHOWPARSE("trn_optdocstring -> trn_docstring");
  $$ = $1;
};
trn_optdocstring: {
  SHOWPARSE("trn_optdocstring -> ");
  $$ = AST::make(at_docString);
};

// INTERFACES [8.1]
trn_interface: tk_INTERFACE blk_ifident {
    if ($2.str.find("bitc.") == 0)
      lexer->isRuntimeUoc = true;
  }
  trn_optdocstring '{' trn_if_definitions '}' {
  SHOWPARSE("trn_interface -> INTERFACE blk_ifident trn_optdocstring { sxp_if_definitions }");
  shared_ptr<AST> ifIdent = AST::make(at_ident, $2);
  $$ = AST::make(at_interface, $1.loc, ifIdent);
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
trn_interface: LP tk_INTERFACE sxp_ifident {
    if ($3.str.find("bitc.") == 0)
      lexer->isRuntimeUoc = true;
  }
  trn_optdocstring trn_if_definitions RP {
  SHOWPARSE("trn_interface -> ( INTERFACE sxp_ifident trn_optdocstring trn_if_definitions )");
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

sxp_interface: LP tk_INTERFACE sxp_ifident {
    if ($3.str.find("bitc.") == 0)
      lexer->isRuntimeUoc = true;
  }
  trn_optdocstring sxp_if_definitions RP {
  SHOWPARSE("sxp_interface -> ( INTERFACE sxp_ifident trn_optdocstring sxp_if_definitions )");
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

// When IfIdentMode is set we are matching fully legal interface
// identifiers in both syntax families.
blk_ifident: {
    lexer->setIfIdentMode(true);
  } blk_ident {
  lexer->setIfIdentMode(false);
  $$ = LToken($2->loc, $2->s);
};
blk_ifident: blk_ifident '.' {
    lexer->setIfIdentMode(true);
  } tk_BlkIdent {
  lexer->setIfIdentMode(false);
  $$ = LToken($1.loc, $1.str + "." + $4.str);
};

sxp_ifident: {
    lexer->setIfIdentMode(true);
  } sxp_ident {
  lexer->setIfIdentMode(false);
  $$ = LToken($2->loc, $2->s);
};
sxp_ifident: sxp_ifident '.' {
    lexer->setIfIdentMode(true);
  } tk_SxpIdent {
  lexer->setIfIdentMode(false);
  $$ = LToken($1.loc, $1.str + "." + $4.str);
};

// MODULES [2.5]
trn_module_seq: trn_module {
 SHOWPARSE("trn_module_seq -> trn_module");
}

trn_module_seq: trn_module_seq trn_module {
 SHOWPARSE("trn_module_seq -> trn_module_seq trn_module");
}

trn_implicit_module: trn_mod_definitions  {
 SHOWPARSE("trn_implicit_module -> trn_mod_definitions");
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

trn_module: tk_MODULE trn_optdocstring '{' trn_mod_definitions '}' {
 SHOWPARSE("trn_module -> tk_MODULE trn_optdocstring { trn_mod_definitions }");
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
trn_module: LP tk_MODULE trn_optdocstring trn_mod_definitions RP {
 SHOWPARSE("trn_module -> ( tk_MODULE trn_optdocstring trn_mod_definitions )");
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

trn_module: tk_MODULE blk_ifident trn_optdocstring '{' trn_mod_definitions '}' {
 SHOWPARSE("trn_module -> tk_MODULE sxp_ifident trn_optdocstring { trn_mod_definitions }");
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
trn_module: LP tk_MODULE sxp_ifident trn_optdocstring trn_mod_definitions RP {
 SHOWPARSE("trn_module -> ( tk_MODULE sxp_ifident trn_optdocstring trn_mod_definitions )");
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

sxp_module: LP tk_MODULE trn_optdocstring sxp_mod_definitions RP {
 SHOWPARSE("sxp_module -> ( tk_MODULE trn_optdocstring sxp_mod_definitions )");
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

sxp_module: LP tk_MODULE sxp_ifident trn_optdocstring sxp_mod_definitions RP {
 SHOWPARSE("sxp_module -> ( tk_MODULE sxp_ifident trn_optdocstring sxp_mod_definitions )");
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
trn_if_definitions: trn_if_definition {
  SHOWPARSE("trn_if_definitions -> trn_if_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

trn_if_definitions: trn_if_definitions trn_if_definition {
  SHOWPARSE("trn_if_definitions -> trn_if_definitions trn_if_definition");
  $$ = $1;
  $$->addChild($2); 
};

trn_if_definition: sxp_common_definition {
  SHOWPARSE("trn_if_definition -> sxp_common_definition");
  $$ = $1;
};

trn_if_definition: blk_common_definition {
  SHOWPARSE("trn_if_definition -> blk_common_definition");
  $$ = $1;
};

trn_if_definition: blk_common_definition SC {
  SHOWPARSE("trn_if_definition -> blk_common_definition SC");
  $$ = $1;
};

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
// Note that the block syntax has no direct analog to this, since we
// moved the constraints back inside the defining form.
sxp_constrained_definition: LP tk_FORALL sxp_constraints sxp_type_val_definition RP {
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

trn_mod_definitions: trn_mod_definition {
  SHOWPARSE("trn_mod_definitions -> trn_mod_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

trn_mod_definitions: trn_mod_definitions trn_mod_definition {
  SHOWPARSE("trn_mod_definitions -> trn_mod_definitions trn_mod_definition");
  $$ = $1;
  $$->addChild($2); 
};

trn_mod_definition: sxp_mod_definition {
  SHOWPARSE("trn_mod_definition -> sxp_mod_defintion");
  $$ = $1;
};

trn_mod_definition: blk_mod_definition {
  SHOWPARSE("trn_mod_definition -> blk_mod_defintion");
  $$ = $1;
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

blk_mod_definition: blk_provide_definition {
  SHOWPARSE("blk_mod_definition -> blk_provide_definition");
  $$ = $1;
};

sxp_mod_definition: sxp_provide_definition {
  SHOWPARSE("sxp_mod_definition -> sxp_provide_definition");
  $$ = $1;
};

blk_mod_definition: blk_common_definition {
  SHOWPARSE("blk_mod_definition -> blk_common_definition");
  $$ = $1;
};

sxp_mod_definition: sxp_common_definition {
  SHOWPARSE("sxp_mod_definition -> sxp_common_definition");
  $$ = $1;
};

blk_common_definition: blk_import_definition {
  SHOWPARSE("blk_common_definition -> blk_import_definition");
  $$ = $1;
}

sxp_common_definition: sxp_import_definition {
  SHOWPARSE("sxp_common_definition -> sxp_import_definition");
  $$ = $1;
};

blk_common_definition: blk_type_val_definition {
  SHOWPARSE("blk_common_definition -> blk_type_val_definition");
  $$ = $1;
};

sxp_common_definition: sxp_type_val_definition {
  SHOWPARSE("sxp_common_definition -> sxp_type_val_definition");
  $$ = $1;
};

// No corresponding production in the block syntax, since constraints
// moved back inside their respective forms.
//
//blk_common_definition: blk_constrained_definition {
//  SHOWPARSE("blk_common_definition -> blk_constrained_definition");
//  $$ = $1;
//}

sxp_common_definition: sxp_constrained_definition {
  SHOWPARSE("sxp_common_definition -> sxp_constrained_definition");
  $$ = $1;
};

blk_type_val_definition: blk_type_decl {
  SHOWPARSE("blk_type_val_definition -> blk_type_decl");
  $$ = $1;
};

sxp_type_val_definition: sxp_type_decl {
  SHOWPARSE("sxp_type_val_definition -> sxp_type_decl");
  $$ = $1;
};

blk_type_val_definition: blk_type_definition {
  SHOWPARSE("blk_type_val_definition -> blk_type_definition");
  $$ = $1;
};

sxp_type_val_definition: sxp_type_definition {
  SHOWPARSE("sxp_type_val_definition -> sxp_type_definition");
  $$ = $1;
};

blk_type_val_definition: blk_value_definition {
  SHOWPARSE("blk_type_val_definition -> blk_value_definition");
  $$ = $1;
};

sxp_type_val_definition: sxp_value_definition {
  SHOWPARSE("sxp_type_val_definition -> sxp_value_definition");
  $$ = $1;
};

blk_type_val_definition: blk_value_declaration {
  SHOWPARSE("blk_type_val_definition -> blk_value_declaration");
  $$ = $1;
};

sxp_type_val_definition: sxp_value_declaration {
  SHOWPARSE("sxp_type_val_definition -> sxp_value_declaration");
  $$ = $1;
};

blk_type_val_definition: blk_tc_definition {
  SHOWPARSE("blk_type_val_definition -> blk_tc_definition");
  $$ = $1;
};

sxp_type_val_definition: sxp_tc_definition {
  SHOWPARSE("sxp_type_val_definition -> sxp_tc_definition");
  $$ = $1;
};

blk_type_val_definition: blk_ti_definition {
  SHOWPARSE("blk_type_val_definition -> blk_ti_definition");
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

blk_constraints: {
  $$ = AST::make(at_constraints);
}

blk_constraints: tk_WHERE blk_constraint_seq {
  SHOWPARSE("blk_constraints -> blk_constraint_seq");
  $$ = $2;
}

blk_constraint_seq: blk_constraint_seq ',' blk_constraint {
 SHOWPARSE("blk_constraint_seq -> blk_constraint_seq , blk_constraint");
 $$ = $1;
 $$->addChild($3);
};

blk_constraint_seq: blk_constraint {
 SHOWPARSE("blk_constraint_seq -> blk_constraint");
 $$ = AST::make(at_constraints, $1->loc, $1);
};

blk_constraint: blk_typeapp {
 SHOWPARSE("blk_constraint -> blk_typeapp");
 $1->astType = at_tcapp;
 $$ = $1;
};

blk_constraint: blk_useident %prec tk_BlkIdent {
 SHOWPARSE("blk_constraint -> blk_useident");
 $$ = AST::make(at_tcapp, $1->loc, $1);
};

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

sxp_constraint: sxp_useident {
 SHOWPARSE("sxp_constraint -> sxp_useident");
 $$ = AST::make(at_tcapp, $1->loc, $1);
};

// Issue: parameterized type names in the block syntax have a
// shift/reduce ambiguity at:
//
//  blk_defident .
//  blk_defident . '(' blk_tvlist ')' blk_constraints

blk_ptype_name: blk_defident %prec tk_BlkIdent {
  SHOWPARSE("blk_ptype_name -> blk_defident");
  shared_ptr<AST> tvlist = AST::make(at_tvlist, $1->loc);
  $$ = AST::make(at_Null, $1->loc, $1, tvlist);
};

blk_ptype_name: blk_defident '(' blk_tvlist ')' %prec '(' {
  SHOWPARSE("blk_ptype_name -> blk_ptype_name_primary ( blk_tvlist )");
  $$ = AST::make(at_Null, $1->loc, $1, $3);
};

//blk_ptype_name: blk_defident '(' blk_tvlist ')' {
//  SHOWPARSE("blk_ptype_name -> blk_defident ( blk_tvlist )");
//  shared_ptr<AST> constraints = AST::make(at_constraints, $1->loc);
//  $$ = AST::make(at_Null, $1->loc, $1, $3, constraints);
//};

sxp_ptype_name: sxp_defident {
  SHOWPARSE("sxp_ptype_name -> sxp_defident");
  shared_ptr<AST> tvlist = AST::make(at_tvlist, $1->loc);
  shared_ptr<AST> constraints = AST::make(at_constraints, $1->loc);
  $$ = AST::make(at_Null, $1->loc, $1, tvlist, constraints);
};

sxp_ptype_name: '(' sxp_defident sxp_tvlist ')' {
  SHOWPARSE("sxp_ptype_name -> '(' sxp_defident sxp_tvlist ')'");
  shared_ptr<AST> constraints = AST::make(at_constraints, $2->loc);
  $$ = AST::make(at_Null, $2->loc, $2, $3, constraints);
};

// These were the originals, where the FORALL did not yet wrap the
// outermost form. This style later abandoned in the S-Expression
// syntax, though in hindsight it was structurally better as-was and
// perhaps should not have been abandoned.

//ptype_name: '(' tk_FORALL sxp_constraints sxp_defident ')' {
//  SHOWPARSE("sxp_ptype_name -> '(' FORALL sxp_constraints '(' sxp_defident sxp_tvlist ')' ')' ");
//  shared_ptr<AST> tvlist = AST::make(at_tvlist, $4->loc);
//  $$ = AST::make(at_Null, $2.loc, $4, tvlist, $3);
//};
//
//ptype_name: '(' tk_FORALL sxp_constraints '(' sxp_defident sxp_tvlist ')' ')' {
//  SHOWPARSE("sxp_ptype_name -> '(' FORALL sxp_constraints '(' sxp_defident sxp_tvlist ')' ')' ");
//  $$ = AST::make(at_Null, $2.loc, $5, $6, $3);
//};

// STRUCTURE TYPES [3.6.1]         
blk_type_definition: tk_STRUCT blk_ptype_name blk_constraints cmn_val trn_optdocstring blk_declares '{' blk_fields_and_methods '}' {
  SHOWPARSE("blk_type_definition -> STRUCT blk_ptype_name blk_constraints cmn_val "
            "trn_optdocstring blk_declares { blk_fields }");
  $$ = AST::make(at_defstruct, $1.loc, $2->child(0), $2->child(1), $4,
                 $6, $8, $3);
  $$->child(0)->defForm = $$;
  };
/*blk_type_definition: tk_STRUCT blk_ptype_name cmn_val trn_optdocstring blk_declares '{' blk_fields_and_methods '}' {
  SHOWPARSE("blk_type_definition -> STRUCT blk_ptype_name blk_constraints cmn_val "
            "trn_optdocstring blk_declares { blk_fields }");
  $$ = AST::make(at_defstruct, $1.loc, $2->child(0), $2->child(1), $3,
                 $5, $7);
  $$->child(0)->defForm = $$;
  $$->addChild($2->child(2));
  };*/

sxp_type_definition: LP tk_DEFSTRUCT sxp_ptype_name cmn_val trn_optdocstring sxp_declares sxp_fields_and_methods RP {
  SHOWPARSE("sxp_type_definition -> ( DEFSTRUCT sxp_ptype_name cmn_val "
            "trn_optdocstring sxp_declares sxp_fields_and_methods )");
  $$ = AST::make(at_defstruct, $2.loc, $3->child(0), $3->child(1), $4,
               $6, $7);
  $$->child(0)->defForm = $$;
  $$->addChild($3->child(2));
};


// UNION TYPES [3.6.2]              
blk_type_definition: tk_UNION blk_ptype_name blk_constraints cmn_val trn_optdocstring blk_declares '{' blk_constructors '}' {
  SHOWPARSE("blk_type_definition -> UNION blk_ptype_name blk_constraints cmn_val "
            "trn_optdocstring blk_declares { blk_constructors }");
  $$ = AST::make(at_defunion, $1.loc, $2->child(0), $2->child(1), $4,
                 $6, $8, $3);
  $$->child(0)->defForm = $$;
}

//HERE
sxp_type_definition: LP tk_DEFUNION sxp_ptype_name cmn_val trn_optdocstring sxp_declares sxp_constructors  RP  {
  SHOWPARSE("sxp_type_definition -> ( DEFUNION sxp_ptype_name cmn_val "
            "trn_optdocstring sxp_declares sxp_constructors");
  $$ = AST::make(at_defunion, $2.loc, $3->child(0), $3->child(1), $4,
               $6, $7);
  $$->child(0)->defForm = $$;
  $$->addChild($3->child(2));
};

/* // REPR TYPES */
/* type_definition: '(' tk_DEFREPR sxp_defident cmn_val trn_optdocstring sxp_declares reprbody  ')'  { */
/*   SHOWPARSE("sxp_type_definition -> ( DEFUNION sxp_ptype_name cmn_val " */
/*             "trn_optdocstring sxp_declares reprbody"); */
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

/* reprtags: sxp_ident { */
/*   SHOWPARSE("reprtags -> sxp_ident"); */
/*   $$ = AST::make(at_reprtag, $1->loc, $1); /\* dummy AST sxp_type *\/ */
/* }; */

/* reprtags: reprtags sxp_ident { */
/*   SHOWPARSE("reprtags -> reprtags sxp_ident"); */
/*   $$ = $1; */
/*   $$->addChild($2); */
/* }; */

// REPR TYPES
//HERE
sxp_type_definition: LP tk_DEFREPR sxp_defident cmn_val trn_optdocstring sxp_declares sxp_repr_constructors  RP {
  SHOWPARSE("sxp_type_definition -> ( DEFREPR sxp_defident cmn_val "
            "trn_optdocstring sxp_declares sxp_repr_constructors");
  $$ = AST::make(at_defrepr, $2.loc, $3, $4, $6, $7);
  $$->child(0)->defForm = $$;
};

// Type Declarations
// External declarations
blk_externals: /* nothing */ {
  SHOWPARSE("sxp_externals -> ");
  $$ = AST::make(at_Null);
  $$->flags = NO_FLAGS;
};

blk_externals: tk_EXTERNAL {
  SHOWPARSE("sxp_externals -> EXTERNAL");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;
};

blk_externals: tk_EXTERNAL blk_exident {
  SHOWPARSE("sxp_externals -> EXTERNAL blk_exident");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;
  $$->externalName = $2->s;
};

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
blk_type_definition: tk_OBJECT blk_ptype_name blk_constraints trn_optdocstring blk_declares '{' blk_methods_only '}'  {
  SHOWPARSE("blk_type_definition -> OBJECT blk_ptype_name blk_constraints "
            "trn_optdocstring blk_declares blk_methods_only )");

  // For the moment, all objects are value types:
  shared_ptr<AST> valCat = AST::make(at_valCat, LToken($1.loc, "val"));

  $$ = AST::make(at_defobject, $1.loc, $2->child(0), $2->child(1),
                 valCat,
                 $5, $7, $3);
  $$->child(0)->defForm = $$;
};

sxp_type_definition: LP tk_DEFOBJECT sxp_ptype_name trn_optdocstring sxp_declares sxp_methods_only RP  {
  SHOWPARSE("sxp_type_definition -> ( DEFOBJECT sxp_ptype_name "
            "trn_optdocstring sxp_declares sxp_methods_only )");

  // For the moment, all objects are value types:
  shared_ptr<AST> valCat = AST::make(at_valCat, LToken($2.loc, "val"));

  $$ = AST::make(at_defobject, $2.loc, $3->child(0), $3->child(1),
                 valCat,
                 $5, $6);
  $$->child(0)->defForm = $$;
  $$->addChild($3->child(2));
};


// STRUCTURE DECLARATIONS
blk_type_decl: tk_STRUCT blk_ptype_name blk_constraints cmn_val blk_externals {
  SHOWPARSE("blk_type_decl -> STRUCT blk_ptype_name blk_constraints cmn_val blk_externals '");
  $$ = AST::make(at_declstruct, $1.loc, $2->child(0), $2->child(1), $4, $3);
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

sxp_type_decl: LP tk_DEFSTRUCT sxp_ptype_name cmn_val sxp_externals RP {
  SHOWPARSE("sxp_type_decl -> ( DEFSTRUCT sxp_ptype_name cmn_val sxp_externals )");
  $$ = AST::make(at_declstruct, $2.loc, $3->child(0), $3->child(1), $4,
               $3->child(2));
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// UNION DECLARATIONS
blk_type_decl: tk_UNION blk_ptype_name blk_constraints cmn_val blk_externals {
  SHOWPARSE("blk_type_decl -> UNION blk_ptype_name cmn_val blk_externals");
  $$ = AST::make(at_declunion, $1.loc, $2->child(0), $2->child(1), $4, $3);
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

sxp_type_decl: LP tk_DEFUNION sxp_ptype_name cmn_val sxp_externals RP {
  SHOWPARSE("sxp_type_decl -> ( DEFUNION sxp_ptype_name cmn_val sxp_externals )");
  $$ = AST::make(at_declunion, $2.loc, $3->child(0), $3->child(1), $4,
               $3->child(2));
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// REPR DECLARATIONS
blk_type_decl: tk_REPR blk_defident cmn_val blk_externals {
  SHOWPARSE("blk_type_decl -> REPR blk_defident cmn_val blk_externals )");
  $$ = AST::make(at_declrepr, $1.loc, $2, $3);
  $$->child(0)->defForm = $$;
  $$->flags |= $4->flags;
  $$->getID()->flags |= $4->flags;
  $$->getID()->externalName = $4->externalName;
};

sxp_type_decl: LP tk_DEFREPR sxp_defident cmn_val sxp_externals RP {
  SHOWPARSE("sxp_type_decl -> ( DEFREPR sxp_defident cmn_val sxp_externals )");
  $$ = AST::make(at_declrepr, $2.loc, $3, $4);
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// CATEGORIES

cmn_val: {
  SHOWPARSE("cmn_val -> <empty>");
  $$ = AST::make(at_refCat);
  $$->printVariant = pf_IMPLIED;
};

cmn_val: ':' tk_VAL {
  SHOWPARSE("cmn_val -> ':' VAL");
  $$ = AST::make(at_valCat, $2);
};
cmn_val: ':' tk_OPAQUE {
  SHOWPARSE("cmn_val -> ':' OPAQUE");
  $$ = AST::make(at_opaqueCat, $2);
};
cmn_val: ':' tk_REF {
  /* Same as :ref, since that is the default. */
  SHOWPARSE("cmn_val -> ':' REF");
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
blk_type_definition: tk_EXCEPTION blk_ident trn_optdocstring {
  SHOWPARSE("blk_type_definition -> EXCEPTION blk_ident");
  $2->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $1.loc, $2);
  $$->child(0)->defForm = $$;
};

sxp_type_definition: LP tk_DEFEXCEPTION sxp_ident trn_optdocstring RP {
  SHOWPARSE("sxp_type_definition -> ( defexception sxp_ident )");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $2.loc, $3);
  $$->child(0)->defForm = $$;
};

blk_type_definition: tk_EXCEPTION blk_ident trn_optdocstring '{' blk_fields '}' {
  SHOWPARSE("blk_type_definition -> exception blk_ident { blk_fields }");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $1.loc, $2);
  $$->child(0)->defForm = $$;
  $$->addChildrenFrom($5);
};

sxp_type_definition: LP tk_DEFEXCEPTION sxp_ident trn_optdocstring sxp_fields RP {
  SHOWPARSE("sxp_type_definition -> ( defexception sxp_ident sxp_fields )");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $2.loc, $3);
  $$->child(0)->defForm = $$;
  $$->addChildrenFrom($5);
};

// TYPE CLASSES [4]
// TYPE CLASS DEFINITION [4.1]

blk_tc_definition: tk_TRAIT blk_ptype_name blk_constraints trn_optdocstring blk_tc_decls sxp_openclosed '{' blk_method_decls '}' {
  SHOWPARSE("blk_tc_definition -> TRAIT blk_ptype_name blk_constraints trn_optdocstring blk_tc_decls sxp_openclosed blk_method_decls)");
  $$ = AST::make(at_deftypeclass, $1.loc, $2->child(0),
                 $2->child(1), $5, $6, $8, $3);
  $$->child(0)->defForm = $$;
};

sxp_tc_definition: LP tk_DEFTYPECLASS sxp_ptype_name trn_optdocstring sxp_tc_decls sxp_openclosed sxp_method_decls RP {
  SHOWPARSE("sxp_tc_definition -> ( DEFTYPECLASS sxp_ptype_name trn_optdocstring sxp_tc_decls sxp_openclosed sxp_method_decls)");
  $$ = AST::make(at_deftypeclass, $2.loc, $3->child(0),
                 $3->child(1), $5, $6, $7);
  $$->addChild($3->child(2));
  $$->child(0)->defForm = $$;
};

//tc_definition: '(' tk_DEFTYPECLASS sxp_ptype_name trn_optdocstring sxp_tc_decls sxp_method_decls ')' {
//  SHOWPARSE("sxp_tc_definition -> ( DEFTYPECLASS sxp_ptype_name trn_optdocstring sxp_tc_decls sxp_openclosed sxp_method_decls)");
//  $$ = AST::make(at_deftypeclass, $2.loc, $3->child(0),
//                 $3->child(1), $5, $6, $3->child(2));
//  $$->child(0)->defForm = $$;
//};

blk_tc_decls: {
  SHOWPARSE("blk_tcdecls -> <empty>");
  $$ = AST::make(at_tcdecls);
};

blk_tc_decls: blk_tc_decls blk_tc_decl {
  SHOWPARSE("blk_tcdecls -> blk_tcdelcs sxp_tcdecl");
  $$ = $1;
  $$->addChild($2);
};

sxp_tc_decls: {
  SHOWPARSE("sxp_tcdecls -> <empty>");
  $$ = AST::make(at_tcdecls);
};

sxp_tc_decls: sxp_tc_decls sxp_tc_decl {
  SHOWPARSE("sxp_tcdecls -> sxp_tcdelcs sxp_tcdecl");
  $$ = $1;
  $$->addChild($2);
};

blk_tc_decl: tk_TYFN '(' blk_tvlist ')' tk_FNARROW typevar {
  //                     ^^^^^^
  // I really mean sxp_tvlist here, arbitrary types
  // are not acceptable.
  SHOWPARSE("blk_tc_decl -> TYFN ( blk_tvlist ) -> typevar");
  $3->astType = at_fnargVec;
  $$ = AST::make(at_tyfn, $2.loc, $3, $6);
};

sxp_tc_decl: '(' tk_TYFN  sxp_tvlist tk_FNARROW typevar ')' {
  //                         ^^^^^^
  // I really mean sxp_tvlist here, arbitrary types
  // are not acceptable.
  SHOWPARSE("sxp_tc_decl -> ( TYFN sxp_tvlist -> typevar )");
  $3->astType = at_fnargVec;
  $$ = AST::make(at_tyfn, $2.loc, $3, $5);
};

blk_method_decls: /* Nothing */ {
  SHOWPARSE("blk_method_decls -> ");
  LexLoc loc;
  $$ = AST::make(at_method_decls, loc);
};

blk_method_decls: blk_method_decls blk_method_decl {
  SHOWPARSE("blk_method_decls -> blk_method_decls blk_method_decl");
  $$ = $1;
  $$->addChild($2);
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

blk_method_decl: blk_ident ':' blk_fntype {
  SHOWPARSE("blk_method_decl -> blk_ident : blk_fntype");
  $1->flags |= ID_IS_GLOBAL;
  $1->identType = id_tcmethod;
  $$ = AST::make(at_method_decl, $1->loc, $1, $3);
};

sxp_method_decl: sxp_ident ':' sxp_fntype {
  SHOWPARSE("sxp_method_decl -> sxp_ident : sxp_fntype");
  $1->flags |= ID_IS_GLOBAL;
  $1->identType = id_tcmethod;
  $$ = AST::make(at_method_decl, $1->loc, $1, $3);
};

// TYPE CLASS INSTANTIATIONS [4.2]
blk_ti_definition: tk_INSTANCE blk_constraint blk_constraints trn_optdocstring {
  SHOWPARSE("blk_ti_definition -> INSTANCE blk_constraint blk_constraints [docstring])");
  $$ = AST::make(at_definstance, $1.loc, $2,
                 AST::make(at_tcmethods, $1.loc), $3);
};
blk_ti_definition: tk_INSTANCE blk_constraint blk_constraints trn_optdocstring '{' blk_method_bindings '}' {
  SHOWPARSE("blk_ti_definition -> INSTANCE blk_constraint blk_constraints [docstring] '{' blk_method_bindings '}'");
  $$ = AST::make(at_definstance, $1.loc, $2, $6, $3);
};

sxp_ti_definition: LP tk_DEFINSTANCE sxp_constraint trn_optdocstring RP {
  SHOWPARSE("sxp_ti_definition -> ( DEFINSTANCE sxp_constraint [docstring])");
  $$ = AST::make(at_definstance, $2.loc, $3,
                 AST::make(at_tcmethods, $5.loc),
                 AST::make(at_constraints, $3->loc));
};
sxp_ti_definition: LP tk_DEFINSTANCE sxp_constraint trn_optdocstring sxp_method_bindings RP {
  SHOWPARSE("sxp_ti_definition -> ( DEFINSTANCE sxp_constraint [docstring] sxp_method_bindings)");
  $$ = AST::make(at_definstance, $2.loc, $3, $5,
                 AST::make(at_constraints, $3->loc));
};

blk_method_bindings: blk_method_binding {
  SHOWPARSE("blk_method_bindings -> blk_method_binding");
  $$ = AST::make(at_tcmethods, $1->loc, $1);
};
blk_method_bindings: blk_method_bindings blk_method_binding {
  SHOWPARSE("blk_method_bindings -> blk_method_bindings blk_method_binding");
  $$ = $1;
  $$->addChild($2);
};

blk_method_binding: blk_ident '=' blk_stmt {
  SHOWPARSE("blk_method_binding -> blk_ident = blk_stmt");

  $$ = AST::make(at_tcmethod_binding, $1->loc, $1, $3);
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

sxp_method_binding: '(' sxp_ident sxp_ident sxp_expr ')' {
  SHOWPARSE("sxp_method_binding -> ( sxp_ident = sxp_expr )");

  if ($3->s != "=") {
    cerr << $2->loc << ": Syntax error, expecting `='.\n";
    lexer->num_errors++;
  }

  $$ = AST::make(at_tcmethod_binding, $1.loc, $2, $4);
};

// DEFINE  [5.1]
//blk_value_definition: tk_DEF blk_defpattern blk_constraints '=' blk_stmt {
//  SHOWPARSE("blk_value_definition -> DEF  blk_defpattern blk_constraints = blk_expr");
//  $$ = AST::make(at_define, $1.loc, $2, $5, $3);
//};
blk_value_definition: tk_DEF blk_defpattern blk_constraints trn_optdocstring '=' blk_stmt {
  SHOWPARSE("blk_value_definition -> DEF blk_defpattern blk_constraints trn_optdocstring = blk_expr");
  $$ = AST::make(at_define, $1.loc, $2, $6, $3);
};
sxp_value_definition: LP tk_DEFINE sxp_defpattern sxp_expr RP  {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  sxp_defpattern sxp_expr )");
  $$ = AST::make(at_define, $2.loc, $3, $4);
  $$->addChild(AST::make(at_constraints));
};
sxp_value_definition: LP tk_DEFINE sxp_defpattern trn_optdocstring sxp_expr RP  {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  sxp_defpattern trn_optdocstring sxp_expr )");
  $$ = AST::make(at_define, $2.loc, $3, $5);
  $$->addChild(AST::make(at_constraints));
};

blk_value_definition: tk_DEF blk_defident '(' ')' blk_constraints trn_optdocstring '=' blk_stmt {
  SHOWPARSE("blk_value_definition -> DEF  blk_defident () blk_constraints trn_optdocstring blk_stmt");
  // $5 = stripDocString($5);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $1.loc, AST::make(at_ident, LToken("__return")), $8);
  shared_ptr<AST> iLambda =
    AST::make(at_lambda, $1.loc, AST::make(at_argVec, $3.loc), iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $2->loc, $2);
  $$ = AST::make(at_recdef, $1.loc, iP, iLambda, $5);
}

// Define convenience syntax case 1: no arguments
// No trn_docstring here because of sxp_block
sxp_value_definition: LP tk_DEFINE '(' sxp_defident ')' sxp_block RP {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  ( sxp_defident ) [docstring] sxp_block )");
  $6 = stripDocString($6);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $2.loc, AST::make(at_ident, LToken("__return")), $6);
  shared_ptr<AST> iLambda =
    AST::make(at_lambda, $2.loc, AST::make(at_argVec, $3.loc), iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $4->loc, $4);
  $$ = AST::make(at_recdef, $2.loc, iP, iLambda);
  $$->addChild(AST::make(at_constraints));
};

blk_value_definition: tk_DEF blk_defident '(' blk_lambdapatterns ')' blk_constraints trn_optdocstring '=' blk_stmt {
  SHOWPARSE("blk_value_definition -> DEF  blk_defident () blk_constraints trn_optdocstring blk_stmt");
  // $5 = stripDocString($5);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $1.loc, AST::make(at_ident, LToken("__return")), $9);
  shared_ptr<AST> iLambda = AST::make(at_lambda, $1.loc, $4, iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $2->loc, $2);
  $$ = AST::make(at_recdef, $1.loc, iP, iLambda, $6);
}

// Define convenience syntax case 3: one or more arguments
// No trn_docstring here because of sxp_block
sxp_value_definition: LP tk_DEFINE '(' sxp_defident sxp_lambdapatterns ')'
                  sxp_block RP {
  SHOWPARSE("sxp_value_definition -> ( DEFINE  ( sxp_defident sxp_lambdapatterns ) "
            "[docstring] sxp_block )");
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
blk_value_declaration: tk_DEF blk_defpattern blk_constraints trn_optdocstring blk_externals {
  SHOWPARSE("blk_value_declaration -> DEF blk_defpattern blk_constraints trn_optdocstring blk_externals");

  // I had to use blk_defpattern above to eliminate a reduce/reduce
  // conflict, but in a declaration we require the type to be present
  if ($2->children.size() == 1)
    lexer->ReportParseError($2->loc, "Declaration forms require a type");

  shared_ptr<AST> declIdent = $2->child(0);
  shared_ptr<AST> declType = $2->child(1);

  $$ = AST::make(at_proclaim, $1.loc, declIdent, declType, $5);
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

sxp_value_declaration: LP tk_PROCLAIM sxp_defident ':' sxp_type trn_optdocstring sxp_externals RP {
  SHOWPARSE("sxp_if_definition -> ( PROCLAIM sxp_defident : sxp_type trn_optdocstring sxp_externals )");
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

//import_definition: '(' tk_IMPORT sxp_ident sxp_ifident ')' {
//  SHOWPARSE("sxp_import_definition -> ( IMPORT sxp_ident sxp_ifident )");
//  shared_ptr<AST> ifIdent = AST::make(at_ifident, $4);
//  ifIdent->uoc = UocInfo::importInterface(lexer->errStream, $4.loc, $4.str);
//  $$ = AST::make(at_import, $2.loc, $3, ifIdent);
//};

blk_import_definition: tk_IMPORT blk_ifident tk_AS blk_ident {
  SHOWPARSE("blk_import_definition -> IMPORT blk_ifident AS blk_ident;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_importAs, $1.loc, ifIdent, $4);
  if (lexer->currentLang & TransitionLexer::lf_LispComments)
    SHOWPARSE("!!Current lang allows lisp comments");
}

sxp_import_definition: LP tk_IMPORT sxp_ifident tk_AS sxp_ident RP {
  SHOWPARSE("sxp_import_definition -> ( IMPORT sxp_ifident AS sxp_ident )");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_importAs, $2.loc, ifIdent, $5);
};

blk_import_definition: tk_IMPORT blk_ifident {
  SHOWPARSE("blk_import_definition -> IMPORT blk_ifident;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_import, $1.loc, ifIdent);
  if (lexer->currentLang & TransitionLexer::lf_LispComments)
    SHOWPARSE("!!Current lang allows lisp comments");
};

sxp_import_definition: LP tk_IMPORT sxp_ifident RP {
  SHOWPARSE("sxp_import_definition -> (IMPORT sxp_ifident)");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_import, $2.loc, ifIdent);
};

blk_import_definition: tk_IMPORT blk_ifident blk_importList {
  SHOWPARSE("blk_import_definition -> IMPORT blk_ifident blk_importList;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_import, $1.loc, ifIdent);
  $$->addChildrenFrom($3);
};

sxp_import_definition: LP tk_IMPORT sxp_ifident sxp_importList RP {
  SHOWPARSE("sxp_import_definition -> (IMPORT sxp_ifident sxp_importList)");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_import, $2.loc, ifIdent);
  $$->addChildrenFrom($4);
};

blk_importList: blk_alias {
  SHOWPARSE("blk_importList -> blk_alias");
  $$ = AST::make(at_Null, $1->loc, $1);
};
blk_importList: blk_importList ',' blk_alias {
  SHOWPARSE("blk_importList -> blk_importList blk_alias");
  $$ = $1;
  $$->addChild($3);
};

blk_alias: blk_ident {
  SHOWPARSE("blk_alias -> blk_ident");
  // The two identifiers in this case are textually the same, but the
  // need to end up with distinct AST nodes, thus getDCopy().
  $$ = AST::make(at_ifsel, $1->loc, $1, $1->getDeepCopy());
};
blk_alias: blk_ident '=' blk_ident {
  SHOWPARSE("blk_alias -> blk_ident '=' blk_ident");

  $$ = AST::make(at_ifsel, $1->loc, $3, $1);
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

sxp_alias: sxp_ident {
  SHOWPARSE("sxp_alias -> sxp_ident");
  // The two identifiers in this case are textually the same, but the
  // need to end up with distinct AST nodes, thus getDCopy().
  $$ = AST::make(at_ifsel, $1->loc, $1, $1->getDeepCopy());
};
sxp_alias: '(' sxp_ident tk_AS sxp_ident ')' {
  SHOWPARSE("sxp_alias -> ( sxp_ident AS sxp_ident )");

  $$ = AST::make(at_ifsel, $2->loc, $4, $2);
};


// PROVIDE DEFINITION [8.3]
blk_provide_definition: tk_PROVIDE blk_ifident blk_provideList {
  SHOWPARSE("blk_provide_definition -> PROVIDE blk_ifident blk_provideList;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_provide, $1.loc, ifIdent);
  $$->addChildrenFrom($3);
  if (lexer->currentLang & TransitionLexer::lf_LispComments)
    SHOWPARSE("!!Current lang allows lisp comments");
};

blk_provideList: blk_ident {
  SHOWPARSE("blk_provideList -> blk_ident");
  $$ = AST::make(at_Null, $1->loc, $1);
};

blk_provideList: blk_provideList ',' blk_ident {
  SHOWPARSE("blk_provideList -> blk_provideList , blk_ident");
  $$ = $1;
  $$->addChild($3);
};

sxp_provide_definition: LP tk_PROVIDE sxp_ifident sxp_provideList RP {
  SHOWPARSE("sxp_provide_definition -> (PROVIDE sxp_ifident sxp_provideList)");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $3);
  UocInfo::importInterface(lexer->errStream, $3.loc, $3.str);
  $$ = AST::make(at_provide, $2.loc, ifIdent);
  $$->addChildrenFrom($4);
};

sxp_provideList: sxp_ident {
  SHOWPARSE("sxp_provideList -> sxp_ident");
  $$ = AST::make(at_Null, $1->loc, $1);
};

sxp_provideList: sxp_provideList sxp_ident {
  SHOWPARSE("sxp_provideList -> sxp_provideList sxp_ident");
  $$ = $1;
  $$->addChild($2);
};

// definition: '(' tk_DEFTHM sxp_ident sxp_expr ')'  {
//    SHOWPARSE("definition -> ( DEFTHM sxp_ident sxp_expr )");
//    $$ = AST::make(at_defthm, $2.loc, $3, $4);
// };

blk_declares: {
  SHOWPARSE("blk_declares -> <empty>");
  $$ = AST::make(at_declares);
};
blk_declares: blk_declares blk_declare {
  SHOWPARSE("blk_declares -> blk_declares blk_declare");
  $$ = $1;
  $$->addChildrenFrom($2);
};

sxp_declares: {
  SHOWPARSE("sxp_declares -> <empty>");
  $$ = AST::make(at_declares);
};
sxp_declares: sxp_declares sxp_declare {
  SHOWPARSE("sxp_declares -> sxp_declares sxp_declare");
  $$ = $1;
  $$->addChildrenFrom($2);
};

blk_declare: tk_DECLARE blk_decls {
  SHOWPARSE("blk_declare -> DECLARE blk_decls");
  $$ = $2;
};

sxp_declare: '(' tk_DECLARE sxp_decls ')' {
  SHOWPARSE("sxp_declare -> ( DECLARE sxp_decls )");
  $$ = $3;
};

blk_decls: blk_decl {
  SHOWPARSE("blk_decls -> bkl_decl");
  $$ = AST::make(at_declares, $1->loc, $1);
};

blk_decls: blk_decls blk_decl {
  SHOWPARSE("blk_decls -> blk_decls blk_decl");
  $$ = $1;
  $$->addChild($2);
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

// For tag type declaration.
blk_decl: blk_ident tk_AS blk_field_type {
  SHOWPARSE("blk_decl -> blk_ident AS blk_field_type");
  $$ = AST::make(at_declare, $1->loc, $1, $3);
};

sxp_decl: '(' sxp_ident sxp_field_type ')' {
  SHOWPARSE("sxp_decl -> ( sxp_ident sxp_field_type )");
  $$ = AST::make(at_declare, $2->loc, $2, $3);
};
//decl: '(' sxp_ident ')' {
//  SHOWPARSE("sxp_decl -> ( sxp_ident )");
//  $$ = AST::make(at_declare, $2->loc, $2);
//};
blk_decl: blk_ident {
  SHOWPARSE("blk_decl -> blk_ident");
  $$ = AST::make(at_declare, $1->loc, $1);
};

sxp_decl: sxp_ident {
  SHOWPARSE("sxp_decl -> sxp_ident");
  $$ = AST::make(at_declare, $1->loc, $1);
};


/* defunion Constructors */
blk_constructors: blk_constructor {
  SHOWPARSE("blk_constructors -> blk_constructor");
  $$ = AST::make(at_constructors, $1->loc, $1);
};
blk_constructors: blk_constructors blk_constructor {
  SHOWPARSE("blk_constructors -> blk_constructors blk_constructor");
  $$ = $1;
  $$->addChild($2);
};
sxp_constructors: sxp_constructor {
  SHOWPARSE("sxp_constructors -> sxp_constructor");
  $$ = AST::make(at_constructors, $1->loc, $1);
};
sxp_constructors: sxp_constructors sxp_constructor {
  SHOWPARSE("sxp_constructors -> sxp_constructors sxp_constructor");
  $$ = $1;
  $$->addChild($2);
};

blk_constructor: blk_ident {                          /* simple constructor */
  SHOWPARSE("blk_constructor -> blk_ident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
};
blk_constructor: blk_ident '{' blk_fields '}' {  /* compound constructor */
  SHOWPARSE("blk_constructor ->  blk_ident { blk_fields }");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
  $$->addChildrenFrom($3);
};

sxp_constructor: sxp_ident {                          /* simple constructor */
  SHOWPARSE("sxp_constructor -> sxp_ident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
};
sxp_constructor: '(' sxp_ident sxp_fields ')' {  /* compound constructor */
  SHOWPARSE("sxp_constructor ->  ( sxp_ident sxp_fields )");
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
/* repr_constructor: sxp_ident sxp_repr_reprs {                          /\* simple sxp_constructor *\/  */
/*   SHOWPARSE("sxp_repr_constructor -> sxp_defident"); */
/*   $1->flags |= (ID_IS_GLOBAL); */
/*   $$ = AST::make(at_reprctr, $1->loc, $1); */
/* }; */
sxp_repr_constructor: '(' sxp_ident sxp_fields '(' tk_WHERE sxp_repr_reprs ')' ')' {  /* compound sxp_constructor */
  SHOWPARSE("sxp_repr_constructor ->  ( sxp_ident sxp_fields ( WHERE sxp_repr_reprs ) )");
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
sxp_repr_repr: '(' sxp_ident sxp_ident intLit')' {
  SHOWPARSE("sxp_repr_repr ->  ( = sxp_ident intLit )");

  if ($2->s != "==") {
    cerr << $2->loc << ": Syntax error, expecting `=='.\n";
    lexer->num_errors++;
  }

  $$ = AST::make(at_reprrepr, $2->loc, $3, $4);
};


/* defstruct / sxp_constructor / exception sxp_fields */
blk_fields: blk_field  {
  SHOWPARSE("blk_fields -> blk_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

blk_fields: blk_fields blk_field {
  SHOWPARSE("sxp_fields -> sxp_fields sxp_field ");
  $$ = $1;
  $$->addChild($2);
};

sxp_fields: sxp_field  {
  SHOWPARSE("sxp_fields -> sxp_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

sxp_fields: sxp_fields sxp_field {
  SHOWPARSE("sxp_fields -> sxp_fields sxp_field ");
  $$ = $1;
  $$->addChild($2);
};

blk_field: blk_ident ':' blk_field_type  {
  SHOWPARSE("blk_field -> blk_ident : blk_field_type");
  $$ = AST::make(at_field, $1->loc, $1, $3);
};
// FIX: Not clear why this is just bitfieldtype. Why can't it be any
// field type at all? I think it can.
blk_field: tk_FILL ':' blk_bitfieldtype  {
  SHOWPARSE("blk_field -> FILL : blk_bitfieldtype");
  $$ = AST::make(at_fill, $1.loc, $3);
};

sxp_field: sxp_ident ':' sxp_field_type  {
  SHOWPARSE("sxp_field -> sxp_ident : sxp_field_type");
  $$ = AST::make(at_field, $1->loc, $1, $3);
};

sxp_field: '(' tk_THE sxp_field_type sxp_ident ')'  {
  SHOWPARSE("sxp_field -> '(' THE sxp_field_type sxp_ident ')'");
  $$ = AST::make(at_field, $1.loc, $4, $3);
};

sxp_field: '(' tk_FILL sxp_bitfieldtype ')'  {
  SHOWPARSE("sxp_field -> '(' FILL sxp_bitfieldtype ')'");
  $$ = AST::make(at_fill, $1.loc, $3);
};

// Some low level data structures have reserved bit positions that are
// required to hold designated values.
sxp_field: '(' tk_RESERVED sxp_bitfieldtype natLit ')'  {
  SHOWPARSE("sxp_field -> '(' RESERVED sxp_bitfieldtype natLit ')'");
  $$ = AST::make(at_fill, $1.loc, $3, $4);
};

blk_methods_only: blk_methdecl  {
  SHOWPARSE("blk_methods_only -> blk_methdecl");
  $$ = AST::make(at_fields, $1->loc, $1);
};

blk_methods_only: blk_methods_only blk_methdecl  {
  SHOWPARSE("blk_methods_only -> blk_methods_only blk_methdecl");
  $$ = $1;
  $$->addChild($2);
};

blk_methdecl: blk_ident ':' blk_method_type  {
  SHOWPARSE("blk_methdecl -> blk_ident : blk_method_type");
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

sxp_methdecl: sxp_ident ':' sxp_method_type  {
  SHOWPARSE("sxp_methdecl -> sxp_ident : sxp_method_type");
  $$ = AST::make(at_methdecl, $1->loc, $1, $3);
};

//HERE
// blk_fields_and_methods: blk_methdecl  {
//   SHOWPARSE("blk_fields_and_methods -> blk_methdecl");
//   $$ = AST::make(at_fields, $1->loc, $1);
// };

sxp_fields_and_methods: sxp_methdecl  {
  SHOWPARSE("sxp_fields_and_methods -> sxp_methdecl");
  $$ = AST::make(at_fields, $1->loc, $1);
};

blk_fields_and_methods: blk_field  {
  SHOWPARSE("blk_fields_and_methods -> blk_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

sxp_fields_and_methods: sxp_field  {
  SHOWPARSE("sxp_fields_and_methods -> sxp_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

//HERE
// blk_fields_and_methods: blk_fields_and_methods blk_methdecl {
//   SHOWPARSE("blk_fields_and_methods -> blk_fields_and_methods blk_methdecl ");
//   $$ = $1;
//   $$->addChild($2);
// };

sxp_fields_and_methods: sxp_fields_and_methods sxp_methdecl {
  SHOWPARSE("sxp_fields_and_methods -> sxp_fields_and_methods sxp_methdecl ");
  $$ = $1;
  $$->addChild($2);
};

blk_fields_and_methods: blk_fields_and_methods blk_field {
  SHOWPARSE("blk_fields_and_methods -> blk_fields_and_methods blk_field ");
  $$ = $1;
  $$->addChild($2);
};

sxp_fields_and_methods: sxp_fields_and_methods sxp_field {
  SHOWPARSE("sxp_fields_and_methods -> sxp_fields_and_methods sxp_field ");
  $$ = $1;
  $$->addChild($2);
};

// Block tvlist is currently the same as sxp_tvlist, and these may
// merge, but we might still need to change this to comma-separated,
// so keep them separate for now.
blk_tvlist: typevar  {
  SHOWPARSE("blk_tvlist -> typevar");
  $$ = AST::make(at_tvlist, $1->loc, $1);
};
blk_tvlist: blk_tvlist ',' typevar {
  SHOWPARSE("blk_tvlist -> blk_tvlist , typevar");
  $$ = $1;
  $1->addChild($3);
};

sxp_tvlist: typevar  {
  SHOWPARSE("sxp_tvlist -> typevar");
  $$ = AST::make(at_tvlist, $1->loc, $1);
};
sxp_tvlist: sxp_tvlist typevar {
  SHOWPARSE("sxp_tvlist -> sxp_tvlist typevar");
  $$ = $1;
  $1->addChild($2);
};

// TYPES [3]
//  Unfortunately, the grammar for block types requires precedence
//  rules, so it can't follow the order of appearance in the
//  specification. Specifically, we have to deal with precedence rules
//  and associativity for type application and vectors, so the overall
//  shape of the block type grammar is:
//
//  blk_primary_type -> {identifiers, keywords}
//
//  blk_postfix_type -> blk_primary_type
//  blk_postfix_type -> blk_postfix_type '[' ']'
//  blk_postfix_type -> blk_postfix_type '[' intLit ']'
//  blk_postfix_type -> blk_postfix_type '(' type args ')'
//
//  blk_prefix_type -> blk_postfix_type
//  blk_prefix_type -> REF blk_prefix_type
//
//  blk_type -> blk_postfix_type
//
// Since the block type grammar and the s-expr type grammar share
// their primary type production, I moved sxp_useident inside
// primary_type. This is in contrast with the ANSI C grammar, where '.'
// (field select) is handled in the postfix_expression production, but
// the context for resolving '.' is never actually ambiguous, so it
// doesn't matter here.

// FIX: This ought to be blk_primary_type, but we don't have such a
// thing. Since primary_type doesn't actually appear in a left-context
// anywhere, using blk_postfix_type should suffice.
blk_postfix_type: '(' blk_type ')' {
  SHOWPARSE("blk_postfix_type -> '(' blk_type ')'");
  $$ = $2;
}

blk_type: blk_prefix_type {
  SHOWPARSE("blk_type -> blk_prefix_type");
  $$ = $1;
}
blk_prefix_type: blk_postfix_type {
  SHOWPARSE("blk_prefix_type -> blk_postfix_type");
  $$ = $1;
}

primary_type: sxp_useident  {   /* previously defined sxp_type */
  SHOWPARSE("primary_type -> sxp_useident");
  $$ = $1;
};

primary_type: '(' ')' {
  SHOWPARSE("primary_type -> ( )");
  $$ = AST::make(at_primaryType, $1.loc);
  $$->s = "unit";                /* for lookup! */
};

// primary_type: tk_Nat {
//   SHOWPARSE("primary_type -> Nat");
//   // Temporary, for testing:
//   $$ = AST::make(at_primaryType, $1.loc);
//   $$->s = "unit";                /* for lookup! */
// }

bool_type: tk_BOOL {
  SHOWPARSE("bool_type -> BOOL");
  $$ = AST::make(at_primaryType, $1);
};

primary_type: bool_type {
  SHOWPARSE("primary_type -> bool_type");
  $$ = $1;
};

primary_type: tk_CHAR {
  SHOWPARSE("primary_type -> CHAR");
  $$ = AST::make(at_primaryType, $1);
};
primary_type: tk_STRING {
  SHOWPARSE("primary_type -> STRING");
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
  SHOWPARSE("sxp_type -> UINT64");
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
float_type: tk_QUAD {
  SHOWPARSE("float_type -> QUAD");
  $$ = AST::make(at_primaryType, $1);
};

primary_type: any_int_type {
  SHOWPARSE("primary_type -> any_int_type");
  $$ = $1;
};
primary_type: float_type {
  SHOWPARSE("primary_type -> float_type");
  $$ = $1;
};

// EXCEPTION sxp_type
primary_type: tk_EXCEPTION {
  SHOWPARSE("primary_type -> EXCEPTION");
  $$ = AST::make(at_exceptionType, $1.loc);
};

// TYPE VARIABLES [3.3]          
primary_type: typevar  {                 
  SHOWPARSE("primary_type -> typevar");
  $$ = $1;
};


blk_postfix_type: primary_type {
  SHOWPARSE("blk_type -> primary_type");
  $$ = $1;
};

sxp_type: primary_type {
  SHOWPARSE("sxp_type -> primary_type");
  $$ = $1;
};

// REF TYPES [3.4.1]             
blk_prefix_type: tk_REF blk_type {
  SHOWPARSE("blk_prefix_type -> REF blk_type");
  $$ = AST::make(at_refType, $1.loc, $2);
};

sxp_type: '(' tk_REF sxp_type ')' {
  SHOWPARSE("sxp_type -> ( REF sxp_type )");
  $$ = AST::make(at_refType, $2.loc, $3);
};

// VAL TYPES [3.4.2]
//HERE
// blk_type: tk_REF blk_type {
//   SHOWPARSE("blk_type -> VAL blk_type");
//   $$ = AST::make(at_valType, $1.loc, $2);
// };
sxp_type: '(' tk_VAL sxp_type ')' {
  SHOWPARSE("sxp_type -> ( VAL sxp_type )");
  $$ = AST::make(at_valType, $2.loc, $3);
};

// FUNCTION TYPES [3.4.3]
blk_type: blk_fntype {
  SHOWPARSE("blk_type -> blk_fntype");
  $$ = $1;
}
sxp_type: sxp_fntype {
  SHOWPARSE("sxp_type -> sxp_fntype");
  $$ = $1;
}

//HERE
trn_fneffect: {
  SHOWPARSE("trn_fneffect -> <empty>");
  $$ = AST::make(at_ident, LToken("impure"));
};

trn_fneffect: tk_PURE {
  SHOWPARSE("trn_fneffect -> PURE");
  $$ = AST::make(at_ident, $1);
};

trn_fneffect: tk_IMPURE {
  SHOWPARSE("trn_fneffect -> IMPURE");
  $$ = AST::make(at_ident, $1);
};

trn_fneffect: tk_EffectVar {
  SHOWPARSE("trn_fneffect -> <EffectVar=" + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

blk_fntype: trn_fneffect tk_FN '(' ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_fntype -> trn_fneffect FN () -> blk_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $5.loc);
  $$ = AST::make(at_fn, $1->loc, fnargVec, $6);
};

// Reworked by shap on 10/9/2008 to use arrow syntax
sxp_fntype: '(' trn_fneffect tk_FN tk_FNARROW sxp_type ')' {
  SHOWPARSE("sxp_fntype -> ( trn_fneffect FN -> sxp_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $4.loc);
  $$ = AST::make(at_fn, $1.loc, fnargVec, $5);
};
//fntype: '(' trn_fneffect tk_FN '(' ')' sxp_type ')' {
//  SHOWPARSE("sxp_fntype -> ( trn_fneffect FN () sxp_type )");
//  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $4.loc);
//  $$ = AST::make(at_fn, $1.loc, fnargVec, $6);
//};

blk_type_args: blk_type  {
  SHOWPARSE("blk_type_args -> blk_type");
  $$ = AST::make(at_Null);
  $$->addChild($1);
};
blk_type_args: blk_type_args ',' blk_type {
  SHOWPARSE("blk_type_args -> blk_type_args, blk_type");
  $$ = $1;
  $1->addChild($3);
};

sxp_type_args: sxp_type  {
  SHOWPARSE("sxp_type_args -> sxp_type");
  $$ = AST::make(at_Null);
  $$->addChild($1);
};
sxp_type_args: sxp_type_args sxp_type {
  SHOWPARSE("sxp_type_args -> sxp_type_args sxp_type");
  $$ = $1;
  $1->addChild($2);
};

blk_fntype: trn_fneffect tk_FN '(' blk_type_args_pl_byref ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_fntype -> trn_fneffect FN ( blk_type_args_pl_byref ) -> blk_type )");
  $$ = AST::make(at_fn, $1->loc, $4, $7);
};

// Reworked by shap on 10/9/2008 to use arrow syntax
sxp_fntype: '(' trn_fneffect tk_FN sxp_type_args_pl_byref tk_FNARROW sxp_type ')'  {
  SHOWPARSE("sxp_fntype -> ( trn_fneffect FN sxp_type_args_pl_byref -> sxp_type )");
  $$ = AST::make(at_fn, $1.loc, $4, $6);
};
//fntype: '(' trn_fneffect tk_FN '(' sxp_type_args_pl_byref ')' sxp_type ')'  {
//  SHOWPARSE("sxp_fntype -> ( trn_fneffect FN ( sxp_type_args_pl_byref ) sxp_type )");
//  $$ = AST::make(at_fn, $1.loc, $5, $7);
//};

// METHOD TYPES [3.9]
blk_method_type: trn_fneffect tk_METHOD '(' ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_method_type -> trn_fneffect METHOD () -> blk_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $5.loc);
  $$ = AST::make(at_methType, $1->loc, fnargVec, $6);
};

sxp_method_type: '(' trn_fneffect tk_METHOD tk_FNARROW sxp_type ')' {
  SHOWPARSE("sxp_method_type -> ( trn_fneffect METHOD -> sxp_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $4.loc);
  $$ = AST::make(at_methType, $1.loc, fnargVec, $5);
};

blk_method_type: trn_fneffect tk_METHOD '(' blk_type_args_pl_byref ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_method_type -> trn_fneffect METHOD ( blk_type_args_pl_byref ) -> blk_type )");
  $$ = AST::make(at_fn, $1->loc, $4, $7);
};

sxp_method_type: '(' trn_fneffect tk_METHOD sxp_type_args_pl_byref tk_FNARROW sxp_type ')' {
  SHOWPARSE("sxp_method_type -> ( trn_fneffect METHOD sxp_type_args_pl_byref -> sxp_type )");
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

blk_type_cpair: blk_type ',' blk_type {
  SHOWPARSE("blk_type_cpair -> blk_type ',' blk_type");
  $$ = AST::make(at_typeapp, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};
blk_type_cpair: blk_type ',' blk_type_cpair {
  SHOWPARSE("blk_type_cpair -> blk_type ',' blk_type_cpair");
  $$ = AST::make(at_typeapp, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};

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

blk_type: '(' blk_type_cpair ')' {
  SHOWPARSE("blk_type -> (blk_type_cpair)");
  $$ = $2;
};
sxp_type: '(' sxp_type_cpair ')' {
  SHOWPARSE("sxp_type -> (sxp_type_cpair)");
  $$ = $2;
};

// ARRAY TYPE [3.5.1]               
blk_postfix_type: blk_postfix_type '[' natLit ']'  {
  SHOWPARSE("blk_postfix_type -> blk_postfix_type '[' natLit ']'");
  $$ = AST::make(at_arrayType, $1->loc, $1, $3);
};
sxp_type: '(' tk_ARRAY sxp_type natLit ')'  {
  SHOWPARSE("sxp_type -> ( ARRAY sxp_type natLit )");
  $$ = AST::make(at_arrayType, $2.loc, $3, $4);
};
// VECTOR TYPE [3.5.2]             
blk_postfix_type: blk_postfix_type '[' ']'  {
  SHOWPARSE("blk_postfix_type -> blk_postfix_type '[' ']'");
  $$ = AST::make(at_vectorType, $1->loc, $1);
};
sxp_type: '(' tk_VECTOR sxp_type ')' {
  SHOWPARSE("sxp_type -> (VECTOR sxp_type )");
  $$ = AST::make(at_vectorType, $2.loc, $3);
};

// TYPE CONSTRUCTORS (typeapp)
//blk_type:  blk_useident '(' blk_type_args ')' {
//  SHOWPARSE("blk_typeapp -> blk_useident (blk_type_args)");
//  $$ = AST::make(at_typeapp, $1->loc, $1);
//  $$->addChildrenFrom($3);
//};
blk_postfix_type: blk_typeapp {
  SHOWPARSE("blk_postfix_type -> blk_typeapp");
  $$ = $1;
};

blk_typeapp: blk_useident '(' blk_type_args ')' %prec '(' {
  SHOWPARSE("blk_typeapp -> blk_useident (blk_type_args)");
  $$ = AST::make(at_typeapp, $1->loc, $1);
  $$->addChildrenFrom($3);
};

sxp_type: sxp_typeapp {
  SHOWPARSE("sxp_type -> sxp_typeapp");
  $$ = $1;
};

sxp_typeapp: '(' sxp_useident sxp_type_args ')' {
  SHOWPARSE("sxp_typeapp -> ( sxp_useident sxp_type_args )");
  $$ = AST::make(at_typeapp, $2->loc, $2);
  $$->addChildrenFrom($3);
};

// MUTABLE TYPE [3.7]            
blk_prefix_type: tk_MUTABLE blk_type {
  SHOWPARSE("blk_prefix_type -> MUTABLE blk_type");
  $$ = AST::make(at_mutableType, $1.loc, $2);
}
blk_prefix_type: tk_CONST blk_type {
  SHOWPARSE("blk_prefix_type -> CONST blk_type");
  $$ = AST::make(at_constType, $1.loc, $2);
}

sxp_type: '(' tk_MUTABLE sxp_type ')' {
  SHOWPARSE("sxp_type -> ( MUTABLE sxp_type )");
  $$ = AST::make(at_mutableType, $2.loc, $3);
};

sxp_type: '(' tk_CONST sxp_type ')' {
  SHOWPARSE("sxp_type -> ( CONST sxp_type )");
  $$ = AST::make(at_constType, $2.loc, $3);
  };

// BITFIELD TYPE
blk_bitfieldtype: any_int_type '(' natLit ')' {
  SHOWPARSE("blk_bitfieldtype -> any_int_type ( natLit )");
  $$ = AST::make(at_bitfield, $1->loc, $1, $3);
};
blk_bitfieldtype: bool_type '(' natLit ')' {
  SHOWPARSE("blk_bitfieldtype -> bool_type ( natLit )");
  $$ = AST::make(at_bitfield, $1->loc, $1, $3);
};

sxp_bitfieldtype: '(' tk_BITFIELD any_int_type natLit ')' {
  SHOWPARSE("sxp_bitfieldtype -> ( BITFIELD any_int_type natLit )");
  $$ = AST::make(at_bitfield, $2.loc, $3, $4);
};
sxp_bitfieldtype: '(' tk_BITFIELD bool_type natLit ')' {
  SHOWPARSE("sxp_bitfieldtype -> ( BITFIELD bool_type natLit )");
  $$ = AST::make(at_bitfield, $2.loc, $3, $4);
};

// Any non-by-ref type, including bitfield type
blk_field_type: blk_bitfieldtype {
  SHOWPARSE("blk_field_type -> blk_bitfieldtype");
  $$ = $1;
};

sxp_field_type: sxp_bitfieldtype {
  SHOWPARSE("sxp_field_type -> sxp_bitfieldtype");
  $$ = $1;
};

blk_field_type: blk_type {
  SHOWPARSE("blk_field_type -> blk_type");
  $$ = $1;
};

sxp_field_type: sxp_type {
  SHOWPARSE("sxp_field_type -> sxp_type");
  $$ = $1;
};

// by-ref sxp_type_args are not a part of general `type' rule.
// They are gramatiocally restricted to apprae only on
// formal function arguments and function types.
blk_type_pl_byref: blk_type {
  SHOWPARSE("blk_type_pl_byref -> blk_type");
  $$ = $1;
};

sxp_type_pl_byref: sxp_type {
  SHOWPARSE("sxp_type_pl_byref -> sxp_type");
  $$ = $1;
};

blk_type_pl_byref: tk_BY_REF blk_type {
  SHOWPARSE("blk_type_pl_byref -> BY-REF blk_type");
  $$ = AST::make(at_byRefType, $1.loc, $2);
};

sxp_type_pl_byref: '(' tk_BY_REF sxp_type ')' {
  SHOWPARSE("sxp_type_pl_byref -> ( BY-REF sxp_type )");
  $$ = AST::make(at_byRefType, $2.loc, $3);
};

blk_type_pl_byref: tk_ARRAY_REF blk_type {
  SHOWPARSE("blk_type_pl_byref -> ARRAY-REF blk_type");
  $$ = AST::make(at_arrayRefType, $1.loc, $2);
};

sxp_type_pl_byref: '(' tk_ARRAY_REF sxp_type ')' {
  SHOWPARSE("sxp_type_pl_byref -> ( ARRAY-REF sxp_type )");
  $$ = AST::make(at_arrayRefType, $2.loc, $3);
};

blk_type_args_pl_byref: blk_type_pl_byref {
  SHOWPARSE("blk_type_args_pl_byref -> blk_type_pl_byref");
  $$ = AST::make(at_fnargVec);
  $$->addChild($1);
};
blk_type_args_pl_byref: blk_type_args_pl_byref ',' blk_type_pl_byref {
  SHOWPARSE("blk_type_args_pl_byref -> blk_type_args_pl_byref blk_type_pl_byref");
  $$ = $1;
  $1->addChild($3);
};

sxp_type_args_pl_byref: sxp_type_pl_byref {
  SHOWPARSE("sxp_type_args_pl_byref -> sxp_type_pl_byref");
  $$ = AST::make(at_fnargVec);
  $$->addChild($1);
};
sxp_type_args_pl_byref: sxp_type_args_pl_byref sxp_type_pl_byref {
  SHOWPARSE("sxp_type_args_pl_byref -> sxp_type_args_pl_byref sxp_type_pl_byref");
  $$ = $1;
  $1->addChild($2);
};

// BINDING PATTERNS [5.1]
blk_bindingpattern: blk_ident {
  SHOWPARSE("blk_bindingpattern -> blk_ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
sxp_bindingpattern: sxp_ident {
  SHOWPARSE("sxp_bindingpattern -> sxp_ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};

blk_bindingpattern: blk_ident ':' blk_type {
  SHOWPARSE("blk_bindingpattern -> blk_ident : blk_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
};
sxp_bindingpattern: sxp_ident ':' sxp_type {
  SHOWPARSE("sxp_bindingpattern -> sxp_ident : sxp_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
};

sxp_bindingpattern: '(' tk_THE sxp_type sxp_ident ')' {
  SHOWPARSE("sxp_bindingpattern -> sxp_ident : sxp_type");
  $$ = AST::make(at_identPattern, $1.loc, $4, $3);
};

// There are no sxp_defpattern sequences, because there is no top-level
// pattern application
// DEFPATTERN
blk_defpattern: blk_defident {
  SHOWPARSE("blk_defpattern -> blk_defident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
blk_defpattern: blk_defident ':' blk_type {
  SHOWPARSE("blk_defpattern -> blk_defident : blk_qual_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
};
sxp_defpattern: sxp_defident {
  SHOWPARSE("sxp_defpattern -> sxp_defident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
sxp_defpattern: sxp_defident ':' sxp_type {
  SHOWPARSE("sxp_defpattern -> sxp_defident : sxp_qual_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
};
sxp_defpattern: '(' tk_THE sxp_type sxp_defident ')' {
  SHOWPARSE("sxp_defpattern -> (THE sxp_qual_type sxp_defident)");
  $$ = AST::make(at_identPattern, $1.loc, $4, $3);
};


/* Lambda Patterns -- with an additional by-ref annotation */
blk_lambdapatterns: blk_lambdapattern {
  SHOWPARSE("blk_lambdapatterns -> blk_lambdapattern");
  $$ = AST::make(at_argVec, $1->loc);
  $$->addChild($1);
};
blk_lambdapatterns: blk_lambdapatterns ',' blk_lambdapattern {
  SHOWPARSE("blk_lambdapatterns -> blk_lambdapatterns , blk_lambdapattern");
  $$ = $1;
  $$->addChild($3);
};
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

blk_lambdapattern: blk_ident {
  SHOWPARSE("blk_lambdapattern -> blk_ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
sxp_lambdapattern: sxp_ident {
  SHOWPARSE("sxp_lambdapattern -> sxp_ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};

blk_lambdapattern: blk_ident ':' blk_type_pl_byref {
  SHOWPARSE("blk_lambdapattern -> blk_ident : blk_type_pl_byref");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
  if ($3->astType == at_byRefType)
    $1->flags |= ARG_BYREF;
};
sxp_lambdapattern: sxp_ident ':' sxp_type_pl_byref {
  SHOWPARSE("sxp_lambdapattern -> sxp_ident : sxp_type_pl_byref");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
  if ($3->astType == at_byRefType)
    $1->flags |= ARG_BYREF;
};

sxp_lambdapattern: '(' tk_THE sxp_type sxp_ident ')' {
  SHOWPARSE("sxp_lambdapattern -> ( THE sxp_type sxp_ident ) ");
  $$ = AST::make(at_identPattern, $1.loc, $4, $3);
};

sxp_lambdapattern: '(' tk_THE '(' tk_BY_REF sxp_type ')' sxp_ident ')' {
  SHOWPARSE("sxp_lambdapattern -> ( THE ( BY-REF sxp_type ) sxp_ident )");
  $$ = AST::make(at_identPattern, $1.loc, $7, $5);
  $5->flags |= ARG_BYREF;
};

sxp_lambdapattern: '(' tk_THE '(' tk_ARRAY_REF sxp_type ')' sxp_ident ')' {
  SHOWPARSE("sxp_lambdapattern -> ( THE ( ARRAY-REF sxp_type ) sxp_ident )");
  $$ = AST::make(at_identPattern, $1.loc, $7, $5);
};

// EXPRESSIONS [7]
//
// sxp_expr   -- an expression form with an optional sxp_type qualifier
// sxp_unqual_expr  -- a naked expression form
//
// As a practical matter, every expression on the RHS of a production
// should be an sxp_expr.
//
// In the block syntax we have prefix, postfix, and infix expressions
// to worry about. The precedence grammar is:
//
//   primary_expr -> { identifiers, literals }
//
//   postfix_expr -> primary_expr
//   postfix_expr -> postfix_expr '(' {arg list} ')'
//   postfix_expr -> SIZEOF '(' expr ')'
//   postfix_expr -> postfix_expr '[' expr ']'
//
//   prefix_expr -> postfix_expr
//   prefix_expr -> REF prefix_expr
//   prefix_expr -> unary_op prefix_expr
//
//   BEGIN INFIX_EXPR
//
//   mul_expr -> prefix_expr
//   mul_expr -> mul_expr '*' prefix_expr
//   mul_expr -> mul_expr '/' prefix_expr
//   mul_expr -> mul_expr '%' prefix_expr
//
//   add_expr -> mul_expr
//   add_expr -> add_expr '+' mul_expr
//   add_expr -> add_expr '-' mul_expr
//
//   shift_expr -> add_expr
//   shift_expr -> shift_expr << add_expr
//   shift_expr -> shift_expr >> add_expr
//
//   relational_expr -> shift_expr
//   relational_expr -> rel_expr < shift_expr
//   relational_expr -> rel_expr > shift_expr
//   relational_expr -> rel_expr <= shift_expr
//   relational_expr -> rel_expr >= shift_expr
//
//   equal_expr -> relational_expr
//   equal_expr -> equal_expr == relational_expr
//   equal_expr -> equal_expr != relational_expr
//
//   bitand_expr -> equal_expr
//   bitand_expr -> bitand_expr & equal_expr
//
//   bitxor_expr -> bitand_expr
//   bitxor_expr -> bitxor_expr ^ bitand_expr
//
//   bitor_expr -> bitxor_expr
//   bitor_expr -> bitor_expr | bitxor_expr
//
//   and_expr -> bitor_expr
//   and_expr -> and_expr && bitor_expr
//
//   or_expr -> and_expr
//   or_expr -> or_expr && and_expr
//
//   END INFIX_EXPR
//
//   if_expr -> or_expr
//   if_expr -> IF expr THEN or_expr ELSE or_expr
//
//   assign_expr -> if_expr
//   assign_expr -> or_expr '=' assign_expr
//
// As a practical matter, I have collapsed all of the infix operator
// cases using the BISON operator precedence specification capabilities.
//
// note that things like SIZEOF can pretty much go anywhere. We put
// them in postfix_expr mainly to put things of like appearance in a
// common production.

blk_primary_expr: blk_block {
  SHOWPARSE("blk_expr -> blk_block");
  $$ = $1;
}

blk_primary_expr: '(' blk_expr ')' {
  SHOWPARSE("blk_primary_expr -> ( blk_expr )");
  $$ = $2;
  // Be careful to preserve precedence when pretty printing:
  $$->printVariant |= pf_PARENWRAP;
}

blk_postfix_expr: blk_primary_expr {
  SHOWPARSE("blk_postfix_expr -> blk_primary_expr");
  $$ = $1;
}
blk_prefix_expr: blk_postfix_expr %prec tk_NotApply {
  SHOWPARSE("blk_prefix_expr -> blk_postfix_expr");
  $$ = $1;
}

// In C, the cast operator appears at this point in the hierarchy:
blk_tqual_expr: blk_prefix_expr {
  SHOWPARSE("blk_tqual_expr -> blk_prefix_expr");
  $$ = $1;
}

blk_infix_expr: blk_tqual_expr {
  SHOWPARSE("blk_infix_expr -> blk_prefix_expr");
  $$ = $1;
}

// This is the only production defining blk_expr. It is causing
// a (correctly resolved) S/R ambiguity that we should probably try to
// clean up.
blk_expr: blk_infix_expr {
  SHOWPARSE("blk_expr -> blk_infix_expr");
  $$ = $1;
}

blk_stmt: blk_expr {
  SHOWPARSE("blk_stmt -> blk_expr");
  $$ = $1;
}

blk_stmt: blk_stmt ';' {
  SHOWPARSE("blk_stmt -> blk_stmt ;");
  $$ = $1;
}

//blk_stmt: blk_expr SC {
//  SHOWPARSE("blk_stmt -> blk_expr ;");
//  $$ = $1;
//}

// See also labeled exit, far below.
//blk_stmt: blk_block {
//  SHOWPARSE("blk_stmt -> blk_block ;");
//  $$ = $1;
//}

blk_block: '{' blk_stmt_seq '}' {
  SHOWPARSE("blk_block -> { blk_stmt_seq }");
  // Remove redundant blocks eagerly:
  if ($2->children.size() == 1 && $2->child(0)->astType == at_begin)
    $2 = $2->child(0);
  $$ = $2;
}
blk_stmt_seq: blk_stmt {
  SHOWPARSE("blk_stmt_seq -> blk_stmt");
  $$ = AST::make(at_begin, $1->loc, $1);
};
blk_stmt_seq: blk_value_definition {
  SHOWPARSE("blk_stmt_seq -> blk_value_definition");
  $$ = AST::make(at_begin, $1->loc, $1);
};
blk_stmt_seq: blk_stmt_seq blk_stmt {
  SHOWPARSE("blk_stmt_seq -> blk_stmt_seq blk_stmt");
  $$ = $1;
  $$->addChild($2);
};
blk_stmt_seq: blk_stmt_seq blk_value_definition {
  SHOWPARSE("blk_stmt_seq -> blk_stmt_seq blk_value_definition");
  $$ = $1;
  $$->addChild($2);
};
blk_stmt_seq: blk_stmt_seq SC blk_stmt {
  SHOWPARSE("blk_stmt_seq -> blk_stmt_seq SC blk_stmt");
  $$ = $1;
  $$->addChild($3);
};
blk_stmt_seq: blk_stmt_seq SC blk_value_definition {
  SHOWPARSE("blk_stmt_seq -> blk_stmt_seq SC blk_value_definition");
  $$ = $1;
  $$->addChild($3);
};

sxp_block: sxp_block_exprs {
  SHOWPARSE("sxp_block -> sxp_block_exprs");
  if ($1->children.size() == 1 && $1->child(0)->astType == at_begin)
    $1 = $1->child(0);
  $$ = $1;
}
sxp_block_exprs: sxp_expr {
  SHOWPARSE("sxp_block_exprs -> sxp_expr");
  $$ = AST::make(at_begin, $1->loc, $1);
  $$->printVariant = pf_IMPLIED;
};
sxp_block_exprs: sxp_value_definition {
  SHOWPARSE("sxp_block_exprs -> sxp_value_definition");
  $$ = AST::make(at_begin, $1->loc, $1);
  $$->printVariant = pf_IMPLIED;
};
sxp_block_exprs: sxp_block_exprs sxp_expr {
  SHOWPARSE("sxp_block_exprs -> sxp_block_exprs sxp_expr");
  $$ = $1;
  $$->addChild($2);
};
sxp_block_exprs: sxp_block_exprs sxp_value_definition {
  SHOWPARSE("sxp_block_exprs -> sxp_block_exprs sxp_value_definition");
  $$ = $1;
  $$->addChild($2);
};

blk_actual_params: {
  SHOWPARSE("blk_actual_params -> ");
  $$ = AST::make(at_Null);
};
blk_actual_params: blk_nonempty_params {
  SHOWPARSE("blk_actual_params -> blk_nonempty_params");
  $$ = $1;
};
blk_nonempty_params: blk_expr {
  SHOWPARSE("blk_nonempty_params -> blk_expr");
  $$ = AST::make(at_Null, $1->loc, $1);
};
blk_nonempty_params: blk_nonempty_params ',' blk_expr {
  SHOWPARSE("blk_nonempty_params -> blk_nonempty_params , blk_expr");
  $$ = $1;
  $$->addChild($3);
};

sxp_actual_params: {
  SHOWPARSE("sxp_actual_params -> ");
  $$ = AST::make(at_Null);
};
sxp_actual_params: sxp_nonempty_params {
  SHOWPARSE("sxp_actual_params -> sxp_nonempty_params");
  $$ = $1;
};
sxp_nonempty_params: sxp_expr {
  SHOWPARSE("sxp_nonempty_params -> sxp_expr");
  $$ = AST::make(at_Null, $1->loc, $1);
};
sxp_nonempty_params: sxp_nonempty_params sxp_expr {
  SHOWPARSE("sxp_nonempty_params -> sxp_nonempty_params sxp_expr");
  $$ = $1;
  $$->addChild($2);
};

// TYPE QUALIFIED EXPRESSIONS  [7.3]
blk_tqual_expr: blk_tqual_expr ':' blk_type {
  SHOWPARSE("blk_tqual_expr -> blk_tqual_expr : blk_type");
  $$ = AST::make(at_tqexpr, $1->loc, $1, $3);
};

sxp_expr: sxp_unqual_expr {
  SHOWPARSE("sxp_expr -> sxp_unqual_expr");
  $$ = $1;
};
sxp_expr: sxp_expr ':' sxp_type {
  SHOWPARSE("sxp_expr -> sxp_expr : sxp_type");
  $$ = AST::make(at_tqexpr, $1->loc, $1, $3);
};
sxp_expr: sxp_the_expr {
  SHOWPARSE("sxp_expr -> sxp_the_expr");
  $$ = $1;
};

sxp_the_expr: '(' tk_THE sxp_type sxp_unqual_expr ')' {
  SHOWPARSE("sxp_the_expr -> ( THE sxp_type sxp_unqual_expr )");
  // Note: argument order swapped for historical reasons.
  $$ = AST::make(at_tqexpr, $2.loc, $4, $3);
};


// SUSPENDED EXPRESSIONS
sxp_expr: '(' tk_SUSPEND sxp_useident sxp_expr ')' {
  SHOWPARSE("sxp_expr -> ( SUSPEND sxp_useident sxp_expr )");
  $$ = AST::make(at_suspend, $2.loc, $3, $4);
};

// LITERALS  [7.1]
blk_primary_expr: trn_literal {
  SHOWPARSE("blk_primary_expr -> Literal");
  $$ = $1;
};
sxp_unqual_expr: trn_literal {
  SHOWPARSE("sxp_unqual_expr -> Literal");
  $$ = $1;
};

blk_postfix_expr: tk_SIZEOF '(' blk_type ')' {
  SHOWPARSE("blk_postfix_expr -> SIZEOF (blk_type)");
  $$ = AST::make(at_sizeof, $1.loc, $3);
};
sxp_unqual_expr: '(' tk_SIZEOF sxp_type ')' {
  SHOWPARSE("sxp_unqual_expr -> (SIZEOF sxp_type)");
  $$ = AST::make(at_sizeof, $2.loc, $3);
};

blk_postfix_expr: tk_BITSIZEOF '(' blk_type ')' {
  SHOWPARSE("blk_postfix_expr -> BITSIZEOF (blk_type)");
  $$ = AST::make(at_bitsizeof, $1.loc, $3);
};
sxp_unqual_expr: '(' tk_BITSIZEOF sxp_type ')' {
  SHOWPARSE("sxp_unqual_expr -> (BITSIZEOF sxp_type)");
  $$ = AST::make(at_bitsizeof, $2.loc, $3);
};

// UNIT EXPRESSIONS   [7.4.1]
blk_primary_expr: '(' ')' {
  SHOWPARSE("blk_primary_expr -> ()");
  $$ = AST::make(at_unit, $1.loc);
};
sxp_unqual_expr: '(' ')' {
  SHOWPARSE("sxp_unqual_expr -> ()");
  $$ = AST::make(at_unit, $1.loc);
};

// Expressions that involve locations:

// IDENTIFIERS [7.2]
/* This would actually have been
sxp_unqual_expr: sxp_useident {
  SHOWPARSE("sxp_unqual_expr -> sxp_useident");
  $$ = $1;
};
but for the ambiguity with record (field) selection.
So, the burden is now passed to further stages */

blk_primary_expr: blk_ident {
  SHOWPARSE("blk_primary_expr -> blk_ident");
  $$ = $1;
};
sxp_unqual_expr: sxp_ident {
  SHOWPARSE("sxp_unqual_expr -> sxp_ident");
  $$ = $1;
};

// MEMBER [7.9]
// In principle, we would like to accept expr.ident here, but that
// creates a parse conflict with expr:type, because the sequence
//
//    expr : type . ident
//
// can turn into:
//
//    expr : Id . Id . ident
//           ^^^^^^^
//             type
//
// which creates a shift-reduce conflict. It's all fine as long as the
// sxp_expr is fully bracketed, so there is no problem accepting:
//
//    (the type expr).ident
//
// We have adopted the solution of declaring that the ":" convenience
// syntax cannot be used inside a member selection. This is not likely
// to be burdensome. If the sxp_expr on the LHS is a locally computed
// result, the sxp_type will be inferred through chaining from the
// sxp_constructor expression. The only case where this is likely to be
// annoying is for arguments of structure type, but we do not infer
// structure types from sxp_field names in any case, so those will
// probably require argument declarations in any case.

blk_postfix_expr: blk_postfix_expr '.' blk_ident {
  SHOWPARSE("blk_postfix_expr -> blk_postfix_expr . blk_ident");
  $$ = AST::make(at_select, $1->loc, $1, $3);
};
sxp_unqual_expr: sxp_unqual_expr '.' sxp_ident {
  SHOWPARSE("sxp_unqual_expr -> sxp_unqual_expr . sxp_ident");
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

// No blk variant
sxp_unqual_expr: sxp_the_expr '.' sxp_ident {
  SHOWPARSE("sxp_unqual_expr -> sxp_the_expr . sxp_ident");
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

// No blk variant
sxp_unqual_expr: '(' tk_MEMBER sxp_expr sxp_ident ')' {
  SHOWPARSE("sxp_unqual_expr -> ( member sxp_expr sxp_ident )");
  $$ = AST::make(at_select, $2.loc, $3, $4);
};

// NTH-REF [7.11.2]         
blk_postfix_expr: blk_postfix_expr '[' blk_expr ']' {
  SHOWPARSE("blk_postfix_expr -> blk_postfix_expr [ blk_expr ]");
  $$ = AST::make(at_nth, $1->loc, $1, $3);
};
sxp_unqual_expr: sxp_expr '[' sxp_expr ']' {
  SHOWPARSE("sxp_unqual_expr -> sxp_expr [ sxp_expr ]");
  $$ = AST::make(at_nth, $1->loc, $1, $3);
};
sxp_unqual_expr: '(' tk_NTH sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> ( NTH sxp_expr sxp_expr )");
  $$ = AST::make(at_nth, $2.loc, $3, $4);
};

// DUP [7.17.1]
blk_postfix_expr: tk_DUP '(' blk_expr ')' {
  SHOWPARSE("blk_postfix_expr -> DUP ( blk_expr )");
  $$ = AST::make(at_dup, $1.loc, $3);
};
sxp_unqual_expr: '(' tk_DUP sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> ( DUP sxp_expr )");
  $$ = AST::make(at_dup, $2.loc, $3);
};

// DEREF [7.17.2]               
// Anybody else around here old enough to remember Pascal?
// Here we are dereferencing values by wirth.
blk_postfix_expr: blk_postfix_expr '^' {
  SHOWPARSE("blk_postfix_expr -> blk_postfix ^");
  $$ = AST::make(at_deref, $1->loc, $1);
  $$->printVariant = pf_IMPLIED;
};
sxp_unqual_expr: sxp_expr '^' {
  SHOWPARSE("sxp_unqual_expr -> sxp_expr ^");
  $$ = AST::make(at_deref, $1->loc, $1);
  $$->printVariant = pf_IMPLIED;
};

blk_postfix_expr: tk_DEREF '(' blk_expr ')' {
  SHOWPARSE("blk_postfix_expr -> DEREF ( blk_expr )");
  $$ = AST::make(at_deref, $1.loc, $3);
};
sxp_unqual_expr: '(' tk_DEREF sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> ( DEREF sxp_expr )");
  $$ = AST::make(at_deref, $2.loc, $3);
};

// INNER-REF
// In the case of structures, the second "sxp_expression"
// must be a label. This cannot be checked until
// type-checking phase.
/* eform: '(' tk_INNER_REF sxp_expr sxp_expr ')' { */
/*   SHOWPARSE("sxp_unqual_expr -> ( INNER_REF sxp_expr sxp_expr)"); */
/*   $$ = AST::make(at_inner_ref, $2.loc, $3, $4); */
/* }; */

// End of locations

// PAIR EXPRESSIONS
// FIX: Should we just do this with a tuple syntax, because this is
//      *begging* for issues with procedure call. The only reason
//      we are going to get away with it is because it must appear
//      within parenthesis, and if we have seen
//
//        expr ( . expr
//
//      we already know at the time of shift that we are doing
//      function application. GACK!
//
//      In any case, this is an issue that we should certainly revisit
//      before introducing mixfix.

blk_expr_cpair: blk_expr ',' blk_expr {
  SHOWPARSE("blk_expr_cpair -> blk_expr ',' blk_expr");
  $$ = AST::make(at_apply, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};
blk_expr_cpair: blk_expr ',' blk_expr_cpair {
  SHOWPARSE("blk_expr_cpair -> blk_expr ',' blk_expr_cpair");
  $$ = AST::make(at_apply, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};

sxp_unqual_expr_cpair: sxp_expr ',' sxp_expr {
  SHOWPARSE("sxp_unqual_expr_cpair -> sxp_expr ',' sxp_expr");
  $$ = AST::make(at_apply, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};
sxp_unqual_expr_cpair: sxp_expr ',' sxp_unqual_expr_cpair {
  SHOWPARSE("sxp_unqual_expr_cpair -> sxp_expr ',' sxp_unqual_expr_cpair");
  $$ = AST::make(at_apply, $2.loc,
               AST::make(at_ident, LToken($2.loc, "pair")),
               $1, $3);
  $$->printVariant = pf_IMPLIED;
};

blk_primary_expr: '(' blk_expr_cpair ')' {
  SHOWPARSE("blk_unqual_expr -> ( blk_expr_cpair )");
  $$ = $2;
};
sxp_unqual_expr: '(' sxp_unqual_expr_cpair ')' {
  SHOWPARSE("sxp_unqual_expr -> ( sxp_unqual_expr_cpair )");
  $$ = $2;
};

blk_postfix_expr: tk_MAKE_VECTOR '(' blk_expr ',' blk_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> MAKE-VECTOR ( sxp_expr , sxp_expr )");
  $$ = AST::make(at_makevectorL, $1.loc, $3, $5);
};
sxp_unqual_expr: '(' tk_MAKE_VECTOR sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> ( MAKE-VECTOR sxp_expr sxp_expr )");
  $$ = AST::make(at_makevectorL, $2.loc, $3, $4);
};

// VECTORS [7.4.3]
blk_postfix_expr: tk_VECTOR '(' blk_actual_params ')' {
  SHOWPARSE("blk_postfix_expr -> VECTOR (blk_actual_params)");
  $$ = $3;
  $$->astType = at_vector;
  $$->loc = $1.loc;
};
sxp_unqual_expr: '(' tk_VECTOR sxp_actual_params ')' {
  // Zero-length vector is useless, but legal
  SHOWPARSE("sxp_unqual_expr -> (VECTOR sxp_actual_params)");
  $$ = $3;
  $$->astType = at_vector;
  $$->loc = $2.loc;
};

// ARRAYS [7.4.3]
blk_postfix_expr: tk_ARRAY '(' blk_nonempty_params ')' {
  // Zero-length vector is illegal
  SHOWPARSE("blk_postfix_expr -> (ARRAY blk_nonempty_params)");
  $$ = $3;
  $$->astType = at_array;
  $$->loc = $1.loc;
};
sxp_unqual_expr: '(' tk_ARRAY sxp_nonempty_params ')' {
  // Zero-length vector is illegal
  SHOWPARSE("sxp_unqual_expr -> (ARRAY sxp_nonempty_params)");
  $$ = $3;
  $$->astType = at_array;
  $$->loc = $2.loc;
};

// BEGIN [7.5]
sxp_unqual_expr: '(' tk_BEGIN sxp_block_exprs ')' {
  // I use sxp_block_exprs here because sxp_block elides the begin
  // form when there is only one element. sxp_block_exprs does not.
  SHOWPARSE("sxp_unqual_expr -> ( BEGIN sxp_block_exprs )");
  $$ = $3;
  $$->loc = $2.loc;
  $$->printVariant &= ~pf_IMPLIED;
};

// LABELS and LABELED EXIT [7.6]
// Note that we do not want a generalized blk_ident in the first
// position here, because that creates an ambiguity with primary
// expressions. Note that we only want a local identifier here in any
// case, and not an operator, so using tk_BlkIdent is fine here.
blk_stmt: tk_BlkIdent ':' blk_block {
  SHOWPARSE("blk_stmt -> Ident : blk_block");
  $$ = AST::make(at_block, $1.loc, 
                 $$ = AST::make(at_ident, $1),
                 $3);
}
sxp_unqual_expr: '(' tk_BLOCK sxp_ident sxp_block ')' {
  SHOWPARSE("sxp_unqual_expr -> (BLOCK sxp_ident sxp_block)");
  $$ = AST::make(at_block, $2.loc, $3, $4);
}

blk_stmt: tk_FROM blk_ident tk_RETURN blk_stmt {
  SHOWPARSE("blk_stmt -> FROM blk_ident RETURN blk_stmt");
  $$ = AST::make(at_return_from, $1.loc, $2, $4);
}
sxp_unqual_expr: '(' tk_FROM sxp_ident tk_RETURN sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> (RETURN sxp_ident FROM sxp_expr)");
  $$ = AST::make(at_return_from, $2.loc, $3, $5);
}

// ARRAY-LENGTH [7.11.1]
// VECTOR-LENGTH [7.11.1]
// Replaced by v.length, a.length, ar.length

// convenience syntax: multiple arguments
blk_stmt: tk_LAMBDA '(' ')' blk_stmt {
  SHOWPARSE("blk_expr -> LAMBDA () blk_expr )");
  shared_ptr<AST> argVec = AST::make(at_argVec, $2.loc);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $1.loc, AST::make(at_ident, LToken("__return")), $4);
  $$ = AST::make(at_lambda, $1.loc, argVec, iRetBlock);
};
sxp_unqual_expr: '(' tk_LAMBDA '(' ')' sxp_block ')'  {
  SHOWPARSE("lambda -> ( LAMBDA () sxp_block )");
  shared_ptr<AST> argVec = AST::make(at_argVec, $3.loc);
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $2.loc, AST::make(at_ident, LToken("__return")), $5);
  $$ = AST::make(at_lambda, $2.loc, argVec, iRetBlock);
};

blk_stmt: tk_LAMBDA '(' blk_lambdapatterns ')' blk_stmt {
  SHOWPARSE("blk_stmt -> LAMBDA (blk_lambdapatterns) blk_expr");
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $1.loc, AST::make(at_ident, LToken("__return")), $5);
  $$ = AST::make(at_lambda, $1.loc, $3, iRetBlock);
};
sxp_unqual_expr: '(' tk_LAMBDA '(' sxp_lambdapatterns ')' sxp_block ')'  {
  SHOWPARSE("lambda -> ( LAMBDA (sxp_lambdapatterns) sxp_block )");
  shared_ptr<AST> iRetBlock =
    AST::make(at_block, $2.loc, AST::make(at_ident, LToken("__return")), $6);
  $$ = AST::make(at_lambda, $2.loc, $4, iRetBlock);
};

// RETURN [7.13]         
blk_stmt: tk_RETURN blk_stmt {
  SHOWPARSE("blk_stmt -> RETURN blk_stmt");
  $$ = AST::make(at_return_from, $1.loc,
                 AST::make(at_ident, LToken("__return")), $2);
}
sxp_unqual_expr: '(' tk_RETURN sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> (RETURN sxp_expr)");
  $$ = AST::make(at_return_from, $2.loc,
                 AST::make(at_ident, LToken("__return")), $3);
}

// APPLICATION [7.14]         
blk_postfix_expr: blk_postfix_expr '(' blk_actual_params ')' %prec '(' { /* apply to zero or more args */
  SHOWPARSE("blk_postfix_expr -> blk_postfix_expr ( blk_actual_params )");
  $$ = AST::make(at_apply, $1->loc, $1);
  $$->addChildrenFrom($3);
};
sxp_unqual_expr: '(' sxp_expr sxp_actual_params ')' { /* apply to zero or more args */
  SHOWPARSE("sxp_unqual_expr -> ( sxp_expr sxp_actual_params )");
  $$ = AST::make(at_apply, $2->loc, $2);
  $$->addChildrenFrom($3);
};

// IF [7.15.1]
blk_stmt: tk_IF blk_expr tk_THEN blk_stmt tk_ELSE blk_stmt {
  SHOWPARSE("blk_stmt -> IF blk_expr THEN blk_stmt ELSE blk_stmt");
  $$ = AST::make(at_if, $1.loc, $2, $4, $6);
};
blk_stmt: tk_IF blk_expr tk_THEN blk_stmt {
  SHOWPARSE("blk_stmt -> IF blk_expr THEN blk_stmt");
  $$ = AST::make(at_when, $1.loc, $2, $4);
};
sxp_unqual_expr: '(' tk_IF sxp_expr sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> (IF sxp_expr sxp_expr sxp_expr )");
  $$ = AST::make(at_if, $2.loc, $3, $4, $5);
};

// WHEN [7.15.2]
blk_stmt: tk_WHEN '(' blk_expr ')' blk_stmt {
  SHOWPARSE("blk_stmt -> WHEN (blk_expr) blk_stmt");
  $$ = AST::make(at_when, $1.loc, $3, $5);
};
sxp_unqual_expr: '(' tk_WHEN sxp_expr sxp_block ')' {
  SHOWPARSE("sxp_unqual_expr -> (WHEN sxp_expr sxp_block)");
  $$ = AST::make(at_when, $2.loc, $3, $4);
};

// Unary negation - block syntax only:
blk_prefix_expr: '-' blk_prefix_expr  {
  SHOWPARSE("blk_prefix_expr -> - blk_prefix_expr");
  $$ = AST::make(at_apply, $1.loc, 
                 AST::make(at_ident, LToken($1.loc, "negate")), $2);
};

// NOT [7.15.3]
// in the s-expression version this is just a procedure.
blk_prefix_expr: tk_NOT blk_prefix_expr  {
  SHOWPARSE("blk_prefix_expr -> NOT blk_prefix_expr");
  $$ = AST::make(at_apply, $1.loc, AST::make(at_ident, $1), $2);
};
blk_prefix_expr: '!' blk_prefix_expr  {
  SHOWPARSE("blk_prefix_expr -> !blk_prefix_expr");
  $$ = AST::make(at_apply, $1.loc, AST::make(at_ident, $1), $2);
};

// AND [7.15.4]                 
blk_infix_expr: blk_infix_expr tk_AND blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr && blk_infix_expr");
  $$ = AST::make(at_and, $2.loc, $1, $3);
};
sxp_unqual_expr: '(' tk_AND sxp_nonempty_params ')'  {
  SHOWPARSE("sxp_unqual_expr -> ( AND sxp_nonempty_params )");
  $$ = $3;
  $$->loc = $2.loc;
  $$->astType = at_and;
};

// OR [7.15.5]
blk_infix_expr: blk_infix_expr tk_OR blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr || blk_infix_expr");
  $$ = AST::make(at_or, $2.loc, $1, $3);
};
sxp_unqual_expr: '(' tk_OR sxp_nonempty_params ')'  {
  SHOWPARSE("sxp_unqual_expr -> ( OR sxp_nonempty_params )");
  $$ = $3;
  $$->loc = $2.loc;
  $$->astType = at_or;
};

// OTHER BLOCK INFIX OPERATORS
blk_infix_expr: blk_infix_expr '|' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr | blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '^' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr ^ blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '&' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr & blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr tk_EQUALS blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr == blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr tk_NOTEQUALS blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr != blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '<' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr < blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '>' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr > blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr tk_LE blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr <= blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr tk_GE blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr >= blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr tk_LSHIFT blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr << blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr tk_RSHIFT blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr >> blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '+' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr + blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '-' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr - blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '*' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr * blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '/' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr / blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};
blk_infix_expr: blk_infix_expr '%' blk_infix_expr  {
  SHOWPARSE("blk_infix_expr -> blk_infix_expr % blk_infix_expr");
  $$ = AST::make(at_apply, $2.loc, AST::make(at_ident, $2), $1, $3);
};

// COND [7.15.6]          
//HERE - need C-like switch mechanism
sxp_unqual_expr: '(' tk_COND sxp_condcases sxp_otherwise ')'  {
  SHOWPARSE("sxp_unqual_expr -> (COND  ( sxp_condcases sxp_otherwise ) ) ");
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

sxp_condcase: '(' sxp_expr sxp_block ')'  {
  SHOWPARSE("sxp_condcase -> ( sxp_expr sxp_block )");
  $$ = AST::make(at_cond_leg, $1.loc, $2, $3);
};

// SET! [7.16]                
blk_stmt: blk_postfix_expr tk_ASSIGN blk_stmt {
  /* Strictly speaking, RHS could be a higher-level parse form,, but
   * none of those are l-values in any case. Need postfix_expr in
   * order to pick up things like 'a.b'. There is a location check
   * later in the compiler, but might as well reject what we can
   * early.
   */
  SHOWPARSE("blk_assignment -> blk_postfix_expr := blk_stmt");
  $$ = AST::make(at_setbang, $2.loc, $1, $3);
};
sxp_unqual_expr: '(' tk_SET sxp_expr sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> ( SET! sxp_expr sxp_expr )");
  $$ = AST::make(at_setbang, $2.loc, $3, $4);
};

// SWITCH
blk_stmt: tk_SWITCH blk_expr '{' blk_sw_legs blk_opt_otherwise '}' {
  SHOWPARSE("blk_stmt -> SWITCH blk_expr blk_sw_legs blk_opt_otherwise");
  $$ = AST::make(at_uswitch, $1.loc, 
                 AST::make(at_ident, LToken("__dummy")),
                 $2, $4, $5);
}

sxp_unqual_expr: '(' tk_SWITCH sxp_ident sxp_expr sxp_sw_legs sxp_opt_otherwise ')' {
  SHOWPARSE("sxp_unqual_expr -> ( SWITCH sxp_ident sxp_expr sxp_sw_legs sxp_opt_otherwise)");
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

blk_sw_legs: blk_sw_legs blk_sw_leg {
  SHOWPARSE("blk_sw_legs -> blk_sw_legs blk_sw_leg");
  $$ = $1;
  $1->addChild($2);
}
blk_sw_legs: blk_sw_leg {
  SHOWPARSE("blk_sw_legs -> blk_sw_leg");
  $$ = AST::make(at_usw_legs, $1->loc, $1);
}
blk_sw_leg: tk_CASE blk_ident tk_AS blk_switch_match tk_IN blk_stmt {
  SHOWPARSE("blk_sw_leg -> CASE blk_ident AS blk_type IN blk_stmt");
  $$ = AST::make(at_usw_leg, $1.loc, $2, $6, $4);
}

sxp_sw_leg: '(' sxp_switch_match sxp_block ')'  {
  SHOWPARSE("sxp_sw_leg -> ( sxp_switch_match sxp_block )");
  $$ = AST::make(at_usw_leg, $1.loc, $3, $2);
};

sxp_sw_leg: '(' '(' sxp_switch_matches ')' sxp_block ')'  {
  SHOWPARSE("sxp_sw_leg -> ( ( sxp_switch_matches ) sxp_block )");
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
   found the sxp_useident.ctr version, otherwise, this is ambiguous, and
   leave the burden on the resolver to find out */
blk_switch_match: blk_ident {
  SHOWPARSE("blk_switch_match -> blk_ident");
  $$ = $1;
};

blk_switch_match: blk_ident '.' blk_ident {
  SHOWPARSE("blk_switch_match -> blk_ident . blk_ident"); 
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

blk_switch_match: blk_ident '.' blk_ident '.' blk_ident {
  SHOWPARSE("blk_switch_match -> blk_ident '.' blk_ident '.' blk_ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $1->loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = AST::make(at_select, $1->loc, usesel, $5);
};

sxp_switch_match: sxp_ident {
  SHOWPARSE("sxp_switch_match -> sxp_ident");
  $$ = $1;
};

sxp_switch_match: sxp_ident '.' sxp_ident {
  SHOWPARSE("sxp_switch_match -> sxp_ident . sxp_ident"); 
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

sxp_switch_match: sxp_ident '.' sxp_ident '.' sxp_ident {
  SHOWPARSE("sxp_switch_match -> sxp_ident '.' sxp_ident '.' sxp_ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $1->loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = AST::make(at_select, $1->loc, usesel, $5);
};

// These precedences are re-used from the productions that favor
// application over bare expressions
blk_opt_otherwise: blk_otherwise {
  SHOWPARSE("blk_opt_otherwise -> blk_otherwise");
  $$ = $1;
};
blk_opt_otherwise: { //empty
  SHOWPARSE("blk_opt_otherwise -> ");
  $$ = AST::make(at_Null);
};
blk_otherwise: tk_OTHERWISE blk_stmt {
  SHOWPARSE("blk_otherwise -> OTHERWISE blk_stmt");
  $$ = AST::make(at_otherwise, $1.loc, $2);
};

sxp_opt_otherwise: sxp_otherwise {
  SHOWPARSE("sxp_opt_otherwise -> sxp_otherwise");
  $$ = $1;
};
sxp_opt_otherwise: { //empty
  SHOWPARSE("sxp_opt_otherwise -> Null");
  $$ = AST::make(at_Null);
};
sxp_otherwise: '(' tk_OTHERWISE sxp_block')' {
  SHOWPARSE("sxp_otherwise -> ( OTHERWISE sxp_block)");
  $$ = AST::make(at_otherwise, $2.loc, $3);
};

/* // TYPECASE  [11]           
sxp_unqual_expr: '(' tk_TYPECASE '(' typecase_legs ')' ')'  {
  SHOWPARSE("sxp_unqual_expr -> ( typecase ( typecase_legs ) )");
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
blk_stmt: tk_TRY blk_expr tk_HANDLE '{' blk_catch_legs blk_opt_otherwise '}' {
  SHOWPARSE("blk_stmt -> TRY blk_expr blk_catch_legs blk_opt_otherwise");
  $$ = AST::make(at_try, $1.loc, $2, 
                 AST::make(at_ident, LToken("__dummy")),
                 $5, $6);

  // In the S-expr syntax, there is a loop here that inserts the ident
  // into each leg, and then in the otherwise clause. In the block
  // syntax we have the ident explicitly in each leg, so we don't do
  // that.
}
blk_stmt: tk_TRY blk_expr blk_otherwise {
  SHOWPARSE("blk_stmt -> TRY blk_expr blk_catch_legs blk_otherwise");
  shared_ptr<AST> dummyID = AST::make(at_ident, LToken("__dummy"));
  $$ = AST::make(at_try, $1.loc, $2,
                 AST::make(at_ident, LToken("__dummy")),
                 AST::make(at_usw_legs, $3->loc), $3);

  // In the S-expr syntax, there is a loop here that inserts the ident
  // into each leg, and then in the otherwise clause. In the block
  // syntax we have the ident explicitly in each leg, so we don't do
  // that.
}


//HERE
//blk_opt_catch_legs: {
//  SHOWPARSE("blk_opt_catch_legs -> ");
//  $$ = AST::make(at_catch_legs);
//}
//blk_opt_catch_legs: blk_catch_legs {
//  SHOWPARSE("blk_opt_catch_legs -> blk_catch_legs");
//  $$ = $1;
//}

//blk_stmt: tk_TRY blk_letbinding tk_IN blk_stmt blk_catch_legs {
//  SHOWPARSE("blk_stmt -> LET blk_letbinding IN expr");
//
//  shared_ptr<AST> bindings = AST::make(at_letbindings, $2->loc, $2);
//  $$ = AST::make(at_kennedy_try, $1.loc, bindings, $4);
//}

blk_catch_leg: tk_CASE blk_ident tk_AS blk_switch_match tk_IN blk_stmt {
  SHOWPARSE("blk_catch_leg -> CATCH blk_ident AS blk_type IN blk_stmt");
  //  $$ = AST::make(at_catchleg, $1.loc, $2, $4, $6);
  $$ = AST::make(at_usw_leg, $1.loc, $2, $4, $6);
}

blk_catch_legs: blk_catch_legs blk_catch_leg {
  SHOWPARSE("blk_catch_legs -> blk_catch_legs blk_catch_leg");
  $$ = $1;
  $1->addChild($2);
}
blk_catch_legs: blk_catch_leg {
  SHOWPARSE("blk_catch_legs -> blk_catch_leg");
  $$ = AST::make(at_usw_legs, $1->loc, $1);
}

sxp_unqual_expr: '(' tk_TRY sxp_expr '(' tk_CATCH  sxp_ident sxp_sw_legs sxp_opt_otherwise ')' ')'  {
  SHOWPARSE("sxp_unqual_expr -> ( TRY sxp_expr ( CATCH sxp_ident sxp_sw_legs sxp_opt_otherwise) )");
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
//HERE
sxp_unqual_expr: '(' tk_TRY sxp_expr '(' tk_CATCH sxp_ident sxp_otherwise ')' ')'  {
  SHOWPARSE("sxp_unqual_expr -> ( TRY sxp_expr ( CATCH sxp_ident sxp_otherwise) )");
  $$ = AST::make(at_try, $2.loc, $3, $6,
                 AST::make(at_usw_legs, $7->loc), $7);

  assert ($7->astType == at_otherwise);

  shared_ptr<AST> ow = $7;
  ow->children.insert(ow->children.begin(),
                      $6->getDeepCopy());
};

// THROW  [7.19.2]              
blk_prefix_expr: tk_THROW blk_prefix_expr {
  SHOWPARSE("blk_prefix_expr -> THROW blk_prefix_expr");
  $$ = AST::make(at_throw, $1.loc, $2);
}

sxp_unqual_expr: '(' tk_THROW sxp_expr ')' {
  SHOWPARSE("sxp_unqual_expr -> ( THROW sxp_expr )");
  $$ = AST::make(at_throw, $2.loc, $3);
};

// let / letrec forms

//HERE
sxp_unqual_expr: sxp_let_eform {
  SHOWPARSE("sxp_unqual_expr -> sxp_let_eform");
  $$ = $1;
};

// LET [5.3.1]                 
blk_stmt: tk_LET blk_letbinding tk_IN blk_stmt {
  //HERE
  SHOWPARSE("blk_stmt -> LET blk_letbinding IN blk_stmt");

  shared_ptr<AST> bindings = AST::make(at_letbindings, $2->loc, $2);
  $$ = AST::make(at_let, $1.loc, bindings, $4);
  $$->addChild(AST::make(at_constraints));
}
blk_letbinding: blk_bindingpattern '=' blk_expr {
  SHOWPARSE("blk_letbinding -> blk_bindingpattern '=' blk_expr");
  $$ = AST::make(at_letbinding, $1->loc, $1, $3);
};
// Following not used in LET, but may end up used in LETREC:
blk_letbindings: blk_letbinding {
  SHOWPARSE("blk_letbindings -> blk_letbinding");
  $$ = AST::make(at_letbindings, $1->loc, $1);
};
blk_letbindings: blk_letbindings SC blk_letbinding {
  SHOWPARSE("blk_letbindings -> blk_letbindings SC blk_letbinding");
  $$ = $1;
  $$->addChild($3);
};

sxp_let_eform: '(' tk_LET '(' sxp_letbindings ')' sxp_block ')' {
  SHOWPARSE("sxp_unqual_expr -> (LET (sxp_letbindings) sxp_block)");
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
blk_stmt: tk_LETREC blk_letbindings tk_IN blk_stmt {
  SHOWPARSE("blk_stmt -> LET blk_letbinding IN blk_stmt");

  $$ = AST::make(at_letrec, $1.loc, $2, $4);
  $$->addChild(AST::make(at_constraints));
}
sxp_let_eform: '(' tk_LETREC '(' sxp_letbindings ')' sxp_block ')' {
  SHOWPARSE("sxp_unqual_expr -> (LETREC (letbindings) sxp_block)");
  $6->astType = at_begin;
  $6->printVariant = pf_IMPLIED;
  shared_ptr<AST> lbs = $4;
  for (size_t c=0; c < lbs->children.size(); c++)
    lbs->child(c)->flags |= LB_REC_BIND;
 
  $$ = AST::make(at_letrec, $2.loc, $4, $6);
  $$->addChild(AST::make(at_constraints));
};

//HERE
sxp_unqual_expr: '(' tk_DO '(' sxp_dobindings ')' sxp_dotest sxp_block ')' {
  SHOWPARSE("sxp_unqual_expr -> (DO (dobindings) sxp_dotest sxp_block)");

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

sxp_unqual_expr: '(' tk_CONTINUE ')' {
  SHOWPARSE("sxp_unqual_expr -> (CONTINUE)");
  $$ = AST::make(at_return_from, $2.loc,
                 AST::make(at_ident, LToken("__continue")),
                 AST::make(at_unit, $2.loc));
}

/* Literals and Variables */
// BOOLEAN LITERALS [2.4.1]
trn_literal: boolLit {
  SHOWPARSE("trn_literal -> boolLit");
  $$ = $1;
};
// (signed) INTEGER LITERALS [2.4.1]
trn_literal: intLit {
  SHOWPARSE("trn_literal -> intLit");
  $$ = $1;
};

// FLOATING POINT LITERALS [2.4.2]
trn_literal: floatLit {
  SHOWPARSE("trn_literal -> floatLit");
  $$ = $1;
};
// CHARACTER LITERALS [2.4.3]
trn_literal: charLit {
  SHOWPARSE("trn_literal -> CharLit");
  $$ = $1;
};
// STRING LITERALS [2.4.4]
trn_literal: strLit {
  SHOWPARSE("trn_literal -> strLit");
  $$ = $1;
};

// External identifiers are not subject to reserved word restrictions...
blk_exident: tk_BlkIdent {
  SHOWPARSE("blk_exident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

blk_exident: tk_ReservedWord {
  SHOWPARSE("blk_exident -> <Reserved " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

sxp_exident: tk_SxpIdent {
  SHOWPARSE("sxp_exident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

sxp_exident: tk_SxpReservedWord {
  SHOWPARSE("sxp_exident -> <Reserved " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

// IDENTIFIERS [2.2]
blk_ident: tk_BlkIdent {
  SHOWPARSE("blk_ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

blk_ident: '`' tk_MixIdent {
  SHOWPARSE("blk_ident -> ` <MixIdent " + $2.str + ">");
  $$ = AST::make(at_ident, $2);
};
blk_ident: '`' '+' {
  SHOWPARSE("blk_ident -> ` <MixIdent " + $2.str + ">");
  $$ = AST::make(at_ident, $2);
};
blk_ident: '`' '-' {
  SHOWPARSE("blk_ident -> ` <MixIdent " + $2.str + ">");
  $$ = AST::make(at_ident, $2);
};
blk_ident: '`' '*' {
  SHOWPARSE("blk_ident -> ` <MixIdent " + $2.str + ">");
  $$ = AST::make(at_ident, $2);
};
blk_ident: '`' '/' {
  SHOWPARSE("blk_ident -> <MixIdent " + $2.str + ">");
  $$ = AST::make(at_ident, $2);
};

blk_ident: tk_ReservedWord {
  SHOWPARSE("blk_ident -> <RESERVED=" + $1.str + ">");
  cerr << $1.loc.asString() << ": The token \"" << $1.str
       << "\" is reserved for future use.\n";
  lexer->num_errors++;
  $$ = AST::make(at_ident, $1);
};

sxp_ident: tk_SxpIdent {
  SHOWPARSE("sxp_ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

sxp_ident: tk_SxpReservedWord {
  SHOWPARSE("sxp_ident -> <RESERVED=" + $1.str + ">");
  cerr << $1.loc.asString() << ": The token \"" << $1.str
       << "\" is reserved for future use.\n";
  lexer->num_errors++;
  $$ = AST::make(at_ident, $1);
};

blk_useident: blk_ident {
  SHOWPARSE("blk_useident -> blk_ident");
  $$ = $1;
};

blk_useident: blk_ident '.' blk_ident {
  SHOWPARSE("blk_useident -> blk_ident . blk_ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $2.loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = usesel;
};

blk_useident: blk_ident '.' blk_ident '.' blk_ident {
  SHOWPARSE("blk_useident -> blk_ident . blk_ident . blk_ident");

  shared_ptr<AST> lhs = AST::make(at_usesel, $2.loc, $1, $3);
  lhs->s = $1->s + "." + $3->s;

  shared_ptr<AST> usesel = AST::make(at_usesel, $4.loc, lhs, $5); 
  usesel->s = lhs->s + "." + $5->s;
  $$ = usesel;
};

sxp_useident: sxp_ident {
  SHOWPARSE("sxp_useident -> sxp_ident");
  $$ = $1;
};

sxp_useident: sxp_ident '.' sxp_ident {
  SHOWPARSE("sxp_useident -> sxp_ident . sxp_ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $2.loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = usesel;
};

sxp_useident: sxp_ident '.' sxp_ident '.' sxp_ident {
  SHOWPARSE("sxp_useident -> sxp_ident . sxp_ident . sxp_ident");

  shared_ptr<AST> lhs = AST::make(at_usesel, $2.loc, $1, $3);
  lhs->s = $1->s + "." + $3->s;

  shared_ptr<AST> usesel = AST::make(at_usesel, $4.loc, lhs, $5); 
  usesel->s = lhs->s + "." + $5->s;
  $$ = usesel;
};

//sxp_defident: sxp_ident {
//  SHOWPARSE("sxp_defident -> sxp_ident ");
//  $1->flags |= (ID_IS_GLOBAL);
//  $$ = $1;
//};

blk_defident: blk_useident {
  SHOWPARSE("blk_defident -> blk_useident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = $1;
};

sxp_defident: sxp_useident {
  SHOWPARSE("sxp_defident -> sxp_useident");
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

charLit: tk_Char {
  SHOWPARSE("charLit -> <Char=" + $1.str +">");
  $$ = AST::makeCharLit($1);
};

intLit: tk_NegativeInt {        /* S-expression only */
  SHOWPARSE("intLit -> <Int=" + $1.str +">");
  $$ = AST::makeIntLit($1);
};

intLit: tk_Nat {
  SHOWPARSE("intLit -> <Nat=" + $1.str +">");
  $$ = AST::makeIntLit($1);
};

natLit: tk_Nat {
  SHOWPARSE("natLit -> <Nat=" + $1.str +">");
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

// TRANSITIONAL SUPPORT FOR MIXED-MODE PARSING
LP: '(' {
  SHOWPARSE("LP -> '('");
  lexer->lispParenDepth++;
  lexer->currentLang |= TransitionLexer::lf_sexpr;
  $$ = $1;
}

RP: ')' {
  SHOWPARSE("RP -> ')'");
  lexer->lispParenDepth--;
  assert(lexer->lispParenDepth >= 0);
  if (lexer->lispParenDepth == 0) {
    lexer->currentLang &= ~TransitionLexer::lf_sexpr;
  }
  $$ = $1;
} 

//SC: {
//  SHOWPARSE("SC -> ");
//}
SC: ';' {
  SHOWPARSE("SC -> ;");
}
%%

