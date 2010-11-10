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

// We disable yyerror entirely, because it emits whenever "error" is
// shifted, and we're going to take over error handling for ourselves
// (entirely). Unfortunately we cannot use YYRECOVERING() to make
// error printing decisions, because that's already set by the time
// the error action is being run (which is where we print our
// message). We therefore use a scheme by which yyerror signals to us
// that the next error message should be printed.
#undef yyerror
#define yyerror(lexer, s) lexer->showNextError = true

#include "TransitionLexer.hxx"

#define SHOWPARSE(s)                           \
  do {                                         \
    if (Options::showParse)                    \
      lexer->errStream << (s) << std::endl;    \
  } while (false);

#define SHOWPARSE1(s,x)                                    \
  do {                                                     \
    if (Options::showParse)                                \
      lexer->errStream << (s) << " " << (x) << std::endl;  \
  } while (false);

static void PrintSyntaxError(TransitionLexer *lexer, const char *s);
#define syntaxError(s) PrintSyntaxError(lexer, (s))

inline int
transition_lex(YYSTYPE *lvalp, TransitionLexer *lexer)
{
  extern int yydebug;

  // yydebug = 1;
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
%token-table
%parse-param {TransitionLexer *lexer}

 /* Place-holder "low priority" terminal for use in %prec */
%nonassoc <tok> prec_PreferShift

%nonassoc <tok> tk_ReservedWord        /* reserved words */

/* Categorical terminals: */
%nonassoc <tok> tk_BlkIdent
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

%nonassoc <tok> '('
%nonassoc <tok> ':'
%nonassoc <tok> ')'                /* procedure call, unit */
%right    <tok> ','                /* pair */
%nonassoc <tok> '{' '}' ';'
%nonassoc <tok> '[' ']'            /* array, vector */
%left     <tok> '.'
%nonassoc <tok> tk_ASSIGN

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

%token <tok> tk_MIXFIX          /* for testing */

%token <tok> tk_SIZEOF
%token <tok> tk_BITSIZEOF

%token <tok> tk_BITFIELD
%token <tok> tk_FILL
%token <tok> tk_RESERVED
%token <tok> tk_WHERE
%token <tok> tk_IS

%token <tok> tk_BITC
%token <tok> tk_VERSION

%token <tok> tk_PURE
%token <tok> tk_IMPURE
%token <tok> tk_CONST

/* The following are on separate lines so that tk_ELSE has slightly
   higher precedence than tk_THEN. This lets us use %prec to
   explicitly resolve the S/R ambiguity for dangling else. */
%token <tok> tk_IF
%nonassoc <tok> tk_THEN
%nonassoc <tok> tk_ELSE

// %token <tok> tk_THE

%token <tok> tk_WHEN tk_UNLESS
%token <tok> tk_COND
%token <tok> tk_SWITCH
%token <tok> tk_TYPECASE
%left <tok> tk_CASE
%nonassoc <tok> tk_OTHERWISE

// MIXFIX: No more infix operators now that we have mixfix. AND an OR
// are handled as special cases in the mixfix engine.

%left <tok> tk_AND tk_OR
%left <tok> tk_EQUALS
// %left <tok> tk_NOTEQUALS

%left <tok> '='

%token <tok> tk_LABEL
%token <tok> tk_RETURN tk_FROM tk_CONTINUE

// MIXFIX:These are regrettably going to get tricky to handle...
%token <tok> tk_PAIR
%token <tok> tk_VECTOR
%token <tok> tk_ARRAY
%token <tok> tk_MAKE_VECTOR

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
%token <tok> tk_DEFTHM
%token <tok> tk_DECLARE
// %token <tok> tk_PROCLAIM
%token <tok> tk_EXTERNAL
%token <tok> tk_TAG

%token <tok> tk_MUTABLE
%token <tok> tk_DEREF
%token <tok> tk_INNER_REF
%token <tok> tk_UNBOXED
%token <tok> tk_BOXED
%left <tok> tk_PTR
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
%token <tok> tk_LOOP
%token <tok> tk_DO
%token <tok> tk_UNTIL
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

//%token <tok> tk_SUPER
%token <tok> tk_SUSPEND
%type <tok>  blk_ifident

//%token <tok> tk_EXPORT
 
%type <ast> trn_module trn_implicit_module trn_module_seq
%type <ast> trn_interface
%type <ast> blk_defpattern
%type <ast> blk_mod_definition
%type <ast> trn_mod_definitions trn_mod_definition
%type <ast> trn_if_definitions trn_if_definition
%type <ast> blk_common_definition
%type <ast> blk_value_declaration
%type <ast> blk_val blk_optval
%type <ast> blk_openclosed
%type <ast> blk_ptype_name
%type <ast> blk_typeapp
%type <ast> blk_type_definition blk_type_decl
%type <ast> blk_externals
%type <ast> blk_importList blk_provideList
%type <ast> blk_alias
%type <ast> blk_type_cpair
%type <ast> blk_value_definition blk_fndef_tail
%type <ast> blk_tc_definition blk_ti_definition
%type <ast> blk_import_definition blk_provide_definition
%type <ast> blk_tc_decls // blk_tc_decl
%type <ast> blk_opt_declares blk_declares blk_declare blk_decl
%type <ast> blk_constructors blk_constructor
%type <ast> blk_repr_constructors blk_repr_constructor
%type <ast> blk_repr_reprs blk_repr_repr
%type <ast> blk_bindingpattern blk_lambdapatterns blk_lambdapattern
%type <ast> blk_actual_params blk_nonempty_params
%type <ast> blk_iblock
%type <ast> blk_expr_seq
// Primary exprs are the truly primitive things.
// Closed exprs are things like LET, DO, WHILE that are bracketed on
// all sides.
%type <ast> blk_expr_primary
%type <ast> blk_expr_apply
%type <ast> blk_expr

%type <ast> blk_expr_mixfix blk_mixfix_elem
%type <ast> blk_mixfix_arglist

%type <ast> blk_expr_if_then_else blk_expr_when blk_expr_unless
%type <ast> blk_expr_switch
%type <ast> blk_expr_try
%type <ast> blk_expr_let blk_expr_letrec
%type <ast> blk_expr_loop
%type <ast> blk_expr_return blk_expr_from_return
%type <ast> blk_expr_throw
%type <ast> blk_expr_lambda
%type <ast> blk_expr_continue

// %type <ast> sxp_method_decls sxp_method_decl
%type <ast> blk_method_decls blk_method_decl
// %type <ast> sxp_method_bindings sxp_method_binding
%type <ast> blk_method_bindings blk_method_binding
%type <ast> blk_constraints blk_constraint_seq blk_constraint

// in order of precedence, *lowest* first:
%type <ast> blk_type
%type <ast> blk_postfix_type
%type <ast> blk_prefix_type
%type <ast> primary_type
%type <ast> blk_primary_type

%type <ast> blk_type_args blk_bitfieldtype
%type <ast> blk_field_type blk_type_pl_byref blk_type_args_pl_byref
%type <ast> int_type uint_type any_int_type float_type bool_type
%type <ast> blk_tvlist
%type <ast> blk_fields blk_field
%type <ast> blk_fields_and_methods blk_methods_only blk_methdecl
%type <ast> trn_literal typevar //mod_ident
%type <ast> blk_expr_switch_matches blk_expr_switch_match
%type <ast> blk_exident
%type <ast> blk_fntype blk_method_type
%type <ast> trn_fneffect
%type <ast> blk_sw_legs blk_sw_leg
%type <ast> blk_otherwise blk_opt_otherwise
%type <ast> blk_letbindings blk_letbinding
%type <ast> blk_loopbindings blk_nonempty_loopbindings blk_expr_loopbinding
%type <ast> blk_type_val_definition
%type <ast> blk_ident blk_defident blk_useident
%type <ast> intLit natLit floatLit charLit strLit boolLit

%type <tok> ILCB  IRCB   // one inserted by layout
%type <tok> OptRCB
%type <tok> IsILCB  // Used in types

%%

// Parser built for version 10.0
// Section Numbers indicated within []

// COMPILATION UNITS [2.5]
// This definition of start must be changed as it ignores
// junk after the body.

start: ILCB blk_version ';' trn_uoc_body IRCB {
  SHOWPARSE("start -> ILCB blk_version SC trn_uoc_body IRCB");
  return 0;
};

start: error {
  syntaxError("input");
  return -1;
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
blk_version: tk_BITC {
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
};

// INTERFACES [8.1]
trn_interface: tk_INTERFACE blk_ifident {
    if ($2.str.find("bitc.") == 0)
      lexer->isRuntimeUoc = true;
  }
  IsILCB {
    lexer->layoutStack->precedingToken = tk_INTERFACE;
  } trn_if_definitions IRCB {
  SHOWPARSE("trn_interface -> INTERFACE blk_ifident IS { trn_if_definitions }");
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
  $$ = LToken(tk_BlkIdent, $2->loc, $2->endLoc(), $2->s);
};
blk_ifident: blk_ifident '.' {
    lexer->setIfIdentMode(true);
  } tk_BlkIdent {
  lexer->setIfIdentMode(false);
  $$ = LToken(tk_BlkIdent, $1.loc, $4.endLoc, $1.str + "." + $4.str);
};

// MODULES [2.5]
trn_module_seq: trn_module {
 SHOWPARSE("trn_module_seq -> trn_module");
}

trn_module_seq: trn_module_seq SC trn_module {
 SHOWPARSE("trn_module_seq -> trn_module_seq SC trn_module");
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

trn_module: tk_MODULE IsILCB {
    lexer->layoutStack->precedingToken = tk_INTERFACE;
  } trn_mod_definitions IRCB {
 SHOWPARSE("trn_module -> tk_MODULE IS { trn_mod_definitions }");
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

trn_module: tk_MODULE blk_ifident IsILCB {
    lexer->layoutStack->precedingToken = tk_INTERFACE;
  } trn_mod_definitions IRCB {
 SHOWPARSE("trn_module -> tk_MODULE blk_ifident trn_optdocstring IS { trn_mod_definitions }");
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

// INTERFACE TOP LEVEL DEFINITIONS
trn_if_definitions: trn_if_definition {
  SHOWPARSE("trn_if_definitions -> trn_if_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

trn_if_definitions: trn_if_definitions SC trn_if_definition {
  SHOWPARSE("trn_if_definitions -> trn_if_definitions SC trn_if_definition");
  $$ = $1;
  $$->addChild($3); 
};

trn_if_definition: blk_common_definition {
  SHOWPARSE("trn_if_definition -> blk_common_definition");
  $$ = $1;
};

// TOP LEVEL DEFINITIONS [2.5.1]
// Note that the block syntax has no direct analog to this, since we
// moved the constraints back inside the defining form.
trn_mod_definitions: trn_mod_definition {
  SHOWPARSE("trn_mod_definitions -> trn_mod_definition");
  $$ = AST::make(at_Null, $1->loc, $1);
};

trn_mod_definitions: trn_mod_definitions SC trn_mod_definition {
  SHOWPARSE("trn_mod_definitions -> trn_mod_definitions SC trn_mod_definition");
  $$ = $1;
  $$->addChild($3); 
};

trn_mod_definition: blk_mod_definition {
  SHOWPARSE("trn_mod_definition -> blk_mod_defintion");
  $$ = $1;
};

blk_mod_definition: blk_provide_definition {
  SHOWPARSE("blk_mod_definition -> blk_provide_definition");
  $$ = $1;
};

blk_mod_definition: blk_common_definition {
  SHOWPARSE("blk_mod_definition -> blk_common_definition");
  $$ = $1;
};

blk_common_definition: blk_import_definition {
  SHOWPARSE("blk_common_definition -> blk_import_definition");
  $$ = $1;
}

blk_common_definition: blk_type_val_definition {
  SHOWPARSE("blk_common_definition -> blk_type_val_definition");
  $$ = $1;
};

// No corresponding production in the block syntax, since constraints
// moved back inside their respective forms.
//
//blk_common_definition: blk_constrained_definition {
//  SHOWPARSE("blk_common_definition -> blk_constrained_definition");
//  $$ = $1;
//}

blk_type_val_definition: blk_type_decl {
  SHOWPARSE("blk_type_val_definition -> blk_type_decl");
  $$ = $1;
};

blk_type_val_definition: blk_type_definition {
  SHOWPARSE("blk_type_val_definition -> blk_type_definition");
  $$ = $1;
};

blk_type_val_definition: blk_value_definition {
  SHOWPARSE("blk_type_val_definition -> blk_value_definition");
  $$ = $1;
};

blk_type_val_definition: blk_value_declaration {
  SHOWPARSE("blk_type_val_definition -> blk_value_declaration");
  $$ = $1;
};

blk_type_val_definition: blk_tc_definition {
  SHOWPARSE("blk_type_val_definition -> blk_tc_definition");
  $$ = $1;
};

blk_type_val_definition: blk_ti_definition {
  SHOWPARSE("blk_type_val_definition -> blk_ti_definition");
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

blk_constraints: tk_FORALL blk_constraint_seq {
  SHOWPARSE("blk_constraints -> FORALL blk_constraint_seq");
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
  $1->astType = at_tcapp;
  $$ = $1;
}

blk_constraint: blk_useident %prec prec_PreferShift {
 SHOWPARSE("blk_constraint -> blk_useident");
 $$ = AST::make(at_tcapp, $1->loc, $1);
};

// Issue: parameterized type names in the block syntax have a
// shift/reduce ambiguity at:
//
//  blk_defident .
//  blk_defident . '(' blk_tvlist ')' blk_constraints

blk_ptype_name: blk_defident %prec prec_PreferShift {
  SHOWPARSE("blk_ptype_name -> blk_defident");
  shared_ptr<AST> tvlist = AST::make(at_tvlist, $1->loc);

  // In the block syntax, we have a reliable syntactic position for
  // constraints later, so we don't build one here as we needed to do
  // in the S-expression syntax.

  // shared_ptr<AST> constraints = AST::make(at_constraints, $1->loc);

  $$ = AST::make(at_Null, $1->loc, $1, tvlist);
};

blk_ptype_name: blk_defident '(' blk_tvlist ')' %prec '(' {
  SHOWPARSE("blk_ptype_name -> blk_defident ( blk_tvlist )");
  // In the block syntax, we have a reliable syntactic position for
  // constraints later, so we don't build one here as we needed to do
  // in the S-expression syntax.

  // shared_ptr<AST> constraints = AST::make(at_constraints, $1->loc);

  $$ = AST::make(at_Null, $1->loc, $1, $3);
};

// STRUCTURE TYPES [3.6.1]         
blk_type_definition: blk_val tk_STRUCT blk_ptype_name blk_constraints blk_opt_declares IsILCB blk_fields_and_methods IRCB {
  SHOWPARSE("blk_type_definition -> blk_val STRUCT blk_ptype_name blk_constraints "
            "blk_declares { blk_fields }");
  $$ = AST::make(at_defstruct, $2.loc, $3->child(0), $3->child(1), $1,
                 $5, $7, $4);
  $$->child(0)->defForm = $$;
};

// UNION TYPES [3.6.2]              
blk_type_definition: blk_val tk_UNION blk_ptype_name blk_constraints blk_opt_declares IsILCB blk_constructors IRCB {
  SHOWPARSE("blk_type_definition -> blk_val UNION blk_ptype_name blk_constraints "
            "trn_optdocstring blk_opt_declares { blk_constructors }");
  $$ = AST::make(at_defunion, $2.loc,
                 $3->child(0),   /* ident */
                 $3->child(1),   /* tvlist */
                 $1,             /* category */
                 $5,             /* declares */
                 $7,             /* constructors */
                 $4);            /* constraints */
  $$->child(0)->defForm = $$;
}

/* // REPR TYPES */
/* type_definition: '(' tk_REPR sxp_defident sxp_val trn_optdocstring sxp_declares reprbody  ')'  { */
/*   SHOWPARSE("sxp_type_definition -> ( DEFUNION sxp_ptype_name sxp_val " */
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
blk_type_definition: blk_val tk_REPR blk_defident blk_opt_declares IsILCB blk_repr_constructors IRCB {
  SHOWPARSE("blk_type_definition -> blk_val REPR blk_defident "
            "blk_opt_declares { blk_repr_constructors }");
  $$ = AST::make(at_defrepr, $2.loc, $3, 
                 AST::make(at_tvlist), /* empty tvlist */
                 $1, $4, $6,
                 AST::make(at_constraints)); /* empty constraints */
  $$->child(0)->defForm = $$;
}

blk_repr_constructors: blk_repr_constructor {
  SHOWPARSE("blk_repr_constructors -> blk_repr_constructor");
  $$ = AST::make(at_reprctrs, $1->loc, $1);
}

blk_repr_constructors: blk_repr_constructors SC blk_repr_constructor {
  SHOWPARSE("blk_repr_constructors -> blk_repr_constructors SC blk_repr_constructor");
  $$ = $1;
  $$->addChild($3);
}

blk_repr_constructor: blk_ident IsILCB blk_fields OptRCB tk_WHERE blk_repr_reprs {
  SHOWPARSE("blk_repr_constructor ->  blk_ident { sxp_fields } WHERE blk_repr_reprs");
  $1->flags |= (ID_IS_GLOBAL);
  shared_ptr<AST> ctr = AST::make(at_constructor, $1->loc, $1);
  ctr->addChildrenFrom($3);
  $$ = AST::make(at_reprctr, $1->loc, ctr);
  $$->addChildrenFrom($6);
}

blk_repr_reprs: blk_repr_repr {
  SHOWPARSE("blk_repr_reprs -> blk_repr_repr");
  $$ = AST::make(at_Null, $1->loc, $1);
};
blk_repr_reprs: blk_repr_reprs ',' blk_repr_repr {
  SHOWPARSE("blk_repr_reprs -> blk_repr_reprs ',' blk_repr_repr");
  $$ = $1;
  $$->addChild($3);
};

blk_repr_repr: blk_ident tk_EQUALS intLit {
  SHOWPARSE("blk_repr_repr ->  sxp_ident == intLit");

  $$ = AST::make(at_reprrepr, $2.loc, $1, $3);
};

// Type Declarations
// External declarations
blk_externals: {
  SHOWPARSE("blk_externals -> ");
  yyerrok;
  lexer->showNextError = false;

  $$ = AST::make(at_Null);
  $$->flags = NO_FLAGS;
};

blk_externals: tk_EXTERNAL {
  SHOWPARSE("blk_externals -> EXTERNAL");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;
};

blk_externals: tk_EXTERNAL blk_exident {
  SHOWPARSE("blk_externals -> EXTERNAL blk_exident");
  $$ = AST::make(at_Null, $1.loc);
  $$->flags = DEF_IS_EXTERNAL;
  $$->externalName = $2->s;
};

// OBJECT TYPES [3.6.1]         
blk_type_definition: tk_OBJECT blk_ptype_name blk_constraints blk_opt_declares IsILCB blk_methods_only IRCB  {
  SHOWPARSE("blk_type_definition -> OBJECT blk_ptype_name blk_constraints "
            "blk_opt_declares blk_methods_only )");

  // For the moment, all objects are value types:
  shared_ptr<AST> valCat = 
    AST::make(at_unboxedCat, LToken(tk_UNBOXED, "unboxed"));

  $$ = AST::make(at_defobject, $1.loc, $2->child(0), $2->child(1),
                 valCat,
                 $4, $6, $3);
  $$->child(0)->defForm = $$;
};

// STRUCTURE DECLARATIONS
blk_type_decl: blk_val tk_STRUCT blk_ptype_name blk_constraints blk_externals {
  SHOWPARSE("blk_type_decl -> blk_val STRUCT blk_ptype_name blk_constraints blk_externals '");
  $$ = AST::make(at_declstruct, $2.loc, 
                 $3->child(0),  /* ident */
                 $3->child(1),  /* tvlist */
                 $1,            /* category */
                 AST::make(at_declares), /* empty declares */
                 AST::make(at_fields), /* empty fields */
                 $4);                  /* constraints */
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// UNION DECLARATIONS
blk_type_decl: blk_val tk_UNION blk_ptype_name blk_constraints blk_externals {
  SHOWPARSE("blk_type_decl -> blk_val UNION blk_ptype_name blk_externals");
  $$ = AST::make(at_declunion, $2.loc, 
                 $3->child(0),  /* ident */
                 $3->child(1),  /* tvlist */
                 $1,            /* category */
                 AST::make(at_declares), /* empty declares */
                 AST::make(at_constructors), /* empty constructors */
                 $4);                        /* constraints */
  $$->child(0)->defForm = $$;
  $$->flags |= $5->flags;
  $$->getID()->flags |= $5->flags;
  $$->getID()->externalName = $5->externalName;
};

// REPR DECLARATIONS
blk_type_decl: blk_val tk_REPR blk_defident blk_externals {
  SHOWPARSE("blk_type_decl -> blk_val REPR blk_defident blk_externals )");
  $$ = AST::make(at_declrepr, $2.loc, $3, 
                 AST::make(at_tvlist), /* empty tvlist */
                 $1,                   /* category */
                 AST::make(at_declares), /* empty declares */
                 AST::make(at_reprctrs), /* empty constructors */
                 AST::make(at_constraints)); /* empty constraints */
  $$->child(0)->defForm = $$;
  $$->flags |= $4->flags;
  $$->getID()->flags |= $4->flags;
  $$->getID()->externalName = $4->externalName;
};

// CATEGORIES

blk_optval: {
  SHOWPARSE("blk_optval ->");
  $$ = AST::make(at_boxedCat);
  $$->printVariant = pf_IMPLIED;
}
blk_optval: blk_val {
  SHOWPARSE("blk_optval -> blk_val");
  $$ = $1;
}

blk_val: tk_BOXED {
  SHOWPARSE("blk_val -> BOXED");
  $$ = AST::make(at_boxedCat);
}

blk_val: tk_UNBOXED {
  SHOWPARSE("blk_val -> UNBOXED");
  $$ = AST::make(at_unboxedCat, $1);
};

blk_val: tk_OPAQUE {
  SHOWPARSE("blk_val -> tk_OPAQUE");
  $$ = AST::make(at_opaqueCat, $1);
};

blk_openclosed: {
  SHOWPARSE("blk_closed -> <empty>");
  $$ = AST::make(at_oc_open);
  $$->printVariant = pf_IMPLIED;
};

blk_openclosed: tk_CLOSED {
  SHOWPARSE("blk_closed -> CLOSED");
  $$ = AST::make(at_oc_closed, $1);
};

// EXCEPTION DEFINITION [3.10]
blk_type_definition: blk_optval tk_EXCEPTION blk_ident {
  SHOWPARSE("blk_type_definition -> optval EXCEPTION blk_ident");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $2.loc, 
                 $3,            /* ident */
                 AST::make(at_tvlist), /* empty tvlist */
                 $1,                   /* category */
                 AST::make(at_declares), /* empty declares */
                 AST::make(at_fields), /* empty fields */
                 AST::make(at_constraints)); /* empty constraints */
  $$->child(0)->defForm = $$;
};

blk_type_definition: blk_optval tk_EXCEPTION blk_ident IsILCB blk_fields IRCB {
  SHOWPARSE("blk_type_definition -> exception blk_ident IS { blk_fields }");
  $3->flags |= ID_IS_GLOBAL;
  $$ = AST::make(at_defexception, $2.loc, 
                 $3,
                 AST::make(at_tvlist), /* empty tvlist */
                 $1,                   /* category */
                 AST::make(at_declares), /* empty declares */
                 $5,                     /* fields */
                 AST::make(at_constraints)); /* empty constraints */
  $$->child(0)->defForm = $$;
};

// TYPE CLASSES [4]
// TYPE CLASS DEFINITION [4.1]

blk_tc_definition: blk_openclosed tk_TRAIT blk_ptype_name blk_constraints blk_tc_decls IsILCB blk_method_decls IRCB {
  SHOWPARSE("blk_tc_definition -> blk_openclosed TRAIT blk_ptype_name "
            "blk_constraints blk_tc_decls blk_method_decls)");
  $$ = AST::make(at_deftypeclass, $2.loc, $3->child(0),
                 $3->child(1), $5, $1, $7, $4);
  $$->child(0)->defForm = $$;
};

blk_tc_decls: {
  SHOWPARSE("blk_tc_decls -> <empty>");
  $$ = AST::make(at_tcdecls);
};

// This used to support TYFN. I've left in the empty node
// as a placeholder against the possible need for other
// declarations later.
//blk_tc_decls: blk_tc_decls blk_tc_decl {
//  SHOWPARSE("blk_tcdecls -> blk_tcdelcs sxp_tcdecl");
//  $$ = $1;
//  $$->addChild($2);
//};
//
//blk_tc_decl: tk_TYFN '(' blk_tvlist ')' tk_FNARROW typevar {
//  //                     ^^^^^^
//  // I really mean sxp_tvlist here, arbitrary types
//  // are not acceptable.
//  SHOWPARSE("blk_tc_decl -> TYFN ( blk_tvlist ) -> typevar");
//  $3->astType = at_fnargVec;
//  $$ = AST::make(at_tyfn, $2.loc, $3, $6);
//};

blk_method_decls: /* Nothing */ {
  SHOWPARSE("blk_method_decls -> ");
  LexLoc loc;
  $$ = AST::make(at_method_decls, loc);
};

blk_method_decls: blk_method_decls SC blk_method_decl {
  SHOWPARSE("blk_method_decls -> blk_method_decls SC blk_method_decl");
  $$ = $1;
  $$->addChild($3);
};

blk_method_decl: blk_ident ':' blk_fntype {
  SHOWPARSE("blk_method_decl -> blk_ident : blk_fntype");
  $1->flags |= ID_IS_GLOBAL;
  $1->identType = id_tcmethod;
  $$ = AST::make(at_method_decl, $1->loc, $1, $3);
};

// TYPE CLASS INSTANTIATIONS [4.2]
blk_ti_definition: tk_INSTANCE blk_constraint blk_constraints {
  SHOWPARSE("blk_ti_definition -> INSTANCE blk_constraint blk_constraints)");
  $$ = AST::make(at_definstance, $1.loc, $2,
                 AST::make(at_tcmethods, $1.loc), $3);
};
blk_ti_definition: tk_INSTANCE blk_constraint blk_constraints IsILCB blk_method_bindings IRCB {
  SHOWPARSE("blk_ti_definition -> INSTANCE blk_constraint blk_constraints IS { blk_method_bindings }");
  $$ = AST::make(at_definstance, $1.loc, $2, $5, $3);
};

blk_method_bindings: blk_method_binding {
  SHOWPARSE("blk_method_bindings -> blk_method_binding");
  $$ = AST::make(at_tcmethods, $1->loc, $1);
};
blk_method_bindings: blk_method_bindings SC blk_method_binding {
  SHOWPARSE("blk_method_bindings -> blk_method_bindings SC blk_method_binding");
  $$ = $1;
  $$->addChild($3);
};

blk_method_binding: blk_ident '=' blk_expr {
  SHOWPARSE("blk_method_binding -> blk_ident = blk_expr");

  $$ = AST::make(at_tcmethod_binding, $1->loc, $1, $3);
};

// DEFINE  [5.1]
//blk_value_definition: tk_DEF blk_defpattern blk_constraints '=' blk_expr {
//  SHOWPARSE("blk_value_definition -> DEF  blk_defpattern blk_constraints = blk_expr");
//  $$ = AST::make(at_define, $1.loc, $2, $5, $3);
//};

blk_value_definition: tk_DEF blk_defpattern blk_constraints '=' blk_expr {
  SHOWPARSE("blk_value_definition -> DEF blk_defpattern blk_constraints = blk_expr");
  $$ = AST::make(at_define, $1.loc, $2, $5, $3);
};
blk_value_definition: tk_DEF blk_defident '(' ')' blk_constraints blk_fndef_tail {
  SHOWPARSE("blk_value_definition -> DEF  blk_defident () blk_constraints blk_expr");
  // $5 = stripDocString($5);
  shared_ptr<AST> iRetBlock =
    AST::make(at_labeledBlock, $1.loc, 
              AST::make(at_ident, LToken(tk_BlkIdent, "__return")), $6);
  shared_ptr<AST> iLambda =
    AST::make(at_lambda, $1.loc, AST::make(at_argVec, $3.loc), iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $2->loc, $2);
  $$ = AST::make(at_recdef, $1.loc, iP, iLambda, $5);
}

blk_value_definition: tk_DEF blk_defident '(' blk_lambdapatterns ')' blk_constraints blk_fndef_tail {
  SHOWPARSE("blk_value_definition -> DEF  blk_defident () blk_constraints blk_expr");
  // $5 = stripDocString($5);
  shared_ptr<AST> iRetBlock =
    AST::make(at_labeledBlock, $1.loc, 
              AST::make(at_ident, LToken(tk_BlkIdent, "__return")), $7);
  shared_ptr<AST> iLambda = AST::make(at_lambda, $1.loc, $4, iRetBlock);
  iLambda->printVariant = pf_IMPLIED;
  shared_ptr<AST> iP = AST::make(at_identPattern, $2->loc, $2);
  $$ = AST::make(at_recdef, $1.loc, iP, iLambda, $6);
}

blk_fndef_tail: '=' blk_expr {
  SHOWPARSE("blk_fndef_tail -> '=' blk_expr");
  $$ = $2;
}
blk_fndef_tail: tk_IN blk_iblock {
  SHOWPARSE("blk_fndef_tail -> IN blk_iblock");
  $$ = $2;
}

// PROCLAIM DEFINITION -- VALUES [6.2]
blk_value_declaration: tk_DEF blk_defpattern blk_constraints blk_externals {
  SHOWPARSE("blk_value_declaration -> DEF blk_defpattern blk_constraints blk_externals");

  // I had to use blk_defpattern above to eliminate a reduce/reduce
  // conflict, but in a declaration we require the type to be present
  if ($2->children.size() == 1)
    lexer->ReportParseError($2->loc, "Declaration forms require a type");

  shared_ptr<AST> declIdent = $2->child(0);
  shared_ptr<AST> declType = $2->child(1);

  $$ = AST::make(at_proclaim, $1.loc, declIdent, declType, $3);
  $$->flags |= $4->flags;
  $$->getID()->flags |= $4->flags;
  $$->getID()->externalName = $4->externalName;
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
  if (!UocInfo::importInterface(lexer->errStream, $2.loc, $2.str)) {
    std::string err = "Unable to import " + $2.str;
    lexer->ReportParseError($1.loc, err);
  }

  $$ = AST::make(at_importAs, $1.loc, ifIdent, $4);
}

blk_import_definition: tk_IMPORT blk_ifident {
  SHOWPARSE("blk_import_definition -> IMPORT blk_ifident;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  if (!UocInfo::importInterface(lexer->errStream, $2.loc, $2.str)) {
    std::string err = "Unable to import " + $2.str;
    lexer->ReportParseError($1.loc, err);
  }
  $$ = AST::make(at_import, $1.loc, ifIdent);
};

blk_import_definition: tk_IMPORT blk_ifident blk_importList {
  SHOWPARSE("blk_import_definition -> IMPORT blk_ifident blk_importList;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  if (!UocInfo::importInterface(lexer->errStream, $2.loc, $2.str)) {
    std::string err = "Unable to import " + $2.str;
    lexer->ReportParseError($1.loc, err);
  }
  $$ = AST::make(at_import, $1.loc, ifIdent);
  $$->addChildrenFrom($3);
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

  $$ = AST::make(at_ifsel, $1->loc, $1, $3);
};

// PROVIDE DEFINITION [8.3]
blk_provide_definition: tk_PROVIDE blk_ifident blk_provideList {
  SHOWPARSE("blk_provide_definition -> PROVIDE blk_ifident blk_provideList;");
  shared_ptr<AST> ifIdent = AST::make(at_ifident, $2);
  UocInfo::importInterface(lexer->errStream, $2.loc, $2.str);
  $$ = AST::make(at_provide, $1.loc, ifIdent);
  $$->addChildrenFrom($3);
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

blk_opt_declares: {
  SHOWPARSE("blk_opt_declares -> <empty>");
  yyerrok;
  lexer->showNextError = false;
  $$ = AST::make(at_declares);
};
blk_opt_declares: blk_declares {
  SHOWPARSE("blk_opt_declares -> blk_declares");
  $$ = $1;
};

blk_declares: blk_declare {
  SHOWPARSE("blk_declares -> blk_declare");
  $$ = AST::make(at_declares, $1->loc, $1);
};

blk_declares: blk_declares SC blk_declare {
  SHOWPARSE("blk_declares -> blk_declares blk_declare");
  $$ = $1;
  $$->addChildrenFrom($3);
};

blk_declare: tk_DECLARE blk_decl {
  SHOWPARSE("blk_declare -> DECLARE blk_decl");
  $$ = $2;
};

// For tag type declaration.
blk_decl: blk_ident tk_AS blk_field_type {
  SHOWPARSE("blk_decl -> blk_ident AS blk_field_type");
  $$ = AST::make(at_declare, $1->loc, $1, $3);
};

//decl: '(' sxp_ident ')' {
//  SHOWPARSE("sxp_decl -> ( sxp_ident )");
//  $$ = AST::make(at_declare, $2->loc, $2);
//};
blk_decl: blk_ident {
  SHOWPARSE("blk_decl -> blk_ident");
  $$ = AST::make(at_declare, $1->loc, $1);
};


/* defunion Constructors */
blk_constructors: blk_constructor {
  SHOWPARSE("blk_constructors -> blk_constructor");
  $$ = AST::make(at_constructors, $1->loc, $1);
};
blk_constructors: blk_constructors SC blk_constructor {
  SHOWPARSE("blk_constructors -> blk_constructors SC blk_constructor");
  $$ = $1;
  $$->addChild($3);
};

blk_constructor: blk_ident { /* simple constructor */
  SHOWPARSE("blk_constructor -> blk_ident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
};
blk_constructor: blk_ident IsILCB blk_fields IRCB { /* compound constructor */
  SHOWPARSE("blk_constructor ->  blk_ident { blk_fields }");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = AST::make(at_constructor, $1->loc, $1);
  $$->addChildrenFrom($3);
};

/* defstruct / sxp_constructor / exception sxp_fields */
blk_fields: blk_field  {
  SHOWPARSE("blk_fields -> blk_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

blk_fields: blk_fields SC blk_field {
  SHOWPARSE("sxp_fields -> sxp_fields SC sxp_field ");
  $$ = $1;
  $$->addChild($3);
};

blk_field: blk_ident ':' blk_field_type {
  SHOWPARSE("blk_field -> blk_ident : blk_field_type");
  $$ = AST::make(at_field, $1->loc, $1, $3);
};
// FIX: Not clear why this is just bitfieldtype. Why can't it be any
// field type at all? I think it can.
blk_field: tk_FILL ':' blk_bitfieldtype {
  SHOWPARSE("blk_field -> FILL : blk_bitfieldtype");
  $$ = AST::make(at_fill, $1.loc, $3);
};

// Some low level data structures have reserved bit positions that are
// required to hold designated values.
blk_field: tk_RESERVED ':' blk_bitfieldtype '=' natLit  {
  SHOWPARSE("blk_field -> RESERVED ':' blk_bitfieldtype = natLit");
  $$ = AST::make(at_fill, $1.loc, $3, $5);
};

blk_methods_only: blk_methdecl  {
  SHOWPARSE("blk_methods_only -> blk_methdecl");
  $$ = AST::make(at_fields, $1->loc, $1);
};

blk_methods_only: blk_methods_only SC blk_methdecl  {
  SHOWPARSE("blk_methods_only -> blk_methods_only SC blk_methdecl");
  $$ = $1;
  $$->addChild($3);
};

blk_methdecl: blk_ident ':' blk_method_type {
  SHOWPARSE("blk_methdecl -> blk_ident : blk_method_type");
  $$ = AST::make(at_methdecl, $1->loc, $1, $3);
};

blk_fields_and_methods: blk_methdecl  {
  SHOWPARSE("blk_fields_and_methods -> blk_methdecl");
  $$ = AST::make(at_fields, $1->loc, $1);
};

blk_fields_and_methods: blk_field  {
  SHOWPARSE("blk_fields_and_methods -> blk_field");
  $$ = AST::make(at_fields, $1->loc, $1);
};

blk_fields_and_methods: blk_fields_and_methods SC blk_methdecl {
  SHOWPARSE("blk_fields_and_methods -> blk_fields_and_methods SC blk_methdecl ");
  $$ = $1;
  $$->addChild($3);
};

blk_fields_and_methods: blk_fields_and_methods SC blk_field {
  SHOWPARSE("blk_fields_and_methods -> blk_fields_and_methods SC blk_field ");
  $$ = $1;
  $$->addChild($3);
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

// TYPES [3]
//  Unfortunately, the grammar for block types requires precedence
//  rules, so it can't follow the order of appearance in the
//  specification. Specifically, we have to deal with precedence rules
//  and associativity for type application and vectors, so the overall
//  shape of the block type grammar is:
//
//  blk_primary_type -> {identifiers, keywords}
//
//  blk_prefix_type -> blk_primary_type
//  blk_prefix_type -> REF blk_prefix_type
//  blk_prefix_type -> MUTABLE blk_prefix_type
//
//  blk_postfix_type -> blk_prefix_type
//  blk_postfix_type -> blk_postfix_type '[' ']'
//  blk_postfix_type -> blk_postfix_type '[' intLit ']'
//  blk_postfix_type -> blk_postfix_type '(' type args ')'
//
//  blk_type -> blk_postfix_type
//
// Since the block type grammar and the s-expr type grammar share
// their primary type production, I moved sxp_useident inside
// primary_type. This is in contrast with the ANSI C grammar, where '.'
// (field select) is handled in the postfix_expression production, but
// the context for resolving '.' is never actually ambiguous, so it
// doesn't matter here.

blk_prefix_type: blk_primary_type {
  SHOWPARSE("blk_prefix_type -> blk_primary_type");
  $$ = $1;
}

blk_prefix_type: tk_MUTABLE blk_prefix_type {
  SHOWPARSE("blk_prefix_type -> MUTABLE blk_prefix_type");
  $$ = AST::make(at_mutableType, $1.loc, $2);
}
blk_prefix_type: tk_CONST blk_prefix_type {
  SHOWPARSE("blk_prefix_type -> CONST blk_prefix_type");
  $$ = AST::make(at_constType, $1.loc, $2);
}

blk_postfix_type: blk_prefix_type {
  SHOWPARSE("blk_postfix_type -> blk_prefix_type");
  $$ = $1;
}

blk_type: blk_postfix_type %prec prec_PreferShift {
  SHOWPARSE("blk_type -> blk_postfix_type");
  $$ = $1;
}

blk_primary_type: primary_type {
  SHOWPARSE("blk_primary_type -> primary_type");
  $$ = $1;
}
blk_primary_type: '(' blk_type ')' {
  SHOWPARSE("blk_primary_type -> '(' blk_type ')'");
  $$ = $2;
  // Preserve precedence when pretty printing:
  $2->printVariant |= pf_PARENWRAP;
}
blk_primary_type: '(' blk_type_cpair ')' {
  SHOWPARSE("blk_primary_type -> (blk_type_cpair)");
  $$ = $2;
};

// Temporary expedient: for each of the postfix types, introduce an
// application-style alternate syntax for use in pretty-printing:
blk_primary_type: tk_BOXED '(' blk_type ')' {
  SHOWPARSE("blk_primary_type -> tk_BOXED ( blk_type )");
  $$ = AST::make(at_boxedType, $1.loc, $3);

}
blk_primary_type: tk_UNBOXED '(' blk_type ')' {
  SHOWPARSE("blk_type -> UNBOXED ( blk_type )");
  $$ = AST::make(at_unboxedType, $1.loc, $3);
};
blk_primary_type: tk_ARRAY '(' blk_type ',' natLit ')' {
  SHOWPARSE("blk_primary_type -> tk_ARRAY ( blk_type, natLit )");
  $$ = AST::make(at_arrayType, $1.loc, $3, $5);

}
blk_primary_type: tk_VECTOR '(' blk_type ')' {
  SHOWPARSE("blk_primary_type -> tk_VECTOR ( blk_type )");
  $$ = AST::make(at_vectorType, $1.loc, $3);

}

blk_primary_type: blk_useident %prec prec_PreferShift {   /* previously defined type */
  SHOWPARSE("blk_primary_type -> blk_useident");
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
  SHOWPARSE("uint_type -> UINT64");
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

// REF TYPES [3.4.1]             
blk_postfix_type: blk_postfix_type tk_PTR %prec tk_PTR {
  SHOWPARSE("blk_postfix_type -> blk_postfix_type PTR");
  $$ = AST::make(at_boxedType, $2.loc, $1);
};

// FUNCTION TYPES [3.4.3]
blk_type: blk_fntype {
  SHOWPARSE("blk_type -> blk_fntype");
  $$ = $1;
}

trn_fneffect: {
  SHOWPARSE("trn_fneffect -> <empty>");
  $$ = AST::make(at_ident, LToken(tk_IMPURE, "impure"));
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

// Really blk_type, but broken out for the sake of trait declarations
blk_fntype: trn_fneffect tk_FN '(' ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_fntype -> trn_fneffect FN () -> blk_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $5.loc);
  $$ = AST::make(at_fn, $1->loc, fnargVec, $6);
};

blk_fntype: trn_fneffect tk_FN '(' blk_type_args_pl_byref ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_fntype -> trn_fneffect FN ( blk_type_args_pl_byref ) -> blk_type )");
  $$ = AST::make(at_fn, $1->loc, $4, $7);
};

// METHOD TYPES [3.9]
blk_method_type: trn_fneffect tk_METHOD '(' ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_method_type -> trn_fneffect METHOD () -> blk_type )");
  shared_ptr<AST> fnargVec = AST::make(at_fnargVec, $5.loc);
  $$ = AST::make(at_methType, $1->loc, fnargVec, $6);
};

blk_method_type: trn_fneffect tk_METHOD '(' blk_type_args_pl_byref ')' tk_FNARROW blk_type {
  SHOWPARSE("blk_method_type -> trn_fneffect METHOD ( blk_type_args_pl_byref ) -> blk_type )");
  $$ = AST::make(at_fn, $1->loc, $4, $7);
};

blk_type_cpair: blk_type ',' blk_type {
  SHOWPARSE("blk_type_cpair -> blk_type ',' blk_type");
  $$ = AST::make(at_typeapp, $2.loc,
                 AST::make(at_ident, LToken(tk_PAIR, $2.loc, $2.endLoc, "pair")),
                 $1, $3);
  $$->printVariant = pf_IMPLIED;
};
blk_type_cpair: blk_type ',' blk_type_cpair {
  SHOWPARSE("blk_type_cpair -> blk_type ',' blk_type_cpair");
  $$ = AST::make(at_typeapp, $2.loc,
                 AST::make(at_ident, LToken(tk_PAIR, $2.loc, $2.endLoc, "pair")),
                 $1, $3);
  $$->printVariant = pf_IMPLIED;
};

// ARRAY TYPE [3.5.1]               
blk_postfix_type: blk_postfix_type '[' natLit ']' %prec '[' {
  SHOWPARSE("blk_postfix_type -> blk_postfix_type '[' natLit ']'");
  $$ = AST::make(at_arrayType, $1->loc, $1, $3);
};
// VECTOR TYPE [3.5.2]             
blk_postfix_type: blk_postfix_type '[' ']' %prec '[' {
  SHOWPARSE("blk_postfix_type -> blk_postfix_type '[' ']'");
  $$ = AST::make(at_vectorType, $1->loc, $1);
};

// TYPE CONSTRUCTORS (typeapp)
//blk_type:  blk_useident '(' blk_type_args ')' {
//  SHOWPARSE("blk_typeapp -> blk_useident (blk_type_args)");
//  $$ = AST::make(at_typeapp, $1->loc, $1);
//  $$->addChildrenFrom($3);
//};
blk_primary_type: blk_typeapp {
  SHOWPARSE("blk_primary_type -> blk_typeapp");
  $$ = $1;
};

blk_typeapp: blk_useident '(' blk_type_args ')' %prec '(' {
  SHOWPARSE("blk_typeapp -> blk_useident (blk_type_args)");
  $$ = AST::make(at_typeapp, $1->loc, $1);
  $$->addChildrenFrom($3);
};

// BITFIELD TYPE
blk_bitfieldtype: any_int_type '(' natLit ')' {
  SHOWPARSE("blk_bitfieldtype -> any_int_type ( natLit )");
  $$ = AST::make(at_bitfieldType, $1->loc, $1, $3);
};
blk_bitfieldtype: bool_type '(' natLit ')' {
  SHOWPARSE("blk_bitfieldtype -> bool_type ( natLit )");
  $$ = AST::make(at_bitfieldType, $1->loc, $1, $3);
};

// Any non-by-ref type, including bitfield type
blk_field_type: blk_bitfieldtype {
  SHOWPARSE("blk_field_type -> blk_bitfieldtype");
  $$ = $1;
};

blk_field_type: blk_type {
  SHOWPARSE("blk_field_type -> blk_type");
  $$ = $1;
};

// by-ref sxp_type_args are not a part of general `type' rule.
// They are gramatiocally restricted to apprae only on
// formal function arguments and function types.
blk_type_pl_byref: blk_type {
  SHOWPARSE("blk_type_pl_byref -> blk_type");
  $$ = $1;
};

blk_type_pl_byref: tk_BY_REF blk_type {
  SHOWPARSE("blk_type_pl_byref -> BY-REF blk_type");
  $$ = AST::make(at_byRefType, $1.loc, $2);
};

blk_type_pl_byref: tk_ARRAY_REF blk_type {
  SHOWPARSE("blk_type_pl_byref -> ARRAY-REF blk_type");
  $$ = AST::make(at_arrayRefType, $1.loc, $2);
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

// BINDING PATTERNS [5.1]
blk_bindingpattern: blk_ident {
  SHOWPARSE("blk_bindingpattern -> blk_ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};

blk_bindingpattern: blk_ident ':' blk_type {
  SHOWPARSE("blk_bindingpattern -> blk_ident : blk_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
};

// There are no sxp_defpattern sequences, because there is no top-level
// pattern application
// DEFPATTERN
blk_defpattern: blk_defident %prec prec_PreferShift {
  SHOWPARSE("blk_defpattern -> blk_defident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};
blk_defpattern: blk_defident ':' blk_type %prec ':' {
  SHOWPARSE("blk_defpattern -> blk_defident : blk_qual_type");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
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

blk_lambdapattern: blk_ident {
  SHOWPARSE("blk_lambdapattern -> blk_ident");
  $$ = AST::make(at_identPattern, $1->loc, $1);
};

blk_lambdapattern: blk_ident ':' blk_type_pl_byref {
  SHOWPARSE("blk_lambdapattern -> blk_ident : blk_type_pl_byref");
  $$ = AST::make(at_identPattern, $1->loc, $1, $3);
  if ($3->astType == at_byRefType)
    $1->flags |= ARG_BYREF;
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

// MIXFIX: This needs a replacement...
blk_expr_primary: blk_iblock {
  SHOWPARSE("blk_expr_primary -> blk_iblock");
  $$ = $1;
}

// blk_expr_primary: '(' blk_expr ')' {
//  SHOWPARSE("blk_expr_primary -> ( blk_expr )");
//   $$ = $2;
//   // Be careful to preserve precedence when pretty printing:
//   $$->printVariant |= pf_PARENWRAP;
// }

blk_expr_apply: blk_expr_primary {
  SHOWPARSE("blk_expr_apply -> blk_expr_primary");
  $$ = $1;
}
blk_expr_apply: tk_SIZEOF '(' blk_type ')' {
  SHOWPARSE("blk_expr_apply -> SIZEOF (blk_type)");
  $$ = AST::make(at_sizeof, $1.loc, $3);
};
blk_expr_apply: tk_BITSIZEOF '(' blk_type ')' {
  SHOWPARSE("blk_expr_apply -> BITSIZEOF (blk_type)");
  $$ = AST::make(at_bitsizeof, $1.loc, $3);
};
// FIX: This should be a built-in procedure
blk_expr_apply: tk_DUP '(' blk_expr ')' {
  SHOWPARSE("blk_expr_apply -> DUP ( blk_expr )");
  $$ = AST::make(at_dup, $1.loc, $3);
};
blk_expr_apply: tk_DEREF '(' blk_expr ')' {
  SHOWPARSE("blk_expr_apply -> DEREF ( blk_expr )");
  $$ = AST::make(at_deref, $1.loc, $3);
};
blk_expr_apply: tk_MAKE_VECTOR '(' blk_expr ',' blk_expr ')' {
  SHOWPARSE("blk_expr_apply -> MAKE-VECTOR ( blk_expr , blk_expr )");
  $$ = AST::make(at_MakeVector, $1.loc, $3, $5);
};
blk_expr_apply: tk_VECTOR '(' blk_actual_params ')' {
  SHOWPARSE("blk_expr_apply -> VECTOR (blk_actual_params)");
  $$ = $3;
  $$->astType = at_vector;
  $$->loc = $1.loc;
};
blk_expr_apply: tk_ARRAY '(' blk_nonempty_params ')' {
  // Zero-length vector is illegal
  SHOWPARSE("blk_expr_apply -> (ARRAY blk_nonempty_params)");
  $$ = $3;
  $$->astType = at_array;
  $$->loc = $1.loc;
};

blk_expr_mixfix: blk_mixfix_elem %prec prec_PreferShift {
  SHOWPARSE("blk_expr_mixfix -> blk_mixfix_elem");
  $$ = $1;
}

blk_expr_mixfix: blk_expr_mixfix blk_mixfix_elem {
  SHOWPARSE("blk_expr_mixfix -> blk_expr_mixfix blk_mixfix_elem");
  $1->addChildrenFrom($2);
  $$ = $1;
}

// blk_mixfix_elem: blk_expr_mixfix '.' blk_ident {
//   SHOWPARSE("blk_expr_mixfix -> blk_expr_mixfix . blk_ident");
//   $$ = $1;
//   $$->addChild(at_ident, $1));
//   $$->addChild($3);
// }

blk_mixfix_elem: blk_expr_apply {
  SHOWPARSE("blk_mixfix_elem -> blk_expr_apply");
  $$ = AST::make(at_mixfix, $1->loc, $1);
}

// This entry is rather strange because it allows mixfix expressions
// like "1 , , 2". Unfortunately, trying to impose any sort of
// syntactic sanity here by doing something like mixfix_expr ','
// mixfix_expr causes gobs of shift/reduce errors. So I'm leaving it
// this way, and we'll field the unsyntactic cases in the mixfix
// parser by virtue of the fact that neither ",_" nor "_," will be
// defined. Then we'll check in the post-rewrite pass that these don't
// appear outside of their legal bracketing contexts, which are
// "(a,..,b)" and "[a,..,b]"

// blk_mixfix_elem: ',' {
//   SHOWPARSE("blk_mixfix_elem -> <Ident " + $1.str + ">");
//   $$ = AST::make(at_mixfix, $1.loc, AST::make(at_ident, $1));
// };

// Note that the "_._" rule is the only rule admitting '.', and will
// force an expr match on the left, which is what we want.
blk_mixfix_elem: '.' blk_ident {
  SHOWPARSE("blk_mixfix_elem -> '.' <Ident " + $1.str + "> blk_ident");
  $$ = AST::make(at_mixfix, $1.loc, AST::make(at_ident, $1), $2);
};

blk_mixfix_elem: ':' blk_type {
  SHOWPARSE("blk_mixfix_elem -> ':' blk_type");
  $$ = AST::make(at_mixfix, $1.loc, AST::make(at_ident, $1), $2);
}

blk_mixfix_arglist: blk_expr {
  SHOWPARSE("blk_mixfix_arglist -> blk_expr");
  $$ = AST::make(at_mixfix, $1->loc, $1);
}

blk_mixfix_arglist: blk_mixfix_arglist ',' blk_expr {
  SHOWPARSE("blk_mixfix_arglist -> blk_mixfix_arglist , blk_expr");
  $$ = $1;
  $$->addChild(AST::make(at_ident, $2));
  $$->addChild(AST::make(at_mixfix, $3->loc, $3));
}

blk_mixfix_elem: '(' blk_mixfix_arglist ')' {
  SHOWPARSE("blk_mixfix_elem -> ( blk_mixfix_arglist )");
  $$ = AST::make(at_mixfix, $1.loc, AST::make(at_ident, $1));
  $$->addChildrenFrom($2);
  $$->addChild(AST::make(at_ident, $3));
}

blk_mixfix_elem: '(' ')' {
  SHOWPARSE("blk_mixfix_elem -> ( )");
  $$ = AST::make(at_mixfix, $1.loc, AST::make(at_ident, $1));
  $$->addChild(AST::make(at_ident, $2));
}

blk_mixfix_elem: '[' blk_mixfix_arglist ']' {
  SHOWPARSE("blk_mixfix_elem -> [ blk_mixfix_arglist ]");
  $$ = AST::make(at_mixfix, $1.loc, AST::make(at_ident, $1));
  $$->addChildrenFrom($2);
  $$->addChild(AST::make(at_ident, $3));
}

// This is the only production defining blk_expr. It is causing
// a (correctly resolved) S/R ambiguity that we should probably try to
// clean up.
blk_expr: blk_expr_mixfix %prec prec_PreferShift {
  SHOWPARSE("blk_expr -> blk_expr_mixfix");
  $$ = $1;
}

blk_iblock: ILCB IRCB {
  SHOWPARSE("blk_block -> { }");
  // Empty blocks are okay:
  $$ = AST::make(at_begin, $1.loc);
}
blk_iblock: ILCB blk_expr_seq IRCB {
  SHOWPARSE("blk_block -> { blk_expr_seq }");

  // Remove redundant blocks eagerly:
  if ($2->children.size() == 1 && $2->child(0)->astType == at_begin)
    $2 = $2->child(0);
  $$ = $2;
}

blk_expr_seq: blk_expr {
  SHOWPARSE("blk_expr_seq -> blk_expr");
  $$ = AST::make(at_begin, $1->loc, $1);
};
blk_expr_seq: blk_value_definition {
  SHOWPARSE("blk_expr_seq -> blk_value_definition");
  $$ = AST::make(at_begin, $1->loc, $1);
};
/*blk_expr_seq: blk_expr_seq blk_expr {
  SHOWPARSE("blk_expr_seq -> blk_expr_seq blk_expr");
  $$ = $1;
  $$->addChild($2);
};
blk_expr_seq: blk_expr_seq SC blk_value_definition {
  SHOWPARSE("blk_expr_seq -> blk_expr_seq SC blk_value_definition");
  $$ = $1;
  $$->addChild($3);
  };*/
blk_expr_seq: blk_expr_seq ';' blk_expr {
  SHOWPARSE("blk_expr_seq -> blk_expr_seq ; blk_expr");
  $$ = $1;
  $$->addChild($3);
};
blk_expr_seq: blk_expr_seq ';' blk_value_definition {
  SHOWPARSE("blk_expr_seq -> blk_expr_seq ; blk_value_definition");
  $$ = $1;
  $$->addChild($3);
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

// LITERALS  [7.1]
blk_expr_primary: trn_literal {
  SHOWPARSE("blk_expr_primary -> Literal");
  $$ = $1;
};

// UNIT EXPRESSIONS   [7.4.1]
// blk_expr_primary: '(' ')' {
//   SHOWPARSE("blk_expr_primary -> ()");
//   $$ = AST::make(at_unit, $1.loc);
// };

// Expressions that involve locations:

// IDENTIFIERS [7.2]
/* This would actually have been
blk_expr_primary: blk_useident {
  SHOWPARSE("blk_expr_primary -> blk_useident");
  $$ = $1;
};
but for the ambiguity with record (field) selection.
So, the burden is now passed to further stages */

blk_expr_primary: blk_ident {
  SHOWPARSE("blk_expr_primary -> blk_ident");
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

// LABELS and LABELED EXIT [7.6]
// Note that we do not want a generalized blk_ident in the first
// position here, because that creates an ambiguity with primary
// expressions. Note that we only want a local identifier here in any
// case, and not an operator, so using tk_BlkIdent is fine here.

blk_expr_primary: tk_LABEL tk_BlkIdent tk_IN blk_iblock {
  SHOWPARSE("blk_expr_primary -> LABEL ident in blk_iblock");
  $$ = AST::make(at_labeledBlock, $1.loc,
                 AST::make(at_ident, $2),
                 $4);
}

blk_expr: blk_expr_continue {
  SHOWPARSE("blk_expr -> blk_continue");
  $$ = $1;
}
blk_expr_continue: tk_CONTINUE {
  SHOWPARSE("blk_expr_continue -> CONTINUE");
  $$ = AST::make(at_return_from, $1.loc,
                 AST::make(at_ident, LToken(tk_BlkIdent, "__continue")),
                 AST::make(at_unit, $1.loc));
}

blk_expr: blk_expr_from_return {
  SHOWPARSE("blk_expr -> blk_expr_from_return");
  $$ = $1;
}
blk_expr_from_return: tk_FROM blk_ident tk_RETURN blk_expr {
  SHOWPARSE("blk_expr_from_return -> FROM blk_ident RETURN blk_expr");
  $$ = AST::make(at_return_from, $1.loc, $2, $4);
}

// ARRAY-LENGTH [7.11.1]
// VECTOR-LENGTH [7.11.1]
// Replaced by v.length, a.length, ar.length

// convenience syntax: multiple arguments
blk_expr: blk_expr_lambda {
  SHOWPARSE("blk_expr -> blk_expr_lambda");
  $$ = $1;
}
blk_expr_lambda: tk_LAMBDA '(' ')' blk_expr {
  SHOWPARSE("blk_expr_lambda -> LAMBDA () blk_expr");
  shared_ptr<AST> argVec = AST::make(at_argVec, $2.loc);
  shared_ptr<AST> iRetBlock =
    AST::make(at_labeledBlock, $1.loc, 
              AST::make(at_ident, LToken(tk_BlkIdent, "__return")), $4);
  $$ = AST::make(at_lambda, $1.loc, argVec, iRetBlock);
};

blk_expr_lambda: tk_LAMBDA '(' blk_lambdapatterns ')' blk_expr {
  SHOWPARSE("blk_expr_lambda -> LAMBDA (blk_lambdapatterns) blk_expr");
  shared_ptr<AST> iRetBlock =
    AST::make(at_labeledBlock, $1.loc, 
              AST::make(at_ident, LToken(tk_BlkIdent, "__return")), $5);
  $$ = AST::make(at_lambda, $1.loc, $3, iRetBlock);
};

// RETURN [7.13]         
blk_expr: blk_expr_return {
  SHOWPARSE("blk_expr -> blk_expr_return");
  $$ = $1;
}
blk_expr_return: tk_RETURN blk_expr {
  SHOWPARSE("blk_expr_return -> RETURN blk_expr");
  $$ = AST::make(at_return_from, $1.loc,
                 AST::make(at_ident, LToken(tk_BlkIdent, "__return")), $2);
}

// IF [7.15.1]
blk_expr: blk_expr_if_then_else {
  SHOWPARSE("blk_expr -> blk_expr_if_then_else");
  $$ = $1;
}

blk_expr_if_then_else: tk_IF blk_expr tk_THEN ILCB blk_expr_seq OptRCB tk_ELSE blk_iblock {
  SHOWPARSE("blk_expr_if_then_else -> IF blk_expr THEN blk_ieblock ELSE blk_iblock");
  $$ = AST::make(at_if, $1.loc, $2, $5, $8);
};
blk_expr: tk_IF blk_expr error {
  SHOWPARSE("blk_expr -> IF blk_expr error");
  LToken lastTok = lexer->getLastToken();
  lexer->ReportParseError($1.loc, 
                          "Expected 'then' following 'if', got " + lastTok.str
                          + " at " + lastTok.loc.asString());
  $$ = AST::make(at_unit, $1.loc);
};
blk_expr: tk_IF blk_expr tk_THEN ILCB blk_expr_seq OptRCB error {
  SHOWPARSE("blk_expr -> IF blk_expr error");
  LToken lastTok = lexer->getLastToken();
  lexer->ReportParseError($1.loc, 
                          "if/then/else missing else clause at "
                          + lastTok.loc.asString() +
                          ", assuming unit. Consider when or unless?");

  // Yacc shifted a token in the position of error. Since we are
  // recovering locally, push that token back onto the input and make
  // the parser re-process it.
  assert(yychar != YYEMPTY);
  yylval.tok.flags |= TF_REPROCESS;
  lexer->pushTokenBack(yylval.tok, true);
  yyclearin;
     
  lexer->showNextError = false;

  $$ = AST::make(at_if, $1.loc, $2, $5,
                 AST::make(at_unit, $1.loc));
};

// WHEN [7.15.2]
blk_expr: blk_expr_when {
  SHOWPARSE("blk_expr -> blk_expr_when");
  $$ = $1;
}
blk_expr_when: tk_WHEN blk_expr tk_DO blk_expr {
  SHOWPARSE("blk_expr -> WHEN blk_expr DO blk_expr");
  $$ = AST::make(at_when, $1.loc, $2, $4);
};
blk_expr_when: tk_WHEN blk_expr tk_THEN blk_expr {
  // Transition:
  SHOWPARSE("blk_expr -> WHEN blk_expr THEN blk_expr");
  lexer->ReportParseError($3.loc, 
                          "when/then has been replaced by when/do.");
  $$ = AST::make(at_when, $1.loc, $2, $4);
};
blk_expr: blk_expr_unless {
  SHOWPARSE("blk_expr -> blk_expr_unless");
  $$ = $1;
}
blk_expr_unless: tk_UNLESS blk_expr tk_DO blk_expr {
  SHOWPARSE("blk_expr -> UNLESS blk_expr DO blk_expr");
  $$ = AST::make(at_unless, $1.loc, $2, $4);
};

// SET! [7.16]                
blk_expr: blk_expr_mixfix tk_ASSIGN blk_expr %prec tk_ASSIGN {
  /* Strictly speaking, RHS could be a higher-level parse form,, but
   * none of those are l-values in any case. Need postfix_expr in
   * order to pick up things like 'a.b'. There is a location check
   * later in the compiler, but might as well reject what we can
   * early.
   */
  SHOWPARSE("blk_assignment -> blk_expr_mixfix := blk_expr");
  $$ = AST::make(at_setbang, $2.loc, $1, $3);
};

// SWITCH
// blk_expr: tk_SWITCH blk_expr OptLCB blk_sw_legs blk_opt_otherwise OptRCB {
//   SHOWPARSE("blk_expr -> SWITCH blk_expr IN blk_sw_legs blk_opt_otherwise");
//   $$ = AST::make(at_uswitch, $1.loc, 
//                  AST::make(at_ident, LToken(tk_BlkIdent, "__dummy")),
//                  $2, $4, $5);
// }
// FIX: For the moment, cannot use blk_letbinding here, because that
// admits a type qualifier on the bound identifier. I don't want to
// deal with that complication in the type inference engine just at
// the moment.
blk_expr: blk_expr_switch {
  SHOWPARSE("blk_expr -> blk_expr_switch");
  $$ = $1;
}
blk_expr_switch: tk_SWITCH ILCB blk_ident '=' blk_expr OptRCB blk_sw_legs blk_opt_otherwise {
  SHOWPARSE("blk_expr_switch -> SWITCH { blk_letbinding } blk_sw_legs blk_opt_otherwise");

  $$ = AST::make(at_uswitch, $1.loc, $3, $5, $7, $8);

  // Inject the ident down into the case legs:
  for (size_t c =0; c < $7->children.size(); c++) {
    shared_ptr<AST> sw_leg = $7->child(c);
    sw_leg->children.insert(sw_leg->children.begin(),
                            $3->getDeepCopy());
  }
  if ($8->astType == at_otherwise) {
    shared_ptr<AST> ow = $8;
    ow->children.insert(ow->children.begin(),
                        $3->getDeepCopy());
  }
}

blk_sw_legs: blk_sw_legs blk_sw_leg {
  SHOWPARSE("blk_sw_legs -> blk_sw_legs blk_sw_leg");
  $$ = $1;
  $1->addChild($2);
}
blk_sw_legs: blk_sw_leg {
  SHOWPARSE("blk_sw_legs -> blk_sw_leg");
  $$ = AST::make(at_usw_legs, $1->loc, $1);
}
//blk_sw_leg: tk_CASE OptLCB blk_ident tk_AS blk_expr_switch_match OptRCB tk_IN blk_block {
//  SHOWPARSE("blk_sw_leg -> CASE { blk_ident AS blk_type } blk_block");
//  $$ = AST::make(at_usw_leg, $1.loc, $3, $8, $5);
//}
blk_sw_leg: tk_CASE blk_expr_switch_matches tk_IN blk_iblock {
  SHOWPARSE("blk_sw_leg -> CASE blk_expr_switch_matches IN blk_iblock");
  $$ = AST::make(at_usw_leg, $1.loc, $4);
  $$->addChildrenFrom($2);
}

blk_expr_switch_matches: blk_expr_switch_match {
  SHOWPARSE("blk_expr_switch_matches -> blk_expr_switch_match");
  $$ = AST::make(at_Null, $1->loc, $1);
}

blk_expr_switch_matches: blk_expr_switch_matches ',' blk_expr_switch_match {
  SHOWPARSE("blk_expr_switch_matches -> blk_expr_switch_matches ',' blk_expr_switch_match");
  $$ = $1;
  $$->addChild($3);
}

/* Constructors may be expressed as:
   i) cons
   ii) list.cons
   iii) bitc.list.cons

   If we find the double-dotted version, we are sure that we have
   found the sxp_useident.ctr version, otherwise, this is ambiguous, and
   leave the burden on the resolver to find out */
blk_expr_switch_match: blk_ident {
  SHOWPARSE("blk_expr_switch_match -> blk_ident");
  $$ = $1;
};

blk_expr_switch_match: blk_ident '.' blk_ident {
  SHOWPARSE("blk_expr_switch_match -> blk_ident . blk_ident"); 
  $$ = AST::make(at_select, $1->loc, $1, $3);
};

blk_expr_switch_match: blk_ident '.' blk_ident '.' blk_ident {
  SHOWPARSE("blk_expr_switch_match -> blk_ident '.' blk_ident '.' blk_ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $1->loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = AST::make(at_select, $1->loc, usesel, $5);
};

// This is just like dangling ELSE...
blk_opt_otherwise: blk_otherwise {
  SHOWPARSE("blk_opt_otherwise -> blk_otherwise");
  $$ = $1;
};
blk_opt_otherwise: %prec prec_PreferShift { //empty
  SHOWPARSE("blk_opt_otherwise -> ");
  $$ = AST::make(at_Null);
};
blk_otherwise: tk_OTHERWISE blk_iblock %prec tk_OTHERWISE {
  SHOWPARSE("blk_otherwise -> OTHERWISE blk_expr");
  $$ = AST::make(at_otherwise, $1.loc, $2);
};

// TRY/CATCH [7.19.1]
blk_expr: blk_expr_try {
  SHOWPARSE("blk_expr -> blk_expr_try");
  $$ = $1;
}
blk_expr_try: tk_TRY blk_iblock tk_CATCH blk_ident blk_sw_legs blk_opt_otherwise {
  SHOWPARSE("blk_expr -> TRY blk_iblock CATCH blk_ident blk_sw_legs blk_opt_otherwise");
  $$ = AST::make(at_try, $1.loc, $2, 
                 AST::make(at_ident, LToken(tk_BlkIdent, "__dummy")),
                 $5, $6);

  for (size_t c =0; c < $5->children.size(); c++) {
    shared_ptr<AST> sw_leg = $5->child(c);
    sw_leg->children.insert(sw_leg->children.begin(),
                            $4->getDeepCopy());
  }
  if ($6->astType == at_otherwise) {
    shared_ptr<AST> ow = $6;
    ow->children.insert(ow->children.begin(),
                        $4->getDeepCopy());
  }
}

blk_expr_try: tk_TRY blk_iblock tk_CATCH blk_ident blk_otherwise {
  SHOWPARSE("blk_expr_try -> TRY blk_expr tk_CATCH blk_ident blk_otherwise");
  shared_ptr<AST> dummyID = 
    AST::make(at_ident, LToken(tk_BlkIdent, "__dummy"));

  $$ = AST::make(at_try, $1.loc, $2, dummyID,
                 AST::make(at_usw_legs, $5->loc), /* empty */
                 $5);

  shared_ptr<AST> ow = $5;
  ow->children.insert(ow->children.begin(), $4->getDeepCopy());
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

//blk_expr: tk_TRY blk_letbinding tk_IN blk_expr blk_catch_legs {
//  SHOWPARSE("blk_expr -> LET blk_letbinding IN expr");
//
//  shared_ptr<AST> bindings = AST::make(at_letbindings, $2->loc, $2);
//  $$ = AST::make(at_kennedy_try, $1.loc, bindings, $4);
//}

// THROW  [7.19.2]              
blk_expr: blk_expr_throw {
  SHOWPARSE("blk_expr -> blk_expr_throw");
  $$ = $1;
}
blk_expr_throw: tk_THROW blk_expr {
  SHOWPARSE("blk_expr_throw -> THROW blk_expr");
  $$ = AST::make(at_throw, $1.loc, $2);
}

// LET [5.3.1]                 
blk_expr: blk_expr_let {
  SHOWPARSE("blk_expr -> blk_expr_let");
  $$ = $1;
}
blk_expr_let: tk_LET ILCB blk_letbindings OptRCB tk_IN blk_iblock {
  SHOWPARSE("blk_expr_let -> LET { blk_letbindings } IN blk_iblock");

  $$ = AST::make(at_let, $1.loc, $3, $6);
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

// LETREC [5.3.2]              
blk_expr: blk_expr_letrec {
  SHOWPARSE("blk_expr -> blk_expr_letrec");
  $$ = $1;
}
blk_expr_letrec: tk_LETREC ILCB blk_letbindings OptRCB tk_IN blk_iblock {
  SHOWPARSE("blk_expr_letrec -> LETREC { blk_letbindings } IN blk_iblock");

  shared_ptr<AST> lbs = $3;
  for (size_t c=0; c < lbs->children.size(); c++)
    lbs->child(c)->flags |= LB_REC_BIND;

  $$ = AST::make(at_letrec, $1.loc, lbs, $6, AST::make(at_constraints));
}

// FIX: Need to reconcile this with until e do { ... }, where we need
// the semicolon to be inserted at start of statement. The problem
// here is that UNTIL can either be a leading keyword or an internal
// keyword, but not both.
blk_expr: blk_expr_loop {
  SHOWPARSE("blk_expr -> blk_expr_loop");
  $$ = $1;
}
blk_expr_loop: tk_LOOP ILCB blk_loopbindings OptRCB tk_UNTIL blk_expr tk_IN blk_iblock {
  SHOWPARSE("blk_expr -> LOOP blk_loopbindings UNTIL blk_expr IN blk_iblock");

  // The body is executed for side effects. We need to know its result
  // type so that the CONTINUE block will be properly typed. Since we
  // are only running the body for side effects, force the result type
  // to be unit by appending a unit constructor after the body:

  shared_ptr<AST> iTest =
    AST::make(at_looptest, $6->loc, $6, AST::make(at_unit, $6->loc));

  shared_ptr<AST> iBody =
    AST::make(at_begin, $8->loc, $8, AST::make(at_unit, $8->loc));

  shared_ptr<AST> iContinueBlock =
    AST::make(at_labeledBlock, $1.loc,
              AST::make(at_ident, LToken(tk_BlkIdent, "__continue")),
              iBody);

  $$ = AST::make(at_loop, $1.loc, $3, iTest, iContinueBlock);
}

blk_loopbindings: {
  SHOWPARSE("blk_loopbindings -> <empty>");
  $$ = AST::make(at_loopbindings);
};
blk_loopbindings: blk_nonempty_loopbindings {
  SHOWPARSE("blk_loopbindings -> blk_nonempty_loopbindings");
  $$ = $1;
};
blk_nonempty_loopbindings: blk_expr_loopbinding {
  SHOWPARSE("blk_nonempty_loopbindings -> blk_expr_loopbinding");
  $$ = AST::make(at_loopbindings, $1->loc, $1);
};
blk_nonempty_loopbindings: blk_nonempty_loopbindings SC blk_expr_loopbinding {
  SHOWPARSE("blk_nonempty_loopbindings -> blk_nonempty_loopbindings SC blk_expr_loopbinding");
  $$ = $1;
  $$->addChild($3);
};
// Fix: the fact that the first is expr and the second is iblock is an
// artifact of layout due to an interaction with the THEN
// keyword. This is quite irritating, and we should probably change
// the keyword here.
blk_expr_loopbinding: blk_bindingpattern '=' blk_expr tk_THEN blk_iblock {
  SHOWPARSE("blk_expr_loopbinding -> blk_bindingpattern = blk_expr THEN blk_iblock");
  $$ = AST::make(at_loopbinding, $1->loc, $1, $3, $5);
};

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

// IDENTIFIERS [2.2]
blk_ident: tk_BlkIdent {
  SHOWPARSE("blk_ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};
blk_ident: tk_EQUALS {
  SHOWPARSE("blk_ident -> <Ident \"" + $1.str + "\">");
  $$ = AST::make(at_ident, $1);
};
// These two remain keywords until we are done with the S-expression syntax:
blk_ident: tk_AND {
  SHOWPARSE("blk_ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};
blk_ident: tk_OR {
  SHOWPARSE("blk_ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};
blk_ident: tk_ReservedWord {
  SHOWPARSE("blk_ident -> <RESERVED=" + $1.str + ">");
  cerr << $1.loc.asString() << ": The token \"" << $1.str
       << "\" is reserved for future use.\n";
  lexer->num_errors++;
  $$ = AST::make(at_ident, $1);
};

blk_useident: blk_ident %prec PreferShift {
  SHOWPARSE("blk_useident -> blk_ident");
  $$ = $1;
};

blk_useident: blk_useident '.' blk_ident %prec '.' {
  SHOWPARSE("blk_useident -> blk_useident . blk_ident");
  shared_ptr<AST> usesel = AST::make(at_usesel, $2.loc, $1, $3); 
  usesel->s = $1->s + "." + $3->s;
  $$ = usesel;
};

blk_defident: blk_useident {
  SHOWPARSE("blk_defident -> blk_useident");
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

// These are for transitional documentation purposes:
ILCB: '{' {
  $$ = $1;
}
IRCB: '}' {
  $$ = $1;
}

//IsErrLCB: '{' {
//  $$ = $1;
//}
//IsErrLCB: tk_IS ErrLCB {
//  $$ = $2;
//}

// OptLCB: {
// }
// OptLCB: ILCB {
// }
OptRCB: {
  assert(yychar != YYEMPTY);

  yylval.tok.flags |= TF_REPROCESS;

  LToken tok = LToken('}', yylval.tok.loc, yylval.tok.loc, "}");
  tok.prevTokType = yylval.tok.prevTokType;
  yylval.tok.prevTokType = '}';
  tok.flags |= (TF_INSERTED|TF_BY_PARSER);

  lexer->pushTokenBack(yylval.tok, true);
  lexer->pushTokenBack(tok, true);
  lexer->lex(&yylval);    /* re-read for side effects */

  // Force parser to re-fetch the lookahead token after we reduce,
  // since it may have changed because of the layout rules:
  if (yychar != YYEMPTY)
    yyclearin;

  $$ = tok;
}
OptRCB: IRCB {
  $$ = $1;
}

IsILCB: tk_IS ILCB {
  $$ = $2;
}

IsILCB: '{' {
  $$ = $1;
}

//ErrLCB: '{' {
//  $$ = $1;
//}
//ErrLCB: error {
//  yyerrok;
//
//  assert(yychar != YYEMPTY);
//
//  yylval.tok.flags |= TF_REPROCESS;
//
//  LToken tok = LToken('{', yylval.tok.loc, yylval.tok.loc, "{");
//  tok.prevTokType = yylval.tok.prevTokType;
//  yylval.tok.prevTokType = '{';
//  tok.flags |= (TF_INSERTED|TF_BY_PARSER);
//
//  lexer->pushTokenBack(yylval.tok, true);
//  lexer->pushTokenBack(tok, true);
//  lexer->lex(&yylval);    /* re-read for side effects */
//
//  // Force parser to re-fetch the lookahead token after we reduce,
//  // since it may have changed because of the layout rules:
//  if (yychar != YYEMPTY)
//    yyclearin;
//
//  lexer->showNextError = false;
//  $$ = tok;
//}

SC: {
  SHOWPARSE("SC -> ");
}
SC: ';' {
  SHOWPARSE("SC -> ;");
}
%%
static void
PrintSyntaxError(TransitionLexer *lexer, const char *s)
{
  std::stringstream ss;
  LToken tok = lexer->getLastToken();

  if (!lexer->showNextError)
    return;

  ss << "Syntax error in " << s << ": unexpected ";

  switch(tok.tokType) {
  case '{':
  case '}':
  case ';':
    {
      ss << '\'' << tok.str << '\'' << " (possibly inserted by layout)";
      break;
    }
  case ':':
    {
      ss << '\'' << tok.str << '\'';
      break;
    }
  case tk_TypeVar:
    ss << "type variable '" << tok.str;
    break;

  case EOF:
    ss << tok.str;
    break;

  default:
    ss << '"' << tok.str << '"';
    break;
  }

  lexer->ReportParseError(ss.str());
  lexer->showNextError = false;
}

/// @brief Given a token number as seen by lex, return the token name
/// as a string.
const char *TransitionTokenName(int lexTokenNumber)
{
  if (lexTokenNumber == -1)
    return "EOF";

  int bisonSymbolNumber = YYTRANSLATE(lexTokenNumber);
  const char * bisonTokenName = yytname[bisonSymbolNumber];

  return bisonTokenName;
}
