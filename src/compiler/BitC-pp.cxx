/**************************************************************************
 *
 * Copyright (C) 2008, Johns Hopkins University.
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
 *
 **************************************************************************/

/// @file
///
/// @brief Pretty printer for BitC ASTs, optionally with type information.

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <string>
#include <iostream>

#include "Options.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"
#include "TypeScheme.hxx"

using namespace boost;
using namespace sherpa;
using namespace std;

// At the moment, the "pretty" part of the pretty printing is broken.
static void
print_type(INOstream& out, shared_ptr <const AST> ast)
{
  shared_ptr<Type> ty = ast->symType;

  if (ty) {
    out << " /* : " << ty->asString() << " */";
  }
  else
    out << " /* : " << "?untyped?" << " */";

}

static void
sxp_BitcP(INOstream& out, shared_ptr <const AST> ast, bool);

static void
blk_BitcP(INOstream& out, shared_ptr <const AST> ast, bool);

/// @brief Wrapper to iterate across all children of @p ast starting
/// at @p from, optionally padding the first element with leading
/// white space if @p firstPad is true and optionally printing types
/// according to @p showTypes.
static void
doChildren(void (*BitcP)(INOstream& out, shared_ptr <const AST> ast, bool),
           INOstream& out, shared_ptr <const AST> ast, size_t from,
           const std::string& firstSep,
           const std::string& sep,
           bool showTypes)
{
  if (ast->children.size() > 1 || from) {
    for (size_t c = from; c < ast->children.size(); c++) {
      if (c == from)
        out << firstSep;
      else if (c > from)
        out << sep;
      BitcP(out, ast->child(c), showTypes);
    }
  }
  if (ast->children.size() == 1) {
    out << firstSep;
    BitcP(out, ast->child(0), showTypes);
  };
}

static void
sxp_doChildren(INOstream& out, shared_ptr <const AST> ast, size_t from,
               bool firstPad,
               bool showTypes)
{
  // Avoid redundant constructions:
  static std::string space = " ";
  static std::string empty = "";

  doChildren(sxp_BitcP,
             out, ast, from, 
             firstPad ? space : empty, 
             space,
             showTypes);
}

static void
sxp_maybe_open_quantifier(INOstream& out, shared_ptr<const AST> defn,
                      const bool showTypes)
{
  size_t nChildren = defn->children.size();
  shared_ptr<const AST> constraints = defn->child(nChildren-1);

  assert(constraints);
  if (constraints->children.size() == 0)
    return;

  out << "(forall ";
  sxp_BitcP(out, constraints, showTypes);
  out << " ";
}

static void
sxp_maybe_close_quantifier(INOstream& out, shared_ptr<const AST> defn,
                       const bool showTypes)
{
  size_t nChildren = defn->children.size();
  shared_ptr<const AST> constraints = defn->child(nChildren-1);

  assert(constraints);
  if (constraints->children.size() == 0)
    return;
  out << ")";
}

static void
sxp_show_qual_name(INOstream &out,  shared_ptr <const AST> ident,
                   shared_ptr <const AST> tvlist,
                   const bool showTypes)
{
  bool argsPresent = (tvlist->children.size() > 0);

  if (argsPresent) {
    out << "(" ;
    sxp_BitcP(out, ident, showTypes);
    out << " ";
    sxp_BitcP(out, tvlist, showTypes);
    out << ")" ;
  }
  else {
    sxp_BitcP(out, ident, showTypes);
  }
}

#if 0
static void
sxp_show_qual_name(INOstream &out,  shared_ptr <const AST> tapp,
               shared_ptr <const AST> constraints, bool showTypes)
{
  bool constraintsPresent = (constraints->children.size() > 0);
  if (constraintsPresent) {
    out << "(forall ";
    sxp_BitcP(out, constraints, showTypes);
    out << " ";
  }

  sxp_BitcP(out, tapp, showTypes);

  if (constraintsPresent)
    out << ")";
}
#endif

static void
blk_BitcP(INOstream& out, shared_ptr <const AST> ast, bool showTypes)
{
  size_t startIndent = out.indentToHere();

    // The following just recurse:
  switch(ast->astType) {
  case at_module:
    {
      out << ast->atKwd();
      out.more();

      size_t start = 0;

      if (ast->printVariant != 0) {
        // explicit module form. Put name on same line:
        out << " ";
        blk_BitcP(out, ast->child(0), showTypes);
        out << " is";
        start = 1;
      }
      else
        out << " is";

      /* Dont call doChildren; that will put spaces in front
         of the top level forms. Remember, bitc-version has
         already been emitted without a space */
      for (size_t i = start; i < ast->children.size(); i++) {
        out << std::endl;
        blk_BitcP(out, ast->child(i), showTypes);
      }

      out.less();
      break;
    }

  case at_interface:
    {
      out << ast->atKwd();
      out.more();

      // Put name on same line:
      out << " ";
      blk_BitcP(out, ast->child(0), showTypes);
      out << " is";

      /* Dont call doChildren; that will put spaces in front
         of the top level forms. Remember, bitc-version has
         already been emitted without a space */
      for (size_t i = 1; i < ast->children.size(); i++) {
        out << std::endl;
        blk_BitcP(out, ast->child(i), showTypes);
      }

      out.less();

      break;
    }

    // Things that (for the moment) we pass back to the s-expression printer:
  case at_import:
  case at_importAs:
  case at_provide:

  case at_proclaim:
  case at_define:
  case at_recdef:

  case at_deftypeclass:
  case at_definstance:
  case at_defexception:
  case at_defrepr:
  case at_defunion:
  case at_defstruct:

  case at_declunion:
  case at_declstruct:
  case at_declrepr:
      sxp_BitcP(out, ast, showTypes);
      break;

  default:
    {
      std::cerr << "blk_BitcP() needs support for AST type " 
                << ast->atKwd() << std::endl;
      assert(false);
    }
  }


  if (showTypes) print_type(out, ast);

  out.setIndent(startIndent);
}

/// @brief Core of the pretty printer.
static void
sxp_BitcP(INOstream& out, shared_ptr <const AST> ast, bool showTypes)
{
  size_t startIndent = out.indentToHere();

  switch(ast->astType) {
  case at_module:
  case at_interface:
    {
      blk_BitcP(out, ast, showTypes);
      break;
    }

  case at_docString:
    sxp_doChildren(out, ast, 0, true, showTypes);
    break;

  case at_Null:
    break;

  case at_mixExpr:
    out << "(" << ast->atKwd();
    out.more();
    sxp_doChildren(out, ast, 0, true, showTypes);
    out << ")";
    out.less();
    break;

  case at_refCat:
  case at_boxedCat:
    if (!(ast->printVariant & pf_IMPLIED))
      out << ":ref";
    break;
  case at_valCat:
  case at_unboxedCat:
    out << ":val";
    break;
  case at_opaqueCat:
    out << ":opaque";
    break;

  case at_closed:
    out << ":closed";
    break;

  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:

    out << ast->s;
    out << " /* " << ast->litValue << " */";
    if (showTypes) print_type(out, ast);

    break;
  case at_stringLiteral:
    out << "\"" << ast->s << "\"";

    if (showTypes) print_type(out, ast);

    break;

  case at_ident:
  case at_ifident:
    out << ast->s;
    if (Options::ppFQNS) {
      out << "{" << ast->fqn;
      if (ast->externalName.size()) {
        out << "," << ast->externalName;
      }
      out << "}";
    }

    if (showTypes) print_type(out, ast);

    break;

  case at_usesel:
    sxp_BitcP(out, ast->child(0), showTypes);
    out << ".";
    sxp_BitcP(out, ast->child(1), showTypes);
    break;

  case at_fqCtr:
  case at_sel_ctr:
  case at_select:
    {
      sxp_BitcP(out, ast->child(0), showTypes);
      out << ".";
      sxp_BitcP(out, ast->child(1), showTypes);
    }
    break;
    ///////////////////////////////////////////////////////////
    //
    // at_define needs to be handled specially, because if
    // it wraps an at_ilambda we need to re-expand it into
    // the convenience syntax.
    //
    ///////////////////////////////////////////////////////////
  case at_recdef:
    {
      sxp_maybe_open_quantifier(out, ast, showTypes);

      out << "(" << ast->atKwd();

      size_t oldIndent = out.indentToHere();

      out << " (";

      // Procedure name:
      sxp_BitcP(out, ast->child(0), showTypes);

      // Procedure arguments:
      shared_ptr<AST> iLambda = ast->child(1);
      sxp_doChildren(out, iLambda->child(0), 0, true, showTypes);

      out << ")";

      out << std::endl;
      out.setIndent(oldIndent);

      out.more();
      sxp_doChildren(out, iLambda, 1, true, showTypes);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, showTypes);

      break;
    }

  case at_define:
    {
      sxp_maybe_open_quantifier(out, ast, showTypes);

      if (ast->child(1)->astType == at_lambda) {
        out << "(" << ast->atKwd() << " ";

        // Procedure name:
        sxp_BitcP(out, ast->child(0), showTypes);

        out << endl;
        out.more();

        sxp_doChildren(out, ast, 1, false, showTypes);
        out << ")";
      }
      else {
        // Handle as normal s-expr:
        out << "(" << ast->atKwd();
        out.more();
        sxp_doChildren(out, ast, 0, true, showTypes);
        out << ")";
        out.less();
      }

      sxp_maybe_close_quantifier(out, ast, showTypes);
      break;
    }

  case at_try:
    out << "(" << ast->atKwd() << " ";
    sxp_BitcP(out, ast->child(0), showTypes);
    out << endl;
    out.more();
    out << "(catch ";
    sxp_BitcP(out, ast->child(1), showTypes);
    out << " ";
    sxp_BitcP(out, ast->child(2), showTypes);
    out << " ";
    sxp_BitcP(out, ast->child(3), showTypes);
    out << ")";
    out.less();
    out << ")";
    break;

  case at_primaryType:
    if (ast->s == "unit")
      out << "()";
    else
      out << ast->s;
    break;

  case at_deref:
    if (ast->printVariant && pf_IMPLIED) {
      sxp_doChildren(out, ast, 0, true, showTypes);
      out << "^";
    }
    else {
      out << "(deref ";
      sxp_doChildren(out, ast, 0, false, showTypes);
      out << ")";
    }
    break;

#if 0
  case at_lambda:
    {
#if 0
      if (ast->printVariant && pf_IMPLIED) {
      // While this test is true while printing whole top-level forms,
      // it is not true in the case of  individual expressions. Hence
      // I have disabled it. If the final version of the compiler has
      // no expression printing, we may re-enable it.
        std::cerr << "The pretty printer should never be printing an "
                  << "at_ilambda pattern!\n";
        exit(1);
      }
      else
#endif
        {
          out << "(" << ast->atKwd();
          sxp_doChildren(out, ast, 0, true, showTypes);
          out << ")";
        }
      break;
    }
#endif

    ///////////////////////////////////////////////////////////
    //
    // The following are all emitted as a parenthesized
    // list of children->
    //
    ///////////////////////////////////////////////////////////

  case at_letGather:
  case at_apply:
  case at_struct_apply:
  case at_object_apply:
  case at_ucon_apply:
    {
      out << "(";
      sxp_BitcP(out, ast->child(0), showTypes);
      out << " ";
      (void) out.indentToHere();

      for (size_t c = 1; c < ast->children.size(); c++) {
        if (c > 1) {
          if (ast->children.size() < 4)
            out << " ";
          else
            out << std::endl;
        }
        sxp_BitcP(out, ast->child(c), showTypes);
      }

      out << ")";
      break;
    }

  case at_when:
    {
      out << "(" << ast->atKwd() << " ";

      (void) out.indentToHere();

      sxp_BitcP(out, ast->child(0), showTypes);
      out << endl;
      sxp_doChildren(out, ast->child(1), 0, false, showTypes);
      out << ")";
      break;
    }

  case at_block:
  case at_return_from:
    {
      // Some blocks are implicitly introduced in the parser. These
      // require special handling
      string ident = ast->child(0)->s;
      if (ident == "__return") {
        if (ast->astType == at_block) {
          // This is the (block __return <body>) that implicitly wraps
          // every user-introduced lambda body. Simply pretty print the
          // <body>.
          sxp_BitcP(out, ast->child(1), showTypes);
          break;
        }
        else {
          // (RETURN expr) is encoded as (RETURN-FROM __return expr).
          // Print it in the form that the user gave:
          out << "(return ";
          sxp_BitcP(out, ast->child(1), showTypes);
          out << ")";
        }

        break;
      }
      else if (ident == "__continue") {
        if (ast->astType == at_block) {
          // This is the (block __return <body>) that implicitly wraps
          // every loop body. Simply pretty print the <body>.
          sxp_BitcP(out, ast->child(1), showTypes);
          break;
        }
        else {
          // (CONTINUE) is encoded as (RETURN-FROM __continue ()).
          // Print it in the form that the user gave:
          out << "(continue)";
        }

        break;
      }

      /* else fall through */
    }


  case at_begin:
  case at_if:
  case at_and:
  case at_or:
  case at_reprrepr:
    {
      out << "(" << ast->atKwd() << " ";

      (void) out.indentToHere();

      for (size_t c = 0; c < ast->children.size(); c++) {
        if (c > 0)
          out << std::endl;
        sxp_BitcP(out, ast->child(c), showTypes);
      }

      out << ")";
      break;
    }

  case at_uswitch:
  case at_vector:
  case at_vectorType:
  case at_makevectorL:
  case at_array:
  case at_arrayType:
  case at_byRefType:
  case at_arrayRefType:
  case at_refType:
  case at_valType:
  case at_letStar:
  case at_declare:
  case at_cond:
  case at_throw:
  case at_setbang:
  case at_mutableType:
  case at_constType:
  case at_condelse:
#ifdef HAVE_INDEXABLE_LENGTH_OPS
  case at_array_length:
  case at_array_ref_length:
  case at_vector_length:
#endif
  case at_localFrame:
  case at_frameBindings:
  case at_loop:
  case at_dup:
  case at_inner_ref:
  case at_suspend:
  case at_fill:
  case at_sizeof:
  case at_bitsizeof:
  case at_mkArrayByref:
    //case at_reprbody:
    {
      out << "(" << ast->atKwd();
      sxp_doChildren(out, ast, 0, true, showTypes);
      out << ")";
      break;
    }

  case at_nth:
  case at_array_nth:
  case at_array_ref_nth:
  case at_vector_nth:
    {
      sxp_BitcP(out, ast->child(0), showTypes);
      out << "[";
      sxp_BitcP(out, ast->child(1), showTypes);
      out << "]";
      break;
    }

  case at_methType:
  case at_fn:
    {
      // Reworked by shap on 10/9/2008 to use arrow syntax
      out << "(" << ast->atKwd();
      sxp_doChildren(out, ast->child(0), 0, true, showTypes);
      out << " ->";
      sxp_doChildren(out, ast, 1, true, showTypes);
      out << ")";
      break;
    }

  case at_constraints:
    {
      if (ast->children.size() > 0) {
        out << "(";
        sxp_doChildren(out, ast, 0, false, showTypes);
        out << ")";
      }
      break;
    }

  case at_letrec:
  case at_let:
    {
      shared_ptr<AST> constraints = ast->child(2);
      if (constraints->children.size()) {
        out << "(constrain ";
        out.indentToHere();
        for (size_t c = 0; c < constraints->children.size(); c++) {
          if (c > 0) {
            if (constraints->children.size() < 4)
              out << " ";
            else
              out << std::endl;
          }
          sxp_BitcP(out, ast->child(c), showTypes);
        }
        out << endl;
        out.setIndent(startIndent);
        out.more();
      }

      out << "(" << ast->atKwd() << " ";

      sxp_BitcP(out, ast->child(0), showTypes);
      out << endl;
      out.more();
      sxp_BitcP(out, ast->child(1), showTypes);

      out << ")";

      if (constraints->children.size())
        out << ")";

      break;
    }

  case at_exceptionType:
  case at_dummyType:
    {
      out << ast->atKwd();
      break;
    }

  case at_unit:
    out << "()";
    break;

  case at_tqexpr:
    // Argument order was swapped.
    out << "(" << ast->atKwd() << " ";
    sxp_BitcP(out, ast->child(1), showTypes);
    out << " ";
    sxp_BitcP(out, ast->child(0), showTypes);
    out << ")";

    if (showTypes) print_type(out, ast);

    break;

  case at_fnargVec:
  case at_argVec:
    out << "(";
    sxp_doChildren(out, ast, 0, false, showTypes);
    out << ")";

    if (showTypes) print_type(out, ast);

    break;

  case at_allocREF:
  case at_copyREF:
  case at_mkClosure:
  case at_setClosure:
    {
      if (ast->printVariant && pf_IMPLIED) {
        out << "(";
        sxp_BitcP(out, ast->child(0), showTypes);
        out << ", ";
        sxp_BitcP(out, ast->child(1), showTypes);
        out << ")";

        if (showTypes) print_type(out, ast);
      }
      else {
        out << "(" << ast->atKwd();
        sxp_doChildren(out, ast, 0, true, showTypes);
        out << ")";
      }
      break;
    }

  case at_importAs:
    {
      shared_ptr<AST> ifAst = ast->child(0);
      shared_ptr<AST> idAst = ast->child(1);

      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, ifAst, showTypes);
      out << " as ";
      sxp_BitcP(out, idAst, showTypes);
      out << ")";
      break;
    }

  case at_provide:
  case at_defexception:
    out << "(" << ast->atKwd();
    sxp_doChildren(out, ast, 0, true, showTypes);
    out << ")";
    break;

  case at_import:
    out << "(" << ast->atKwd();
    sxp_doChildren(out, ast, 0, true, showTypes);
    out << ")";
    break;

  case at_ifsel:
    if (ast->child(0)->s == ast->child(1)->s)
      out << ast->child(0)->s;
    else
      out << "(" << ast->child(1)->s
          << " as "
          << ast->child(0)->s
          << ")";
    break;

  case at_proclaim:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> proc_type = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, ident, showTypes);
      out << " : ";
      sxp_BitcP(out, proc_type, showTypes);
      if (ident->flags & DEF_IS_EXTERNAL) {
        out << " external";
        if (ident->externalName.size())
          out << " " << ident->externalName;        
      }

      sxp_BitcP(out, ast->child(2), showTypes);
      out << ")";
    }
    break;

  case at_deftypeclass:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> tcdecls = ast->child(2);
      shared_ptr<AST> openclosed = ast->child(3);
      shared_ptr<AST> methods = ast->child(4);
      shared_ptr<AST> constraints = ast->child(5);

      sxp_maybe_open_quantifier(out, ast, showTypes);

      out << "(" << ast->atKwd() << " ";
      sxp_show_qual_name(out, ident, tvlist, showTypes);
      if(openclosed->astType != at_Null) {
        out << " ";
        sxp_BitcP(out, openclosed, showTypes);
      }
      out << " ";
      sxp_BitcP(out, tcdecls, showTypes);
      out << " ";
      sxp_BitcP(out, methods, showTypes);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, showTypes);

      break;
    }

  case at_definstance:
    {
      shared_ptr<AST> tapp = ast->child(0);
      shared_ptr<AST> methods = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      sxp_maybe_open_quantifier(out, ast, showTypes);

      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, tapp, showTypes);
      sxp_BitcP(out, methods, showTypes);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, showTypes);

      break;
    }

  case at_tcmethods:
    {
      sxp_doChildren(out, ast, 0, false, showTypes);
      break;
    }

  case at_tcmethod_binding:
    {
      out << "(= " << ast->child(0)->s
          << " " << ast->child(1)->s
          << ")";
      break;
    }
 
  case at_tyfn:
    out << "(" << ast->atKwd();
    out << "(";
    sxp_BitcP(out, ast->child(0), showTypes);
    out << ")";
    sxp_BitcP(out, ast->child(1), showTypes);
    out << ")";
    break;

  case at_method_decl:
    sxp_BitcP(out, ast->child(0), showTypes);
    out << " : ";
    sxp_BitcP(out, ast->child(1), showTypes);
    break;

  case at_qualType:
    {
      out << "(forall ";
      sxp_BitcP(out, ast->child(0), showTypes);
      sxp_BitcP(out, ast->child(1), showTypes);
      out << ")";
      break;
    }

  case at_defrepr:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> category = ast->child(1);
      shared_ptr<AST> declares = ast->child(2);
      shared_ptr<AST> body = ast->child(3);

      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, ident, showTypes);
      sxp_BitcP(out, category, showTypes);
      out << " ";
      sxp_BitcP(out, declares, showTypes);
      out << "(";
      sxp_doChildren(out, body, 0, false, showTypes);
      out << "))";
      break;
    }


    //case at_reprcase:
    //     {
    //       out << "(case ";
    //       sxp_doChildren(out, ast, 0, false, showTypes);
    //       out << ")";
    //       break;
    //     }

    //case at_reprcaselegR:
    //     {
    //       shared_ptr<AST> body = ast->child(0);
    //       out << "(";
    //       if (ast->children.size() > 2)
    //         out << "(";
    //       sxp_doChildren(out, ast, 1, false, showTypes);
    //       if (ast->children.size() > 2)
    //         out << ")";
    //       out << " (";
    //       sxp_doChildren(out, body, 0, false, showTypes);
    //       out << "))";
    //       break;
    //     }

    // case at_reprtag:
    //     {
    //       out << "(tag ";
    //       sxp_doChildren(out, ast, 0, false, showTypes);
    //       out << ")";
    //       break;
    //     }

  case at_defunion:
  case at_defstruct:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> declares = ast->child(3);
      shared_ptr<AST> fc = ast->child(4);
      shared_ptr<AST> constraints = ast->child(5);

      sxp_maybe_open_quantifier(out, ast, showTypes);

      out << "(" << ast->atKwd() << " ";
      out.indentToHere();
      sxp_show_qual_name(out, ident, tvlist, showTypes);
      sxp_BitcP(out, category, showTypes);
      out << endl;
      sxp_BitcP(out, declares, showTypes);
      if (declares->children.size())
        out << endl;
      sxp_BitcP(out, fc, showTypes);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, showTypes);

      break;
    }

  case at_declunion:
  case at_declstruct:
  case at_declrepr:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> constraints = ast->child(3);

      sxp_maybe_open_quantifier(out, ast, showTypes);

      out << "(" << ast->atKwd() << " ";
      sxp_show_qual_name(out, ident, tvlist, showTypes);
      sxp_BitcP(out, category, showTypes);

      if (ident->flags & DEF_IS_EXTERNAL) {
        out << " external";
        if (ident->externalName.size())
          out << " " << ident->externalName;        
      }
      out << ")";

      sxp_maybe_close_quantifier(out, ast, showTypes);

      break;
    }

  case at_letbindings:
  case at_cond_legs:
    {
      out << "(";
      out.indentToHere();
      for (size_t c = 0; c < ast->children.size(); c++) {
        if (c > 0)
          out << endl;
        sxp_BitcP(out, ast->child(c), showTypes);
      }
      out << ")";

      break;
    }
        
    // The following are all done in the style of apply --
    // a parenthesized list of children
  case at_letbinding:
  case at_loopbindings:
  case at_loopbinding:
  case at_looptest:
  case at_cond_leg:
  case at_typeapp:
    //  case at_catchclause:
  case at_tcapp:
    {
      if (ast->printVariant && pf_IMPLIED) {
        sxp_doChildren(out, ast, 1, false, showTypes);
      }
      else {
        out << "(";
        sxp_doChildren(out, ast, 0, false, showTypes);
        out << ")";
        break;
      }
    }

  case at_otherwise:
    {
      out << "(otherwise ";
      sxp_BitcP(out, ast->child(1), showTypes);
      out << ")";
      break;
    }

  case at_usw_leg:
    {
      out << "(";
      sxp_doChildren(out, ast, 2, false, showTypes);
      out << " ";
      sxp_BitcP(out, ast->child(1), showTypes);
      out << ")";
      break;
    }

    // The following just recurse:
  case at_lambda:
    {
      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, ast->child(0), showTypes);
      out.more();

      for (size_t c = 1; c < ast->children.size(); c++) {
        out << std::endl;
        sxp_BitcP(out, ast->child(c), showTypes);
      }

      out << ")";

      if (showTypes) print_type(out, ast);

      break;
    }

  case at_usw_legs:
  case at_constructors:
  case at_reprctrs:
  case at_fields:
  case at_declares:
  case at_method_decls:
    {
      for (size_t c = 0; c < ast->children.size(); c++) {
        if (c > 0) out << std::endl;
        sxp_BitcP(out, ast->child(c), showTypes);
      }
      break;
    }

  case at_tvlist:
  case at_tcdecls:
  case at_fieldType:
    {
      sxp_doChildren(out, ast, 0, false, showTypes);
      break;
    }

  case at_constructor:
  case at_reprctr:
    {
      if (ast->children.size() > 1) {
        out << "(";
        sxp_doChildren(out, ast, 0, false, showTypes);
        out << ")";
      }
      else
        sxp_BitcP(out, ast->child(0), showTypes);
      break;
    }

  case at_field:
  case at_methdecl:
    {
      sxp_BitcP(out, ast->child(0), showTypes);
      if (ast->children.size() > 1) {
        out << ":";
        sxp_BitcP(out, ast->child(1), showTypes);
      }
      break;
    }

  //  case at_vpattern:
  case at_bitfield:
    {
      out << "(";
      out << ast->atKwd();
      sxp_doChildren(out, ast, 0, true, showTypes);
      out << ")";
      break;
    }

  case at_identPattern:
    {
      sxp_BitcP(out, ast->child(0), showTypes);
      if (ast->children.size() > 1) {
        out << ":";
        sxp_BitcP(out, ast->child(1), showTypes);
      }
      break;
    }

  case at_container:
    {
      out << "({";
      sxp_BitcP(out, ast->child(0), showTypes);
      out << "} ";
      sxp_BitcP(out, ast->child(1), showTypes);
      out << ")";
      break;
    }
  case at_identList:
    {
      sxp_doChildren(out, ast, 0, false, showTypes);
      break;
    }

    // The following cases should get handled in a default way, but
    // I don't want to use the default: target because it suppresses
    // errors that I want to see.
  case at_AnyGroup:
    {
      cerr << "BAD AST TYPE "
           << ast->astTypeName()
           << "TO BitC-pp.\n";
      break;
    }

  case agt_var:
  case agt_literal:
  case agt_tvar:
  case agt_CompilationUnit:
  case agt_definition:
  case agt_type_definition:
  case agt_tc_definition:
  case agt_value_definition:
  case agt_if_definition:
  case agt_type:
  case agt_qtype:
  case agt_fielditem:
  case agt_expr:
  case agt_eform:
  case agt_ow:
  case agt_category:
    //case agt_reprbodyitem:
  case agt_ucon:
  case agt_expr_or_define:
  case agt_openclosed:
  case agt_uselhs:
#if 0
  case at_xdefrepr:
  case at_reprcase:
  case at_reprcaselegR:
  case at_reprtag:
#endif
    {
      assert(false);
      break;
    }
  }

  if (showTypes) print_type(out, ast);

  out.setIndent(startIndent);
}

/// @brief Display the type associated with a form.
static void
doShowTypes(std::ostream& out, shared_ptr<AST> ast,
            shared_ptr<TSEnvironment > gamma,
            bool showMangName,
            bool raw = false,
            shared_ptr<TvPrinter> tvP=GC_NULL)
{
  switch(ast->astType) {
  case at_ident:

    // Move this to at_define ... etc, in case
    // there is a need to see differentiated
    // tvars in a single definition.

    if (!raw)
      tvP = TvPrinter::make();

    out << ast->s  << ": "
              << ((!ast->scheme)
            ? "??"
            : ast->scheme->asString(tvP, true));

    if (showMangName)
      if (ast->symType->isOfInfiniteType())
        out << "[Infinite Type]";
      else
        out << " [" << ast->symType->mangledString() << "]";

    break;

  case at_usesel:
    {
      doShowTypes(out, ast->child(1), gamma,
                  showMangName, raw, tvP);
      break;
    }

  case at_interface:
  case at_module:
    {
      size_t i=0;
      if (ast->astType == at_interface) {
        out << "(" << ast->atKwd() << " "
            << ast->child(0)->s << endl;
        i=1;
      }
      else {
        out << "(" << "source-unit"  << endl;
        i=0;
      }

      for (; i<ast->children.size(); i++) {
        switch(ast->child(i)->astType){
        case at_importAs:
        case at_provide:
        case at_declare:
          break;
        default:
          doShowTypes(out, ast->child(i), gamma,
                      showMangName, raw, tvP);
          out << endl;
          break;
        }
      }

      out << ")";
      break;
    }

  case at_proclaim:
    out << " " << " opaque ";
    doShowTypes(out, ast->child(0), gamma,
                showMangName, raw, tvP);
    break;

  case at_declare:
  case at_importAs:
  case at_provide:
    break;

  case at_defexception:
    {
      out << "  " << "exception " << ast->child(0)->s;
      break;
    }

  case at_deftypeclass:
    {
      //       out << "  " << "type-class "
      //           << ast->child(0)->s << ": "
      //           << ast->child(0)->symType->asString();
      out << "  " << "type-class ";
      doShowTypes(out, ast->child(0), gamma,
                  showMangName, raw, tvP);
      break;
    }

  case at_definstance:
    {
      if (!raw && (!tvP))
        tvP = TvPrinter::make();

      out << "  " << "Instance : "
          << ((!ast->scheme)
              ? "??"
              : ast->scheme->asString(tvP));

      break;
    }

  case at_defunion:
  case at_declunion:
    {
      out << "  " << "union ";
      doShowTypes(out, ast->child(0), gamma,
                  showMangName, raw, tvP);
      break;
    }

  case at_defstruct:
  case at_declstruct:
    {
      out << "  " << "struct ";
      doShowTypes(out, ast->child(0), gamma,
                  showMangName, raw, tvP);
      break;
    }

  case at_define:
  case at_recdef:
    {
      doShowTypes(out, ast->child(0), gamma,
                  showMangName, raw, tvP);
      break;
    }

  case at_identPattern:
    out << "  " << "val ";
    doShowTypes(out, ast->child(0), gamma,
                showMangName, raw, tvP);
    break;

  default:
    cerr << ast->loc.asString() << ": "
         << "Internal Compiler Error."
         << " Unexpected AST type "
         << ast->astTypeName()
         << " obtained by doshowTypes() routine."
         << endl;
    break;
  }
}

/// @brief top-level wrapper for the pretty printer
void
AST::PrettyPrint(std::ostream& strm, bool decorated,
                 bool endline) const
{
  INOstream out(strm);
  sxp_BitcP(out, shared_from_this(), decorated);
  if (endline)
    out << std::endl;
}

void
AST::PrettyPrint(INOstream& out, bool decorated,
                 bool endline) const
{
  sxp_BitcP(out, shared_from_this(), decorated);
  if (endline)
    out << std::endl;
}

/// @brief top-level wrapper for the pretty printer
void
AST::PrettyPrint(bool decorated) const
{
  INOstream out(std::cerr);
  sxp_BitcP(out, shared_from_this(), decorated);
  out << endl;
}

/// @brief top-level wrapper for the pretty printer
void
UocInfo::PrettyPrint(std::ostream& out, bool decorated)
{
  assert (uocAst->astType == at_module
          || uocAst->astType == at_interface);

  uocAst->PrettyPrint(out, decorated);
}

void
UocInfo::ShowTypes(std::ostream& out)
{
  doShowTypes(out, uocAst, gamma, false);
}

