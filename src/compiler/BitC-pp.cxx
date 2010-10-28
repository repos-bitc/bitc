/**************************************************************************
 *
 * Copyright (C) 2010, Jonathan S. Shapiro
 * Portions Copyright (C) 2008, Johns Hopkins University.
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
#include "libsherpa/EnumSet.hxx"

using namespace boost;
using namespace sherpa;
using namespace std;

const PrettyPrintFlags NonRecursiveFlags = pp_InLayoutBlock | pp_FinalNewline;

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
sxp_BitcP(INOstream& out, shared_ptr <const AST> ast, PrettyPrintFlags);

static void
blk_BitcP(INOstream& out, shared_ptr <const AST> ast, PrettyPrintFlags);

/// @brief Wrapper to iterate across all children of @p ast starting
/// at @p from, optionally padding the first element with leading
/// white space if @p firstPad is true and optionally printing types
/// according to @p showTypes.
static void
doChildren(void (*BitcP)(INOstream& out, shared_ptr <const AST> ast, PrettyPrintFlags),
           INOstream& out, shared_ptr <const AST> ast, size_t from,
           const std::string& startMark,
           const std::string& sep,
           const std::string& endMark,
           PrettyPrintFlags flags)
{
  assert( (flags & NonRecursiveFlags) == pp_NONE );

  if (from == ast->children.size())
    return;

  out << startMark;
  for (size_t c = from; c < ast->children.size(); c++) {
    if (c > from)
      out << sep;
    BitcP(out, ast->child(c), flags);
  }
  out << endMark;
}

static void
sxp_doChildren(INOstream& out, shared_ptr <const AST> ast, size_t from,
               bool firstPad,
               PrettyPrintFlags flags)
{
  // Avoid redundant constructions:
  static std::string space = " ";
  static std::string empty = "";

  doChildren(sxp_BitcP,
             out, ast, from, 
             firstPad ? space : empty, 
             space,
             "",
             flags);
}

#if 0
/// @brief Called to emit the constraints on a type declaration or definition.
/// 
/// If no constraints are in force, emits " " so that the succeeding
/// "is", if present, will not abut the type name.
static void
blk_pp_constraints(INOstream& out, shared_ptr<const AST> constraints,
                     const bool showTypes)
{
  if (constraints->children.size() == 0)
    return;

  out << endl << "where";
  doChildren(blk_BitcP, out, constraints, 0, " ", ", ", showTypes);
}
#endif

static void
sxp_maybe_open_quantifier(INOstream& out, shared_ptr<const AST> defn,
                          PrettyPrintFlags flags)
{
  size_t nChildren = defn->children.size();
  shared_ptr<const AST> constraints = defn->child(nChildren-1);

  assert(constraints);
  if (constraints->children.size() == 0)
    return;

  out << "(forall ";
  sxp_BitcP(out, constraints, flags);
  out << " ";
}

static void
sxp_maybe_close_quantifier(INOstream& out, shared_ptr<const AST> defn,
                           PrettyPrintFlags flags)
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
                   PrettyPrintFlags flags)
{
  bool argsPresent = (tvlist->children.size() > 0);

  if (argsPresent) {
    out << "(" ;
    sxp_BitcP(out, ident, flags);
    out << " ";
    sxp_BitcP(out, tvlist, flags);
    out << ")" ;
  }
  else {
    sxp_BitcP(out, ident, flags);
  }
}

// Not yet handled:
//
//  at_docstring
//  at_Null - should it be? (no)
//  at_declare
//
//  at_qualType   - obsolete; last use removed from MethDecl.cxx
//
//  at_inner_ref  - deferred, not currently supported
//  at_suspend    - deferred, not currently supported
//  
static void
blk_BitcP(INOstream& out, shared_ptr <const AST> ast, PrettyPrintFlags flags)
{
  size_t startIndent = out.indentToHere();

  PrettyPrintFlags nrFlags = flags & NonRecursiveFlags;
  flags &= ~NonRecursiveFlags;

  // The following just recurse:
  switch(ast->astType) {
  case at_module:
    {
      out << ast->atKwd();

      // All modules are anonymous - the module name is just window
      // dressing, and we do not preserve it.

      out.more();
      doChildren(blk_BitcP, out, ast, 0, " is\n", "\n\n", "", flags);
      out.less();
      break;
    }

  case at_interface:
    {
      out << ast->atKwd();

      // Put name on same line:
      out << " ";
      blk_BitcP(out, ast->child(0), flags);

      out.more();
      doChildren(blk_BitcP, out, ast, 1, " is\n", "\n\n", "", flags);
      out.less();

      break;
    }

  case at_import:
    {
      out << ast->atKwd();
      out << " ";
      blk_BitcP(out, ast->child(0), flags);
      doChildren(blk_BitcP, out, ast, 1, " ", ", ", "", flags);
      break;
    }

  case at_ifsel:
    {
      // Note: arguments are somewhat inverted. Local ident is child(1).
      out << ast->child(0)->s;
      if (ast->child(0)->s != ast->child(1)->s) {
        out << " = "
            << ast->child(1)->s;
      }
      break;
    }

  case at_importAs:
    { 
      out << ast->atKwd();

      doChildren(blk_BitcP, out, ast, 0, " ", " as ", "", flags);

      break;
    }
  case at_provide:
    {
      out << ast->atKwd();

      out << " ";
      blk_BitcP(out, ast->child(0), flags);

      doChildren(blk_BitcP, out, ast, 1, " ", ", ", "", flags);

      break;
    }

  case at_deftypeclass:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> tcdecls = ast->child(2);
      shared_ptr<AST> openclosed = ast->child(3);
      shared_ptr<AST> methods = ast->child(4);
      shared_ptr<AST> constraints = ast->child(5);

      blk_BitcP(out, openclosed, flags);
      out << ast->atKwd() << " ";
      blk_BitcP(out, ident, flags);
      doChildren(blk_BitcP, out, tvlist, 0, "(", ", ", ")", flags);
      out.more();
      doChildren(blk_BitcP, out, constraints, 0, "\nwhere ", ", ", "", flags);
      //      blk_BitcP(out, tcdecls, flags);
      out.less();

      if (methods->children.size()) {
        out << "\nis ";

        out.indentToHere();
        doChildren(blk_BitcP, out, methods, 0, "", 
                   "\n",
                   "", flags);
      }
      else
        out << " {}";

      break;
    }

  case at_definstance:
    {
      shared_ptr<AST> tapp = ast->child(0);
      shared_ptr<AST> methods = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      out << ast->atKwd() << " ";
      blk_BitcP(out, tapp, flags);

      out.more();
      doChildren(blk_BitcP, out, constraints, 0, "\nwhere ", ", ", "", flags);
      out.less();

      if (methods->children.size()) {
        out << "\nis ";

        out.indentToHere();
        doChildren(blk_BitcP, out, methods, 0, "", "\n", "", flags);
      }

      break;
    }
  case at_tcmethod_binding:
    {
      doChildren(blk_BitcP, out, ast, 0, "", " = ", "", flags);
      break;
    }

  case at_defrepr:
  case at_defstruct:
  case at_defunion:
  case at_defexception:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> declares = ast->child(3);
      shared_ptr<AST> fc = ast->child(4);
      shared_ptr<AST> constraints = ast->child(5);

      blk_BitcP(out, category, flags);

      // Careful! Reprs are transformed into unions by the reprSimp
      // pass, so what we get here might actually be a repr:
      if (ast->flags & UNION_IS_REPR)
        out << "repr ";
      else
        out << ast->atKwd() << " ";
      blk_BitcP(out, ident, flags);
      doChildren(blk_BitcP, out, tvlist, 0, "(", ", ", ")", flags);

      out.more();
      doChildren(blk_BitcP, out, constraints, 0, "\nwhere ", ", ", "", flags);
      doChildren(blk_BitcP, out, declares, 0, "\n", "\n", "", flags);
      out.less();

      if (fc->children.size()) {
        out << "\nis ";

        out.indentToHere();
        doChildren(blk_BitcP, out, fc, 0, "", 
#if 0
                   (ast->astType == at_defstruct) ? "\n" : "\n\n",
#else
                   "\n",
#endif
                   "", flags);
      }

      break;
    }

  case at_reprctr:
    {
      bool emittedSome = false;

      blk_BitcP(out, ast->child(0), flags);
      out << "\nwhere ";
      out.indentToHere();

      doChildren(blk_BitcP, out, ast, 1, "", ",\n", "", flags);
      break;
    }
  case at_reprrepr:
    {
      doChildren(blk_BitcP, out, ast, 0, "", " == ", "", flags);
      break;
    }

  case at_constructor:
    {
      bool isRepr = false;

      if (ast->children.size() == 1) {
        blk_BitcP(out, ast->child(0), flags);
      }
      else {
        blk_BitcP(out, ast->child(0), flags);
        out << " ";
        (void) out.indentToHere(); // record the "is" depth

        out << "is ";
        size_t isDepth = out.indentToHere();
        doChildren(blk_BitcP, out, ast, 1, "", "\n", "", flags);
        out.setIndent(isDepth);

        for(size_t c = 1; c < ast->children.size(); c++) {
          shared_ptr<AST> fld = ast->child(c);
          if (fld->flags & FLD_IS_DISCM) {
            isRepr = true;
            break;
          }
        }

        //        out.setIndent(isDepth);

        if (isRepr) {
          bool emittedSome = false;

          out << "\nwhere ";
          out.indentToHere();
          for(size_t c = 1; c < ast->children.size(); c++) {
            shared_ptr<AST> fld = ast->child(c);
            if (fld->flags & FLD_IS_DISCM) {
              if (emittedSome)
                out << ",\n";

              blk_BitcP(out, fld->child(0), flags); // field name
              out << " == "
                  << fld->unin_discm;
              emittedSome = true;
            }
          }
        }
      }
      
      break;
    }

  case at_declstruct:
  case at_declunion:
  case at_declrepr:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> constraints = ast->child(5);

      blk_BitcP(out, category, flags);
      if (ast->flags & UNION_IS_REPR)
        out << "repr ";
      else
        out << ast->atKwd() << " ";
      blk_BitcP(out, ident, flags);
      doChildren(blk_BitcP, out, tvlist, 0, "(", ", ", ")", flags);

      out.more();
      doChildren(blk_BitcP, out, constraints, 0, "\nwhere ", ",", "", flags);
      if (ident->flags & DEF_IS_EXTERNAL) {
        out << "\nexternal";
        if (ident->externalName.size())
          out << " " << ident->externalName;        
      }
      out.less();


      break;
    }

  case at_proclaim:
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> proc_type = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      out << " " << ast->atKwd() << " ";
      blk_BitcP(out, ident, flags);
      out << " : ";
      blk_BitcP(out, proc_type, flags);

      out.more();
      doChildren(blk_BitcP, out, constraints, 0, "\nwhere ", ",", "", flags);

      if (ident->flags & DEF_IS_EXTERNAL) {
        out << "\nexternal";
        if (ident->externalName.size())
          out << " " << ident->externalName;        
      }
      out.less();
    }
    break;

    ////////////////////////////////////////////////////////////////////
    // Things that are still implemented in sxp_BitcP in support of
    // something that hasn't moved yet:
    ////////////////////////////////////////////////////////////////////
  case at_fill:
    out << ast->atKwd() << " : ";
    blk_BitcP(out, ast->child(0), flags);
    if (ast->children.size() > 0) { // if RESERVED
      out << " = ";
      blk_BitcP(out, ast->child(0), flags);
    }
      
    break;

  case at_field:
  case at_methdecl:
  case at_method_decl:          // TC method
    doChildren(blk_BitcP, out, ast, 0, "", " : ", "", flags);
    break;

  case at_fieldType:
    {
      blk_BitcP(out, ast->child(0), flags);
      break;
    }
  case at_ident:
  case at_ifident:
    out << ast->s;
    if (Options::ppFQNS) {
      out << " /*" << ast->fqn;
      if (ast->externalName.size()) {
        out << "," << ast->externalName;
      }
      out << "*/";
    }

    break;

  case at_tcapp:
  case at_typeapp:
    {
      /// @bug Decide whether the shape of these should be similar to
      /// the shape of procedure application.
      blk_BitcP(out, ast->child(0), flags);
      doChildren(blk_BitcP, out, ast, 1, "(", ", ", ")", flags);
      break;
    }

  case at_methType:
  case at_fn:
  case at_tyfn:
    {
      out << ast->atKwd();
      blk_BitcP(out, ast->child(0), flags);
      out << " -> ";
      blk_BitcP(out, ast->child(1), flags);
      break;
    }

  case at_tvlist:
  case at_fnargVec:
  case at_argVec:
    {
      out << "(";
      doChildren(blk_BitcP, out, ast, 0, "", ", ", "", flags);
      out << ")";
      break;
    }

  case at_primaryType:
    if (ast->s == "unit")
      out << "()";
    else
      out << ast->s;
    break;

  case at_mixExpr:
    {
      // This should never occur in production, but we need it to
      // support --dumpafter for the early passes:
      doChildren(blk_BitcP, out, ast, 0, "", " ", "", flags);
      break;
    }

  case at_mutableType:
  case at_constType:
  case at_arrayRefType:
  case at_byRefType:
    out << ast->atKwd() << " ";
    blk_BitcP(out, ast->child(0), flags);
    break;

    /// @bug Temporary expedient: use alternate forms for postfix types:
  case at_boxedType:
  case at_unboxedType:
  case at_vectorType:
  case at_arrayType:
    {
      out << ast->atKwd();
      doChildren(blk_BitcP, out, ast, 0, "(", ",", ")", flags);
      break;
    }

    // This is exactly like the expedient case above. I'm keeping it
    // separate because this one isn't merely an expedient.
  case at_bitfieldType:
    {
      out << ast->atKwd();
      doChildren(blk_BitcP, out, ast, 0, "", "(", ")", flags);
      break;
    }

  case at_boxedCat:
    if (!(ast->printVariant & pf_IMPLIED))
      out << "boxed ";
    break;
  case at_unboxedCat:
    out << "unboxed ";
    break;
  case at_opaqueCat:
    out << "opaque ";
    break;

  case at_oc_closed:
    out << "closed ";
    break;
  case at_oc_open:
    // does not print!
    break;

    ////////////////////////////////////////////////////////////////////
    // EXPRESSIONS
    ////////////////////////////////////////////////////////////////////
  case at_tqexpr:
    // Argument order was swapped.
    doChildren(blk_BitcP, out, ast, 0, "", " : ", "", flags & ~pp_ShowTypes);
    break;

  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:

    out << ast->s;
    if (flags & pp_LitValues)
      out << " /* " << ast->litValue << " */";
    break;

  case at_stringLiteral:
    out << "\"" << ast->s << "\"";

    break;

  case at_recdef:
    {
      shared_ptr<AST> identPattern = ast->child(0);
      shared_ptr<AST> iLambda = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      out << ast->atKwd() << " ";

      // Procedure name:
      blk_BitcP(out, identPattern, flags);

      // Procedure arguments:
      shared_ptr<AST> iLamArgs = iLambda->child(0);
      shared_ptr<AST> iLamRetBlock = iLambda->child(1);
      if (iLamArgs->children.size())
        doChildren(blk_BitcP, out, iLamArgs, 0, "(", ", ", ")", flags);
      else
        out << "()";

      doChildren(blk_BitcP, out, constraints, 0, "\nwhere ", ", ", "", flags);
      out << " = " ;

      out.more();

      // Because of the way we build lambdas, the body is always an
      // at_labeledBlock for __return, and we are going to suppress
      // that during pretty print. The body of *that* is the true body
      // of the lambda:
      shared_ptr<AST> iLamBody = iLamRetBlock->child(1);

      // If it's a begin form, let the opening brace be on the same
      // line as the '='. This requires that we emit the '{' here.
      if (iLamBody->astType == at_begin)
        out << "{\n";
      else
        out << "\n";

      blk_BitcP(out, iLamRetBlock, flags| pp_InLayoutBlock);

      if (iLamBody->astType == at_begin) {
        out.less();
        out << "\n}";
      }

      out << "\n";
      break;
    }

  case at_define:
    {
      shared_ptr<AST> identPattern = ast->child(0);
      shared_ptr<AST> expr = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      out << ast->atKwd() << " ";

      // Procedure name:
      blk_BitcP(out, identPattern, flags);

      doChildren(blk_BitcP, out, constraints, 0, "\nwhere ", ", ", "\n ", flags);

      out << " = " ;
      blk_BitcP(out, expr, flags);
      break;
    }

  case at_identPattern:
    doChildren(blk_BitcP, out, ast, 0, "", " : ", "", flags);
    break;

  case at_container:
    {
      out << "__letSSA ";
      size_t outer = out.indentToHere();
      doChildren(blk_BitcP, out, ast->child(0), 0, "", "\n", "", flags);
      out.setIndent(outer);
      out << "\nin ";
      out.indentToHere();
      blk_BitcP(out, ast->child(1), flags | pp_InLayoutBlock);
      break;
    }

  case at_localFrame:
    {
      out << ast->atKwd();
      size_t outer = out.indentToHere();
      doChildren(blk_BitcP, out, ast->child(0), 0, "", "\n", "", flags);
      out.setIndent(outer);
      out << "\nin ";
      out.indentToHere();
      blk_BitcP(out, ast->child(1), flags | pp_InLayoutBlock);
      break;
    }

  case at_letStar:
  case at_let:
  case at_letrec:
    {
      out << ast->atKwd() << " ";
      size_t outer = out.indentToHere();
      doChildren(blk_BitcP, out, ast->child(0), 0, "", "\n", "", flags);
      out.setIndent(outer);
      out << "\nin ";
      out.indentToHere();
      blk_BitcP(out, ast->child(1), flags | pp_InLayoutBlock);
      break;
    }
  case at_letbinding:
    {
      doChildren(blk_BitcP, out, ast, 0, "", " = ", "", flags);
      break;
    }
  case at_loopbinding:
    {
      blk_BitcP(out, ast->child(0), flags);
      out << " = ";
      blk_BitcP(out, ast->child(1), flags);
      out << " then ";
      blk_BitcP(out, ast->child(2), flags);
      break;
    }

  case at_dummyType:
  case at_exceptionType:
    {
      out << ast->atKwd();
      break;
    }

  case at_begin:
    {
      if (nrFlags & pp_InLayoutBlock) {
        doChildren(blk_BitcP, out, ast, 0, "", "\n", "", flags);
      }
      else {
        out << "{\n";
        out.more();
        doChildren(blk_BitcP, out, ast, 0, "", "\n", "", flags);
        out.less();
        out << "\n}";
      }
      break;
    }

  case at_labeledBlock:
    {
      // labeled blocks get automatically inserted to handle return
      // and continue. The labels used are compiler-reserved symbols,
      // so we need to undo the labeling in those two cases.
      std::string label = ast->child(0)->s;

      if ((flags & pp_Raw) ||
          (label != "__return" && label != "__continue")) {
        out << ast->atKwd() << " ";
        blk_BitcP(out, ast->child(0), flags);
        out << "\nin ";
        out.indentToHere();
        blk_BitcP(out, ast->child(1), flags | pp_InLayoutBlock);
      }
      else {
        // Preserve the layout block context here, since if we are the
        // expression in a layout block context, and we are
        // suppressing the labels, then our body is in a layout block
        // context.
        blk_BitcP(out, ast->child(1), flags | (nrFlags & pp_InLayoutBlock));
      }

      break;
    }

  case at_return_from:
    {
      // __return and __continue are internally inserted. Emit
      // those the way the user keyed them.
      string ident = ast->child(0)->s;
      
      if (ident == "__continue") {
        out << "continue";
      }
      else if (ident == "__return") {
        out << "return ";
        blk_BitcP(out, ast->child(1), flags);
      }
      else {
        out << "from ";
        blk_BitcP(out, ast->child(0), flags);
        out << "return ";
        blk_BitcP(out, ast->child(1), flags);
      }
      break;
    }

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
#endif
  
      shared_ptr<AST> lamArgs = ast->child(0);
      shared_ptr<AST> lamRetBlock = ast->child(1);

      out << ast->atKwd();
      if (lamArgs->children.size())
        doChildren(blk_BitcP, out, lamArgs, 0, "(", ", ", ") ", flags);
      else
        out << "() ";

      blk_BitcP(out, lamRetBlock, flags| pp_InLayoutBlock);
      break;
    }

  case at_usesel:
  case at_fqCtr:
  case at_sel_ctr:
  case at_select:
    {
      doChildren(blk_BitcP, out, ast, 0, "", ".", "", flags);
      break;
    }

  case at_nth:
  case at_array_nth:
  case at_array_ref_nth:
  case at_vector_nth:
    {
      doChildren(blk_BitcP, out, ast, 0, "", "[", "]", flags);
      break;
    }

  case at_loop:
    {
      out << ast->atKwd() << " ";
      size_t baseIndent = out.indentToHere();
      doChildren(blk_BitcP, out, ast->child(0), 0, "", "\n", "", flags);
      out.setIndent(baseIndent);
      out << "\n  until ";
      out.indentToHere();
      blk_BitcP(out, ast->child(1), flags);
      out.setIndent(baseIndent);
      out << "\nin ";
      out.indentToHere();
      
      shared_ptr<AST> loopBody = ast->child(2);

      // Following is not true in gen-c.cxx, because an at_letStar has
      // been emitted.
      if (flags.lacks(pp_Raw) && 
          loopBody->astType == at_labeledBlock) {
        assert ((loopBody->astType == at_labeledBlock)
                && (loopBody->child(0)->s == "__continue"));
        loopBody = loopBody->child(1);
        // For the moment, I'm wrapping loop bodies in a BEGIN with a
        // trailing () for typing purposes. Suppress that:
        assert((loopBody->astType == at_begin)
               && (loopBody->children.size() == 2)
               && (loopBody->child(1)->astType == at_unit));
        loopBody = loopBody->child(0);
      }

      blk_BitcP(out, loopBody, flags | pp_InLayoutBlock);

      break;
    }

  case at_looptest:
    {
      // The at_looptest node is now vestigial, but I don't want to
      // make that change simultaneous with this one.
      blk_BitcP(out, ast->child(0), flags);
      break;
    }

  case at_uswitch:
    {
      // switch id = expr
      // case T in block
      // otherwise block

      out << ast->atKwd() << " ";
      blk_BitcP(out, ast->child(0), flags);
      out << " = ";
      blk_BitcP(out, ast->child(1), flags);
      out << "\n";
      doChildren(blk_BitcP, out, ast->child(2), 0, "", "\n", "", flags);
      if (ast->child(3)->astType != at_Null)
        blk_BitcP(out, ast->child(3), flags);
      
      break;
    }

  case at_usw_leg:
    {
      // Technically, the AST allows for multiple matches, and we
      // should add support for that in the grammar.
      out << ast->atKwd() << " ";
      blk_BitcP(out, ast->child(2), flags);
      out << " in ";
      out.indentToHere();
      blk_BitcP(out, ast->child(1), flags);
      break;
    }

  case at_otherwise:
    {
      // Can be under at_uswitch or at_try
      out << "\n" << ast->atKwd() << " ";
      blk_BitcP(out, ast->child(0), flags);
      break;
    }

  case at_try:
    {
      out << ast->atKwd() << " ";
      blk_BitcP(out, ast->child(0), flags);
      out << "\ncatch ";
      blk_BitcP(out, ast->child(1), flags);

      doChildren(blk_BitcP, out, ast->child(2), 0, "", "\n", "", flags);

      if (ast->child(3)->astType != at_Null)
        blk_BitcP(out, ast->child(3), flags);
      break;
    }

  case at_apply:
  case at_struct_apply:
  case at_object_apply:
  case at_ucon_apply:
    {
      blk_BitcP(out, ast->child(0), flags);
      out << "(";
      doChildren(blk_BitcP, out, ast, 1, "", ", ", "", flags);
      out << ")";
      break;
    }

  case at_setbang:
    {
      doChildren(blk_BitcP, out, ast, 0, "", " := ", "", flags);
      break;
    }

  case at_unit:
    {
      out << "()";
      break;
    }

    // Apply-style output, but built-in:
  case at_deref:
  case at_dup:
  case at_sizeof:
  case at_bitsizeof:
  case at_MakeVector:
  case at_vector:
  case at_array:
  case at_allocREF:
  case at_copyREF:
  case at_mkClosure:
  case at_setClosure:
  case at_mkArrayRef:
#ifdef HAVE_INDEXABLE_LENGTH_OPS
  case at_array_length:
  case at_array_ref_length:
  case at_vector_length:
#endif
    {
      out << ast->atKwd() << "(";
      doChildren(blk_BitcP, out, ast, 0, "", ", ", "", flags);
      out << ")";
      break;
    }

  case at_throw:
    {
      out << ast->atKwd() << " ";
      blk_BitcP(out, ast->child(0), flags);
      break;
    }

    // Following three are legacy transition from the S-expression
    // syntax, and can be removed once the transition is complete:
  case at_cond:
    {
      shared_ptr<AST> legs = ast->child(0);
      shared_ptr<AST> condElse = ast->child(1);

      if (legs->children.size()) {
        doChildren(blk_BitcP, out, legs, 0, "", "\nelse ", "", flags);

        out << "\nelse ";
        blk_BitcP(out, condElse, flags);
      }
      else {
        // No legs - simplifies to just the else case:
        blk_BitcP(out, condElse, flags);
      }

      break;
    }

  case at_cond_leg:
    {
      out << "if ";
      blk_BitcP(out, ast->child(0), flags);
      out << "\nthen ";
      blk_BitcP(out, ast->child(1), flags);
      break;
    }

  case at_if:
    {
      out << ast->atKwd() << " ";
      blk_BitcP(out, ast->child(0), flags);
      out << "\nthen ";

      size_t thenIndent = out.indentToHere();
      blk_BitcP(out, ast->child(1), flags | pp_InLayoutBlock );
      out.setIndent(thenIndent);

      if (ast->children.size() > 2) {
        out << "\nelse ";
        blk_BitcP(out, ast->child(2), flags | pp_InLayoutBlock );
      }
      break;
    }

  case at_when:
  case at_unless:
    {
      out << ast->atKwd() << " ";
      blk_BitcP(out, ast->child(0), flags);
      out << "\ndo ";
      out.indentToHere();
      blk_BitcP(out, ast->child(1), flags | pp_InLayoutBlock);
      break;
    }

  case at_and:
    {
      doChildren(blk_BitcP, out, ast, 0, "((", ") && (", "))", flags);
      break;
    }
  case at_or:
    {
      doChildren(blk_BitcP, out, ast, 0, "((", ") || (", "))", flags);
      break;
    }

  case at_letGather:
    {
      std::cerr << "blk_BitcP() should never be asked to print an internal node of type "
                << ast->tagName() << std::endl;
      assert(false);
    }

  default:
    {
      std::cerr << "blk_BitcP() needs support for AST type " 
                << ast->tagName() << std::endl;
      assert(false);
    }
  }


  if (flags & pp_ShowTypes) print_type(out, ast);

  out.setIndent(startIndent);
}

/// @brief Core of the pretty printer.
static void
sxp_BitcP(INOstream& out, shared_ptr <const AST> ast, PrettyPrintFlags flags)
{
  size_t startIndent = out.indentToHere();

  switch(ast->astType) {
  case at_module:               // migrated
  case at_interface:            // migrated
    {
      blk_BitcP(out, ast, flags);
      break;
    }

    ////////////////////////////////////////////////////////////////////
    // Things that are implemented in blk_BitcP, but still need to be
    // here in support of something that hasn't moved yet:
    ////////////////////////////////////////////////////////////////////
  case at_boxedCat:             // migrated
    if (!(ast->printVariant & pf_IMPLIED))
      out << ":boxed";
    break;
  case at_unboxedCat:           // migrated
    out << ":unboxed";
    break;
  case at_opaqueCat:            // migrated
    out << ":opaque";
    break;

  case at_oc_closed:            // migrated
    out << ":closed";
    break;

  case at_oc_open:              // migrated
    break;

    //////////////////////////////////////////////
    // Things that still need to be migrated:
    //////////////////////////////////////////////
  case at_docString:
    sxp_doChildren(out, ast, 0, true, flags);
    break;

  case at_Null:
    break;

  case at_mixExpr:              // migrated
    out << "(" << ast->atKwd();
    out.more();
    sxp_doChildren(out, ast, 0, true, flags);
    out << ")";
    out.less();
    break;

  case at_boolLiteral:          // migrated
  case at_charLiteral:          // migrated
  case at_intLiteral:           // migrated
  case at_floatLiteral:         // migrated

    out << ast->s;
    if (flags & pp_LitValues)
      out << " /* " << ast->litValue << " */";
    break;

  case at_stringLiteral:        // migrated
    out << "\"" << ast->s << "\"";
    break;

  case at_ident:                // migrated
  case at_ifident:              // migrated
    out << ast->s;
    if (Options::ppFQNS) {
      out << "{" << ast->fqn;
      if (ast->externalName.size()) {
        out << "," << ast->externalName;
      }
      out << "}";
    }
    break;

  case at_usesel:               // migrated
  case at_fqCtr:                // migrated
  case at_sel_ctr:              // migrated
  case at_select:               // migrated
    {
      sxp_BitcP(out, ast->child(0), flags);
      out << ".";
      sxp_BitcP(out, ast->child(1), flags);
    }
    break;

    ///////////////////////////////////////////////////////////
    //
    // at_define needs to be handled specially, because if
    // it wraps an at_ilambda we need to re-expand it into
    // the convenience syntax.
    //
    ///////////////////////////////////////////////////////////
  case at_recdef:               // migrated
    {
      sxp_maybe_open_quantifier(out, ast, flags);

      out << "(" << ast->atKwd();

      size_t oldIndent = out.indentToHere();

      out << " (";

      // Procedure name:
      sxp_BitcP(out, ast->child(0), flags);

      // Procedure arguments:
      shared_ptr<AST> iLambda = ast->child(1);
      sxp_doChildren(out, iLambda->child(0), 0, true, flags);

      out << ")";

      out << std::endl;
      out.setIndent(oldIndent);

      out.more();
      sxp_doChildren(out, iLambda, 1, true, flags);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, flags);

      break;
    }

  case at_define:               // migrated
    {
      sxp_maybe_open_quantifier(out, ast, flags);

      if (ast->child(1)->astType == at_lambda) {
        out << "(" << ast->atKwd() << " ";

        // Procedure name:
        sxp_BitcP(out, ast->child(0), flags);

        out << endl;
        out.more();

        sxp_doChildren(out, ast, 1, false, flags);
        out << ")";
      }
      else {
        // Handle as normal s-expr:
        out << "(" << ast->atKwd();
        out.more();
        sxp_doChildren(out, ast, 0, true, flags);
        out << ")";
        out.less();
      }

      sxp_maybe_close_quantifier(out, ast, flags);
      break;
    }

  case at_try:                  // migrated
    out << "(" << ast->atKwd() << " ";
    sxp_BitcP(out, ast->child(0), flags);
    out << endl;
    out.more();
    out << "(catch ";
    sxp_BitcP(out, ast->child(1), flags);
    out << " ";
    sxp_BitcP(out, ast->child(2), flags);
    out << " ";
    sxp_BitcP(out, ast->child(3), flags);
    out << ")";
    out.less();
    out << ")";
    break;

  case at_primaryType:          // migrated
    if (ast->s == "unit")
      out << "()";
    else
      out << ast->s;
    break;

  case at_deref:                // migrated
    if (ast->printVariant && pf_IMPLIED) {
      sxp_doChildren(out, ast, 0, true, flags);
      out << "^";
    }
    else {
      out << "(deref ";
      sxp_doChildren(out, ast, 0, false, flags);
      out << ")";
    }
    break;

  case at_lambda:               // migrated
    {
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
        {
          out << "(" << ast->atKwd();
          sxp_doChildren(out, ast, 0, true, flags);
          out << ")";
        }
      break;
    }

    ///////////////////////////////////////////////////////////
    //
    // The following are all emitted as a parenthesized
    // list of children->
    //
    ///////////////////////////////////////////////////////////

  case at_letGather:            // migrated
  case at_apply:                // migrated
  case at_struct_apply:         // migrated
  case at_object_apply:         // migrated
  case at_ucon_apply:           // migrated
    {
      out << "(";
      sxp_BitcP(out, ast->child(0), flags);
      out << " ";
      (void) out.indentToHere();

      for (size_t c = 1; c < ast->children.size(); c++) {
        if (c > 1) {
          if (ast->children.size() < 4)
            out << " ";
          else
            out << std::endl;
        }
        sxp_BitcP(out, ast->child(c), flags);
      }

      out << ")";
      break;
    }

  case at_when:                 // migrated
    {
      out << "(" << ast->atKwd() << " ";

      (void) out.indentToHere();

      sxp_BitcP(out, ast->child(0), flags);
      out << endl;
      sxp_doChildren(out, ast, 1, false, flags);
      out << ")";
      break;
    }

  case at_labeledBlock:         // migrated
  case at_return_from:          // migrated
    {
      /// @bug The emission for labelled block appears to be simply
      /// wrong.

      // Some blocks are implicitly introduced in the parser. These
      // require special handling
      string ident = ast->child(0)->s;
      if (ident == "__return") {
        if (ast->astType == at_labeledBlock) {
          // This is the (block __return <body>) that implicitly wraps
          // every user-introduced lambda body. Simply pretty print the
          // <body>.
          sxp_BitcP(out, ast->child(1), flags);
          break;
        }
        else {
          // (RETURN expr) is encoded as (RETURN-FROM __return expr).
          // Print it in the form that the user gave:
          out << "(return ";
          sxp_BitcP(out, ast->child(1), flags);
          out << ")";
        }

        break;
      }
      else if (ident == "__continue") {
        if (ast->astType == at_labeledBlock) {
          // This is the (block __continue <body>) that implicitly wraps
          // every loop body. Simply pretty print the <body>.
          sxp_BitcP(out, ast->child(1), flags);
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


  case at_begin:                // migrated
  case at_if:                   // migrated
  case at_and:                  // migrated
  case at_or:                   // migrated
  case at_reprrepr:             // migrated
    {
      out << "(" << ast->atKwd() << " ";

      (void) out.indentToHere();

      for (size_t c = 0; c < ast->children.size(); c++) {
        if (c > 0)
          out << std::endl;
        sxp_BitcP(out, ast->child(c), flags);
      }

      out << ")";
      break;
    }

  case at_uswitch:              // migrated
  case at_vector:               // migrated
  case at_vectorType:           // migrated
  case at_MakeVector:           // migrated
  case at_array:                // migrated
  case at_arrayType:            // migrated
  case at_byRefType:            // migrated
  case at_arrayRefType:         // migrated
  case at_boxedType:            // migrated
  case at_unboxedType:          // migrated
  case at_letStar:              // migrated
  case at_declare:
  case at_cond:                 // migrated
  case at_throw:                // migrated
  case at_setbang:              // migrated
  case at_mutableType:          // migrated
  case at_constType:            // migrated
  case at_condelse:             // not migrated - explicit
#ifdef HAVE_INDEXABLE_LENGTH_OPS
  case at_array_length:         // migrated
  case at_array_ref_length:     // migrated
  case at_vector_length:        // migrated
#endif
  case at_localFrame:           // migrated
  case at_frameBindings:        // not migrated - explicit
  case at_loop:                 // migrated
  case at_dup:                  // migrated
  case at_inner_ref:
  case at_suspend:
  case at_fill:                 // migrated
  case at_sizeof:               // migrated
  case at_bitsizeof:            // migrated
  case at_mkArrayRef:           // migrated
    //case at_reprbody:
    {
      out << "(" << ast->atKwd();
      sxp_doChildren(out, ast, 0, true, flags);
      out << ")";
      break;
    }

  case at_nth:                  // migrated
  case at_array_nth:            // migrated
  case at_array_ref_nth:        // migrated
  case at_vector_nth:           // migrated
    {
      sxp_BitcP(out, ast->child(0), flags);
      out << "[";
      sxp_BitcP(out, ast->child(1), flags);
      out << "]";
      break;
    }

  case at_methType:             // migrated
  case at_fn:                   // migrated
    {
      // Reworked by shap on 10/9/2008 to use arrow syntax
      out << "(" << ast->atKwd();
      sxp_doChildren(out, ast->child(0), 0, true, flags);
      out << " ->";
      sxp_doChildren(out, ast, 1, true, flags);
      out << ")";
      break;
    }

  case at_constraints:          // not migrated - explicit
    {
      if (ast->children.size() > 0) {
        out << "(";
        sxp_doChildren(out, ast, 0, false, flags);
        out << ")";
      }
      break;
    }

  case at_letrec:               // migrated
  case at_let:                  // migrated
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
          sxp_BitcP(out, ast->child(c), flags);
        }
        out << endl;
        out.setIndent(startIndent);
        out.more();
      }

      out << "(" << ast->atKwd() << " ";

      sxp_BitcP(out, ast->child(0), flags);
      out << endl;
      out.more();
      sxp_BitcP(out, ast->child(1), flags);

      out << ")";

      if (constraints->children.size())
        out << ")";

      break;
    }

  case at_exceptionType:        // migrated
  case at_dummyType:            // migrated
    {
      out << ast->atKwd();
      break;
    }

  case at_unit:                 // migrated
    out << "()";
    break;

  case at_tqexpr:               // migrated
    // Argument order was swapped.
    sxp_BitcP(out, ast->child(0), flags);
    out << " : ";
    sxp_BitcP(out, ast->child(1), flags);
#if 0
    out << "(the ";
    sxp_BitcP(out, ast->child(1), flags);
    out << " ";
    sxp_BitcP(out, ast->child(0), flags);
    out << ")";
#endif

    break;

  case at_tvlist:               // migrated
  case at_fnargVec:             // migrated
  case at_argVec:               // migrated
    out << "(";
    sxp_doChildren(out, ast, 0, false, flags);
    out << ")";

    break;

  case at_allocREF:             // migrated
  case at_copyREF:              // migrated
  case at_mkClosure:            // migrated
  case at_setClosure:           // migrated
    {
      out << "(" << ast->atKwd();
      sxp_doChildren(out, ast, 0, true, flags);
      out << ")";
      break;
    }

  case at_importAs:             // migrated
    {
      shared_ptr<AST> ifAst = ast->child(0);
      shared_ptr<AST> idAst = ast->child(1);

      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, ifAst, flags);
      out << " as ";
      sxp_BitcP(out, idAst, flags);
      out << ")";
      break;
    }

  case at_defexception:         // migrated
    out << "(" << ast->atKwd();
    sxp_doChildren(out, ast, 0, true, flags);
    out << ")";
    break;

  case at_provide:              // migrated
  case at_import:               // migrated
    out << "(" << ast->atKwd();
    sxp_doChildren(out, ast, 0, true, flags);
    out << ")";
    break;

  case at_ifsel:                // migrated
    if (ast->child(0)->s == ast->child(1)->s)
      out << ast->child(0)->s;
    else
      out << "(" << ast->child(1)->s
          << " as "
          << ast->child(0)->s
          << ")";
    break;

  case at_proclaim:             // migrated
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> proc_type = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, ident, flags);
      out << " : ";
      sxp_BitcP(out, proc_type, flags);
      if (ident->flags & DEF_IS_EXTERNAL) {
        out << " external";
        if (ident->externalName.size())
          out << " " << ident->externalName;        
      }

      sxp_BitcP(out, ast->child(2), flags);
      out << ")";
    }
    break;

  case at_deftypeclass:         // migrated
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> tcdecls = ast->child(2);
      shared_ptr<AST> openclosed = ast->child(3);
      shared_ptr<AST> methods = ast->child(4);
      shared_ptr<AST> constraints = ast->child(5);

      sxp_maybe_open_quantifier(out, ast, flags);

      out << "(" << ast->atKwd() << " ";
      sxp_show_qual_name(out, ident, tvlist, flags);
      if(openclosed->astType != at_Null) {
        out << " ";
        sxp_BitcP(out, openclosed, flags);
      }
      out << " ";
      sxp_BitcP(out, tcdecls, flags);
      out << " ";
      sxp_BitcP(out, methods, flags);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, flags);

      break;
    }

  case at_definstance:          // migrated
    {
      shared_ptr<AST> tapp = ast->child(0);
      shared_ptr<AST> methods = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);

      sxp_maybe_open_quantifier(out, ast, flags);

      out << "(" << ast->atKwd() << " ";
      sxp_BitcP(out, tapp, flags);
      sxp_BitcP(out, methods, flags);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, flags);

      break;
    }

  case at_tcmethods:            // not migrated - explicit recurse
    {
      sxp_doChildren(out, ast, 0, false, flags);
      break;
    }

  case at_tcmethod_binding:     // migrated
    {
      out << "(= " << ast->child(0)->s
          << " " << ast->child(1)->s
          << ")";
      break;
    }
 
  case at_tyfn:                 // migrated
    out << "(" << ast->atKwd();
    out << "(";
    sxp_BitcP(out, ast->child(0), flags);
    out << ")";
    sxp_BitcP(out, ast->child(1), flags);
    out << ")";
    break;

    /// @bug Should this have been merged with at_methdecl? I'm not
    /// clear if we needed a distinct AST type for this. Check the
    /// handling in the resolver and the type inference pass.
  case at_method_decl:          // migrated
    sxp_BitcP(out, ast->child(0), flags);
    out << " : ";
    sxp_BitcP(out, ast->child(1), flags);
    break;

  case at_qualType:
    {
      out << "(forall ";
      sxp_BitcP(out, ast->child(0), flags);
      sxp_BitcP(out, ast->child(1), flags);
      out << ")";
      break;
    }

  case at_defrepr:              // migrated
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1); // empty
      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> declares = ast->child(3);
      shared_ptr<AST> fc = ast->child(4);
      shared_ptr<AST> constraints = ast->child(5); // empty

      out << "(" << ast->atKwd() << " ";
      out.indentToHere();
      sxp_show_qual_name(out, ident, tvlist, flags);
      sxp_BitcP(out, category, flags);
      out << endl;
      sxp_BitcP(out, declares, flags);
      if (declares->children.size())
        out << endl;

      sxp_BitcP(out, fc, flags);
      out << ")";
      break;
    }

  case at_defstruct:            // migrated
  case at_defunion:             // migrated
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> declares = ast->child(3);
      shared_ptr<AST> fc = ast->child(4);
      shared_ptr<AST> constraints = ast->child(5);

      sxp_maybe_open_quantifier(out, ast, flags);

      out << "(" << ast->atKwd() << " ";
      out.indentToHere();
      sxp_show_qual_name(out, ident, tvlist, flags);
      sxp_BitcP(out, category, flags);
      out << endl;
      sxp_BitcP(out, declares, flags);
      if (declares->children.size())
        out << endl;
      sxp_BitcP(out, fc, flags);
      out << ")";

      sxp_maybe_close_quantifier(out, ast, flags);

      break;
    }

  case at_declunion:            // migrated
  case at_declstruct:           // migrated
  case at_declrepr:             // migrated
    {
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> tvlist = ast->child(1);
      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> constraints = ast->child(5);

      sxp_maybe_open_quantifier(out, ast, flags);

      out << "(" << ast->atKwd() << " ";
      sxp_show_qual_name(out, ident, tvlist, flags);
      sxp_BitcP(out, category, flags);

      if (ident->flags & DEF_IS_EXTERNAL) {
        out << " external";
        if (ident->externalName.size())
          out << " " << ident->externalName;        
      }
      out << ")";

      sxp_maybe_close_quantifier(out, ast, flags);

      break;
    }

  case at_letbindings:          // not migrated - explicit
  case at_cond_legs:            // not migrated - explicit
    {
      out << "(";
      out.indentToHere();
      for (size_t c = 0; c < ast->children.size(); c++) {
        if (c > 0)
          out << endl;
        sxp_BitcP(out, ast->child(c), flags);
      }
      out << ")";

      break;
    }
        
    // The following are all done in the style of apply --
    // a parenthesized list of children
  case at_letbinding:           // migrated
  case at_loopbindings:         // not migrated - explicit
  case at_loopbinding:          // migrated
  case at_looptest:             // migrated
  case at_cond_leg:             // migrated
  case at_typeapp:              // migrated
    //  case at_catchclause:
  case at_tcapp:                // migrated
    {
      if (ast->printVariant && pf_IMPLIED) {
        sxp_doChildren(out, ast, 1, false, flags);
      }
      else {
        out << "(";
        sxp_doChildren(out, ast, 0, false, flags);
        out << ")";
        break;
      }
    }

  case at_otherwise:            // migrated
    {
      out << "(otherwise ";
      sxp_BitcP(out, ast->child(1), flags);
      out << ")";
      break;
    }

  case at_usw_leg:              // migrated
    {
      out << "(";
      sxp_doChildren(out, ast, 2, false, flags);
      out << " ";
      sxp_BitcP(out, ast->child(1), flags);
      out << ")";
      break;
    }

  case at_usw_legs:             // not migrated - explicit
  case at_constructors:         // not migrated - explicit
  case at_reprctrs:             // not migrated - explicit
  case at_fields:               // not migrated - explicit
  case at_declares:             // not migrated - explicit
  case at_method_decls:         // not migrated - explicit
    {
      for (size_t c = 0; c < ast->children.size(); c++) {
        if (c > 0) out << std::endl;
        sxp_BitcP(out, ast->child(c), flags);
      }
      break;
    }

  case at_tcdecls:              // migrated
  case at_fieldType:            // migrated
    {
      sxp_doChildren(out, ast, 0, false, flags);
      break;
    }

  case at_constructor:          // migrated
  case at_reprctr:              // migrated
    {
      if (ast->children.size() > 1) {
        out << "(";
        sxp_doChildren(out, ast, 0, false, flags);
        out << ")";
      }
      else
        sxp_BitcP(out, ast->child(0), flags);
      break;
    }

  case at_field:                // migrated
  case at_methdecl:             // migrated
    {
      sxp_BitcP(out, ast->child(0), flags);
      assert(ast->children.size() > 1);
      if (ast->children.size() > 1) {
        out << ":";
        sxp_BitcP(out, ast->child(1), flags);
      }
      break;
    }

  //  case at_vpattern:
  case at_bitfieldType:         // migrated
    {
      out << "(";
      out << ast->atKwd();
      sxp_doChildren(out, ast, 0, true, flags);
      out << ")";
      break;
    }

  case at_identPattern:         // migrated
    {
      sxp_BitcP(out, ast->child(0), flags);
      if (ast->children.size() > 1) {
        out << ":";
        sxp_BitcP(out, ast->child(1), flags);
      }
      break;
    }

  case at_container:            // migrated
    {
      out << "({";
      sxp_BitcP(out, ast->child(0), flags);
      out << "} ";
      sxp_BitcP(out, ast->child(1), flags);
      out << ")";
      break;
    }
  case at_identList:            // not migrated - explictly visited
    {
      sxp_doChildren(out, ast, 0, false, flags);
      break;
    }

    // The following cases should get handled in a default way, but
    // I don't want to use the default: target because it suppresses
    // errors that I want to see.
  case at_AnyGroup:
    {
      cerr << "BAD AST TYPE "
           << ast->tagName()
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
    {
      assert(false);
      break;
    }
  }

  if (flags & pp_ShowTypes) print_type(out, ast);

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
         << ast->tagName()
         << " obtained by doshowTypes() routine."
         << endl;
    break;
  }
}

/// @brief top-level wrapper for the pretty printer
void
AST::PrettyPrint(std::ostream& strm, PrettyPrintFlags flags) const
{
  INOstream out(strm);
  blk_BitcP(out, shared_from_this(), flags);
  if (flags & pp_FinalNewline)
    out << std::endl;
}

void
AST::PrettyPrint(INOstream& out, PrettyPrintFlags flags) const
{
  blk_BitcP(out, shared_from_this(), flags);
  if (flags & pp_FinalNewline)
    out << std::endl;
}

/// @brief top-level wrapper for the pretty printer
void
AST::PrettyPrint(bool decorated) const
{
  INOstream out(std::cerr);

  PrettyPrintFlags flags = 
    decorated ? PrettyPrintFlags(pp_ShowTypes) : PrettyPrintFlags();

  blk_BitcP(out, shared_from_this(), flags);
  out << endl;
}

/// @brief top-level wrapper for the pretty printer
void
UocInfo::PrettyPrint(std::ostream& out, bool decorated)
{
  assert (uocAst->astType == at_module
          || uocAst->astType == at_interface);

  PrettyPrintFlags flags = 
    decorated ? PrettyPrintFlags(pp_ShowTypes) : PrettyPrintFlags();
  uocAst->PrettyPrint(out, flags);
}

void
UocInfo::ShowTypes(std::ostream& out)
{
  doShowTypes(out, uocAst, gamma, false);
}

