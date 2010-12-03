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

      doChildren(blk_BitcP, out, constraints, 0, "where ", ", ", "\n", flags);
      blk_BitcP(out, openclosed, flags);
      out << ast->atKwd() << " ";
      blk_BitcP(out, ident, flags);
      doChildren(blk_BitcP, out, tvlist, 0, "(", ", ", ")", flags);
      out.more();
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

      doChildren(blk_BitcP, out, constraints, 0, "where ", ", ", "\n", flags);
      out << ast->atKwd() << " ";
      blk_BitcP(out, tapp, flags);

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

      doChildren(blk_BitcP, out, constraints, 0, "where ", ", ", "\n", flags);

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

      doChildren(blk_BitcP, out, constraints, 0, "where ", ",", "\n", flags);
      blk_BitcP(out, category, flags);
      if (ast->flags & UNION_IS_REPR)
        out << "repr ";
      else
        out << ast->atKwd() << " ";
      blk_BitcP(out, ident, flags);
      doChildren(blk_BitcP, out, tvlist, 0, "(", ", ", ")", flags);

      out.more();
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

      doChildren(blk_BitcP, out, constraints, 0, "where ", ",", "\n", flags);
      out << " " << ast->atKwd() << " ";
      blk_BitcP(out, ident, flags);
      out << " : ";
      blk_BitcP(out, proc_type, flags);

      out.more();

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

  case at_mixfix:
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
  case at_typeAnnotation:
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

      doChildren(blk_BitcP, out, constraints, 0, "where ", ", ", "\n", flags);
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

      doChildren(blk_BitcP, out, constraints, 0, "where ", ", ", "\n", flags);

      out << ast->atKwd() << " ";

      // Identifier:
      blk_BitcP(out, identPattern, flags);

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

#if dbg_flags != 0
  case at_letGather:            // DEBUG ONLY
  case at_fields:               // DEBUG_ONLY
    {
      out << ast->atKwd() << " { ";
      out.indentToHere();
      doChildren(blk_BitcP, out, ast, 0, "", ";\n", "\n}", flags);
      break;      
    }
#endif

    // CATCH-ALL:
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

