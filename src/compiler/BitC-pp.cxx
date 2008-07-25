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

#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <string>
#include <iostream>
#include <assert.h>
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"
#include "TypeScheme.hxx"
#include "INOstream.hxx"

using namespace sherpa;
using namespace std;

#define SWAROOP_TYPES

// At the moment, the "pretty" part of the pretty printing is broken.
static void
print_type(INOstream& out, GCPtr <const AST> ast)
{
  GCPtr<Type> ty = ast->symType;

  if(ty)
    out << " {" << ty->asString() << "}";
  else
    out << " {" << "??" << "}";
    
}  

static void
BitcP(INOstream& out, GCPtr <const AST> ast, bool);

static void
doChildren(INOstream& out, GCPtr <const AST> ast, size_t from, 
	   bool firstPad,
	   bool showTypes)
{
  if (ast->children.size() > 1 || from) {
    for(size_t c = from; c < ast->children.size(); c++) {
      if (c == from && firstPad)
	out << " ";
      else if (c > from)
	out << " ";
      BitcP(out, ast->child(c), showTypes);
    }
  }
  if (ast->children.size() == 1) {
    if (firstPad) out << " ";
    BitcP(out, ast->child(0), showTypes);
  };
}

static void
show_qual_name(INOstream &out,  GCPtr <const AST> ident, 
	       GCPtr <const AST> tvlist, GCPtr <const AST> constraints,
	       const bool showTypes) 
{ 
  bool constraintsPresent = constraints && (constraints->children.size() > 0);
  bool argsPresent = (tvlist->children.size() > 0);
  if(constraintsPresent) {
    out << "(forall ";
    BitcP(out, constraints, showTypes);
    out << " ";
  }
  
  if(argsPresent) {
    out << "(" ;
    BitcP(out, ident, showTypes);
    out << " ";
    BitcP(out, tvlist, showTypes);
    out << ")" ;
  }
  else {
    BitcP(out, ident, showTypes);
  }
  
  if(constraintsPresent)
    out << ")"; 
}

static void
show_qual_name(INOstream &out,  GCPtr <const AST> tapp, 
	       GCPtr <const AST> constraints, bool showTypes) 
{ 
  bool constraintsPresent = (constraints->children.size() > 0);
  if(constraintsPresent) {
    out << "(forall ";
    BitcP(out, constraints, showTypes);
    out << " ";
  }
  
  BitcP(out, tapp, showTypes);
  
  if(constraintsPresent)
    out << ")"; 
}

static void
BitcP(INOstream& out, GCPtr <const AST> ast, bool showTypes)
{
  size_t startIndent = out.indentToHere();

  switch(ast->astType) {
  case at_docString:
    doChildren(out, ast, 0, true, showTypes);
    break;
    
  case at_Null:
    break;

  case at_refCat:
    if (ast->printVariant != 1)
      out << ":ref";
    break;
  case at_valCat:
    out << ":val";
    break;
  case at_opaqueCat:
    out << ":opaque";
    break;

  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:

    out << ast->s;

#ifdef SWAROOP_TYPES 
    if (showTypes) print_type(out, ast);
#endif

    break;
  case at_stringLiteral:
    out << "\"" << ast->s << "\"";

#ifdef SWAROOP_TYPES 
    if (showTypes) print_type(out, ast);
#endif

    break;

  case at_ident:
  case at_ifident:
    out << ast->s;
    if (Options::ppFQNS) {
      out << "{" << ast->fqn.asString("::");
      if (ast->externalName.size()) {
	out << "," << ast->externalName;
      }
      out << "}";
    }
    
#ifdef SWAROOP_TYPES 
    if (showTypes) print_type(out, ast);
#endif
      
    break;

  case at_usesel:
    BitcP(out, ast->child(0), showTypes);
    out << ".";
    BitcP(out, ast->child(1), showTypes);
    break;

  case at_fqCtr:
  case at_sel_ctr:
  case at_select:
    {
      BitcP(out, ast->child(0), showTypes);
      out << ".";
      BitcP(out, ast->child(1), showTypes);
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
      out << "(" << ast->atKwd();
      
      size_t oldIndent = out.indentToHere();
      
      out << " (";
      
      // Procedure name:
      BitcP(out, ast->child(0), showTypes);
      
      // Procedure arguments:
      GCPtr<AST> iLambda = ast->child(1);
      doChildren(out, iLambda->child(0), 0, true, showTypes);
      
      out << ")";
      
      out << std::endl;
      out.setIndent(oldIndent);
      
      out.more();
      doChildren(out, iLambda, 1, true, showTypes);
      out << ")";
      
      break;
    }

  case at_define:
    {
      if (ast->child(1)->astType == at_lambda) {
	out << "(" << ast->atKwd() << " ";

	// Procedure name:
	BitcP(out, ast->child(0), showTypes);

	out << endl;
	out.more();

	doChildren(out, ast, 1, false, showTypes);
	out << ")";
      }
      else {
	// Handle as normal s-expr:
	out << "(" << ast->atKwd();
	out.more();
	doChildren(out, ast, 0, true, showTypes);
	out << ")";
	out.less();
      }
      break;
    }


  case at_try:
    out << "(" << ast->atKwd();
    BitcP(out, ast->child(0), showTypes);
    out << endl;
    out.more();
    out << "(catch ";
    BitcP(out, ast->child(1), showTypes);
    out << " ";
    BitcP(out, ast->child(2), showTypes);
    out << " ";
    BitcP(out, ast->child(3), showTypes);
    out << ")";
    out.less();
    out << ")";
    break;

  case at_primaryType:
    if(ast->s == "unit")
      out << "()";
    else
      out << ast->s;
    break;

  case at_deref:
    if (ast->printVariant == 1) {
      doChildren(out, ast, 0, true, showTypes);
      out << "^";
    }
    else {
      out << "(deref ";
      doChildren(out, ast, 0, false, showTypes);
      out << ")";
    }
    break;

#if 0
  case at_lambda: 
    {
#if 0
      if (ast->printVariant == 1) {
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
	  doChildren(out, ast, 0, true, showTypes);
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
  case at_ucon_apply:
    {
      out << "(";
      BitcP(out, ast->child(0), showTypes);
      out << " ";
      (void) out.indentToHere();

      for(size_t c = 1; c < ast->children.size(); c++) {
	if (c > 1) {
	  if (ast->children.size() < 4)
	    out << " ";
	  else
	    out << std::endl;
	}
	BitcP(out, ast->child(c), showTypes);
      }

      out << ")";
      break;
    }

  case at_when:
    {
      out << "(" << ast->atKwd() << " ";

      (void) out.indentToHere();

      BitcP(out, ast->child(0), showTypes);
      out << endl;
      doChildren(out, ast->child(1), 0, false, showTypes);
      out << ")";
      break;
    }

  case at_begin:
  case at_if:
  case at_and:
  case at_or:
  case at_reprrepr:
    {
      out << "(" << ast->atKwd() << " ";

      (void) out.indentToHere();

      for(size_t c = 0; c < ast->children.size(); c++) {
	if (c > 0)
	  out << std::endl;
	BitcP(out, ast->child(c), showTypes);
      }

      out << ")";
      break;
    }

  case at_switch:
  case at_vector:
  case at_vectorType:
  case at_makevectorL:
  case at_array:
  case at_arrayType:
  case at_byrefType:
  case at_refType:
  case at_valType:
  case at_letStar:
  case at_declare:
  case at_not:
  case at_cond:
  case at_throw:
  case at_setbang:
  case at_mutableType:
  case at_fn: 
  case at_otherwise:
  case at_array_nth:
  case at_vector_nth:
  case at_array_length:
  case at_vector_length:
  case at_localFrame:
  case at_frameBindings:
  case at_do:
  case at_dup:
  case at_inner_ref:
  case at_suspend:
  case at_fill:
  case at_reserved:
    //case at_reprbody:
    {
      out << "(" << ast->atKwd();
      doChildren(out, ast, 0, true, showTypes);
      out << ")";
      break;
    }
    
  case at_constraints:
    {
      if(ast->children.size() > 0) {
	out << "(";
	doChildren(out, ast, 0, false, showTypes);
	out << ")";
      }
      break;
    }

  case at_letrec:
  case at_let:
    {
      GCPtr<AST> constraints = ast->child(2);
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
	  BitcP(out, ast->child(c), showTypes);
	}
	out << endl;
	out.setIndent(startIndent);
	out.more();
      }
      
      out << "(" << ast->atKwd() << " ";
      
      BitcP(out, ast->child(0), showTypes);
      out << endl;
      out.more();
      BitcP(out, ast->child(1), showTypes);
      
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
    BitcP(out, ast->child(1), showTypes);
    out << " ";
    BitcP(out, ast->child(0), showTypes);
    out << ")";


#ifdef SWAROOP_TYPES 
    if (showTypes) print_type(out, ast);
#endif
    break;

  case at_fnargVec: 
  case at_argVec:
    out << "(";
    doChildren(out, ast, 0, false, showTypes);
    out << ")";
#ifdef SWAROOP_TYPES 
    if (showTypes) print_type(out, ast);
#endif
    break;

  case at_allocREF:
  case at_copyREF:
  case at_mkClosure:
  case at_setClosure:    
    {
      if (ast->printVariant == 1) {
	out << "(";
	BitcP(out, ast->child(0), showTypes);
	out << ", ";
	BitcP(out, ast->child(1), showTypes);
	out << ")";
      
#ifdef SWAROOP_TYPES 
	if (showTypes) print_type(out, ast);
#endif
      }
      else {
	out << "(" << ast->atKwd();
	doChildren(out, ast, 0, true, showTypes);
	out << ")";
      }
      break;
    }

  case at_importAs:
    {
      GCPtr<AST> ifAst = ast->child(0); 
      GCPtr<AST> idAst = ast->child(1);

      out << "(" << ast->atKwd();
      BitcP(out, ifAst, showTypes);
      out << " as ";
      BitcP(out, idAst, showTypes);
      out << ")";
      break;
    }

  case at_provide:
  case at_defexception:
    out << "(" << ast->atKwd();
    doChildren(out, ast, 0, true, showTypes);
    out << ")";
    break;

  case at_import:
    out << "(" << ast->atKwd();
    BitcP(out, ast->child(0), showTypes);
    out << " import ";
    doChildren(out, ast, 1, true, showTypes);
    break;

  case at_ifsel:
    if(ast->child(0)->s == ast->child(1)->s)
      out << ast->child(0)->s;
    else
      out << "(= " << ast->child(0)->s
	  << " " << ast->child(1)->s
	  << ")";
    break;
    
  case at_proclaim:
    {
      GCPtr<AST> ident = ast->child(0);
      GCPtr<AST> proc_type = ast->child(1);
      out << "(" << ast->atKwd() << " ";
      BitcP(out, ident, showTypes);
      out << " : ";
      BitcP(out, proc_type, showTypes);
      if(ident->Flags & DEF_IS_EXTERNAL) {
	out << " external";
	if(ident->externalName.size())
	  out << " " << ident->externalName;	
      }

      BitcP(out, ast->child(2), showTypes);
      out << ")";            
    }
    break;
 
  case at_deftypeclass:
    {
      GCPtr<AST> ident = ast->child(0);
      GCPtr<AST> tvlist = ast->child(1);
      GCPtr<AST> tcdecls = ast->child(2);
      GCPtr<AST> methods = ast->child(3);
      GCPtr<AST> constraints = ast->child(4);

      out << "(" << ast->atKwd() << " ";
      show_qual_name(out, ident, tvlist, constraints, showTypes);
      out << " ";
      BitcP(out, tcdecls, showTypes);
      out << " ";
      BitcP(out, methods, showTypes);      
      out << ")";
      break;
    }

  case at_definstance:
    {
      GCPtr<AST> tapp = ast->child(0);
      GCPtr<AST> methods = ast->child(1);
      GCPtr<AST> constraints = ast->child(2);
      out << "(" << ast->atKwd() << " ";
      show_qual_name(out, tapp, constraints, showTypes);
      BitcP(out, methods, showTypes);
      out << ")";
      break;
    }

  case at_methods:
    {
      doChildren(out, ast, 0, false, showTypes);
      break;
    }

  case at_tyfn:
    out << "(" << ast->atKwd();
    out << "(";
    BitcP(out, ast->child(0), showTypes);
    out << ")";
    BitcP(out, ast->child(1), showTypes);
    out << ")";
    break;
    
  case at_method_decl:
    BitcP(out, ast->child(0), showTypes);
    out << " : ";
    BitcP(out, ast->child(1), showTypes);
    break;

  case at_qualType:
    {
      out << "(forall ";
      BitcP(out, ast->child(0), showTypes);
      BitcP(out, ast->child(1), showTypes);
      out << ")";
      break;
    }
            
  case at_defrepr:
    {
      GCPtr<AST> ident = ast->child(0);
      GCPtr<AST> category = ast->child(1);
      GCPtr<AST> declares = ast->child(2);
      GCPtr<AST> body = ast->child(3);

      out << "(" << ast->atKwd() << " ";
      BitcP(out, ident, showTypes);
      BitcP(out, category, showTypes);
      out << " ";
      BitcP(out, declares, showTypes);
      out << "(";
      doChildren(out, body, 0, false, showTypes);
      out << "))";
      break;
    }

    
    //case at_reprcase:
    //     {
    //       out << "(case ";
    //       doChildren(out, ast, 0, false, showTypes);
    //       out << ")";
    //       break;
    //     }

    //case at_reprcaselegR:
    //     {
    //       GCPtr<AST> body = ast->child(0);
    //       out << "(";
    //       if (ast->children.size() > 2)
    // 	out << "(";
    //       doChildren(out, ast, 1, false, showTypes);
    //       if (ast->children.size() > 2)
    // 	out << ")";
    //       out << " (";
    //       doChildren(out, body, 0, false, showTypes);
    //       out << "))";
    //       break;
    //     }

    // case at_reprtag:
    //     {
    //       out << "(tag ";
    //       doChildren(out, ast, 0, false, showTypes);
    //       out << ")";
    //       break;
    //     }
    
  case at_defunion:
  case at_defstruct:
    {
      GCPtr<AST> ident = ast->child(0);
      GCPtr<AST> tvlist = ast->child(1);
      GCPtr<AST> category = ast->child(2);
      GCPtr<AST> declares = ast->child(3);
      GCPtr<AST> fc = ast->child(4);
      GCPtr<AST> constraints = ast->child(5);

      out << "(" << ast->atKwd() << " ";
      out.indentToHere();
      show_qual_name(out, ident, tvlist, constraints, showTypes);
      BitcP(out, category, showTypes);
      out << endl;
      BitcP(out, declares, showTypes);
      out << endl;
      BitcP(out, fc, showTypes);
      out << ")";
      break;
    }

  case at_declunion:
  case at_declstruct:
  case at_declrepr:
    {
      GCPtr<AST> ident = ast->child(0);
      GCPtr<AST> tvlist = ast->child(1);
      GCPtr<AST> category = ast->child(2);
      GCPtr<AST> constraints = ast->child(3);
      

      out << "(" << ast->atKwd() << " ";
      show_qual_name(out, ident, tvlist, constraints, showTypes);
      BitcP(out, category, showTypes);
      
      if(ident->Flags & DEF_IS_EXTERNAL) {
	out << " external";
	if(ident->externalName.size())
	  out << " " << ident->externalName;	
      }
      out << ")";
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
	BitcP(out, ast->child(c), showTypes);
      }
      out << ")";

      break;
    }
	
    // The following are all done in the style of apply -- 
    // a parenthesized list of children
  case at_letbinding:
  case at_dobindings:
  case at_dobinding:
  case at_dotest:
  case at_cond_leg:
  case at_typeapp:
    //  case at_catchclause:
  case at_tcapp:
    {
      if (ast->printVariant == 1) {
	doChildren(out, ast, 1, false, showTypes);
      }
      else {
	out << "(";
	doChildren(out, ast, 0, false, showTypes);
	out << ")";
	break;
      }
    }

  case at_sw_leg:
    {
      out << "(";
      doChildren(out, ast, 2, false, showTypes);
      out << " ";
      BitcP(out, ast->child(1), showTypes);
      out << ")";
      break;
    }

    // The following just recurse:
  case at_module:
    {
      out << "(" << ast->atKwd();
      out.more();

      if (ast->printVariant != 0) {
	// explicit module form. Put name on same line:
	out << " ";
	BitcP(out, ast->child(0), showTypes);
      }
      else
	// Each form on its own line:
	out << std::endl;

      /* Dont call doChildren; that will put spaces in front
	 of the top level forms. Remember, bitc-version has
	 already been emitted without a space */
      for(unsigned i = 0; i < ast->children.size(); i++) {
	if (i > 0)
	  out << std::endl;
	BitcP(out, ast->child(i), showTypes);
      }
      out << ")";

      break;
    }
    
  case at_interface:
  case at_lambda:
    {
      out << "(" << ast->atKwd() << " ";
      BitcP(out, ast->child(0), showTypes);
      out.more();

      for(size_t c = 1; c < ast->children.size(); c++) {
	out << std::endl;
	BitcP(out, ast->child(c), showTypes);
      }

      out << ")";
#ifdef SWAROOP_TYPES 
      if (showTypes) print_type(out, ast);
#endif
      break;
    }

  case at_sw_legs:
  case at_constructors:
  case at_reprctrs:
  case at_fields:
  case at_declares:
  case at_method_decls:
    {
      for(size_t c = 0; c < ast->children.size(); c++) {
	if (c > 0) out << std::endl;
	BitcP(out, ast->child(c), showTypes);
      }
      break;
    }

  case at_tvlist:
  case at_tcdecls:
    {
      doChildren(out, ast, 0, false, showTypes);
      break;
    }

  case at_constructor:
  case at_reprctr:
    {
      if (ast->children.size() > 1) {
	out << "(";
	doChildren(out, ast, 0, false, showTypes);
	out << ")";
      }
      else
	BitcP(out, ast->child(0), showTypes);
      break;
    }

  case at_field:
    {
      BitcP(out, ast->child(0), showTypes);
      if (ast->children.size() > 1) {
	out << ":";
	BitcP(out, ast->child(1), showTypes);
      }
      break;
    }
    
  //  case at_vpattern:
  case at_bitfield:  
    {
      out << "(";
      out << ast->atKwd();    
      doChildren(out, ast, 0, true, showTypes);
      out << ")";
      break;
    }

  case at_identPattern: 
    {
      BitcP(out, ast->child(0), showTypes);
      if (ast->children.size() > 1) {
	out << ":";
	BitcP(out, ast->child(1), showTypes);
      }
      break;
    }

  case at_container:
    {
      out << "({";
      BitcP(out, ast->child(0), showTypes);
      out << "} ";
      BitcP(out, ast->child(1), showTypes);
      out << ")";
      break;
    }
  case at_identList:
    {
      doChildren(out, ast, 0, false, showTypes);
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

#ifndef SWAROOP_TYPES 
  if (showTypes) print_type(out, ast);
#endif

  out.setIndent(startIndent);
}

static void 
doShowTypes(std::ostream& out, GCPtr<AST> ast, 
	    GCPtr<Environment<TypeScheme> > gamma,
	    bool showMangName,
	    bool raw = false,
	    GCPtr<TvPrinter> tvP=NULL)
{
  switch(ast->astType) {
  case at_ident:

    // Move this to at_define ... etc, in case 
    // there is a need to see differentiated 
    // tvars in a single definition.

    if (!raw)
      tvP = new TvPrinter;
    
    out << ast->s  << ": "
      	<< ((!ast->scheme)
	    ? "??" 
	    : ast->scheme->asString(tvP, true));

    if(showMangName)
      if(ast->symType->isOfInfiniteType())
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
      if(ast->astType == at_interface) {
	out << "(" << ast->atKwd() << " "
	    << ast->child(0)->s << endl;
	i=1;
      }
      else {
	out << "(" << "source-unit"  << endl;
	i=0;
      }
      
      for(; i<ast->children.size(); i++) {
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
      // 	  << ast->child(0)->s << ": "
      // 	  << ast->child(0)->symType->asString();
      out << "  " << "type-class ";
      doShowTypes(out, ast->child(0), gamma, 
		  showMangName, raw, tvP);
      break;
    }

  case at_definstance:
    {
      if (!raw && (!tvP))
	tvP = new TvPrinter;

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

void
AST::PrettyPrint(std::ostream& strm, bool decorated, 
		 bool endline) const
{
  INOstream out(strm);
  BitcP(out, this, decorated);
  if(endline)
    out << std::endl;
}

void
AST::PrettyPrint(bool decorated) const
{
  INOstream out(std::cerr);
  BitcP(out, this, decorated);
  std::cerr << endl;
}

void
UocInfo::PrettyPrint(std::ostream& out, bool decorated)
{
  assert (uocAst->astType == at_module
	  || uocAst->astType == at_interface);

  uocAst->PrettyPrint(out, decorated);
  out << endl;
}

void
UocInfo::ShowTypes(std::ostream& out)
{
  doShowTypes(out, uocAst, gamma, false);  
}

