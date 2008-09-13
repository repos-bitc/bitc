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

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <set>

#include <libsherpa/UExcept.hxx>

#include "Options.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;

#define NULL_MODE  0x0u
#define LOCAL_MODE 0x2u  // Parameters
#define USE_MODE   0x3u
#define TYPE_MODE  0x4u
 
typedef set<shared_ptr<AST> > AstSet;

/**
 * @brief Mark all defining occurrences that are closed over so that
 * we can later rewrite them.
 */
static void
clearusedef(shared_ptr<AST> ast)
{
  ast->flags &= ~(ID_IS_CLOSED|ID_IS_CAPTURED|ID_NEEDS_HEAPIFY);

  for (size_t c=0; c < ast->children.size(); c++)
    clearusedef(ast->child(c));
}

// See if The identifier `id' is used in the ast `ast'
// id must be a defining form.
static bool
used(shared_ptr<AST> id, shared_ptr<AST> ast)
{  
  if (ast->astType == at_ident && ast->symbolDef == id) {
    assert(ast != id);
    assert(!id->symbolDef);
    return true;
  }
  
  for (size_t i=0; i < ast->children.size(); i++) 
    if (used(id, ast->child(i)))
      return true;
  
  return false;
}


/**
 * @brief Mark all defining occurrences that are closed over so that
 * we can later rewrite them.
 */
static bool
findusedef(std::ostream &errStream,
	   shared_ptr<AST> topAst, shared_ptr<AST> ast, const int mode,
	   // list of vars that are bound within the lambda at the
	   // current point:
	   AstSet& boundVars,
	   // list of vars that lambda uses, but are not in
	   // boundVars. Globals are not entered into this.
	   AstSet& freeVars)
{
  bool errFree = true;
  switch(ast->astType) {
  case at_letGather:
    assert(false);

  case at_Null:
  case at_AnyGroup:
  case agt_literal:
  case agt_tvar:
  case agt_var:
  case agt_definition:
  case agt_type:
  case agt_expr:
  case agt_expr_or_define:
  case agt_eform:
  case agt_type_definition:
  case agt_value_definition:
  case at_letbindings:
  case at_letbinding:
  case at_dobindings:
  case at_dobinding:
  case agt_CompilationUnit:
  case at_ifident:
  case at_localFrame:
  case at_frameBindings:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_category:
  case agt_ow:
  case agt_qtype:
  case agt_fielditem:
  case at_refCat:
  case at_valCat:
  case at_opaqueCat:
  case at_tcdecls:
  case at_tyfn:
  case at_usesel:
  case at_identList:
  case at_container:
  case at_defrepr:
    //case at_reprbody:
    //case agt_reprbodyitem:
    //case at_reprcase:
    //case at_reprcaselegR:
    //case at_reprtag:
  case at_reprctrs:
  case at_reprctr:
  case at_reprrepr:
  case at_unit:
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_bitfield:
  case at_declunion:
  case at_declstruct:
  case at_declrepr:
  case at_importAs:
  case at_provide:
  case at_import:
  case at_ifsel:
  case at_declares:
  case at_declare:
  case at_tvlist:
  case at_docString:
  case agt_ucon:
    break;


  case at_ident:
    {
      //     errStream << ast->loc.asString() 
      //               << "astType = at_ident; mode = " << mode 
      // 	       << std::endl;

      switch(mode) {

      case TYPE_MODE:
	break;
	
      case LOCAL_MODE:
	boundVars.insert(ast);
	break;
	
      case USE_MODE:
	{
	  if (!ast->symbolDef)
	    std::cerr << "Warning: No definition for "
		      << ast->fqn << std::endl;

	  // could check ast->symbolDef->symType->isMutable()
	  
	  if (ast->symbolDef->isGlobal()) 
	    break;
	  
	  if (boundVars.find(ast->symbolDef) != boundVars.end()) 
	    break;
	  
	  if (Options::noAlloc) {
	    errStream << ast->loc << ": "
		      << "Usage of identifier " << ast->s
		      << " mandates closure conversion -- "
		      << "disallowed in noAlloc mode"
		      << std::endl;
	    errFree = false;
	  }

	  ast->flags |= ID_IS_CLOSED;
	  ast->symbolDef->flags |= ID_IS_CAPTURED;

	  if (ast->symbolDef->getType()->needsCaptureConversion()) {
	    ast->flags |= ID_NEEDS_HEAPIFY;
	    ast->symbolDef->flags |= ID_NEEDS_HEAPIFY;
	  }

	  CLCONV_DEBUG std::cerr << "Append " << ast->symbolDef->fqn
				 << " to freeVars" << std::endl;
	  
	  freeVars.insert(ast->symbolDef);
	  break;
	}      
      case NULL_MODE:
      default:
	break;
      }
      break;
    }

  case at_deftypeclass:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(3), 
				 TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_definstance:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 TYPE_MODE, boundVars, freeVars));
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(2), 
				 TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_method_decl: 
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      break;
    }

  case at_defunion:
  case at_defstruct:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(4), 
				 TYPE_MODE, boundVars, freeVars));
      break;
    }
    
  case at_proclaim:
    {      
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 TYPE_MODE, boundVars, freeVars));
      break;
    }
     
  case at_fn: 
    {      
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 TYPE_MODE, boundVars, freeVars));
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_define:
  case at_recdef:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      break;
    }

  case at_identPattern:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 mode, boundVars, freeVars));
      break;
    }

  case at_tqexpr:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 USE_MODE, boundVars, freeVars));
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_suspend:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      break;
    }
    
  case at_fqCtr:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_select:
  case at_sel_ctr:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 USE_MODE, boundVars, freeVars));
      break;
    }

  case at_lambda:
    {
      if (ast == topAst) {
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				   LOCAL_MODE, boundVars, freeVars));
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				   USE_MODE, boundVars, freeVars));
      }
      else {
	AstSet freeVars;
	AstSet boundVars;

	CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				   LOCAL_MODE, boundVars, freeVars));
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				   USE_MODE, boundVars, freeVars));
      }

      break;
    }
    
  case at_interface:
  case at_module:
    {
      for (size_t c=0; c < ast->children.size(); c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   NULL_MODE, boundVars, freeVars));
      break;
    }
  case at_methods:
    {
      for (size_t c=0; c < ast->children.size(); c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_method_decls:
  case at_tcapp:    
  case at_typeapp:
  case at_exceptionType:
  case at_dummyType:
  case at_arrayType:
  case at_vectorType:
  case at_refType:
  case at_byrefType:
  case at_valType:
  case at_primaryType:
  case at_fnargVec:
  case at_mutableType:
  case at_qualType:
  case at_constraints:
  case at_constructors:
  case at_fields:
  case at_fill:
  case at_reserved:
    {
      for (size_t c=0; c < ast->children.size(); c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_constructor:
  case at_field:
  case at_defexception:
    {
      for (size_t c=1; c<ast->children.size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_argVec:
    {
      for (size_t c=0; c<ast->children.size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   LOCAL_MODE, boundVars, freeVars));
      break;
    }

  case at_setbang:
    {
      for (size_t c=0; c<ast->children.size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_begin:
  case at_block:
  case at_return_from:
  case at_allocREF:
  case at_setClosure:
  case at_copyREF:
  case at_mkClosure:
  case at_and:
  case at_or:
  case at_not:
  case at_cond:
  case at_cond_legs:
  case at_dup:
  case at_deref:
  case at_sw_legs:
  case at_otherwise:
  case at_throw:
  case at_array_length:
  case at_vector_length:
  case at_array_nth:
  case at_vector_nth:
  case at_vector:
  case at_array:
  case at_makevectorL:    
  case at_apply:
    {
      for (size_t c=0; c<ast->children.size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_switch:
  case at_try:
    {
      for (size_t c=0; c<ast->children.size();c++)
	if (c != IGNORE(ast))
	  CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				     USE_MODE, boundVars, freeVars));
      break;
    }

  case at_inner_ref:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 USE_MODE, boundVars, freeVars));

      if (ast->flags & INNER_REF_NDX) 
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				   USE_MODE, boundVars, freeVars));      
      
      break;
    }

  case at_ucon_apply:
  case at_struct_apply:
    {
      for (size_t c=1; c<ast->children.size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_if:
  case at_when:
    {
      for (size_t c=0; c<ast->children.size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_cond_leg:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 USE_MODE, boundVars, freeVars));
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      break;
    }
        
  case at_sw_leg:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 LOCAL_MODE, boundVars, freeVars));
      for (size_t c=1; c<ast->children.size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }
    
  case at_do:
    {
      shared_ptr<AST> dbs = ast->child(0);      
      // Initializers
      for (size_t c = 0; c < dbs->children.size(); c++) {
	shared_ptr<AST> db = dbs->child(c);
	CHKERR(errFree, findusedef(errStream, topAst, db->child(1), 
				   USE_MODE, boundVars, freeVars));
      }
      
      // Binding
      for (size_t c = 0; c < dbs->children.size(); c++) {
	shared_ptr<AST> db = dbs->child(c);
	CHKERR(errFree, findusedef(errStream, topAst, db->child(0), 
				   LOCAL_MODE, boundVars, freeVars));
      }
      
      //Step-wise update
      for (size_t c = 0; c < dbs->children.size(); c++) {
	shared_ptr<AST> db = dbs->child(c);
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(2), 
				   USE_MODE, boundVars, freeVars));
      }
      
      // Test
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      // Boody
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(2), 
				 USE_MODE, boundVars, freeVars));
      break;
    }

  case at_dotest:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 USE_MODE, boundVars, freeVars));
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      break;
    }    

  case at_letStar:
  case at_let:
  case at_letrec:
    {
      shared_ptr<AST> lbs = ast->child(0);

      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children.size(); c++) {
	shared_ptr<AST> lb = lbs->child(c);

	CHKERR(errFree, findusedef(errStream, topAst, lb->child(1), 
				   USE_MODE, boundVars, freeVars));
	CHKERR(errFree, findusedef(errStream, topAst, lb->child(0), 
				   LOCAL_MODE, boundVars, freeVars));
      }
      
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      break;
    }
  }
  
  return errFree;
}

shared_ptr<AST> 
cl_rewrite_captured_idents(shared_ptr<AST> ast, shared_ptr<AST> clenvName)
{
  for (size_t c = 0; c < ast->children.size(); c++)
    ast->child(c) = 
      cl_rewrite_captured_idents(ast->child(c), clenvName);

  switch(ast->astType) {
  case at_ident:
    {
      if (ast->flags & ID_IS_CLOSED) {
	shared_ptr<AST> clUse = AST::make(at_select, ast->loc);
	clUse->addChild(clenvName->getDeepCopy());
	clUse->addChild(ast);

	ast = clUse;
      }

      break;
    }
  default:
    break;
  }

  return ast;
}


static shared_ptr<AST>
getClenvUse(shared_ptr<AST> ast, shared_ptr<AST> clenvName, 
	    std::vector<std::string>& tvs)
{
  shared_ptr<AST> clType;
  if (tvs.size()) {
    shared_ptr<AST> typeApp = AST::make(at_typeapp, ast->loc);
    typeApp->addChild(clenvName->Use());
    
    for (size_t i=0; i<tvs.size(); i++) {
      shared_ptr<AST> tv = AST::make(at_ident, ast->loc);
      tv->identType = id_tvar;
      tv->s = tv->fqn.ident = tvs[i];
      typeApp->addChild(tv);
    }
    
    clType = typeApp;
  }
  else
    clType = clenvName->Use();
  
  return clType;
}


#if 0
static void
rewriteMyCapture(shared_ptr<AST> ast, shared_ptr<AST> me, shared_ptr<AST> him) 
{
  if (ast->astType == at_set_closure) {
    shared_ptr<AST> envApp = ast->child(1);
    for (size_t c=1; c < envApp->children.size(); c++)
      if (envApp->child(c)->symbolDef == me)
	envApp->child(c) = him->Use();
    return;
  }
  
  for (size_t c=0; c < ast->children.size(); c++)
    rewriteMyCapture(ast->child(c), me, him);
} 
#endif


// Walk an AST. If it contains a lambda form that is going 
// to require a closure record, fabricate the closure record 
// and append it to outASTs
static shared_ptr<AST> 
cl_convert_ast(shared_ptr<AST> ast, 
	       std::vector<shared_ptr<AST> >& outAsts, 
	       bool shouldHoist)
{
  bool hoistChildren = true;

  /* Pre Processing */  
  if (ast->astType == at_define || ast->astType == at_recdef) {
    hoistChildren = false;

    shared_ptr<AST> ident = ast->getID();
    if (!ident->decl) {
      shared_ptr<AST> proclaim = AST::make(at_proclaim, ast->loc,
			      ident->getDeepCopy(),
			      cl_convert_ast(ident->symType->asAST(ast->loc),
					     outAsts, hoistChildren),
			      AST::make(at_constraints, ast->loc));
      
      if (ident->externalName.size())
	proclaim->child(0)->flags |= DEF_IS_EXTERNAL;
      
      ast->flags |= PROCLAIM_IS_INTERNAL;
      outAsts.push_back(proclaim);      
    }
  }

  /* Process children (inside-out) */
  for (size_t c = 0; c < ast->children.size(); c++)
    ast->child(c) = 
      cl_convert_ast(ast->child(c), outAsts, hoistChildren);
  
  /* Post Processing */
  switch(ast->astType) {
  case at_letrec:
    {
      shared_ptr<AST> lbs = ast->child(0);
      shared_ptr<AST> expr = ast->child(1);
      
      // A list of copyclosures to append in the end.
      shared_ptr<AST> ccs = AST::make(at_begin, expr->loc);

      for (size_t c=0; c < lbs->children.size(); c++) {
	shared_ptr<AST> lb = lbs->child(c);
	shared_ptr<AST> id = lb->child(0)->child(0);
	
	shared_ptr<AST> rhs = lb->child(1);
	
	// If this identifier may be used in *any* let-binding
	// within the current letrec, we must use the alloc-ref /
	// copy-ref scheme for closure conversion.
	if (used(id, lbs)) {
	  assert(id->flags & ID_IS_CAPTURED);
	  shared_ptr<AST> qual = 
	    id->symType->getBareType()->asAST(rhs->loc); 
	  qual = cl_convert_ast(qual, outAsts, hoistChildren);
	  shared_ptr<AST> ac = AST::make(at_allocREF, rhs->loc, qual);
	  shared_ptr<AST> cc = AST::make(at_copyREF, rhs->loc, id->Use(), rhs);
	  ccs->children.push_back(cc);
	  lb->child(1) = ac;
	}
      }

      if (expr->astType == at_begin)
	ccs->addChildrenFrom(expr);
      else
	ccs->addChild(expr);
      
      ast->child(1) = ccs;
      //ast->astType = at_let;
      break;
    }

  case at_fn:
    {
      // ast = AST::make(at_closureType, ast->loc, ast);
      break;
    }
    
  case at_lambda:
    {
      AstSet freeVars;
      AstSet boundVars;
      
      shared_ptr<AST> clenvName = GC_NULL;
      shared_ptr<TvPrinter> tvP = TvPrinter::make();

      CLCONV_DEBUG std::cerr << "Processing lambda. " << std::endl;

      // Need to re-run this here, because we may have hoisted inner
      // lambdas and/or introduced a closure conversion, which will
      // have introduced new identifiers.
      findusedef(std::cerr, ast, ast, NULL_MODE, boundVars, freeVars);

      std::vector<std::string> tvs;

      // Closure object is only generated if we actually have some
      // free variables.
      bool needsClosure = !freeVars.empty();
      if (needsClosure) {

	CLCONV_DEBUG std::cerr << "Need to generate closure struct. " 
			       << std::endl;

	//////// Build the Environment Structure //////////////
	// defstruct = ident tvlist category declares fields constraints;
	shared_ptr<AST> defStruct = AST::make(at_defstruct, ast->loc);
      
	// Note defstruct does not use an identPattern
	clenvName = AST::genSym(ast, "clenv");
	clenvName->identType = id_struct;
	clenvName->flags |= (ID_IS_GLOBAL);
	defStruct->addChild(clenvName);
      
	// tvList: to be fixed-up later.
	shared_ptr<AST> tvlist = AST::make(at_tvlist, ast->loc);
	defStruct->addChild(tvlist);      
	// env records are ref types
	defStruct->addChild(AST::make(at_refCat));
	// no declares
	defStruct->addChild(AST::make(at_declares));
	// Parent AST for fields:
	shared_ptr<AST> fields = AST::make(at_fields, ast->loc);
	defStruct->addChild(fields);      
	// Add empty constraints subtree
	defStruct->addChild(AST::make(at_constraints, ast->loc));
      
	for (AstSet::iterator fv = freeVars.begin();
	    fv != freeVars.end(); ++fv) {
	  assert((*fv)->astType == at_ident);
	  shared_ptr<AST> field = AST::make(at_field, ast->loc);
	  shared_ptr<AST> ident = AST::make(at_ident, ast->loc);
	  ident->s = ident->fqn.ident = (*fv)->s;
	  
	  field->addChild(ident);
	  shared_ptr<AST> fvType = (*fv)->symType->asAST(ast->loc, tvP);
	  fvType = cl_convert_ast(fvType, outAsts, hoistChildren);
	  field->addChild(fvType);
	  
	  fields->addChild(field);
	}
      
	// If the Type of arg contains a type variable,	  
	// then add to the tvlist.
	// No type variables can appear if closure conversion happens
	// after polyinstantiation.      
	tvs = tvP->getAllTvarStrings();

	for (size_t i=0; i < tvs.size(); i++) {
	  shared_ptr<AST> tv = AST::make(at_ident, tvlist->loc);
	  tv->identType = id_tvar;
	  tv->s = tv->fqn.ident = tvs[i];
	  tvlist->children.push_back(tv);
	}
      
	// Okay. We have built the type declaration for the closure
	// record. Append it to outAsts
	outAsts.push_back(defStruct);
      }            

      //////////// Hoist the inner Lambda ///////////////
      if (shouldHoist) {
	CLCONV_DEBUG std::cerr << "Need to hoist this lambda. " 
			       << std::endl;      
	CLCONV_DEBUG ast->PrettyPrint(std::cerr);
	
	// AST define = bindingPattern expr;
	// This can be done as a recdef since we don't allow top-level
	// names to be shadowed.
	shared_ptr<AST> newDef = AST::make(at_recdef, ast->loc);
      
	shared_ptr<AST> lamName = AST::genSym(ast, "lam");
	lamName->identType = id_value;
	lamName->flags |= ID_IS_GLOBAL;
	
	shared_ptr<AST> lamType = ast->symType->asAST(ast->loc);
	// Once we removed closure types, this conversion 
	// operation is redundant. Of historic interest, 
	// placeholder reminder in case we switch back.
	lamType = cl_convert_ast(lamType, outAsts, hoistChildren);
	shared_ptr<AST> lamPat = AST::make(at_identPattern, ast->loc,
				    lamName, lamType);	
	newDef->addChild(lamPat);
	newDef->addChild(ast);
	newDef->addChild(AST::make(at_constraints));
      
	if (needsClosure) {
	  newDef->flags |= LAM_NEEDS_TRANS;

	  // Insert the extra closure argument and prepend the type to
	  // the attached function type signature      
	  shared_ptr<AST> argVec = ast->child(0);
	  shared_ptr<AST> body = ast->child(1);
	  shared_ptr<AST> clArgName = AST::make(at_ident, ast->loc);
	  clArgName->s = clArgName->fqn.ident = "__clArg";
	  shared_ptr<AST> clArgPat = AST::make(at_identPattern, ast->loc, clArgName);      
	  // Note: tvs was populated in the if (needsClosure) above
	  // if (shouldHoist).
	  shared_ptr<AST> clType = getClenvUse(ast, clenvName, tvs);
	  
	  lamType->child(0)->children.insert(lamType->child(0)->children.begin(),
					     clType);
	  clArgPat->addChild(clType->getDeepCopy());
	  argVec->children.insert(argVec->children.begin(), clArgPat);
      
	  ast->child(1) = cl_rewrite_captured_idents(body, clArgName);
	}
	
	// We have built the hoisted procedure. Emit that:
	outAsts.push_back(newDef);
      
	CLCONV_DEBUG ast->PrettyPrint(newDef);      
      
	// If the lambda requires a closure, emit a make-closure, else
	// emit an identifier reference in place of the lambda:
	shared_ptr<AST> lamUse = lamName->Use();
	if (freeVars.size()) {	  
	  shared_ptr<AST> mkEnv = AST::make(at_struct_apply, ast->loc);
	  mkEnv->addChild(clenvName->Use());	  
	  for (AstSet::iterator fv = freeVars.begin();
	       fv != freeVars.end(); ++fv)
	    mkEnv->addChild((*fv)->Use());
	
	  shared_ptr<AST> mkClo = AST::make(at_mkClosure, ast->loc, mkEnv, lamUse);
	  ast = mkClo;
	}
	else {
	  ast = lamUse;
	}
      }
      break;
    }
    
  default:
    {
      break;
    }
  }
  
  return ast;
}

void
cl_convert(shared_ptr<UocInfo> uoc)
{
  std::vector<shared_ptr<AST> > outAsts;

  shared_ptr<AST> modOrIf = uoc->uocAst;

  for (size_t c = 0;c < modOrIf->children.size(); c++) {
    shared_ptr<AST> child = modOrIf->child(c);
   
    child = cl_convert_ast(child, outAsts, true);
    outAsts.push_back(child);
  }

  modOrIf->children = outAsts;
}

// Simple re-writing pass. Takes all of the identifiers that were 
// identfied above as being closed over and re-writes them in such a
// way as to push them into the heap. In the following examples, the
// '*' after the bound identifier indicates that it is captured. The
// rewrites that we need to do on the defining occurrences are:
//
//  (let[rec] ((a* e-init) ...) <body>)
//  => (let[ref] ((a (dup e-init))) <body>)
//
//  (switch id* e <cases>) => (switch id (dup e) <cases>)
//
//  (lambda (a* ...) <body>)
//   => (lambda (a ...)
//         (let ((a (dup a)))
//            <body>))
//
// And then in the use-occurrences we simply need (at this stage) to
// wrap the use-occurrences with (__clmember id)
//
shared_ptr<AST> 
cl_heapify(shared_ptr<AST> ast)
{
  switch(ast->astType) {
  case at_lambda:
    {
      // Proceed through the arguments. For each argument that is
      // captured, rewrite the body to be surrounded by a dup'ing LET
      // form.
      shared_ptr<AST> args = ast->child(0);
      shared_ptr<AST> body = ast->child(1);
      
      shared_ptr<AST> bindings = AST::make(at_letbindings, body->loc);
      
      // Wrap the existing body in a LET binding:
      body = AST::make(at_let, body->loc, bindings, body);
      body->addChild(AST::make(at_constraints));
      
      bool atleastOneHeapified = false;
      
      // The RHS is not yet dup'd here. This will happen when this let
      // is processed in the at_letbinding handler.
      for (size_t i = 0; i < args->children.size(); i++) {
	shared_ptr<AST> arg = args->child(i)->child(0);
	if ((arg->flags & ID_NEEDS_HEAPIFY) == 0)
	  continue;
	
	atleastOneHeapified = true;
	// Exchange the arg to a new Identifier with the same name
	// Te reason for the exchange is that the use cases in the
	// body will be pointing to the *OLD* ident AST.
	shared_ptr<AST> newArg = AST::make(at_ident, arg->loc);
	newArg->s = newArg->fqn.ident = arg->s;
	args->child(i)->child(0) = newArg;
	
 	// We have moved the point of capture into the let, 
	// which was the point:
	shared_ptr<AST> letIdent = arg;
	shared_ptr<AST> letExpr = newArg->Use();
	shared_ptr<AST> identPattern = AST::make(at_identPattern, arg->loc, letIdent);
	
	shared_ptr<AST> theBinding = AST::make(at_letbinding, arg->loc,
				  identPattern, letExpr); 
	bindings->addChild(theBinding);
      }
      
      if (atleastOneHeapified)
	ast->child(1) = body;
      
      // Process the body.
      ast->child(1) = cl_heapify(ast->child(1));
      break;
    }

  case at_letbinding:
    {
      shared_ptr<AST> bpattern = ast->child(0);
      shared_ptr<AST> expr = ast->child(1);

      assert(bpattern->astType == at_identPattern);
      shared_ptr<AST> ident = bpattern->child(0);

      CLCONV_DEBUG std::cerr << "Let binding for " << ident->s << std::endl;

      /* Must heapify EXPR unconditionally */
      expr = cl_heapify(expr);

      if (ident->flags & ID_NEEDS_HEAPIFY) {
	// Process the RHS:

	CLCONV_DEBUG std::cerr << "  Needs dup " << ident->s << std::endl;
	
	assert (ident->flags & ID_IS_CAPTURED);
	expr = AST::make(at_dup, expr->loc, expr);
	// Previously: Clconv was marking Dup-ed variables specially 
	// at this point expr->flags2 |= DUPED_BY_CLCONV;
 
	// if the binding pattern was qualified by a type qualification,
	// wrap that in a REF:
	if (bpattern->children.size() == 2)
	  bpattern->child(1) = 
	    AST::make(at_refType, bpattern->loc, bpattern->child(1));
      }

      ast->child(1) = expr;
      break;
    }

  case at_ident:
    {
      // If this is a use occurrence of a heapified symbol, rewrite it
      // into a DEREF reference.

      CLCONV_DEBUG std::cerr << "Processing " << ast->loc << ": " 
			  << ast->s << std::endl;
      /* Swaroop: ID_NEEDS_HEAPIFY must always be checked on the
	 symbolDef because all use cases may not be corerectly
	 marked. For example see:
	 (define PQR (let ((p #t))
	      (set! p #f)          
	      (lambda () p))) 
	      
	 Thhe first use case within the set! will not be heapified if
	 this is not done.  */

      shared_ptr<AST> def = (ast->symbolDef) ? ast->symbolDef : ast;

      CLCONV_DEBUG if (def->flags & ID_NEEDS_HEAPIFY)
	std::cerr << "  needs heapify" << std::endl;
      CLCONV_DEBUG if (def->flags & ID_IS_CAPTURED)
	std::cerr << "  closed" << std::endl;
      
      if (def->flags & ID_NEEDS_HEAPIFY) {
	CLCONV_DEBUG std::cerr << "Needs deref " << ast->s << std::endl;
	ast = AST::make(at_deref, ast->loc, ast);
      }
      break;
    }

  case at_switch:
  case at_try:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	if (c != IGNORE(ast))
	  ast->child(c) = cl_heapify(ast->child(c));
      break;
    }
    
  default:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	ast->child(c) = cl_heapify(ast->child(c));
      break;
    }
  }

  return ast;
}

bool
UocInfo::be_clconv(std::ostream& errStream,
		   bool init, unsigned long flags)
{ 
  bool errFree = true;
  
  AstSet freeVars;
  AstSet boundVars;
  
  CLCONV_DEBUG std::cerr << "findusedef 1" << std::endl;

  CHKERR(errFree, findusedef(errStream, uocAst, uocAst,
			     NULL_MODE, boundVars, freeVars));
  
  if (!errFree)
    return false;

  CLCONV_DEBUG PrettyPrint(errStream, true);

  CLCONV_DEBUG std::cerr << "cl_heapify" << std::endl;
  uocAst = cl_heapify(uocAst);

  CLCONV_DEBUG PrettyPrint(errStream, true);

  CLCONV_DEBUG std::cerr << "RandT 1" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, 
	 RandT(errStream, true, REF_SYM_FLAGS, REF_TYP_FLAGS));
  assert(errFree);

  CLCONV_DEBUG std::cerr << "findusedef 2" << std::endl;

  // This *shouldn't* be necessary, but it doesn't hurt anything.
  clearusedef(uocAst);
  findusedef(errStream, uocAst, uocAst, NULL_MODE,
	     boundVars, freeVars);

  CLCONV_DEBUG PrettyPrint(errStream);

  CLCONV_DEBUG std::cerr << "cl_convert" << std::endl;
  cl_convert(shared_from_this());

  CLCONV_DEBUG PrettyPrint(errStream);

  CLCONV_DEBUG std::cerr << "RandT 2" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, 
	 RandT(errStream, true, CL_SYM_FLAGS, CL_TYP_FLAGS));
  
  if (!errFree)
    std::cerr << uocAst->asString();
  assert(errFree);

  return true;
}


