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
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/avl.hxx>
#include <assert.h>

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"
#include "Clconv.hxx"
#include "Options.hxx"

using namespace sherpa;

#define NULL_MODE  0x0u
#define LOCAL_MODE 0x2u  // Parameters
#define USE_MODE   0x3u
#define TYPE_MODE  0x4u
 
static void
markRecBound(GCPtr<AST> ast)
{
  for(size_t c=0; c < ast->children->size(); c++)
    markRecBound(ast->child(c));
  if (ast->astType == at_ident)
    ast->Flags2 |= ID_IS_RECBOUND;
}

/**
 * @brief Mark all defining occurrences that are closed over so that
 * we can later rewrite them.
 */
static void
clearusedef(GCPtr<AST> ast)
{
  ast->Flags2 &= ~(ID_IS_DEF|ID_IS_USE|ID_IS_CLOSED|ID_IS_CAPTURED|ID_NEEDS_HEAPIFY|ID_IS_RECBOUND);

  for(size_t c=0; c < ast->children->size(); c++)
    clearusedef(ast->child(c));
}

// See if The identifier `id' is used in the ast `ast'
// id must be a defining form.
static bool
used(GCPtr<AST> id, GCPtr<AST> ast)
{  
  if(ast->astType == at_ident && ast->symbolDef == id) {
    assert(ast != id);
    assert(!id->symbolDef);
    return true;
  }
  
  for(size_t i=0; i < ast->children->size(); i++) 
    if(used(id, ast->child(i)))
      return true;
  
  return false;
}


/**
 * @brief Mark all defining occurrences that are closed over so that
 * we can later rewrite them.
 */
static bool
findusedef(std::ostream &errStream,
	   GCPtr<AST> topAst, GCPtr<AST> ast, const int mode,
	   // list of vars that are bound within the lambda at the
	   // current point:
	   GCPtr<CVector<GCPtr<AST> > > boundVars,
	   // list of vars that lambda uses, but are not in
	   // boundVars. Globals are not entered into this.
	   GCPtr<CVector<GCPtr<AST> > > freeVars)
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
	boundVars->append(ast);
	ast->Flags2 |= ID_IS_DEF;
	break;
	
      case USE_MODE:
	{
	  if (!ast->symbolDef)
	    std::cerr << "Warning: No definition for "
		      << ast->fqn << std::endl;

	  ast->Flags2 |= ID_IS_USE;
	  
	  // could check ast->symbolDef->symType->isMutable()
	  
	  if(ast->symbolDef->isGlobal()) 
	    break;
	  
	  if(boundVars->contains(ast->symbolDef)) 
	    break;
	  
	  if(Options::noAlloc) {
	    errStream << ast->loc << ": "
		      << "Usage of identifier " << ast->s
		      << " mandates closure conversion -- "
		      << "disallowed in noAlloc mode"
		      << std::endl;
	    errFree = false;
	  }

	  ast->Flags2 |= ID_IS_CLOSED;
	  ast->symbolDef->Flags2 |= ID_IS_CAPTURED;

	  if (ast->symbolDef->getType()->needsCaptureConversion()) {
	    ast->Flags2 |= ID_NEEDS_HEAPIFY;
	    ast->symbolDef->Flags2 |= ID_NEEDS_HEAPIFY;
	  }

	  CLCONV_DEBUG std::cerr << "Append " << ast->symbolDef->fqn
				 << " to freeVars" << std::endl;
	  
	  if (!freeVars->contains(ast->symbolDef))
	    freeVars->append(ast->symbolDef);
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
	GCPtr<CVector<GCPtr<AST> > > freeVars = new CVector<GCPtr<AST> >;
	GCPtr<CVector<GCPtr<AST> > > boundVars = new CVector<GCPtr<AST> >;

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
      for(size_t c=0; c < ast->children->size(); c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   NULL_MODE, boundVars, freeVars));
      break;
    }
  case at_methods:
    {
      for(size_t c=0; c < ast->children->size(); c++)
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
      for(size_t c=0; c < ast->children->size(); c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_constructor:
  case at_field:
  case at_defexception:
    {
      for(size_t c=1; c<ast->children->size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   TYPE_MODE, boundVars, freeVars));
      break;
    }

  case at_argVec:
    {
      for(size_t c=0; c<ast->children->size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   LOCAL_MODE, boundVars, freeVars));
      break;
    }

  case at_setbang:
    {
      for(size_t c=0; c<ast->children->size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_begin:
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
      for(size_t c=0; c<ast->children->size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_switch:
  case at_try:
    {
      for(size_t c=0; c<ast->children->size();c++)
	if(c != IGNORE(ast))
	  CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				     USE_MODE, boundVars, freeVars));
      break;
    }

  case at_inner_ref:
    {
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(0), 
				 USE_MODE, boundVars, freeVars));

      if(ast->Flags2 & INNER_REF_NDX) 
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				   USE_MODE, boundVars, freeVars));      
      
      break;
    }

  case at_ucon_apply:
  case at_struct_apply:
    {
      for(size_t c=1; c<ast->children->size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }

  case at_if:
  case at_when:
    {
      for(size_t c=0; c<ast->children->size();c++)
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
      for(size_t c=1; c<ast->children->size();c++)
	CHKERR(errFree, findusedef(errStream, topAst, ast->child(c), 
				   USE_MODE, boundVars, freeVars));
      break;
    }
    
  case at_do:
    {
      GCPtr<AST> dbs = ast->child(0);      
      // Initializers
      for (size_t c = 0; c < dbs->children->size(); c++) {
	GCPtr<AST> db = dbs->child(c);
	CHKERR(errFree, findusedef(errStream, topAst, db->child(1), 
				   USE_MODE, boundVars, freeVars));
      }
      
      // Binding
      for (size_t c = 0; c < dbs->children->size(); c++) {
	GCPtr<AST> db = dbs->child(c);
	CHKERR(errFree, findusedef(errStream, topAst, db->child(0), 
				   LOCAL_MODE, boundVars, freeVars));
      }
      
      //Step-wise update
      for (size_t c = 0; c < dbs->children->size(); c++) {
	GCPtr<AST> db = dbs->child(c);
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
      GCPtr<AST> lbs = ast->child(0);

      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children->size(); c++) {
	GCPtr<AST> lb = lbs->child(c);

	CHKERR(errFree, findusedef(errStream, topAst, lb->child(1), 
				   USE_MODE, boundVars, freeVars));
	CHKERR(errFree, findusedef(errStream, topAst, lb->child(0), 
				   LOCAL_MODE, boundVars, freeVars));
	if (ast->astType == at_letrec)
	  markRecBound(lb->child(0));
      }
      
      CHKERR(errFree, findusedef(errStream, topAst, ast->child(1), 
				 USE_MODE, boundVars, freeVars));
      break;
    }
  }
  
  return errFree;
}

GCPtr<AST> 
cl_rewrite_captured_idents(GCPtr<AST> ast, GCPtr<AST> clenvName)
{
  for (size_t c = 0; c < ast->children->size(); c++)
    ast->child(c) = 
      cl_rewrite_captured_idents(ast->child(c), clenvName);

  switch(ast->astType) {
  case at_ident:
    {
      if (ast->Flags2 & ID_IS_CLOSED) {
	GCPtr<AST> clUse = new AST(at_select, ast->loc);
	clUse->addChild(clenvName->getDCopy());
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


static GCPtr<AST>
getClenvUse(GCPtr<AST> ast, GCPtr<AST> clenvName, 
	    GCPtr<CVector<std::string> > tvs)
{
  GCPtr<AST> clType;
  if(tvs->size()) {
    GCPtr<AST> typeApp = new AST(at_typeapp, ast->loc);
    typeApp->addChild(clenvName->Use());
    
    for(size_t i=0; i<tvs->size(); i++) {
      GCPtr<AST> tv = new AST(at_ident, ast->loc);
      tv->Flags |= ID_IS_TVAR;
      tv->s = tv->fqn.ident = tvs->elem(i);
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
rewriteMyCapture(GCPtr<AST> ast, GCPtr<AST> me, GCPtr<AST> him) 
{
  if(ast->astType == at_set_closure) {
    GCPtr<AST> envApp = ast->child(1);
    for(size_t c=1; c < envApp->children->size(); c++)
      if(envApp->child(c)->symbolDef == me)
	envApp->child(c) = him->Use();
    return;
  }
  
  for(size_t c=0; c < ast->children->size(); c++)
    rewriteMyCapture(ast->child(c), me, him);
} 
#endif


// Walk an AST. If it contains a lambda form that is going 
// to require a closure record, fabricate the closure record 
// and append it to outASTs
GCPtr<AST> 
cl_convert_ast(GCPtr<AST> ast, 
	       GCPtr< CVector<GCPtr<AST> > > outAsts, 
	       bool shouldHoist)
{
  bool hoistChildren = true;

  /* Pre Processing */  
  if(ast->astType == at_define || ast->astType == at_recdef) {
    hoistChildren = false;

    GCPtr<AST> ident = ast->getID();
    if(!ident->decl) {
      GCPtr<AST> proclaim = new AST(at_proclaim, ast->loc,
			      ident->getDCopy(),
			      cl_convert_ast(ident->symType->asAST(ast->loc),
					     outAsts, hoistChildren),
			      new AST(at_constraints, ast->loc));
      
      if(ident->externalName.size())
	proclaim->child(0)->Flags |= DEF_IS_EXTERNAL;
      
      ast->Flags2 |= PROCLAIM_IS_INTERNAL;
      outAsts->append(proclaim);      
    }
  }

  /* Process children (inside-out) */
  for (size_t c = 0; c < ast->children->size(); c++)
    ast->child(c) = 
      cl_convert_ast(ast->child(c), outAsts, hoistChildren);
  
  /* Post Processing */
  switch(ast->astType) {
  case at_letrec:
    {
      GCPtr<AST> lbs = ast->child(0);
      GCPtr<AST> expr = ast->child(1);
      
      // A list of copyclosures to append in the end.
      GCPtr<AST> ccs = new AST(at_begin, expr->loc);

      for(size_t c=0; c < lbs->children->size(); c++) {
	GCPtr<AST> lb = lbs->child(c);
	GCPtr<AST> id = lb->child(0)->child(0);
	
	GCPtr<AST> rhs = lb->child(1);
	
	// If this identifier may be used in *any* let-binding
	// within the current letrec, we must use the alloc-ref /
	// copy-ref scheme for closure conversion.
	if(used(id, lbs)) {
	  assert(id->Flags2 & ID_IS_CAPTURED);
	  GCPtr<AST> qual = 
	    id->symType->getBareType()->asAST(rhs->loc); 
	  qual = cl_convert_ast(qual, outAsts, hoistChildren);
	  GCPtr<AST> ac = new AST(at_allocREF, rhs->loc, qual);
	  GCPtr<AST> cc = new AST(at_copyREF, rhs->loc, id->Use(), rhs);
	  ccs->children->append(cc);
	  lb->child(1) = ac;
	}
      }

      if(expr->astType == at_begin)
	ccs->addChildrenFrom(expr);
      else
	ccs->addChild(expr);
      
      ast->child(1) = ccs;
      //ast->astType = at_let;
      break;
    }

  case at_fn:
    {
      // ast = new AST(at_closureType, ast->loc, ast);
      break;
    }
    
  case at_lambda:
    {
      GCPtr<CVector<GCPtr<AST> > > freeVars = new CVector<GCPtr<AST> >;
      GCPtr<CVector<GCPtr<AST> > > boundVars = new CVector<GCPtr<AST> >;
      
      GCPtr<AST> clenvName = 0;
      GCPtr<TvPrinter> tvP = new TvPrinter;
      GCPtr<CVector<std::string> > tvs = new CVector<std::string>;

      CLCONV_DEBUG std::cerr << "Processing lambda. " << std::endl;

      // Need to re-run this here, because we may have hoisted inner
      // lambdas and/or introduced a closure conversion, which will
      // have introduced new identifiers.
      findusedef(std::cerr, ast, ast, NULL_MODE, boundVars, freeVars);

      // Closure object is only generated if we actually have some
      // free variables.
      bool needsClosure = (freeVars->size() > 0);
      if(needsClosure) {

	CLCONV_DEBUG std::cerr << "Need to generate closure struct. " 
			       << std::endl;

	//////// Build the Environment Structure //////////////
	// defstruct = ident tvlist category declares fields constraints;
	GCPtr<AST> defStruct = new AST(at_defstruct, ast->loc);
      
	// Note defstruct does not use an identPattern
	clenvName = AST::genSym(ast, "clenv");
	clenvName->identType = id_type;
	clenvName->Flags |= (ID_IS_GLOBAL | ID_IS_CTOR);
	defStruct->addChild(clenvName);
      
	// tvList: to be fixed-up later.
	GCPtr<AST> tvlist = new AST(at_tvlist, ast->loc);
	defStruct->addChild(tvlist);      
	// env records are ref types
	defStruct->addChild(new AST(at_refCat));
	// no declares
	defStruct->addChild(new AST(at_declares));
	// Parent AST for fields:
	GCPtr<AST> fields = new AST(at_fields, ast->loc);
	defStruct->addChild(fields);      
	// Add empty constraints subtree
	defStruct->addChild(new AST(at_constraints, ast->loc));
      
	for(size_t fv = 0; fv < freeVars->size(); fv++) {
	  assert(freeVars->elem(fv)->astType == at_ident);
	  GCPtr<AST> field = new AST(at_field, ast->loc);
	  GCPtr<AST> ident = new AST(at_ident, ast->loc);
	  ident->s = ident->fqn.ident = freeVars->elem(fv)->s;
	  
	  field->addChild(ident);
	  GCPtr<AST> fvType = freeVars->elem(fv)->symType->asAST(ast->loc, tvP);
	  fvType = cl_convert_ast(fvType, outAsts, hoistChildren);
	  field->addChild(fvType);
	  
	  fields->addChild(field);
	}
      
	// If the Type of arg contains a type variable,	  
	// then add to the tvlist.
	// This case is impossible if closure conversion happens
	// after polyinstantiation.      
	tvs = tvP->getAllTvarStrings();
	for(size_t i=0; i<tvs->size(); i++) {
	  GCPtr<AST> tv = new AST(at_ident, tvlist->loc);
	  tv->Flags |= ID_IS_TVAR;
	  tv->s = tv->fqn.ident = tvs->elem(i);
	  tvlist->children->append(tv);
	}
      
	// Okay. We have built the type declaration for the closure
	// record. Append it to outAsts
	outAsts->append(defStruct);
      }            

      //////////// Hoist the inner Lambda ///////////////
      if(shouldHoist) {
	CLCONV_DEBUG std::cerr << "Need to hoist this lambda. " 
			       << std::endl;      
	CLCONV_DEBUG ast->PrettyPrint(std::cerr);
	
	// AST define = bindingPattern expr;
	// This can be done as a recdef since we don't allow top-level
	// names to be shadowed.
	GCPtr<AST> newDef = new AST(at_recdef, ast->loc);
      
	GCPtr<AST> lamName = AST::genSym(ast, "lam");
	lamName->identType = id_value;
	lamName->Flags |= ID_IS_GLOBAL;
	
	GCPtr<AST> lamType = ast->symType->asAST(ast->loc);
	// Once we removed closure types, this conversion 
	// operation is redundant. Of historic interest, 
	// placeholder reminder in case we switch back.
	lamType = cl_convert_ast(lamType, outAsts, hoistChildren);
	GCPtr<AST> lamPat = new AST(at_identPattern, ast->loc,
				    lamName, lamType);	
	newDef->addChild(lamPat);
	newDef->addChild(ast);
	newDef->addChild(new AST(at_constraints));
      
	if(needsClosure) {
	  newDef->Flags2 |= LAM_NEEDS_TRANS;

	  // Insert the extra closure argument and prepend the type to
	  // the attached function type signature      
	  GCPtr<AST> argVec = ast->child(0);
	  GCPtr<AST> body = ast->child(1);
	  GCPtr<AST> clArgName = new AST(at_ident, ast->loc);
	  clArgName->s = clArgName->fqn.ident = "__clArg";
	  GCPtr<AST> clArgPat = new AST(at_identPattern, ast->loc, clArgName);      
	  GCPtr<AST> clType = getClenvUse(ast, clenvName, tvs);
	  
	  lamType->child(0)->children->insert(0, clType);
	  clArgPat->addChild(clType->getDCopy());
	  argVec->children->insert(0, clArgPat);
      
	  ast->child(1) = cl_rewrite_captured_idents(body, clArgName);
	}
	
	// We have built the hoisted procedure. Emit that:
	outAsts->append(newDef);
      
	CLCONV_DEBUG ast->PrettyPrint(newDef);      
      
	// If the lambda requires a closure, emit a make-closure, else
	// emit an identifier reference in place of the lambda:
	GCPtr<AST> lamUse = lamName->Use();
	if(freeVars->size() > 0) {	  
	  GCPtr<AST> mkEnv = new AST(at_struct_apply, ast->loc);
	  mkEnv->addChild(clenvName->Use());	  
	  for (size_t fv = 0; fv < freeVars->size(); fv++)
	    mkEnv->addChild(freeVars->elem(fv)->Use());
	
	  GCPtr<AST> mkClo = new AST(at_mkClosure, ast->loc, mkEnv, lamUse);
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
cl_convert(GCPtr<UocInfo> uoc)
{
  GCPtr< CVector<GCPtr<AST> > > outAsts = new CVector<GCPtr<AST> >;

  GCPtr<AST> modOrIf = uoc->uocAst;

  for (size_t c = 0;c < modOrIf->children->size(); c++) {
    GCPtr<AST> child = modOrIf->child(c);
   
    child = cl_convert_ast(child, outAsts, true);
    outAsts->append(child);
  }

  modOrIf->children = outAsts;
}

// Collect all of the at_ident ASTs that are defined in this argument
// binding pattern and are marked as captured.
static void
collectHeapifiedArgs(GCPtr<AST> ast, 
		     GCPtr< CVector<GCPtr<AST> > > capturedArgs)
{
  if (ast->astType == at_ident && (ast->Flags2 & ID_NEEDS_HEAPIFY)) {
    assert (ast->Flags2 & ID_IS_CAPTURED);

    capturedArgs->append(ast);
  }

  for(size_t i=0; i < ast->children->size(); i++)
    collectHeapifiedArgs(ast->child(i), capturedArgs);
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
GCPtr<AST> 
cl_heapify(GCPtr<AST> ast)
{
  switch(ast->astType) {
  case at_lambda:
    {
      // Proceed through the arguments. For each argument that is
      // captured, rewrite the body to be surrounded by a dup'ing LET
      // form.
      GCPtr<AST> args = ast->child(0);
      GCPtr<AST> body = ast->child(1);
      
      GCPtr<AST> bindings = new AST(at_letbindings, body->loc);
      
      // Wrap the existing body in a LET binding:
      body = new AST(at_let, body->loc, bindings, body);
      body->addChild(new AST(at_constraints));
      
      bool atleastOneHeapified = false;
      
      // The RHS is not yet dup'd here. This will happen when this let
      // is processed in the at_letbinding handler.
      for (size_t i = 0; i < args->children->size(); i++) {
	GCPtr<AST> arg = args->child(i)->child(0);
	if((arg->Flags2 & ID_NEEDS_HEAPIFY) == 0)
	  continue;
	
	atleastOneHeapified = true;
	// Exchange the arg to a new Identifier with the same name
	// Te reason for the exchange is that the use cases in the
	// body will be pointing to the *OLD* ident AST.
	GCPtr<AST> newArg = new AST(at_ident, arg->loc);
	newArg->s = newArg->fqn.ident = arg->s;
	args->child(i)->child(0) = newArg;
	
 	// We have moved the point of capture into the let, 
	// which was the point:
	GCPtr<AST> letIdent = arg;
	GCPtr<AST> letExpr = newArg->Use();
	GCPtr<AST> identPattern = new AST(at_identPattern, arg->loc, letIdent);
	
	GCPtr<AST> theBinding = new AST(at_letbinding, arg->loc,
				  identPattern, letExpr); 
	bindings->addChild(theBinding);
      }
      
      if(atleastOneHeapified)
	ast->child(1) = body;
      
      // Process the body.
      ast->child(1) = cl_heapify(ast->child(1));
      break;
    }

  case at_letbinding:
    {
      GCPtr<AST> bpattern = ast->child(0);
      GCPtr<AST> expr = ast->child(1);

      assert(bpattern->astType == at_identPattern);
      GCPtr<AST> ident = bpattern->child(0);

      CLCONV_DEBUG std::cerr << "Let binding for " << ident->s << std::endl;

      /* Must heapify EXPR unconditionally */
      expr = cl_heapify(expr);

      if (ident->Flags2 & ID_NEEDS_HEAPIFY) {
	// Process the RHS:

	CLCONV_DEBUG std::cerr << "  Needs dup " << ident->s << std::endl;
	
	assert (ident->Flags2 & ID_IS_CAPTURED);
	expr = new AST(at_dup, expr->loc, expr);
	expr->Flags2 |= DUPED_BY_CLCONV;
 
	// if the binding pattern was qualified by a type qualification,
	// wrap that in a REF:
	if (bpattern->children->size() == 2)
	  bpattern->child(1) = 
	    new AST(at_refType, bpattern->loc, bpattern->child(1));
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

      GCPtr<AST> def = (ast->symbolDef) ? ast->symbolDef : ast;

      CLCONV_DEBUG if (def->Flags2 & ID_NEEDS_HEAPIFY)
	std::cerr << "  needs heapify" << std::endl;
      CLCONV_DEBUG if (def->Flags2 & ID_IS_CAPTURED)
	std::cerr << "  closed" << std::endl;
      
      if (def->Flags2 & ID_NEEDS_HEAPIFY) {
	CLCONV_DEBUG std::cerr << "Needs deref " << ast->s << std::endl;
	ast = new AST(at_deref, ast->loc, ast);
      }
      break;
    }

  case at_switch:
  case at_try:
    {
      for (size_t c = 0; c < ast->children->size(); c++)
	if(c != IGNORE(ast))
	  ast->child(c) = cl_heapify(ast->child(c));
      break;
    }
    
  default:
    {
      for (size_t c = 0; c < ast->children->size(); c++)
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
  
  GCPtr<CVector<GCPtr<AST> > > freeVars = new CVector<GCPtr<AST> >;
  GCPtr<CVector<GCPtr<AST> > > boundVars = new CVector<GCPtr<AST> >;
  
  CLCONV_DEBUG std::cerr << "findusedef 1" << std::endl;

  CHKERR(errFree, findusedef(errStream, uocAst, uocAst,
			     NULL_MODE, boundVars, freeVars));
  
  if(!errFree)
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
  cl_convert(this);

  CLCONV_DEBUG PrettyPrint(errStream);

  CLCONV_DEBUG std::cerr << "RandT 2" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, 
	 RandT(errStream, true, CL_SYM_FLAGS, CL_TYP_FLAGS));
  
  if(!errFree)
    std::cerr << uocAst->asString();
  assert(errFree);

  return true;
}


