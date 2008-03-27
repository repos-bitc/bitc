/**************************************************************************
 *
 * Copyright (C) 2006, Johns Hopkins University.
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
#include "AST.hxx"
#include "Type.hxx"
#include "backend.hxx"
#include "Symtab.hxx"
#include "Unify.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

#define NULL_MODE 0x0u
#define LOCAL_MODE 0x2u  // Parameters
#define USE_MODE 0x3u



static void
refSub(AST *ast, AST *from, AST *parent, const size_t chno)
{
  switch(ast->astType) {
  case at_ident:
    {
      if(ast->symbolDef == from) {
	AST *deref = new AST(at_deref, ast->loc, ast);
	parent->children[chno] = deref;
      }
      break;
    }
    
  default:
    {
      for(size_t i=0; i<ast->children.size(); i++)
	refSub(ast->children[i], from, ast, i);
    }
  }
}



//WARNING: **REQUIRES** answer and errFree.
#define REFIZE(a, b, c, d) do{					\
    answer = refize((a), (b), (c), (d));			\
    if(answer == false)						\
      errFree = false;						\
  }while(0)

  
bool
refize(std::ostream& errStream,
       AST *ast, AST *parent, 
       const size_t chno)
{
  size_t c;
  bool errFree = true, answer = true;
  switch(ast->astType) {
  case at_Null:
  case at_AnyGroup:
  case at_version:
  case agt_literal:
  case agt_tvar:
  case agt_var:
  case agt_definition:
  case agt_type:
  case agt_bindingPattern:
  case agt_valuePattern:
  case agt_expr:
  case agt_eform:
  case agt_type_definition:
  case agt_value_definition:
  case agt_CompilationUnit:
  case at_ifident:
  case at_ifsel:
  case at_localFrame:
  case at_frameBindings:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_category:
  case agt_tcdecl:
  case agt_ow:
  case at_refCat:
  case at_valCat:
  case at_opaqueCat:
  case at_tcdecls:
  case at_tyfn:
  case at_super:
  case at_tcreqs:
  case at_tcreq:
  case at_method_decls:
  case at_method_decl: 
  case at_usesel:
  case at_use_case:
  case at_identList:
  case at_container:
    {
      errStream << ast->loc.asString() << "Internal Compiler Error. " 
	   << "Function refize, unexpected astType: " 
	   << ast->astTypeName()
	   << std::endl;
      
      errFree = false;
      break;
    } 

  case at_unit:
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_bitfield:
  case at_ident:
  case at_defexception:
  case at_deftypeclass:
  case at_definstance:
  case at_declunionr:
  case at_declstructr:
  case at_defstruct:
  case at_defunion:
  case at_declValue:
  case at_use:
  case at_import:
  case at_stateful_import:
  case at_provide:
  case at_stateful_provide:
  case at_declares:
  case at_declare:
  case at_tvlist:
  case at_constructors:
  case at_constructor:
  case at_fields:
  case at_field:
  case at_fn:
  case at_arrayType:
  case at_vectorType:
  case at_typeapp:
  case at_refType:
  case at_valType:
  case at_primaryType:
  case at_pairType:
  case at_fnargVec:
  case at_mutableType:
  case at_methodType:
  case at_identPattern:
  case at_literalPattern:
  case at_unitPattern:
  case at_pairPattern:
  case at_applyPattern:    
  case at_argVec:
  case at_letbindings:
  case at_letbinding:
    {
      break;
    }

  case at_start:
    {
      // match at_module/at_interface
      REFIZE(errStream, ast->children[1], ast, 1);
      break;
    }

  case at_interface:
  case at_module:
    {
      // match agt_definition*    
      c = (ast->astType == at_module)?0:1;
      while (c < ast->children.size()) {
	REFIZE(errStream, ast->children[c], ast, c);
	c++;
      }
      break;
    }

  case at_define:
    {
      // match agt_expr
      REFIZE(errStream, ast->children[1], ast, 1);
      break;
    }

  case at_lambda:
    {      
      // AST *argVec = ast->children[0];
      AST *body = ast->children[1];
      REFIZE(errStream, body, ast, 1);      
      
#if 0
      CVector<AST *> *argIds = new CVector<AST *>;
      for(size_t i=0; i<argVec->children.size(); i++)
	argVec->children[i]->getIds(errStream, argIds, true);
      
      for(size_t i=0; i<argIds->size(); i++) {	
      	AST *argPat = (*argIds)[i];
      	AST *arg = argPat->children[0];
	AST *argtyp = argPat->children[1];
      	if(!arg->symType->isRefType()) {
	  if(argtyp != NULL) {
	    AST *refType = new AST(at_refType, argtyp->loc, argtyp);
	    argPat->children[1] = refType;
	  }
	  AST *deref = new AST(at_deref, arg->loc, arg);	  
	  refSub(body, arg, ast, 1);
      	}
      }
#endif      
      break;
    }

  case at_apply:
    {      
      AST *fn = ast->children[0];
      // Not handling need to do some work
      REFIZE(errStream, fn, ast, 0);
      
      for(size_t c = 1; c < ast->children.size(); c++) {
	AST *arg = ast->children[c];
	REFIZE(errStream, arg, ast, c);
#if 0
	// Fix this if needed, dup is now a keyword
	if(arg->symType->getType()->kind != ty_ref ||
	   fn->symType->getType()->kind == ty_tvar) {
	  AST *dupIdent = new AST(at_ident, arg->loc);	
	  // This needs to use a non-maskable name ex "#dup"
	  // which can be bound in the prelude, in addition to dup
	  dupIdent->s = depIdent->fqn.ident = "dup";
	  AST *dupedArg = new AST(at_apply, arg->loc, 
					dupIdent, arg);
	  ast->children[c] = dupedArg;
	}
#endif	
      }      
      
      break;
    }
    

  case at_tqexpr:
  case at_begin:
  case at_do:
  case at_dotest:
  case at_select:  
  case at_ucon_apply:
  case at_struct_apply:
  case at_if:
  case at_and:
  case at_or:
  case at_cond:
  case at_cond_legs:
  case at_deref:
  case at_dup:
  case at_case:
  case at_case_legs:
  case at_otherwise:
  case at_try:
  case at_throw:
  case at_array_length:
  case at_vector_length:
  case at_array_nth:
  case at_vector_nth:
  case at_vector:
  case at_array:
  case at_pair:
  case at_makevector:    
  case at_cond_leg:
  case at_setbang:
  case at_case_leg:
    {
      // match agt_expr+
      c = 0;
      while (c < ast->children.size()) {
	REFIZE(errStream, ast->children[c], ast, c);
	c++;
      }      
      break;
    }

  case at_letStar:
    {
      assert(false);
      break;
    }

  case at_let:
  case at_letrec:
    {
      AST *lbs = ast->children[0];
     
      //  errStream << "Before: " << ast->asString() << std::endl;
      
      // This assumes that letSimp pass has been done
      for (size_t c = 0; c < lbs->children.size(); c++) {
	AST *lb = lbs->children[c];
	AST *ip = lb->children[0];
	AST *expr = lb->children[1];
	REFIZE(errStream, expr, lb, 1);
	AST *id = ip->children[0];
	
	if(!id->symType->isRefType()) {
	  id->identFlags |= ID_IS_REFIZED;
	  assert(false);
	  // Fix this if needed, dup is now a keyword
	  AST *dupIdent = new AST(at_ident, expr->loc);
	  dupIdent->s = dupIdent->fqn.ident = "dup";
	  AST *dup = new AST(at_apply, expr->loc, 
				   dupIdent, expr);
	  lb->children[1] = dup;	
	  
	  // Fix the type qualifier if there is one
	  AST *tq = ip->children[1];
	  if(tq != NULL) {
	    AST *refType = new AST(at_refType, tq->loc, tq);
	    ip->children[1] = refType;
	  }
	}
      }
      
      REFIZE(errStream, ast->children[1], ast, 1);
      
      if(ast->astType == at_letrec)
	for (size_t c = 0; c < lbs->children.size(); c++) {
	  AST *lbc = lbs->children[c];	  
	  AST *idc = lbc->children[0]->children[0];
	  if(idc->identFlags & ID_IS_REFIZED) {
	    for (size_t d = 1; d < lbs->children.size(); d++) {
	      AST *lbd = lbs->children[d];
	      AST *exprd = lbd->children[1];
	      refSub(exprd, idc, lbd, 1);
	    }
	  }
	}

      for (size_t c = 0; c < lbs->children.size(); c++) {
	AST *lb = lbs->children[c];
	 AST *id = lb->children[0]->children[0];
	if(id->identFlags & ID_IS_REFIZED)
	  refSub(ast->children[1], id, ast, 1);
      }
      
      //  errStream << "After:  " << ast->asString() << std::endl;
      break;
    }
  }
  return errFree;
}

bool
UocInfo::do_refize(std::ostream& errStream, 
		   bool init, unsigned long flags)
{
  // This pass not needed under bootstrap-assumptions.
  return true; 

  bool errFree = refize(errStream, ast, NULL, 0);
  errFree = RandT(errStream, this, true,
		  NO_RESOLVE_DECL, DEF_DECL_NO_MATCH);
  return errFree;
}

