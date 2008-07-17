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
#include "AST.hxx"
#include "Type.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

#define NULL_MODE 0x0u
#define LOCAL_MODE 0x2u  // Parameters
#define USE_MODE 0x3u

//WARNING: **REQUIRES** answer and errFree.
#define HOIST(a, b, c, d, e, f, g) do{			\
    answer = hoist((a), (b), (c), (d), (e), (f), (g));	\
    if(answer == false)					\
      errFree = false;					\
  }while(0)


struct hstruct{
  size_t toppos;
  AST *ident;
  AST *lambda;

  hstruct(size_t _toppos, AST *_ident, AST *_lambda)
  {
    toppos = _toppos;
    ident = _ident;
    lambda = _lambda;
  }
};
  
bool
hoist(std::ostream& errStream,
      AST *ast, 
      CVector<hstruct *> *hs,
      const size_t toppos,
      AST *parent, 
      const size_t chno,
      const bool hoistme)
{
  bool errFree = true, answer = true;
  switch(ast->astType) {
  case at_Null:
    break;

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
  case at_tcapp:
  case at_usesel:
  case at_use_case:
  case at_identList:
  case at_container:
    {
      errStream << ast->loc.asString() << "Internal Compiler Error. " 
	   << "Function hoist, unexpected astType: " 
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
  case at_declunion:
  case at_declstruct:
  case at_declrepr:
  case at_defstruct:
  case at_defunion:
  case at_defrepr:
  case at_reprbody:
  case at_reprcase:
  case at_reprcaselegR:
  case at_reprtag:
  case agt_reprbodyitem:
  case at_declValue:
  case at_use:
  case at_import:
  case at_provide:
  case at_declares:
  case at_declare:
  case at_tvlist:
  case at_constructors:
  case at_constructor:
  case at_fields:
  case at_field:
  case at_fill:
  case at_fn:
  case at_arrayType:
  case at_vectorType:
  case at_typeapp:
  case at_refType:
  case at_exceptionType:
  case at_valType:
  case at_primaryType:
  case at_pairType:
  case at_fnargVec:
  case at_mutableType:
  case at_identPattern:
  case at_literalPattern:
  case at_unitPattern:
  case at_pairPattern:
  case at_applyPattern:    
  case at_argVec:
  case at_qualType:
  case at_constraints:
    {
      break;
    }

  case at_start:
    {
      // match at_module/at_interface
      HOIST(errStream, ast->children[0], hs, 0, ast, 1, true);
      break;
    }

  case at_interface:
  case at_module:
    {
      // match agt_definition*    
      for(size_t c = (ast->astType == at_module)?0:1; 
	  c < ast->children.size(); c++)
	HOIST(errStream, ast->children[c], hs, c, ast, c, true);      
      break;
    }

  case at_define:
    {
      // match agt_expr
      if(ast->children[0]->astType == at_identPattern &&
	 (ast->children[1]->astType == at_lambda))
	HOIST(errStream, ast->children[1], hs, toppos, ast, 1, false);
      else
	HOIST(errStream, ast->children[1], hs, toppos, ast, 1, true);
      break;
    }

  case at_definstance:
    {
      HOIST(errStream, ast->children[1], hs, toppos, ast, 1, true);
      break;
    }

  case at_method_decls:
  case at_methods:
    {
      for(size_t c = 0; c < ast->children.size(); c++)
	HOIST(errStream, ast->children[c], hs, toppos, ast, c, true);
	
      break;
    }
  case at_method_decl: 
    {
      HOIST(errStream, ast->children[1], hs, toppos, ast, 1, true);       
      break;
    }

  case at_lambda:
    {
      AST *id = 0;
      if(hoistme) {
	assert(false);
	AST *lam = ast;
 	id = AST::genSym(ast, "lam");
	id->identType = id_value;
	hs->append(new hstruct(toppos, id, lam));
      }
      HOIST(errStream, ast->children[1], hs, toppos, ast, 1, true);           

      // id is used here directly, trulyHoist makes copies of the `id' AST.
      if(hoistme)
	parent->children[chno] = id;

      break;
    }

  case at_tqexpr:
  case at_suspend:
  case at_begin:
  case at_select:  
  case at_apply:
  case at_ucon_apply:
  case at_struct_apply:
  case at_if:
  case at_and:
  case at_or:
  case at_cond:
  case at_cond_legs:
  case at_dup:
  case at_deref:
  case at_switchR:
  case at_sw_legs:
  case at_otherwise:
  case at_tryR:
  case at_throw:
  case at_array_length:
  case at_vector_length:
  case at_array_nth:
  case at_vector_nth:
  case at_vector:
  case at_array:
  case at_pair:
  case at_mkclosure:
  case at_makevector:    
  case at_cond_leg:
  case at_setbang:
  case at_sw_leg:
  case at_let:
  case at_letrec:
  case at_letStar:
  case at_letbindings:
  case at_letbinding:
  case at_do:
  case at_dotest:
    {
      // match agt_expr+
      for(size_t c = 0; c < ast->children.size(); c++)
	HOIST(errStream, ast->children[c], hs, toppos, ast, c, hoistme);
      break;
    }
  }
  return errFree;
}

bool
trulyHoist(std::ostream& errStream, UocInfo *uoc,
	   AST *ast,
	   CVector<hstruct *> *hs)
{
  // Just to be sure, verify hs-vector is sorted.
  if(hs->size() > 0)
    for(size_t i=0; i < (hs->size() - 1); i++) {    
      hstruct *hhs1 = (*hs)[i];
      hstruct *hhs2 = (*hs)[i+1];
      if(!(hhs1->toppos <= hhs2->toppos)) {
	errStream << hhs1->lambda->loc << ": Internal Compiler Error."
		  << " In function do_hoist, " 
		  << hhs1->toppos << " > " << hhs2->toppos << std::endl;
	return false;
      }
    }

#if 0
  for(size_t i=0; i < hs->size(); i++) {
    hstruct *hhs = (*hs)[i];
    errStream << hhs->toppos << "  " << hhs->ident->s << ": "
	      << hhs->lambda->asString()
	      << std::endl;    
  }
#endif
  
  size_t insertion_count = 0;
  AST *comp = ast->children[0];
  assert((comp->astType == at_module) || (comp->astType == at_interface));
  for(size_t i=0; i < hs->size(); /*empty*/) {
    size_t ipos = i;
    
    hstruct *hhs = (*hs)[i];    
    size_t toppos = hhs->toppos;    
    do {
      size_t pos = hhs->toppos + insertion_count;
      AST *topAst = comp->children[pos];
      
      AST *id = hhs->ident;
      AST *idUse = id->getDCopy();
      idUse->Flags |= ID_IS_GLOBAL;
      
      AST *proclaim = new AST(at_declValue, topAst->loc,
				    idUse, 
				    hhs->lambda->symType->asAST(topAst->loc));
      proclaim->addChild(new AST(at_constraints));
      comp->children.insert(pos, proclaim);
      insertion_count++; 
      i++;      
      hhs = (*hs)[i];
    }while((i < hs->size()) && (toppos == hhs->toppos));
    
    
    i = ipos;    
    hhs = (*hs)[i];    
    toppos = hhs->toppos;    
    do {
      size_t pos = hhs->toppos + insertion_count + 1; // Past the definition.
      AST *topAst = comp->children[pos-1];
      
      AST *id = hhs->ident;
      AST *idUse = id->getDCopy();
      idUse->Flags |= ID_IS_GLOBAL;
      
      AST *fun = new AST(at_define,  topAst->loc, 
			       new AST(at_identPattern, topAst->loc, idUse),
			       hhs->lambda);
      fun->addChild(new AST(at_constraints));

      comp->children.insert(pos, fun);
      insertion_count++;
      i++;
      hhs = (*hs)[i];
    }while((i < hs->size()) && (toppos == hhs->toppos));
  }
  return true;
}

bool
UocInfo::fe_hoist(std::ostream& errStream, 
		  bool init, unsigned long flags)
{
  CVector<hstruct *> *hs = new CVector<hstruct *>;
  bool errFree = true;
  
  CHKERR(errFree, hoist(errStream, ast, hs, 0, NULL, 0, true));
  
  if(!errFree)
    return false;
  
  CHKERR(errFree, trulyHoist(errStream, this, ast, hs));

  //CHKERR(errFree, markTail(errStream, this, ast, hs));
  
  //errStream << "Hoisted AST = " << ast->asString() << std::endl;
  CHKERR(errFree, RandT(errStream, this, true, 
			PP_SYM_FLAGS, PP_TYP_FLAGS));

  return errFree;
}

