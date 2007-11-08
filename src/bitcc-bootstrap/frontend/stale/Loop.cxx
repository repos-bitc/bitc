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
#include <string>
#include <sstream>
#include <gmp.h>
#include <errno.h>
#include "Version.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Environment.hxx"
#include "Symtab.hxx"
#include "inter-pass.hxx"
#include "inter-pass.hxx"
#include "backend.hxx"
#include "Type.hxx"
#include <sstream>

using namespace sherpa;
using namespace std;

#define LOOPOK(errStream, ast, loops, noLVuse)                  \
do{								\
  answer = loopOK((errStream), (ast), (loops), (noLVuse));	\
  if(answer == false)						\
    errFree = false;						\
 }while(0)

bool
loopOK(std::ostream &errStream, GCPtr<AST> ast, 
       CVector< GCPtr<AST> > &loops, bool noLVuse)
{
  bool errFree = true, answer = false;

  //errStream << "LoopChk: " << ast->asString() << std::endl;
  
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
  case at_letbindings:
  case at_letbinding:
  case agt_CompilationUnit:
  case at_ifident:
  case at_ifsel:
  case at_localFrame:
  case at_frameBindings:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_category:
  case agt_tcdecl:
  case agt_use_cases:
  case agt_sometcreqs:
  case agt_ow:
  case at_irefCat:
  case at_refCat:
  case at_valCat:
  case at_opaqueCat:
  case at_tcdecls:
  case at_tyfn:
  case at_super:
  case at_tcreqs:
  case at_itcreqs:
  case at_tcreq:
  case at_method_decls:
  case at_method_decl: 
  case at_usesel:
  case at_use_case:
  case at_iuse_case:
  case at_identList:
  case at_container:
    {
      errStream << ast->loc.asString() << "Internal Compiler Error. " 
	   << "Function loopOK, unexpected astType: " 
	   << ast->astTypeName()
	   << std::endl;
      
      errFree = false;
      break;
    } 
    
  case at_ident:
    {
      // Should this be checked?
      if(loops.size() && ast->symbolDef != NULL) {
	for(size_t i=0; i < loops.size() - 1; i++)
 	  if(ast->symbolDef == loops[i]) {
	    errStream << ast->loc << ": Usage of loop variable " 
		      << ast->s << " violates nesting."
		      << " i = " << i
		      << " ## " << loops[i]->asString() << " ## " << ast->asString()
		      << std::endl;
	    errFree = false;
	    break;
	  }
	
	if(ast->symbolDef == loops[loops.size()-1]) {
	  if(noLVuse) {
	    errStream << ast->loc << ": Loop variable " << ast->s 
		      << " is either escaping, or is at a non-tail position." 
		      << std::endl;
	    errFree = false;
	  }
	  else {
	    ast->markFlags(LOOP_APP);
	  }
	}
      }
      break;
    }
    


  case at_unit:
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_bitfield:
  case at_deftypeclass:
  case at_definstance:
  case at_declunionr:
  case at_declstructr:
  case at_use:
  case at_import:
  case at_stateful_import:
  case at_provide:
  case at_stateful_provide:
  case at_declares:
  case at_declare:
  case at_tvlist:
  case at_methodType:
  case at_typeapp:
  case at_arrayType:
  case at_vectorType:
  case at_refType:
  case at_valType:
  case at_primaryType:
  case at_pairType:
  case at_cpairType:
  case at_fnargVec:
  case at_mutableType: 
  case at_defunion:
  case at_defstruct:
  case at_constructors:
  case at_fields:
  case at_constructor:
  case at_field:
  case at_defexception:
  case at_declValue:
  case at_fn: 
  case at_identPattern:
  case at_literalPattern:
  case at_unitPattern:
  case at_pairPattern:
  case at_cpairPattern:
  case at_applyPattern:
  case at_argVec:
    {
      break;
    }
    
  case at_start:
    {
      // match at_module/at_interface
      LOOPOK(errStream, ast->children[1], loops, noLVuse);
      break;
    }

  case at_interface:
  case at_module:
    {
      // match agt_definition*    
      size_t c = (ast->astType == at_module)?0:1;
      for(; c < ast->children.size(); c++)
	LOOPOK(errStream, ast->children[c], loops, noLVuse);
      break;
    }


  case at_define:
    {
      LOOPOK(errStream, ast->children[1], loops, noLVuse);
      break;
    }

  case at_tqexpr:
    {
      LOOPOK(errStream, ast->children[0], loops, noLVuse);
      break;
    }
    
  case at_ibegin:
  case at_begin:
    {
      // match agt_expr+
      for(size_t c = 0; c < ast->children.size() -1; c++)
	LOOPOK(errStream, ast->children[c], loops, true);
      LOOPOK(errStream, ast->children[ast->children.size() -1], 
	     loops, noLVuse);
      break;
    }
    
  case at_select:
    {
      // match agt_expr
      LOOPOK(errStream, ast->children[0], loops, true);
      break; 
    }

  case at_lambda:
  case at_ilambda:
    {
      LOOPOK(errStream, ast->children[1], loops, true);      
      break;
    }
    
  case at_apply:
    {
      LOOPOK(errStream, ast->children[0], loops, noLVuse);
      for(size_t c = 1; c < ast->children.size(); c++)
	LOOPOK(errStream, ast->children[c], loops, true);
      break;
    }
    
  case at_ucon_apply:
  case at_struct_apply:
    {
      for(size_t c = 1; c < ast->children.size(); c++)
	LOOPOK(errStream, ast->children[c], loops, true);
      break;
    }

  case at_if:
    {
      // match agt_expr
      LOOPOK(errStream, ast->children[0], loops, true);
      LOOPOK(errStream, ast->children[1], loops, noLVuse);
      LOOPOK(errStream, ast->children[2], loops, noLVuse);
      break;
    }
    
  case at_and:
  case at_or:
  case at_deref:
  case at_array_length:
  case at_vector_length:
  case at_array_nth:
  case at_vector_nth:
  case at_vector:
  case at_array:
  case at_pair:
  case at_cpair:
  case at_makevector:    
  case at_setbang:
    {
      for(size_t c = 0; c < ast->children.size(); c++)
	LOOPOK(errStream, ast->children[c], loops, true);
      break;
    }
    
  case at_cond:
  case at_cond_legs:
  case at_case:
  case at_case_legs:
  case at_otherwise:
  case at_try:
  case at_throw:
    {
      for(size_t c = 0; c < ast->children.size(); c++)
	LOOPOK(errStream, ast->children[c], loops, noLVuse);
      break;
    }

  case at_cond_leg:
    {
      LOOPOK(errStream, ast->children[0], loops, true);
      LOOPOK(errStream, ast->children[1], loops, noLVuse);
      break;
    }

  case at_case_leg:
    {
      LOOPOK(errStream, ast->children[1], loops, noLVuse);
      break;
    }
    
  case at_let:
  case at_letrec:
  case at_letStar:
    {
      GCPtr<AST> lbs = ast->children[0];
      for(size_t c = 0; c < lbs->children.size(); c++) {
	GCPtr<AST> lb = lbs->children[c];
	LOOPOK(errStream, lb->children[1], loops, true);
      }
      
      LOOPOK(errStream, ast->children[1], loops, noLVuse);
      break;
    }
    
  case at_loop:
    {
      loops.append(ast->children[0]);
      GCPtr<AST> lbs = ast->children[1];
      for(size_t c = 0; c < lbs->children.size(); c++) {
	GCPtr<AST> lb = lbs->children[c];
	LOOPOK(errStream, lb->children[1], loops, true);
      }
      
      LOOPOK(errStream, ast->children[2], loops, false);
      loops.remove(loops.size() - 1);
      break;
    }
  }
  return errFree; 
}

bool
UocInfo::do_loopchk(std::ostream& errStream,
		    bool init, unsigned long flags)
{
    bool errFree = true;
    CVector< GCPtr<AST> > loops;
    errFree = loopOK(errStream, ast, loops, false);
    return errFree;
}
