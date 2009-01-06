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
#include <string>

#include "AST.hxx"
#include "Environment.hxx"
#include "inter-pass.hxx"

using namespace boost;
using namespace sherpa;
 
/* 
   Notes:
   1) This pass assumes successful completion of Symbol Resolution. 

   2) This pass deals with Values definitions only.
      That is, it does not deal with 
        - Constructor Visibility
	- Type variable scoping

   3) For top-level definitions, the environment pointer refers to: 
        - The definition if available in scope
        - The declaration if no definition is available in scope.

      Therefore, if a source module defined an implementation for a
      declaration in an interface, 
        - Only the provider for that definition will have a pointer
	  to the defintion
	- The interface and all other importers/providers will have a
  	  pointer to the declaration.

   4) We still need the ID_OBSERV_DEF flag (and cannot just rely on
      whether the env points to decl/def) because in the case of
      recursive definitions, the env of first definition points to
      definition even tough it is not observably completely defined. 

   5) We will mark the definitions ID_OBSERV_DEF only after full
      checking. It is OK to mark it in the AST.
      In the interface/provider case above, the definition will be
      marked ID_OBSERV_DEF in the provider source module, where it is
      a valid initializer. All others will refer to the declaration,
      which will never be marked ID_OBSERV_DEF.

   6) The only way mutually recursive defintions can be ID_OBSERV_DEF
      is by using other mutually recursive definitions within a
      lambda form.

   7) This pass need not assume the incompleteness check if run after
      type checker. 
        - Defintions like:
            (letrec ((a b)
                     (b a)) ... ) 
          will fail due to the observably defined check.
        - Definitions like
            (let rec ((a (lambda () b))
                      (b a)) ... )
          will fail to type check.
*/

enum pop_type {
  pop_none,
  pop_sclar,
  pop_intFl,
  pop_int
};

#define CHOOSE(a, b) do { \
    if (ast->s == #a)	  \
      return b;		  \
  }while (0)

static pop_type
primOp(shared_ptr<AST> ast)
{
  if ((ast->astType != at_ident) || 
     (!ast->symbolDef->isGlobal()))
    return pop_none;

  CHOOSE(==, pop_sclar);
  CHOOSE(!=, pop_sclar);
  CHOOSE(<, pop_sclar);
  CHOOSE(>, pop_sclar);
  CHOOSE(<=, pop_sclar);
  CHOOSE(>=, pop_sclar);
  CHOOSE(+, pop_intFl);
  CHOOSE(-, pop_intFl);
  CHOOSE(*, pop_intFl);
  CHOOSE(/, pop_intFl);
  CHOOSE(%, pop_int);
  return pop_none;
}

//WARNING: **REQUIRES** errStream, errFree.
#define TOPINIT(ast, flags)					\
  CHKERR(errFree, TopInit((errStream), (ast), (flags)));	


bool
TopInit(std::ostream& errStream, 
	shared_ptr<AST> ast, 
	unsigned long flags)
{
  bool errFree = true;

  switch(ast->astType) {
  case at_refCat:
  case at_valCat:
  case at_opaqueCat:
  case agt_category:
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
    //case agt_reprbodyitem:
  case agt_value_definition:
  case agt_CompilationUnit:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_ow:
  case at_localFrame:
  case at_frameBindings:
  case at_identList:
  case at_container:
  case agt_qtype:
  case agt_fielditem:
  case at_defrepr:
    //case at_reprbody:
    //case at_reprcase:
    //case at_reprcaselegR:
    //case at_reprtag:
  case at_reprctrs:
  case at_reprctr:
  case at_reprrepr:
  case at_docString:
  case at_letGather:
  case agt_ucon:

  case at_usesel:
  case at_allocREF:
  case at_mkClosure:
  case at_copyREF:
  case at_setClosure:
  case at_argVec:
    {
      assert(false);
      break;
    }

  case at_Null: 
    // switch contains at_Null of otherwise clause is absent. 
  case at_defunion:
  case at_defstruct:
  case at_declunion:
  case at_declstruct:
  case at_declrepr:
  case at_tvlist:
  case at_constructors:
  case at_constructor:
  case at_fields:
  case at_field:
  case at_fill:
  case at_proclaim:
  case at_defexception:
  case at_deftypeclass:
  case at_tcdecls:
  case at_tyfn:
  case at_tcapp:
  case at_method_decls:
  case at_method_decl:
  case at_definstance:
  case at_tcmethods:
  case at_tcmethod_binding:
  case at_ifident:
  case at_importAs:
  case at_provide:
  case at_import:
  case at_ifsel:
  case at_declares:
  case at_declare:
  case at_suspend:
  case at_bitfield:
  case at_refType:
  case at_valType:
  case at_byrefType:
  case at_fn:
  case at_fnargVec:
  case at_primaryType:
  case at_arrayType:
  case at_vectorType:
  case at_exceptionType:
  case at_dummyType:
  case at_mutableType:
  case at_constType:
  case at_typeapp:
  case at_qualType:
  case at_constraints:
    break;
    
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_unit:
  case at_lambda:
  case at_sizeof:
  case at_bitsizeof:
    break;

  case at_ident:
    {
      // We must only deal with value definitions.
      assert(ast->isIdentType(id_value));
      
      if (ast->isTcMethod())
	break;
      
      shared_ptr<AST> def = ast->symbolDef;
      if (!def) {
	// Defining Occurence.
	// This loop MUST only enter the ident case ONLY for 
	//  definitions that are _observably defined_.
	assert(!ast->isDecl);
	ast->flags |= ID_OBSERV_DEF;
      }
      else {
	// Constructors like `nil'.
	if (def->isUnionLeg())
	  break;

	if ((def->flags & ID_OBSERV_DEF) == 0) {
	  errStream << ast->loc << ": "
		    << "Identifier " << ast->s
		    << " is not observably defined, "
		    << "but is used in an initializing expression"
		    << std::endl;
	  errFree = false;
	}
      }
      break;
    }
    
  case at_interface:
    {
      // match agt_definition*
      for (size_t c = 1; c < ast->children.size(); c++)
	TOPINIT(ast->child(c), flags);

      break;
    }

  case at_module:
    {
      // match agt_definition*
      for (size_t c = 0; c < ast->children.size(); c++)
	TOPINIT(ast->child(c), flags);
      
      break;
    }
    
  case at_define:
  case at_recdef:
    {
      TOPINIT(ast->child(1), flags);      

      if (errFree)
	TOPINIT(ast->child(0), flags);
      
      break;
    }

  case at_fqCtr:
    break;

  case at_identPattern:
  case at_literalType:
  case at_tqexpr:
  case at_select:
  case at_sel_ctr:
    {
      TOPINIT(ast->child(0), flags);
      break;
    }

  case at_array:
  case at_block:
  case at_return_from:
  case at_vector:
  case at_makevectorL:
  case at_begin:
  case at_array_length:
  case at_vector_length:
  case at_array_nth:
  case at_vector_nth:
  case at_if:
  case at_when:
  case at_and:
  case at_or:
  case at_not: 
  case at_throw:
  case at_cond:
  case at_cond_legs:
  case at_cond_leg:
  case at_condelse:
  case at_dup:
  case at_deref:
  case at_inner_ref:
  case at_sw_legs:
  case at_let:
  case at_letrec:    
  case at_letStar:
  case at_letbindings:
  case at_do:
  case at_dotest:
  case at_dobindings:
  case at_dobinding:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	TOPINIT(ast->child(c), flags);
      
      break;
    }

  case at_try:
  case at_switch:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	if (c != IGNORE(ast))
	  TOPINIT(ast->child(c), flags);
      break;
    }
    
  case at_struct_apply:
  case at_ucon_apply: 
    {
      for (size_t c = 1; c < ast->children.size(); c++)
	TOPINIT(ast->child(c), flags);
      
      break;
    }
    
  case at_apply:
    { 
      
      pop_type pop = primOp(ast->child(0));
      switch(pop) {
      case pop_sclar:
	for (size_t i=1; i < ast->children.size(); i++)
	  if (!ast->child(0)->symType->isScalar()) {
	    errFree = false;
	    break;
	  }
	break;

      case pop_intFl:
	for (size_t i=1; i < ast->children.size(); i++)
	  if (!ast->child(0)->symType->isPrimInt() &&
	     !ast->child(0)->symType->isPrimFloat()) {
	    errFree = false;
	    break;
	  }
	break;

      case pop_int:
	for (size_t i=1; i < ast->children.size(); i++)
	  if (!ast->child(0)->symType->isPrimInt()) {
	    errFree = false;
	    break;
	  }
	break;

      case pop_none:
	errFree = false;
	break;
      }

      if (!errFree)
	errStream << ast->loc << ": "
		  << "This kind of application is not "
		  << " a valid Initializer." 
		  << std::endl;
      break;
    }
    
  case at_setbang:
    {
      errStream << ast->loc << ": "
		<< "set! is not a valid Initializer."
		<< std::endl;
      errFree = false;
      break;
    }

  case at_sw_leg:
  case at_otherwise:
    {
      // The identifier that binds the copy is observably defined.
      TOPINIT(ast->child(0), flags);
      TOPINIT(ast->child(1), flags);
    }
    
  case at_letbinding:
    {
      TOPINIT(ast->child(1), flags);
      if (errFree)
	TOPINIT(ast->child(0), flags);
      break;
    }
  }
  return errFree;
}

bool
UocInfo::fe_initCheck(std::ostream& errStream,
		      bool init, unsigned long flags)
{
  return true;

  bool errFree = true;
  CHKERR(errFree, TopInit(errStream, uocAst, flags));
  
  return errFree;
}

 
