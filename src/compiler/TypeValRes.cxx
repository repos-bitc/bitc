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
#include <sstream>

#include <libsherpa/UExcept.hxx>

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "Unify.hxx"

using namespace boost;
using namespace sherpa;
using namespace std;


#define CHKEXP(itsExpansive, exp) do {\
bool ans = (exp);\
if (ans == true) \
 (itsExpansive) = true; \
}while (0)
	   
// FIX: I am not clear why an identifier is intrinsically considered
// expansive, since many identifiers are bound to compile-time deeply 
// immutable values constants and these can be viewed as term
// substitutions. Handling them in this way would let many of the
// special cases below collapse.
// Swaroop: Identifiers are intrinsically NOT expansive. This code
// only detects applications as expansive. I think I am missing your
// point.

bool
isExpansive(std::ostream& errStream, 
	    shared_ptr<const TSEnvironment > gamma,
	    shared_ptr<const AST> ast) 
{
  bool itsExpansive = false;
  
  switch (ast->astType) {
  case at_intLiteral:
  case at_floatLiteral:
  case at_boolLiteral:
  case at_charLiteral:
  case at_stringLiteral:
  case at_lambda:
  case at_sizeof:
  case at_bitsizeof:
    {
      itsExpansive = false;
      break;
    }

  case at_apply:
    {
      itsExpansive = true;
      break;
    }
  case at_setbang:
    {
      itsExpansive = true;
      break;
    }

  case at_allocREF:
  case at_mkClosure:
  case at_ident:
  case at_fill:
  case at_usesel:
  case at_dup:
  case at_docString:
    {
      itsExpansive = false;
      break;
    }

  case at_letStar:
  case at_let:
  case at_letrec:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(0)));
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(1)));
      break;
    }

  case at_copyREF:
  case at_setClosure:
  case at_letbindings:
  case at_dobindings:
    {
      for (size_t i=0; !itsExpansive && i < ast->children.size(); i++)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(i)));
      break;
    }

  case at_letbinding:
    {
      itsExpansive = isExpansive(errStream, gamma,
				 ast->child(1));
      break;
    }

  case at_dobinding:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(1)));
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(2)));
      break;
    }

  case at_try:
  case at_throw:
  case at_block:
  case at_return_from:
#if 0
    {
      itsExpansive = true;
      break;
    }
#endif
  case at_do:
  case at_dotest:
  case at_begin:
    {
      for (size_t i=0; !itsExpansive && i < ast->children.size(); i++)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(i)));
      break;
    }

  case at_suspend:
    {
      itsExpansive = isExpansive(errStream, gamma,
 				 ast->child(1));
      break;
    }

  case at_tqexpr:
    {
      itsExpansive = isExpansive(errStream, gamma,
 				 ast->child(0));
      break;
    }

  case at_if:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(0)));
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(1)));
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(2)));
      break;
    }

  case at_when:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(0)));
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(1)));
      break;
    }

  case at_cond:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(0)));
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(1)));
      break;
    }

  case at_cond_legs:
    {
      for (size_t i=0; !itsExpansive && i < ast->children.size(); i++)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(i)));
      break;
    }

  case at_cond_leg:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(1)));
      break;
    }
    
  case at_condelse:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(0)));
      break;
    }

  case at_and:
  case at_or:
  case at_not:
    {
      itsExpansive = false;
      break;
    }

  case at_switch:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(2)));
      if (ast->child(3)->astType != at_Null)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(3)));
      break;
    }

  case at_sw_legs:
    {
      for (size_t i=0; !itsExpansive && i < ast->children.size(); i++)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(i)));
      break;
    }

  case at_otherwise:
  case at_sw_leg:
    {
      // expr is at the same position
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(1)));
      break;
    }
    
  case at_unit:
    break;

  case at_makevectorL:	     
    {
      /* Make vector takes a lambda as the second argument, but
	 implicitely performs application on it possibly 
	 multiple times */ 
      itsExpansive = true;	
      break;
    }
    
  case at_letGather:
  case at_vector:
  case at_array:
    {
      for (size_t i=0; !itsExpansive && i < ast->children.size(); i++)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(i)));
      break;
    }
    
  case at_fqCtr:
    {
      itsExpansive = false;
      break;
    }

  case at_select:
  case at_sel_ctr:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(0)));      
      break;
    }

  case at_array_length:
  case at_vector_length:
  case at_array_nth:
  case at_vector_nth:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       ast->child(0)));
      break;
    }

  case at_inner_ref:
  case at_deref:
    {    
      for (size_t i=0; !itsExpansive && i < ast->children.size(); i++)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(i)));
      break;
    }

  case at_struct_apply:
  case at_ucon_apply:
    {
      for (size_t i=1; !itsExpansive && i < ast->children.size(); i++)
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(i)));
      break;
    }
   
  case at_container:
    {
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 ast->child(1)));
	break;
    }
    
  case at_tcapp:
    {
      // Special case to facilitate generalization 
      // of instance declarations
      itsExpansive = false;
      break;
    }
    
  case at_refCat:
  case at_valCat:
  case at_opaqueCat:
  case agt_category:
  case at_module:
  case at_define:
  case at_recdef:
  case at_defunion:
  case at_declunion:
  case at_defrepr:
  case at_defstruct:
  case at_declstruct:
  case at_declrepr:
  case at_declares:
  case at_declare:
  case at_tvlist:
  case at_constructors:
  case at_constructor:
  case at_fields:
  case at_field:
  case at_bitfield:
  case at_byrefType:
  case at_refType:
  case at_exceptionType:
  case at_dummyType:
  case at_valType:
  case at_fn:
  case at_primaryType:
  case at_arrayType:
  case at_vectorType:
  case at_mutableType:
  case at_constType:
  case at_typeapp:
  case at_qualType:
  case at_constraints:
  case at_identPattern:
  case at_Null:
  case at_AnyGroup:
  case agt_literal:
  case agt_tvar:
  case agt_var:
  case agt_definition:
  case agt_type_definition:
  case agt_value_definition:
  case agt_type:
    //case at_reprbody:
    //case at_reprcase:
    //case at_reprcaselegR:
    //case at_reprtag:
    //case agt_reprbodyitem:
  case at_reprctrs:
  case at_reprctr:
  case at_reprrepr:
  case agt_expr:
  case agt_expr_or_define:
  case agt_eform:
  case at_proclaim:
  case at_interface:
  case at_importAs:
  case at_provide:
  case at_import:
  case at_ifsel:
  case agt_CompilationUnit:
  case at_defexception:
  case at_deftypeclass:
  case at_tcdecls:
  case at_tyfn:
  case at_method_decls:
  case at_method_decl:
  case at_definstance:
  case at_tcmethods:
  case at_tcmethod_binding:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_ow:
  case agt_qtype:
  case agt_fielditem:
  case at_ifident:
  case at_argVec:
  case at_fnargVec:
  case at_localFrame:
  case at_frameBindings:
  case at_identList:
  case agt_ucon:
    {
      errStream << ast->loc << ": "
		<< "Internal Compiler Error." 
		<< "Unexpected ast-type " << ast->astTypeName()
		<< " obtained by isExpansive() routine."
		<< std::endl;
      itsExpansive = true; // be conservative ...
      break;
    }
  }
  return itsExpansive;
}


// Is this AST a Syntactic value? 
// Permitted cases are literals, functions, value constructors that
// take no arguments, and the special case of array-length.
// This function must only be passed an expresssion AST.
// It returns false (rather than asserting false) if an unexpected AST
// like type-AST, group AST or at_Null is passed.
bool
isAValue(shared_ptr<const AST> ast) 
{
  switch (ast->astType) {
  case at_unit:
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_lambda:
  case at_sizeof:
  case at_bitsizeof:
  case at_mkClosure:
  case at_ident:
  case at_usesel:
    return true;
    
  case at_tqexpr:
    return isAValue(ast->child(0));
    
  case at_array_length:
    return isAValue(ast->child(0));
    
  default:
    return false;
  }
}

bool
isExpansive(std::ostream& errStream, 
	    shared_ptr<const TSEnvironment > gamma,
	    shared_ptr<Type> typ) 
{
  bool itsExpansive = false;
  shared_ptr<Type> t = typ->getType();
  
  if (t->mark & MARK_PREDICATE)
    return itsExpansive;

  t->mark |= MARK_PREDICATE;

  switch(t->kind) {
  case ty_unit:
  case ty_bool:
  case ty_char:
  case ty_string:
  case ty_int8:
  case ty_int16:
  case ty_int32:
  case ty_int64:
  case ty_uint8:
  case ty_uint16:
  case ty_uint32:
  case ty_uint64:
  case ty_word:
  case ty_float:
  case ty_double:
  case ty_quad:

  case ty_tvar:
  case ty_dummy:
  case ty_kvar:
  case ty_kfix:
    
#ifdef KEEP_BF
  case ty_bitfield:
#endif
  case ty_fn:
  case ty_typeclass:
    break;

  case ty_tyfn:
  case ty_fnarg:
  case ty_byref:
    assert(false); // Function case breaks
    break;

  case ty_letGather:
  case ty_array:
  case ty_vector:
    for (size_t i=0; i<t->components.size(); i++) 
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       t->CompType(i)));
    break;

  case ty_structv:
  case ty_structr:
  case ty_unionv: 
  case ty_unionr:
  case ty_uconv:
  case ty_uconr:
  case ty_uvalv:
  case ty_uvalr:
  case ty_mbFull:
  case ty_mbTop:
  case ty_pcst:
  case ty_ref:
  case ty_exn:
    {    
      for (size_t i=0; i<t->typeArgs.size(); i++) 
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 t->TypeArg(i)));
    
      for (size_t i=0; i<t->components.size(); i++) 
	CHKEXP(itsExpansive, isExpansive(errStream, gamma,
					 t->CompType(i)));
      break;
    }

  case ty_const:
    {
      CHKEXP(itsExpansive, isExpansive(errStream, gamma,
				       t->Base()->minimizeMutability()));
      break;
    }
    
  case ty_mutable:
    {
      itsExpansive = true;
      break;
    }
  }
  
  t->mark &= ~MARK_PREDICATE;
  return itsExpansive;
}
