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
#include <string>
#include "AST.hxx"
#include "Environment.hxx"
#include "Symtab.hxx"
#include "Unify.hxx"
#include "inter-pass.hxx"
#include "Special.hxx"
#include "Options.hxx"
#include "Instantiate.hxx"

using namespace sherpa;

static inline GCPtr<AST> 
newGrandLet(GCPtr<AST> ref)
{
  GCPtr<AST> lbs = new AST(at_letbindings, ref->loc);
  GCPtr<AST> res = new AST(at_ident, ref->loc);
  res->s = res->fqn.ident = "NULL";
  GCPtr<AST> grandLet = new AST(at_letStar, ref->loc, lbs, res);
  return grandLet;
}


static inline GCPtr<AST> 
getLastLB(GCPtr<AST> grandLet)
{
  GCPtr<AST> lbs = grandLet->child(0);
  return lbs->child(lbs->children->size() - 1);
}

static void
addIL(GCPtr<AST> identList, GCPtr<AST> id)
{
  identList->children->append(id);
}


// This is the use case, but input might be a definition,
// or another use. Non-ident input is returned as is.
static inline GCPtr<AST> 
UseCase(GCPtr<AST> ast)
{
  if(ast->astType == at_ident) {
    if(!ast->symbolDef)
      return ast->Use();
    else
      return ast->getTrueCopy();  
  }
  else
    return ast;
}

static GCPtr<AST> 
addLB(GCPtr<AST> grandLet, GCPtr<AST> identList, 
      GCPtr<AST> ast, unsigned long lbFlags=0,
      GCPtr<AST> id=NULL, bool addToIL=true)
{
  if(!id)
    id = AST::genSym(ast, "t");
  id->symType = ast->symType;
  GCPtr<AST> ip = new AST(at_identPattern, id->loc, id);
  GCPtr<AST> lb = new AST(at_letbinding, ip->loc, ip, ast);
  lb->Flags |= (LB_IS_ART | lbFlags);
  grandLet->child(0)->children->append(lb);
  if(addToIL)
    addIL(identList, id);  
  return UseCase(id);
}

#if 0
static bool
warnTmp(std::ostream &errStream, GCPtr<AST> ast)
{
  switch(ast->astType) {
  case at_ident:
  case at_select:
  case at_array_nth:
  case at_vector_nth:
  case at_deref:
    // AST_IS_LOCATION
    return true;
    
  case at_tqexpr:
    return warnTmp(errStream, ast->child(0));
    
  default:
    if(!ast->symType->isRefType()) {
      errStream << ast->loc << ": WARNING:"
		<< " expression causing a temporary value copy"
		<< " appears in location context"
		<< "Type of expression is: "
		<< ast->symType->asString()
		<< std::endl;
      return false;
    }
    else {
      return true;
    }
  }
}
#endif

#define SETGL(exp, gl)			   \
  do {					   \
    if((gl)->child(0)->children->size()) \
      (exp) = (gl);			   \
    else				   \
      (exp) = FEXPR(gl);		   \
  }while(0)
 
//WARNING: **REQUIRES** answer and errorFree.
#define SSA(errStream, uoc, ast, grandLet, identList,	\
	       parent, chno,  flags)				\
  do {								\
    answer = ssa((errStream), (uoc), (ast), (grandLet),	\
		    (identList), (parent), (chno), (flags));	\
    if(answer == false)						\
      errorFree = false;					\
  }while(0)


bool
isTrivialInit(GCPtr<AST> ast)
{
  switch(ast->astType) {
    
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_unit:
  case at_lambda:
    {
      return true;
    }

  case at_ident:
    {
      return ast->symType->isAtomic();
    }

  case at_tqexpr:
    {
      return isTrivialInit(ast->child(0));
    }
    
  case at_begin:
    {
      for(size_t c = 0; c < ast->children->size(); c++)
	if(!isTrivialInit(ast->child(c)))
	  return false;
      return true;
    }

  default:
    return false;
  }
}


bool
ssa(std::ostream& errStream, 
    GCPtr<UocInfo> uoc,
    GCPtr<AST> ast, 
    GCPtr<AST> grandLet,
    GCPtr<AST> identList,
    GCPtr<AST> parent, 
    const size_t chno,
    unsigned long flags)
{
  bool errorFree = true, answer = false;
  size_t c = 0;
  
  //errStream << "SSA: " << ast->asString();

  switch(ast->astType) {

  case at_Null:
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
  case agt_value_definition:
  case at_letbindings:
  case at_letbinding:
  case at_dobindings:
  case at_dobinding:
  case at_dotest:
  case agt_CompilationUnit:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_ow:
  case agt_qtype:
  case agt_fielditem:
  case at_localFrame:
  case at_frameBindings:
  case at_use_case:
  case at_ifident:
  case at_declares:
  case at_declare:
  case at_tvlist:
  case at_constructors:
  case at_constructor:
  case at_fields:
  case at_field:
  case at_fill:
  case at_reserved:
  case at_tcdecls:
  case at_tyfn:
  case at_tcapp:
  case at_method_decls:
  case at_method_decl:
  case at_usesel:
  case at_identList:
  case at_container:
  case agt_ucon:
    
  case at_exceptionType:
  case at_dummyType:
  case at_refType:
  case at_valType:
  case at_byrefType:
  case at_fn:
  case at_fnargVec:
  case at_primaryType:
  case at_arrayType:
  case at_vectorType:
  case at_mutableType:
  case at_typeapp:
  case at_bitfield:
  case at_qualType:    
  case at_constraints:
    
  case at_cond_legs:
  case at_cond_leg:
  case at_sw_legs:
  case at_sw_leg:
  case at_otherwise:
  case at_letGather:
    {
      errStream << ast->loc << "Internal Compiler Error. " 
		<< "Function SSA, unexpected astType: " 
		<< ast->astTypeName()
		<< std::endl;
      
      FEXPR(grandLet) = NULL;
      errorFree = false;
      break;
    }

  case at_identPattern:
    {
      if((ast->child(0)->symbolDef) &&
	 (ast->child(0)->symbolDef->Flags & ID_IS_CTOR))
	break;

      addIL(identList, ast->child(0));
      break;
    }
    
  case at_interface:
  case at_module:
    {            
      // match agt_definition*
      for (c = (ast->astType == at_interface)?1:0;
	   c < ast->children->size(); 
	   c++) {
	GCPtr<AST> defn = ast->child(c);
	SSA(errStream, uoc, defn, grandLet, 
	       identList, ast, c, flags);
      }
      break;
    }
    
  case at_defunion:
  case at_defstruct:
  case at_declunion:
  case at_declstruct:
  case at_declrepr:
  case at_proclaim:
  case at_defexception:
  case at_deftypeclass:
  case at_definstance:
  case at_methods:
  case at_use:
  case at_import:
  case at_provide:
  case at_from:
  case at_ifsel:
  case at_defrepr:
    //case at_reprbody:
    //case at_reprcase:
    //case at_reprcaselegR:
    //case at_reprtag:
    //case agt_reprbodyitem:
  case at_reprctrs:
  case at_reprctr:
  case at_reprrepr:
  case at_docString:
    {
      break;
    }
    
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_unit:
    {
      FEXPR(grandLet) = ast;
      break;
    }

  case at_ident:
    {
      //       if((ast->symbolDef == NULL) && 
      // 	 (!identList->contains(ast)))
      // 	identList->append(ast);
      FEXPR(grandLet) = ast;
      break;
    }
    
  case at_define:
    {
      if(ast->child(1)->astType == at_lambda) {
	SSA(errStream, uoc, ast->child(1), grandLet, identList, 
	       ast, 1, flags);
	ast->Flags2 |= DEF_IS_TRIVIAL_INIT;	
      }      
      else if(isTrivialInit(ast)) {
	ast->Flags2 |= DEF_IS_TRIVIAL_INIT;
      }
      else {
	GCPtr<AST> gl = newGrandLet(ast);
	identList = new AST(at_identList, ast->loc);
	
	SSA(errStream, uoc, ast->child(1), gl, identList, 
	    ast, 1, flags);
	
	GCPtr<AST> body=0;
	SETGL(body, gl);
	assert(body);
	ast->child(1) = new AST(at_container, ast->loc, identList, body);	
      }
      break;
    }

  case at_lambda:
    {
      GCPtr<AST> gl = newGrandLet(ast);
      identList = new AST(at_identList, ast->loc);
      
      SSA(errStream, uoc, ast->child(0), gl, identList, 
	     ast, 0, flags);
      SSA(errStream, uoc, ast->child(1), gl, identList, 
	     ast, 1, flags);
      
      GCPtr<AST> body = 0;
      SETGL(body, gl);
      assert(body);
      ast->child(1) = new AST(at_container, ast->loc, identList, body);
	
      // FEXPR carry over
      break;
    }

  case at_argVec:
    {
      break;
    }

  case at_tqexpr:
    {
      // match agt_eform
      SSA(errStream, uoc, ast->child(0), grandLet, identList, 
	     ast, 0, flags);      
      ast->child(0) = FEXPR(grandLet);
      FEXPR(grandLet) = ast;
      // No need to add a new Binding
      break;
    }

  case at_suspend:
    {
      GCPtr<AST> gl = newGrandLet(ast);
      GCPtr<AST> res = AST::genSym(ast, "t");
      SSA(errStream, uoc, ast->child(1), gl, identList, 
	  ast, 1, flags);
      FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 
			1, res, true);

      SETGL(ast->child(1), gl);
      GCPtr<AST> topres = UseCase(res);
      FEXPR(grandLet) = addLB(grandLet, identList, ast, 
			      LB_IS_DUMMY, topres, false);
      break;
    }
    
  case at_allocREF:
    {
      FEXPR(grandLet) = ast;
      break;
    }
    
  case at_copyREF:
  case at_setClosure:
    {      
      for(c=0; c < ast->children->size(); c++) {
	SSA(errStream, uoc, ast->child(c), grandLet, identList, 
	    ast, c, flags);
	ast->child(c) = FEXPR(grandLet);
      }
      FEXPR(grandLet) = addLB(grandLet, identList, ast,
			      LB_IS_DUMMY);
      break;
    }
    
  case at_array_length:
  case at_vector_length:
  case at_struct_apply:
  case at_ucon_apply: 
    {
      for(c=0; c < ast->children->size(); c++) {
	SSA(errStream, uoc, ast->child(c), grandLet, identList, 
	    ast, c, flags);
	ast->child(c) = FEXPR(grandLet);
      }
      FEXPR(grandLet) = addLB(grandLet, identList, ast);
      break;
    }

  case at_dup:
  case at_array:
  case at_vector:
  case at_makevectorL:
  case at_throw:
  case at_mkClosure:
    {
      for(c=0; c < ast->children->size(); c++) {
	SSA(errStream, uoc, ast->child(c), grandLet, identList, 
	    ast, c, flags);
 	ast->child(c) = FEXPR(grandLet);
      }
      FEXPR(grandLet) = addLB(grandLet, identList, ast, 
			      LB_POSTPONED);
      break;
    }
    
  case at_fqCtr:
    {
      FEXPR(grandLet) = ast;
      break;      
    }

  case at_sel_ctr:
    {
      SSA(errStream, uoc, ast->child(0), grandLet, identList, 
	  ast, 0, flags);      
      ast->child(0) = FEXPR(grandLet);
      
      FEXPR(grandLet) = addLB(grandLet, identList, ast);	
      break;      
    }

  case at_deref:
  case at_select:
    {
      SSA(errStream, uoc, ast->child(0), grandLet, identList, 
	  ast, 0, flags);      
      ast->child(0) = FEXPR(grandLet);
      FEXPR(grandLet) = ast;
      break;      
    }

  case at_inner_ref:
    {
      GCPtr<AST> expr = ast->child(0);
      
      SSA(errStream, uoc, expr, grandLet, identList, 
	  ast, 0, flags);      
      ast->child(0) = FEXPR(grandLet);

      if(ast->Flags2 & INNER_REF_NDX) {
	GCPtr<AST> ndx = ast->child(1);
	SSA(errStream, uoc, ndx, grandLet, identList, 
	    ast, 1, flags);      
	ast->child(1) = FEXPR(grandLet);
	
	// Need to Check Index bounds here, 
	// Get the array_nth or vector_nth case to do it
	// Final replacement of FEXPR(grandLet) will
	// change the resultant value to 
	// inner-ref.
	
	GCPtr<AST> tempAst = NULL;
	if(expr->symType->getBareType()->kind == ty_vector) {
	  // Vector-Index
	  tempAst = new AST(at_vector_nth, expr->loc, 
			    expr, ndx);
	  
	}
	else {
	  // ref(Array)-Index
	  assert(expr->symType->getBareType()->kind == ty_ref);
	  tempAst = new AST(at_array_nth, expr->loc, 
			    new AST(at_deref, expr->loc, expr), ndx);	  
	}
	
	// Careful: tempAst has no real parent. 
	// Fortunately, arrray_nth and vector_nth does not need this
	// information, and all other sub-trees will recieve correct
	// information from that case. Actually, there will be no
	// further cases as the sub-expression have already been
	// SSAed in the previous steps. 
	SSA(errStream, uoc, tempAst, grandLet, identList, 
	    NULL, 0, flags);      
	
      }

      FEXPR(grandLet) = ast;      
      break;      
    }

  case at_array_nth:
  case at_vector_nth:
    {
      GCPtr<AST> expr = ast->child(0);
      GCPtr<AST> ndx = ast->child(1);

      SSA(errStream, uoc, expr, grandLet, identList, 
	  ast, 0, flags);      
      ast->child(0) = FEXPR(grandLet);      
      //warnTmp(errStream, expr);
      expr = ast->child(0);

      SSA(errStream, uoc, ndx, grandLet, identList, 
	  ast, 1, flags);      
      ast->child(1) = FEXPR(grandLet);
      ndx = ast->child(1);

      
      GCPtr<AST> lt = new AST(at_ident, ast->loc);
      GCPtr<AST> unit = new AST(at_unit, ast->loc);
      GCPtr<AST> IOB = new AST(at_ident, ast->loc);

      lt->s = "__index_lt";
      lt->fqn = FQName("bitc.prelude", "__index_lt");
      lt->symType = new Type(ty_fn, 
			     new Type(ty_fnarg, 
				      new Type(ty_word, lt),
				      new Type(ty_word, lt)),
			     new Type(ty_bool, lt));
      InstMangle(lt);

      IOB->s = "IndexBoundsError";
      IOB->fqn = FQName("bitc.prelude", "IndexBoundsError");
      IOB->symType = new Type(ty_exn, IOB);
      InstMangle(IOB);

      GCPtr<AST> len;
      if(ast->astType == at_array_nth)
	len = new AST(at_array_length, ast->loc, UseCase(expr));
      else
	len = new AST(at_vector_length, ast->loc, UseCase(expr));
      
      GCPtr<AST> ltApp = new AST(at_apply, ast->loc, lt, UseCase(ndx), len);
      
      GCPtr<AST> throwAst = new AST(at_throw, ast->loc, IOB);
      GCPtr<AST> ifAst = new AST(at_if, ast->loc, ltApp, unit, throwAst);
      addLB(grandLet, identList, ifAst, LB_IS_DUMMY);      
      
      FEXPR(grandLet) = ast;
      break;
    }

  case at_apply:
    {
      GCPtr<AST> id = ast->child(0);      

      GCPtr<Type> fn = id->symType->getBareType();
      assert(fn->isFnxn());
      GCPtr<Type> argsType = fn->Args()->getType();

      SSA(errStream, uoc, ast->child(0), grandLet, identList, 
	  ast, 0, flags);
      ast->child(0) = FEXPR(grandLet);
      
      for(c=1; c < ast->children->size(); c++) {
	
	SSA(errStream, uoc, ast->child(c), grandLet, identList, 
	    ast, c, flags);
	
	/* We must make sure that all by-ref applications can be legal
	   -- some applications may need temporary variable
	   introductions. For example: 
	   
	   (fnxn:(fn ((by-ref bool)) ()) #t)
	*/
	
	if((argsType->CompFlags(c-1) & COMP_BYREF) && 
	   (!ast->child(c)->isLocation())) {
	  
	  assert(ast->child(c)->isLiteral());
	  
	  // While application of literals to mutable-by-reference
	  // parameters is illegal, this should have been a type
	  // error. 
	  assert(!argsType->CompType(c-1)->isMutable());
	  
	  // Now it is safe to allow the byref application by
	  // introducing a temporary variable.
	  FEXPR(grandLet) = addLB(grandLet, identList, ast->child(c));
	}
	
	ast->child(c) = FEXPR(grandLet);	
      }

      if(id->Flags & SELF_TAIL) {
	assert(id == ast->child(0)); // we did not change the identifier
	FEXPR(grandLet) = addLB(grandLet, identList, ast, LB_IS_DUMMY);
      }
      else {
	FEXPR(grandLet) = addLB(grandLet, identList, ast);
      }
      break;
    }

  case at_setbang:
    {
      GCPtr<AST> lhs = ast->child(0);
      SSA(errStream, uoc, lhs, grandLet, identList, 
	  ast, 0, flags);
      ast->child(0) = FEXPR(grandLet);
      //warnTmp(errStream, lhs);
      lhs = ast->child(0);

      GCPtr<AST> rhs = ast->child(1);
      SSA(errStream, uoc, rhs, grandLet, identList, 
	  ast, 1, flags);      
      ast->child(1) = FEXPR(grandLet);
      rhs = ast->child(1);
      
      FEXPR(grandLet) = addLB(grandLet, identList, ast, 
			      LB_POSTPONED);
      // This must be 
      // FEXPR(grandLet) = addLB(grandLet, identList, ast);      
      // and not:
      // FEXPR(grandLet) = ast; or
      // FEXPR(grandLet) = addLB(grandLet, identList, ast, 
      //                        LB_IS_DUMMY);
      break;
    }    

  case at_begin:
    {
      for(c=0; c < ast->children->size(); c++) {
	SSA(errStream, uoc, ast->child(c), grandLet, identList, 
	       ast, c, flags);
	ast->child(c) = FEXPR(grandLet);
      }
      // FEXPR(grandLet), the last one is the right one.
      break;
    }    

  case at_not:
    {
      for(c=0; c < ast->children->size(); c++) {
	SSA(errStream, uoc, ast->child(c), grandLet, identList, 
	       ast, c, flags);
	ast->child(c) = FEXPR(grandLet);
      }

      FEXPR(grandLet) = ast;
      break;
    }    

  case at_if:
    {
      SSA(errStream, uoc, ast->child(0), grandLet, identList, 
	     ast, 0, flags);
      ast->child(0) = FEXPR(grandLet);

      GCPtr<AST> res = AST::genSym(ast, "t");
      GCPtr<AST> gl1 = newGrandLet(ast);
      GCPtr<AST> gl2 = newGrandLet(ast);

      SSA(errStream, uoc, ast->child(1), gl1, identList, 
	     ast, 1, flags);
      SSA(errStream, uoc, ast->child(2), gl2, identList, 
	     ast, 2, flags);

      FEXPR(gl1) = addLB(gl1, identList, FEXPR(gl1), 
			 0, res, true);
      FEXPR(gl2) = addLB(gl2, identList, FEXPR(gl2), 
			 0, res, false);
      SETGL(ast->child(1), gl1);
      SETGL(ast->child(2), gl2);

      GCPtr<AST> topres = UseCase(res);
      FEXPR(grandLet) = addLB(grandLet, identList, ast, LB_IS_DUMMY, topres, false);
      break;
    }
    
  case at_and:
    {
      GCPtr<AST> ifizedAST = new AST(at_if, ast->child(0)->loc);
      GCPtr<AST> ifAst = ifizedAST;
      GCPtr<AST> prev = NULL;
      for(c = 0; c < ast->children->size() - 1; c++) {
	GCPtr<AST> falseAst =  new AST(at_boolLiteral, ast->child(c)->loc);
	falseAst->litValue.b = false;
	falseAst->s = "#f";
	falseAst->symType = new Type(ty_bool, falseAst);

	ifAst->symType = ast->child(c)->symType;
	ifAst->children->append(ast->child(c));
	ifAst->children->append(new AST(at_if, ast->child(c+1)->loc));
	ifAst->children->append(falseAst);
	prev = ifAst;
	ifAst = ifAst->child(1);
      }
      
      if(prev)
	prev->child(1) = ifAst = 
	  ast->child(ast->children->size() -1);
      else
	ifAst = ast->child(ast->children->size() -1);
	
      parent->child(chno) = ifizedAST;
      SSA(errStream, uoc, ifizedAST, grandLet, identList, 
	     parent, chno, flags);
      break;
    }
  case at_or:
    {
      GCPtr<AST> ifizedAST = new AST(at_if, ast->child(0)->loc);
      GCPtr<AST> ifAst = ifizedAST;
      GCPtr<AST> prev = NULL;
      for(c = 0; c < ast->children->size() - 1; c++) {
	GCPtr<AST> trueAst =  new AST(at_boolLiteral, ast->child(c)->loc);
	trueAst->litValue.b = true;
	trueAst->s = "#t";
	trueAst->symType = new Type(ty_bool, trueAst);

	ifAst->symType = ast->child(c)->symType;
	ifAst->children->append(ast->child(c));
	ifAst->children->append(trueAst);
	ifAst->children->append(new AST(at_if, ast->child(c+1)->loc));
	prev = ifAst;
	ifAst = ifAst->child(2);
      }      

      if(prev)
	prev->child(2) = ifAst = 
	  ast->child(ast->children->size() -1);
      else
	ifAst = ast->child(ast->children->size() -1);
	
      parent->child(chno) = ifizedAST;
      SSA(errStream, uoc, ifizedAST, grandLet, identList, 
	     parent, chno, flags);      
      break;
    }

  case at_cond:
    {
      GCPtr<AST> caselegs = ast->child(0);
      GCPtr<AST> ow = ast->child(1)->child(0);
      GCPtr<AST> ifizedAST = new AST(at_if, caselegs->loc);
      GCPtr<AST> ifAst = ifizedAST;
      GCPtr<AST> prev = NULL;
      for(c = 0; c < caselegs->children->size(); c++) {
	GCPtr<AST> caseleg = caselegs->child(c);	
	ifAst->loc = caseleg->loc;
	ifAst->symType = caseleg->symType;
	ifAst->children->append(caseleg->child(0));
	ifAst->children->append(caseleg->child(1));

	ifAst->children->append(new AST(at_if, 
				       caselegs->child(c)->loc));
	prev = ifAst;
	ifAst = ifAst->child(2);	
      }

      if(prev) {	
	prev->child(2) = ifAst = ow;
      }
      else {
	ifAst = ow;
      }
      
      parent->child(chno) = ifizedAST;
      SSA(errStream, uoc, ifizedAST, grandLet, identList, 
	     parent, chno, flags);      
      break;
    }


  case at_switchR:
  case at_tryR:
    {           
      GCPtr<AST> res = AST::genSym(ast, "t");
      addIL(identList, res);
      
      // The result of the top-expression is a return value	
      // only in the case of a try block
      if(ast->astType == at_tryR) {
	GCPtr<AST> gl = newGrandLet(ast);
	SSA(errStream, uoc, ast->child(0), gl, identList, 
	    ast, 0, flags);	
	FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 0, res, false);
	SETGL(ast->child(0), gl);
      }
      else {
	SSA(errStream, uoc, ast->child(0), grandLet, identList, 
	    ast, 0, flags);
	ast->child(0) = FEXPR(grandLet);	
      }
      
      GCPtr<AST> cases = ast->child(1);      
      // the cases
      for(c=0; c < cases->children->size(); c++) {
	GCPtr<AST> theCase = cases->child(c);
	GCPtr<AST> gl = newGrandLet(theCase);
	
	if(ast->astType == at_switchR || ast->astType == at_tryR) {
	  addIL(identList, theCase->child(0));
	}
	else {
	  // Case only
	  SSA(errStream, uoc, theCase->child(0), gl, 
	      identList, theCase, 0, flags);
	}
	
	SSA(errStream, uoc, theCase->child(1), gl, 
	    identList, theCase, 1, flags);

	FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 0, res, false);
	SETGL(theCase->child(1), gl);
      }

      // otherwise
      GCPtr<AST> ow = ast->child(2);
      if(ow->astType == at_otherwise) {
	GCPtr<AST> owExpr = ow->child(0);
	GCPtr<AST> gl = newGrandLet(owExpr);
	SSA(errStream, uoc, owExpr, gl, identList, ast, 2, flags);
	FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 0, res, false);
	SETGL(ow->child(0), gl);
      }
      GCPtr<AST> topres = UseCase(res);
      FEXPR(grandLet) = addLB(grandLet, identList, ast, LB_IS_DUMMY, topres, false);
      break;
    }

  case at_letStar:
    {
      assert(false);
      break;
    }
 
  case at_do:
    {
      GCPtr<AST> res = AST::genSym(ast, "t");
      GCPtr<AST> gl;
      GCPtr<AST> theIdent;

      GCPtr<AST> dbs = ast->child(0);     
      for (size_t c = 0; c < dbs->children->size(); c++) {
	GCPtr<AST> db = dbs->child(c);
	assert(db->child(0)->astType == at_identPattern);	
	GCPtr<AST> ident = db->child(0)->child(0);
	addIL(identList, ident);
 	
	GCPtr<AST> init = db->child(1);
	gl = newGrandLet(ast);      
	SSA(errStream, uoc, init, gl, identList, db, 1, flags);
	theIdent = UseCase(ident);
	FEXPR(gl) = addLB(gl, identList, FEXPR(gl),
			  0, theIdent, false);
	SETGL(db->child(1), gl);
	db->Flags |= LB_IS_DUMMY; 
 	
	GCPtr<AST> step = db->child(2);
	gl = newGrandLet(ast);      
	SSA(errStream, uoc, step, gl, identList, db, 2, flags);
	theIdent = UseCase(ident);
	FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 
			  0, theIdent, false);
	SETGL(db->child(2), gl);
      }

      // The test
      // test
      GCPtr<AST> dotest = ast->child(1);
      gl = newGrandLet(ast);
      SSA(errStream, uoc, dotest->child(0), gl, identList, 
	     dotest, 0, flags);
      SETGL(dotest->child(0), gl);

      // result
      gl = newGrandLet(dotest);
      SSA(errStream, uoc, dotest->child(1), gl, identList, 
	     dotest, 1, flags);
      FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 
			0, res, true);
      SETGL(dotest->child(1), gl);
      
      
      // The expression
      gl = newGrandLet(ast);      
      SSA(errStream, uoc, ast->child(2), gl, identList, 
	  ast, 2, flags);      
      SETGL(ast->child(2), gl);
      
      GCPtr<AST> topres = UseCase(res);
      FEXPR(grandLet) = addLB(grandLet, identList, ast, 
			      LB_IS_DUMMY, topres, false);
      break;
    }

  case at_let:
  case at_letrec:
    {
      GCPtr<AST> res = AST::genSym(ast, "t");

      // Let bindings
      GCPtr<AST> lbs = ast->child(0);     
      for (size_t c = 0; c < lbs->children->size(); c++) {
	GCPtr<AST> lb = lbs->child(c);
	assert(lb->child(0)->astType == at_identPattern);	
	GCPtr<AST> ident = lb->child(0)->child(0);
	GCPtr<AST> gl = newGrandLet(ast);      
	SSA(errStream, uoc, lb->child(1), gl, identList, 
	       lb, 1, flags);
	GCPtr<AST> theIdent = ident;
	addIL(identList, theIdent);
	//theIdent->markFlags(ID_IS_LETBOUND);
	FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 
			  0, theIdent, false);
	SETGL(lb->child(1), gl);
	lb->Flags |= LB_IS_DUMMY; 
      }
            
      // The real Final expression of the real let
      GCPtr<AST> gl = newGrandLet(ast);      
      SSA(errStream, uoc, ast->child(1), gl, identList, 
	     ast, 1, flags);      
      FEXPR(gl) = addLB(gl, identList, FEXPR(gl), 
			0, res, true);
      SETGL(ast->child(1), gl);
      
      GCPtr<AST> topres = UseCase(res);
      FEXPR(grandLet) = addLB(grandLet, identList, ast, 
			      LB_IS_DUMMY, topres, false);
      break;
    }
  }
  return errorFree;
}


/* NOTE: SSA Pass introduces explicit bounds checks.
   Beyond this pass, no fuether bounds checks are done */

bool
UocInfo::be_ssaTrans(std::ostream& errStream,
		     bool init, unsigned long flags)
{  
  bool errFree = true;

  GCPtr<UocInfo> uoc = this;
  
  CHKERR(errFree, ssa(errStream, uoc, uoc->uocAst, NULL, 
			 NULL, uoc->uocAst, 0, flags));
  
  //errStream << "ATransed AST = " << std::endl 
  //	    << uoc->ast->asString() << std::endl;
  CHKERR(errFree, uoc->RandT(errStream, true, 
			     CL_SYM_FLAGS, CL_TYP_FLAGS));
  return errFree;
}

