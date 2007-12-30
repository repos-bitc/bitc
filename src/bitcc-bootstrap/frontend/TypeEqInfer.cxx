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
#include <iostream>
#include <string>
#include <sstream>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <assert.h>
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "Unify.hxx"
#include <libsherpa/BigNum.hxx>
#include "TypeInfer.hxx"
#include "TypeInferCommon.hxx"


//WARNING: **REQUIRES** errFree.
#define TYPEEQINFER(ast, gamma, instEnv, impTypes, isVP, tcc,	\
		    uflags, trail, mode, flags)			\
  do {								\
    CHKERR((errFree),						\
	   (typeEqInfer(errStream, (ast), (gamma), (instEnv),	\
			(impTypes), (isVP), (tcc), (uflags),	\
			(trail), (mode), (flags))));		\
  }while(0)


bool
typeEqInfer(std::ostream& errStream, GCPtr<AST> ast, 
	    GCPtr<Environment<TypeScheme> > gamma,
	    GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	    GCPtr< CVector<GCPtr<Type> > >impTypes,
	    bool isVP, 
	    GCPtr<Constraints> tcc,
	    unsigned long uflags,
	    GCPtr<Trail> trail,
	    int mode,
	    unsigned flags)
{
  bool errFree = true;
  
  // Save the current environment in the AST.
  // If we create a new environment, we will update it later.
  ast->envs.gamma = gamma;
  ast->envs.instEnv = instEnv;  
  
  switch(ast->astType) {
    
  default:
    {
      errStream << ast->loc << ": Unhandled case: " 
		<< ast->astTypeName() << std::endl;
      
      errFree = false;
      break;
    }

  case at_boolLiteral:
    {
      ast->symType = new Type(Type::LookupKind("bool"), ast);
      break;
    }

  case at_charLiteral:
    {
      ast->symType = new Type(Type::LookupKind("char"), ast);
      break;
    }

  case at_intLiteral:
    {      
      ast->symType = new Type(Type::LookupKind("int32"), ast);
      break;
    }

  case at_floatLiteral:
    {
      ast->symType = new Type(Type::LookupKind("float"), ast);
      break;
    }
    
  case at_stringLiteral:
    {
      ast->symType = new Type(Type::LookupKind("string"), ast);
      break;
    }

  case at_ident:
    {
      switch(mode) {
      case DEF_MODE:
	{
	  unsigned long bindFlags = 0;
	  GCPtr<TypeScheme> sigma = gamma->getBinding(ast->s);

	  if(sigma) {	    
	    if(ast->isDecl) {
	      /* Note: This case is ONLY used for proclaims. 
		 structure and union declarations and definitions
		 do not make this recursive call */
	      
	      // We need to preserve this un-unified until after the
	      // real type is inferred. So, this unification will now be
	      // done in handle-decls.
	      //
	      //   CHKERR(errFree, unify(errStream, trail, gamma, 
	      //                         ast, ast->symType, 
	      // 	                       sigma->tau, uflags));
	      //
	      // Make way for the actual definition of the type.
	     
	      ast->symType = newBindType(ast, flags);
	      GCPtr<TypeScheme> sigma = new TypeScheme(ast->symType, ast, NULL);
	      ast->symType->getBareType()->defAst = sigma->tau->getBareType()->defAst;
	      ast->scheme = sigma;
	    }
	    else {	      
	      bindFlags = BF_REBIND;
	      sigma = bindIdentDef(ast, gamma, bindFlags, flags);
	      ast->symType->defAst = sigma->tau->getType()->defAst = ast;
	      break;
	    }
	  }
	  else
	    sigma = bindIdentDef(ast, gamma, bindFlags, flags);	  
	  break;
	}
      
      case REDEF_MODE:
	{
	  (void) bindIdentDef(ast, gamma, BF_REBIND, flags);
	  break;
	}

      case USE_MODE:
	{
	  assert(tcc);

	  GCPtr<TypeScheme> sigma = gamma->getBinding(ast->s);
	  if(!sigma) {

	    // If this is a type variable that is used as in
	    //
	    //   (define a:'a 10)
	    //
	    // there will be no prior definition of it ('a).  So, it
	    // should now be defined.  In-correct usages should be
	    // taken care of by the symbol resolver.  So, it is safe
	    // to add this type to Gamma now.

	    if((ast->identType == id_type) && (ast->Flags & ID_IS_TVAR)) {
	      sigma = bindIdentDef(ast, gamma, 0, flags);	      
	    }
	    else {  
	      errStream << ast->loc << ": "
			<< ast->s << " Unbound in Gamma" << std::endl;
	      
	      //errStream << "Available bindings are: "
	      //	  << gamma->asString()
	      //	  << std::endl;	      
	      
	      ast->symType = newTvar(ast);
	      return false;
	    }
	  }
	  
	  GCPtr<TypeScheme> tsIns =  Instantiate(ast, sigma);
	  GCPtr<Type> ins = tsIns->tau->getType();
	  ast->symType = ins;
	  
#ifdef VERBOSE  
	  errStream << " For " << ast->s << ":\n";
	  errStream << "Obtained " << ins->asString()
		    << " From " << sigma->asString() << std::endl;
#endif
	      
	  ins = ins->getBareType();

	  if((flags & TI_TYP_EXP) && 
	     ((flags & TI_TYP_APP) == 0) && 
	     (ins->typeArgs->size() > 0)) {
	    errStream << ast->loc << ": "
		      << ast->s << " cannot be instantiated without " 
		      << ins->typeArgs->size() << " type arguments."
		      << std::endl;
	    
	    ast->symType = newTvar(ast);
	    return false;
	  }
	  
	  if(tsIns->tcc) {
	    for(size_t i = 0; i < tsIns->tcc->pred->size(); i++) {
	      GCPtr<Typeclass> pred = tsIns->tcc->Pred(i)->getType();	      
	      if(flags & TI_TCC_SUB)
		pred->flags |= TY_CT_SUBSUMED;
	      tcc->addPred(pred);
	    }
	  }
	  break;
	}
      }
      break;
    }

  case at_start:
    {
      // match at_module
    
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_NONE);
    
      if (ast->children->size() > 1) {
	// match at_version
	TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  mode, TI_NONE);
      }

      break;
    }

  case at_version:
    {
      // match at_stringLiteral
      //       TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP,
      // 		uflags, trail,  USE_MODE, TI_COMP2);

      break;
    }

  case at_module:
    {
      for(size_t c = 0; c < ast->children->size(); c++) {
	TYPEEQINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  mode, TI_NONE);
	// errStream << " - - - - - - - - - - - - - - - - - - - - - - - - - "
	// 	     << std::endl;
      }
      break;
    }

  case at_interface:
    {
      // match at_ident
      //    TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
      //              uflags, trail,  mode, TI_COMP2);
    
      // match agt_definition*

      for(size_t c = 1; c < ast->children->size(); c++)
	TYPEEQINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  mode, TI_NONE);
      break;
    }

  case at_define:
    {
      // Maybe, we have a prior declaration?
      GCPtr<AST> ident = ast->child(0)->child(0);
      
      GCPtr<TypeScheme> declTS = gamma->getBinding(ident->s);
      
      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      GCPtr<TCConstraints> currTcc = new TCConstraints;
      
      // match agt_bindingPattern
      // match agt_expr
      TYPEEQINFER(ast->child(0), defGamma, instEnv, impTypes, isVP, 
		  currTcc, uflags, trail, DEF_MODE, TI_NONE);

      TYPEEQINFER(ast->child(1), defGamma, instEnv, impTypes, isVP, 
		  currTcc, uflags, trail, USE_MODE, TI_NONE);
      
      TYPEEQINFER(ast->child(2), defGamma, instEnv, impTypes, isVP, 
		  currTcc, uflags, trail,  mode, TI_CONSTR);
      
      GCPtr<Type> lhsType = ast->child(0)->symType->getType();
      GCPtr<Type> rhsType = ast->child(1)->symType;

      
      //       CHKERR(errFree, unify(errStream, trail, ast->child(0), 
      // 			    lhsType, rhsType, uflags));
      
      //       CHKERR(errFree, generalizePat(errStream, ast->loc, 
      // 				    gamma, instEnv, 
      // 				    ast->child(0),
      // 				    ast->child(1), 
      // 				    false, currTcc, NULL, trail));
      
      //       gamma->mergeBindingsFrom(defGamma);
      
      //       if(declTS) 
      // 	CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
      // 				     declTS, ident->scheme, uflags, true));	
      
#ifdef VERBOSE  
      errStream << "At " << ast->asString()
      		<< "[0] = " << ast->child(0)->child(0)->scheme->asString()
		<< "[1] = " << ast->child(1)->symType->asString()
      		<< std::endl;            
#endif
      ast->symType = ast->child(0)->symType;
      break;
    }
    
  case at_refType:
    {
      // match agt_type
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP1);
    
      GCPtr<Type> t = ast->child(0)->getType();
      
      ast->symType = new Type(ty_ref, ast);
      ast->symType->components->append(new comp(t));
      
      break;
    }
    
  case at_exceptionType:
    {
      ast->symType = new Type(ty_exn, ast);
      break;
    }

  case at_dummyType:
    {
      ast->symType = new Type(ty_dummy, ast);
      break;
    }

  case at_fn:
    {
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP1);
      TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP1);
      
      ast->symType = new Type(ty_fn, ast);
      GCPtr<Type> fnarg = ast->child(0)->symType->getType();
      ast->symType->components->append(new comp(fnarg));
      GCPtr<comp> nComp = new comp(ast->child(1)->getType());
      ast->symType->components->append(nComp);    
      break;
    }

  case at_fnargVec:
    {      
      GCPtr<Type> fnarg = new Type(ty_fnarg, ast);
      for (size_t c = 0; c < ast->children->size(); c++) {
	TYPEEQINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail, mode, TI_COMP1);
	GCPtr<Type> argType = ast->child(c)->symType->getType();

	GCPtr<comp> nComp = new comp(argType);	
	if(argType->isByrefType()) {
	  nComp = new comp(argType->CompType(0));
	  nComp->flags |= COMP_BYREF;
	}
	
	fnarg->components->append(nComp);
      }
      ast->symType = fnarg;
      break;
    }

  case at_primaryType:
    {
      ast->symType = new Type(Type::LookupKind(ast->s), ast);
      break;
    }

  case at_mutableType:
    {
      // match agt_type
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP1);
    
      GCPtr<Type> t = ast->child(0)->symType->getType();
      
      if(t->kind == ty_mutable) {
	//The Type is already mutable
	ast->symType = t;
      }
      if(t->kind == ty_maybe) {
	//maybe-types appear around type variables
	t->kind = ty_mutable;
	ast->symType = t;
      }
      else {
	ast->symType = new Type(ty_mutable, ast);
	ast->symType->components->append(new comp(t));
      }
      break;
    }

  case at_identPattern:
    {
      
      if(ast->Flags & AST_IS_VALPAT) {
	// AST_IS_VALPAT ONLY for the ROOT of a case leg
	assert(mode == REDEF_MODE);

	GCPtr<AST> var = ast->child(0);
	GCPtr<AST> def = var->symbolDef;

	if((def) && def->isUnionLeg()) {
	  TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		      uflags, trail, USE_MODE, TI_COMP2);
	}
	else {
	  // match agt_var
	  TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		      uflags, trail, REDEF_MODE, TI_COMP2);
	}      
      }
      else {
	// match agt_var
	TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  mode, TI_COMP2);
      }
      
      // Type Qualifications ONLY in Binding Patterns
      // match agt_type?
      if (ast->children->size() > 1) {
	TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  USE_MODE, TI_COMP1);
      
	if(ast->child(1)->symType->isByrefType()) {
	  CHKERR(errFree, unify(errStream, trail, ast->child(0), 
				ast->child(0)->symType, 
				ast->child(1)->getType()->CompType(0), 
				uflags));
	}
	else {
	  CHKERR(errFree, unify(errStream, trail, ast->child(0), 
				ast->child(0)->symType, 
				ast->child(1)->symType, 
				uflags));
	}
	
	// Very Important that we pick the type of 
	// the qualification, in light of by-ref types.
	ast->symType = ast->child(1)->symType;
      }
      else {
	ast->symType = ast->child(0)->symType;
      }
      
      break;
    }

  case at_tqexpr:
    {
      // match agt_eform
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
    
      TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP1);

      CHKERR(errFree, unify(errStream, trail, ast->child(1), 
			    ast->child(0)->symType, 
			    ast->child(1)->symType,
			    uflags));

      ast->symType = ast->child(0)->symType;
      break;
    }
    
  case at_unit:
    {
      ast->symType = nonCopyType(ty_unit, ast);
      break;
    }

  case at_lambda:
    {
      // match agt_bindingPattern
      // match agt_expr

      GCPtr<Environment<TypeScheme> > lamGamma = gamma->newScope();
      ast->envs.gamma = lamGamma;
      
      GCPtr<AST> argVec = ast->child(0);      
      GCPtr<Type> fnarg = new Type(ty_fnarg, ast->child(0));
      // FnArgVec is never mutable

      for (size_t c = 0; c < argVec->children->size(); c++) {
	TYPEEQINFER(argVec->child(c), lamGamma, instEnv, impTypes, 
		    isVP, tcc, uflags, trail,  REDEF_MODE, TI_COMP2);

	GCPtr<Type> argType = argVec->child(c)->getType();

	GCPtr<comp> nComp = new comp(argType);
	if(argType->isByrefType()) {
	  nComp = new comp(argType->CompType(0));
	  nComp->flags |= COMP_BYREF;
	}
	
	fnarg->components->append(nComp);
      }
      argVec->symType = fnarg;      

      TYPEEQINFER(ast->child(1), lamGamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail,  USE_MODE, TI_COMP2);
    
      GCPtr<Type> retType = ast->child(1)->getType();

      /* Copy-compatibility is currently handled at apply-time
	 This could be handled at lanbda only if desired, but we will
	 have to deal with constructor applications as well.
	 So, in the present scheme, lambda preserves exact typing. */
      ast->symType = nonCopyType(ty_fn, ast);
      GCPtr<Type> fnType = ast->symType->getBareType();
      fnType->components->append(new comp(fnarg));
      fnType->components->append(new comp(retType));      

      break;
    }

  case at_argVec:
    {
      assert(false);
      break;
    }

  case at_apply:
    {
      // match agt_expr agt_expr
      //NOTE: One operation safe. (+)
      ast->symType = newTvar(ast); 
      
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);

      GCPtr<Type> t1 = ast->child(0)->symType->getType();
      if(t1->kind == ty_tvar) {
	GCPtr<Type> fn = buildFnFromApp(ast, uflags);
	GCPtr<Type> mbfn = fn->TypeOfCopy();
	t1->link = mbfn;
      }
      
      GCPtr<Type> t = t1->getBareType();

      switch(t->kind) {
      case ty_tvar:
	{
	  GCPtr<Type> fn = buildFnFromApp(ast, uflags);
	  t->link = fn;
	  t = t->getType();

	  // fall through
	}
	
      case ty_fn:
	{
	  GCPtr<Type> targ = t->CompType(0)->getType();
	  // This is what we fall in to in all of the apply cases.
	  if ((ast->children->size()-1) != targ->components->size()) {
	    errStream << ast->child(0)->loc << ": "
		      << "Function applied to wrong number of"
		      << " arguments.." 
		      << " at AST " << ast->asString()
		      << " fn Type is " 
		      << t->asString() << ", "
		      << "Expecting " << targ->components->size()
		      << " but obtained " 
		      << (ast->children->size()-1) << ";"
		      << std::endl;
	    errFree = false;
	    break;
	  }
	  
	  for (size_t i = 0; i < ast->children->size()-1; i++) {
	    GCPtr<AST> arg = ast->child(i+1);
	    TYPEEQINFER(arg, gamma, instEnv, impTypes, isVP, tcc,
			uflags, trail,  USE_MODE, TI_COMP2);
	    
	    GCPtr<Type> rhsType = 0;
	    if(targ->CompFlags(i) & COMP_BYREF) {
	      // by-ref arguments need strict compatibality.
	      rhsType = arg->symType;
	    }
	    else {
	      // by-value arguments can have copy-compatibility.
	      rhsType = ArgType(arg->symType);
	    }
	    
	    CHKERR(errFree, unify(errStream, trail, ast, 
				  targ->CompType(i), 
				  rhsType, uflags));
	  }
	  
	  GCPtr<Type> retType = RetType(t->CompType(1));
	  if(errFree) {
	    CHKERR(errFree, unify(errStream, trail, ast, 
				  retType, ast->symType, uflags));	    
	  }
	  break;
	}

      case ty_structv:
      case ty_structr:
	{
	  if(ast->child(0)->astType == at_ident &&
	     (ast->child(0)->symbolDef->Flags & ID_IS_CTOR)) {
	    ast->astType = at_struct_apply;
	    TYPEEQINFER(ast, gamma, instEnv, impTypes, isVP, tcc,
			uflags, trail,  USE_MODE, TI_COMP2);
	  }
	  else {
	    errStream << ast->child(0)->loc
		      << ": Expected structure"
		      << " constructor taking at least one argument."
		      << std::endl;
	    errFree = false;
	  }
	
	  break;
	}

      case ty_uconr:
      case ty_uconv:
      case ty_exn:
	{		  
	  GCPtr<AST> ctr = ast->child(0);
	  if((ctr->astType != at_ident) && 
	     (ctr->astType != at_select)) {
	    errStream << ast->child(0)->loc
		      << ": union/exception"
		      << " constructor expected."
		      << std::endl;
	    errFree = false;	    
	    break;
	  }	    

	  ctr = ctr->getCtr();
	  if(ctr->symbolDef->Flags & ID_IS_CTOR) {
	    ast->astType = at_ucon_apply;
	    TYPEEQINFER(ast, gamma, instEnv, impTypes, isVP, tcc,
			uflags, trail,  USE_MODE, TI_COMP2);
	  }
	  else { 
	    errStream << ast->child(0)->loc
		      << ": Expected union/exception"
		      << " constructor taking at least one argument."
		      << std::endl;
	    errFree = false;
	  }
	  break;
	}
      case ty_unionv:
      case ty_unionr:
	{
	  errStream << ast->loc << ": "
		    << " Cannot use the union name to construct values."
		    << " Use one of its value constructors."
		    << std::endl;
	  errFree = false;
	  break;
	}
	//case at_usesel:
	//	assert(false);
	
      default: 
	{
	  errStream << ast->child(0)->loc
		    << ": First argument in application must be a function"
		    << " or a value constructor."
		    << std::endl;
	  errFree = false;
	  break;
	}
      }

      break;
    }

  case at_ucon_apply:
    {
      GCPtr<AST> ctr = ast->child(0);
      if(!ctr->symType) {
	TYPEEQINFER(ctr, gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  USE_MODE, TI_COMP2);
      }
      
      GCPtr<Type> t = ctr->symType->getBareType();

      if(t->kind != ty_uconv && t->kind != ty_uconr && 
	 t->kind != ty_exn) {
	
	if(t->kind == ty_unionv || t->kind == ty_unionr) {
	  
	  errStream << ast->loc << ": "
		    << "Cannot use the union name to construct values. "
		    << "Did you mean to use one of its value constructors?"
		    << std::endl;
	  errFree = false;
	  break;
	}	
	
	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " cannot be resolved" 
		  << " to a Union (or exception) Constructor. Obtained " 
		  << t->asString()
		  << std::endl;
	errFree = false;
	break;
      }
    
      GCPtr<Type> ut = t;
      //       if(ast->children->size() != ut->components->size() + 1) {
      // 	errStream << ast->child(0)->loc << ": "
      // 		  << "Constructor " << ast->child(0)->s << " cannot be" 
      // 		  << " partially instantiated" << std::endl;
      // 	errFree = false;
      // 	break;
      //       }
      
      size_t astCnt=1;
      for(size_t i=0; i < ut->components->size(); i++) {
	GCPtr<comp> ctrComp = ut->components->elem(i);
	if(ctrComp->flags & COMP_UNIN_DISCM)
	  continue;
	
	if(astCnt >= ast->children->size()) {
	  errStream << ast->child(0)->loc << ": "
		    << "Constructor " << ast->child(0)->s << " cannot be" 
		    << " partially instantiated" << std::endl;
	  errFree = false;
	  break;	  
	}
	
	TYPEEQINFER(ast->child(astCnt), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail, USE_MODE, TI_COMP2);
	
	GCPtr<Type> rhsType = ast->child(astCnt)->symType->TypeOfCopy();
	
	CHKERR(errFree, unify(errStream, trail, ast->child(astCnt), 
			      ut->CompType(i), rhsType, 
			      uflags));
	astCnt++;
      }
      
      if(astCnt < ast->children->size()) {
	errStream << ast->child(0)->loc << ": Too many arguments to "
		  << "constructor " << ast->child(0)->s << "."
		  << std::endl;
	errFree = false;
	break;
      }
      else
	assert(astCnt == ast->children->size());

      if(!errFree)
	break;

      GCPtr<Type> t1 = new Type(ty_tvar, ast);
      if(t->kind == ty_uconr || t->kind == ty_uconv) {
	// Now Form the union value
	switch(t->kind) {
	case ty_uconr:
	  t1->kind = ty_uvalr;
	  break;
	case ty_uconv:
	  t1->kind = ty_uvalv;
	  break;
	default:
	  die();
	}
	t1->defAst = ut->defAst;
	t1->myContainer = ut->myContainer;
	// The type-arguments should have unified and stabilized by now.
	for(size_t i=0; i < ut->typeArgs->size(); i++) {
	  t1->typeArgs->append(ut->TypeArg(i));
	}	
      }
      else {
	assert(t->kind == ty_exn);
	// ut is well-formed for my purpose
	t1->link = ut;
      }      

      GCPtr<Type> realType = RetType(t1);
      ast->symType = realType;
      
      break;
    }

  case at_if:
    {
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);      
      
      CHKERR(errFree, unifyPrim(errStream, trail, ast->child(0), 
				conditionalType(ast->child(0)->symType), 
				"bool"));
      
      // match agt_expr
      TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);
      GCPtr<Type> ifType = conditionalType(ast->child(1)->symType);

      // match agt_expr
      TYPEEQINFER(ast->child(2), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);
      GCPtr<Type> elseType = conditionalType(ast->child(2)->symType);
    

      ast->symType = newTvar(ast);
      CHKERR(errFree, unify(errStream, trail, ast, ast->symType, 
			    ifType, uflags));
      CHKERR(errFree, unify(errStream, trail, ast, ast->symType, 
			    elseType, uflags));
    
      break;
    }

  case at_setbang:
    {
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      ast->symType = new Type(ty_tvar, ast);
      CHKERR(errFree, unifyPrim(errStream, trail, ast, 
				ast->symType, "unit")); 
      
      GCPtr<Type> t = ast->child(0)->symType->getType();
      
      if(t->kind == ty_maybe) {
	t->kind = ty_mutable;
      }
      else if(t->kind == ty_tvar) {
	t->kind = ty_mutable;
	t->components->append(new comp(new Type(ty_tvar, ast->child(0))));
      }
      else if (t->kind != ty_mutable) {
	errStream << ast->child(0)->loc << ": "
		  << "set! can only be applied to a mutable value" 
		  << ", but here obtained " 
		  << t->asString()
		  << std::endl;
	errFree = false;
	break;
      }
      
      // match agt_expr
      TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> rhsType = ast->child(1)->symType->TypeOfCopy();      
      CHKERR(errFree, unify(errStream, trail, ast->child(1), 
			    t, rhsType, uflags));
      
      break;
    }

  case at_dup:
    {
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> t = ast->child(0)->symType;
      
      ast->symType = ConstructedType(ty_ref, ast);
      GCPtr<Type> t1 = ast->symType->getBareType();
      t1->components->append(new comp(t->TypeOfCopy()));
      //       if((ast->Flags2 & DUPED_BY_CLCONV) == 0)
      // 	t1->components->append(new comp(t));
      //       else
      // 	t1->components->append(new comp(t->TypeOfCopy()));
      break;      
    }

  case at_deref:
    {
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> realType = ast->child(0)->getType();
      GCPtr<Type> t = realType->getBareType();


      switch(t->kind) {

      case ty_tvar:
	{
	  t->kind = ty_ref;
	  assert(t->components->size() == 0);
	  GCPtr<Type> innerType = newTvar(t->ast);
	  t->components->append(new comp(innerType));
	  ast->symType = innerType;
	  break;
	}

      case ty_ref:
	{	
	  ast->symType = t->CompType(0)->getType();
	  break;
	}

      default:
	{	
	  ast->symType = newTvar(ast);      	  
	  errStream << ast->loc << ": "
		    << "Target of a deref should be a (ref 'a) type." 
		    << "But obtained" << realType->asString() << std::endl;
	  
	  errFree = false;
	  break;
	}	
      }
      
      break;
    }
    
  } /* switch */
  
  return errFree;  
}

