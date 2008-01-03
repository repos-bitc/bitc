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
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include <libsherpa/BigNum.hxx>
#include "TypeInfer.hxx"
#include "TypeEqInfer.hxx"
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


// For debugging only.
GCPtr<TvPrinter> debugTvp = new TvPrinter;
static std::string
ctypeAsString(GCPtr<Type> t, GCPtr<Constraints> cset)
{
  stringstream ss;
  ss << t->asString(debugTvp);
  if(cset->size()) {
    ss << " / {";
    for(size_t i=0; i < cset->size(); i++) {
      if(i > 0)
	ss << ", ";
      ss << cset->Pred(i)->asString(debugTvp);
    }
    ss << "}";
  }
  return ss.str();
}


#define PRINT(out, ast, ct)				\
  do {							\
    out << "[" << ast->atKwd() << "]"			\
	<< ast->asString() << " : "			\
	<< ctypeAsString(ast->symType, ct)		\
	<< std::endl;					\
  } while(0)


/**************************************************************/
/*                     Some Helper Functions                  */
/**************************************************************/

/* Some of the following fure repeated (and marked static) in both 
   inference routines due to the use/non-use of maybe types */

static GCPtr<Type> 
buildFnFromApp(GCPtr<AST> ast, unsigned long uflags)
{
  assert(ast->astType == at_apply);
  GCPtr<Type> fn = new Type (ty_fn, ast);
  GCPtr<Type> targ = new Type(ty_fnarg, ast);
  for (size_t i = 1; i < ast->children->size(); i++) {
    GCPtr<Type> argi = new Type(ty_tvar, ast->child(i));
    targ->components->append(new comp(argi));
  }
  
  fn->components->append(new comp(targ));
  GCPtr<Type> ret = new Type(ty_tvar, ast);
  fn->components->append(new comp(ret));
  
  return fn;
}


static GCPtr<TypeScheme> 
bindIdentDef(GCPtr<AST> ast, 
	     GCPtr<Environment<TypeScheme> > gamma,
	     unsigned long bindFlags,
	     unsigned long flags)
{
  if(ast->Flags2 & ID_IS_MUTATED) {
    assert((flags & TI_TYP_EXP) == 0);
    assert((ast->Flags & ID_IS_TVAR) == 0);
    ast->symType = new Type(ty_mutable, new Type(ty_tvar, ast));
  }
  else
    ast->symType = new Type(ty_tvar, ast); 
  
  GCPtr<TypeScheme> sigma = new TypeScheme(ast->symType);
  ast->scheme = sigma;
  
  if (ast->Flags & ID_IS_TVAR) {
    assert(flags & TI_TYP_EXP);
    bindFlags |= BF_NO_MERGE;
    ast->tvarLB->envs.gamma->addBinding(ast->s, sigma);
  }
  else {
    gamma->addBinding(ast->s, sigma);      
  }
  
  gamma->setFlags(ast->s, bindFlags);    
  return sigma;
}

static GCPtr<TypeScheme> 
Instantiate(GCPtr<AST> ast, GCPtr<TypeScheme> sigma)
{	      
  if(ast->symbolDef)
    ast = ast->symbolDef;
  
  if(ast->Flags & ID_IS_CTOR)
    return sigma->ts_instance_copy();
  else
    return sigma->ts_instance();
}

/**************************************************************/
/*                     Constraint Generation                  */
/**************************************************************/

void
addSubCst(GCPtr<AST> errAst, GCPtr<Type> t1, GCPtr<Type> t2,
	  GCPtr<Constraints> tcc)
{
  GCPtr<Constraint> sub = new Constraint(ty_subtype, errAst, 
					 t1->getType(), t2->getType());
  tcc->addPred(sub);
}

void
addEqCst(GCPtr<AST> errAst, GCPtr<Type> t1, GCPtr<Type> t2,
	 GCPtr<Constraints> tcc)
{
  addSubCst(errAst, t1, t2, tcc);
  addSubCst(errAst, t2, t1, tcc);
}
	
void
addCcCst(GCPtr<AST> errAst, GCPtr<Type> t1, GCPtr<Type> t2,
	 GCPtr<Constraints> tcc)
{
  GCPtr<Type> via = new Type(ty_tvar, errAst);
  addSubCst(errAst, t1, via, tcc);
  addSubCst(errAst, t2, via, tcc);
}

void
addPcst(GCPtr<AST> errAst, GCPtr<Type> t, GCPtr<Constraints> tcc)
{
  GCPtr<Type> k = new Type(ty_kvar, errAst);
  t = t->getType();
  GCPtr<Constraint> pcst = new Constraint(ty_pcst, errAst);  
  pcst->components->append(new comp(k));
  pcst->components->append(new comp(t));
  pcst->components->append(new comp(t));
  tcc->addPred(pcst);
}

/**************************************************************/
/****                   MAIN INFERENCE FUNCTION            ****/
/**************************************************************/


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
      ast->symType = new Type(ty_bool, ast);
      PRINT(errStream, ast, tcc);
      break;
    }

  case at_charLiteral:
    {
      ast->symType = new Type(ty_char, ast);
      PRINT(errStream, ast, tcc);
      break;
    }

  case at_intLiteral:
    {      
      ast->symType = new Type(ty_int32, ast);
      PRINT(errStream, ast, tcc);
      break;
    }

  case at_floatLiteral:
    {
      ast->symType = new Type(ty_float, ast);
      PRINT(errStream, ast, tcc);
      break;
    }
    
  case at_stringLiteral:
    {
      ast->symType = new Type(ty_string, ast);
      PRINT(errStream, ast, tcc);
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
	     
	      ast->symType = new Type(ty_tvar, ast);
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
	      
	      ast->symType = new Type(ty_tvar, ast);
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
	    
	    ast->symType = new Type(ty_tvar, ast);
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
      PRINT(errStream, ast, tcc);
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
      break;
    }
    
  case at_module:
    {
      // FIX: This must be eventually removed when 
      // new constraint sets are created at every define
      GCPtr<TCConstraints> tcc = new TCConstraints;
      for(size_t c = 0; c < ast->children->size(); c++) {
	TYPEEQINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  mode, TI_NONE);
      }
      break;
    }
    
  case at_interface:
    {
      // match agt_definition*
      
      // FIX: This must be eventually removed when 
      // new constraint sets are created at every define
      GCPtr<TCConstraints> tcc = new TCConstraints;
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

      // This is the right place to start constraints, but for now, 
      // define is like  let .... 
      //GCPtr<TCConstraints> currTcc = new TCConstraints;
      GCPtr<TCConstraints> currTcc = tcc;
      
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
      
      addCcCst(ast, lhsType, rhsType, currTcc);
      //       CHKERR(errFree, unify(errStream, trail, ast->child(0), 
      // 			    lhsType, rhsType, uflags));
      
      //       CHKERR(errFree, generalizePat(errStream, ast->loc, 
      // 				    gamma, instEnv, 
      // 				    ast->child(0),
      // 				    ast->child(1), 
      // 				    false, currTcc, NULL, trail));
      
      gamma->mergeBindingsFrom(defGamma);
      //       if(declTS) 
      // 	CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
      // 				     declTS, ident->scheme, uflags, true));	
      
      ast->symType = ast->child(0)->symType;

      GCPtr<AST> id = ast->getID();
      errStream << "[define]"		
		<< id->asString() << " : "	
		<< ctypeAsString(id->symType, currTcc)
		<< ast->symType->asString(debugTvp)
		<< std::endl;
      
      EqUnify(errStream, currTcc);
      errStream << "  UNF:"
		<< id->asString() << " : "
		<< ctypeAsString(id->symType, currTcc)
		<< std::endl;
      
      //PRINT(errStream, ast, currTcc);
      break;
    }
    
  case at_constraints:
    {
      for(size_t c=0; c < ast->children->size(); c++)      
	TYPEEQINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  mode, TI_CONSTR);
      ast->symType = new Type(ty_tvar, ast);
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
      
      PRINT(errStream, ast, tcc);
      break;
    }
    
  case at_exceptionType:
    {
      ast->symType = new Type(ty_exn, ast);
      PRINT(errStream, ast, tcc);
      break;
    }

  case at_dummyType:
    {
      ast->symType = new Type(ty_dummy, ast);
      PRINT(errStream, ast, tcc);
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
      PRINT(errStream, ast, tcc);
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
      PRINT(errStream, ast, tcc);
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
      else {
	ast->symType = new Type(ty_mutable, ast);
	ast->symType->components->append(new comp(t));
      }
      PRINT(errStream, ast, tcc);
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
      
	GCPtr<Type> qualType = (ast->child(1)->symType->isByrefType()?
				ast->child(1)->getType()->CompType(0):
				ast->child(1)->symType);
	
	addEqCst(ast, ast->child(0)->symType, qualType, tcc);
	
	// Very Important that we pick the type of 
	// the qualification, in light of by-ref types.
	ast->symType = ast->child(1)->symType;
	PRINT(errStream, ast, tcc);
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
 
      addEqCst(ast, ast->child(0)->symType, 
	       ast->child(1)->symType, tcc);
      
      ast->symType = ast->child(0)->symType;
      PRINT(errStream, ast, tcc);
      break;
    }
    
  case at_unit:
    {
      ast->symType = new Type(ty_unit, ast);
      PRINT(errStream, ast, tcc);
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
      
      for (size_t c = 0; c < argVec->children->size(); c++) {
	GCPtr<AST> arg = argVec->child(c);
	TYPEEQINFER(arg, lamGamma, instEnv, impTypes, 
		    isVP, tcc, uflags, trail,  REDEF_MODE, TI_COMP2);

	GCPtr<Type> argInfType = arg->getType();
	
	if(argInfType->isByrefType()) {
	  GCPtr<comp> nComp = new comp(argInfType->CompType(0));
	  nComp->flags |= COMP_BYREF;
	  fnarg->components->append(nComp);
	}
	else {	
	  GCPtr<Type> argFnType = new Type(ty_tvar, arg);
	  addSubCst(arg, argInfType, argFnType, tcc);
	  GCPtr<comp> nComp = new comp(argFnType);
	  fnarg->components->append(nComp);
	}
      }
      argVec->symType = fnarg;      
      
      GCPtr<AST> ret = ast->child(1);
      TYPEEQINFER(ret, lamGamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> retInfType = ast->child(1)->getType();
      GCPtr<Type> retFnType = new Type(ty_tvar, ret);
      addSubCst(ret, retFnType, retInfType, tcc);
      
      ast->symType = new Type(ty_fn, ast, fnarg, retFnType);
      PRINT(errStream, ast, tcc);
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
      ast->symType = new Type(ty_tvar, ast); 
      
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail, USE_MODE, TI_COMP2);
      GCPtr<Type> t = ast->child(0)->symType->getType();
      GCPtr<Type> innerT = ast->child(0)->symType->getBareType();
      
      switch(innerT->kind) {
      case ty_tvar:
	{
	  GCPtr<Type> fn = buildFnFromApp(ast, uflags);
	  addSubCst(ast->child(0), t, fn, tcc);
	  t = fn;
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
	    
	    GCPtr<Type> fnArgType = targ->CompType(i);
	    GCPtr<Type> argType = arg->symType;
	    
	    if(targ->CompFlags(i) & COMP_BYREF) {
	      // by-ref arguments need strict compatibality.
	      addEqCst(arg, argType, fnArgType, tcc);
	    }
	    else {
	      // by-value arguments can have copy-compatibility.
	      addSubCst(arg, argType, fnArgType, tcc); 
	    }
	  }
	  
	  GCPtr<Type> retType = t->CompType(1);
	  addSubCst(ast, retType, ast->symType, tcc);
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

      PRINT(errStream, ast, tcc);
      break;
    }

  case at_if:
    {
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);      
      
      addSubCst(ast->child(0), ast->child(0)->symType,
		new Type(ty_bool, ast->child(0)), tcc); 
      
      // match agt_expr
      TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);

      // match agt_expr
      TYPEEQINFER(ast->child(2), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);
      
      // Type of the full expression
      ast->symType = new Type(ty_tvar, ast);
      
      // I am not using addCcCst() because it uses new type variables 
      // on every invocation.
      GCPtr<Type> latticeTop = new Type(ty_tvar, ast);
      addSubCst(ast->child(1), ast->child(1)->symType,
		latticeTop, tcc);
      addSubCst(ast->child(2), ast->child(2)->symType,
		latticeTop, tcc);
      addSubCst(ast, ast->symType,
		latticeTop, tcc);
      
      PRINT(errStream, ast, tcc);
      break;
    }

  case at_setbang:
    {
      ast->symType = new Type(ty_unit, ast);
      
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> t = ast->child(0)->symType->getType();
      addSubCst(ast->child(0), ast->child(0)->symType, 
		new Type(ty_mutable, 
			 new Type(ty_tvar, ast->child(0))), tcc);
      
      // match agt_expr
      TYPEEQINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      addSubCst(ast->child(1), ast->child(0)->symType, 
		ast->child(0)->symType, tcc);
      
      PRINT(errStream, ast, tcc);
      break;
    }

  case at_dup:
    {
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> copyType = new Type(ty_tvar, ast->child(0));
      ast->symType = new Type(ty_ref, copyType);
      
      addCcCst(ast->child(0), copyType, ast->child(0)->symType, tcc);
      
      PRINT(errStream, ast, tcc);
      break;      
    }

  case at_deref:
    {
      // match agt_expr
      TYPEEQINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      ast->symType = new Type(ty_tvar, ast);
      GCPtr<Type> expectType = new Type(ty_ref, ast->symType);
      
      addSubCst(ast->child(0), ast->child(0)->symType,
		expectType, tcc);
      
      PRINT(errStream, ast, tcc);
      break;
    }
    
  } /* switch */
  
  return errFree;  
}

