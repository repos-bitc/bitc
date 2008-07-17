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
#include <sstream>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/avl.hxx>
#include <assert.h>
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "Pair.hxx"
#include "inter-pass.hxx"
#include "Unify.hxx"
#include <libsherpa/BigNum.hxx>
#include "TypeInferUtil.hxx"

using namespace sherpa;
using namespace std;

/**************************************************************/
/*                     Some Declarations                      */
/**************************************************************/

//WARNING: **REQUIRES** errFree.
#define TYPEINFER(ast, gamma, instEnv, impTypes, isVP, tcc,	\
		  uflags, trail, mode, flags)			\
  do {								\
    CHKERR((errFree),						\
	   (typeInfer(errStream, (ast), (gamma), (instEnv),	\
		      (impTypes), (isVP), (tcc), (uflags),	\
		      (trail), (mode), (flags))));		\
  }while(0)

bool
typeInfer(std::ostream& errStream, GCPtr<AST> ast, 
	  GCPtr<Environment<TypeScheme> > gamma,
	  GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	  GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
	  bool isVP, 
	  GCPtr<TCConstraints> tcc,
	  unsigned long uflags,
	  GCPtr<Trail> trail,
	  int mode,
	  unsigned flags);

bool isExpansive(std::ostream& errStream, 
		 GCPtr<const Environment<TypeScheme> > gamma,
		 GCPtr<AST> ast);

bool isExpansive(std::ostream& errStream, 
		 GCPtr<const Environment<TypeScheme> > gamma,
		 GCPtr<Type> typ);

bool
generalizePat(std::ostream& errStream,
	      const sherpa::LexLoc &errLoc,
	      GCPtr<Environment<TypeScheme> > gamma,
	      GCPtr<const Environment< CVector<GCPtr<Instance> > > > instEnv,
	      GCPtr<AST> bp, GCPtr<AST> expr,
	      GCPtr<TCConstraints> tcc,
	      GCPtr<TCConstraints> parentTCC,
	      GCPtr<Trail> trail);


/**************************************************************/
/*                     Some Helper Functions                  */
/**************************************************************/

/* Some of the following fure repeated (and marked static) in both 
   inference routines due to the use/non-use of maybe types */

static GCPtr<Type> 
buildFnFromApp(GCPtr<AST> ast, unsigned long uflags)
{
  assert(ast->astType == at_apply);
  GCPtr<Type> targ = new Type(ty_fnarg);
  for (size_t i = 1; i < ast->children->size(); i++) {
    GCPtr<Type> argi = MBF(newTvar());
    GCPtr<comp> ncomp = new comp(argi);
    ncomp->flags |= COMP_BYREF_P;
    targ->components->append(ncomp);
  }
  
  GCPtr<Type> ret = MBF(newTvar());
  GCPtr<Type> fn = new Type (ty_fn, targ, ret);
  return fn;
}

static GCPtr<TypeScheme> 
bindIdentDef(GCPtr<AST> ast, 
	     GCPtr<Environment<TypeScheme> > gamma,
	     unsigned long bindFlags,
	     unsigned long flags)
{
  if(!ast->symType) {
    if(ast->Flags & ID_IS_TVAR)
      ast->symType = newTvar();
    else
      ast->symType = MBF(newTvar()); 
  }

  GCPtr<TypeScheme> sigma = new TypeScheme(ast->symType, ast);
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
  
  // Need to copy structure/union definitions at all times even though
  // there might not be any type variables. 
  // The ID_IS_CTOR test alone does not capture union type
  // definitions. It is only set for the structre constructor, each
  // constructor of a union definition and excception constructors.
  GCPtr<TypeScheme> ins = NULL;
  if(ast->Flags & ID_ENV_COPY) 
    ins = sigma->ts_instance_copy();
  else
    ins = sigma->ts_instance();
  
  ins->tau->fixupFnTypes();
  return ins;
}

static bool
findField(std::ostream& errStream, 
	  GCPtr<Type> t, GCPtr<AST> fld, GCPtr<Type> &fType)
{
  t = t->getBareType();
  for(size_t i=0; i < t->components->size(); i++)
    if(t->CompName(i) == fld->s) {
      fType = t->CompType(i);
      return true;
    }
	  
  errStream << fld->loc << ": "
	    << " Unknown field " << fld->s
	    << " in structure "
	    << t->defAst->s 
	    << std::endl;
  fType = 0;
  return false;
}


static bool
findComponent(std::ostream& errStream, 
	      GCPtr<Type> sut, GCPtr<AST> ast, GCPtr<Type> &fct)
{
  sut = sut->getType();
  assert(ast->astType == at_select || 
	 ast->astType == at_sel_ctr || 
	 ast->astType == at_fqCtr);
  fct = NULL;

  if(sut->isUType())
    sut = obtainFullUnionType(sut)->getType();
  
  if(sut->components->size() == 0) {
    errStream << ast->loc << ": "
	      << "cannot dereference fields as only "
	      << "an opaque declaration is available."
	      << std::endl;
    return false;
  }
  
  bool valid=false;
  for(size_t i=0; i < sut->components->size(); i++)
    if(sut->CompName(i) == ast->child(1)->s) {
      fct = sut->CompType(i)->getType();	  
      valid = ((sut->CompFlags(i) & COMP_INVALID) == 0);
      break;
    }
      
  if(!fct) {
    errStream << ast->loc << ": "
	      << " In the expression " << ast->asString() << ", "
	      << " structure/constructor " << sut->defAst->s 
	      << " has no Field/Constructor named " 
	      << ast->child(1)->s << "." << std::endl;
    return false;
  } 

  if(!valid) {
    errStream << ast->child(0)->loc << ": "
	      << " The expression " << ast->asString()
	      << " has no field " 
	      << ast->child(1)->s << "." << std::endl;

    fct = NULL;
    return false;
  }

  return true;
}

static bool
ProcessLetExprs(std::ostream& errStream, GCPtr<AST> lbs, 
		GCPtr<Environment<TypeScheme> > gamma,
		GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
		GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
		bool isVP, GCPtr<TCConstraints> tcc,
		unsigned long uflags, GCPtr<Trail> trail,
		int mode, unsigned flags)
{
  bool errFree = true;
  for (size_t c = 0; c < lbs->children->size(); c++) {
    GCPtr<AST> lb = lbs->child(c);
    GCPtr<AST> expr = lb->child(1);
    TYPEINFER(expr, gamma, instEnv, impTypes, isVP, tcc,
	      uflags, trail, USE_MODE, TI_COMP2);
  }
  return errFree;
}

static bool
ProcessLetBinds(std::ostream& errStream, GCPtr<AST> lbs, 
		GCPtr<Environment<TypeScheme> > gamma,
		GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
		GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
		bool isVP, GCPtr<TCConstraints> tcc,
		unsigned long uflags, GCPtr<Trail> trail,
		int mode, unsigned flags)
{
  bool errFree = true;
  for (size_t c = 0; c < lbs->children->size(); c++) {
    GCPtr<AST> lb = lbs->child(c);
    GCPtr<AST> idPat = lb->child(0);
    
    TYPEINFER(idPat, gamma, instEnv, impTypes, isVP, tcc,
	      uflags, trail, REDEF_MODE, TI_COMP2);
  }
  return errFree;
}

static bool
UnifyLetBinds(std::ostream& errStream, GCPtr<AST> lbs,
	      unsigned long uflags, GCPtr<Trail> trail)
{
  bool errFree = true;
  for (size_t c = 0; c < lbs->children->size(); c++) {
    GCPtr<AST> lb = lbs->child(c);
    GCPtr<AST> id = lb->getID();
    GCPtr<AST> expr = lb->child(1);
    
    // Note: It is safe to say MBF(id->symType) because
    // bindIdentDef() introduces identifiers with MBF()
    // always.
    CHKERR(errFree, unify(errStream, trail, id->loc, expr->symType,
			  MBF(id->symType), uflags));
    lb->symType = id->symType;
  }
  return errFree;
}

static void
makeLetGather(GCPtr<AST> lbs, GCPtr<AST> &bAst, GCPtr<AST> &vAst)
{
  // Because all types in a letrec share a context, we need a
  // container form to glue things together temporarily.
  // The following loop constructs the container in reversed
  // order, but that is okay, because the container only exists
  // to let mutual recursion unify correctly.
  bAst = new AST(at_letGather, lbs->child(0)->loc);
  vAst = new AST(at_letGather, lbs->child(0)->loc);
  GCPtr<Type> bType = new Type(ty_letGather);
  GCPtr<Type> vType = new Type(ty_letGather);
  
  for (size_t c = 0; c < lbs->children->size(); c++) {
    GCPtr<AST> lb = lbs->child(c);
    
    bAst->addChild(lb->child(0));
    vAst->addChild(lb->child(1));
    bType->components->append(new comp(lb->getID()->symType));
    vType->components->append(new comp(lb->child(1)->symType));
  }
  
  bAst->symType = bType;
  vAst->symType = vType;
}

	
/**************************************************************/
/*                Type consistency checkng                   */
/**************************************************************/

bool
checkImpreciseTypes(std::ostream& errStream, 
		    const GCPtr<Environment<TypeScheme> > gamma,
		    GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes)
{
  bool errFree = true;
  for(size_t i=0; i<impTypes->size(); i++) {
    GCPtr<AST> ast = impTypes->elem(i)->snd;
    GCPtr<Type> t = impTypes->elem(i)->fst->getBareType();
    switch(t->kind) {
    case ty_array:
      {
	if(t->arrlen->len == 0) {
	  errStream << ast->loc << ": "
		    << "Type " << t->asString() 
		    << " is not precise enough "
		    << "to be instantiable."
		    << std::endl;
	  errFree = false;
	}
	break;
      }
      
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

#ifdef KEEP_BF
    case ty_bitfield:
#endif
    case ty_structv:
      //     case ty_unionr:
      //     case ty_structr:
      break;

    default:
      {
	errStream << ast->loc << ": "
		  << "Internal Compiler Error. "
		  << "checkImpreciseTypes obtained "
		  << t->asString() << " type."
		  << std::endl;
	errFree = false;
	break;
      }
    }
  }
  
  return errFree;
}

static bool
checkConstraints(std::ostream& errStream, 
		 const GCPtr<TypeScheme> defSigma,
		 const GCPtr<TypeScheme> declSigma,
		 const GCPtr<AST> declAst)
{
  bool errFree = true;
  
  GCPtr<TCConstraints> defTcc = new TCConstraints;
  GCPtr<TCConstraints> declTcc = new TCConstraints;
  unsigned long unmatched = MARK20;

  defSigma->addConstraints(defTcc);
  declSigma->addConstraints(declTcc);

  if(defTcc->pred->size() != declTcc->pred->size()) 
    errFree = false;
  
  for(size_t i = 0; errFree && i < defTcc->pred->size(); i++)
    defTcc->Pred(i)->mark |= unmatched;
  for(size_t j = 0; errFree && j < declTcc->pred->size(); j++)
    declTcc->Pred(j)->mark |= unmatched;

  for(size_t i = 0; errFree && i < defTcc->pred->size(); i++) {
    GCPtr<Typeclass> defct = defTcc->Pred(i);
      
    if((defct->mark & unmatched) == 0)
      continue;
      
    bool unified = false;
      
    for(size_t j = 0; errFree && j < declTcc->pred->size(); j++) {
      GCPtr<Typeclass> declct = declTcc->Pred(j);

      if((defct->mark & unmatched) == 0)
	continue;

      if(defct->strictlyEquals(declct)) {
	defct->mark &= ~unmatched;
	declct->mark &= ~unmatched;
	unified = true;
	break;
      }
    }
    	
    if(!unified)
      errFree = false;
  }

  for(size_t i = 0; errFree && i < defTcc->pred->size(); i++)
    if(defTcc->Pred(i)->mark & unmatched)
      errFree = false;
  for(size_t j = 0; errFree && j < declTcc->pred->size(); j++)
    if(declTcc->Pred(j)->mark & unmatched)
      errFree = false;  

  if(!errFree) {
    errStream << declAst->loc << ": For the declaration of `" 
	      << declAst->s << "', the constraints "
	      << "on the declaration here, do not match "
	      << "with the definition."
	      << " Declaration: "
	      << declSigma->asString()
	      << " Definition: "
	      << defSigma->asString()
	      << std::endl;
  }
  
  return errFree;
}

/* Checks the types of:
     - A definition vs Declaration
     - A declaration vs a previous declaration
   for EXACT compatibility */
static bool
matchDefDecl(std::ostream& errStream, 
	     GCPtr<Trail> trail,
	     GCPtr<const Environment<TypeScheme> > gamma,
	     GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	     GCPtr<TypeScheme> declSigma,
	     GCPtr<TypeScheme> defSigma,
	     unsigned long flags,
	     bool fnCopyCompatibility)
{
  bool errorFree = true;   
  const GCPtr<AST> decl = declSigma->ast;
  GCPtr<const AST>  def = defSigma->ast;
  
  if(flags & DEF_DECL_NO_MATCH)
    return true;  
  
  if(declSigma->ftvs->size() != defSigma->ftvs->size()) {
    errorFree = false;
  }
  else {
    
    GCPtr<TypeScheme> declTS = declSigma;
    GCPtr<TypeScheme> defTS = defSigma;
    GCPtr<Type> declT = declTS->tau->getType();
    GCPtr<Type> defT  = defTS->tau->getType();
    
    // Rigidness preservation.
    for(size_t i=0; i < defTS->ftvs->size(); i++) {
      assert(declTS->Ftv(i)->kind == ty_tvar);
      declTS->Ftv(i)->link = defTS->Ftv(i);
    }
    
    if(fnCopyCompatibility && declT->isFnxn() && defT->isFnxn()) {
      declTS = declSigma->ts_instance_copy();
      declT = declTS->tau->getType();      
      
      GCPtr<Type> argsDecl = declT->getBareType()->Args();
      GCPtr<Type> argsDef = defT->getBareType()->Args();
      if(argsDecl->components->size() == argsDef->components->size()) {
	for(size_t c=0; c < argsDecl->components->size(); c++) {	    
	  GCPtr<Type> argDecl = argsDecl->CompType(c)->minimizeMutability();
	  GCPtr<Type> argDef = argsDef->CompType(c)->minimizeMutability();
	  CHKERR(errorFree, argDecl->strictlyEquals(argDef));
	  
	  if(!errorFree) {
	    errStream << "ARG@Def " 
		      << argDef->asString(Options::debugTvP)
		      << " != "
		      << "ARG@Decl " 
		      << argDecl->asString(Options::debugTvP)
		      << std::endl;
	  }
	}
      }
      else
	errorFree = false;
      
      GCPtr<Type> retDecl = declT->getBareType()->Ret()->minimizeMutability();
      GCPtr<Type> retDef = defT->getBareType()->Ret()->minimizeMutability();
      CHKERR(errorFree, retDecl->strictlyEquals(retDef));
    }
    else {
      CHKERR(errorFree, declT->strictlyEquals(defT));
    }

    if(errorFree)
      CHKERR(errorFree, checkConstraints(errStream, defTS, declTS, decl));
  }
  
  if(!errorFree)
    errStream << def->loc <<": The type of " << def->s 
	      << " at definition/declaration "  << defSigma->asString()
	      << " does not match that of "
	      << std::endl
	      << decl->loc << ": declaration / definition "
	      << declSigma->asString() << " EXACTLY."
	      << std::endl;
  
  // unify for real. At this point, it may be reasonable to 
  // do this in one step using ONLY UNIFY_STRICT flag
  //CHKERR(errorFree, unify(errStream, trail, gamma, decl,
  //			      decl->symType, 
  //			      sigma->tau, UNIFY_STRICT));
  
  return errorFree;
}


/**************************************************************/
/****                   MAIN INFERENCE ROUTINES            ****/
/**************************************************************/

bool
InferTvList(std::ostream& errStream, GCPtr<AST> tvList, 
	    GCPtr<Environment<TypeScheme> > gamma,
	    GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	    GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
	    bool isVP, 
	    GCPtr<TCConstraints> tcc,
	    unsigned long uflags,
	    GCPtr<Trail> trail,
	    int mode, unsigned flags,
	    GCPtr<Type> container)  
{  
  bool errFree = true;
  for(size_t i = 0; i < tvList->children->size(); i++) {
    GCPtr<AST> tv = tvList->child(i);
    TYPEINFER(tv, gamma, instEnv, impTypes, isVP, 
	      tcc, uflags, trail, DEF_MODE, TI_TYP_EXP);
    GCPtr<Type> tvType = tv->symType->getType();
    assert(tvType->kind == ty_tvar);
    tvType->flags |= TY_RIGID;
    container->typeArgs->append(tvType);
  }

  return errFree;
}


// Tvs are not added to the typeScheme in InferTvList itself because
// this has to be done after all the components (fields/constructors)
// of a type have been processed. Otherwise, the type will be
// polymorphic within itself.
static void
addTvsToSigma(std::ostream& errStream, GCPtr<AST> tvList, 
	      GCPtr<TypeScheme> sigma, GCPtr<Trail> trail)  
{  
  for(size_t i = 0; i < tvList->children->size(); i++) {
    GCPtr<AST> tv = tvList->child(i);
    GCPtr<Type> tvType = tv->symType->getType();
    assert(tvType->kind == ty_tvar);
    sigma->ftvs->append(tvType);
  }
}

// In case of value type definitions, mark all those  
// type arguments that are candidiates for copy-compatibility.
static void
markCCC(GCPtr<Type> ct)
{
  if(!ct->isValType())
    return;

  // We first need to mark all arguments CCCOK, and then remove
  // those that are not OK so that determoneCCC can get recursive
  // type definitions right.
  for(size_t i = 0; i < ct->typeArgs->size(); i++) {
    GCPtr<Type> arg = ct->TypeArg(i)->getType();
    arg->flags |= TY_CCC;
  }
  
  for(size_t i = 0; i < ct->typeArgs->size(); i++) {
    GCPtr<Type> arg = ct->TypeArg(i)->getType();      
    if(!arg->determineCCC(ct))
      arg->flags &= ~TY_CCC;
  }
}


// Called only for definitions
bool
InferStruct(std::ostream& errStream, GCPtr<AST> ast, 
	    GCPtr<Environment<TypeScheme> > gamma,
	    GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	    GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
	    bool isVP, 
	    GCPtr<TCConstraints> tcc,
	    unsigned long uflags,
	    GCPtr<Trail> trail,
	    int mode,
	    bool isReference,
	    bool mustDefine,
	    bool mustEvalBody,
	    unsigned flags)
{
  bool errFree = true;
  size_t c;
  Kind structKind;
  
  GCPtr<AST> sIdent = ast->child(0);

  // match at_ident
  structKind = (isReference)? ty_structr : ty_structv;
   
  GCPtr<Type> st = new Type(structKind);
  st->defAst = sIdent;
  st->myContainer = sIdent;
  sIdent->symType = st;
  GCPtr<TypeScheme> sigma = new TypeScheme(st, sIdent, new TCConstraints);

  // match at_tvlist
  GCPtr<AST> tvList = ast->child(1);
  CHKERR(errFree, InferTvList(errStream, tvList, gamma, instEnv, impTypes, 
			      isVP, sigma->tcc, uflags, trail, DEF_MODE, 
			      TI_TYP_EXP, st));
  sIdent->scheme = sigma;

  // Type all constraints
  TYPEINFER(ast->child(5), gamma, instEnv, impTypes, isVP, 
	    sigma->tcc, uflags, trail,  mode, TI_CONSTR);
  
  GCPtr<TypeScheme> declTS = gamma->getBinding(sIdent->s);
  unsigned long bindFlags = 0;
  if(declTS) {
    declTS->tau->getBareType()->defAst = sIdent;
    bindFlags = BF_REBIND;
  }
  gamma->addBinding(sIdent->s, sigma);
  gamma->setFlags(sIdent->s, bindFlags);

  // Ignore the category

  // match at_declares
  TYPEINFER(ast->child(3), gamma, instEnv, impTypes, isVP, sigma->tcc,
	    uflags, trail,  mode, TI_NONE);
    
  // match at_fields
  GCPtr<AST> fields = ast->child(4);
  for(c = 0; c < fields->children->size(); c++) {
    // match at_ident
    // match agt_type
    GCPtr<AST> field = fields->child(c);
    TYPEINFER(field, gamma, instEnv, impTypes, isVP, 
	      sigma->tcc, uflags, trail,  USE_MODE, TI_COMP1);
    
    switch(field->astType) {
    case at_field:
      {
	st->components->append(new comp(field->child(0)->s,
					field->child(1)->symType));
	break;
      }
      
    case at_fill:
    case at_reserved:
      {
	ast->total_fill += field->field_bits;
	break;
      }
    default:
      {
	assert(false);
	break;
      }
    }
  }

  // Add Ftvs so that they get generalized in future uses
  addTvsToSigma(errStream, tvList, sigma, trail); 
  
  // Mark that the structure must always be copied.
  // It is important that this step be done late so that the recursive
  // uses do not prompt copy.
  sIdent->Flags |= ID_ENV_COPY;

  // In case of value type definitions, mark all those  
  // type arguments that are candidiates for copy-compatibility.
  markCCC(st);
  
  // Set the main AST's type.
  ast->symType = sIdent->symType;
   
  // Solve current Predicates.
  CHKERR(errFree, sigma->solvePredicates(errStream, ast->loc,
					 instEnv, trail)); 

  // Ensure that the definition matches the declarations
  if(declTS)
    CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
				 declTS, sigma, uflags, false));
  
  return errFree;
}


// Called only for definitions
bool
InferUnion(std::ostream& errStream, GCPtr<AST> ast, 
	   GCPtr<Environment<TypeScheme> > gamma,
	   GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	   GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
	   bool isVP, 
	   GCPtr<TCConstraints> tcc,
	   unsigned long uflags,
	   GCPtr<Trail> trail,
	   int mode,
	   bool isReference,
	   bool mustDefine,
	   bool mustEvalBody,
	   unsigned flags)
{

  bool errFree = true;
  size_t c;
  Kind unionKind;
  
  GCPtr<AST> uIdent = ast->child(0);

  // match at_ident
  unionKind = (isReference)? ty_unionr : ty_unionv;
  
  GCPtr<Type> ut = new Type(unionKind);
  ut->defAst = uIdent;
  ut->myContainer = uIdent;
  uIdent->symType = ut;
  GCPtr<TypeScheme> sigma = new TypeScheme(ut, uIdent, new TCConstraints);
  
  // match at_tvlist
  GCPtr<AST> tvList = ast->child(1);
  CHKERR(errFree, InferTvList(errStream, tvList, gamma, instEnv, impTypes, 
			      isVP, sigma->tcc, uflags, trail, DEF_MODE, 
			      TI_TYP_EXP, ut));
  uIdent->scheme = sigma;
  
  // Type all constraints
  TYPEINFER(ast->child(5), gamma, instEnv, impTypes, isVP, 
	    sigma->tcc, uflags, trail,  mode, TI_CONSTR);
  
  GCPtr<TypeScheme> declTS = gamma->getBinding(uIdent->s);
  unsigned long bindFlags = 0;
  
  if(declTS) {
    declTS->tau->getType()->defAst = uIdent;
    bindFlags = BF_REBIND;
  }
  gamma->addBinding(uIdent->s, sigma);
  gamma->setFlags(uIdent->s, bindFlags);
  
  // Ignore the category
  
  // match at_declares
  GCPtr<AST> declares = ast->child(3);
  TYPEINFER(declares, gamma, instEnv, impTypes, isVP, sigma->tcc,
	    uflags, trail,  mode, TI_NONE);
  
  
  // match at_constructors
  GCPtr<AST> ctrs = ast->child(4);
  for(c = 0; c < ctrs->children->size(); c++) {
    // match at_ident
    // match agt_type
    GCPtr<AST> ctr = ctrs->child(c);
    GCPtr<AST> ctrId = ctr->child(0);    
    // Careful: 
    // Constructors with components are typed ucon 
    // and those without any are typed uval.
    Kind ctrKind;

    if(ctr->children->size() > 1)
      ctrKind = (isReference) ? ty_uconr : ty_uconv;
    else
      ctrKind = (isReference) ? ty_uvalr : ty_uvalv;
    
    ctrId->symType = new Type(ctrKind);
    ctrId->symType->defAst = ctrId;
    ctrId->symType->myContainer = uIdent;
    ctr->symType = ctrId->symType;
    
    for(size_t i = 1; i < ctr->children->size(); i++) {
      GCPtr<AST> field = ctr->child(i);
      TYPEINFER(field, gamma, instEnv, impTypes, isVP, 
		sigma->tcc, uflags, trail,  USE_MODE, TI_TYP_EXP);
      

      switch(field->astType) {
      case at_field:
	{
	  GCPtr<comp> nComp = new comp(field->child(0)->s,
				       field->child(1)->symType);
	  if(field->Flags2 & FLD_IS_DISCM)
	    nComp->flags |= COMP_UNIN_DISCM;
	  
	  ctrId->symType->components->append(nComp);
	  break;
	}
      case at_fill:
      case at_reserved:
	{
	  ctr->total_fill += field->field_bits;
	  break;
	}
      default:
	{
	  assert(false);
	  break;
	}
      }
    }
    
    // All constructors share the same type-class constraints
    // as the union. This rule -- unlike the one in Haskell -- 
    // requires that the tcc be absolutely enforced.
    GCPtr<TypeScheme> ctrSigma = new TypeScheme(ctrId->symType, 
						ctrId, sigma->tcc);
    
    // This may feel wierd -- that All constructors point to the same
    // type arguments rather than a copy. But since every instance is 
    // newly obtained, this is OK.
    for(size_t i = 0; i < tvList->children->size(); i++) {
      ctrId->symType->typeArgs->append(tvList->child(i)->symType);
    }
    
    // Don't add ctrSigma to gamma yet. Constructors are 
    // bound at the end of the definition
    ctrId->scheme = ctrSigma;
    
    GCPtr<comp> nComp = new comp(ctrId->s, ctrId->symType);
    uIdent->symType->components->append(nComp);
  } 

  // Add Ftvs so that they get generalized in future uses
  // Mark that the structure must always be copied.
  // It is important that this step be done late so that the recursive
  // uses do not prompt copy.
  addTvsToSigma(errStream, tvList, sigma, trail);
  uIdent->Flags |= ID_ENV_COPY;
  
  // In case of value type definitions, mark all those  
  // type arguments that are candidiates for copy-compatibility.
  markCCC(ut);
  
  // Solve current Predicates.
  CHKERR(errFree, sigma->solvePredicates(errStream, ast->loc,
					 instEnv, trail)); 
  
  //Now add all constructor bindings to the environment.
  for(c = 0; c < ctrs->children->size(); c++) {
    GCPtr<AST> ctr = ctrs->child(c);
    GCPtr<AST> ctrId = ctr->child(0);   
    addTvsToSigma(errStream, tvList, ctrId->scheme, trail);
    gamma->addBinding(ctrId->s, ctrId->scheme);
    ctrId->Flags |= ID_ENV_COPY;   
    
    // Solve current Predicates.
    // Since we all share the same constraints, automatically solved.
  }

  //Build structure types for all constructors.
  for(c = 0; c < ctrs->children->size(); c++) {
    GCPtr<AST> ctr = ctrs->child(c)->child(0);
    GCPtr<Type> ctrType = ctr->symType->getType();
    GCPtr<TypeScheme> ctrSigma = ctr->scheme;
    GCPtr<Type> sType = NULL;
    GCPtr<TypeScheme> stSigma = NULL;
    
    for(size_t i=0; i < c; i++) {
      GCPtr<AST> thatCtr = ctrs->child(i)->child(0);
      GCPtr<Type> thatCtrType = thatCtr->symType->getType();
      
      if(ctrType->components->size() != 
	 thatCtrType->components->size())
	continue;
      
      // Since there can be no constructors with only fills
      // and no field names are repeated, it is sufficient
      // to check types regardless of fills.
      bool same = true;
      for(size_t j=0; j < ctrType->components->size(); j++) {	
	GCPtr<comp> thisComp = ctrType->Component(j);
	GCPtr<comp> thatComp = thatCtrType->Component(j);
	
	if((thisComp->name != thatComp->name) ||
	   !thisComp->typ->strictlyEquals(thatComp->typ)) {
	  same = false;
	  break;
	}
      }
	 
      if(same) {
	assert(thatCtr->stSigma);
	stSigma = thatCtr->stSigma;
	ctr->stSigma = thatCtr->stSigma;
	ctr->stCtr = thatCtr;
	break;
      }
    }
    
    if(!stSigma) {
      sType = new Type(ty_structv);
      sType->defAst = ctr; // structures have names like (cons 'a)
      for(size_t i=0; i < ctrType->components->size(); i++)
	sType->components->append(new comp(ctrType->CompName(i),
					  ctrType->CompType(i)));
      // Comp's Flags are not necesary here ?
 
      
      for(size_t i=0; i < ctrType->typeArgs->size(); i++)
	sType->typeArgs->append(ctrType->TypeArg(i));
      
      stSigma = new TypeScheme(sType, ctr, sigma->tcc); 
      for(size_t i=0; i < ctrSigma->ftvs->size(); i++)
	stSigma->ftvs->append(ctrSigma->Ftv(i));

      ctr->stCtr = ctr;
      ctr->stSigma = stSigma;
    }

    assert(ctr->stSigma);
    /* No need to add anything to the environment, these structures
       i) Have the same name as the constructor
       ii) Are accessible through the stSigma field.

       If necessary, add them with some other name */
  }
  
  // Deal with tag-type declaration and Cardelli Optimization

  /* If we are dealing with defrepr, don't perform any
     optimization */ 
  if(ast->Flags2 & UNION_IS_REPR) {
    if(declares->tagType) {
      errStream << ast->loc << ": "
		<< "tag-type declarations cannot be "
		<< "given with defreprs."
		<< std::endl;
      errFree = false;      
    }
  }
  else if(errFree) {  

    //Check if we can do Cardelli Optimization:
    
    unsigned long long maxCtrs = 0;  
    size_t lastTagValue = (ctrs->children->size() - 1);
    size_t lastTagValueCardelli = lastTagValue;

    if(declares->tagType) {
      maxCtrs = (((unsigned long long)1) << declares->nBits());

      if(lastTagValue > (maxCtrs - 1)) {
	errStream << ast->loc << ": "
		  << "Not enough bits in the tag-type to represent "
		  << "all Constructors. Use a bigger tag-type. "
		  << "[If no tag-type declaration is found, "
		  << "the defalut is `word']"
		  << std::endl;
	errFree = false;
      }      
    }
    else if(ctrs->children->size() == 1) {
      declares->tagType = new Type(ty_word);
      assert(declares->field_bits == 0);
      uIdent->Flags |= SINGLE_LEG_UN;
    }
    else {      
      declares->tagType = new Type(ty_word);     
      assert(declares->field_bits == 0);

      maxCtrs = (((unsigned long long)1) << declares->nBits());          
      
      bool cardelli = true;
      bool seenRef = false;
      bool isEnum = true;
      
      for(size_t c = 0; 
	  cardelli && (c < ctrs->children->size()); 
	  c++) {
	GCPtr<AST> ctr = ctrs->child(c);
	
	switch(ctr->children->size()) {
	case 0:
	  assert(false);
	  break;

	case 1:
	  break;
	  
	case 2:
	  isEnum = false;

	  if(seenRef) {
	    cardelli = false;
	    break;
	  }
	  
	  if(ctr->child(1)->symType->isRefType())
	    seenRef = true;
	  else
	    cardelli = false;
	    
	  break;
	  
	default:
	  isEnum = false;
	  cardelli = false;
	  break;	   
	}	  
      }
     
      // The (nullable 'a) union is declared in the preamble and
      // requires special handling for tag numbering.
      bool isNullable = (cardelli && uIdent->s == "nullable");

      if(isEnum) {
	assert(!seenRef);
	cardelli = false;
	uIdent->Flags |= ENUM_UN;
      }
      else if(cardelli) {	
	assert(!isEnum);
	uIdent->Flags |= CARDELLI_UN;
	if (isNullable)
	  uIdent->Flags |= NULLABLE_UN;
	lastTagValueCardelli = (2 * lastTagValue) - 1;
      }
      
      UNION_INF_DEBUG
	errStream << "Union " << uIdent->s << ": " 
		  << std::endl
		  << "  nBits = " << declares->tagType->nBits() 
		  << std::endl 
		  << "  maxCtrs = " << maxCtrs 
		  << std::endl
		  << "  ltv = " << lastTagValue 
		  << std::endl
		  << "  ltvC = " << lastTagValueCardelli 
		  << std::endl
		  << "  isEnum = " << isEnum 
		  << std::endl
		  << "  cardelli = " << cardelli 
		  << std::endl
		  << "  nullable = " << isNullable
		  << std::endl;
    }
    
    uIdent->tagType = declares->tagType;
    uIdent->field_bits = declares->field_bits;
  }
    
  // Set the main AST's type.
  ast->symType = uIdent->symType;

  // Ensure that the definition matches the declarations
  if(declTS)
    CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
				 declTS, sigma, uflags, false));
  
  return errFree;
}

bool
superDAG(GCPtr<AST> super, GCPtr<AST> curr)
{
  if(super ==  curr)
    return false;
  
  assert(super->scheme);
  GCPtr<TCConstraints> tcc = super->scheme->tcc;
  assert(tcc);

  for(size_t c = 0; c < tcc->pred->size(); c++) {
    GCPtr<Typeclass> pred = tcc->Pred(c);
    if(pred->flags & TY_CT_SELF)
      continue;

    if(superDAG(pred->defAst, curr) == false)
      return false;
  }
  return true;
}

bool
InferTypeClass(std::ostream& errStream, GCPtr<AST> ast, 
	       GCPtr<Environment<TypeScheme> > gamma,
	       GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	       GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
	       bool isVP, 
	       GCPtr<TCConstraints> tcc,
	       unsigned long uflags,
	       GCPtr<Trail> trail,
	       int mode,
	       unsigned flags)
{
  bool errFree = true;
  GCPtr<AST> ident = ast->child(0);
  GCPtr<Typeclass> tc = new Typeclass(ty_typeclass);
  tc->defAst = ident;
  GCPtr<TypeScheme> sigma = new TypeScheme(tc, ident, new TCConstraints);
  tc->flags |= TY_CT_SELF;
  sigma->tcc->addPred(tc);

  GCPtr<AST> tvList = ast->child(1);
  CHKERR(errFree, InferTvList(errStream, tvList, gamma, instEnv, impTypes, 
			      isVP, sigma->tcc, uflags, trail, DEF_MODE, 
			      TI_TYP_EXP, tc));
  addTvsToSigma(errStream, tvList, sigma, trail);
  ident->symType = tc;
  ident->scheme = sigma;
  
  // Type all constraints
  TYPEINFER(ast->child(4), gamma, instEnv, impTypes, isVP, 
	    sigma->tcc, uflags, trail,  mode, 
	    TI_CONSTR | TI_TCC_SUB);

  // Typeclass Declarations
  GCPtr<AST> tcdecls = ast->child(2);
  for(size_t c = 0; c < tcdecls->children->size(); c++) {
    GCPtr<AST> tcdecl = tcdecls->child(c);
    assert(tcdecl->astType == at_tyfn);
    GCPtr<AST> domain = tcdecl->child(0);	       
    GCPtr<AST> range =  tcdecl->child(1);
    GCPtr<Type> tyfn = new Type(ty_tyfn);
    tyfn->defAst = tcdecl;
    TYPEINFER(domain, gamma, instEnv, impTypes, isVP, sigma->tcc, 
	      uflags, trail, USE_MODE, TI_TYP_EXP);
    TYPEINFER(range, gamma, instEnv, impTypes, isVP, sigma->tcc, 
	      uflags, trail, USE_MODE, TI_TYP_EXP);
    tyfn->components->append(new comp(domain->symType));
    tyfn->components->append(new comp(range->symType));
    
    //errStream << "***"
    //          << domain->asString() << ": "
    // 	        << domain->symType->asString(NULL) 
    // 	        << std::endl
    // 	        << range->asString() << ": "
    // 	        << range->symType->asString(NULL)
    // 	        << std::endl
    // 	        << "tyfn = " << tyfn->asString(NULL)
    // 	        << std::endl;
    tc->addFnDep(tyfn);
  }

  GCPtr<AST> methods = ast->child(3);
  for(size_t c = 0; c < methods->children->size(); c++) {
    GCPtr<AST> method = methods->child(c);
    GCPtr<AST> mID = method->child(0);
    GCPtr<AST> mtType = method->child(1);
    
    TYPEINFER(mtType, gamma, instEnv, impTypes, isVP, sigma->tcc,
	      uflags, trail,  USE_MODE, TI_TYP_EXP);
    mID->symType = mtType->symType;
    
    GCPtr<Type> mType = mID->symType->getType();
    mType->defAst = mID;
    mType->myContainer = ident;
    
    GCPtr<TypeScheme> mSigma = new TypeScheme(mType, mID, 
					      new TCConstraints);
    for(size_t i=0; i < sigma->tcc->pred->size(); i++)
      mSigma->tcc->addPred(sigma->tcc->Pred(i));
 
    do { // Dummy loop
      if(!mType) {
	assert(!errFree); 
	break;
      }
	
      if(mType->kind != ty_fn) {
	errStream << ast->loc << ": " 
		  << "The type of \"method\" " << mID->s
		  << "was infered as " << mType->asString()
		  << ", but all methods must have a function type."
		  << std::endl;
 	errFree = false;
	break;
      }

      mSigma->collectAllFtvs();
      
      // Solve current Predicates.
      CHKERR(errFree, mSigma->solvePredicates(errStream, method->loc,
					      instEnv, trail)); 
      
      mType->myContainer = ident;
      mID->scheme = mSigma;      
      GCPtr<comp> nComp = new comp(mID->s,mType); 
      ident->symType->components->append(nComp);      
    } while(0);				   
  }

  if(!errFree)
    return false;
  
  gamma->addBinding(ident->s, ident->scheme);
  for(size_t c = 0; c < methods->children->size(); c++) {
    GCPtr<AST> method = methods->child(c);
    GCPtr<AST> mID = method->child(0);
    gamma->addBinding(mID->s, mID->scheme);
  }
  
  assert(!instEnv->getBinding(ident->fqn.asString()));
  instEnv->addBinding(ident->fqn.asString(), 
		      new CVector<GCPtr<Instance> >);
  ast->symType = ident->symType;
  return errFree;
}

bool
InferInstance(std::ostream& errStream, GCPtr<AST> ast, 
	      GCPtr<Environment<TypeScheme> > gamma,
	      GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	      GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
	      bool isVP, 
	      GCPtr<TCConstraints> tcc,
	      unsigned long uflags,
	      GCPtr<Trail> trail,
	      int mode,
	      unsigned flags)
{
  bool errFree = true;
  
  GCPtr<AST> tcapp = ast->child(0);
  GCPtr<AST> methods = ast->child(1);
  GCPtr<AST> constraints = ast->child(2);
  
  GCPtr<AST> TCident = tcapp;
  if(tcapp->children->size())
    TCident = tcapp->child(0);
  
  GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
  ast->envs.gamma = defGamma;
  ast->envs.instEnv = instEnv;
  
  ast->symType = new Type(ty_tvar);
  GCPtr<TCConstraints> myTcc = new TCConstraints;
  TYPEINFER(tcapp, defGamma, instEnv, impTypes, isVP,
	    myTcc, uflags, trail, USE_MODE, TI_CONSTR);

  // Mark myself
  for(size_t m=0; m < myTcc->pred->size(); m++) {
    GCPtr<Typeclass> pred = myTcc->Pred(m);
    if(pred->defAst == TCident->symbolDef)
      pred->flags |= TY_CT_SELF;
  }
  
  if(!errFree) 
    return false;
  
  // Type all constraints
  TYPEINFER(constraints, defGamma, instEnv, impTypes, isVP,
	    myTcc, uflags, trail, USE_MODE, TI_CONSTR);

  
  if(!errFree) 
    return false;
      
  GCPtr<Typeclass> tc = tcapp->symType->getType();

  // Get the set of current instances 
  GCPtr<CVector<GCPtr<Instance> > > currInsts = 
    instEnv->getBinding(tc->defAst->fqn.asString());

  
  if((uflags & ALL_INSTS_OK) == 0) {
    
    // Make sure that the instance definition is consistent
    // with the known functional dependencies.
    
    //errStream << ast->loc << ": #Preds = " << myTcc->pred->size()
    //		<< std::endl;
    for(size_t i = 0; i < myTcc->pred->size(); i++) {
      GCPtr<Typeclass> pred = myTcc->Pred(i)->getType();
      //errStream << "Processing : " << pred->asString()
      //	  << std::endl;
      if(pred->fnDeps)
	for(size_t d = 0; d < pred->fnDeps->size(); d++) {
	  GCPtr<Typeclass> fnDep =  pred->FnDep(d);
	  GCPtr< CVector<GCPtr<Type> > > domain = new CVector<GCPtr<Type> >;
	  GCPtr< CVector<GCPtr<Type> > > range = new CVector<GCPtr<Type> >;
	  fnDep->Args()->collectAllftvs(domain);
	  fnDep->Ret()->collectAllftvs(range);
	  
	  //errStream << "  Processing : " << fnDep->asString()
	  //	      << std::endl;
	  
	  for(size_t j=0; j < range->size(); j++) {
	    if(!domain->contains(range->elem(j))) {
	      errStream << ast->loc << ": "
			<< "Invalid Instance. Definition contradicts"
			<< " with the functional dependency "
			<< fnDep->asString() << " of predicate "
			<< pred->asString() << ". At least one "
			<< " type variable in the range was not in"
			<< " the domain."
			<< std::endl;
	      errFree = false;
	      break;
	    }
	  }
	}
    }

    if(!errFree) 
      return false;

    // Make sure that the instance definition does not contradict
    // with any functional dependencies in the presence of 
    // previous instance definitions
    // This is a O(n^5) loop !!! Hopefully there will not be
    // too many typeclass and fnDep constraints, and 
    // the compilation will finish in time ... 
    // The other way to think about this is that this is actually
    // a O(n^3) algorithm ranging over all instances and the 
    // functional dependencies. The other two loops occur because
    // fnDeps are stored inside predicates. 
    // Is there a better algorithm?
    for(size_t i = 0; i < currInsts->size(); i++) {
      GCPtr<Instance> inst = currInsts->elem(i);
      // Since Equals will not unify any variables in place,
      // I don't have to do a ts_instance_copy() here.
      GCPtr<TCConstraints> theirTcc = inst->ast->scheme->tcc;

      for(size_t l = 0; l < myTcc->pred->size(); l++) {
	GCPtr<Typeclass> myPred = myTcc->Pred(l)->getType();
	for(size_t m = 0; m < theirTcc->pred->size(); m++) {
	  GCPtr<Typeclass> theirPred = theirTcc->Pred(m)->getType();

	  if((myPred->defAst == theirPred->defAst) &&  
	     (myPred->fnDeps && theirPred->fnDeps))
	    for(size_t j = 0; j < myPred->fnDeps->size(); j++) {
	      GCPtr<Type> myFnDep =  myPred->FnDep(j)->getType();
	      GCPtr<Type> myDomain = myFnDep->Args();
		  
	      for(size_t k = 0; k < theirPred->fnDeps->size(); k++) {		  
		GCPtr<Type> theirFnDep =  theirPred->FnDep(k)->getType();
		GCPtr<Type> theirDomain = theirFnDep->Args();
		    
		assert(myFnDep->defAst == theirFnDep->defAst);
		    
		if( myDomain->equals(theirDomain) && 
		    !myFnDep->equals(theirFnDep)) {
		  errStream << ast->loc << ": "
			    << "The following is a contradiction: \n"
			    << inst->ast->loc << ": "
			    << "Instance definition " 
			    << inst->asString()
			    << " with Predicate "
			    << theirPred->asString() << " and "
			    << "associated functional dependency "
			    << theirFnDep->asString() << ", and\n"
			    << ast->loc << ": "
			    << "Instance definition " 
			    << tc->asString()
			    << " with Predicate "
			    << myPred->asString() << " and "
			    << "associated functional dependency "
			    << myFnDep->asString()
			    << std::endl;
		  errFree = false;
		  break;
		}
	      }
	    }
	}
      }	
    }
           
    if(!errFree) 
      return false;

  }
 
  tcapp->scheme = new TypeScheme(tc, tcapp, myTcc);
      
  size_t nMethods = tc->components->size();
  if(methods->children->size() == nMethods) {
    for(size_t i = 0; i < nMethods; i++) {
      GCPtr<Type> mtType = tc->CompType(i);
      GCPtr<AST> method = methods->child(i);
      TYPEINFER(method, defGamma, instEnv, impTypes, isVP, myTcc,
		uflags, trail, USE_MODE, TI_NONE);
      GCPtr<Type> methodType = method->symType->getType();

      // Methods are functions, remove top mutability.
      CHKERR(errFree, unify(errStream, trail, method->loc, 
			    mtType, methodType->minimizeMutability(),
			    uflags));
    }
  }
  else {
    errStream << ast->loc << ": "
	      << "Type class" << tcapp->child(0)->s
	      << " needs " << nMethods << " methods for "
	      << "instantiation, but here obtained "
	      << methods->children->size() << "."
	      << std::endl;
  }

  if(!errFree) 
    return false;

  GCPtr<TypeScheme> sigma = tcapp->scheme;

  gamma->mergeBindingsFrom(defGamma);
  sigma->generalize(errStream, ast->loc, gamma, instEnv, tcapp, 
		    NULL, trail, gen_instance);      
  
  if(!errFree)
    return false;
  
  GCPtr<Instance> myInstance = new Instance(sigma, ast);
  
  if((uflags & ALL_INSTS_OK) == 0) {
    // Make sure there are no absolute conflicts 
    // with existing instances
    assert(currInsts);
  
    for(size_t i = 0; i < currInsts->size(); i++) {
      GCPtr<Instance> inst = currInsts->elem(i);
      if(inst->equals(errStream, myInstance, instEnv)) {
	errStream << tcapp->loc << ": "
		  << "Instance declaration "
		  << sigma->asString() << " conflicts with "
		  << " previous definition at "
		  << inst->ast->loc 
		  << "(" << inst->ts->asString() << ")."
		  << std::endl;
	errFree = false;
	break;				  
      }	
    } 

    if(!errFree)
      return false; 
  }
  
  // Add current Predicate.
  currInsts->append(myInstance);
  
  //errStream << "Added " << sigma->asString() 
  //	    << " to " << tc->defAst->s << std::endl;
      
  ast->symType = tcapp->symType;
  ast->scheme =  tcapp->scheme;

  return errFree;
}
  
static bool
CheckLetrecFnxnRestriction(std::ostream &errStream, GCPtr<AST> ast)
{
  bool errFree = true;
  switch(ast->astType) {
  case at_ident:
    {
      if(!ast->symType->isFnxn() && !ast->symType->isClosure()) {
	errStream << ast->loc << ": Identifier " << ast->s
		  << " bound in a letrec, has non-function type "
		  << ast->symType->asString();
	errFree = false;	      
      }
      break;
    }
    
  case at_identPattern:
    {
      CHKERR(errFree, CheckLetrecFnxnRestriction(errStream, 
						 ast->child(0)));
      break;
    }

  case at_switch:
  case at_try:
    {
      for(size_t c=0; c < ast->children->size(); c++)
	if(c != IGNORE(ast))
	  CHKERR(errFree, CheckLetrecFnxnRestriction(errStream, 
						     ast->child(c)));
      break;
    }

  default:
    {
      for(size_t c=0; c < ast->children->size(); c++)
	CHKERR(errFree, CheckLetrecFnxnRestriction(errStream, 
						   ast->child(c)));
      break;
    }
  }
  return errFree;
}

bool
typeInfer(std::ostream& errStream, GCPtr<AST> ast, 
	  GCPtr<Environment<TypeScheme> > gamma,
	  GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	  GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes,
	  bool isVP, 
	  GCPtr<TCConstraints> tcc,
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

  //    errStream << "INF: " //<< ast->loc << ": " 
  //   	    << ast->s << "[" << ast->astTypeName() << "]" 
  //   	    << "   mode = " << mode
  // 	     << "   isInTypapp = " << ((isInTypapp)?"true":"false")
  //   	    << std::endl;
  
  switch(ast->astType) {
  case agt_expr:
  case agt_expr_or_define:
  case agt_eform:
  case at_Null:
  case at_refCat:
  case at_valCat:
  case at_opaqueCat:
  case at_methods:
  case agt_category:
  case at_AnyGroup:
  case agt_literal:
  case agt_var:
  case agt_tvar:
  case agt_definition:
  case agt_type_definition:
  case agt_value_definition:
  case agt_type:
  case at_letbindings:
  case at_dobindings:
  case at_dobinding:
  case agt_CompilationUnit:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_ow:
  case agt_qtype:
  case agt_fielditem:
  case at_ifident:
  case at_localFrame:
  case at_frameBindings:
  case at_identList:
  case agt_ucon:

  case at_defrepr:
    //case at_reprbody:
    //case agt_reprbodyitem:
    //case at_reprcase:
    //case at_reprcaselegR:
    //case at_reprtag:
  case at_reprctrs:
  case at_reprctr:
  case at_reprrepr:

    {
      errStream << ast->loc << ": Internal Compiler Error. Invalid AST type" 
		<< ast->astTypeName() << std::endl;
    
      errFree = false;
      break;
    }

  case at_boolLiteral:
    {
      /*------------------------------------------------
	  ___________________________________________
                 A |- BOOL_LITERAL: bool
	------------------------------------------------*/
      ast->symType = new Type(ty_bool);
      break;
    }

  case at_charLiteral:
    {
      /*------------------------------------------------
	  ___________________________________________
                 A |- CHAR_LITERAL: char
	------------------------------------------------*/
      ast->symType = new Type(ty_char);
      break;
    }

  case at_intLiteral:
    {
      /*------------------------------------------------
	  ___________________________________________
                 A |- INT_LITEREAL: 'a \ IntLit('a)
	------------------------------------------------*/
      
      if(Options::noPrelude) {
	ast->symType = new Type(ty_word);
	break;
      }

      if(uflags & NO_MORE_TC) {
	ast->symType = new Type(ty_tvar);
	break;
      }

      const std::string& intLit = SpecialNames::spNames.sp_integral;
      GCPtr<TypeScheme> icSigma = gamma->getBinding(intLit);
      assert(icSigma);
      
      GCPtr<Typeclass> ic = icSigma->type_instance_copy();
      assert(ic->typeArgs->size() == 1);
      ast->symType = ic->TypeArg(0)->getType();
      tcc->addPred(ic);
      break;
    }

  case at_floatLiteral:
    {
      /*------------------------------------------------
	  ___________________________________________
                 A |- FLOAT_LITERAL: 'a \ FloarLit('a)
	------------------------------------------------*/

      if(Options::noPrelude) {
	ast->symType = new Type(ty_float);
	break;
      }

      if(uflags & NO_MORE_TC) {
	ast->symType = new Type(ty_tvar);
	break;
      }

      std::string& floatLit = SpecialNames::spNames.sp_fp;
      GCPtr<TypeScheme> fcSigma = gamma->getBinding(floatLit);
      assert(fcSigma);
      
      GCPtr<Typeclass> fc = fcSigma->type_instance_copy();
      assert(fc->typeArgs->size() == 1);
      ast->symType = fc->TypeArg(0)->getType();
      tcc->addPred(fc);
      break;
    }
    
  case at_docString:
    // FIX: Not sure this is right. In truth, we really shouldn't be
    // bothering to type check these at all.
    {
      ast->symType = new Type(ty_string);

      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_NONE);
      break;
    }

  case at_stringLiteral:
    {
      /*------------------------------------------------
	  ___________________________________________
                    A |- STRING_LITERAL: string
	------------------------------------------------*/
      ast->symType = new Type(ty_string);
      break;
    }

  case at_ident:
    {
      /*------------------------------------------------
	DEF:
                EXTEND A with x:'b|'a
	  ___________________________________________
                    A |- x: 'b|'a
   
	USE:
                A(x) = forall 'a* t\C
	  ___________________________________________
                  A |- x: t['b* / 'a*]  \ C
	------------------------------------------------*/
      switch(mode) {
      case DEF_MODE:
	{
	  unsigned long bindFlags = 0;
	  GCPtr<TypeScheme> sigma = gamma->getBinding(ast->s);

	  if(sigma) {	    
	    // NOTE: none of the declaration forms make this 
	    // recursive call. 
	    assert(!ast->isDecl);

	    // All rebinding forms (lambda, let, etc) make calls
	    // through REDEF_MODE. Threfore, this case MUST be
	    // a definition for which we have already seen a 
	    // declaration.
	    bindFlags = BF_REBIND;
	    sigma = bindIdentDef(ast, gamma, bindFlags, flags);
	    ast->symType->defAst = sigma->tau->getType()->defAst = ast;
	  }
	  else {
	    sigma = bindIdentDef(ast, gamma, bindFlags, flags);	  
	  }
	  break;
	}
	
      case REDEF_MODE:
	{
	  bindIdentDef(ast, gamma, BF_REBIND, flags);
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
	      
	      ast->symType = newTvar();
	      return false;
	    }
	  }
	  
	  GCPtr<TypeScheme> tsIns =  Instantiate(ast, sigma);
	  GCPtr<Type> ins = tsIns->tau->getType();
	  ast->symType = ins;
	  
	  ID_INS_DEBUG
	    errStream << " For " << ast->s << ", "
		      << "Obtained " << ins->asString(Options::debugTvP)
		      << " From " 
		      << sigma->asString(Options::debugTvP) 
		      << std::endl;
	      
	  ins = ins->getBareType();

	  if((flags & TI_TYP_EXP) && 
	     ((flags & TI_TYP_APP) == 0) && 
	     (ins->typeArgs->size() > 0)) {
	    errStream << ast->loc << ": "
		      << ast->s << " cannot be instantiated without " 
		      << ins->typeArgs->size() << " type arguments."
		      << std::endl;
	    
	    ast->symType = newTvar();
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

  case at_module:
    {
      for(size_t c = 0; c < ast->children->size(); c++) {
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_NONE);
	// errStream << " - - - - - - - - - - - - - - - - - - - - - - - - - "
	// 	     << std::endl;
      }
      break;
    }

  case at_interface:
    {
      // match at_ident
      //    TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
      //              uflags, trail,  mode, TI_COMP2);
    
      // match agt_definition*

      for(size_t c = 1; c < ast->children->size(); c++)
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_NONE);
      break;
    }

  case at_usesel:
    {
      // Impossible to get here. Symtab rewrote this as ident.
      assert(false);
    }
    break;

  case at_use:
    {      
      ast->symType = new Type(ty_tvar);

      for(size_t c = 0; c < ast->children->size(); c++)
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_NONE);
      break;
    }

  case at_use_case:
    {
      ast->symType = new Type(ty_tvar);
      
      GCPtr<TypeScheme> sigma = gamma->getBinding(ast->child(1)->s);
      
      if(!sigma) {
	errStream << ast->loc << ": "
		  << " attempt to use " << ast->child(1)->s 
		  << ", which has an unknown, or buggy type"
		  << std::endl;
	errFree = false;
	break;
      }

      gamma->addBinding(ast->child(0)->s, sigma);
      gamma->setFlags(ast->child(0)->s, BF_PRIVATE);
      break;
    }

  case at_defunion:
    {
      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;
      
      GCPtr<AST> category = ast->child(2);
      bool isRefType = (category->astType == at_refCat);
      
      CHKERR(errFree, InferUnion(errStream, ast, defGamma, instEnv,
				 impTypes, isVP, tcc,
				 uflags, trail,  mode, isRefType, 
				 true, true, TI_NONE));
      
      gamma->mergeBindingsFrom(defGamma);      
      break;
    }

  case at_defstruct:
    {
      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      GCPtr<AST> category = ast->child(2);
      bool isRefType = (category->astType == at_refCat);

      CHKERR(errFree, InferStruct(errStream, ast, defGamma, instEnv,
				  impTypes, isVP, tcc,
				  uflags, trail,  mode, isRefType, 
				  true, true, TI_NONE));

      gamma->mergeBindingsFrom(defGamma);      
      break;
    }

  case at_declunion:
  case at_declstruct:
  case at_declrepr:
    {
      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      // match at_ident
      // FIX: (shap) Not convinced this is correct for opaque...
      Kind decl_ty;      
      switch(ast->astType) {
      case at_declunion:
	decl_ty = 
	  (ast->child(2)->astType == at_refCat) ?  ty_unionr : ty_unionv;
	break;
      case at_declstruct:
	decl_ty = 
	  (ast->child(2)->astType == at_refCat) ?  ty_structr : ty_structv;
	break;
      case at_declrepr:
	decl_ty = 
	  (ast->child(2)->astType == at_refCat) ?  ty_reprr : ty_reprv;
	break;
      default:
	die();
      }

      GCPtr<AST> ident = ast->child(0);
      ident->symType = new Type(decl_ty);
      ident->symType->defAst = ident;
      ident->symType->myContainer = ident;
      GCPtr<TypeScheme> sigma = new TypeScheme(ident->symType, ident, new TCConstraints);

      // match at_tvlist
      GCPtr<AST> tvList = ast->child(1);
      CHKERR(errFree, InferTvList(errStream, tvList, defGamma, instEnv,
				  impTypes, isVP, sigma->tcc, uflags, 
				  trail, DEF_MODE, 
				  TI_TYP_EXP, ident->symType));
      ident->scheme = sigma;

      // Category keywork at position 2

      // Type all constraints
      TYPEINFER(ast->child(3), gamma, instEnv, impTypes, isVP, 
		sigma->tcc, uflags, trail,  mode, TI_CONSTR);

      // Solve current Predicates.
      CHKERR(errFree, sigma->solvePredicates(errStream, ident->loc,
					     instEnv, trail)); 

      if (sigma->ftvs->size() && ast->getID()->externalName.size()) {
	errStream << ast->loc << ": Polymorphic declarations may not specify "
		  << "an external identifier."
		  << std::endl;
	errFree = false;
      }
    
      GCPtr<TypeScheme> ts = gamma->getBinding(ident->s);
      if(ts) {
	ident->symType->defAst = ts->tau->getType()->defAst;

	CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
				     ts, sigma, uflags, false));
      }
      else {
	defGamma->addBinding(ident->s, sigma);
	// 	cout << "Added decl for " << ident->s 
	// 	     << " with  base = " 
	//           << &(*ident->symType->defAst)
	// 	     << std::endl;
      }

      ast->symType = ident->symType;

      gamma->mergeBindingsFrom(defGamma);

      break;
    }

  case at_proclaim:
    {
      /*--------------------------------------------
	   A(x) = t1   U(t1 = t)
	_________________________
	     A |- (proclaim x:t): t 
	
	  x:t' notin A,  EXTEND A with x:t
	_______________________________
       	   A |- (proclaim x:t): t 
	------------------------------------------------*/
      
      // FIX Incompeteness Issue here
      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      GCPtr<TCConstraints> newTcc = new TCConstraints;
      GCPtr<AST> ident = ast->child(0);
      GCPtr<AST> typ = ast->child(1);
      GCPtr<AST> constraints = ast->child(2);
      assert(ident->isDecl);

      // WAS: newBindType() which had a maybe() around.
      ident->symType = newTvar();
      GCPtr<TypeScheme> sigma = new TypeScheme(ident->symType, ident);
      ident->scheme = sigma;
      
      TYPEINFER(typ, defGamma, instEnv, impTypes, isVP, newTcc,
		uflags, trail, USE_MODE, TI_TYP_EXP);
      
      CHKERR(errFree, unify(errStream, trail, ident->loc, 
			    ident->symType, 
			    typ->symType, uflags)); 
      
      TYPEINFER(constraints, defGamma, instEnv, impTypes, isVP, 
		newTcc, uflags, trail,  mode, TI_CONSTR);
      
      if(!errFree)
	break;
      
      sigma->tcc = newTcc;
      CHKERR(errFree, sigma->generalize(errStream, ast->loc, gamma,
					instEnv, ident, NULL, trail,
					gen_top)); 
      
      if(!errFree) {
	errStream << ast->loc << ": Invalid Proclaimation"
		  << " The type specified could not be"
		  << " properly generalized."
		  << std::endl;
      }
      
      if (sigma->ftvs->size() && ast->getID()->externalName.size()) {
	errStream << ast->loc << ": Polymorphic declarations may not specify "
		  << "an external identifier."
		  << std::endl;
	errFree = false;
      }

      GCPtr<TypeScheme> ts = gamma->getBinding(ident->s);
      if(ts) {
	ident->symType->defAst = ts->tau->getType()->defAst;
	CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
				     ts, sigma, uflags, false));      
      }
      else {
	defGamma->addBinding(ident->s, sigma);
      }
      
      gamma->mergeBindingsFrom(defGamma);
      ast->symType = ident->symType;      
      break;
    }

  case at_deftypeclass:
    {
      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      CHKERR(errFree, InferTypeClass(errStream, ast, defGamma, instEnv,
				     impTypes, isVP, tcc, uflags, 
				     trail, DEF_MODE, TI_NONE));
      
      gamma->mergeBindingsFrom(defGamma);
      break;
    }

  case at_tcdecls:
  case at_tyfn:
  case at_method_decls:
  case at_method_decl:
    {
      assert(false);
      break;
    }

  case at_tcapp:
    {      
      GCPtr<AST> tcIdent = ast->child(0);
      TYPEINFER(tcIdent, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail, USE_MODE, 
		flags | TI_TYP_EXP | TI_TYP_APP);
      GCPtr<Typeclass> tc = tcIdent->symType->getType();      

      if(tc->kind != ty_typeclass) {
	// This is the result of some other error
	errFree = false;
	break;
      }

      if(tc->typeArgs->size() == (ast->children->size() - 1)) {
	for(size_t i = 1; i < ast->children->size(); i++) {
	  TYPEINFER(ast->child(i), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail, USE_MODE, TI_COMP1);
	  CHKERR(errFree, unify(errStream, trail,
				ast->child(i)->loc,
				ast->child(i)->symType,
				tc->TypeArg(i-1), uflags));
	}
      }
      else {
	errStream << ast->loc << ": "
		  << "Typeclass cannot be Partially "
		  << "or over instantiated. "
		  << "Typeclass " << tc->asString()
		  << " expects " << tc->typeArgs->size() 
		  << " args, but is here applied to "
		  << (ast->children->size() - 1) << "."
		  << std::endl;
      }


      /* Special Handling for the copy-compatibility constraint 
         The copy-compat type-class constraint is transformed into a
         unification constraint using maybe-types, and the constraint
         is immediately eliminated as solved. */ 
      const std::string& copy_compat =
	SpecialNames::spNames.sp_copy_compat;
      if(tc->defAst->s == copy_compat) {
	GCPtr<Type> tv = newTvar();
	CHKERR(errFree, unify(errStream, trail,
			      ast->child(1)->loc,
			      ast->child(1)->symType,
			      MBF(tv), uflags));
	CHKERR(errFree, unify(errStream, trail,
			      ast->child(2)->loc,
			      ast->child(2)->symType,
			      MBF(tv), uflags));
	tcc->clearPred(tc);
      }

      ast->symType = tc;
      break;
    }

  case at_definstance:
    {
      CHKERR(errFree, InferInstance(errStream, ast, gamma, instEnv,
				    impTypes, isVP, tcc, uflags, trail, 
				    DEF_MODE, TI_NONE));				    
      break;
    }

  case at_defexception:
    {
      GCPtr<AST> ctr = ast->child(0);
      ctr->Flags |= ID_ENV_COPY;   

      // Maybe, we have a prior declaration?
      GCPtr<TypeScheme> declTS = gamma->getBinding(ctr->s);

      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      GCPtr<TCConstraints> myTcc = new TCConstraints;

      TYPEINFER(ctr, defGamma, instEnv, impTypes, isVP, myTcc, 
		uflags, trail, DEF_MODE, TI_TYP_EXP);      
      
      GCPtr<Type> exn = new Type(ty_exn);
      exn->defAst = ctr;      
      ctr->symType->getType()->link = exn;
      GCPtr<TypeScheme> sigma = ctr->scheme;
      sigma->tcc = myTcc;
      
      GCPtr<Type> t = ctr->symType->getType();
      for (size_t c = 1; c < ast->children->size(); c++) {
	GCPtr<AST> field = ast->child(c);	
	if(field->astType == at_fill)
	  continue;

	TYPEINFER(field, defGamma, instEnv, impTypes, 
		  isVP, sigma->tcc,
		  uflags, trail, USE_MODE, TI_TYP_EXP);
	GCPtr<Type> t1 = field->child(1)->getType();
	t->components->append(new comp(field->child(0)->s, t1));
      }

      // Build the structure type for the component structure.
      GCPtr<Type> sType = new Type(ty_structv);
      sType->defAst = ctr;
      for(size_t i=0; i < t->components->size(); i++)
	sType->components->append(new comp(t->CompName(i),
					  t->CompType(i)));
      
      ctr->stCtr = ctr;
      ctr->stSigma = new TypeScheme(sType, ctr, sigma->tcc);

      // Solve current Predicates.
      CHKERR(errFree, sigma->solvePredicates(errStream, ast->loc,
					     instEnv, trail)); 

      ast->symType = ctr->symType;

      gamma->mergeBindingsFrom(defGamma);

      if(declTS)
	CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
				     declTS, sigma, uflags, false));	
      break;
    }

  case at_define:
    {
      /*------------------------------------------------
	        t' = 'a|'b       [U(t = t')]
   	     A, x:t' |- e:t1    U(t1 = 'c|'b)	
          S = generalize(A, t', e)   EXTEND A with x:S
	_______________________________________________
    	        A |- (define x:[t] = e): t'
     ---------------------------------------------------*/
      // Maybe, we have a prior declaration?
      GCPtr<AST> ident = ast->getID();
      GCPtr<TypeScheme> declTS = gamma->getBinding(ident->s);

      GCPtr<Environment<TypeScheme> > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      GCPtr<TCConstraints> currTcc = new TCConstraints;
      
      // match agt_bindingPattern
      // match agt_expr
      TYPEINFER(ast->child(0), defGamma, instEnv, impTypes, isVP, 
		currTcc, uflags, trail, DEF_MODE, TI_NONE);

      TYPEINFER(ast->child(1), defGamma, instEnv, impTypes, isVP, 
		currTcc, uflags, trail, USE_MODE, TI_NONE);
      
      TYPEINFER(ast->child(2), defGamma, instEnv, impTypes, isVP, 
		currTcc, uflags, trail,  mode, TI_CONSTR);

      GCPtr<Type> idType = ident->symType;
      GCPtr<Type> rhsType = ast->child(1)->symType;
      GCPtr<TypeScheme> sigma = ident->scheme;
      sigma->tcc = currTcc;

      DEF_INF_DEBUG
	errStream << "At define " << ident->asString() << ":"
		  << " LHS = " << idType->asString()
		  << " RHS = " << rhsType->asString()
		  << std::endl;
      
      CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			    ast->child(1)->symType,
			    MBF(ast->child(0)->symType), uflags));
      
      DEF_INF_DEBUG
	errStream << "After Unification: " 
		  << ast->getID()->symType->asString()
		  << " LHS = " << idType->asString()
		  << " RHS = " << rhsType->asString()
		  << std::endl;            
      
      CHKERR(errFree, sigma->generalize(errStream, ast->loc, gamma,
					instEnv,  ast->child(1), NULL, 
					trail, gen_top));
      DEF_INF_DEBUG
	errStream << "After Generalization: " 
		  << ast->getID()->scheme->asString()
		  << std::endl << std::endl;
      
      gamma->mergeBindingsFrom(defGamma);
      
      if(declTS) 
	CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
				     declTS, ident->scheme, uflags, true));	
      
      ast->symType = ast->child(0)->symType;
      break;
    }
    
  case at_import:
  case at_provide:
    {
      GCPtr<Environment<TypeScheme> > tmpGamma = gamma->newScope();
      ast->envs.gamma = gamma;
      
      assert(ast->child(0)->envs.gamma);
      assert(ast->child(0)->envs.instEnv);
      
      useIFGamma(ast->child(0)->s, ast->child(0)->envs.gamma,
		 tmpGamma);
      useIFInsts(ast->child(0)->s, ast->child(0)->envs.instEnv, 
		 instEnv);
      
      gamma->mergeBindingsFrom(tmpGamma);
      break;
    }

  case at_from:
    {
      GCPtr<Environment<TypeScheme> > tmpGamma = gamma->newScope();
      ast->envs.gamma = gamma;

      GCPtr<AST> ifName = ast->child(0);
      
      assert(ifName->envs.gamma);
      assert(ifName->envs.instEnv);
      
      if(ast->children->size() == 1) {
	// This is an import-all form
	useIFGamma(std::string(), ifName->envs.gamma, tmpGamma);
	useIFInsts(std::string(), ifName->envs.instEnv, instEnv);
      }
      else {
	for (size_t c = 1; c < ast->children->size(); c++) {
	  GCPtr<AST> alias = ast->child(c);
	  GCPtr<AST> thisName = alias->child(0);
	  GCPtr<AST> thatName = alias->child(1);
	
	  GCPtr<TypeScheme> sigma = ifName->envs.gamma->getBinding(thatName->s);
	
	  if(!sigma) {
	    errStream << ast->loc << ": "
		      << " attempt to use " << thatName->s 
		      << ", which has an unknown, or buggy type"
		      << std::endl;
	    errFree = false;
	    break;
	  }
	  
	  tmpGamma->addBinding(thisName->s, sigma);
	  tmpGamma->setFlags(ast->child(0)->s, BF_PRIVATE);
	}
      }
      
      gamma->mergeBindingsFrom(tmpGamma);
      break;
    }

  case at_ifsel:
    {
      assert(false);
      break;
    }

  case at_declares:
    {
      ast->tagType = NULL;
      
      // match at_declare*
      for(size_t c = 0; c < ast->children->size(); c++) {
 	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_NONE);
	
	if(ast->child(c)->tagType) {
	  if(!ast->tagType) {
	    ast->tagType = ast->child(c)->tagType;
	    ast->field_bits = ast->child(c)->field_bits;
	  }
	  else {
	    errStream << ast->child(c)->loc << ": "
		      << "Only one tag-type declaration "
		      << "is allowed per definition"
		      << std::endl;	  
	  }
	}
      }
      ast->symType = new Type(ty_tvar);
      break;
    }

  case at_declare:
    {
      // match at_ident
      // The first identifier has special meaning, and must be 
      // dealt with by hand.
      GCPtr<AST> ident = ast->child(0);      
      GCPtr<AST> typ = ast->child(1);

      // match agt_type?
      if (ast->children->size() > 1) {
	TYPEINFER(typ, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_NONE);

      if(!typ->symType)
	typ->symType = new Type(ty_tvar);
      }

      // These names are never mangled.
      if(ident->s == "tag-type") {
	// compatible type
	
	GCPtr<Type> realType = ast->child(1)->symType->getType();
	GCPtr<Type> t = realType->getBareType();	
	if(t->kind == ty_mutable) {
	  errStream << ast->child(1)->loc << ": "
		    << "Tag type cannot be mutable"
		    << std::endl;
	  errFree = false;
	}
	else if(!t->isInteger()) {
	  errStream << ast->child(1)->loc << ": "
		    << "Tag type must be an integral type"
		    << std::endl;
	  errFree = false;
	  break;
	}
	else {
	  ast->tagType = ast->child(1)->symType;
	  ast->field_bits = ast->child(1)->field_bits;
	}	  
      }

      ast->symType = new Type(ty_tvar);
      break;
    }

  case at_tvlist:
    // match agt_tvar*
  case at_constructors:
    // match at_constructor+
  case at_constructor:

  case at_fields:
    // match at_field*
    break;

  case at_field: 
    {
      // match at_ident
      GCPtr<AST> fName = ast->child(0);
      fName->symType = new Type(ty_tvar);
      
      // match agt_type
      GCPtr<AST> fType = ast->child(1);
      TYPEINFER(fType, gamma, instEnv, impTypes, isVP, 
		tcc, uflags, trail,  USE_MODE, TI_TYP_EXP);
      
      ast->symType = fType->symType;
      ast->field_bits = fType->field_bits;
      ast->child(0)->field_bits = fType->field_bits;
      break;
    }

  case at_fill:
    {      
      // match agt_type 
      GCPtr<AST> fillType = ast->child(0);
      TYPEINFER(fillType, gamma, instEnv, impTypes, isVP, 
		tcc, uflags, trail,  USE_MODE, TI_TYP_EXP);     
      ast->field_bits = fillType->field_bits;
      break;
    }

  case at_reserved:
    {      
      // match agt_type 
      GCPtr<AST> fillType = ast->child(0);
      TYPEINFER(fillType, gamma, instEnv, impTypes, isVP, 
		tcc, uflags, trail,  USE_MODE, TI_TYP_EXP);     
      ast->field_bits = fillType->field_bits;

      GCPtr<AST> fillVal = ast->child(0);
      TYPEINFER(fillVal, gamma, instEnv, impTypes, isVP, 
		tcc, uflags, trail,  USE_MODE, TI_TYP_EXP);     
      
      uint64_t val = fillVal->litValue.i.as_uint64();
      uint64_t maxVal = (((uint64_t)1) << fillType->nBits()) - 1;
      
      if(val > maxVal) {
	errStream << ast->loc << ": "
		  << "Not enough bits to store the reserved value"
		  << std::endl;
	errFree = false;
      }

      break;
    }

  case at_bitfield:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP1);
      
      GCPtr<AST> len = ast->child(1);
      len->symType = new Type(ty_word);
      
      ast->symType = ast->child(0)->symType;
      ast->field_bits = len->litValue.i.as_uint32();

      if(!errFree)
	break;

      if(ast->field_bits > ast->symType->nBits()) {
	errStream << ast->loc << ": Invalid bitfield specification"
		  << "No. of bits requested = " << ast->field_bits
		  << ", Max available for type = "
		  << ast->symType->nBits()
		  << std::endl;
	errFree = false;
      }
#ifdef KEEP_BF
      ast->symType = new Type(ty_bitfield);
      ast->symType->components->append(new comp(ast->child(0)->symType));
      // match at_intLiteral
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP2);
      
      ast->child(1)->symType = ast->child(1)->symType->getTheType();

      // FIX TO WORD
      CHKERR(errFree, unifyPrim(errStream, trail, ast->child(1), 
				ast->child(1)->symType, "word", gamma)); 
      char lenStr[mpz_sizeinbase(ast->child(1)->litValue.i, 10)];
      mpz_get_str(lenStr, 10, ast->child(1)->litValue.i);
      ast->symType->Isize = strtoull(lenStr, 0, 10);
#endif
    
      break;
    }

    // The restriction that byref types can only appear on the
    // arguemnts of a function is enforced in the parser.
  case at_byrefType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP1);
    
      GCPtr<Type> t = ast->child(0)->getType();
    
      ast->symType = new Type(ty_byref);
      ast->symType->components->append(new comp(t));
    
      break;
    }

  case at_refType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP1);
    
      GCPtr<Type> t = ast->child(0)->getType();
    
      ast->symType = new Type(ty_ref);
      ast->symType->components->append(new comp(t));
    
      break;
    }

  case at_exceptionType:
    {
      ast->symType = new Type(ty_exn);
      break;
    }

  case at_dummyType:
    {
      ast->symType = new Type(ty_dummy);
      break;
    }

  case at_valType:
    {
      ast->symType = new Type(ty_tvar);
      
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP1);
      
      GCPtr<Type> t1 = ast->child(0)->symType->getType();
      GCPtr<Type> t = ast->child(0)->symType->getBareType();
      
      switch(t->kind) {
      case ty_tvar:
	{
	  GCPtr<Type> tvar = new Type(ty_tvar);
	  t->kind = ty_ref;	  
	  t->components->append(new comp(tvar));
	  ast->symType = tvar;
	  break;
	}

      case ty_ref:
	{
	  ast->symType = t->Base();
	  break;
	}

      case ty_vector:
	{
	  errStream << ast->loc << ": Cannot dereference a "
		    << "vector type. Obtained: "
		    << t->asString()
		    << std::endl;
	  errFree = false;
	  break;
	}

      case ty_structr:
      case ty_unionr:
	{
	  if (t->components->size() == 0) {
	    errStream << ast->loc << ": "
		      << "Target of (val 'a) must be a defined type "
		      << "(not just declared)." 
		    << "But obtained" << t1->asString() << std::endl;
	    errFree = false;
	    break;
	  }       	
	  else {
	    ast->symType = t->getDCopy();
	    ast->symType->kind = Type::getValKind(t->kind);
	  }
	  break;
	}
	
      case ty_uconv: 
      case ty_uconr:
      case ty_uvalv: 
      case ty_uvalr:
	{	
	  errStream << ast->loc << ": "
		    << "Target of a val should be a reference type." 
		    << " you cannot use a value constructor " 
		    << "(" << ast->child(0)->s << ") "
		    << "here, Use "
		    << "the union name."	  
		    << std::endl;
	  errFree = false;
	  break;
	}
	
      default:
	{
	  errStream << ast->loc << ": "
		    << "Target of a val should be a ref type." 
		    << "But obtained" << t1->asString() << std::endl;
	  errFree = false;
	  break;
	}
      }
      
      break;
    }    

  case at_fn:
    {
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP1);
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP1);
      
      ast->symType = new Type(ty_fn);
      GCPtr<Type> fnarg = ast->child(0)->symType->getType();
      ast->symType->components->append(new comp(fnarg));
      GCPtr<comp> nComp = new comp(ast->child(1)->getType());
      ast->symType->components->append(nComp);    
      break;
    }

  case at_fnargVec:
    {      
      GCPtr<Type> fnarg = new Type(ty_fnarg);
      for (size_t c = 0; c < ast->children->size(); c++) {
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail, mode, TI_COMP1);
	GCPtr<Type> argType = ast->child(c)->symType->getType();

	GCPtr<comp> nComp = new comp(argType);	
	if(argType->isByrefType()) {
	  nComp = new comp(argType->Base());
	  nComp->flags |= COMP_BYREF;
	}
	
	fnarg->components->append(nComp);
      }
      ast->symType = fnarg;
      break;
    }

  case at_primaryType:
    {
      ast->symType = new Type(Type::LookupKind(ast->s));
      break;
    }

  case at_arrayType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP1);
    
      GCPtr<Type> arrType = new Type(ty_array);
      ast->symType = arrType;
      arrType->components->append(new comp(ast->child(0)->symType));

      // match at_intLiteral
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP1);
 
      // FIX TO WORD
      CHKERR(errFree, unifyPrim(errStream, trail, ast->child(1)->loc, 
				ast->child(1)->symType, "word")); 

      arrType->arrlen->len = ast->child(1)->litValue.i.as_uint32();
      break;
    }

  case at_vectorType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP1);
    
      ast->symType = new Type(ty_vector);
      ast->symType->components->append(new comp(ast->child(0)->symType));

      break;
    }
     
  case at_mutableType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP1);
    
      GCPtr<Type> t = ast->child(0)->symType->getType();
      
      if(t->kind == ty_mutable) {
	//The Type is already mutable
	ast->symType = t;
      }
      if(t->isMaybe()) {
	assert(false);
      }
      else {
	ast->symType = new Type(ty_mutable);
	ast->symType->components->append(new comp(t));
      }
      break;
    }

  case at_typeapp:
    {
      // match agt_var 
      // match agt_tvar+
      ast->symType = new Type(ty_tvar);
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, 		
		(flags | TI_TYP_EXP | TI_TYP_APP));
    
      // Constructor cannot return a mutable type by default
      GCPtr<Type> t = ast->child(0)->getType(); 
      GCPtr<Type> realType = t;
      t = t->getBareType(); 
      
      if(t->kind != ty_structv && t->kind != ty_structr &&
	 t->kind != ty_unionv && t->kind != ty_unionr) {
	
	if(t->kind == ty_uconv || t->kind == ty_uconr || 
	   t->kind == ty_uvalv || t->kind == ty_uvalr) {
	  
	  errStream << ast->loc << ": "
		    << "cannot use a value constructor "
		    << "(" << ast->child(0)->s << ") "	    
		    << "here, Use the "
		    << "union name."
		    << std::endl;
	  errFree = false;
	  break;
	}

	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " cannot be resolved" 
		  << " to a structure or union type."
		  << " But obtained " 
		  << ast->child(0)->symType->asString()
		  << std::endl;
	
	errFree = false;
	break;
      }
      
      ast->symType = realType;
      
      GCPtr<Type> sut = t;
      
      if((ast->children->size()-1) != sut->typeArgs->size()) {
	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " - Type cannot be" 
		  << " partially/over instantiated" 
		  << " For type " << sut->asString()
		  << ", " << sut->typeArgs->size()
		  << " arguments are needed. But obtained "
		  << ast->children->size() -1
		  << std::endl;
	errFree = false;
      }
      else { 
	for(size_t i=0; i < sut->typeArgs->size(); i++) {
	  TYPEINFER(ast->child(i+1), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail,  USE_MODE, TI_COMP1);
	  
	  CHKERR(errFree, unify(errStream, trail, ast->child(i+1)->loc, 
				ast->child(i+1)->symType, 
				sut->TypeArg(i), uflags));
	}
      }
      
      break;
    }

  case at_qualType:
    {
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_CONSTR);
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP1);
      ast->symType = ast->child(1)->symType;
      break;
    }

  case at_constraints:
    {
      for(size_t c=0; c < ast->children->size(); c++)      
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_CONSTR);
      ast->symType = new Type(ty_tvar);
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
	  TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail, USE_MODE, TI_COMP2);
	}
	else {
	  // match agt_var
	  TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		    uflags, trail, REDEF_MODE, TI_COMP2);
	}      
      }
      else {
	// match agt_var
	TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);
      }
      
      // Type Qualifications ONLY in Binding Patterns
      // match agt_type?
      if (ast->children->size() > 1) {
	TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP1);
      
	if(ast->child(1)->symType->isByrefType()) {
	  CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
				ast->child(0)->symType, 
				ast->child(1)->getType()->Base(), 
				uflags));
	}
	else {
	  CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
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
      /*------------------------------------------------
              A |- e:t1  U(t = t1)
	  ______________________________
                A |- (e:t): t
	------------------------------------------------*/
      // match agt_eform
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
    
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP1);

      // tqExpr: U(t1 == t2)
      CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			    ast->child(0)->symType, 
			    ast->child(1)->symType,
			    uflags));

      ast->symType = ast->child(0)->symType;
      break;
    }
    
  case at_suspend:
    {
      // match agt_eform
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);

      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      ast->symType = ast->child(1)->symType;
      break;
    }

  case at_unit:
    {
      /*------------------------------------------------
	  ___________________________________________
              A |- unit: ()
	------------------------------------------------*/
      ast->symType = new Type(ty_unit);
      break;
    }

  case at_letGather:
    {
      /*------------------------------------------------
	         A |- e1:t1  ...  A |- en:tn 
	  ___________________________________________
              A |- (let-gather e1 ... en): (t1, ..., tn)
	------------------------------------------------*/

      ast->symType = new Type(ty_letGather);
      GCPtr<Type> gatherType = ast->symType->getBareType();

      for(size_t c=0; c < ast->children->size(); c++) {
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);
      
	gatherType->components->append(new comp(ast->child(c)->symType));
      }
      break;
    }

  case at_makevectorL:
    {
      /*------------------------------------------------
	         A |- en:tn  U(tn =  'w|word)
                 A |- el:tl  U(tl = 'f|('a|word -> 'b|'c))
                       tv = vector('d|'c)              
	  ___________________________________________
                A |- (make-vector en el): tv
	------------------------------------------------*/

      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      // FIX TO WORD
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			    ast->child(0)->symType, 
			    MBF(new Type(ty_word)),
			    uflags));

      // match agt_expr
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);

      // Build a type that I expect the second argument to be, and
      // unify with it.
      // Thee type of the function that builds the vector
      // is built as:
      // (fn ('a|word) 'b|'c)
      GCPtr<Type> wordType = MBF(new Type(ty_word));
      GCPtr<Type> arg = new Type(ty_fnarg, wordType);
      GCPtr<Type> ret = MBF(newTvar());
      GCPtr<Type> fnType = MBF(new Type(ty_fn, arg, ret));
      
      CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			    ast->child(1)->symType, fnType, uflags));
      
      ast->symType = new Type(ty_vector, MBF(ret));
      break;
    }

  case at_array:
  case at_vector:
    {
    /*------------------------------------------------
                A |- e1: t1 ... A |- en: tn
             U(t1 = 'a1|'b) ... U(tn = 'an|'b)
          _________________________________________
             A |- (array e1 ... en): array('a|'b, n)


                A |- e1: t1 ... A |- en: tn
             U(t1 = 'a1|'b) ... U(tn = 'an|'b)
          _________________________________________
             A |- (vector e1 ... en): vector('a|'b)
       ------------------------------------------------*/

      Kind k = (ast->astType == at_array) ? ty_array : ty_vector;
      GCPtr<Type> compType = MBF(newTvar());
      ast->symType = new Type(k, compType);
      ast->symType->arrlen->len = ast->children->size();
      
      // match agt_expr+
      for (size_t c = 0; c < ast->children->size(); c++) {
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	
	CHKERR(errFree, unify(errStream, trail, ast->child(c)->loc, 
			      ast->child(c)->symType,
			      MBF(compType), uflags));
      }
      
      break;
    }

  case at_array_length:
  case at_vector_length:
    {
    /*------------------------------------------------
             A |- e: t   U(t = 'a|array('b, ?len))
          _________________________________________
             A |- (array-length e): word


             A |- e: t   U(t = 'a|vector('b))
          _________________________________________
             A |- (vector-length e): word
       ------------------------------------------------*/
      Kind k = (ast->astType == at_array_length) ? ty_array : ty_vector;
      
      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> av = MBF(new Type(k, newTvar()));
      if(ast->astType == at_array_length)
	impTypes->append(new Pair<GCPtr<Type>, GCPtr<AST> >(av, ast->child(0)));

      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			    ast->child(0)->symType, av, uflags));
 
      // FIX TO WORD, not mutable
      ast->symType = new Type(ty_word);
      break;    
    }

  case at_array_nth:
  case at_vector_nth:
    {
    /*------------------------------------------------
             A |- e: t   U(t = 'a!array('b|'c, ?len))
             A |- en: tn  U(tn = 'd|word)
          _________________________________________
             A |- (array-nth e): 'b|'c


             A |- e: t   U(t = 'a|vector('b))
             A |- en: tn  U(tn = 'd|word)
          _________________________________________
             A |- (vector-nth e en): 'b
       ------------------------------------------------*/
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);

      GCPtr<Type> av=0;
      GCPtr<Type> cmp=0;
      if(ast->astType == at_array_nth) {
	cmp = MBF(newTvar());
	av = MBT(new Type(ty_array, cmp));
	impTypes->append(new Pair<GCPtr<Type>, GCPtr<AST> >(av, ast->child(0)));
      }
      else {
	cmp = newTvar();
	av = MBF(new Type(ty_vector, cmp));
      }
      
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			    ast->child(0)->symType, av, uflags));
      
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      // FIX TO WORD
      CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			    ast->child(1)->symType, 
			    MBF(new Type(ty_word)), 
			    uflags));

      ast->symType = cmp;
      break;
    }

  case at_begin:
    {
    /*------------------------------------------------
             A |- e1: t1 ... A |- en: tn
          _________________________________________
             A |- (begin e1 ... en): tn

       ------------------------------------------------*/
      // match agt_expr+
      for(size_t c = 0; c < ast->children->size(); c++)
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      ast->symType = ast->child(ast->children->size()-1)->symType;
      break;
    }

  case at_select:
    {
    /*------------------------------------------------
              A(r) = ['a1.. 'am] {... fld:t ... }  
        A |- e: tr     tr' = 'd!r('c1|'b1, ... 'c1|'bm)
               U(tr = tr')   tf = tr'.fld
          _________________________________________
                  A |- e.fld: tf
       ------------------------------------------------*/

      // match agt_expr 
      /* Selection is only permitted on 
	 - structures: for selecting field
	 - union values: determining tag (need to convert it to at_sel_ctr)
	 Note that selection for fqn-naming a union constructor
	 is already handled by the symbol resolver pass  */
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
            
      GCPtr<Type> t = ast->child(0)->symType->getType();
      GCPtr<Type> t1 = t->getBareType();

      if(t1->isUType()) {
	ast->astType = at_sel_ctr;
	ast->Flags2 &= ~AST_IS_LOCATION;	
	TYPEINFER(ast, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail, USE_MODE, TI_COMP2);
	break;
      }
      
      if(t1->kind != ty_structv && t1->kind != ty_structr) {
	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " cannot be resolved" 
		  << " to a structure, union, or exception type." 
		  << " but obtained " << t1->asString() 
		  << std::endl;
	errFree = false;
	break;
      }
      
      GCPtr<TypeScheme> stScheme;
      if(t1->defAst->symType->isULeg() ||
	 t1->defAst->symType->isException()) 
	stScheme = t1->defAst->stSigma;
      else
	stScheme = t1->defAst->scheme;
      
      GCPtr<Type> tr = stScheme->type_instance_copy();

      if(tr->isValType())
	for(size_t i=0; i < tr->typeArgs->size(); i++) {
	  GCPtr<Type> arg = tr->TypeArg(i)->getType();
	  if(tr->argCCOK(i))
	    trail->subst(arg, MBF(newTvar()));
	}
      
      GCPtr<Type> trt = MBT(tr);

      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			    t, trt, uflags));
      
      GCPtr<Type> fld;
      CHKERR(errFree, findComponent(errStream, tr, ast, fld));

      if(errFree)
	ast->symType = fld;
      else
	ast->symType = new Type(ty_tvar); 
      
      break;
    }

  case at_fqCtr:
    {
      /*------------------------------------------------
           A(v) = ['a1.. 'am] ... | Ctr{...} | ...
          _________________________________________
                  A |- v.Ctr:t
       ------------------------------------------------*/


      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> t1 = ast->child(0)->symType->getBareType();
      if(!t1->isUType()) {
	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " cannot be resolved" 
		  << " to a union, or exception type." 
		  << " but obtained " << t1->asString() 
		  << std::endl;
	errFree = false;
	break;
      }
      
      GCPtr<Type> fct;
      CHKERR(errFree, findComponent(errStream, t1, ast, fct));
      
      if(!errFree) {
	ast->symType = new Type(ty_tvar); 
	break;
      }
      
      ast->child(1)->symbolDef = fct->defAst;	  
      ast->child(1)->Flags |= fct->defAst->Flags;
      ast->child(1)->Flags2 |= fct->defAst->Flags2;
      ast->child(1)->symType = fct;
      ast->symType = ast->child(1)->symType;
      break;
    }
    
  case at_sel_ctr:
    {
      /*------------------------------------------------
           A(v) = ['a1.. 'am] ... | Ctr{...} | ...
  	  tv = v(...) or mutable(v(...)) or 'a!v(...)
                      A |- e:tv  
          _________________________________________
                  A |- e.Ctr:bool
       ------------------------------------------------*/

      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);

      GCPtr<Type> t1 = ast->child(0)->symType->getBareType();
      if(!t1->isUType()) {
	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " cannot be resolved" 
		  << " to a union, or exception type." 
		  << " but obtained " << t1->asString() 
		  << std::endl;
	errFree = false;
	break;
      }

      ast->symType = new Type(ty_bool);
      
      GCPtr<Type> fct;
      CHKERR(errFree, findComponent(errStream, t1, ast, fct));
      if(!errFree)
	break;
      
      ast->child(1)->symbolDef = fct->defAst;	  
      ast->child(1)->Flags |= fct->defAst->Flags;
      ast->child(1)->Flags2 |= fct->defAst->Flags2;
      ast->child(1)->symType = fct;
      break;
    }
    
  case at_lambda:
    {
      /*------------------------------------------------
              [U(t1 = 'b1|'a1)] ... [U(tn = 'bn|'an)]
             A, x1:'b1|'a1 ... xn:'bn|'an |- e: tr
           t1' = IF (t1 = byref(t1'') THEN t1 ELSE 'c1|'a1
	                      ...
           tn' = IF (tn = byref(tn'') THEN tn ELSE 'cn|'an
                           U(tr = 'd|'g)
       _____________________________________________________________
       A |- (lambda (x1[:t1] ... xn[:tn]) e): (t1' ... tn') -> 'h|'g
       ------------------------------------------------*/

      // match agt_bindingPattern
      // match agt_expr
      GCPtr<Environment<TypeScheme> > lamGamma = gamma->newScope();
      ast->envs.gamma = lamGamma;
      
      GCPtr<AST> argVec = ast->child(0);
      GCPtr<Type> fnarg = new Type(ty_fnarg);
      argVec->symType = fnarg;      
      
      for (size_t c = 0; c < argVec->children->size(); c++) {
	TYPEINFER(argVec->child(c), lamGamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail,  REDEF_MODE, TI_COMP2);

	GCPtr<Type> argType = argVec->child(c)->getType();
	GCPtr<comp> nComp = 0;
	if(argType->isByrefType()) {
	  nComp = new comp(argType->Base());
	  nComp->flags |= COMP_BYREF;
	}
	else {
	  nComp = new comp(MBF(argType));
	}
	
	fnarg->components->append(nComp);
      }

      TYPEINFER(ast->child(1), lamGamma, instEnv, impTypes, 
		isVP, tcc, uflags, trail,  USE_MODE, TI_COMP2);
      CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			    ast->child(1)->symType, 
			    MBF(newTvar()), uflags));
      
      GCPtr<Type> retType = MBF(ast->child(1)->getType());
      ast->symType = new Type(ty_fn, fnarg, retType);      
      break;
    }

  case at_argVec:
    {
      assert(false);
      break;
    }

  case at_allocREF:
    {
      /*------------------------------------------------
                    A |- e:t
          __________________________
              A |- (alloc-ref e): t
       ------------------------------------------------*/
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail, USE_MODE, TI_COMP1);
      ast->symType = ast->child(0)->symType;
      break;
    }
    
  case at_copyREF:
    {
       /*------------------------------------------------
            A |- e1:t1   A |- e2:t2    U(t1 = t2)
          ___________________________________________
              A |- (copy-ref e1 e2): ()
       ------------------------------------------------*/
      GCPtr<AST> lhs = ast->child(0);
      GCPtr<AST> rhs = ast->child(1);

      TYPEINFER(lhs, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail, USE_MODE, TI_COMP2);
      TYPEINFER(rhs, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail, USE_MODE, TI_COMP2);
      
      CHKERR(errFree, unify(errStream, trail, lhs->loc, 
			    lhs->symType, rhs->symType, uflags));
      
      ast->symType =  new Type(ty_unit);
      break;      
    }

  case at_mkClosure:
    {
       /*------------------------------------------------
             mkclosure: TODO
       ------------------------------------------------*/

      GCPtr<AST> clEnv = ast->child(0);
      // Type check the closure structure apply
      TYPEINFER(clEnv, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<AST> thisLambda = ast->child(1);
      // Type check the lambda forms
      TYPEINFER(thisLambda, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail, USE_MODE, TI_COMP2);
      
      GCPtr<Type> fullClFnType = thisLambda->symType->getType();
      GCPtr<Type> clFnType = fullClFnType->getBareType();
      assert(clFnType->isFnxn());
      GCPtr<Type> args = clFnType->Args()->getType();
      assert(args->components->size() >= 1);
      GCPtr<Type> clArg = args->CompType(0);
      // Make sure we have the right env on all the functions.
      CHKERR(errFree, unify(errStream, trail, clEnv->loc, 
			    clArg, clEnv->symType, uflags));
      
      // Build the closure Type.      
      GCPtr<Type> fullMkClType = fullClFnType->getDCopy();
      GCPtr<Type> mkClType= fullMkClType->getBareType();
      GCPtr<Type> mkClArg = mkClType->Args()->getType(); 
      assert(mkClArg->kind == ty_fnarg);
      
      // To Achieve: mkClArg->components->remove (0);
      GCPtr<CVector<GCPtr<comp> > > rcomp = new CVector<GCPtr<comp> >;
      for(size_t r=1; r < mkClArg->components->size(); r++)
	rcomp->append(mkClArg->Component(r));
      mkClArg->components = rcomp;
      /////////////////////////////////////

      ast->symType = fullMkClType;
      break;
    }

  case at_setClosure:
    {
       /*------------------------------------------------
             setclosure: TODO
       ------------------------------------------------*/
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, 
    		isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);
      
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, 
    		isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);
      
      for (size_t c = 2; c < ast->children->size(); c++) {
    	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
    		  uflags, trail, USE_MODE, TI_COMP2);
      }
    
      ast->symType = new Type(ty_unit);
      break;      
    }
    
  case at_apply:
    {
       /*------------------------------------------------
           A |- ef:tf      A |- e1:t1 ... A |- en: tn
	   U(tf = 'a|('c1|'b1 ... 'cn|'bn) -> 'cr|'br   
       IF byref(1) THEN U(t1 = 'c1|'b1) ELSE U(t1 = 'd1|'b1)
                            ... 
       IF byref(1) THEN U(t1 = 'cn|'bn) ELSE U(tn = 'dn|'bn)
	  ______________________________________________
              A |- (ef e1 ... en): 'e|'br 
       ------------------------------------------------*/
      // match agt_expr agt_expr
      //NOTE: One operation safe. (+)
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail, USE_MODE, TI_COMP2);
      GCPtr<Type> fType = ast->child(0)->getType();

      if(fType->isStruct()) {
	ast->astType = at_struct_apply;
	TYPEINFER(ast, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	break;
      }
      if(fType->isUType() || fType->isException()) {
	ast->astType = at_ucon_apply;
	TYPEINFER(ast, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	break;
      }

      GCPtr<Type> Fn = buildFnFromApp(ast, uflags);
      GCPtr<Type> expectFn = MBF(Fn);

      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			    fType, expectFn, uflags));
      
      if(!errFree) {
	ast->symType = newTvar(); 
	break;
      }
      
      GCPtr<Type> fnArgs = Fn->Args();
      for (size_t i = 0; i < ast->children->size()-1; i++) {
	GCPtr<AST> arg = ast->child(i+1);
	GCPtr<Type> fnArg = fnArgs->CompType(i);
	TYPEINFER(arg, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	
	// by-ref arguments need strict compatibality.
	// by-value arguments can have copy-compatibility.
	if(fnArgs->CompFlags(i) & COMP_BYREF)
	  CHKERR(errFree, unify(errStream, trail, ast->child(i+1)->loc, 
				fnArg, arg->symType, uflags));
	else
	  CHKERR(errFree, unify(errStream, trail, ast->child(i+1)->loc, 
				MBF(fnArg), arg->symType, uflags));
      }
      
      ast->symType = MBF(Fn->Ret());
      break;
    }
    
  case at_ucon_apply:
    {
       /*------------------------------------------------
        A(v) = ['a1.. 'am] ... | Ctr{f1:t1 ... fn:tn} | ...
       A |- ef:v(s1 ... sm)    A |- e1:t1' ... A |- en: tn'
                     
               U(t1  = 'c1|'b1) ... U(tn  = 'cn|'bn)
               U(t1' = 'd1|'b1) ... U(tn' = 'dn|'bn)
      ______________________________________________________
              A |- (Ctr e1 ... en): v(s1 ... sm)

      NOTE: s and t both denote standard types
       ------------------------------------------------*/

      ast->symType = newTvar();
      GCPtr<AST> ctr = ast->child(0);

      if((ctr->astType == at_ident) &&
	 (ctr->symbolDef->Flags & ID_IS_CTOR)) {
	// Constructor direct usage
      }
      else if ((ctr->astType == at_fqCtr) &&
	       (ctr->child(1)->symbolDef->Flags & ID_IS_CTOR)) {
	// Constructor qualified usage 
      }
      else {
	errStream << ctr->loc << ": "
		  << "union/exception"
		  << " constructor expected."
		  << std::endl;
	errFree = false;	    
	break;
      }
      
      if(!ctr->symType) {
	TYPEINFER(ctr, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      }
      
      // The constructor type cannot be a mutable or a maybe type
      GCPtr<Type> t = ctr->symType->getType();
      if(t->kind != ty_uconv && t->kind != ty_uconr && 
	 t->kind != ty_exn) {
	
	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " cannot be resolved" 
		  << " to a Union (or exception) Constructor."
		  << std::endl;
	errFree = false;
	break;
     }
    
      size_t cnt = nCtArgs(t);
      if(cnt != (ast->children->size() - 1)) {
	  errStream << ast->child(0)->loc << ": "
		    << "Constructor " << ast->child(0)->s << " needs "
		    << cnt << " arguments, but obtained"
		    << (ast->children->size() - 1)
		    << std::endl;
	  errFree = false;
	  break;
      }

      for(size_t i=0, j=1; i < t->components->size(); i++) {
	GCPtr<comp> ctrComp = t->components->elem(i);
	if(ctrComp->flags & COMP_UNIN_DISCM)
	  continue;
	
	GCPtr<AST> arg = ast->child(j);	
	
	TYPEINFER(arg, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail, USE_MODE, TI_COMP2);

	GCPtr<Type> tv = newTvar();
	CHKERR(errFree, unify(errStream, trail, arg->loc, 
			      t->CompType(i), MBF(tv), uflags)); 
	CHKERR(errFree, unify(errStream, trail, arg->loc, 
			      arg->symType, MBF(tv), uflags));
	j++;
      }
      
      if(!errFree)
	break;

      if(t->isUType()) {
	// A uval type was being returned here.
	ast->symType = obtainFullUnionType(t);
      }
      else {
	ast->symType = t;
      }
      
      break;
    }
    
  case at_struct_apply:
    {
       /*------------------------------------------------
           A(r) = ['a1.. 'am] {f1:t1 ... fn:tn}
      A |- ef:r(s1 ... sm)   A |- e1:t1' ... A |- en: tn'
      
               U(t1  = 'c1|'b1) ... U(tn  = 'cn|'bn)
               U(t1' = 'd1|'b1) ... U(tn' = 'dn|'bn)
      ______________________________________________________
              A |- (r e1 ... en): r(s1 ... sm)
       ------------------------------------------------*/
      // match at_ident
      
      ast->symType = newTvar();
      GCPtr<AST> ctr = ast->child(0);
      if(!ctr->symType)
	TYPEINFER(ctr, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
      
      // Structure constructor cannot be a mutable or maybe type.
      GCPtr<Type> t = ctr->symType->getType();
      if((ctr->astType != at_ident) ||
	 ((ctr->symbolDef->Flags & ID_IS_CTOR) == 0) ||
	 (!t->isStruct())) {
	errStream << ctr->loc
		  << ": Expected structure"
		  << " constructor taking at least one argument."
		  << std::endl;
	errFree = false;
	break;
      }
      if(t->components->size() == 0) {
	errStream << ast->child(0)->loc << ": "
		  << ast->child(0)->s << " cannot instantiate without "
		  << "definition in scope."
		  << std::endl;
	errFree = false;
	break;
      }
      if((ast->children->size()-1) != t->components->size()) {
	errStream << ast->child(0)->loc << ": "
		  << "Structure " << ast->child(0)->s << " cannot be" 
		  << " partially/over instantiated."	
		  << std::endl;
	
	errFree = false;
	break;
      }

      for(size_t i=0; i < t->components->size(); i++) {
	GCPtr<AST> arg = ast->child(i+1);
	TYPEINFER(arg, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	
	GCPtr<Type> tv = newTvar();
	CHKERR(errFree, unify(errStream, trail, arg->loc, 
			      t->CompType(i), MBF(tv), uflags));
	
	CHKERR(errFree, unify(errStream, trail, arg->loc, 
			      arg->symType, MBF(tv), uflags));
      }

      ast->symType = t;
      break;
    }
    
  case at_if:
    {
       /*------------------------------------------------
   	    A |- e0:t0   A |- e1:t1    A |- e2: t2
         U(t0 = 'a|bool)  U(t1 = 'b|'c)  U(t2 = 'd|'c)
      ______________________________________________________
                   A |- (if e0 e1 e2): 'e|'c
       ------------------------------------------------*/

      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP2);      
      
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			    ast->child(0)->symType,
			    MBF(new Type(ty_bool)), 
			    uflags));
      
      // match agt_expr
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP2);
      // match agt_expr
      TYPEINFER(ast->child(2), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP2);
      
      GCPtr<Type> tv = newTvar();
      CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			    ast->child(1)->symType, MBF(tv), uflags));
      CHKERR(errFree, unify(errStream, trail, ast->child(2)->loc, 
			    ast->child(2)->symType, MBF(tv), uflags));
      ast->symType = MBF(tv);
      break;
    }

  case at_when:
    {
      /* FIX: shap doesn't know the type rule here. Swaroop should
	 check this. */
       /*------------------------------------------------
   	    A |- e0:t0 
         U(t0 = 'a|bool)
	 ___________________________________________
              A |- unit: ()
       ------------------------------------------------*/

      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP2);      
      
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			    ast->child(0)->symType,
			    MBF(new Type(ty_bool)), 
			    uflags));
      
      // match agt_expr
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  mode, TI_COMP2);
      // FIX: Is a CHKERR call required here, given that there are no
      // constraints to enforce?
      

      ast->symType = new Type(ty_unit);
      break;
    }

  case at_and:
  case at_or:
  case at_not:
    {
       /*------------------------------------------------
     	         A |- e1:t1  ...  A |- en: tn
            U(t1 = 'a1|bool) ...  U(tn = 'an|bool)
       ________________________________________________
                   A |- (and e1 ... en): bool

     	         A |- e1:t1  ...  A |- en: tn
            U(t1 = 'a1|bool) ...  U(tn = 'an|bool)
       ________________________________________________
                   A |- (or e1 ... en): bool

       	       A |- e:t       U(t = 'a|bool)
       ________________________________________________
                   A |- (not e): bool
       ------------------------------------------------*/

      // match agt_expr+
      
      ast->symType = new Type(ty_bool);
      
      for(size_t c = 0; c < ast->children->size(); c++) {
	TYPEINFER(ast->child(c), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  mode, TI_COMP2);
	
	CHKERR(errFree, unify(errStream, trail, ast->child(c)->loc, 
			      ast->child(c)->symType,
			      MBF(ast->symType), uflags));
      }
      break;
    }

 case at_cond:
   {
       /*------------------------------------------------
  	    A |- c1: t1  ...  A |- cn: tn   A |- ow: tw
         U(t1 = 'a1|'b) ... U(tn = 'an|'b) U(tw = 'aw|'b)
       _____________________________________________________
                   A |- (cond c1 ... cn ow): 'c|'b
       ------------------------------------------------*/

     GCPtr<Type> tv = newTvar();
     // match at_cond_legs
     GCPtr<AST> conds = ast->child(0);
     for(size_t c = 0; c < conds->children->size(); c++) {
       GCPtr<AST> cond = conds->child(c);
       TYPEINFER(cond, gamma, instEnv, impTypes, isVP, tcc,
		 uflags, trail, USE_MODE, TI_COMP2);
       
       CHKERR(errFree, unify(errStream, trail, cond->loc, 
			     cond->symType,
			     MBF(tv), uflags));
     }
     conds->symType = MBF(tv);
     
     // match at_otherwise
     TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
	       uflags, trail,  USE_MODE, TI_COMP2);    
     
     CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			   ast->child(1)->symType,
			   MBF(tv), uflags));
     ast->symType = MBF(tv);
     break;
   }
   
 case at_cond_legs:
   {
     assert(false);
     break;
   }

 case at_cond_leg:
   {
     /*------------------------------------------------
         A |- cond: t1      A |- e: t     U(t1 = 'a|bool)
       __________________________________________________
                    A |- (cond e): t
       ------------------------------------------------*/
     GCPtr<Type> t = newTvar();
     CHKERR(errFree, unifyPrim(errStream, trail, ast->loc, t, "bool"));
      
     TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
	       uflags, trail,  USE_MODE, TI_COMP2);
     CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc, 
			   ast->child(0)->symType, 
			   MBF(new Type(ty_bool)), 
			   uflags));

     TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
	       uflags, trail,  USE_MODE, TI_COMP2);
     
     ast->symType = ast->child(1)->symType;
     break;
   }

  case at_setbang:
    {
     /*------------------------------------------------
       A |- e1: t1    A |- e2: t2    U(t1 = (mutable 'a))
          U(t1 = 'b|'c)    U(t2 = 'd|'c) |-lval e1
       __________________________________________________
                    A |- (set! e1 e2): ()

       NOTE: lval(e1) check enforced in the loc-chk pass
       ------------------------------------------------*/
      // match agt_expr
      ast->symType = new Type(ty_unit);
      
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> mTv = new Type(ty_mutable, newTvar());
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc,
			    ast->child(0)->symType, mTv, uflags));
      GCPtr<Type> tv = newTvar();
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc,
			    ast->child(0)->symType,
			    MBF(tv), uflags));

      CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc,
			    ast->child(1)->symType,
			    MBF(tv), uflags));
      break;
    }

  case at_dup:
    {
     /*------------------------------------------------
            A |- e: t     U(t = 'c|'b)
       _____________________________________
             A |- (dup t): ref('a|'b)
       ------------------------------------------------*/
      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> tv = newTvar();
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc,
			    ast->child(0)->symType,
			    MBF(tv), uflags));
      ast->symType = new Type(ty_ref, MBF(tv));
      break;      
    }

  case at_deref:
    {
     /*------------------------------------------------
            A |- e: t     U(t = 'b|ref('a))
       _____________________________________
             A |- (dup t): 'a
       ------------------------------------------------*/

      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);

      ast->symType = newTvar();
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc,
			    ast->child(0)->symType,
			    MBF(new Type(ty_ref, ast->symType)), uflags));
      break;
    }

  case at_inner_ref:
    {
     /*------------------------------------------------
        A |- e: 'a|ref(array t n)     U(en = 'a|word)
       _____________________________________________
             A |- (inner-ref e en): ref(t)


        A |- e: 'a|(vector t n)    U(en = 'a|word)
       ____________________________________________
             A |- (inner-ref e en): ref(t)


          A(r) = ['a1.. 'am] (unboxed) {... f:t ...} 
              A |- e: 'a|ref('b|r(t1...tm))
        _______________________________________________
             A |- (inner-ref e f): ref(t)


          A(r) = ['a1.. 'am] (boxed) {... f:t ...} 
                  A |- e: 'a|r(t1...tm)
        _______________________________________________
             A |- (inner-ref e f): ref(t)
       ------------------------------------------------*/

      
      ast->symType = newTvar();
      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> t = ast->child(0)->symType->getBareType();
      bool process_ndx = false;
      
      switch(t->kind) {
      case ty_ref:
	{		  
	  GCPtr<Type> drType = t->Base()->getBareType();
	  if(drType->kind == ty_array) {
	    ast->symType = new Type(ty_ref, drType->Base());
	    process_ndx = true;
	  }
	  else if (drType->kind == ty_structv) {
	    GCPtr<Type> fType=0;
	    CHKERR(errFree, findField(errStream, drType, 
				      ast->child(1), fType));
	    if(errFree)
	      ast->symType = new Type(ty_ref, fType);
	  }
	  
	  break;
	}
	
      case ty_structr:
	{
	  GCPtr<Type> fType=0;
	  CHKERR(errFree, findField(errStream, t, 
				     ast->child(1), fType));
	  if(errFree)
	    ast->symType = new Type(ty_ref, fType);
	  
	  break;
	}

      case ty_vector:
	{
	  process_ndx = true;
	  ast->symType = new Type(ty_ref, t->Base());
	  break;
	}

      default:
	{	
	  errStream << ast->loc << ": "
		    << "Invalid use of inner-ref."  << std::endl;
	  
	  errFree = false;
	  break;
	}
      }
      
      if(process_ndx) {
	ast->Flags2 |= INNER_REF_NDX;
	// match agt_expr
	TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	
	// FIX TO WORD
	CHKERR(errFree, unify(errStream, trail, ast->child(1)->loc, 
			      ast->child(1)->symType, 
			      MBF(new Type(ty_word)),
			      uflags));
      }
      break;
    }
    
  case at_switch:    
    {      
      /*------------------------------------------------
        A(v) = ['a1.. 'am] C11(r1) | ... | C1p(r1) |
                     ... | Cn1(rn) | ... | Cnq(rn) | ...

                   A|- e: t  U(t = v(s1 ... sm)
         A, x:r1 |- e1:t1 ... A, x:rn |- en:tn   A |- ew: tw
         U(t1 = 'a1|'b) ... U(tn = 'an|'b)   U(tw = 'aw|'b)
	_________________________________________________
           A |- (switch x e (C11 ... C1p x1 e1)
                                 ...
                            (Cn1 ... Cnq xn en)
                            (ow             ew)): 'c|'b
	------------------------------------------------*/

      // match at at_ident

      // match at agt_expr
      GCPtr<AST> topExpr = ast->child(1);
      TYPEINFER(topExpr, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      
      GCPtr<Type> tv = newTvar();
      // match at_case_legs
      GCPtr<AST> cases = ast->child(2);
      for(size_t c = 0; c < cases->children->size(); c++) {
	GCPtr<AST> thecase = cases->child(c);
	for(size_t j=2; j < thecase->children->size(); j++) {
	  GCPtr<AST> aCtr = thecase->child(j);
	  
	  TYPEINFER(aCtr, gamma, instEnv, impTypes, isVP, 
		    tcc, uflags, trail,  USE_MODE, TI_COMP2);      
	  
	  GCPtr<Type> aCtrType = aCtr->symType->getType();
	  CHKERR(errFree, unify(errStream, trail, aCtr->loc, 
				topExpr->symType, aCtrType, uflags));
	}
	
	TYPEINFER(thecase, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	
	if(!errFree)
	  continue;
	
	CHKERR(errFree, unify(errStream, trail, thecase->loc, 
			      thecase->symType, MBF(tv), uflags));
      }
      cases->symType = MBF(tv);

      // match at_otherwise
      GCPtr<AST> otherwise = ast->child(3);
      if(otherwise->astType != at_Null) {
	TYPEINFER(otherwise, gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	
	CHKERR(errFree, unify(errStream, trail, otherwise->loc, 
			      otherwise->symType, MBF(tv), uflags));
      }
      ast->symType = MBF(tv);
      
      /* Check consistency and coverage of the switch */
      if(!topExpr->symType->isUType()) {
	errStream << topExpr->loc << ": "
		  << "Only unions are permitted for switching"
		  << " but obtained "
		  << topExpr->symType->asString()
		  << std::endl;
	ast->symType = newTvar();
	errFree = false;
	break;
      } 

      GCPtr<Type> ut = topExpr->symType->getBareType();
      GCPtr<Type> uType = obtainFullUnionType(ut);
      
      for(size_t c = 0; c < cases->children->size(); c++) {
	GCPtr<AST> thecase = cases->child(c);
	for(size_t i=2; i < thecase->children->size(); i++) {
	  GCPtr<AST> ctr = thecase->child(i)->getCtr();	  
	  bool found=false;
	  
	  for(size_t j=0; j < uType->components->size(); j++) {
	    if(!uType->CompType(j))
	      continue;
	    
	    GCPtr<Type> cTyp = uType->CompType(j)->getType();
	    if(cTyp->defAst == ctr->symbolDef) {
	      found = true;
	      uType->CompType(j) = NULL;
	      break;
	    }
	  }
	  
	  if(!found) {
	    errStream << ctr->loc << ": "
		      << "Duplicate case label"
		      << ctr->asString()
		      << "." << endl;
	    errFree = false;
	  }
	}
      }


      bool moreCases = false;
      for(size_t j=0; j < uType->components->size(); j++) 
	if(uType->CompType(j)) {
	  moreCases = true;
	  break;
	}
      
      if(moreCases) {
	if(otherwise->astType == at_Null) {
	  errStream << ast->loc << ": The following cases"
		    << "are not covered: ";
	  for(size_t j=0; j < uType->components->size(); j++) {
	    GCPtr<Type> cTyp = uType->CompType(j)->getType();
	    if(j > 0)
	      errStream << ", ";
	    errStream << cTyp->defAst->s;
	  }
	  errStream << std::endl;
	  errFree = false;
	}
      }
      else {
	if(otherwise->astType != at_Null) {
	  errStream << otherwise->loc << ": "
		    << "Otherwise is present even after all cases"
		    << "are covered."
		    << std::endl;
	  errFree = false;
	}
      }
      break;
    }

  case at_sw_legs:
    /* FIX: I forgot if I really mean break here or assert(false); */
    break;

  case at_otherwise:
    {
      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
    
      ast->symType = ast->child(0)->symType;
      break;
    }

  case at_sw_leg:
    {
      // This is used only in the case of a union leg match.
      // match agt_valuePattern
      GCPtr<Environment<TypeScheme> > legGamma = gamma->newScope();
      ast->envs.gamma = legGamma;

      /* Deal with the constructors first */
      GCPtr<AST> aCtr = ast->child(2)->getCtr();
      GCPtr<Type> aCtrType = ast->child(2)->symType->getBareType();

      /* Deal with the component structure identifier */
      GCPtr<TypeScheme> stSigma = aCtr->symbolDef->stSigma;
      GCPtr<Type> stType = stSigma->type_instance_copy();
      /* If we decide to alow the unification of the structure
	 type with the union type, this must be done there */
      assert(stType->typeArgs->size() == aCtrType->typeArgs->size());      
      for(size_t m=0; m < stType->typeArgs->size(); m++) {
	CHKERR(errFree, unify(errStream, trail, ast->loc, 
			      stType->TypeArg(m), 
			      aCtrType->TypeArg(m), uflags));	
      }
      
      /* Checking compatibility of different constructors used in the
	 same leg of as switch is done in differently for unions
	 and reprs. 
	 ** In the case of unions, all union legs must have
	 the same field structure. 
	 **In  the case of reprs, we have more
	 constraints on field layour -- identically names fields must
	 be at the same bit-offset and be of the same type. Therefore,
	 we can allow different constructors such that only common
	 fields are efective within the switch-leg.

	 Therefore, in the case of unions, we check for stSigma
	 compatibility, but in the case of reprs, we just give the
	 switched-leg identifier the type obtained from the stSigma of
	 the first constructor, but mark fields that are not in the
	 intersection of all constructors as invalid. This flag (on
	 the component) is ONLY checked in at_select.  */
      GCPtr<AST> uninID = aCtr->symType->myContainer;
      bool isRepr = (uninID->Flags2 & UNION_IS_REPR);


      for(size_t c=2; c < ast->children->size(); c++) {
 	GCPtr<AST> ctr = ast->child(c)->getCtr()->symbolDef;
	if(!ctr->stSigma) {
	  errStream << ctr->loc << ": Use of constructor "
		    << ctr->s << " whose definition had an error"
		    << std::endl;
	  errFree = false;
	  break;
	}

	if(!isRepr) {
	  if(ctr->stSigma != stSigma) {
	    errStream << ctr->loc << ": Use of constructor " 
		      << ctr->s << " whose components are "
		      << "incompatible with other constructors used "
		      << "in this case"
		      << std::endl;
	    errFree = false;
	    break;
	  }
	}
	else {
	  if(aCtr == ctr)
	    continue;
	  
	  const GCPtr<const Type> ctType = ctr->stSigma->tau;
	  for(size_t ac=0; ac < stType->components->size(); ac++) {
	    GCPtr<comp> stComp = stType->Component(ac);
	    bool found=false;	    
	    
	    for(size_t tc=0; tc < ctType->components->size(); tc++) {
	      const GCPtr<const comp> ctComp = ctType->Component(tc);
	      
	      if(ctComp->name == stComp->name) {
		found = true;
		break;
	      }
	    }
	    
	    if(!found) 
	      stComp->flags |= COMP_INVALID;	    
	  }
	}
      }
      
      if(!errFree) {
	ast->symType = newTvar();
	break;
      }
      
      GCPtr<AST> stIdent = ast->child(0); 
      TYPEINFER(stIdent, legGamma, instEnv, impTypes, isVP, 
		tcc, uflags, trail,  REDEF_MODE, TI_COMP2);
      stIdent->symType = stType;
      stIdent->scheme->tau = stType;
      assert(stIdent->scheme->ftvs->size() == 0);

      // match agt_expr
      TYPEINFER(ast->child(1), legGamma, instEnv, impTypes, isVP, 
		tcc, uflags, trail,  USE_MODE, TI_COMP2);

       ast->symType = ast->child(1)->symType;
      break;
    }
    
  case at_try:
    {
      /*------------------------------------------------
           A(exn) =  E11(r11) | ... | E1p(r1p) | ... |
                     Em(rm)   | ... 

  	           A|- e: t     U(t = 'a|'b)
      A |- e1:t1 ...  A, x:rm |- em:tm ...   [A |- ew: tw]
      U(t1 = 'a1|'b) ... U(tm = 'am|'b) ... [U(tw = 'aw|'b)]
	_________________________________________________
         A |- (try e x
                   (catch (E11 ... E1p    e1)
		               ...
                          (Em          xm em)
		               ...
                         [(ow             ew)]): 'c|'b
	------------------------------------------------*/

      // match agt_expr
      GCPtr<Type> tv = newTvar();
      GCPtr<AST> expr = ast->child(0);     
      TYPEINFER(expr, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      CHKERR(errFree, unify(errStream, trail, expr->loc, 
			    expr->symType, MBF(tv), uflags));
      
      ast->symType = MBF(tv);
      
      if(!errFree)
	break;

      // match at_ident: ignore

      // match at_sw_legs
      GCPtr<AST> cases = ast->child(2);     
      cases->symType = MBF(tv);
      for (size_t c = 0; c < cases->children->size(); c++) {
	GCPtr<AST> theCase = cases->child(c);
	
	for(size_t j=2; j < theCase->children->size(); j++) {
	  GCPtr<AST> aCtr = theCase->child(j);
	  
	  TYPEINFER(aCtr, gamma, instEnv, impTypes, isVP, 
		    tcc, uflags, trail, USE_MODE, TI_COMP2);      
	  
	  if(aCtr->symType->getType()->kind != ty_exn) {
	    errStream << aCtr->loc << ": "
		      << " Only Exceptions can be caught"
		      << " Obtained type " 
		      << aCtr->symType->asString()
		      << std::endl;
	    errFree = false;
	  }
	}	
	
	GCPtr<Environment<TypeScheme> > legGamma = gamma;
	if(theCase->children->size() == 3) {
	  legGamma = gamma->newScope();
	  theCase->envs.gamma = legGamma;

	  GCPtr<AST> stIdent = theCase->child(0);
	  // Add sIdent to the legGamma environment.
	  TYPEINFER(stIdent, legGamma, instEnv, impTypes, isVP, 
		    tcc, uflags, trail,  REDEF_MODE, TI_COMP2);

	  // Make sIdent's type the correct type.
	  GCPtr<AST> onlyCtr = theCase->child(2)->getCtr();
	  assert(onlyCtr->symbolDef->stSigma);
	  GCPtr<Type> stType = onlyCtr->symbolDef->stSigma->type_instance_copy();
	  stIdent->symType = stType;
	  stIdent->scheme->tau = stType;
	  assert(stIdent->scheme->ftvs->size() == 0);
	}
	
	GCPtr<AST> expr = theCase->child(1);
	TYPEINFER(expr, legGamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail, USE_MODE, TI_COMP2);

	CHKERR(errFree, unify(errStream, trail, expr->loc, 
			      expr->symType, MBF(tv), uflags));
	
	theCase->symType = expr->symType;
      }
      
      // match agt_ow
      GCPtr<AST> ow = ast->child(3);
      if(ow->astType != at_Null) {
	TYPEINFER(ow->child(0), gamma, instEnv, impTypes, isVP, tcc,
		  uflags, trail,  USE_MODE, TI_COMP2);
	CHKERR(errFree, unify(errStream, trail, ow->child(0)->loc,
			      ow->child(0)->symType, 
			      MBF(tv), uflags));  
	ow->symType = ow->child(0)->symType;
      }
      
      break;
    }

  case at_throw:
    {
      /*------------------------------------------------
                A |- e: t   U(t = 'b|exn)
	_________________________________________________
                   A |- (throw e): 'a
	------------------------------------------------*/
      // match agt_var
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      CHKERR(errFree, unify(errStream, trail, ast->child(0)->loc,
			    ast->child(0)->symType, 
			    MBF(new Type(ty_exn)), 
			    uflags));  
      
      ast->symType = newTvar();    
      break;
    }

  case at_container:
    {
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);
      ast->symType = ast->child(1)->symType;
      break;
    }    

  case at_do:
    {      
      /*------------------------------------------------
               A |- ei1: t1 ... A |- ein: tn
             U(t1 = 'a1|'b1) ... U(tn = 'an|'bn)
	       
          A, x1:'c1|'b1, ... xn:'cn|'bn |- es1: ts1 ...
          A, x1:'c1|'b1, ... xn:'cn|'bn |- esn: tsn
             U(ts1 = 'd1|'b1) ... U(tsn = 'dn|'bn)

           A, x1:'a1|'b1, ... xn:'an|'bn |- (et er): tr
           A, x1:'a1|'b1, ... xn:'an|'bn |- eb: t
	_________________________________________________
                   A |- (do ((x1 ei1 es1)
                                 ...
                             (xn ein esn))
                         (et  er)  eb): tr
	------------------------------------------------*/
      // match at_letbindings
      GCPtr<Environment<TypeScheme> > doGamma = gamma->newScope();
      ast->envs.gamma = doGamma;

      GCPtr<TCConstraints> doTcc = new TCConstraints;
      
      GCPtr<AST> dbs = ast->child(0);
      dbs->symType = new Type(ty_tvar);

      // Initializers
      for (size_t c = 0; c < dbs->children->size(); c++) {
	GCPtr<AST> db = dbs->child(c);
	GCPtr<AST> init = db->child(1);
	GCPtr<Type> tv = newTvar();
	TYPEINFER(init, doGamma, instEnv, impTypes, isVP, doTcc,
		  uflags, trail, USE_MODE, TI_COMP2);
	CHKERR(errFree, unify(errStream, trail, init->loc, 
			      init->symType, MBF(tv), 
			      uflags));
      }
      
      // Definitions
      for (size_t c = 0; c < dbs->children->size(); c++) {
	GCPtr<AST> db = dbs->child(c);
	GCPtr<AST> localDefPat = db->child(0);
	GCPtr<AST> localDef = localDefPat->child(0);
	GCPtr<AST> init = db->child(1);
	
	localDef->symType = MBF(init->symType);
	TYPEINFER(localDefPat, doGamma, instEnv, impTypes, isVP, doTcc,
		  uflags, trail, REDEF_MODE, TI_COMP2);
      }

      // Next step initializers
      for (size_t c = 0; c < dbs->children->size(); c++) {
	GCPtr<AST> db = dbs->child(c);
	GCPtr<AST> localDef = db->getID();
	GCPtr<AST> step = db->child(2);
	
	TYPEINFER(step, doGamma, instEnv, impTypes, isVP, doTcc,
		  uflags, trail, USE_MODE, TI_COMP2);
	
	CHKERR(errFree, unify(errStream, trail, step->loc, step->symType, 
			      MBF(localDef->symType), uflags));
      }

      // Finally evaluate the test and the final expression           
      TYPEINFER(ast->child(1), doGamma, instEnv, impTypes, 
		isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);
      TYPEINFER(ast->child(2), doGamma, instEnv, impTypes, 
		isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);

      ast->symType = ast->child(1)->symType;      
      break;
    }

  case at_dotest:
    {
      /*------------------------------------------------
               A |- et: tb ... A |- er: tr
            U(tb = 'a|bool)   U(tr = 'a|'b)
	_________________________________________________
                   A |- ((et  er)): 'c|'b
	------------------------------------------------*/
      GCPtr<AST> test = ast->child(0);
      GCPtr<AST> result = ast->child(1);
      TYPEINFER(test, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);

      CHKERR(errFree, unify(errStream, trail, test->loc, test->symType, 
			    MBF(new Type(ty_bool)), uflags));
      
      GCPtr<Type> tv = newTvar();
      TYPEINFER(result, gamma, instEnv, impTypes, isVP, tcc,
		uflags, trail,  USE_MODE, TI_COMP2);

      CHKERR(errFree, unify(errStream, trail, result->loc, 
			    result->symType, MBF(tv), uflags));
      
      ast->symType = MBF(tv);
      break;
    }

  case at_letrec:
  case at_let:
    {
      /*------------------------------------------------
                 A |- e1: t1'  ...  A |- en: tn'
             U(t1' = 'c1|'b1)  ...  U(tn' = 'cn|'bn)
            [U(t1  = 'a1|'b1)] ... [U(tn  = 'an|'bn)]
	       
      (S1, ..., Sn) = Generalize(A, ('a1|'b1, ..., 'an|'bn),
                                    (e1, ... en))
               
               A, x1:S1, ... xn:Sn |- e: t
	_________________________________________________
         A |- (let ((x1[:t1] e1) ... (xn[:tn] en)) e): t


	 
   	     [U(t1 = 'a1|'b1)] ... [U(tn = 'an|'bn)]
           A, x1:'a1|'b1, ..., xn:'a1:'bn |- e1: t1' ... 
  	   A, x1:'a1|'b1, ..., xn:'a1:'bn |- en: tn'
             U(t1' = 'c1|'b1) ... U(tn' = 'cn|'bn)
	       
      (S1, ..., Sn) = Generalize(A, ('a1|'b1, ..., 'an|'bn),
                                    (e1, ... en))
               
               A, x1:S1, ... xn:Sn |- e: t
     __________________________________________________________
         A |- (letrec ((x1[:t1] e1) ... (xn[:tn] en)) e): t
	------------------------------------------------*/
      // match at_letbindings
      GCPtr<Environment<TypeScheme> > letGamma = gamma->newScope();
      GCPtr<TCConstraints> letTcc = new TCConstraints;

      GCPtr<AST> lbs = ast->child(0);
      lbs->symType = new Type(ty_tvar);
      
      ast->envs.gamma = letGamma;
      ast->envs.instEnv = instEnv;
      lbs->envs.gamma = letGamma;
      lbs->envs.instEnv = instEnv;

         
      if(ast->astType == at_let) {
	CHKERR(errFree, 
	       ProcessLetExprs(errStream, lbs, letGamma, instEnv,
			       impTypes, isVP, letTcc, uflags, trail,
			       USE_MODE, TI_COMP2)); 
	CHKERR(errFree, 
	       ProcessLetBinds(errStream, lbs, letGamma, instEnv,
			       impTypes, isVP, letTcc, uflags, trail,
			       REDEF_MODE, TI_COMP2)); 
      }
      else {
	CHKERR(errFree, 
	       ProcessLetBinds(errStream, lbs, letGamma, instEnv,
			       impTypes, isVP, letTcc, uflags, trail,
			       REDEF_MODE, TI_COMP2)); 
	CHKERR(errFree, 
	       ProcessLetExprs(errStream, lbs, letGamma, instEnv,
			       impTypes, isVP, letTcc, uflags, trail, 
			       USE_MODE, TI_COMP2)); 
      }
      CHKERR(errFree, UnifyLetBinds(errStream, lbs, uflags, trail));

      if(!errFree) {
	ast->symType = newTvar();
	break;
      }
      
      // Consider all constraints
      TYPEINFER(ast->child(2), letGamma, instEnv, impTypes, isVP, 
		letTcc, uflags, trail,  mode, TI_CONSTR);
   
      GCPtr<AST> bAst, vAst;
      makeLetGather(lbs, bAst, vAst);
      
      CHKERR(errFree, generalizePat(errStream, ast->loc, 
				    gamma, instEnv, bAst, vAst, 
				    letTcc, tcc, trail));
      
      lbs->symType = bAst->symType;
      lbs->scheme = bAst->scheme;
      
      // Finally evaluate the final expression
      TYPEINFER(ast->child(1), letGamma, instEnv, impTypes, 
		isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);
      
      //if((ast->astType == at_letrec) && ((uflags & POST_REFIZE) == 0))
      //CHKERR(errFree, CheckLetrecFnxnRestriction(errStream, bAst));
      
      ast->symType = ast->child(1)->symType;
      break;
    }
    
    // letStar is not a letStar. It is an internal representation.
    // types are NOT generalized
  case at_letStar:
    {
      /*------------------------------------------------
                         A |- e1: t1'    
              U(t1' = 'c1|'b1)     [U(t1  = 'a1|'b1)]
                A, x1:'a1|'b1 |- e2: t2'     
     	     U(t2' = 'c2|'b2)      [U(t2  = 'a2|'b2)]
                             ...
	A, x1:'a1|'b1, ... xn-1:'an-1|'n-1 |- en: tn' ...
     	     U(tn' = 'cn|'bn)      [U(tn  = 'an|'bn)]

             A, x1:'a1|'b1, ... xn:'an|'bn |- e: t
	_________________________________________________
	 A |- (let* ((x1[:t1] e1) ... (xn[:tn] en)) e): t
	------------------------------------------------*/

      // match at_letbindings
      GCPtr<Environment<TypeScheme> > letGamma = gamma->newScope();
      
      GCPtr<AST> lbs = ast->child(0);
      lbs->symType = new Type(ty_tvar);
      
      ast->envs.gamma = letGamma;
      ast->envs.instEnv = instEnv;
      lbs->envs.gamma = letGamma;
      lbs->envs.instEnv = instEnv;
      
      for (size_t c = 0; c < lbs->children->size(); c++) {
	GCPtr<AST> lb = lbs->child(c);
	GCPtr<AST> id = lb->getID();
	GCPtr<AST> ip = lb->child(0);
	GCPtr<AST> expr = lb->child(1);
	
	TYPEINFER(expr, letGamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);
	
	TYPEINFER(ip, letGamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail, REDEF_MODE, TI_COMP2);
	
	CHKERR(errFree, unify(errStream, trail, lb->getID()->loc,
			      expr->symType, 
			      MBF(id->symType), uflags));
      }
      
      TYPEINFER(ast->child(1), letGamma, instEnv, impTypes, 
		isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);
      
      ast->symType = ast->child(1)->symType;
      break;
    }

    // CAREFUL: CAREFUL:
    // This is *NOT* dead code, though, it appears to be so, from the
    // way the above let-cases are written. 
    // this case is used by the (new) polyinstantiator to R&T
    // let-binding instantiations. It is OK to use it ther because we
    // don't have any more polymorphism at that stage.
    // THIS CASE MUST NOT BE USED BY OTHER LET FORMS
  case at_letbinding:
    {
      GCPtr<AST> id = ast->getID();
      GCPtr<AST> ip = ast->child(0);
      GCPtr<AST> expr = ast->child(1);
      if(ast->Flags & LB_REC_BIND) {
	TYPEINFER(ip, gamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail, REDEF_MODE, TI_COMP2);

	TYPEINFER(expr, gamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);	
      }	
      else {
	TYPEINFER(expr, gamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail, USE_MODE, TI_COMP2);
	
	TYPEINFER(ip, gamma, instEnv, impTypes, 
		  isVP, tcc, uflags, trail, REDEF_MODE, TI_COMP2);
      }
      
      CHKERR(errFree, unify(errStream, trail, ast->getID()->loc,
			    expr->symType, MBF(id->symType), uflags));
      break;
    }
    
  } /* switch */
  
//   if(ast->symType)
//     errStream << ast->loc << " [" << ast->atKwd() << "] " 
// 	      << ast->asString() << ": "
// 	      << ast->symType->asString() 
// 	      << endl << endl; 
  
  return errFree;
}

/**************************************************************/
/*              Interface to the outside world                */
/**************************************************************/

bool
UocInfo::fe_typeCheck(std::ostream& errStream,
		      bool init, unsigned long flags)
{
  // Careful: the input flags are interface flags `uflags,'
  // and not the internal flags `flags.' 
  
  TI_TOP_DEBUG
    errStream << "Now Processing " << uocName
	      << " ast = " << uocAst->astTypeName()
	      << std::endl;
  
  GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes = 
    new CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > >;
  GCPtr<Trail> trail = new Trail;
  bool errFree = true;

  if(Options::noPrelude)
    flags |= TYP_NO_PRELUDE;
  
  if(init) {
    
    if(flags & INF_REINIT) {
      assert(gamma);      
      assert(gamma->parent);
      gamma = gamma->parent->newDefScope();

      assert(instEnv);      
      assert(instEnv->parent);
      instEnv = instEnv->parent->newDefScope();      
    }
    else {
      gamma = new Environment<TypeScheme>(this->uocName);
      instEnv = new Environment< CVector<GCPtr<Instance> > >(this->uocName);
    }
    if((flags & TYP_NO_PRELUDE) == 0)
      CHKERR(errFree, initGamma(std::cerr, gamma, instEnv, uocAst, flags));
    
    if(!errFree)
      return false;
  }

  CHKERR(errFree, typeInfer(errStream, uocAst, gamma, instEnv, 
			    impTypes, false, 
			    new TCConstraints, flags, trail, 
			    USE_MODE, TI_NONE));
  CHKERR(errFree, checkImpreciseTypes(errStream, gamma, impTypes));

  TI_TOP_DEBUG {
    errStream << "- - - - - - - - - - - - - - - - - - - - - - - " 
	      << endl;
    
    GCPtr<AST> mod = uocAst;
    for(size_t i=0; i < mod->children->size(); i++) {
      GCPtr<AST> ast = mod->child(i);
      errStream << ast->atKwd() << std::endl;
      if(ast->astType == at_define) {
	GCPtr<AST> id = ast->child(0)->child(0);
	errStream << id->asString() << ": "	
		  << id->scheme->asString(Options::debugTvP, true)
		  << std::endl;
      }
    }
    
    errStream << "________________________________________" 
	      << std::endl;
  }

  
  return errFree;
}
