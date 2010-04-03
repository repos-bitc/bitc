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
#include <libsherpa/BigNum.hxx>
#include "TypeInferUtil.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;

typedef map<shared_ptr<Type>, shared_ptr<AST> > TypeAstMap;

/**************************************************************/
/*                     Some Declarations                      */
/**************************************************************/

// Some compositions of type inference flags, as a shorthand.
// A Type Expression, but not a type application.
#define TI_NON_APP_TYPE    ((ti_flags | TI_TYP_EXP) & (~TI_TYP_APP))
// A value expression
#define TI_EXPRESSION      (ti_flags & (~(TI_TYP_EXP | TI_TYP_APP)))
// A constraint
#define TI_CONSTRAINT      (TI_NON_APP_TYPE)

//WARNING: **REQUIRES** errFree and errStream
#define TYPEINFER(ast, gamma, instEnv, impTypes, rewrite, tcc,        \
                  trail, mode, flags)                                \
  do {                                                                \
    CHKERR((errFree),                                                \
           (typeInfer(errStream, (ast), (gamma), (instEnv),        \
                      (impTypes), (rewrite), (tcc),                \
                      (trail), (mode), (flags))));                \
  }while (0)

//WARNING: **REQUIRES** errFree and errStream
#define UNIFY(trail, errLoc, type1, type2)                        \
  do {                                                                \
  CHKERR(errFree, unify(errStream, trail, errLoc,                \
                        type1, type2, UFLG_NO_FLAGS));                \
  }while (0)


static bool
typeInfer(std::ostream& errStream, shared_ptr<AST> ast, 
          shared_ptr<TSEnvironment > gamma,
          shared_ptr<InstEnvironment > instEnv,
          TypeAstMap& impTypes,
          bool &rewrite, 
          shared_ptr<TCConstraints> tcc,
          shared_ptr<Trail> trail,
          ResolutionMode mode,
          TI_Flags ti_flags);

bool isExpansive(std::ostream& errStream, 
                 shared_ptr<const TSEnvironment > gamma,
                 shared_ptr<AST> ast);

bool isExpansive(std::ostream& errStream, 
                 shared_ptr<const TSEnvironment > gamma,
                 shared_ptr<Type> typ);

bool
generalizePat(std::ostream& errStream,
              const sherpa::LexLoc &errLoc,
              shared_ptr<TSEnvironment > gamma,
              shared_ptr<const InstEnvironment > instEnv,
              shared_ptr<AST> bp, shared_ptr<AST> expr,
              shared_ptr<TCConstraints> tcc,
              shared_ptr<TCConstraints> parentTCC,
              shared_ptr<Trail> trail);


/**************************************************************/
/*                     Some Helper Functions                  */
/**************************************************************/

/* Some of the following fure repeated (and marked static) in both 
   inference routines due to the use/non-use of maybe types */

static shared_ptr<Type>
buildFnFromApp(shared_ptr<AST> ast)
{
  assert(ast->astType == at_apply);
  shared_ptr<Type> targ = Type::make(ty_fnarg);
  for (size_t i = 1; i < ast->children.size(); i++) {
    shared_ptr<Type> argi = MBF(newTvar());
    shared_ptr<comp> ncomp = comp::make(argi);
    ncomp->flags |= COMP_MAYBE_BYREF;
    targ->components.push_back(ncomp);
  }
  
  shared_ptr<Type> ret = MBF(newTvar());
  shared_ptr<Type> fn = Type::make(ty_fn, targ, ret);
  return fn;
}

static shared_ptr<TypeScheme> 
bindIdentDef(shared_ptr<AST> ast, 
             shared_ptr<TSEnvironment > gamma,
             unsigned long bindFlags,
             TI_Flags ti_flags)
{
  if (!ast->symType) {
    if (ast->isIdentType(id_tvar))
      ast->symType = newTvar();
    else
      ast->symType = MBF(newTvar()); 
  }

  shared_ptr<TypeScheme> sigma = TypeScheme::make(ast->symType, ast);
  ast->scheme = sigma;
  
  if (ast->isIdentType(id_tvar)) {
    assert(ti_flags & TI_TYP_EXP);
    bindFlags |= BF_NO_MERGE;
    ast->tvarLB->envs.gamma->addBinding(ast->s, sigma);
  }
  else {
    gamma->addBinding(ast->s, sigma);      
  }
  
  gamma->setFlags(ast->s, bindFlags);    
  return sigma;
}

static shared_ptr<TypeScheme> 
Instantiate(shared_ptr<AST> ast, shared_ptr<TypeScheme> sigma,
            shared_ptr<Trail> trail)
{              
  if (ast->symbolDef)
    ast = ast->symbolDef;
  
  shared_ptr<TypeScheme> ins = GC_NULL;
  
  // Are we instantiating a structure/union definition?
  bool suInst = (ast->isIdentType(idc_ctor) ||
                 ast->isIdentType(id_union)); 
  
  ins = sigma->ts_instance();
  ins->tau->fixupFnTypes();
  
  if(suInst && !ins->tau->isException())
    ins->tau->fixupConstArguments(trail);
  
  return ins;
}

static bool
findField(std::ostream& errStream, 
          shared_ptr<Type> t, shared_ptr<AST> fld, shared_ptr<Type> &fType)
{
  t = t->getBareType();
  for (size_t i=0; i < t->components.size(); i++)
    if (t->CompName(i) == fld->s) {
      fType = t->CompType(i);
      return true;
    }
          
  errStream << fld->loc << ": "
            << " Unknown field " << fld->s
            << " in structure "
            << t->defAst->s 
            << std::endl;
  fType = GC_NULL;
  return false;
}

static bool
findComponent(std::ostream& errStream, 
              shared_ptr<Type> sut, shared_ptr<AST> ast,
              shared_ptr<Type> &fct, bool orMethod = false)
{
  sut = sut->getType();
  assert(ast->astType == at_select || 
         ast->astType == at_sel_ctr || 
         ast->astType == at_fqCtr);
  fct = GC_NULL;

  if (sut->isUType())
    sut = obtainFullUnionType(sut)->getType();
  
  if (sut->components.empty()) {
    errStream << ast->loc << ": "
              << "cannot dereference fields as only "
              << "an opaque declaration is available."
              << std::endl;
    return false;
  }
  
  bool valid=false;
  for (size_t i=0; i < sut->components.size(); i++) {
    if (sut->CompName(i) == ast->child(1)->s) {
      fct = sut->CompType(i)->getType();          
      valid = ((sut->CompFlags(i) & COMP_INVALID) == 0);
      break;
    }
  }
      
  if (orMethod && !fct) {
    for (size_t i=0; i < sut->methods.size(); i++) {
      if (sut->MethodName(i) == ast->child(1)->s) {
        fct = sut->MethodType(i)->getType();          
        valid = ((sut->MethodFlags(i) & COMP_INVALID) == 0);
        break;
      }
    }
  }

  if (!fct) {
    errStream << ast->loc << ": "
              << " In the expression " << ast->asString() << ", "
              << " structure/constructor " << sut->defAst->s 
              << " has no Field/Constructor"
              << (orMethod ? "/Method" : "")
              << " named " 
              << ast->child(1)->s << "." << std::endl;
    return false;
  } 

  if (!valid) {
    errStream << ast->child(0)->loc << ": "
              << " The expression " << ast->asString()
              << " has no field " 
              << ast->child(1)->s << "." << std::endl;

    fct = GC_NULL;
    return false;
  }

  return true;
}

static bool
testNonEscaping(std::ostream& errStream, shared_ptr<AST> errAst,
                shared_ptr<Type> t)
{
  if(t->isNonEscaping()) {
    errStream << errAst->loc << ": Non-Capturable type "
              << t->asString()
              << " in Captuarable/Escape position."
              << std::endl;
    return false;
  }
  
  return true;
}

static bool
ProcessLetExprs(std::ostream& errStream, shared_ptr<AST> lbs, 
                shared_ptr<TSEnvironment > gamma,
                shared_ptr<InstEnvironment > instEnv,
                TypeAstMap& impTypes,
                bool &rewrite, shared_ptr<TCConstraints> tcc,
                shared_ptr<Trail> trail,
                ResolutionMode mode, TI_Flags ti_flags)
{
  bool errFree = true;
  for (size_t c = 0; c < lbs->children.size(); c++) {
    shared_ptr<AST> lb = lbs->child(c);
    shared_ptr<AST> expr = lb->child(1);
    TYPEINFER(expr, gamma, instEnv, impTypes, rewrite, tcc,
              trail, USE_MODE, TI_EXPRESSION);
    
    CHKERR(errFree, testNonEscaping(errStream, expr, expr->symType));
  }
  return errFree;
}

static bool
ProcessLetBinds(std::ostream& errStream, shared_ptr<AST> lbs, 
                shared_ptr<TSEnvironment > gamma,
                shared_ptr<InstEnvironment > instEnv,
                TypeAstMap& impTypes,
                bool &rewrite, shared_ptr<TCConstraints> tcc,
                shared_ptr<Trail> trail,
                ResolutionMode mode, TI_Flags ti_flags)
{
  bool errFree = true;
  for (size_t c = 0; c < lbs->children.size(); c++) {
    shared_ptr<AST> lb = lbs->child(c);
    shared_ptr<AST> idPat = lb->child(0);
    
    TYPEINFER(idPat, gamma, instEnv, impTypes, rewrite, tcc,
              trail, DEF_MODE, TI_EXPRESSION);
  }
  return errFree;
}

static bool
UnifyLetBinds(std::ostream& errStream, shared_ptr<AST> lbs,
              shared_ptr<Trail> trail)
{
  bool errFree = true;
  for (size_t c = 0; c < lbs->children.size(); c++) {
    shared_ptr<AST> lb = lbs->child(c);
    shared_ptr<AST> id = lb->getID();
    shared_ptr<AST> expr = lb->child(1);
    
    // Note: It is safe to say MBF(id->symType) because
    // bindIdentDef() introduces identifiers with MBF()
    // always.
    UNIFY(trail, id->loc, expr->symType, MBF(id->symType));
    
    lb->symType = id->symType;

    // In the case of heuristic inference, fix the top-most level
    // mutability based on the lexical usage analysis of the
    // identifier. This must be done in the case of local variables
    // only. So, the code is within unifyletbinds
    if (Options::heuristicInference) {
      shared_ptr<Type> idType = id->symType->getType();

      if ((id->flags & ID_IS_MUTATED) && !idType->isMutable()) {
        std::stringstream ss;  
        shared_ptr<Type> mTv = Type::make(ty_mutable, newTvar());
        
        // Not reporting an error here, since it will be reported
        // later when the identifier is actually used.
        unify(ss, trail, id->loc, idType, mTv, UFLG_NO_FLAGS);
      }
    }
  }
  return errFree;
}

static void
makeLetGather(shared_ptr<AST> lbs, shared_ptr<AST> &bAst, shared_ptr<AST> &vAst)
{
  // Because all types in a letrec share a context, we need a
  // container form to glue things together temporarily.
  // The following loop constructs the container in reversed
  // order, but that is okay, because the container only exists
  // to let mutual recursion unify correctly.
  bAst = AST::make(at_letGather, lbs->child(0)->loc);
  vAst = AST::make(at_letGather, lbs->child(0)->loc);
  shared_ptr<Type> bType = Type::make(ty_letGather);
  shared_ptr<Type> vType = Type::make(ty_letGather);
  
  for (size_t c = 0; c < lbs->children.size(); c++) {
    shared_ptr<AST> lb = lbs->child(c);
    
    bAst->addChild(lb->child(0));
    vAst->addChild(lb->child(1));
    bType->components.push_back(comp::make(lb->getID()->symType));
    vType->components.push_back(comp::make(lb->child(1)->symType));
  }
  
  bAst->symType = bType;
  vAst->symType = vType;
}

/// @brief Abort on unrecoverable error
void
die()
{
  std::cerr << "Internal Compiler Error, Aborting." << std::endl;
  throw 0;
}
        
/**************************************************************/
/*                Type consistency checkng                   */
/**************************************************************/

static bool
checkImpreciseTypes(std::ostream& errStream, 
                    const shared_ptr<TSEnvironment > gamma,
                    TypeAstMap& impTypes)
{
  bool errFree = true;
  for (TypeAstMap::iterator itr = impTypes.begin();
       itr != impTypes.end(); ++itr) {
    shared_ptr<Type> t = itr->first->getBareType();
    shared_ptr<AST> ast = itr->second;
    switch(t->kind) {
    case ty_array:
      {
        if (t->arrLen->len == 0) {
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
                 const shared_ptr<TypeScheme> defSigma,
                 const shared_ptr<TypeScheme> declSigma,
                 const shared_ptr<AST> declAst)
{
  bool errFree = true;
  
  shared_ptr<TCConstraints> defTcc = TCConstraints::make();
  shared_ptr<TCConstraints> declTcc = TCConstraints::make();
  MarkFlags unmatched = MARK_CHECK_CONSTRAINTS;

  defSigma->addConstraints(defTcc);
  declSigma->addConstraints(declTcc);

  if (defTcc->size() != declTcc->size()) 
    errFree = false;
  
  for (TypeSet::iterator itr = defTcc->begin();
      errFree && itr != defTcc->end(); ++itr)
    (*itr)->mark |= unmatched;
  for (TypeSet::iterator itr_j = declTcc->begin();
      errFree && itr_j != declTcc->end(); ++itr_j)
    (*itr_j)->mark |= unmatched;

  for (TypeSet::iterator itr = defTcc->begin();
      errFree && itr != defTcc->end(); ++itr) {
    shared_ptr<Typeclass> defct = (*itr);
      
    if ((defct->mark & unmatched) == 0)
      continue;
      
    bool unified = false;
      
    for (TypeSet::iterator itr_j = declTcc->begin();
        errFree && itr_j != declTcc->end(); ++itr_j) {
      shared_ptr<Typeclass> declct = (*itr_j);

      if ((defct->mark & unmatched) == 0)
        continue;
      
      if (defct->strictlyEquals(declct)) {
        defct->mark &= ~unmatched;
        declct->mark &= ~unmatched;
        unified = true;
        break;
      }
    }
            
    if (!unified)
      errFree = false;
  }

  for (TypeSet::iterator itr = defTcc->begin();
      errFree && itr != defTcc->end(); ++itr)
    if ((*itr)->mark & unmatched)
      errFree = false;
  for (TypeSet::iterator itr_j = declTcc->begin();
      errFree && itr_j != declTcc->end(); ++itr_j)
    if ((*itr_j)->mark & unmatched)
      errFree = false;  

  if (!errFree) {
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

/// @brief Return true if the type described by sigmaA is at least as general
/// as the type described by sigmaB.
///
/// Returns false if the two types are not compatible.
///
/// Concept: If the result of unifying the two types sigmaA and sigmaB
/// succeeds and does not reduce the number of free type variables in
/// sigmaB, then unification with sigmaA did not cause a resolution of
/// any type variable in sigmaB, and sigmaA must therefore be at least
/// as general as sigmaB.
static bool
isAsGeneral(std::ostream& errStream,
            shared_ptr<Trail> trail,
            shared_ptr<const TSEnvironment > gamma,
            shared_ptr<InstEnvironment > instEnv,
            shared_ptr<TypeScheme> sigmaA,
            shared_ptr<TypeScheme> sigmaB)
{
  bool isAsGeneral = true;

  shared_ptr<AST> astB = sigmaB->ast;

  size_t num_B_tvs = sigmaB->ftvs.size();

  {
    shared_ptr<Trail> testTrail = Trail::make();
    bool errFree = true;
    UNIFY(testTrail, LexLoc(),
          sigmaA->tau->getType(), sigmaB->tau->getType());
    CHKERR(isAsGeneral, errFree);
    
    TypeSet gottenTypes;
    for(TypeSet::iterator itr = sigmaB->ftvs.begin();
        isAsGeneral && itr != sigmaB->ftvs.end(); ++itr) {
      shared_ptr<Type> ftv = (*itr)->getType();
      gottenTypes.insert(ftv);
      CHKERR(isAsGeneral, (ftv->kind == ty_tvar));
    }

    if (isAsGeneral)
      CHKERR(isAsGeneral, checkConstraints(errStream, sigmaA, sigmaB, astB));

    CHKERR(isAsGeneral, gottenTypes.size() == num_B_tvs);

    testTrail->rollBack();
  }

  return isAsGeneral;
}

/* Checks the types of:
     - A definition vs Declaration
     - A declaration vs a previous declaration
   for EXACT compatibility */
static bool
matchDefDecl(std::ostream& errStream, 
             shared_ptr<Trail> trail,
             shared_ptr<const TSEnvironment > gamma,
             shared_ptr<InstEnvironment > instEnv,
             shared_ptr<TypeScheme> declSigma,
             shared_ptr<TypeScheme> defSigma,
             TI_Flags ti_flags)
{
  if (ti_flags & TI_DEF_DECL_NO_MATCH)
    return true;  
  
  bool errorFree = true;   
  const shared_ptr<AST> decl = declSigma->ast;
  shared_ptr<const AST>  def = defSigma->ast;
  bool verbose = false;
  DEBUG(DEF_DECL) 
    verbose = true;
  
  if (declSigma->ftvs.size() != defSigma->ftvs.size()) {
    errorFree = false;
  }
  else {
    shared_ptr<TypeScheme> declTS = declSigma;
    shared_ptr<TypeScheme> defTS = defSigma;
    shared_ptr<Type> declT = declTS->tau->getType();
    shared_ptr<Type> defT  = defTS->tau->getType();
    
    CHKERR(errorFree, declT->strictlyEquals(defT, verbose));
    if (errorFree)
      CHKERR(errorFree, checkConstraints(errStream, defTS, declTS, decl));
    
    // Rigidness preservation:
    // Make sure that after unification, the declaration/definition is
    // no less general than what was previously declared.
    // The number of type variables are initially determined to be
    // equal, but this check catches errors such as:
    // 1) DECL: \/'a. 'a -> int
    //    DEF:  \/'a. int -> 'a 
    //    POST UNIFY: int -> int
    // 2) DECL: \/'a,'b. 'a -> 'b -> 'b
    //    DEF:  \/'a,'b. 'a -> 'a -> 'b
    //    POST UNIFY: \/'a. 'a -> 'a -> 'a
    {
      TypeSet gottenTypes;

      for (TypeSet::iterator itr = defTS->ftvs.begin(); 
          errorFree && itr != defTS->ftvs.end(); ++itr) {
        shared_ptr<Type> ftv = (*itr)->getType();
        gottenTypes.insert(ftv);
        CHKERR(errorFree, (ftv->kind == ty_tvar));
      }

      CHKERR(errorFree, gottenTypes.size() == defTS->ftvs.size());
    }
  }

  if (!errorFree)
    errStream << def->loc <<": The type of " << def->s 
              << " at definition/declaration "  << defSigma->asString()
              << " does not match that of "
              << std::endl
              << decl->loc << ": declaration / definition "
              << declSigma->asString() << " EXACTLY."
              << std::endl;
  
  return errorFree;
}


/* Wrapper around Type's checkMutConsistency function that prints an 
   error message. This function is used wherever the programmer writes
   an explicit type annotation, to check that the annotated type is
   consistent wrt mutability (that is, it does not include types such
   as (mutable (pair bool int32)) */
static bool
CheckMutConsistency(std::ostream& errStream, 
                    const sherpa::LexLoc &errLoc,
                    shared_ptr<Type> t)
{
  bool errFree = t->checkMutConsistency();
  if(!errFree) 
    errStream << errLoc << ": Type Annotation "
              << " is inconsistent wrt mutability."
              << std::endl;
  return errFree;
}

/* Due to the presence of explicit programmer annotations, the unifier
   alone cannot sufficiently check the consistency of mutability at
   all AST positions. For example, consider:
   
   (defstruct St f:bool)

   (lambda (x)                  x: 'a|'b
     (let ((p x))               p: 'c|'b
        (set! p p)              p:  M'd|'b
        x:St))                  x:St, p:M'd|St  ;; ERROR 

  The uniffier cannot detect this error since it is due to non-local 
  propagation --  the type of p is not involved in the unification at 
  the expression x:St. Therefore, we introduce an extra consistency 
  checking pass that traverses all ASTs and reports an error if an 
  inconsistent type is found.

  This does not affect inferred types since a failure at this step 
  can only lead to a type error. This pass is only necessary if we 
  have explicit type annotation (albeit in the form of structure 
  definitions) where we control the type of all instantiations 
  and copies at one place. */

static bool
CheckMutConsistency(std::ostream& errStream, 
                    shared_ptr<AST> ast)
{
  bool errFree = true;

  if(ast->symType &&
     // error is better reported on the identifier than the
     // let-binding, which will have a ty_letgather type
     ast->astType != at_letbindings && ast->astType != at_letbinding)
    CHKERR(errFree, ast->symType->checkMutConsistency());
  
  if(!errFree) { 
    errStream << ast->loc << ": Unsound Mutable Type "
              << ast->symType->asString()
              << std::endl;
    return false;
  }

  for (size_t i = 0; errFree && i < ast->children.size(); i++) 
    CHKERR(errFree, CheckMutConsistency(errStream, ast->child(i)));
  
  return errFree;
}


/**************************************************************/
/****                   MAIN INFERENCE ROUTINES            ****/
/**************************************************************/

static bool
InferTvList(std::ostream& errStream, shared_ptr<AST> tvList, 
            shared_ptr<TSEnvironment > gamma,
            shared_ptr<InstEnvironment > instEnv,
            TypeAstMap& impTypes,
            bool &rewrite, 
            shared_ptr<TCConstraints> tcc,
            shared_ptr<Trail> trail,
            ResolutionMode mode, TI_Flags ti_flags,
            shared_ptr<Type> container)  
{  
  bool errFree = true;
  for (size_t i = 0; i < tvList->children.size(); i++) {
    shared_ptr<AST> tv = tvList->child(i);
    TYPEINFER(tv, gamma, instEnv, impTypes, rewrite, 
              tcc, trail, DEF_MODE, ti_flags | TI_TYP_EXP);
    shared_ptr<Type> tvType = tv->symType->getType();
    assert(tvType->kind == ty_tvar);
    tvType->flags |= TY_RIGID;
    container->typeArgs.push_back(tvType);
  }

  return errFree;
}


// Tvs are not added to the typeScheme in InferTvList itself because
// this has to be done after all the components (fields/constructors)
// of a type have been processed. Otherwise, the type will be
// polymorphic within itself.
static void
addTvsToSigma(std::ostream& errStream, shared_ptr<AST> tvList, 
              shared_ptr<TypeScheme> sigma, shared_ptr<Trail> trail)  
{  
  for (size_t i = 0; i < tvList->children.size(); i++) {
    shared_ptr<AST> tv = tvList->child(i);
    shared_ptr<Type> tvType = tv->symType->getType();
    assert(tvType->kind == ty_tvar);
    sigma->ftvs.insert(tvType);
  }
}

// In case of value type definitions, mark all those  
// type arguments that are candidiates for copy-compatibility.
static void
markCCC(shared_ptr<Type> ct)
{
  if (!ct->isValType())
    return;

  // We first need to mark all arguments CCCOK, and then remove
  // those that are not OK so that determoneCCC can get recursive
  // type definitions right.
  for (size_t i = 0; i < ct->typeArgs.size(); i++) {
    shared_ptr<Type> arg = ct->TypeArg(i)->getType();
    arg->flags |= TY_CCC;
  }
  
  for (size_t i = 0; i < ct->typeArgs.size(); i++) {
    shared_ptr<Type> arg = ct->TypeArg(i)->getType();      
    if (!arg->determineCCC(ct))
      arg->flags &= ~TY_CCC;
  }
}

// Called only for definitions
static bool
InferStruct(std::ostream& errStream, shared_ptr<AST> ast, 
            shared_ptr<TSEnvironment > gamma,
            shared_ptr<InstEnvironment > instEnv,
            TypeAstMap& impTypes,
            bool &rewrite, 
            shared_ptr<TCConstraints> tcc,
            shared_ptr<Trail> trail,
            ResolutionMode mode,
            bool isReference,
            bool mustDefine,
            bool mustEvalBody,
            TI_Flags ti_flags)
{
  bool errFree = true;
  Kind structKind;
  
  shared_ptr<AST> sIdent = ast->child(0);

  // match at_ident
  structKind = (isReference)? ty_structr : ty_structv;
   
  shared_ptr<Type> st = Type::make(structKind);
  st->defAst = sIdent;
  st->myContainer = sIdent;
  sIdent->symType = st;
  shared_ptr<TypeScheme> sigma = TypeScheme::make(st, sIdent, TCConstraints::make());

  // match at_tvlist
  shared_ptr<AST> tvList = ast->child(1);
  CHKERR(errFree, InferTvList(errStream, tvList, gamma, instEnv, impTypes, 
                              rewrite, sigma->tcc, trail, DEF_MODE, 
                              ti_flags | TI_TYP_EXP, st));
  sIdent->scheme = sigma;

  // Type all constraints
  TYPEINFER(ast->child(5), gamma, instEnv, impTypes, rewrite, 
            sigma->tcc, trail, mode, TI_CONSTRAINT);
  
  shared_ptr<TypeScheme> declTS = gamma->getBinding(sIdent->s);
  unsigned long bindFlags = 0;
  if (declTS) {
    declTS->tau->getBareType()->defAst = sIdent;
    bindFlags = BF_REBIND;
  }
  gamma->addBinding(sIdent->s, sigma);
  gamma->setFlags(sIdent->s, bindFlags);

  // Ignore the category

  // match at_declares
  TYPEINFER(ast->child(3), gamma, instEnv, impTypes, rewrite, sigma->tcc,
            trail, mode, ti_flags);
    
  // match at_fields
  shared_ptr<AST> fields = ast->child(4);
  for (size_t c = 0; c < fields->children.size(); c++) {
    // match at_ident
    // match agt_type
    shared_ptr<AST> field = fields->child(c);
    TYPEINFER(field, gamma, instEnv, impTypes, rewrite, 
              sigma->tcc, trail,  USE_MODE, 
              ti_flags | TI_TYP_EXP | TI_TYP_DEFN);
    
    CHKERR(errFree, CheckMutConsistency(errStream,
                                        field->loc, field->symType));
    
    switch(field->astType) {
    case at_field:
      {
        st->components.push_back(comp::make(field->child(0)->s,
                                            field->child(1)->symType));
        break;
      }
      
    case at_fill:
      {
        ast->total_fill += field->field_bits;
        break;
      }
    case at_methdecl:
      {
        st->methods.push_back(comp::make(field->child(0)->s,
                                         field->child(1)->symType));
        field->child(1)->symType->myContainer = sIdent;
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
  
  // In case of value type definitions, mark all those  
  // type arguments that are candidiates for copy-compatibility.
  markCCC(st);
  
  // Set the main AST's type.
  ast->symType = sIdent->symType;
   
  // Solve current Predicates.
  CHKERR(errFree, sigma->solvePredicates(errStream, ast->loc,
                                         instEnv, trail)); 

  // Ensure that the definition matches the declarations
  if (declTS)
    CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
                                 declTS, sigma, ti_flags));
  
  return errFree;
}

// Called only for definitions
static bool
InferObject(std::ostream& errStream, shared_ptr<AST> ast, 
            shared_ptr<TSEnvironment > gamma,
            shared_ptr<InstEnvironment > instEnv,
            TypeAstMap& impTypes,
            bool &rewrite, 
            shared_ptr<TCConstraints> tcc,
            shared_ptr<Trail> trail,
            ResolutionMode mode,
            bool isReference,
            bool mustDefine,
            bool mustEvalBody,
            TI_Flags ti_flags)
{
  bool errFree = true;
  Kind structKind;
  
  shared_ptr<AST> sIdent = ast->child(0);

  // match at_ident
  structKind = (isReference)? ty_objectr : ty_objectv;
   
  shared_ptr<Type> st = Type::make(structKind);
  st->defAst = sIdent;
  st->myContainer = sIdent;
  sIdent->symType = st;
  shared_ptr<TypeScheme> sigma = TypeScheme::make(st, sIdent, TCConstraints::make());

  // match at_tvlist
  shared_ptr<AST> tvList = ast->child(1);
  CHKERR(errFree, InferTvList(errStream, tvList, gamma, instEnv, impTypes, 
                              rewrite, sigma->tcc, trail, DEF_MODE, 
                              TI_TYP_EXP, st));
  sIdent->scheme = sigma;

  // Type all constraints
  TYPEINFER(ast->child(5), gamma, instEnv, impTypes, rewrite, 
            sigma->tcc, trail,  mode, TI_CONSTRAINT);
  
  shared_ptr<TypeScheme> declTS = gamma->getBinding(sIdent->s);
  unsigned long bindFlags = 0;
  if (declTS) {
    declTS->tau->getBareType()->defAst = sIdent;
    bindFlags = BF_REBIND;
  }
  gamma->addBinding(sIdent->s, sigma);
  gamma->setFlags(sIdent->s, bindFlags);

  // Ignore the category

  // match at_declares
  TYPEINFER(ast->child(3), gamma, instEnv, impTypes, rewrite, sigma->tcc,
            trail,  mode, TI_NO_FLAGS);
    
  // match at_fields
  shared_ptr<AST> fields = ast->child(4);
  for (size_t c = 0; c < fields->children.size(); c++) {
    // match at_ident
    // match agt_type
    shared_ptr<AST> field = fields->child(c);
    TYPEINFER(field, gamma, instEnv, impTypes, rewrite, 
              sigma->tcc, trail,  USE_MODE, 
              TI_TYP_EXP | TI_TYP_DEFN);
    
    CHKERR(errFree, CheckMutConsistency(errStream,
                                        field->loc, field->symType));
    
    assert(field->astType == at_methdecl);

    st->methods.push_back(comp::make(field->child(0)->s,
                                     field->child(1)->symType));
    field->child(1)->symType->myContainer = sIdent;
    break;
  }
  
  // Add Ftvs so that they get generalized in future uses
  addTvsToSigma(errStream, tvList, sigma, trail); 
  
  // In case of value type definitions, mark all those  
  // type arguments that are candidiates for copy-compatibility.
  markCCC(st);
  
  // Set the main AST's type.
  ast->symType = sIdent->symType;
   
  // Solve current Predicates.
  CHKERR(errFree, sigma->solvePredicates(errStream, ast->loc,
                                         instEnv, trail)); 

#if 0
  // Ensure that the definition matches the declarations
  if (declTS)
    CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
                                 declTS, sigma, uflags, false));
#endif
  
  return errFree;
}


// Called only for definitions
static bool
InferUnion(std::ostream& errStream, shared_ptr<AST> ast, 
           shared_ptr<TSEnvironment > gamma,
           shared_ptr<InstEnvironment > instEnv,
           TypeAstMap& impTypes,
           bool &rewrite, 
           shared_ptr<TCConstraints> tcc,
           shared_ptr<Trail> trail,
           ResolutionMode mode,
           bool isReference,
           bool mustDefine,
           bool mustEvalBody,
           TI_Flags ti_flags)
{

  bool errFree = true;
  Kind unionKind;
  
  shared_ptr<AST> uIdent = ast->child(0);

  // match at_ident
  unionKind = (isReference)? ty_unionr : ty_unionv;
  
  shared_ptr<Type> ut = Type::make(unionKind);
  ut->defAst = uIdent;
  ut->myContainer = uIdent;
  uIdent->symType = ut;
  shared_ptr<TypeScheme> sigma = TypeScheme::make(ut, uIdent, TCConstraints::make());
  
  // match at_tvlist
  shared_ptr<AST> tvList = ast->child(1);
  CHKERR(errFree, InferTvList(errStream, tvList, gamma, instEnv, impTypes, 
                              rewrite, sigma->tcc, trail, DEF_MODE, 
                              ti_flags | TI_TYP_EXP, ut));
  uIdent->scheme = sigma;
  
  // Type all constraints
  TYPEINFER(ast->child(5), gamma, instEnv, impTypes, rewrite, 
            sigma->tcc, trail, mode, TI_CONSTRAINT);
  
  shared_ptr<TypeScheme> declTS = gamma->getBinding(uIdent->s);
  unsigned long bindFlags = 0;
  
  if (declTS) {
    declTS->tau->getType()->defAst = uIdent;
    bindFlags = BF_REBIND;
  }
  gamma->addBinding(uIdent->s, sigma);
  gamma->setFlags(uIdent->s, bindFlags);
  
  // Ignore the category
  
  // match at_declares
  shared_ptr<AST> declares = ast->child(3);
  TYPEINFER(declares, gamma, instEnv, impTypes, rewrite, sigma->tcc,
            trail, mode, ti_flags);
  
  
  // match at_constructors
  shared_ptr<AST> ctrs = ast->child(4);
  for (size_t c = 0; c < ctrs->children.size(); c++) {
    // match at_ident
    // match agt_type
    shared_ptr<AST> ctr = ctrs->child(c);
    shared_ptr<AST> ctrId = ctr->child(0);    
    // Careful: 
    // Constructors with components are typed ucon 
    // and those without any are typed uval.
    Kind ctrKind;

    if (ctr->children.size() > 1)
      ctrKind = (isReference) ? ty_uconr : ty_uconv;
    else
      ctrKind = (isReference) ? ty_uvalr : ty_uvalv;
    
    ctrId->symType = Type::make(ctrKind);
    ctrId->symType->defAst = ctrId;
    ctrId->symType->myContainer = uIdent;
    ctr->symType = ctrId->symType;
    
    for (size_t i = 1; i < ctr->children.size(); i++) {
      shared_ptr<AST> field = ctr->child(i);
      TYPEINFER(field, gamma, instEnv, impTypes, rewrite, 
                sigma->tcc, trail,  USE_MODE, 
                ti_flags | TI_TYP_EXP | TI_TYP_DEFN);
      
      CHKERR(errFree, CheckMutConsistency(errStream,
                                          field->loc, field->symType));
      
      switch(field->astType) {
      case at_field:
        {
          shared_ptr<comp> nComp = comp::make(field->child(0)->s,
                                              field->child(1)->symType);
          if (field->flags & FLD_IS_DISCM)
            nComp->flags |= COMP_UNIN_DISCM;
          
          ctrId->symType->components.push_back(nComp);
          break;
        }
      case at_fill:
        {
          ctr->total_fill += field->field_bits;
          break;
        }

        // Note: at_methdecl illegal here, so omission is intentional.
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
    shared_ptr<TypeScheme> ctrSigma = TypeScheme::make(ctrId->symType, 
                                                       ctrId, sigma->tcc);
    
    // This may feel wierd -- that All constructors point to the same
    // type arguments rather than a copy. But since every instance is 
    // newly obtained, this is OK.
    for (size_t i = 0; i < tvList->children.size(); i++)
      ctrId->symType->typeArgs.push_back(tvList->child(i)->symType);
    
    // Don't add ctrSigma to gamma yet. Constructors are 
    // bound at the end of the definition
    ctrId->scheme = ctrSigma;
    
    shared_ptr<comp> nComp = comp::make(ctrId->s, ctrId->symType);
    uIdent->symType->components.push_back(nComp);
  } 

  // Add Ftvs so that they get generalized in future uses
  // Mark that the structure must always be copied.
  // It is important that this step be done late so that the recursive
  // uses do not prompt copy.
  addTvsToSigma(errStream, tvList, sigma, trail);
  
  // In case of value type definitions, mark all those  
  // type arguments that are candidiates for copy-compatibility.
  markCCC(ut);
  
  // Solve current Predicates.
  CHKERR(errFree, sigma->solvePredicates(errStream, ast->loc,
                                         instEnv, trail)); 
  
  //Now add all constructor bindings to the environment.
  for (size_t c = 0; c < ctrs->children.size(); c++) {
    shared_ptr<AST> ctr = ctrs->child(c);
    shared_ptr<AST> ctrId = ctr->child(0);   
    addTvsToSigma(errStream, tvList, ctrId->scheme, trail);
    gamma->addBinding(ctrId->s, ctrId->scheme);
    
    // Solve current Predicates.
    // Since we all share the same constraints, automatically solved.
  }

  //Build structure types for all constructors.
  for (size_t c = 0; c < ctrs->children.size(); c++) {
    shared_ptr<AST> ctr = ctrs->child(c)->child(0);
    shared_ptr<Type> ctrType = ctr->symType->getType();
    shared_ptr<TypeScheme> ctrSigma = ctr->scheme;
    shared_ptr<Type> sType = GC_NULL;
    shared_ptr<TypeScheme> stSigma = GC_NULL;
    
    for (size_t i=0; i < c; i++) {
      shared_ptr<AST> thatCtr = ctrs->child(i)->child(0);
      shared_ptr<Type> thatCtrType = thatCtr->symType->getType();
      
      if (ctrType->components.size() != 
          thatCtrType->components.size())
        continue;
      
      // Since there can be no constructors with only fills
      // and no field names are repeated, it is sufficient
      // to check types regardless of fills.
      bool same = true;
      for (size_t j=0; j < ctrType->components.size(); j++) {        
        shared_ptr<comp> thisComp = ctrType->Component(j);
        shared_ptr<comp> thatComp = thatCtrType->Component(j);
        
        if ((thisComp->name != thatComp->name) ||
            !thisComp->typ->strictlyEquals(thatComp->typ)) {
          same = false;
          break;
        }
      }
         
      if (same) {
        assert(thatCtr->stSigma);
        stSigma = thatCtr->stSigma;
        ctr->stSigma = thatCtr->stSigma;
        ctr->stCtr = thatCtr;
        break;
      }
    }
    
    if (!stSigma) {
      Kind ctrStructKind = (isReference)?ty_structr:ty_structv;      
      sType = Type::make(ctrStructKind);
      sType->defAst = ctr; // structures have names like (cons 'a)
      for (size_t i=0; i < ctrType->components.size(); i++)
        sType->components.push_back(comp::make(ctrType->CompName(i),
                                               ctrType->CompType(i)));
      // Comp's Flags are not necesary here ?
 
      
      for (size_t i=0; i < ctrType->typeArgs.size(); i++)
        sType->typeArgs.push_back(ctrType->TypeArg(i));
      
      stSigma = TypeScheme::make(sType, ctr, sigma->tcc); 
      stSigma->ftvs = ctrSigma->ftvs;

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
  if (ast->flags & UNION_IS_REPR) {
    if (declares->tagType) {
      errStream << ast->loc << ": "
                << "tag-type declarations cannot be "
                << "given with defreprs."
                << std::endl;
      errFree = false;      
    }
  }
  else if (errFree) {  

    //Check if we can do Cardelli Optimization:
    
    unsigned long long maxCtrs = 0;  
    size_t lastTagValue = (ctrs->children.size() - 1);
    size_t lastTagValueCardelli = lastTagValue;

    if (declares->tagType) {
      maxCtrs = (((unsigned long long)1) << declares->nBits());

      if (lastTagValue > (maxCtrs - 1)) {
        errStream << ast->loc << ": "
                  << "Not enough bits in the tag-type to represent "
                  << "all Constructors. Use a bigger tag-type. "
                  << "[If no tag-type declaration is found, "
                  << "the defalut is `word']"
                  << std::endl;
        errFree = false;
      }      
    }
    else if (ctrs->children.size() == 1) {
      declares->tagType = Type::make(ty_word);
      assert(declares->field_bits == 0);
      uIdent->flags |= SINGLE_LEG_UN;
    }
    else {      
      declares->tagType = Type::make(ty_word);     
      assert(declares->field_bits == 0);

      maxCtrs = (((unsigned long long)1) << declares->nBits());          
      
      bool cardelli = true;
      bool seenRef = false;
      bool isEnum = true;
      
      for (size_t c = 0; 
           cardelli && (c < ctrs->children.size()); 
           c++) {
        shared_ptr<AST> ctr = ctrs->child(c);
        
        switch(ctr->children.size()) {
        case 0:
          assert(false);
          break;

        case 1:
          break;
          
        case 2:
          isEnum = false;

          if (seenRef) {
            cardelli = false;
            break;
          }
          
          if (ctr->child(1)->symType->isRefType() || 
              ctr->child(1)->symType->isConstrainedToRefType(sigma->tcc) ||
              ctr->child(1)->symType->isNullableType())
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

      if (isEnum) {
        assert(!seenRef);
        cardelli = false;
        uIdent->flags |= ENUM_UN;
      }
      else if (cardelli) {        
        assert(!isEnum);
        uIdent->flags |= CARDELLI_UN;
        if (isNullable)
          uIdent->flags |= NULLABLE_UN;
        lastTagValueCardelli = (2 * lastTagValue) - 1;
      }
      
      DEBUG(UNION_INF)
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
  if (declTS)
    CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
                                 declTS, sigma, ti_flags));
  
  return errFree;
}

bool
superDAG(shared_ptr<AST> super, shared_ptr<AST> curr)
{
  if (super ==  curr)
    return false;
  
  assert(super->scheme);
  shared_ptr<TCConstraints> tcc = super->scheme->tcc;
  assert(tcc);

  for (TypeSet::iterator itr = tcc->begin();
      itr != tcc->end(); ++itr) {
    shared_ptr<Typeclass> pred = (*itr);
    if (pred->flags & TY_CT_SELF)
      continue;

    if (superDAG(pred->defAst, curr) == false)
      return false;
  }
  return true;
}

static bool
InferTypeClass(std::ostream& errStream, shared_ptr<AST> ast, 
               shared_ptr<TSEnvironment > gamma,
               shared_ptr<InstEnvironment > instEnv,
               TypeAstMap& impTypes,
               bool &rewrite, 
               shared_ptr<TCConstraints> tcc,
               shared_ptr<Trail> trail,
               ResolutionMode mode,
               TI_Flags ti_flags)
{
  bool errFree = true;
  shared_ptr<AST> ident = ast->child(0);
  shared_ptr<Typeclass> tc = Typeclass::make(ty_typeclass);
  tc->defAst = ident;
  shared_ptr<TypeScheme> sigma = TypeScheme::make(tc, ident, TCConstraints::make());
  tc->flags |= TY_CT_SELF;
  sigma->tcc->addPred(tc);

  shared_ptr<AST> tvList = ast->child(1);
  CHKERR(errFree, InferTvList(errStream, tvList, gamma, instEnv, impTypes, 
                              rewrite, sigma->tcc, trail, DEF_MODE, 
                              ti_flags | TI_TYP_EXP, tc));
  addTvsToSigma(errStream, tvList, sigma, trail);
  ident->symType = tc;
  ident->scheme = sigma;
  
  // Type all constraints
  TYPEINFER(ast->child(5), gamma, instEnv, impTypes, rewrite, 
            sigma->tcc, trail, mode, 
            TI_CONSTRAINT | TI_TCC_SUB);

  // Typeclass Declarations
  shared_ptr<AST> tcdecls = ast->child(2);
  for (size_t c = 0; c < tcdecls->children.size(); c++) {
    shared_ptr<AST> tcdecl = tcdecls->child(c);
    assert(tcdecl->astType == at_tyfn);
    shared_ptr<AST> domain = tcdecl->child(0);               
    shared_ptr<AST> range =  tcdecl->child(1);
    shared_ptr<Type> tyfn = Type::make(ty_tyfn);
    tyfn->defAst = tcdecl;
    TYPEINFER(domain, gamma, instEnv, impTypes, rewrite, sigma->tcc, 
              trail, USE_MODE, ti_flags | TI_TYP_EXP);
    TYPEINFER(range, gamma, instEnv, impTypes, rewrite, sigma->tcc, 
              trail, USE_MODE, ti_flags | TI_TYP_EXP);
    tyfn->components.push_back(comp::make(domain->symType));
    tyfn->components.push_back(comp::make(range->symType));
    
    //errStream << "***"
    //          << domain->asString() << ": "
    //                 << domain->symType->asString(GC_NULL) 
    //                 << std::endl
    //                 << range->asString() << ": "
    //                 << range->symType->asString(GC_NULL)
    //                 << std::endl
    //                 << "tyfn = " << tyfn->asString(GC_NULL)
    //                 << std::endl;
    tc->addFnDep(tyfn);
  }

  shared_ptr<AST> methods = ast->child(4);
  for (size_t c = 0; c < methods->children.size(); c++) {
    shared_ptr<AST> method = methods->child(c);
    shared_ptr<AST> mID = method->child(0);
    shared_ptr<AST> mtType = method->child(1);
    
    TYPEINFER(mtType, gamma, instEnv, impTypes, rewrite, sigma->tcc,
              trail,  USE_MODE, ti_flags | TI_TYP_EXP);
    mID->symType = mtType->symType;
    CHKERR(errFree, CheckMutConsistency(errStream,
                                        mtType->loc, mtType->symType));
    
    shared_ptr<Type> mType = mID->symType->getType();
    mType->defAst = mID;
    mType->myContainer = ident;
    
    shared_ptr<TypeScheme> mSigma = TypeScheme::make(mType, mID, 
                                                     TCConstraints::make());
    for (TypeSet::iterator itr = sigma->tcc->begin();
        itr != sigma->tcc->end(); ++itr)
      mSigma->tcc->addPred((*itr));
 
    do { // Dummy loop
      if (!mType) {
        assert(!errFree); 
        break;
      }
        
      if (mType->kind != ty_fn) {
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
      shared_ptr<comp> nComp = comp::make(mID->s,mType); 
      ident->symType->components.push_back(nComp);      
    } while (0);                                   
  }

  if (!errFree)
    return false;
  
  gamma->addBinding(ident->s, ident->scheme);
  for (size_t c = 0; c < methods->children.size(); c++) {
    shared_ptr<AST> method = methods->child(c);
    shared_ptr<AST> mID = method->child(0);
    gamma->addBinding(mID->s, mID->scheme);
  }
  
  assert(!instEnv->getBinding(ident->fqn.asString()));

  InstanceSet *instSet = new set<shared_ptr<Instance> >;
  shared_ptr<InstanceSet> instSetPtr(instSet);
  
  instEnv->addBinding(ident->fqn.asString(), instSetPtr);
  ast->symType = ident->symType;
  return errFree;
}

static bool
InferInstance(std::ostream& errStream, shared_ptr<AST> ast, 
              shared_ptr<TSEnvironment > gamma,
              shared_ptr<InstEnvironment > instEnv,
              TypeAstMap& impTypes,
              bool &rewrite, 
              shared_ptr<TCConstraints> tcc,
              shared_ptr<Trail> trail,
              ResolutionMode mode,
              TI_Flags ti_flags)
{
  bool errFree = true;
  
  shared_ptr<AST> tcapp = ast->child(0);
  shared_ptr<AST> methods = ast->child(1);
  shared_ptr<AST> constraints = ast->child(2);
  
  shared_ptr<AST> TCident = tcapp;
  if (tcapp->children.size())
    TCident = tcapp->child(0);
  
  shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
  ast->envs.gamma = defGamma;
  ast->envs.instEnv = instEnv;
  
  ast->symType = Type::make(ty_tvar);
  shared_ptr<TCConstraints> myTcc = TCConstraints::make();
  TYPEINFER(tcapp, defGamma, instEnv, impTypes, rewrite,
            myTcc, trail, USE_MODE, TI_CONSTRAINT);

  // Mark myself
  for (TypeSet::iterator itr = myTcc->begin();
      itr != myTcc->end(); ++itr) {
    shared_ptr<Typeclass> pred = (*itr);
    if (pred->defAst == TCident->symbolDef)
      pred->flags |= TY_CT_SELF;
  }
  
  if (!errFree) 
    return false;
  
  // Type all constraints
  TYPEINFER(constraints, defGamma, instEnv, impTypes, rewrite,
            myTcc, trail, USE_MODE, TI_CONSTRAINT);

  
  if (!errFree) 
    return false;
      
  shared_ptr<Typeclass> tc = tcapp->symType->getType();

  // Get the set of current instances 
  shared_ptr<InstanceSet> currInsts = 
    instEnv->getBinding(tc->defAst->fqn.asString());

  
  if ((ti_flags & TI_ALL_INSTS_OK) == 0) {
    
    // Make sure that the instance definition is consistent
    // with the known functional dependencies.
    
    //errStream << ast->loc << ": #Preds = " << myTcc->pred->size()
    //                << std::endl;
    for (TypeSet::iterator itr = myTcc->begin();
        itr != myTcc->end(); ++itr) {
      shared_ptr<Typeclass> pred = (*itr)->getType();
      //errStream << "Processing : " << pred->asString()
      //          << std::endl;

      for (TypeSet::iterator itr_d = pred->fnDeps.begin(); 
          itr_d != pred->fnDeps.end(); ++itr_d) {
        shared_ptr<Typeclass> fnDep =  (*itr_d);
        TypeSet domain;
        TypeSet range;
        fnDep->Args()->collectAllftvs(domain);
        fnDep->Ret()->collectAllftvs(range);
          
        //errStream << "  Processing : " << fnDep->asString()
        //              << std::endl;
          
        for (TypeSet::iterator itr_j = range.begin();
            itr_j != range.end(); ++itr_j) {
          if (domain.find(*itr_j) == domain.end()) {
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

    if (!errFree) 
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
    for (InstanceSet::iterator itr = currInsts->begin();
        itr != currInsts->end(); ++itr) {
      shared_ptr<Instance> inst = (*itr);
      // Since Equals will not unify any variables in place,
      // I don't have to do a ts_instance_copy() here.
      shared_ptr<TCConstraints> theirTcc = inst->ast->scheme->tcc;

      for (TypeSet::iterator itr = myTcc->begin();
          itr != myTcc->end(); ++itr) {
        shared_ptr<Typeclass> myPred = (*itr)->getType();
        for (TypeSet::iterator itr_m = theirTcc->begin();
            itr_m != theirTcc->end(); ++itr_m) {
          shared_ptr<Typeclass> theirPred = (*itr_m)->getType();

          if ((myPred->defAst == theirPred->defAst) &&  
             (myPred->fnDeps.size() && theirPred->fnDeps.size()))
            for (TypeSet::iterator itr_j = myPred->fnDeps.begin();
                itr_j != myPred->fnDeps.end(); ++itr_j) {
              shared_ptr<Type> myFnDep =  (*itr_j)->getType();
              shared_ptr<Type> myDomain = myFnDep->Args();
                  
              for (TypeSet::iterator itr_k = 
                    theirPred->fnDeps.begin();
                  itr_k != theirPred->fnDeps.end(); ++itr_k) {
                shared_ptr<Type> theirFnDep =  (*itr_k)->getType();
                shared_ptr<Type> theirDomain = theirFnDep->Args();
                    
                assert(myFnDep->defAst == theirFnDep->defAst);
                    
                if ( myDomain->equals(theirDomain) && 
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
           
    if (!errFree) 
      return false;

  }
 
  tcapp->scheme = TypeScheme::make(tc, tcapp, myTcc);
      
  size_t nMethods = tc->components.size();

  if (methods->children.size() != nMethods) {
    errStream << ast->loc << ": "
              << "Type class" << tcapp->child(0)->s
              << " needs " << nMethods << " methods for "
              << "instantiation, but here obtained "
              << methods->children.size() << "."
              << std::endl;
    errFree = false;
    return errFree;
  }

  for (size_t i = 0; i < tc->components.size(); i++) {
    shared_ptr<Type> mtType = tc->CompType(i);
    std::string mtName = tc->CompName(i);

    bool found = false;
    for (size_t j = 0; j < methods->children.size(); j++) {
      shared_ptr<AST> method = methods->child(i);
      shared_ptr<AST> method_name = method->child(0);
      shared_ptr<AST> method_val = method->child(1);

      if(mtName == method_name->s) {
        found = true;

        method_name->symType = mtType;
        TYPEINFER(method_val, defGamma, instEnv, impTypes, rewrite, myTcc,
                  trail, USE_MODE, ti_flags);
        shared_ptr<Type> methodType = method_val->symType->getType();
      
        // Methods are functions, remove top mutability.
        UNIFY(trail, method->loc, 
              mtType, methodType->minimizeMutability());

        // Symbol resolver guarantees that there cannot be duplicate
        // method definitions.
        break;
      }
    }
    
    if(!found) {
      errStream << ast->loc << ": No definition for method "
                << mtName << " in this instance."
                << std::endl;
      errFree = false;
    }
  }

  if (!errFree) 
    return false;

  shared_ptr<TypeScheme> sigma = tcapp->scheme;

  gamma->mergeBindingsFrom(defGamma);
  sigma->generalize(errStream, ast->loc, gamma, instEnv, tcapp, 
                    GC_NULL, trail, gen_instance);      
  
  if (!errFree)
    return false;
  
  shared_ptr<Instance> myInstance = Instance::make(sigma, ast);
  
  if ((ti_flags & TI_ALL_INSTS_OK) == 0) {
    // Make sure there are no absolute conflicts 
    // with existing instances
    assert(currInsts);
  
    for (InstanceSet::iterator itr = currInsts->begin();
        itr != currInsts->end(); ++itr) {
      shared_ptr<Instance> inst = (*itr);
      if (inst->overlaps(myInstance)) {
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

    if (!errFree)
      return false; 
  }
  
  // Add current Predicate.
  currInsts->insert(myInstance);
  
  //errStream << "Added " << sigma->asString() 
  //            << " to " << tc->defAst->s << std::endl;
      
  ast->symType = tcapp->symType;
  ast->scheme =  tcapp->scheme;

  return errFree;
}

#if 0  
static bool
CheckLetrecFnxnRestriction(std::ostream &errStream, shared_ptr<AST> ast)
{
  bool errFree = true;
  switch(ast->astType) {
  case at_ident:
    {
      if (!ast->symType->isFnxn() && !ast->symType->isClosure()) {
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
      for (size_t c=0; c < ast->children.size(); c++)
        if (c != IGNORE(ast))
          CHKERR(errFree, CheckLetrecFnxnRestriction(errStream, 
                                                     ast->child(c)));
      break;
    }

  default:
    {
      for (size_t c=0; c < ast->children.size(); c++)
        CHKERR(errFree, CheckLetrecFnxnRestriction(errStream, 
                                                   ast->child(c)));
      break;
    }
  }
  return errFree;
}
#endif

static bool
typeInfer(std::ostream& errStream, shared_ptr<AST> ast, 
          shared_ptr<TSEnvironment > gamma,
          shared_ptr<InstEnvironment > instEnv,
          TypeAstMap& impTypes,
          bool &rewrite, 
          shared_ptr<TCConstraints> tcc,
          shared_ptr<Trail> trail,
          ResolutionMode mode,
          TI_Flags ti_flags)
{
  bool errFree = true;

  // Save the current environment in the AST.
  // If we create a new environment, we will update it later.
  ast->envs.gamma = gamma;
  ast->envs.instEnv = instEnv;  

  DEBUG(TI_AST)
    errStream << "INF: " << ast->loc << ": " 
              << ast->s << " [" << ast->astTypeName() << "]" 
              << "   mode = " << mode
              << std::endl;
  
  switch(ast->astType) {
  case agt_expr:
  case agt_expr_or_define:
  case agt_eform:
  case at_Null:
  case at_refCat:
  case at_valCat:
  case at_closed:
  case at_opaqueCat:
  case at_tcmethods:
  case at_tcmethod_binding:
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
  case agt_openclosed:
  case agt_ow:
  case agt_qtype:
  case agt_fielditem:
  case at_ifident:
  case at_localFrame:
  case at_frameBindings:
  case at_identList:
  case agt_ucon:
  case agt_uselhs:

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
      ast->symType = Type::make(ty_bool);
      break;
    }

  case at_charLiteral:
    {
      /*------------------------------------------------
          ___________________________________________
                 A |- CHAR_LITERAL: char
        ------------------------------------------------*/
      ast->symType = Type::make(ty_char);
      break;
    }

  case at_intLiteral:
    {
      /*------------------------------------------------
          ___________________________________________
                 A |- INT_LITEREAL: 'a \ IntLit('a)
        ------------------------------------------------*/
      
      if (Options::noPrelude) {
        ast->symType = Type::make(ty_word);
        break;
      }

      if (ti_flags & TI_NO_MORE_TC) {
        ast->symType = Type::make(ty_tvar);
        break;
      }

      const std::string& intLit = SpecialNames::spNames.sp_integral;
      shared_ptr<TypeScheme> icSigma = gamma->getBinding(intLit);
      assert(icSigma);
      
      shared_ptr<Typeclass> ic = icSigma->type_instance();
      assert(ic->typeArgs.size() == 1);
      ast->symType = ic->TypeArg(0)->getType();
      tcc->addPred(ic);
      break;
    }

  case at_floatLiteral:
    {
      /*------------------------------------------------
          ___________________________________________
                 A |- FLOAT_LITERAL: 'a \ FloatLit('a)
        ------------------------------------------------*/

      if (Options::noPrelude) {
        ast->symType = Type::make(ty_float);
        break;
      }

      if (ti_flags & TI_NO_MORE_TC) {
        ast->symType = Type::make(ty_tvar);
        break;
      }

      std::string& floatLit = SpecialNames::spNames.sp_fp;
      shared_ptr<TypeScheme> fcSigma = gamma->getBinding(floatLit);
      assert(fcSigma);
      
      shared_ptr<Typeclass> fc = fcSigma->type_instance();
      assert(fc->typeArgs.size() == 1);
      ast->symType = fc->TypeArg(0)->getType();
      tcc->addPred(fc);
      break;
    }
    
  case at_docString:
    // FIX: Not sure this is right. In truth, we really shouldn't be
    // bothering to type check these at all.
    {
      ast->symType = Type::make(ty_string);

      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, ti_flags);
      break;
    }

  case at_stringLiteral:
    {
      /*------------------------------------------------
          ___________________________________________
                    A |- STRING_LITERAL: string
        ------------------------------------------------*/
      ast->symType = Type::make(ty_string);
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
          shared_ptr<TypeScheme> sigma = gamma->getBinding(ast->s);

          if (sigma && ast->isGlobal()) {            
            // NOTE: none of the declaration forms make this 
            // recursive call. 
            // Therefore, this case MUST be
            // a definition for which we have already seen a 
            // declaration.

            assert(!ast->isDecl);

            bindFlags = BF_REBIND;
            sigma = bindIdentDef(ast, gamma, bindFlags, ti_flags);
            ast->symType->defAst = sigma->tau->getType()->defAst = ast;
          }
          else {
            sigma = bindIdentDef(ast, gamma, bindFlags, ti_flags);
          }
          break;
        }
        
      case USE_MODE:
        {
          assert(tcc);

          shared_ptr<TypeScheme> sigma = gamma->getBinding(ast->s);

          if (!sigma) {

            // If this is a type variable that is used as in
            //
            //   (define a:'a 10)
            //
            // there will be no prior definition of it ('a).  So, it
            // should now be defined.  In-correct usages should be
            // taken care of by the symbol resolver.  So, it is safe
            // to add this type to Gamma now.

            if (ast->isIdentType(id_tvar)) {
              sigma = bindIdentDef(ast, gamma, 0, ti_flags);              
            }
            else {  
              errStream << ast->loc << ": "
                        << ast->s << " Unbound in Gamma" << std::endl;
              
              //errStream << "Available bindings are: "
              //          << gamma->asString()
              //          << std::endl;              
              
              ast->symType = newTvar();
              return false;
            }
          }
          
          shared_ptr<TypeScheme> tsIns =  Instantiate(ast, sigma, trail);
          shared_ptr<Type> ins = tsIns->tau->getType();
          ast->symType = ins;
          
          DEBUG(ID_INS)
            errStream << " For " << ast->s << ", "
                      << "Obtained " << ins->asString(Options::debugTvP)
                      << " From " 
                      << sigma->asString(Options::debugTvP) 
                      << std::endl;
              
          ins = ins->getBareType();

          if ((ti_flags & TI_TYP_EXP) && 
             ((ti_flags & TI_TYP_APP) == 0) && 
             (ins->typeArgs.size() > 0)) {
            errStream << ast->loc << ": "
                      << ast->s << " cannot be instantiated without " 
                      << ins->typeArgs.size() << " type arguments."
                      << std::endl;
            
            ast->symType = newTvar();
            return false;
          }
          
          if (tsIns->tcc) {
            for (TypeSet::iterator itr = tsIns->tcc->begin();
                itr != tsIns->tcc->end(); ++itr) {
              shared_ptr<Typeclass> pred = (*itr)->getType();              
              if (ti_flags & TI_TCC_SUB)
                pred->flags |= TY_CT_SUBSUMED;
              tcc->addPred(pred);
            }
          }
          break;
        }
      default:
        {
          assert(false);
          break;
        }
      }
      break;
    }

  case at_module:
    {
      for (size_t c = 0; c < ast->children.size(); c++) {
        TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                  trail, mode, ti_flags);
        // errStream << " - - - - - - - - - - - - - - - - - - - - - - - - - "
        //              << std::endl;
      }
      break;
    }

  case at_interface:
    {
      // match at_ident
      //    TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
      //              trail, mode, TI_EXPRESSION);
    
      // match agt_definition*

      for (size_t c = 1; c < ast->children.size(); c++)
        TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                  trail, mode, ti_flags);
      break;
    }

  case at_usesel:
    {
      // Impossible to get here. Symtab rewrote this as ident.
      assert(false);
    }
    break;

  case at_defunion:
    {
      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;
      
      shared_ptr<AST> category = ast->child(2);
      bool isRefType = (category->astType == at_refCat);
      
      CHKERR(errFree, InferUnion(errStream, ast, defGamma, instEnv,
                                 impTypes, rewrite, tcc,
                                 trail, mode, isRefType, 
                                 true, true, ti_flags));
      
      gamma->mergeBindingsFrom(defGamma);      
      break;
    }

  case at_defstruct:
    {
      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      shared_ptr<AST> category = ast->child(2);
      bool isRefType = (category->astType == at_refCat);

      CHKERR(errFree, InferStruct(errStream, ast, defGamma, instEnv,
                                  impTypes, rewrite, tcc,
                                  trail, mode, isRefType, 
                                  true, true, ti_flags));

      gamma->mergeBindingsFrom(defGamma);      
      break;
    }

  case at_defobject:
    {
      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      shared_ptr<AST> category = ast->child(2);
      bool isRefType = (category->astType == at_refCat);

      CHKERR(errFree, InferObject(errStream, ast, defGamma, instEnv,
                                  impTypes, rewrite, tcc,
                                  trail,  mode, isRefType, 
                                  true, true, TI_NO_FLAGS));

      gamma->mergeBindingsFrom(defGamma);      
      break;
    }

  case at_declrepr:
    {
      // reprSimp pass should have trnsformed this into a at_declunion
      assert(false);
      break;
    }

  case at_declunion:
  case at_declstruct:
    {
      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
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
      default:
        die();
      }

      shared_ptr<AST> ident = ast->child(0);
      ident->symType = Type::make(decl_ty);
      ident->symType->defAst = ident;
      ident->symType->myContainer = ident;
      shared_ptr<TypeScheme> sigma = TypeScheme::make(ident->symType, ident,
                                                      TCConstraints::make());

      // match at_tvlist
      shared_ptr<AST> tvList = ast->child(1);
      CHKERR(errFree, InferTvList(errStream, tvList, defGamma, instEnv,
                                  impTypes, rewrite, sigma->tcc, trail, DEF_MODE, 
                                  ti_flags | TI_TYP_EXP, ident->symType));
      ident->scheme = sigma;

      // Category keywork at position 2

      // Type all constraints
      TYPEINFER(ast->child(3), gamma, instEnv, impTypes, rewrite, 
                sigma->tcc, trail, mode, TI_CONSTRAINT);

      // Solve current Predicates.
      CHKERR(errFree, sigma->solvePredicates(errStream, ident->loc,
                                             instEnv, trail)); 

      if (sigma->ftvs.size() && ast->getID()->externalName.size()) {
        errStream << ast->loc << ": Polymorphic declarations may not specify "
                  << "an external identifier."
                  << std::endl;
        errFree = false;
      }
    
      shared_ptr<TypeScheme> ts = gamma->getBinding(ident->s);
      if (ts) {
        ident->symType->defAst = ts->tau->getType()->defAst;

        CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
                                     ts, sigma, ti_flags));
      }
      else {
        defGamma->addBinding(ident->s, sigma);
        //         cout << "Added decl for " << ident->s 
        //              << " with  base = " 
        //           << &(*ident->symType->defAst)
        //              << std::endl;
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
      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      shared_ptr<TCConstraints> newTcc = TCConstraints::make();
      shared_ptr<AST> ident = ast->child(0);
      shared_ptr<AST> typ = ast->child(1);
      shared_ptr<AST> constraints = ast->child(2);
      assert(ident->isDecl);

      // WAS: newBindType() which had a maybe() around.
      ident->symType = newTvar();
      shared_ptr<TypeScheme> sigma = TypeScheme::make(ident->symType, ident);
      ident->scheme = sigma;
      
      TYPEINFER(typ, defGamma, instEnv, impTypes, rewrite, newTcc,
                trail, USE_MODE, ti_flags | TI_TYP_EXP);
      
      UNIFY(trail, ident->loc, ident->symType, typ->symType); 
      
      TYPEINFER(constraints, defGamma, instEnv, impTypes, rewrite, 
                newTcc, trail, mode, TI_CONSTRAINT);
      
      if (!errFree)
        break;
      
      sigma->tcc = newTcc;
      CHKERR(errFree, sigma->generalize(errStream, ast->loc, gamma,
                                        instEnv, ident, GC_NULL, trail,
                                        gen_top)); 
      
      if (!errFree) {
        errStream << ast->loc << ": Invalid Proclaimation"
                  << " The type specified could not be"
                  << " properly generalized."
                  << std::endl;
      }
      
      if (sigma->ftvs.size() && ast->getID()->externalName.size()) {
        errStream << ast->loc << ": Polymorphic declarations may not specify "
                  << "an external identifier."
                  << std::endl;
        errFree = false;
      }

      shared_ptr<TypeScheme> ts = gamma->getBinding(ident->s);
      if (ts) {
        ident->symType->defAst = ts->tau->getType()->defAst;
        CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
                                     ts, sigma, ti_flags));      
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
      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      CHKERR(errFree, InferTypeClass(errStream, ast, defGamma, instEnv,
                                     impTypes, rewrite, tcc, 
                                     trail, DEF_MODE, ti_flags));
      
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
      shared_ptr<AST> tcIdent = ast->child(0);
      TYPEINFER(tcIdent, gamma, instEnv, impTypes, rewrite, tcc,
                trail, USE_MODE, 
                ti_flags | TI_TYP_EXP | TI_TYP_APP);
      shared_ptr<Typeclass> tc = tcIdent->symType->getType();      

      if (tc->kind != ty_typeclass) {
        // This is the result of some other error
        errFree = false;
        break;
      }

      if (tc->typeArgs.size() == (ast->children.size() - 1)) {
        for (size_t i = 1; i < ast->children.size(); i++) {
          TYPEINFER(ast->child(i), gamma, instEnv, impTypes, rewrite, tcc,
                    trail, USE_MODE, TI_NON_APP_TYPE);
          UNIFY(trail, ast->child(i)->loc,
                ast->child(i)->symType, tc->TypeArg(i-1));
        }
      }
      else {
        errStream << ast->loc << ": "
                  << "Typeclass cannot be Partially "
                  << "or over instantiated. "
                  << "Typeclass " << tc->asString()
                  << " expects " << tc->typeArgs.size() 
                  << " args, but is here applied to "
                  << (ast->children.size() - 1) << "."
                  << std::endl;
      }

      /* Special Handling for the copy-compatibility constraint 
         The copy-compat type-class constraint is transformed into a
         unification constraint using maybe-types, and the constraint
         is immediately eliminated as solved. */ 
      const std::string& copy_compat =
        SpecialNames::spNames.sp_copy_compat;
      const std::string& copy_from_to =
        SpecialNames::spNames.sp_copy_from_to;

      if (tc->defAst->s == copy_compat) {
        shared_ptr<Type> tv = newTvar();
        UNIFY(trail, ast->child(1)->loc, 
              ast->child(1)->symType, MBF(tv));
        UNIFY(trail, ast->child(2)->loc,
              ast->child(2)->symType, MBF(tv));
        tcc->clearPred(tc);
      }
      else if (tc->defAst->s == copy_from_to) {
        shared_ptr<Type> tv = newTvar();
        UNIFY(trail, ast->child(1)->loc, 
              ast->child(1)->symType, MBF(tv));
        UNIFY(trail, ast->child(2)->loc,
              ast->child(2)->symType, MBF(tv));
        tcc->clearPred(tc);
      }
      
      ast->symType = tc;
      break;
    }

  case at_definstance:
    {
      CHKERR(errFree, InferInstance(errStream, ast, gamma, instEnv,
                                    impTypes, rewrite, tcc, trail, 
                                    DEF_MODE, ti_flags));                                    
      break;
    }

  case at_defexception:
    {
      shared_ptr<AST> ctr = ast->child(0);

      // Maybe, we have a prior declaration?
      shared_ptr<TypeScheme> declTS = gamma->getBinding(ctr->s);

      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      shared_ptr<TCConstraints> myTcc = TCConstraints::make();

      TYPEINFER(ctr, defGamma, instEnv, impTypes, rewrite, myTcc, 
                trail, DEF_MODE, ti_flags | TI_TYP_EXP);      
      
      shared_ptr<Type> exn = Type::make(ty_exn);
      exn->defAst = ctr;      
      ctr->symType->getType()->link = exn;
      shared_ptr<TypeScheme> sigma = ctr->scheme;
      sigma->tcc = myTcc;
      
      shared_ptr<Type> t = ctr->symType->getType();
      for (size_t c = 1; c < ast->children.size(); c++) {
        shared_ptr<AST> field = ast->child(c);        
        if (field->astType == at_fill)
          continue;

        TYPEINFER(field, defGamma, instEnv, impTypes, 
                  rewrite, sigma->tcc,
                  trail, USE_MODE, ti_flags | TI_TYP_EXP | TI_TYP_DEFN);
        shared_ptr<Type> t1 = field->child(1)->getType();
        t->components.push_back(comp::make(field->child(0)->s, t1));
      }

      // Build the structure type for the component structure.
      shared_ptr<Type> sType = Type::make(ty_structr);
      sType->defAst = ctr;
      for (size_t i=0; i < t->components.size(); i++)
        sType->components.push_back(comp::make(t->CompName(i),
                                          t->CompType(i)));
      
      ctr->stCtr = ctr;
      ctr->stSigma = TypeScheme::make(sType, ctr, sigma->tcc);

      // Solve current Predicates.
      CHKERR(errFree, sigma->solvePredicates(errStream, ast->loc,
                                             instEnv, trail)); 

      ast->symType = ctr->symType;

      gamma->mergeBindingsFrom(defGamma);

      if (declTS)
        CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
                                     declTS, sigma, ti_flags));        
      break;
    }

  case at_recdef:
  case at_define:
    {
      /*------------------------------------------------
                t' = 'a|'b     [U(t = t')]
                         A |- e:t1    U(t1 = 'c|'b)        
          S = generalize(A, t', e)   EXTEND A with x:S
        _______________________________________________
                    A |- (define x:[t] = e): t'


                  t' = 'a|'b   [U(t = t')]
                A, x:t' |- e:t1    U(t1 = 'c|'b)        
          S = generalize(A, t', e)   EXTEND A with x:S
        _______________________________________________
                    A |- (recdef x:[t] = e): t'
     ---------------------------------------------------*/
      // Maybe, we have a prior declaration?
      shared_ptr<AST> ident = ast->getID();
      shared_ptr<TypeScheme> declTS = gamma->getBinding(ident->s);

      shared_ptr<TSEnvironment > defGamma = gamma->newDefScope();
      ast->envs.gamma = defGamma;

      shared_ptr<TCConstraints> currTcc = TCConstraints::make();


      if(ast->child(0)->child(0)->s ==
         "_34../../tests/unit/FM.bitc#0:fm-poly#FN1SR0_2S2_5int32") {
        errStream << " Came Here " << std::endl;
        
      }
      
      if (ast->astType == at_recdef) {
        // match agt_bindingPattern
        // match agt_expr
        TYPEINFER(ast->child(0), defGamma, instEnv, impTypes, rewrite, 
                  currTcc, trail, DEF_MODE, ti_flags);
      }

      TYPEINFER(ast->child(1), defGamma, instEnv, impTypes, rewrite, 
                currTcc, trail, USE_MODE, ti_flags);
      
      if (ast->astType == at_define) {
        // match agt_bindingPattern
        // match agt_expr
        TYPEINFER(ast->child(0), defGamma, instEnv, impTypes, rewrite, 
                  currTcc, trail, DEF_MODE, ti_flags);
      }

      TYPEINFER(ast->child(2), defGamma, instEnv, impTypes, rewrite, 
                currTcc, trail, mode, TI_CONSTRAINT);

      shared_ptr<Type> idType = ident->symType;
      shared_ptr<Type> rhsType = ast->child(1)->symType;
      shared_ptr<TypeScheme> sigma = ident->scheme;
      sigma->tcc = currTcc;

      DEBUG(DEF_INF)
        errStream << "At define " << ident->asString() << ":"
                  << " LHS = " << idType->asString()
                  << " RHS = " << rhsType->asString()
                  << std::endl;
      
      UNIFY(trail, ast->child(1)->loc, 
            ast->child(1)->symType, MBF(ast->child(0)->symType));
      
      DEBUG(DEF_INF)
        errStream << "After Unification: " 
                  << ast->getID()->symType->asString()
                  << " LHS = " << idType->asString()
                  << " RHS = " << rhsType->asString()
                  << std::endl;            

      // Check the consistency of types wrt mutability at ALL asts
      // in this definition.
      if(errFree)
        CHKERR(errFree, CheckMutConsistency(errStream, ast));

      CHKERR(errFree, sigma->generalize(errStream, ast->loc, gamma,
                                        instEnv,  ast->child(1), GC_NULL, 
                                        trail, gen_top));
      DEBUG(DEF_INF)
        errStream << "After Generalization: " 
                  << ast->getID()->scheme->asString()
                  << std::endl << std::endl;
      
      gamma->mergeBindingsFrom(defGamma);
      
      if (declTS) 
        CHKERR(errFree, matchDefDecl(errStream, trail, gamma, instEnv,
                                     declTS, ident->scheme, ti_flags));

      ast->symType = ast->child(0)->symType;
      break;
    }
    
  case at_importAs:
    {
      shared_ptr<AST> ifAst = ast->child(0);
      shared_ptr<AST> idAst = ast->child(1);

      shared_ptr<TSEnvironment > tmpGamma = gamma->newScope();
      ast->envs.gamma = gamma;
      
      assert(idAst->envs.gamma);
      assert(idAst->envs.instEnv);
      
      useIFGamma(idAst->s, idAst->envs.gamma,
                 tmpGamma);
      useIFInsts(idAst->s, idAst->envs.instEnv, 
                 instEnv);
      
      gamma->mergeBindingsFrom(tmpGamma);
      break;
    }

  case at_provide:
    {
      // In the new at_provide scheme, at_provide does not imply any
      // import, so we probably should not be attempting any type
      // inference here anymore.
      break;
    }

  case at_import:
    {
      shared_ptr<TSEnvironment > tmpGamma = gamma->newScope();
      ast->envs.gamma = gamma;

      shared_ptr<AST> ifName = ast->child(0);
      
      assert(ifName->envs.gamma);
      assert(ifName->envs.instEnv);
      
      if (ast->children.size() == 1) {
        // This is an import-all form
        useIFGamma(std::string(), ifName->envs.gamma, tmpGamma);
        useIFInsts(std::string(), ifName->envs.instEnv, instEnv);
      }
      else {
        for (size_t c = 1; c < ast->children.size(); c++) {
          shared_ptr<AST> alias = ast->child(c);
          shared_ptr<AST> thisName = alias->child(0);
          shared_ptr<AST> thatName = alias->child(1);
        
          shared_ptr<TypeScheme> sigma = ifName->envs.gamma->getBinding(thatName->s);
        
          if (!sigma) {
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
      ast->tagType = GC_NULL;
      
      // match at_declare*
      for (size_t c = 0; c < ast->children.size(); c++) {
         TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                  trail, mode, ti_flags);
        
        if (ast->child(c)->tagType) {
          if (!ast->tagType) {
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
      ast->symType = Type::make(ty_tvar);
      break;
    }

  case at_declare:
    {
      // match at_ident
      // The first identifier has special meaning, and must be 
      // dealt with by hand.
      shared_ptr<AST> ident = ast->child(0);      
      shared_ptr<AST> typ = ast->child(1);

      // match agt_type?
      if (ast->children.size() > 1) {
        TYPEINFER(typ, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, ti_flags);

      if (!typ->symType)
        typ->symType = Type::make(ty_tvar);
      }

      // These names are never mangled.
      if (ident->s == "tag-type") {
        // compatible type
        
        shared_ptr<Type> realType = ast->child(1)->symType->getType();
        shared_ptr<Type> t = realType->getBareType();        
        if (t->kind == ty_mutable) {
          errStream << ast->child(1)->loc << ": "
                    << "Tag type cannot be mutable"
                    << std::endl;
          errFree = false;
        }
        else if (!t->isInteger()) {
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

      ast->symType = Type::make(ty_tvar);
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

  case at_methdecl: 
    // Handling for at_methdecl is basically the same as for at_field.
    // However, note the a post-pass over structures  is made in
    // InferStruct to synthesize declarations for the functions
    // corresponding to each method.
  case at_field: 
    {
      // match at_ident
      shared_ptr<AST> fName = ast->child(0);
      fName->symType = Type::make(ty_tvar);
      
      // match agt_type
      shared_ptr<AST> fType = ast->child(1);
      TYPEINFER(fType, gamma, instEnv, impTypes, rewrite, 
                tcc, trail,  USE_MODE, 
                ti_flags | TI_TYP_EXP | TI_TYP_DEFN);
      
      ast->symType = fType->symType;
      ast->field_bits = fType->field_bits;
      ast->child(0)->field_bits = fType->field_bits;
      break;
    }

  case at_fill:
    {      
      // match agt_type 
      shared_ptr<AST> fillType = ast->child(0);
      TYPEINFER(fillType, gamma, instEnv, impTypes, rewrite, 
                tcc, trail,  USE_MODE, 
                ti_flags | TI_TYP_EXP | TI_TYP_DEFN);     
      ast->field_bits = fillType->field_bits;
  
      if(ast->children.size() == 2) {
        shared_ptr<AST> fillVal = ast->child(0);
        TYPEINFER(fillVal, gamma, instEnv, impTypes, rewrite, 
                  tcc, trail,  USE_MODE, 
                  TI_TYP_EXP | TI_TYP_DEFN);     
      
        uint64_t val = fillVal->litValue.i.as_uint64();
        uint64_t maxVal = (((uint64_t)1) << fillType->nBits()) - 1;
      
        if (val > maxVal) {
          errStream << ast->loc << ": "
                    << "Not enough bits to store the reserved value"
                    << std::endl;
          errFree = false;
        }
      }

      ast->symType = ast->child(0)->symType;
      break;
    }

  case at_bitfield:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_NON_APP_TYPE);
      
      shared_ptr<AST> len = ast->child(1);
      len->symType = Type::make(ty_word);
      
      ast->symType = ast->child(0)->symType;
      ast->field_bits = len->litValue.i.as_uint32();

      if (!errFree)
        break;

      if (ast->field_bits > ast->symType->nBits()) {
        errStream << ast->loc << ": Invalid bitfield specification"
                  << "No. of bits requested = " << ast->field_bits
                  << ", Max available for type = "
                  << ast->symType->nBits()
                  << std::endl;
        errFree = false;
      }
#ifdef KEEP_BF
      ast->symType = Type::make(ty_bitfield);
      ast->symType->components.push_back(comp::make(ast->child(0)->symType));
      // match at_intLiteral
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_EXPRESSION);
      
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
    // arguments of a function is enforced in the parser.
  case at_arrayRefType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_NON_APP_TYPE);
    
      shared_ptr<Type> t = ast->child(0)->getType();
    
      ast->symType = Type::make(ty_array_ref);
      ast->symType->components.push_back(comp::make(t));
    
      break;
    }
  case at_byRefType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_NON_APP_TYPE);
    
      shared_ptr<Type> t = ast->child(0)->getType();
    
      ast->symType = Type::make(ty_byref);
      ast->symType->components.push_back(comp::make(t));
    
      break;
    }

  case at_refType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_NON_APP_TYPE);
    
      shared_ptr<Type> t = ast->child(0)->getType();
    
      ast->symType = Type::make(ty_ref);
      ast->symType->components.push_back(comp::make(t));
    
      break;
    }

  case at_exceptionType:
    {
      ast->symType = Type::make(ty_exn);
      break;
    }

  case at_dummyType:
    {
      ast->symType = Type::make(ty_dummy);
      break;
    }

  case at_valType:
    {
      ast->symType = Type::make(ty_tvar);
      
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_NON_APP_TYPE);
      
      shared_ptr<Type> t1 = ast->child(0)->symType->getType();
      shared_ptr<Type> t = ast->child(0)->symType->getBareType();
      
      switch(t->kind) {
      case ty_tvar:
        {
          shared_ptr<Type> tvar = Type::make(ty_tvar);
          t->kind = ty_ref;          
          t->components.push_back(comp::make(tvar));
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
          if (t->components.size() == 0) {
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

  case at_methType:
  case at_fn:
    {
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_NON_APP_TYPE);
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_NON_APP_TYPE);
      
      ast->symType = Type::make((ast->astType == at_fn) ? ty_fn : ty_method);
      shared_ptr<Type> fnarg = ast->child(0)->symType->getType();
      shared_ptr<Type> ret = ast->child(1)->symType->getType();
      ast->symType->components.push_back(comp::make(fnarg));
      ast->symType->components.push_back(comp::make(ret));    
      break;
    }

  case at_fnargVec:
    {      
      shared_ptr<Type> fnarg = Type::make(ty_fnarg);
      for (size_t c = 0; c < ast->children.size(); c++) {
        shared_ptr<AST> arg = ast->child(c);
        TYPEINFER(arg, gamma, instEnv, impTypes, rewrite, tcc,
                  trail, mode, TI_NON_APP_TYPE);
        shared_ptr<Type> argType = arg->symType->getType();

        shared_ptr<comp> nComp = comp::make(argType);        
        if (argType->isByrefType()) {
          nComp = comp::make(argType->Base());
          nComp->flags |= COMP_BYREF;
        }

        fnarg->components.push_back(nComp);
      }
      ast->symType = fnarg;
      break;
    }

  case at_primaryType:
    {
      ast->symType = Type::make(Type::LookupKind(ast->s));
      break;
    }

  case at_fieldType:
    {
      shared_ptr<AST> fName = ast->child(0);
      shared_ptr<Type> ft = Type::make(ty_field);
      ft->litValue.s = fName->s;
      ast->symType = fName->symType = ft;
      break;
    }

  case at_arrayType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_NON_APP_TYPE);
    
      shared_ptr<Type> arrType = Type::make(ty_array);
      ast->symType = arrType;
      arrType->components.push_back(comp::make(ast->child(0)->symType));

      // match at_intLiteral
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_NON_APP_TYPE);
 
      // FIX TO WORD
      CHKERR(errFree, unifyPrim(errStream, trail, ast->child(1)->loc, 
                                ast->child(1)->symType, "word")); 

      arrType->arrLen->len = ast->child(1)->litValue.i.as_uint32();
      break;
    }

  case at_vectorType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_NON_APP_TYPE);
    
      ast->symType = Type::make(ty_vector);
      ast->symType->components.push_back(comp::make(ast->child(0)->symType));

      break;
    }
     
  case at_mutableType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_NON_APP_TYPE);
    
      shared_ptr<Type> t = ast->child(0)->symType->getType();
      
      if (t->kind == ty_mutable) {
        //The Type is already mutable
        ast->symType = t;
      }
      if (t->isMaybe()) {
        assert(false);
      }
      else {
        ast->symType = Type::make(ty_mutable);
        ast->symType->components.push_back(comp::make(t));
      }
      break;
    }

  case at_constType:
    {
      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_NON_APP_TYPE);
      
      shared_ptr<Type> t = ast->child(0)->symType->getType();

      // If the inner type is already const, we are done.
      if (t->kind == ty_const) {
        ast->symType = t;
        break;
      }
      
      // To ensure completeness of inference, bare type variables cannot
      // occur within a const meta-constructor. They must only occur
      // within mbFull types. 
      //
      // For example:
      //
      // \x:ref(const('a)). \y.ref(const('b)). if true then x else y.
      // 
      // here, 'a gets unified with 'b, which leads to incompleteness,
      // and the expression 
      //
      // \x:ref(const('a)). \y.ref(const('b)). 
      //   (if true then x else y, (p:'a:bool, q:'b:(mutable bool)))
      // will fail to type check.
      //
      // We need to ensure that bare type variables do not occur at
      // any shallow position within a const type. For example,
      //
      // (const (pair 'a 'b)) must be turned into
      //
      // (const (pair 'a1|'a 'b1|'b) and NOT
      //
      // (const 'a|(pair 'a 'b))
      //
      // since unification under mutability minimization will fix 'a
      // and 'b without scope of mutability variance.
      //
      // The theoritical development does not have this probllem,
      // since pair types are correctly generated with appropriate
      // mbFull types. Here, since the type within the annotation is
      // programmer controlled, we must take extra care.

      

      const bool markOnly = ti_flags & TI_TYP_DEFN;
      t->ensureMinimizability(trail, markOnly);

      ast->symType = Type::make(ty_const);
      ast->symType->components.push_back(comp::make(t));
      break;
    }

  case at_typeapp:
    {
      // match agt_var 
      // match agt_tvar+
      ast->symType = Type::make(ty_tvar);
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE,                 
                ti_flags | TI_TYP_EXP | TI_TYP_APP);
    
      // Constructor cannot return a mutable type by default
      shared_ptr<Type> t = ast->child(0)->getType(); 
      shared_ptr<Type> realType = t;
      t = t->getBareType(); 
      
      if (t->kind != ty_structv && t->kind != ty_structr &&
         t->kind != ty_unionv && t->kind != ty_unionr) {
        
        if (t->kind == ty_uconv || t->kind == ty_uconr || 
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
      
      shared_ptr<Type> sut = t;
      
      if ((ast->children.size()-1) != sut->typeArgs.size()) {
        errStream << ast->child(0)->loc << ": "
                  << ast->child(0)->s << " - Type cannot be" 
                  << " partially/over instantiated" 
                  << " For type " << sut->asString()
                  << ", " << sut->typeArgs.size()
                  << " arguments are needed. But "
                  << ast->children.size() -1
                  << " were provided."
                  << std::endl;
        errFree = false;
      }
      else { 
        for (size_t i=0; i < sut->typeArgs.size(); i++) {
          TYPEINFER(ast->child(i+1), gamma, instEnv, impTypes, rewrite, tcc,
                    trail,  USE_MODE, TI_NON_APP_TYPE);
          
          UNIFY(trail, ast->child(i+1)->loc, 
                ast->child(i+1)->symType, sut->TypeArg(i));
        }
      }
      
      break;
    }

  case at_qualType:
    {
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_CONSTRAINT);
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_NON_APP_TYPE);
      ast->symType = ast->child(1)->symType;
      break;
    }

  case at_constraints:
    {
      for (size_t c=0; c < ast->children.size(); c++)      
        TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                  trail, mode, TI_CONSTRAINT);
      ast->symType = Type::make(ty_tvar);
      break;
    }    

  case at_identPattern:
    {
      // match agt_var
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_EXPRESSION);
      
      
      // Type Qualifications ONLY in Binding Patterns
      // match agt_type?
      if (ast->children.size() > 1) {
        TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_NON_APP_TYPE);
      
        if (ast->child(1)->symType->isByrefType()) {
          UNIFY(trail, ast->child(0)->loc, 
                ast->child(0)->symType, ast->child(1)->getType()->Base());
        }
        else {
          UNIFY(trail, ast->child(0)->loc, 
                ast->child(0)->symType, ast->child(1)->symType);
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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
    
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_NON_APP_TYPE);

      CHKERR(errFree, CheckMutConsistency(errStream,
                                          ast->child(1)->loc,
                                          ast->child(1)->symType));
      
      // tqExpr: U(t1 == t2)
      UNIFY(trail, ast->child(1)->loc, 
            ast->child(0)->symType, ast->child(1)->symType);

      ast->symType = ast->child(0)->symType;
      break;
    }
    
  case at_suspend:
    {
      // match agt_eform
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      ast->symType = ast->child(1)->symType;
      break;
    }

  case at_unit:
    {
      /*------------------------------------------------
          ___________________________________________
              A |- unit: ()
        ------------------------------------------------*/
      ast->symType = Type::make(ty_unit);
      break;
    }

  case at_letGather:
    {
      /*------------------------------------------------
                 A |- e1:t1  ...  A |- en:tn 
          ___________________________________________
              A |- (let-gather e1 ... en): (t1, ..., tn)
        ------------------------------------------------*/

      ast->symType = Type::make(ty_letGather);
      shared_ptr<Type> gatherType = ast->symType->getBareType();

      for (size_t c=0; c < ast->children.size(); c++) {
        TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                  trail, mode, TI_EXPRESSION);
      
        gatherType->components.push_back(comp::make(ast->child(c)->symType));
      }
      break;
    }


  case at_mkArrayByref:
    {
      /*------------------------------------------------
                 A |- e:t  U(t = 'w|(array 'a ?))
          _________________________________________________
            A |- (make-array-byref e): (array-byref 'a)
        ------------------------------------------------*/
      // match agt_expr
      shared_ptr<AST> arg = ast->child(0);

      // Usually, the type of the child is always inferred because 
      // at_maArrayByref is constructed from at_apply case after
      // observing the type of the argument
      if(!arg->symType)
        TYPEINFER(arg, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> var = Type::make(ty_tvar);
      shared_ptr<Type> arr = Type::make(ty_array, var);
      
      UNIFY(trail, arg->loc, arg->symType, MBF(arr));
      if(errFree)
        ast->symType = Type::make(ty_array_ref, arr->Base());
      else
        ast->symType = Type::make(ty_tvar);
      
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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      // FIX TO WORD
      UNIFY(trail, ast->child(0)->loc, 
            ast->child(0)->symType, MBF(Type::make(ty_word)));

      // match agt_expr
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      // Build a type that I expect the second argument to be, and
      // unify with it.
      // Thee type of the function that builds the vector
      // is built as:
      // (fn ('a|word) 'b|'c)
      shared_ptr<Type> wordType = MBF(Type::make(ty_word));
      shared_ptr<Type> arg = Type::make(ty_fnarg, wordType);
      shared_ptr<Type> ret = MBF(newTvar());
      shared_ptr<Type> fnType = MBF(Type::make(ty_fn, arg, ret));
      
      UNIFY(trail, ast->child(1)->loc, 
            ast->child(1)->symType, fnType);
      
      CHKERR(errFree, testNonEscaping(errStream, ast, ret));

      ast->symType = Type::make(ty_vector, MBF(ret));
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
      shared_ptr<Type> compType = MBF(newTvar());
      ast->symType = Type::make(k, compType);
      if(k == ty_array)
        ast->symType->arrLen->len = ast->children.size();
      
      // match agt_expr+
      for (size_t c = 0; c < ast->children.size(); c++) {
        shared_ptr<AST> expr = ast->child(c);
        TYPEINFER(expr, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        
        CHKERR(errFree, testNonEscaping(errStream, expr,
                                        expr->symType)); 
        
        UNIFY(trail, expr->loc, expr->symType, MBF(compType));
      }
      
      break;
    }

  case at_array_length:
  case at_array_ref_length:
  case at_vector_length:
    {
    /*------------------------------------------------
             A |- e: t   U(t = 'a|array('c|'b, ?len))
          _________________________________________
             A |- (array-length e): word


             A |- e: t   U(t = 'a|array-ref('c|'b))
          _________________________________________
             A |- (array-ref-length e): word

             A |- e: t   U(t = 'a|vector('c|'b))
          _________________________________________
             A |- (vector-length e): word
       ------------------------------------------------*/
      Kind k = ((ast->astType == at_array_length) ? ty_array :
                ((ast->astType == at_array_ref_length) ?
                 ty_array_ref : ty_vector));
      
      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> av = MBF(Type::make(k, MBF(newTvar())));
      if (ast->astType == at_array_length)
        impTypes[av] = ast->child(0);

      UNIFY(trail, ast->child(0)->loc, ast->child(0)->symType, av);
 
      // FIX TO WORD, not mutable
      ast->symType = Type::make(ty_word);
      break;    
    }

  case at_array_nth:
  case at_array_ref_nth:
  case at_vector_nth:
    {
    /*------------------------------------------------
             A |- e: t   U(t = 'a!array('b|'c, ?len))
             A |- en: tn  U(tn = 'd|word)
          _________________________________________
             A |- (array-nth e en): 'b|'c

             A |- e: t   U(t = 'a!array-ref('b|'c))
             A |- en: tn  U(tn = 'd|word)
          _________________________________________
             A |- (array-ref-nth e en): 'b|'c


             A |- e: t   U(t = 'a|vector('b|'c))
             A |- en: tn  U(tn = 'd|word)
          _________________________________________
             A |- (vector-nth e en): 'b|'c
       ------------------------------------------------*/
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      shared_ptr<Type> av = GC_NULL;
      shared_ptr<Type> cmp = MBF(newTvar());
      Kind k = ((ast->astType == at_array_nth) ? ty_array :
                ((ast->astType == at_array_ref_nth) ?
                 ty_array_ref : ty_vector));
      
      av = MBT(Type::make(k, cmp));
      if (ast->astType == at_array_nth)
        impTypes[av] = ast->child(0);
      
      UNIFY(trail, ast->child(0)->loc, ast->child(0)->symType, av);
      
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      // FIX TO WORD
      UNIFY(trail, ast->child(1)->loc, 
            ast->child(1)->symType, MBF(Type::make(ty_word)));

      ast->symType = cmp;
      break;
    }

  case at_block:
    {
    /*------------------------------------------------
              A |- x: 'a|'b   A |- e: t
                    U(t = 'c|'b)
          _________________________________________
                 A |- (block x e): 'd|'b
       ------------------------------------------------*/

      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  DEF_MODE, ti_flags);
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      UNIFY(trail, ast->child(0)->loc,
            ast->child(0)->symType, MBF(ast->child(1)->symType));  
      
      ast->symType = MBF(ast->child(1)->symType);
      break;
    }

  case at_return_from:
    {
    /*------------------------------------------------
                A(x) = tx   A |- e: t
              U(tx = 'a|'b)   U('c|'b)
          _________________________________________
                 A |- (return-from x e): 'd
       ------------------------------------------------*/

      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      UNIFY(trail, ast->child(0)->loc,
            ast->child(0)->symType, MBF(ast->child(1)->symType));  
      
      // Not returning, so don't really care about return type, but it
      // needs to unify compatibly in various other situations, so
      // generate a new tyep variable.
      ast->symType = newTvar();    
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
      for (size_t c = 0; c < ast->children.size(); c++)
        TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
      
      
      ast->symType = ast->child(ast->children.size()-1)->symType;
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

                          A |- e:'a    
       ___________________________________________________
          A |- e.fld: 'b \ has-field('a, field(fld), 'b)

       ------------------------------------------------*/

      // match agt_expr 
      /* Selection is only permitted on 
         - structures: for selecting field or method
         - union values: determining tag (need to convert it to at_sel_ctr)
         Note that selection for fqn-naming a union constructor
         is already handled by the symbol resolver pass  */
      ast->symType = Type::make(ty_tvar); 
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
            
      shared_ptr<Type> t = ast->child(0)->symType->getType();
      shared_ptr<Type> t1 = t->getBareType();
      
      if (t1->isUType()) {
        ast->astType = at_sel_ctr;
        TYPEINFER(ast, gamma, instEnv, impTypes, rewrite, tcc,
                  trail, USE_MODE, TI_EXPRESSION);
        break;
      }

      if(t1->isTvar() && !Options::noPrelude) {

        if (ti_flags & TI_NO_MORE_TC) {
          ast->symType = Type::make(ty_tvar);
          break;
        }
        
        const std::string& hasFld = SpecialNames::spNames.sp_has_field;
        shared_ptr<TypeScheme> hfSigma = gamma->getBinding(hasFld);
        assert(hfSigma);
        
        shared_ptr<Typeclass> hf = hfSigma->type_instance();
        assert(hf->typeArgs.size() == 3);
        
        shared_ptr<Type> fldName = Type::make(ty_field);
        fldName->litValue.s = ast->child(1)->s;

        // has-field constraint is of the form:
        // has-field('structure, 'field-name, 'field-type)
        UNIFY(trail, ast->loc, hf->TypeArg(0), t1);
        UNIFY(trail, ast->loc, hf->TypeArg(1), fldName);
        UNIFY(trail, ast->loc, hf->TypeArg(2), ast->symType);

        tcc->addPred(hf);
        break;
      }
      
      if (t1->kind != ty_structv && t1->kind != ty_structr) {
        errStream << ast->child(0)->loc << ": "
                  << ast->child(0)->s << " cannot be resolved" 
                  << " to a structure, union, or exception type." 
                  << " but obtained " << t1->asString() 
                  << std::endl;
        errFree = false;
        break;
      }
      
      shared_ptr<TypeScheme> stScheme;
      if (t1->defAst->symType->isULeg() ||
         t1->defAst->symType->isException()) 
        stScheme = t1->defAst->stSigma;
      else
        stScheme = t1->defAst->scheme;
      
      shared_ptr<Type> tr = stScheme->type_instance();

      if (tr->isValType())
        for (size_t i=0; i < tr->typeArgs.size(); i++) {
          shared_ptr<Type> arg = tr->TypeArg(i)->getType();
          if (tr->argCCOK(i))
            trail->subst(arg, MBF(newTvar()));
        }
      
      shared_ptr<Type> trt = MBT(tr);

      UNIFY(trail, ast->child(0)->loc, t, trt);
      
      shared_ptr<Type> fld;
      CHKERR(errFree, findComponent(errStream, tr, ast, fld, ti_flags & TI_METHOD_OK));
      if (errFree)
        ast->symType = fld;
      
      break;
    }

  case at_fqCtr:
    {
      /*------------------------------------------------
           A(v) = ['a1.. 'am] ... | Ctr{...} | ...
          _________________________________________
                  A |- v.Ctr:t
       ------------------------------------------------*/


      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> t1 = ast->child(0)->symType->getBareType();
      if (!t1->isUType()) {
        errStream << ast->child(0)->loc << ": "
                  << ast->child(0)->s << " cannot be resolved" 
                  << " to a union, or exception type." 
                  << " but obtained " << t1->asString() 
                  << std::endl;
        errFree = false;
        break;
      }
      
      shared_ptr<Type> fct;
      CHKERR(errFree, findComponent(errStream, t1, ast, fct));
      
      if (!errFree) {
        ast->symType = Type::make(ty_tvar); 
        break;
      }
      
      ast->child(1)->symbolDef = fct->defAst;          
      ast->child(1)->flags |= fct->defAst->flags;
      ast->child(1)->flags |= fct->defAst->flags;
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

      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      shared_ptr<Type> t1 = ast->child(0)->symType->getBareType();
      if (!t1->isUType()) {
        errStream << ast->child(0)->loc << ": "
                  << ast->child(0)->s << " cannot be resolved" 
                  << " to a union, or exception type." 
                  << " but obtained " << t1->asString() 
                  << std::endl;
        errFree = false;
        break;
      }

      ast->symType = Type::make(ty_bool);
      
      shared_ptr<Type> fct;
      CHKERR(errFree, findComponent(errStream, t1, ast, fct));
      if (!errFree)
        break;
      
      ast->child(1)->symbolDef = fct->defAst;          
      ast->child(1)->flags |= fct->defAst->flags;
      ast->child(1)->flags |= fct->defAst->flags;
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
      shared_ptr<TSEnvironment > lamGamma = gamma->newScope();
      ast->envs.gamma = lamGamma;
      
      shared_ptr<AST> argVec = ast->child(0);
      shared_ptr<Type> fnarg = Type::make(ty_fnarg);
      argVec->symType = fnarg;      
      
      for (size_t c = 0; c < argVec->children.size(); c++) {
        TYPEINFER(argVec->child(c), lamGamma, instEnv, impTypes, 
                  rewrite, tcc, trail,  DEF_MODE, TI_EXPRESSION);

        shared_ptr<Type> argType = argVec->child(c)->getType();
        shared_ptr<comp> nComp = GC_NULL;
        if (argType->isByrefType()) {
          nComp = comp::make(argType->Base());
          nComp->flags |= COMP_BYREF;
        }
        else {
          nComp = comp::make(MBF(argType));
        }
        
        fnarg->components.push_back(nComp);
      }

      TYPEINFER(ast->child(1), lamGamma, instEnv, impTypes, 
                rewrite, tcc, trail,  USE_MODE, TI_EXPRESSION);
      UNIFY(trail, ast->child(1)->loc, 
            ast->child(1)->symType, MBF(newTvar()));
      
      shared_ptr<Type> retType = MBF(ast->child(1)->getType());
      ast->symType = Type::make(ty_fn, fnarg, retType);      
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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, USE_MODE, TI_NON_APP_TYPE);
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
      shared_ptr<AST> lhs = ast->child(0);
      shared_ptr<AST> rhs = ast->child(1);

      TYPEINFER(lhs, gamma, instEnv, impTypes, rewrite, tcc,
                trail, USE_MODE, TI_EXPRESSION);
      TYPEINFER(rhs, gamma, instEnv, impTypes, rewrite, tcc,
                trail, USE_MODE, TI_EXPRESSION);
      
      UNIFY(trail, lhs->loc, lhs->symType, rhs->symType);
      
      ast->symType =  Type::make(ty_unit);
      break;      
    }

  case at_mkClosure:
    {
       /*------------------------------------------------
             mkclosure: TODO
       ------------------------------------------------*/

      shared_ptr<AST> clEnv = ast->child(0);
      // Type check the closure structure apply
      TYPEINFER(clEnv, gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<AST> thisLambda = ast->child(1);
      // Type check the lambda forms
      TYPEINFER(thisLambda, gamma, instEnv, impTypes, rewrite, tcc,
                trail, USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> fullClFnType = thisLambda->symType->getType();
      shared_ptr<Type> clFnType = fullClFnType->getBareType();
      assert(clFnType->isFnxn());
      shared_ptr<Type> args = clFnType->Args()->getType();
      assert(args->components.size() >= 1);
      shared_ptr<Type> clArg = args->CompType(0);
      // Make sure we have the right env on all the functions.
      UNIFY(trail, clEnv->loc, clArg, clEnv->symType);
      
      // Build the closure Type.      
      shared_ptr<Type> fullMkClType = fullClFnType->getDCopy();
      shared_ptr<Type> mkClType= fullMkClType->getBareType();
      shared_ptr<Type> mkClArg = mkClType->Args()->getType(); 
      assert(mkClArg->kind == ty_fnarg);
      
      mkClArg->components.erase(mkClArg->components.begin());

      ast->symType = fullMkClType;
      break;
    }

  case at_setClosure:
    {
       /*------------------------------------------------
             setclosure: TODO
       ------------------------------------------------*/
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, 
                    rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);
      
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, 
                    rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);
      
      for (size_t c = 2; c < ast->children.size(); c++) {
            TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                      trail, USE_MODE, TI_EXPRESSION);
      }
    
      ast->symType = Type::make(ty_unit);
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

      TI_Flags appFlags = TI_EXPRESSION;

      // A method selector is ONLY permitted in applicative position,
      // and that will necessarily involve a select AST in that
      // position. We refuse to accept that in any other context.
      if (ast->child(0)->astType == at_select)
        appFlags |= TI_METHOD_OK;

      // match agt_expr agt_expr
      //NOTE: One operation safe. (+)

      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, USE_MODE, appFlags);
      shared_ptr<Type> fType = ast->child(0)->getType();

      if (fType->isStruct()) {
        ast->astType = at_struct_apply;
        TYPEINFER(ast, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        break;
      }
      if (fType->isObject()) {
        ast->astType = at_object_apply;
        TYPEINFER(ast, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        break;
      }
      else if (fType->isMethod()) {
        assert (ast->child(0)->astType == at_select);
        // Need to re-write this AST as a normal application. Given
        // "(s.M args...)", where s is an instance of type S, rewrite
        // this as (S.M s args...):

        shared_ptr<AST> theSelect = ast->child(0);
        shared_ptr<AST> theMethod = theSelect->child(1);
        shared_ptr<AST> theStructure = theSelect->child(0);

        ast->children.insert(ast->children.begin() + 1, theStructure);

        std::stringstream qs;
        if((ti_flags & TI_USING_FQNS) == 0)
          qs << fType->myContainer->s;
        else
          qs << fType->myContainer->fqn.asString();
        qs << "." << theMethod->s;
        theMethod->s = qs.str();
        
        theMethod->symbolDef = ast->envs.env->getBinding(theMethod->s);
        assert(theMethod->symbolDef);
        ast->children[0] = theMethod;
        rewrite = true;

        TYPEINFER(ast, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        break;
      }
      else if (fType->isUType() || fType->isException()) {
        ast->astType = at_ucon_apply;
        TYPEINFER(ast, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        break;
      }

      shared_ptr<Type> Fn = buildFnFromApp(ast);
      shared_ptr<Type> expectFn = MBF(Fn);

      UNIFY(trail, ast->child(0)->loc, fType, expectFn);
      
      if (!errFree) {
        ast->symType = newTvar(); 
        break;
      }
      
      shared_ptr<Type> fnArgs = Fn->Args();
      for (size_t i = 0; i < ast->children.size()-1; i++) {
        shared_ptr<AST> arg = ast->child(i+1);
        TYPEINFER(arg, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        
        shared_ptr<Type> fnArg = fnArgs->CompType(i)->getType();
        shared_ptr<Type> acArg = arg->symType->getType();
        
        // by-ref arguments need strict compatibility.
        // by-value arguments can have copy-compatibility.
        if (fnArgs->CompFlags(i) & COMP_BYREF)
          UNIFY(trail, arg->loc, fnArg, acArg);
        else if(fnArg->isArrayByref()) {

          // This case is an array argument being applied when an
          // array-byref is expected, so need to construct the
          // array-byref pair.
          if(acArg->isArray()) {
            ast->child(1) = AST::make(at_mkArrayByref, arg->loc, arg);
            arg = ast->child(1);
            TYPEINFER(arg, gamma, instEnv, impTypes, rewrite, tcc,
                      trail,  USE_MODE, TI_EXPRESSION);
            acArg = arg->symType->getType();
          }
          
          UNIFY(trail, arg->loc, fnArg, acArg);
        }
        else
          UNIFY(trail, arg->loc, MBF(fnArg), acArg);
        
      }
      
      shared_ptr<Type> ret = MBF(Fn->Ret());
      CHKERR(errFree, testNonEscaping(errStream, ast, ret));

      ast->symType = ret;
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
      shared_ptr<AST> ctr = ast->child(0);

      if ((ctr->astType == at_ident) &&
         (ctr->symbolDef->isIdentType(idc_uctor))) {
        // Constructor direct usage
      }
      else if ((ctr->astType == at_fqCtr) &&
               (ctr->child(1)->symbolDef->isIdentType(idc_uctor))) {
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
      
      if (!ctr->symType) {
        TYPEINFER(ctr, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
      }
      
      // The constructor type cannot be a mutable or a maybe type
      shared_ptr<Type> t = ctr->symType->getType();
      if (t->kind != ty_uconv && t->kind != ty_uconr && 
         t->kind != ty_exn) {
        
        errStream << ast->child(0)->loc << ": "
                  << ast->child(0)->s << " cannot be resolved" 
                  << " to a Union (or exception) Constructor."
                  << std::endl;
        errFree = false;
        break;
     }
    
      size_t cnt = nCtArgs(t);
      if (cnt != (ast->children.size() - 1)) {
          errStream << ast->child(0)->loc << ": "
                    << "Constructor " << ast->child(0)->s << " needs "
                    << cnt << " arguments, but obtained"
                    << (ast->children.size() - 1)
                    << std::endl;
          errFree = false;
          break;
      }

      for (size_t i=0, j=1; i < t->components.size(); i++) {
        shared_ptr<comp> ctrComp = t->components[i];
        if (ctrComp->flags & COMP_UNIN_DISCM)
          continue;
        
        shared_ptr<AST> arg = ast->child(j);        
        
        TYPEINFER(arg, gamma, instEnv, impTypes, rewrite, tcc,
                  trail, USE_MODE, TI_EXPRESSION);

        CHKERR(errFree, testNonEscaping(errStream, arg,
                                        arg->symType)); 

        shared_ptr<Type> tv = newTvar();
        UNIFY(trail, arg->loc, t->CompType(i), MBF(tv)); 
        UNIFY(trail, arg->loc, arg->symType, MBF(tv));
        j++;
      }
      
      if (!errFree)
        break;

      if (t->isUType()) {
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
      shared_ptr<AST> ctr = ast->child(0);
      if (!ctr->symType)
        TYPEINFER(ctr, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
      
      // Structure constructor cannot be a mutable or maybe type.
      shared_ptr<Type> t = ctr->symType->getType();
      if ((ctr->astType != at_ident) ||
          (!ctr->symbolDef->isIdentType(id_struct))) {
        errStream << ctr->loc
                  << ": Expected structure"
                  << " constructor taking at least one argument."
                  << std::endl;
        errFree = false;
        break;
      }
      if (t->components.size() == 0) {
        errStream << ast->child(0)->loc << ": "
                  << ast->child(0)->s << " cannot instantiate without "
                  << "definition in scope."
                  << std::endl;
        errFree = false;
        break;
      }
      if ((ast->children.size()-1) != t->components.size()) {
        errStream << ast->child(0)->loc << ": "
                  << "Structure " << ast->child(0)->s << " cannot be" 
                  << " partially/over instantiated." << '\n'
                  << "Constructor call has " << (ast->children.size()-1)
                  << " arguments but structure type has" 
                  << t->components.size() << " components."
                  << std::endl;
        
        errFree = false;
        break;
      }

      for (size_t i=0; i < t->components.size(); i++) {
        shared_ptr<AST> arg = ast->child(i+1);
        TYPEINFER(arg, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        
        shared_ptr<Type> tv = newTvar();
        UNIFY(trail, arg->loc, t->CompType(i), MBF(tv));
        
        UNIFY(trail, arg->loc, arg->symType, MBF(tv));
      }

      ast->symType = t;
      break;
    }

  case at_object_apply:
    // FIX!!
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
      shared_ptr<AST> ctr = ast->child(0);
      if (!ctr->symType)
        TYPEINFER(ctr, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
      
      // Object constructor cannot be a mutable or maybe type.
      shared_ptr<Type> t = ctr->symType->getType();
      if ((ctr->astType != at_ident) ||
          (!ctr->symbolDef->isIdentType(id_struct))) {
        errStream << ctr->loc
                  << ": Expected object"
                  << " constructor taking at least one argument."
                  << std::endl;
        errFree = false;
        break;
      }
      if (t->components.size() == 0) {
        errStream << ast->child(0)->loc << ": "
                  << ast->child(0)->s << " cannot instantiate without "
                  << "definition in scope."
                  << std::endl;
        errFree = false;
        break;
      }

      // Object construction takes exactly one argument, which must be
      // a compatible structure type:
      if ((ast->children.size()-1) != 1) {
        errStream << ast->child(0)->loc << ": "
                  << "Object " << ast->child(0)->s
                  << " should be instantiated with exactly one"
                  << " argument of compatible structure type." 
                  << std::endl;
        
        // FIX: Rest of checks here.

        errFree = false;
        break;
      }

      for (size_t i=0; i < t->components.size(); i++) {
        shared_ptr<AST> arg = ast->child(i+1);
        TYPEINFER(arg, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);

        CHKERR(errFree, testNonEscaping(errStream, arg,
                                        arg->symType)); 

        shared_ptr<Type> tv = newTvar();
        UNIFY(trail, arg->loc, t->CompType(i), MBF(tv));
        
        UNIFY(trail, arg->loc, arg->symType, MBF(tv));
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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_EXPRESSION);      
      
      UNIFY(trail, ast->child(0)->loc, 
            ast->child(0)->symType, MBF(Type::make(ty_bool)));
      
      // match agt_expr
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_EXPRESSION);
      // match agt_expr
      TYPEINFER(ast->child(2), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_EXPRESSION);
      
      shared_ptr<Type> tv = newTvar();
      UNIFY(trail, ast->child(1)->loc, 
            ast->child(1)->symType, MBF(tv));
      UNIFY(trail, ast->child(2)->loc, 
            ast->child(2)->symType, MBF(tv));
      ast->symType = MBF(tv);
      break;
    }

  case at_when:
    {
       /*------------------------------------------------
               A |- e0:t0   A |- e1:t1  U(t0 = 'a|bool)
      ______________________________________________________
                 A |- (when e0 e1): ()
       ------------------------------------------------*/

      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_EXPRESSION);      
      
      UNIFY(trail, ast->child(0)->loc, 
            ast->child(0)->symType, MBF(Type::make(ty_bool)));
      
      // match agt_expr
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail, mode, TI_EXPRESSION);
      
      ast->symType = Type::make(ty_unit);
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
      
      ast->symType = Type::make(ty_bool);
      
      for (size_t c = 0; c < ast->children.size(); c++) {
        TYPEINFER(ast->child(c), gamma, instEnv, impTypes, rewrite, tcc,
                  trail, mode, TI_EXPRESSION);
        
        UNIFY(trail, ast->child(c)->loc, 
              ast->child(c)->symType, MBF(ast->symType));
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

     shared_ptr<Type> tv = newTvar();
     // match at_cond_legs
     shared_ptr<AST> conds = ast->child(0);
     for (size_t c = 0; c < conds->children.size(); c++) {
       shared_ptr<AST> cond = conds->child(c);
       TYPEINFER(cond, gamma, instEnv, impTypes, rewrite, tcc,
                 trail, USE_MODE, TI_EXPRESSION);
       
       UNIFY(trail, cond->loc, cond->symType, MBF(tv));
     }
     conds->symType = MBF(tv);
     
     // match at_otherwise
     TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
               trail,  USE_MODE, TI_EXPRESSION);    
     
     UNIFY(trail, ast->child(1)->loc, 
           ast->child(1)->symType, MBF(tv));
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
     shared_ptr<Type> t = newTvar();
     CHKERR(errFree, unifyPrim(errStream, trail, ast->loc, t, "bool"));
      
     TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
               trail,  USE_MODE, TI_EXPRESSION);
     UNIFY(trail, ast->child(0)->loc, 
           ast->child(0)->symType, MBF(Type::make(ty_bool)));

     TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
               trail,  USE_MODE, TI_EXPRESSION);
     
     ast->symType = ast->child(1)->symType;
     break;
   }

  case at_setbang:
    {
     /*------------------------------------------------
            A |- e1: t1    A |- e2: t2    |-lval e1
             U(t1 = (mutable 'a)|'b   U(t1 = 'c|'b)  
       __________________________________________________
                    A |- (set! e1 e2): ()

       NOTE: lval(e1) check enforced in the loc-chk pass
       ------------------------------------------------*/
      // match agt_expr
      ast->symType = Type::make(ty_unit);
      
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> base = newTvar();
      shared_ptr<Type> mTv = Type::make(ty_mutable, newTvar());
      shared_ptr<Type> mb = Type::make(ty_mbFull, mTv, base);
      
      UNIFY(trail, ast->child(0)->loc,
            ast->child(0)->symType, mb);
      
      UNIFY(trail, ast->child(1)->loc,
            ast->child(1)->symType, MBF(base));

      CHKERR(errFree, testNonEscaping(errStream, ast->child(0), base));
      break;
    }

  case at_sizeof:
  case at_bitsizeof:
    {
      /*------------------------------------------------
          ___________________________________________
                 A |- sizeof(t): word
        ------------------------------------------------*/

      // match agt_type
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, 
                tcc, trail,  USE_MODE, 
                TI_NON_APP_TYPE);

      ast->symType = Type::make(ty_word);

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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> tv = newTvar();
      UNIFY(trail, ast->child(0)->loc,
            ast->child(0)->symType, MBF(tv));
      ast->symType = Type::make(ty_ref, MBF(tv));
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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      ast->symType = newTvar();
      UNIFY(trail, ast->child(0)->loc,
            ast->child(0)->symType,
            MBF(Type::make(ty_ref, ast->symType)));
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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> t = ast->child(0)->symType->getBareType();
      bool process_ndx = false;
      
      switch(t->kind) {
      case ty_ref:
        {                  
          shared_ptr<Type> drType = t->Base()->getBareType();
          if (drType->kind == ty_array) {
            ast->symType = Type::make(ty_ref, drType->Base());
            process_ndx = true;
          }
          else if (drType->kind == ty_structv) {
            shared_ptr<Type> fType = GC_NULL;
            CHKERR(errFree, findField(errStream, drType, 
                                      ast->child(1), fType));
            if (errFree)
              ast->symType = Type::make(ty_ref, fType);
          }
          
          break;
        }
        
      case ty_structr:
        {
          shared_ptr<Type> fType = GC_NULL;
          CHKERR(errFree, findField(errStream, t, 
                                     ast->child(1), fType));
          if (errFree)
            ast->symType = Type::make(ty_ref, fType);
          
          break;
        }

      case ty_vector:
        {
          process_ndx = true;
          ast->symType = Type::make(ty_ref, t->Base());
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
      
      if (process_ndx) {
        ast->flags |= INNER_REF_NDX;
        // match agt_expr
        TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        
        // FIX TO WORD
        UNIFY(trail, ast->child(1)->loc, 
              ast->child(1)->symType, MBF(Type::make(ty_word)));
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
      shared_ptr<AST> topExpr = ast->child(1);
      TYPEINFER(topExpr, gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      shared_ptr<Type> tv = newTvar();
      // match at_case_legs
      shared_ptr<AST> cases = ast->child(2);
      for (size_t c = 0; c < cases->children.size(); c++) {
        shared_ptr<AST> thecase = cases->child(c);
        for (size_t j=2; j < thecase->children.size(); j++) {
          shared_ptr<AST> aCtr = thecase->child(j);
          
          TYPEINFER(aCtr, gamma, instEnv, impTypes, rewrite, 
                    tcc, trail,  USE_MODE, TI_EXPRESSION);      
          
          shared_ptr<Type> aCtrType = aCtr->symType->getType();
          UNIFY(trail, aCtr->loc, 
                topExpr->symType, aCtrType);
        }
        
        TYPEINFER(thecase, gamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        
        if (!errFree)
          continue;
        
        UNIFY(trail, thecase->loc,
              thecase->symType, MBF(tv));
      }
      cases->symType = MBF(tv);

      // match at_otherwise
      shared_ptr<AST> otherwise = ast->child(3);
      if (otherwise->astType != at_Null) {
        shared_ptr<TSEnvironment > legGamma = gamma->newScope();
        otherwise->envs.gamma = legGamma;

        shared_ptr<AST> stIdent = otherwise->child(0); 
        TYPEINFER(stIdent, legGamma, instEnv, impTypes, rewrite, 
                  tcc, trail,  DEF_MODE, TI_EXPRESSION);

        stIdent->symType->link = topExpr->symType;

        // match agt_expr
        TYPEINFER(otherwise->child(1), legGamma, instEnv, impTypes, rewrite, 
                  tcc, trail,  USE_MODE, TI_EXPRESSION);

        otherwise->symType = otherwise->child(1)->symType;

        UNIFY(trail, otherwise->loc, 
              otherwise->symType, MBF(tv));
      }

      ast->symType = MBF(tv);
      
      /* Check consistency and coverage of the switch */
      if (!topExpr->symType->isUType()) {
        errStream << topExpr->loc << ": "
                  << "Only unions are permitted for switching"
                  << " but obtained "
                  << topExpr->symType->asString()
                  << std::endl;
        ast->symType = newTvar();
        errFree = false;
        break;
      } 

      shared_ptr<Type> ut = topExpr->symType->getBareType();
      shared_ptr<Type> uType = obtainFullUnionType(ut);
      
      for (size_t c = 0; c < cases->children.size(); c++) {
        shared_ptr<AST> thecase = cases->child(c);
        for (size_t i=2; i < thecase->children.size(); i++) {
          shared_ptr<AST> ctr = thecase->child(i)->getCtr();          
          bool found=false;
          
          for (size_t j=0; j < uType->components.size(); j++) {
            if (!uType->CompType(j))
              continue;
            
            shared_ptr<Type> cTyp = uType->CompType(j)->getType();
            if (cTyp->defAst == ctr->symbolDef) {
              found = true;
              uType->CompType(j) = GC_NULL;
              break;
            }
          }
          
          if (!found) {
            errStream << ctr->loc << ": "
                      << "Duplicate case label"
                      << ctr->asString()
                      << "." << endl;
            errFree = false;
          }
        }
      }


      bool moreCases = false;
      for (size_t j=0; j < uType->components.size(); j++) 
        if (uType->CompType(j)) {
          moreCases = true;
          break;
        }
      
      if (moreCases) {
        if (otherwise->astType == at_Null) {
          errStream << ast->loc << ": The following cases"
                    << "are not covered: ";
          for (size_t j=0; j < uType->components.size(); j++) {
            shared_ptr<Type> cTyp = uType->CompType(j)->getType();
            if (j > 0)
              errStream << ", ";
            errStream << cTyp->defAst->s;
          }
          errStream << std::endl;
          errFree = false;
        }
      }
      else {
        if (otherwise->astType != at_Null) {
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

  case at_condelse:
    {
      // match agt_expr
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
    
      ast->symType = ast->child(0)->symType;
      break;
    }

  case at_otherwise:
    {
      // Handled explicitly in at_try and at_switch
      assert(false);
    }
  case at_sw_leg:
    {
      // This is used only in the case of a union leg match.
      // match agt_valuePattern
      shared_ptr<TSEnvironment > legGamma = gamma->newScope();
      ast->envs.gamma = legGamma;

      /* Deal with the constructors first */
      shared_ptr<AST> aCtr = ast->child(2)->getCtr();
      shared_ptr<Type> aCtrType = ast->child(2)->symType->getBareType();

      /* Deal with the component structure identifier */
      shared_ptr<TypeScheme> stSigma = aCtr->symbolDef->stSigma;
      shared_ptr<Type> stType = stSigma->type_instance();
      /* If we decide to alow the unification of the structure
         type with the union type, this must be done there */
      assert(stType->typeArgs.size() == aCtrType->typeArgs.size());      
      for (size_t m=0; m < stType->typeArgs.size(); m++) {
        UNIFY(trail, ast->loc, 
              stType->TypeArg(m), aCtrType->TypeArg(m));
      }
      
      /* Checking compatibility of different constructors used in the
         same leg of as switch is done in differently for unions
         and reprs. 
         ** In the case of unions, all union legs must have
         the same field structure. 
         **In  the case of reprs, we have more
         constraints on field layout -- identically named fields must
         be at the same bit-offset and be of the same type. Therefore,
         we can allow different constructors such that only common
         fields are efective within the switch-leg.

         Therefore, in the case of unions, we check for stSigma
         compatibility, but in the case of reprs, we just give the
         switched-leg identifier the type obtained from the stSigma of
         the first constructor, but mark fields that are not in the
         intersection of all constructors as invalid. This flag (on
         the component) is ONLY checked in at_select.  */
      shared_ptr<AST> uninID = aCtr->symType->myContainer;
      bool isRepr = (uninID->flags & UNION_IS_REPR);


      for (size_t c=2; c < ast->children.size(); c++) {
         shared_ptr<AST> ctr = ast->child(c)->getCtr()->symbolDef;
        if (!ctr->stSigma) {
          errStream << ctr->loc << ": Use of constructor "
                    << ctr->s << " whose definition had an error"
                    << std::endl;
          errFree = false;
          break;
        }

        if (!isRepr) {
          if (ctr->stSigma != stSigma) {
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
          if (aCtr == ctr)
            continue;
          
          const shared_ptr<const Type> ctType = ctr->stSigma->tau;
          for (size_t ac=0; ac < stType->components.size(); ac++) {
            shared_ptr<comp> stComp = stType->Component(ac);
            bool found=false;            
            
            for (size_t tc=0; tc < ctType->components.size(); tc++) {
              const shared_ptr<const comp> ctComp = ctType->Component(tc);
              
              if (ctComp->name == stComp->name) {
                found = true;
                break;
              }
            }
            
            if (!found) 
              stComp->flags |= COMP_INVALID;
          }
        }
      }
      
      if (!errFree) {
        ast->symType = newTvar();
        break;
      }
      
      shared_ptr<AST> stIdent = ast->child(0); 
      TYPEINFER(stIdent, legGamma, instEnv, impTypes, rewrite, 
                tcc, trail,  DEF_MODE, TI_EXPRESSION);
      stIdent->symType = stType;
      stIdent->scheme->tau = stType;
      assert(stIdent->scheme->ftvs.empty());

      // match agt_expr
      TYPEINFER(ast->child(1), legGamma, instEnv, impTypes, rewrite, 
                tcc, trail,  USE_MODE, TI_EXPRESSION);

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
      shared_ptr<Type> tv = newTvar();
      shared_ptr<AST> expr = ast->child(0);     
      TYPEINFER(expr, gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      UNIFY(trail, expr->loc, expr->symType, MBF(tv));
      
      ast->symType = MBF(tv);
      
      if (!errFree)
        break;

      // match at_ident: ignore

      // match at_sw_legs
      shared_ptr<AST> cases = ast->child(2);     
      cases->symType = MBF(tv);
      for (size_t c = 0; c < cases->children.size(); c++) {
        shared_ptr<AST> theCase = cases->child(c);
        
        for (size_t j=2; j < theCase->children.size(); j++) {
          shared_ptr<AST> aCtr = theCase->child(j);
          
          TYPEINFER(aCtr, gamma, instEnv, impTypes, rewrite, 
                    tcc, trail, USE_MODE, TI_EXPRESSION);      
          
          if (aCtr->symType->getType()->kind != ty_exn) {
            errStream << aCtr->loc << ": "
                      << " Only Exceptions can be caught"
                      << " Obtained type " 
                      << aCtr->symType->asString()
                      << std::endl;
            errFree = false;
          }
        }        
        
        shared_ptr<TSEnvironment > legGamma = gamma;

        // The deconstructed identifier is bound for use in the
        // legGamma environment only if we are catching a single
        // constructor.
        if (theCase->children.size() == 3) {
          legGamma = gamma->newScope();
          theCase->envs.gamma = legGamma;

          shared_ptr<AST> stIdent = theCase->child(0);
          // Add sIdent to the legGamma environment.
          TYPEINFER(stIdent, legGamma, instEnv, impTypes, rewrite, 
                    tcc, trail,  DEF_MODE, TI_EXPRESSION);

          // Make sIdent's type the correct type.
          shared_ptr<AST> onlyCtr = theCase->child(2)->getCtr();
          assert(onlyCtr->symbolDef->stSigma);
          shared_ptr<Type> stType = onlyCtr->symbolDef->stSigma->type_instance();
          stIdent->symType = stType;
          stIdent->scheme->tau = stType;
          assert(stIdent->scheme->ftvs.empty());
        }
        
        shared_ptr<AST> expr = theCase->child(1);
        TYPEINFER(expr, legGamma, instEnv, impTypes, rewrite, tcc,
                  trail, USE_MODE, TI_EXPRESSION);

        UNIFY(trail, expr->loc, expr->symType, MBF(tv));
        
        theCase->symType = expr->symType;
      }
      
      // match agt_ow
      shared_ptr<AST> ow = ast->child(3);
      if (ow->astType != at_Null) {
        shared_ptr<TSEnvironment > legGamma = gamma->newScope();
        ow->envs.gamma = legGamma;

        shared_ptr<AST> stIdent = ow->child(0);

        // Add sIdent to the legGamma environment.
        TYPEINFER(stIdent, legGamma, instEnv, impTypes, rewrite, 
                  tcc, trail,  DEF_MODE, TI_EXPRESSION);

        stIdent->symType->link = Type::make(ty_exn);

        TYPEINFER(ow->child(1), legGamma, instEnv, impTypes, rewrite, tcc,
                  trail,  USE_MODE, TI_EXPRESSION);
        UNIFY(trail, ow->child(1)->loc,
              ow->child(1)->symType, MBF(tv));  
        ow->symType = ow->child(1)->symType;
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
      TYPEINFER(ast->child(0), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
      
      // HACK: We now allow exception legs as arguments to throw. If
      // an exception leg is being presented, then it is presented as
      // an identifier that is known to be lexically in scope.
      //
      // If that case pertains, then TYPEINFER will have looked up the
      // identifier and arrived at a structure type, and we can chase
      // the pointers to determine whether we built that structure
      // type as a leg type for some exception.
      //
      // If that check fails, or if the argument to THROW is something
      // other than an identifier, we should introduce a unification
      // constraint that the argument must be of some exception type.

      shared_ptr<AST> id = ast->child(0);
      if (id->astType == at_ident &&
          (id->symbolDef->flags & ID_FOR_SWITCH) &&
          (id->symType->defAst->symType->isException())) {
        // Concrete type already determined; nothing further to do.
      }
      else {
        // Try to resolve it. See if resolved result is marked as a
        // switched id. If so we are good, and no unification is
        // required here.
        //
        // If that fails, then unify with ty_exn.
        UNIFY(trail, ast->child(0)->loc,
              ast->child(0)->symType, MBF(Type::make(ty_exn)));
      }
      
      ast->symType = newTvar();    
      break;
    }

  case at_container:
    {
      TYPEINFER(ast->child(1), gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);
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
      shared_ptr<TSEnvironment > doGamma = gamma->newScope();
      ast->envs.gamma = doGamma;

      shared_ptr<AST> dbs = ast->child(0);
      dbs->symType = Type::make(ty_tvar);

      // Initializers
      for (size_t c = 0; c < dbs->children.size(); c++) {
        shared_ptr<AST> db = dbs->child(c);
        shared_ptr<AST> init = db->child(1);
        shared_ptr<Type> tv = newTvar();
        TYPEINFER(init, doGamma, instEnv, impTypes, rewrite, tcc,
                  trail, USE_MODE, TI_EXPRESSION);
        UNIFY(trail, init->loc, 
              init->symType, MBF(tv));
      }
      
      // Definitions
      for (size_t c = 0; c < dbs->children.size(); c++) {
        shared_ptr<AST> db = dbs->child(c);
        shared_ptr<AST> localDefPat = db->child(0);
        shared_ptr<AST> localDef = localDefPat->child(0);
        shared_ptr<AST> init = db->child(1);
        
        localDef->symType = MBF(init->symType);
        TYPEINFER(localDefPat, doGamma, instEnv, impTypes, rewrite, tcc,
                  trail, DEF_MODE, TI_EXPRESSION);
      }

      // Next step initializers
      for (size_t c = 0; c < dbs->children.size(); c++) {
        shared_ptr<AST> db = dbs->child(c);
        shared_ptr<AST> localDef = db->getID();
        shared_ptr<AST> step = db->child(2);
        
        TYPEINFER(step, doGamma, instEnv, impTypes, rewrite, tcc,
                  trail, USE_MODE, TI_EXPRESSION);
        
        UNIFY(trail, step->loc, step->symType, 
              MBF(localDef->symType));
      }

      // Finally evaluate the test and the final expression           
      TYPEINFER(ast->child(1), doGamma, instEnv, impTypes, 
                rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);
      TYPEINFER(ast->child(2), doGamma, instEnv, impTypes, 
                rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);

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
      shared_ptr<AST> test = ast->child(0);
      shared_ptr<AST> result = ast->child(1);
      TYPEINFER(test, gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      UNIFY(trail, test->loc, test->symType, 
            MBF(Type::make(ty_bool)));
      
      shared_ptr<Type> tv = newTvar();
      TYPEINFER(result, gamma, instEnv, impTypes, rewrite, tcc,
                trail,  USE_MODE, TI_EXPRESSION);

      UNIFY(trail, result->loc, 
            result->symType, MBF(tv));
      
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
      shared_ptr<TSEnvironment > letGamma = gamma->newScope();
      shared_ptr<TCConstraints> letTcc = TCConstraints::make();

      shared_ptr<AST> lbs = ast->child(0);
      lbs->symType = Type::make(ty_tvar);
      
      ast->envs.gamma = letGamma;
      ast->envs.instEnv = instEnv;
      lbs->envs.gamma = letGamma;
      lbs->envs.instEnv = instEnv;

         
      if (ast->astType == at_let) {
        CHKERR(errFree, 
               ProcessLetExprs(errStream, lbs, letGamma, instEnv,
                               impTypes, rewrite, letTcc, trail,
                               USE_MODE, TI_EXPRESSION)); 
        CHKERR(errFree, 
               ProcessLetBinds(errStream, lbs, letGamma, instEnv,
                               impTypes, rewrite, letTcc, trail,
                               DEF_MODE, TI_EXPRESSION)); 
      }
      else {
        CHKERR(errFree, 
               ProcessLetBinds(errStream, lbs, letGamma, instEnv,
                               impTypes, rewrite, letTcc, trail,
                               DEF_MODE, TI_EXPRESSION)); 
        CHKERR(errFree, 
               ProcessLetExprs(errStream, lbs, letGamma, instEnv,
                               impTypes, rewrite, letTcc, trail, 
                               USE_MODE, TI_EXPRESSION)); 
      }
      CHKERR(errFree, UnifyLetBinds(errStream, lbs, trail));

      if (!errFree) {
        ast->symType = newTvar();
        break;
      }
      
      // Consider all constraints
      TYPEINFER(ast->child(2), letGamma, instEnv, impTypes, rewrite, 
                letTcc, trail, mode, TI_CONSTRAINT);
   
      shared_ptr<AST> bAst, vAst;
      makeLetGather(lbs, bAst, vAst);
      
      CHKERR(errFree, generalizePat(errStream, ast->loc, 
                                    gamma, instEnv, bAst, vAst, 
                                    letTcc, tcc, trail));
      
      lbs->symType = bAst->symType;
      lbs->scheme = bAst->scheme;
      
      // Finally evaluate the final expression
      TYPEINFER(ast->child(1), letGamma, instEnv, impTypes, 
                rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);
      
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
      shared_ptr<TSEnvironment > letGamma = gamma->newScope();
      
      shared_ptr<AST> lbs = ast->child(0);
      lbs->symType = Type::make(ty_tvar);
      
      ast->envs.gamma = letGamma;
      ast->envs.instEnv = instEnv;
      lbs->envs.gamma = letGamma;
      lbs->envs.instEnv = instEnv;
      
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);
        shared_ptr<AST> id = lb->getID();
        shared_ptr<AST> ip = lb->child(0);
        shared_ptr<AST> expr = lb->child(1);
        
        TYPEINFER(expr, letGamma, instEnv, impTypes, 
                  rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);
        
        TYPEINFER(ip, letGamma, instEnv, impTypes, 
                  rewrite, tcc, trail, DEF_MODE, TI_EXPRESSION);
        
        UNIFY(trail, lb->getID()->loc,
              expr->symType, MBF(id->symType));
      }
      
      TYPEINFER(ast->child(1), letGamma, instEnv, impTypes, 
                rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);
      
      ast->symType = ast->child(1)->symType;
      break;
    }

    // CAREFUL: CAREFUL:
    //
    // This is *NOT* dead code, though, it appears to be so, from the
    // way the above let-cases are written. 
    // this case is used by the (new) polyinstantiator to R&T
    // let-binding instantiations. It is OK to use it ther because we
    // don't have any more polymorphism at that stage.
    //
    // THIS CASE MUST NOT BE USED BY OTHER LET FORMS
  case at_letbinding:
    {
      shared_ptr<AST> id = ast->getID();
      shared_ptr<AST> ip = ast->child(0);
      shared_ptr<AST> expr = ast->child(1);
      if (ast->flags & LB_REC_BIND) {
        TYPEINFER(ip, gamma, instEnv, impTypes, 
                  rewrite, tcc, trail, DEF_MODE, TI_EXPRESSION);

        TYPEINFER(expr, gamma, instEnv, impTypes, 
                  rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);        
      }        
      else {
        TYPEINFER(expr, gamma, instEnv, impTypes, 
                  rewrite, tcc, trail, USE_MODE, TI_EXPRESSION);
        
        TYPEINFER(ip, gamma, instEnv, impTypes, 
                  rewrite, tcc, trail, DEF_MODE, TI_EXPRESSION);
      }
      
      UNIFY(trail, ast->getID()->loc, 
            expr->symType, MBF(id->symType));
      break;
    }
    
  } /* switch */

  DEBUG(TI_AST)
    if (ast->symType)
      errStream << "\t Obtained [" << ast->atKwd() << "] " 
                << ast->asString() << ": "
                << ast->symType->asString(Options::debugTvP) 
                << "{" << (errFree?"OK":"ERR") << "}"
                << endl;
  
  return errFree;
}

/**************************************************************/
/*              Interface to the outside world                */
/**************************************************************/

bool 
UocInfo::DoTypeCheck(std::ostream& errStream, bool init, 
                     bool &rewrite, TI_Flags ti_flags)
{
  DEBUG(TI_UNITWISE)
    errStream << "Now Processing " << uocName
              << " ast = " << uocAst->astTypeName()
              << std::endl;
  
  TypeAstMap impTypes;
  shared_ptr<Trail> trail = Trail::make();
  bool errFree = true;

  if (Options::noPrelude)
    ti_flags |= TI_NO_PRELUDE;
  
  if (init) {
    
    if (false) {
      assert(gamma);      
      assert(gamma->parent);
      gamma = gamma->parent->newDefScope();

      assert(instEnv);      
      assert(instEnv->parent);
      instEnv = instEnv->parent->newDefScope();      
    }
    else {
      gamma = TSEnvironment::make(uocName);
      instEnv = InstEnvironment::make(uocName);
    }
    if ((ti_flags & TI_NO_PRELUDE) == 0)
      CHKERR(errFree, initGamma(std::cerr, gamma, instEnv, uocAst));
    
    if (!errFree)
      return false;
  }

  CHKERR(errFree, typeInfer(errStream, uocAst, gamma, instEnv, 
                            impTypes, rewrite, 
                            TCConstraints::make(), trail, 
                            USE_MODE, ti_flags));
  CHKERR(errFree, checkImpreciseTypes(errStream, gamma, impTypes));

  DEBUG(TI_UNITWISE) {
    errStream << "- - - - - - - - - - - - - - - - - - - - - - - " 
              << endl;
    
    shared_ptr<AST> mod = uocAst;
    for (size_t i=0; i < mod->children.size(); i++) {
      shared_ptr<AST> ast = mod->child(i);
      //errStream << ast->atKwd() << std::endl;
      if (ast->astType == at_define || ast->astType == at_recdef) {
        shared_ptr<AST> id = ast->child(0)->child(0);
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

bool 
UocInfo::TypeCheck(std::ostream& errStream, bool init, 
                   bool &rewrite, TI_Flags ti_flags, std::string mesg)
{
  bool errFree = true;

  // If one considers removing this clear clause,
  // be careful about old types. Pay attention to
  // bindIdentDef() function which preserves types
  // if a type already exists.
  uocAst->clearTypes();  

  errFree = DoTypeCheck(errStream, init, rewrite, ti_flags);
  if (!errFree) 
    errStream << mesg
              << std::endl;
  return errFree;
}

bool
UocInfo::fe_typeCheck(std::ostream& errStream,
                      bool init, unsigned long flags)
{
  // Careful: the input flags are interface flags `uflags,'
  // and not the internal flags `flags.' 
  bool rewrite = false;
  return DoTypeCheck(errStream, init, rewrite, TI_NO_FLAGS);    
}
