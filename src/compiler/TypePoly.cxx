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

#include "Options.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "Unify.hxx"

using namespace sherpa;
using namespace std;

bool isExpansive(std::ostream& errStream, 
		GCPtr<const TSEnvironment > gamma,
		GCPtr<const AST> ast);
bool isExpansive(std::ostream& errStream, 
		 GCPtr<const TSEnvironment > gamma,
		 GCPtr<Type> typ);

/**********************************************************
 **********************************************************

    FTV Handling: Support routines for Type generalization
                  and Specialization

 **********************************************************
 **********************************************************/


bool
Type::boundInType(GCPtr<Type> tv)
{
  GCPtr<Type> t = getType();
  
  if(t == tv->getType())
    return true;
   
  if(t->mark & MARK1)
    return false;
  
  t->mark |= MARK1;
  bool bound = false;
  
  for(size_t i=0; (!bound) && (i < t->components->size()); i++) 
    bound = t->CompType(i)->boundInType(tv);

  // To consider cases like (define aNil nil)
  for(size_t i=0; (!bound) && (i < t->typeArgs->size()); i++) 
    bound = t->TypeArg(i)->boundInType(tv);

  // Deal with fnDeps if present
  if(t->fnDeps)
    for(size_t i=0; (!bound) && (i < t->fnDeps->size()); i++) 
      bound = t->FnDep(i)->boundInType(tv);
  
  t->mark &= ~MARK1;
  return bound;
}

bool
Type::boundInGamma(GCPtr<const TSEnvironment > gamma)
{
  GCPtr<Type> tvar = getType();
  while (gamma) {
    for(TSEnvironment::const_iterator itr = gamma->begin();
	itr != gamma->end(); ++itr) {
      GCPtr<TypeScheme> sigma = itr->second->val;

      for (size_t tv = 0; tv < sigma->ftvs->size(); tv++) {
	if (sigma->Ftv(tv)->uniqueID == tvar->uniqueID)
	  return true;
      }
      
      if(sigma->tau->boundInType(tvar))
	return true;
    }
    
    gamma = gamma->parent;
  }
  
  return false;
}

// Collect ALL ftvs regardless of gamma
// This APPENDS TO the vector `tvs'. IT IS NOT NECESSARY THAT
// `tvs' BE EMPTY TO START WITH. 
void
Type::collectAllftvs(GCPtr<CVector<GCPtr<Type> > > tvs)
{
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK3)
    return;

  t->mark |= MARK3;
  
  if(t->kind == ty_tvar) {
    if(!(tvs->contains(t))) {
      tvs->append(t);
    }
  }      
  else {
    for(size_t i=0; i < t->components->size(); i++)
      t->CompType(i)->collectAllftvs(tvs);

    for(size_t i=0; i < t->typeArgs->size(); i++)
      t->TypeArg(i)->collectAllftvs(tvs);

    if(t->fnDeps)
      for(size_t i=0; i < t->fnDeps->size(); i++)
	t->FnDep(i)->collectAllftvs(tvs);
  }

  t->mark &= ~MARK3;
}
 
// Collects ftvs wrt the basic type and TC predicates 
void
TypeScheme::collectAllFtvs()
{
  tau->collectAllftvs(ftvs);  
  if(tcc) {
    for(TCConstraints::iterator itr = tcc->begin();
	itr != tcc->end(); ++itr)
      (*itr)->collectAllftvs(ftvs);      
  }  
}

// Collect the Free Type Variables in a type
// that are unbound in gamma
void
Type::collectftvsWrtGamma(GCPtr<CVector<GCPtr<Type> > > tvs,
			  GCPtr<const TSEnvironment > gamma)
{   
  GCPtr<Type> t = getType();

  if(t->mark & MARK2)
    return;

  t->mark |= MARK2;

  if(t->kind == ty_tvar) {
    assert(t->components->size() == 0);
    if(!t->boundInGamma(gamma) && !(tvs->contains(t))) 
      tvs->append(t);
  }
  else {
    for(size_t i=0; i < t->components->size(); i++)      
      t->CompType(i)->collectftvsWrtGamma(tvs, gamma);
    
    for(size_t i=0; i < t->typeArgs->size(); i++)
      t->TypeArg(i)->collectftvsWrtGamma(tvs, gamma);

    if(t->fnDeps)
      for(size_t i=0; i < t->fnDeps->size(); i++) 
	t->FnDep(i)->collectftvsWrtGamma(tvs, gamma);
  }

  t->mark &= ~MARK2;
}
 

// Remove Free Type Variables that are indirectly influenced
// by type variablles that are bound in Gamma through
// Functional Dependencies
static void
remftvsWrtFnDeps(GCPtr<CVector<GCPtr<Type> > > &ftvs,
		 set<GCPtr<Type> > fnDeps,
		 GCPtr<const TSEnvironment > gamma)
{
  // closure wrt tvs in fnDeps influenced by Gamma.
  set<GCPtr<Type> > closure;

  for(set<GCPtr<Type> >::iterator itr = fnDeps.begin();
      itr != fnDeps.end(); ++itr) {
    GCPtr<Type> fnDep = (*itr);
    GCPtr<CVector<GCPtr<Type> > > tvs =
      new CVector<GCPtr<Type> >;
    fnDep->collectAllftvs(tvs);
    for(size_t j=0; j < tvs->size(); j++) {
      GCPtr<Type> tv = (*tvs)[j];
      if(closure.find(tv) == closure.end() && tv->boundInGamma(gamma))
	closure.insert(tv);
    }
  }

  TCConstraints::close(closure, fnDeps);
  
  GCPtr<CVector<GCPtr<Type> > > newFtvs = new CVector<GCPtr<Type> >;
  for(size_t i=0; i < ftvs->size(); i++) {
    GCPtr<Type> ftv = ftvs->elem(i)->getType();
    if(closure.find(ftv) == closure.end())
      newFtvs->append(ftv);
  }

  ftvs = newFtvs;
}

/**********************************************************
                     FTV Collection
 **********************************************************/
// Collect the Free Type Variables in a type
// that are unbound in gamma
void
TypeScheme::collectftvs(GCPtr<const TSEnvironment > gamma)
{
  tau->collectftvsWrtGamma(ftvs, gamma);  
  if(tcc) {    
    for(TCConstraints::iterator itr = tcc->begin();
	itr != tcc->end(); ++itr) {
      GCPtr<Typeclass> pred = (*itr);
      pred->collectftvsWrtGamma(ftvs, gamma);  
    }
 
    set<GCPtr<Type> > allFnDeps;
    tcc->collectAllFnDeps(allFnDeps);
    remftvsWrtFnDeps(ftvs, allFnDeps, gamma);
  }
}

/**********************************************************
                  Type Scheme Imprevoment
 **********************************************************/

// Remove Ftvs that will never be instantiable.
bool
TypeScheme::removeUnInstFtvs()
{
  bool removed = false;

  for(size_t c=0; c < ftvs->size(); c++) {
    GCPtr<Type> ftv = Ftv(c)->getType();
    if(tau->boundInType(ftv))
      ftv->flags |= TY_CLOS;
  }

  for(TCConstraints::iterator itr = tcc->begin();
      itr != tcc->end(); ++itr) {
    GCPtr<Constraint> ct = (*itr)->getType();

    bool mustAdd=false;
    for(size_t c=0; c < ftvs->size(); c++) {
      GCPtr<Type> ftv = Ftv(c)->getType();
      if(ct->boundInType(ftv) && (ftv->flags & TY_CLOS)) {
	mustAdd = true;
	break;
      }
    }

    if(mustAdd)
      for(size_t c=0; c < ftvs->size(); c++) {
	GCPtr<Type> ftv = Ftv(c)->getType();
	
	if(ct->boundInType(ftv))
	  ftv->flags |= TY_CLOS;
      }
  }

  GCPtr< CVector< GCPtr<Type> > > newTvs = new CVector < GCPtr<Type> >;
  for(size_t c=0; c < ftvs->size(); c++) {
    GCPtr<Type> ftv = Ftv(c)->getType();
    if(ftv->flags & TY_CLOS) {
      newTvs->append(ftv);
      ftv->flags &= ~TY_CLOS;
    }
    else
      removed = true;
  }
  
  ftvs = newTvs;
  return removed;
}

// Remove *generalizable* Ftvs that appear only at copy-positions of
// function types or Typeclass Predicates.
bool
TypeScheme::normalizeConstruction(GCPtr<Trail> trail)
{
  bool removed = false;

  for(size_t c=0; c < ftvs->size(); c++) {
    GCPtr<Type> ftv = Ftv(c)->getType();
    ftv->flags |= TY_COERCE;
  }
  
  tau->markSignMbs(false);
  
  for(TCConstraints::iterator itr = tcc->begin();
      itr != tcc->end(); ++itr) {
    GCPtr<Constraint> ct = (*itr)->getType();
    ct->markSignMbs(true);
  }

  GCPtr< CVector< GCPtr<Type> > > newTvs = new CVector < GCPtr<Type> >;
  for(size_t c=0; c < ftvs->size(); c++) {
    GCPtr<Type> ftv = Ftv(c)->getType();
    if((ftv->flags & TY_COERCE) == 0)
      newTvs->append(ftv);
    else
      removed = true;
  }
  ftvs = newTvs;
  
  // TY_COERCE flag is not cleared, but it does not matter, because
  // all of these types are substituted within the next adjMaybe call.
  tau->adjMaybe(trail, true, false, true);
  for(TCConstraints::iterator itr = tcc->begin();
      itr != tcc->end(); ++itr) {
    GCPtr<Constraint> ct = (*itr)->getType();
    ct->adjMaybe(trail, true, false, true);
  }
  
  return removed;
}


/**********************************************************
 **********************************************************
                Type Generalization

 **********************************************************
 **********************************************************/


/**********************************************************
                   THE Type Generalizer 

   The generalizer returns true if all free-type-variables could be
   completely generalized, false otherwise.

   Here is the Type generalization algorithm:
   Input is a type t and a set of constraints C, wrt to 
   the current let expression let(k) x = e in ...

   1) Solve predicates: let (t', C') = SolvePredicates(C)
      The constraint set C' contains residual constraints. It cannot
      contain any constraints over concrete types.
      This step is not performed for instance generalizations.

   2) Add the polymorphic instantiation constraint *(k, t, t) to C
      if necessary.
      -- This step is not performed for instance generalizations.      
      -- The constraint is only added if !Mut(t) and !Immut(t)

   3) In case of top-level definitions, make a choice for all
      *-constraints: The type is made immutable upto the function
      boundary, and all 'ks are resolved to polymorphic.
      At this stage, re-solve all consrtainst.

   4) Check for value restriction: 
      exp = isExpansive(e) || isExpansive(t')

   5) If !exp, Determine the set of generalizable type variables:
      'a* = (FTVS(t') U FTVS(C')) \ FTVS(gamma)
      Otherwise, 'a* = {}

   6) Remove FTVs that are present purely only in constraints (no need
      for generalization)
         -- remove FTVs that only appear in a constraint that does not 
            contain an FTV that is transitively reachable from the 
            type t' (possibly through other constraints)

   7) Function type simplification: Remove *generalizable* FTVs that
      appear as maybe-type variables at copy-positions of function
      types, since these will be not result in any loss of
      generality. Coerce the type to its non-maybe form.

      If any variables were removed due to (6) or (7), 
      then re-solve all consrtainsts .

   8) If exp, and if generalizing at top-level, instantiate
      free variables to dummy types and issue warnings

   9) Migrate appropriate constraints to parent's TCC, if one exists
      let C'' = migrate(parent-sigma, C')
      --> Constraints purely over monomorphic type variables can be
          migrated to the containing scope.

   10) Check for ambiguity: This is a no-op.

    Finally,  The generalized type is forall 'a*, t' \ C''

 *********************************************************/

enum GenSteps {
  gs_fixAll=0,
  gs_solve=1,
  gs_pcst=2,
  gs_fixTop=3,
  gs_valRes=4,
  gs_genFtvs=5,
  gs_ctrNorm=6,
  gs_fnNorm=7,
  gs_dummy=8,
  gs_migrate=9,
  gs_ambgCheck=10};

// 0     1      2      3       4    5     6       7       8      9    10
//fAll,  Solve, PCST,  fTop,  VRes, Gen, CtNorm, FnNorm, Dummy, Mig, Ambg

static bool genSteps[6][11] = {
  /////////////////////////   COMPLETE INFERENCE  ////////////////////
// Case gen_instance[0]
//fAll,  Solve, PCST,  fTop,  VRes, Gen, CtNorm, FnNorm, Dummy, Mig, Ambg
  {false, false, false, false, true, true, false, false,  false, true, true},
// Case gen_top [1]
//fAll,  Solve, PCST,  fTop,  VRes, Gen, CtNorm, FnNorm, Dummy, Mig, Ambg
  {false, true, true,  true, true, true, true,   true,   true, true, true},
// Case gen_local [2]
//fAll,  Solve, PCST,  fTop,  VRes, Gen, CtNorm, FnNorm, Dummy, Mig, Ambg
  {false, true, true,  false, true, true, true,  false,  false, true, true},

  /////////////////////////   HEURISTIC INFERENCE  ////////////////////
// Case gen_instance[3]
//fAll,  Solve, PCST,  fTop,  VRes, Gen, CtNorm, FnNorm, Dummy, Mig, Ambg
  {true, false, false, false, true, true, false, false,  false, true, true},
// Case gen_top [4]
//fAll,  Solve, PCST,  fTop,  VRes, Gen, CtNorm, FnNorm, Dummy, Mig, Ambg
  {true, true,  false, false, true, true, true,  true,   true,  true, true},
// Case gen_local [5]
//fAll,  Solve, PCST,  fTop,  VRes, Gen, CtNorm, FnNorm, Dummy, Mig, Ambg
  {true, true,  false, false, true, true, true,  false,  false, true, true},
};

#define GEN_STEP(m,s) if(genSteps[m][s])
#define GEN_STEP2(m,s1,s2) if(genSteps[m][s1] || genSteps[m][s2])

bool
TypeScheme::generalize(std::ostream& errStream, 
		       const LexLoc &errLoc,
		       GCPtr<const TSEnvironment > gamma,
		       GCPtr<const InstEnvironment >
		       instEnv, 
		       GCPtr<const AST> expr, 
		       GCPtr<TCConstraints> parentTCC,
		       GCPtr<Trail> trail,
		       GeneralizeMode mode)
{
  bool errFree = true;  
  bool exprExpansive=false;
  bool typExpansive=false;
  bool expansive=false;
  bool rem1=false;
  bool rem2=false;

  GEN_DEBUG errStream << "To Generalize " 
		      << asString(Options::debugTvP)
		      << " for expression "
		      << expr->asString() 
		      << std::endl;
  
  GEN_DEBUG_TL if(mode == gen_top)
    mode = gen_local;
  
  if(Options::heuristicInference) {
    switch(mode) {
    case gen_instance:
      mode = gen_Hinstance;
      break;
    case gen_top:
      mode = gen_Htop;
      break;
    case gen_local:
      mode = gen_Hlocal;
      break;
    default:
      assert(false);
      break;
    }
  }
  
  // Step 0: Heiristic Inference
  GEN_STEP(mode, gs_fixAll) {
    tau->adjMaybe(trail, false, true);

    GEN_DEBUG errStream << "[0] Heuristic Adjustment " 
			<< asString(Options::debugTvP)
			<< std::endl;
  }

  // Step 1
  GEN_STEP(mode, gs_solve) {
    CHKERR(errFree, solvePredicates(errStream, errLoc, 
				    instEnv, trail)); 
    
    GEN_DEBUG errStream << "[1] Solve: " 
			<< asString(Options::debugTvP)
			<< std::endl;
  }

  // Step 2
  GEN_STEP(mode, gs_pcst) {
    if(!tau->isDeepMut() && !tau->isDeepImmut()) {
      GCPtr<Type> pcst = new Constraint(ty_pcst); 
      pcst->components->append(new comp(new Type(ty_kvar)));
      pcst->components->append(new comp(tau)); // General Type
      pcst->components->append(new comp(tau)); // Instantiation Type
      tcc->addPred(pcst);
      
      GEN_DEBUG errStream << "[2] With Pcst: " 
			  << asString(Options::debugTvP)
			  << std::endl;
      
    }
  }

  // Step 3
  GEN_STEP(mode, gs_fixTop) {
    tau->adjMaybe(trail, false, true);

    bool cleared = false;
    for(TCConstraints::iterator itr = tcc->begin();
	itr != tcc->end(); ++itr) {
      GCPtr<Type> pred = (*itr);
      if(!pred->isPcst())
	continue;

      cleared = true;
      GCPtr<Type> k = pred->CompType(0)->getType();
      GCPtr<Type> gen = pred->CompType(1)->getType();
      GCPtr<Type> ins = pred->CompType(2)->getType();
      
      assert(k != Type::Kmono);
      if(k->kind == ty_kvar) {
	trail->subst(k, Type::Kpoly);
	k = k->getType();
      }
      assert(k == Type::Kpoly);
      
      GCPtr<Type> tgg = gen->minimizeDeepMutability();
      GCPtr<Type> tii = ins->minimizeDeepMutability();
      assert(gen->unifyWith(tgg, false, trail, errStream));
      assert(ins->unifyWith(tii, false, trail, errStream));
    }

    if(cleared) {
      set< GCPtr<Typeclass> > oldPreds = tcc->pred;
      tcc->pred.clear();
      
      for(TCConstraints::iterator itr = oldPreds.begin();
	  itr != oldPreds.end(); ++itr) {
	GCPtr<Type> pred = (*itr);
	if(!pred->isPcst())
	  tcc->addPred(pred);
      }
      
      CHKERR(errFree, solvePredicates(errStream, errLoc, 
				      instEnv, trail)); 
    }

    GEN_DEBUG errStream << "[3] Top-Fix: " 
			<< asString(Options::debugTvP)
			<< std::endl;
  }
  
  // Step 4
  GEN_STEP(mode, gs_valRes) {
    exprExpansive = isExpansive(errStream, gamma, expr);
    typExpansive = isExpansive(errStream, gamma, tau);
    expansive = exprExpansive || typExpansive;
  }

  // Step 5
  GEN_STEP(mode, gs_genFtvs) {
    if(!expansive)
      collectftvs(gamma);
    
    GEN_DEBUG errStream << "[5] Generalize: " 
			<< ((expansive) ? " {Expansive} " : " {Value} ")
			<< asString(Options::debugTvP)
			<< std::endl;    
  }
  
  // Step 6
  GEN_STEP(mode, gs_ctrNorm) {
    if(!expansive) {
      rem1 = removeUnInstFtvs();
    
      GEN_DEBUG errStream << "[6] Remove Uninst-Ftvs: " 
			  << asString(Options::debugTvP)
			  << std::endl;
    }
  }
  
  // Step 7
  GEN_STEP(mode, gs_fnNorm) {
    if(!expansive) {
      rem2 = normalizeConstruction(trail);
      
      GEN_DEBUG errStream << "[7] Construction Normalization: " 
			  << asString(Options::debugTvP)
			  << std::endl;
    }
  }
  
  GEN_STEP2(mode, gs_ctrNorm, gs_fnNorm) {
    if(rem1 || rem2)
      CHKERR(errFree, solvePredicates(errStream, errLoc, 
				      instEnv, trail)); 
    
    GEN_DEBUG errStream << "[7#] Re-Solve: " 
			<< asString(Options::debugTvP)
			<< std::endl;
  }
  
  // Step 8
  GEN_STEP(mode, gs_dummy) {
    if (expansive) {
      collectftvs(gamma);

      if(ftvs->size()) {
	GCPtr< CVector< GCPtr<Type> > > dummys = ftvs;
	ftvs = new CVector< GCPtr<Type> >;
	
	for(size_t i=0; i < dummys->size(); i++) {
	  GCPtr<Type> ftv = dummys->elem(i)->getType();
	  ftv->link = new Type(ty_dummy);
	}
	
	errStream << errLoc << ": WARNING: The type of"
		  << " this toplevel definition "
		  << expr->asString() << " "
		  << " cannot be fully generalized"
		  << " due to the value restriction."
		  << " The type obtained is: "
		  << tau->asString() << "."
		  << std::endl;    
      }
    }
  }

  // Step 9
  GEN_STEP(mode, gs_migrate) {
    migratePredicates(parentTCC);
    GEN_DEBUG errStream << "[9] Migrated Constraints: " 
			<< asString(Options::debugTvP)
			<< std::endl;
  }

  // Step 10
  GEN_STEP(mode, gs_ambgCheck) {
    CHKERR(errFree, checkAmbiguity(errStream, errLoc));
    
    GEN_DEBUG errStream << "FINAL: " 
			<< asString(Options::debugTvP)
			<< std::endl 
			<< std::endl;
  }
  
  return errFree;
}


/**********************************************************
                     Pattern Generalization
 **********************************************************/

/* Helper routines to generalize a pattern */
void
updateSigmas(GCPtr<const AST> bp, GCPtr<CVector<GCPtr<Type> > > ftvs,
	     GCPtr<TCConstraints> tcc)
{
  switch(bp->astType) {
  case at_identPattern:
    {
      GCPtr<AST> ident = bp->child(0);
      GCPtr<TypeScheme> sigma = ident->scheme;
      assert(ident->scheme);
      GCPtr<Type> tau = sigma->tau;;
      
      for(size_t i=0; i<ftvs->size(); i++) {
	if(tau->boundInType(ftvs->elem(i))) {
	  sigma->ftvs->append(ftvs->elem(i));
	  continue;
	}
	
	if(tcc)
	  for(TCConstraints::iterator itr = tcc->begin();
	      itr != tcc->end(); ++itr) {
	    GCPtr<Constraint> pred = (*itr);
	    if(pred->boundInType(ftvs->elem(i))) {
	      sigma->ftvs->append(ftvs->elem(i));
	      break;
	    }
	  }
      }
      
      sigma->tcc = tcc;
      break;
    }
    
  case at_letGather:
    {
      for (size_t c = 0; c < bp->children.size(); c++)
	updateSigmas(bp->child(c), ftvs, tcc);
      break;
    }

  default:
    {
      assert(false);
      break;
    }
  }
}

bool
generalizePat(std::ostream& errStream,
	      const LexLoc &errLoc,
	      GCPtr<TSEnvironment > gamma,
	      GCPtr<const InstEnvironment > instEnv,
	      GCPtr<AST> bp, GCPtr<AST> expr,
	      GCPtr<TCConstraints> tcc,
	      GCPtr<TCConstraints> parentTCC,
	      GCPtr<Trail> trail)
{
  bool errFree = true;

  // Make a temporary typeScheme for the pattern.
  // Individual identifiers' TypeScheme will be updated after the 
  // pattern is generalized as a whole.
  GCPtr<TypeScheme> sigma = new TypeScheme(bp->symType, bp, tcc);
  
  CHKERR(errFree, 
	 sigma->generalize(errStream, errLoc, 
			   gamma, instEnv, expr, parentTCC,
			   trail, gen_local));
  
  updateSigmas(bp, sigma->ftvs, tcc);

  // Puts the letgather type here.
  expr->symType = bp->symType = sigma->tau;
  expr->scheme = bp->scheme = sigma;

  return errFree;
}

/******************************************************************
                       Predicate Migration 

  Migrate appropriate constraints to parent's TCC, if one exists
   let C'' = migrate(parent-sigma, C')
      --> Constraints purely over monomorphic type variables can be
          migrated to the containing scope.
   
  This function returns true if at least one predicate was migrated to
  the containing scope, false otherwise. 

  Suppose the current type scheme s = forall 'a*.t\C

  For each predicate p in C,

  1) FTV(p) = 0 CANNOT HAPPEN. The prediate is concrete, and must be
     have been solved at this step.

  2) If FTVS(p) intersection 'a* = {}, then this predicate can be
     migrated. 
*********************************************************************/


bool
TypeScheme::migratePredicates(GCPtr<TCConstraints> parentTCC)
{
  if(!parentTCC)
    return false;
  
  bool migrated = false;
  set<GCPtr<Typeclass> > newPred;
  
  for(TCConstraints::iterator itr = tcc->begin();
      itr != tcc->end(); ++itr) {
    GCPtr<Typeclass> pred = (*itr)->getType();
    GCPtr< CVector< GCPtr<Type> > > allFtvs = new CVector<GCPtr<Type> >;
    pred->collectAllftvs(allFtvs);
    
    assert(allFtvs->size() != 0);
    
    bool hasFtv = false;
    for(size_t j=0; j < allFtvs->size(); j++) {
      GCPtr<Type> ftv = allFtvs->elem(j)->getType();
      
      if(ftvs->contains(ftv)) {
	hasFtv = true;
	break;
      }
    }
    
    if(hasFtv) {
      newPred.insert(pred);
    }
    else {
      parentTCC->addPred(pred);
      migrated = true;
    }
  }
    
  tcc->pred = newPred;
  return migrated;
}

/**************************************************************
                         Ambiguity Check

   Check if a type scheme s = forall 'a*. t\C is ambiguous.
   If there exists a 'a such that 
      'a in {'a*}, 
      'a in FTVS(C) and 
      'a not in FTVS(t')
    then s is ambiguous.

   For example: 
     read:  forall 'a. () -> 'a \ {Readable('a)}
     write: forall 'a. 'a -> () \ {Writable('a)}

   What about write(read ()) ?
  
     write(read ()): forall 'a. () \ Readable('a), Writable('a).

   This case is traditionally declared and error because there is no
   way to instantiate the 'a at the use location.

   It is not clear that "ambiguous" typing is an error. In fact, it
   does not break subject reduction, and execution can continue by
   picking any instantiation of the variable. In particular, it is (in
   a way) necessary in the case of polymorphic consrtaints.

   For example, consider:
   
    let(k1) id = \x.x 
    Ignoring the internal maybe types at function argument and return
    positions, we can write:
    
    id: forall 'a,'b. 'b|'a->'a  \ {*(k1, 'b|'a->'a, 'b|'a->'a)}
    
    This type is not ambiguous. Now, if we write:

    let(k2) id2 = \y.(id y),

    the type if id2 will be:

    id2: forall 'c,'d,'e. 'd|'c->'c  \ {*(k1, 'b|'a->'a, 'e|'c->'c),
                                        *(k2, 'd|'c->'c, 'd|'c->'c)}

    Here, the type of id2 will be declared ambiguous, which we cannot
    accept. 
    
   It seems that we can take a middle ground where ambiguity check is
   performed only for type-class constraints. However, in the presence
   of copy-compatibility, even this is insufficient. Consider the
   following case:

   (deftypeclass (Tc 'a)
      mt: (fn ('a) bool))

   (define (f x) (mt x))
   f: forall 'a,'b,'c,'d. (fn ('a|'b) 'c|bool) \ Tc('d|'b)

   Actually, the top-level mutability on the type-class does not
   matter (as long as the argument) is not used in a reference
   context, and can be ignored in the ambiguity check. However,
   it seems that the correct solution is to turn off the ambiguity
   check. There is now only a stub-code for the ambiguty check.
*********************************************************************/

bool 
TypeScheme::checkAmbiguity(std::ostream &errStream, const LexLoc &errLoc)
{
#if 0
  bool errFree =true;
  for(size_t j=0; j < ftvs->size(); j++) {
    GCPtr<Type> ftv = ftvs->elem(j);
    
    if(!tau->boundInType(ftv)) {
      // ftv must be bound in some predicate.

      for(size_t c=0; c < tcc->size(); c++) {
	GCPtr<Typeclass> pred = tcc->Pred(c);
	if(pred->isPcst())
	  continue;
	
	// The ftv is bound in a type-class predicate.
	if(pred->boundInType(ftv)) {
	  errStream << errLoc << ": "
		    << "Type variable "
		    << ftv->asString(Options::debugTvP)
		    << " unbound in "
		    << tau->asString(Options::debugTvP)
		    << " wrt "
		    << asString(Options::debugTvP)
		    << std::endl;
	  
	  errFree = false;
	  break;
	}
      }
    }
  }

  if(!errFree)
    errStream << errLoc << ": "
	      << "Ambiguous type definition:"
	      << asString()
	      << std::endl;
  return errFree;
#else
  return true;
#endif
}


/**********************************************************
 **********************************************************
               Type Specialization

 **********************************************************
 **********************************************************/


/**********************************************************
                  THE Type Specializer 
***********************************************************/

GCPtr<Type> 
Type::TypeSpecializeReal(GCPtr<CVector<GCPtr<Type> > > ftvs,
			 GCPtr<CVector<GCPtr<Type> > > nftvs)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> theType = new Type(t);
  theType->flags &= ~TY_SP_MASK;
  theType->typeArgs->erase();
  theType->components->erase();
  theType->fnDeps = NULL;
  GCPtr<Type> retType = theType;
  
  INS_DEBUG std::cout << "To Specialize " 
		      << this->asString()  
		      << std::endl;  

  if(t->sp)
    retType = t->sp;
  else {    
    t->sp = retType;
  
    switch(t->kind) {    
    case ty_kvar:
      {
	retType = t;
	break;
      }
    case ty_pcst:
      {
	// the let-kind and generic type are added as is.
	theType->components->append(new comp(t->CompType(0)));
	theType->components->append(new comp(t->CompType(1)));
	// The instance of the constraint is specialized.
	GCPtr<Type> ins = t->CompType(2)->TypeSpecializeReal(ftvs, nftvs);
	theType->components->append(new comp(ins));
	break;
      }
    case ty_tvar:
      {
	size_t i=0;
	for(i=0; i<ftvs->size(); i++) {
	  GCPtr<Type> ftv = ftvs->elem(i)->getType();	  
	  if(ftv->kind == ty_tvar && t->uniqueID == ftv->uniqueID) {
	    theType->link = nftvs->elem(i); 
	    break;
	  }
	}
	
	// If the variable was NOT in ftv list, then 
	// we should link it to the original, in order to honor
	// variable capture
	if(i == ftvs->size())
	  theType->link = t;
      	break;
      }

    default:
      {      
	/* Deal with Type-args */
	for(size_t i=0; i<t->typeArgs->size(); i++) {
	  GCPtr<Type> arg = t->TypeArg(i)->getType();
	  GCPtr<Type> newArg = arg->TypeSpecializeReal(ftvs, nftvs);
	  
	  theType->typeArgs->append(newArg);
	}
            
	/* Deal with Components */
	for(size_t i=0; i<t->components->size(); i++) {
	  comp *nComp = 
	    new comp(t->CompName(i),
		     t->CompType(i)->TypeSpecializeReal(ftvs, nftvs),
		     t->CompFlags(i));
	  theType->components->append(nComp);
	}

	/* Deal with fnDeps if any */
	if(t->fnDeps) {
	  theType->fnDeps = new CVector<GCPtr<Type> >;

	  for(size_t i=0; i<t->fnDeps->size(); i++) {
	    GCPtr<Type> fnDep = t->FnDep(i)->TypeSpecializeReal(ftvs, nftvs);
	    theType->addFnDep(fnDep);
	  }
	}
	
	break;
      }      
    }
  }
  
  INS_DEBUG std::cout << "\t Specialized " 
		      << getType()->asString(NULL) 
		      << " to " 
		      << retType->getType()->asString(NULL) 
		      << std::endl;
  
  return retType;
}

// Clear the sp (specialization) field of type records recursively.
void
Type::clear_sp()
{
  GCPtr<Type> t = getType();
  if(!t->sp)
    return;

  t->sp = NULL;

  for(size_t i=0; i<t->typeArgs->size(); i++)
    t->TypeArg(i)->clear_sp();

  for(size_t i=0; i<t->components->size(); i++)
    t->CompType(i)->clear_sp();

  if(t->fnDeps)
    for(size_t i=0; i<t->fnDeps->size(); i++)
      t->FnDep(i)->clear_sp();
}

/**********************************************************
                  The Specizlizer interface 
***********************************************************/

GCPtr<Type> 
Type::TypeSpecialize(GCPtr<CVector<GCPtr<Type> > > ftvs,
		     GCPtr<CVector<GCPtr<Type> > > nftvs)
{
  GCPtr<Type> specializedType = TypeSpecializeReal(ftvs, nftvs);
  clear_sp();
  return specializedType;
}


