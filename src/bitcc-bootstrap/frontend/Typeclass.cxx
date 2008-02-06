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
#include "inter-pass.hxx"
#include "Unify.hxx"

using namespace sherpa;
using namespace std;

bool 
Instance::equals(std::ostream &errStream, GCPtr<Instance> ins, 
		 GCPtr<const Environment< sherpa::CVector<GCPtr<Instance> > > >
		 instEnv) const
{
  GCPtr<TypeScheme> mySigma = ts->ts_instance_copy();
  GCPtr<TypeScheme> hisSigma = ins->ts->ts_instance_copy();

  //std::cerr << mySigma->asString() << " vs " 
  //	    << hisSigma->asString() 
  //	    << std::endl;

  bool unifies = true;
  
  CHKERR(unifies, mySigma->tau->unifyWith(hisSigma->tau)); 
  
  if(!unifies)
    return false;
    
  assert(mySigma->tcc);
  assert(hisSigma->tcc);
  
  // This will also add self constraints.
  for(size_t j=0; j < hisSigma->tcc->pred->size(); j++) {
    GCPtr<Typeclass> hisPred = hisSigma->tcc->Pred(j);      
    mySigma->tcc->addPred(hisPred);
  }
  
  std::stringstream ss;
  CHKERR(unifies, mySigma->solvePredicates(ss, ast->loc, instEnv)); 
  
  if(!unifies)
    return false;
  
  if (mySigma->tcc->pred->size() == 0)
    return true;
  else
    return false;
}

bool 
Instance::satisfies(std::ostream &errStream,
		    GCPtr<Typeclass> pred, 		    
		    GCPtr<const Environment< sherpa::CVector<GCPtr<Instance> > > >
		    instEnv) const
{
  bool unifies = true;
  GCPtr<TypeScheme> sigma = ts->ts_instance_copy();

  CHKERR(unifies, sigma->tau->unifyWith(pred));   
  
  if(!unifies)
    return false;
    
  if(!sigma->tcc)
    return true;

  std::stringstream ss;
  CHKERR(unifies, sigma->solvePredicates(ss, pred->ast->loc, instEnv));   
  
  if(!unifies)
    return false;
  
  if (sigma->tcc->pred->size() == 0)
    return true;
  else
    return false;
}

bool 
Typeclass::addFnDep(GCPtr<Type> dep) 
{
  if(link)
    return getType()->addFnDep(dep); // getType() OK
  
  size_t c;
  
  if(kind != ty_typeclass)
    assert(false);

  if(dep->kind != ty_tyfn)
    assert(false);
  
  if(!fnDeps)
    fnDeps = new CVector<GCPtr<Type> >;

  for(c = 0; c < fnDeps->size(); c++)
    if(FnDep(c)->strictlyEquals(dep, false, true))
      return false;

  //   std::cout << "Adding fnDep " << dep->asString(NULL) 
  //   	    << " to " << this->asString(NULL) << "."
  //   	    << std::endl;
  fnDeps->append(dep);
  return true;
}

/* Check if a type scheme s = forall 'a*. t\C is ambiguous.
   If there exists a 'a such that 
      'a in {'a*}, 
      'a in FTVS(C) and 
      'a not in FTVS(t')
    then s is ambiguous. */
bool 
TypeScheme::checkAmbiguity(std::ostream &errStream, LexLoc &errLoc)
{
  bool errFree =true;
  for(size_t j=0; j < ftvs->size(); j++) {
    GCPtr<Type> ftv = ftvs->elem(j);
    
    if(!tau->boundInType(ftv) && pred->boundInType(ftv)) {
      // ftv must be bound in some predicate.
      errFree = false;
      break;
    }
  }
  
  if(!errFree)
    errStream << errLoc << ": "
	      << "Ambiguous type definition:"
	      << asString()
	      << std::endl;
  return errFree;
}

/* Migrate appropriate constraints to parent's TCC, if one exists
   let C'' = migrate(parent-sigma, C')
      --> Constraints purely over monomorphic type variables can be
          migrated to the containing scope.
   
  This function returns true if at least one predicate was migrated to
  the containing scope, false otherwise. 

  Suppose the current tye scheme s = forall 'a*.t\C

  For each predicate p in C,

  1) FTV(p) = 0 CANNOT HAPPEN. The prediate is concrete, and must be
     have been solved at this step.

  2) If FTVS(p) intersection 'a* = {}, then this predicate can be
     migrated. 
*/


bool
TypeScheme::migratePredicates(GCPtr<TCConstraints> parentTCC)
{
  if(!parentTCC)
    return false;
  
  bool migrated = false;
  GCPtr<CVector<GCPtr<Typeclass> > > newPred =
    new CVector<GCPtr<Typeclass> >;
  
  for(size_t i=0; i < tcc->pred->size(); i++) {
    GCPtr<Typeclass> pred = tcc->Pred(i)->getType();
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
      newpred->append(pred);
    }
    else {
      parentTcc->addPred(pred);
      migrated = true;
    }
  }
    
  tcc->pred = newPred;
  return migrated;
}

//#define VERBOSE_SOLVE

static bool
mustSolve(GCPtr<Typeclass> t)
{
  t = getType();
  GCPtr< CVector< GCPtr<Type> > > args = new CVector< GCPtr<Type> >;
  
  for(size_t i=0; i < t->typeArgs->size(); i++)
    args->append(t->TypeArg(i));
  
  if(pred->fnDeps)
    for(size_t fd = 0; fd < t->fnDeps->size(); fd++) {
      GCPtr<Type> fdep = t->FnDep(fd);
      GCPtr<Type> ret = fdep->Ret();
      
      GCPtr< CVector< GCPtr<Type> > > newArgs = 
	new CVector< GCPtr<Type> >;
      
      for(size_t i=0; i < args->size(); i++)
	if((*args)[i]->getType() != ret->getType())
	  newArgs->append((*args)[i]);
      
      args = newArgs;
    }
  
  for(size_t i=0; i < args->size(); i++) {
    GCPtr<Type> arg = (*args)[i]->getType();
    if(arg->kind == ty_tvar || 
       arg->kind == ty_mbTop || arg->kind == ty_mbFull)
      return false;
  }

  return true;
}

bool
handlePcst(bool &errFree, std::ostream &errStream,
	   GCPtr<Constraint> ct, 
	   GCPtr<Constraints> caset, 
	   bool &handled)
{
  GCPtr<Type> k = ct->CompType(0)->getType();
  GCPtr<Type> gen = ct->CompType(1)->getType();
  GCPtr<Type> ins = ct->CompType(2)->getType();


  // *(m, tg, ti)
  if(k == Type::Kmono) {
    cset->clearPred(ct);
    handled = true;
    return ins->unifyWith(gen) ;
  }

  if (k == Type::Kpoly) {
    
    // *(p, tg, ti), Immutable(ti)
    if(ins->isDeepImmutable()) {
      cset->clearPred(ct);
      handled = true;
      return true;
    }

    // *(p, tg, ti), ~Immut(ti) (type variables OK here)
    if (!ins->isDeepImmut()) {
      cset->clearPred(ct);
      handled = true;
      return false;
    }
    
    // *(p, tg, ti), Immut(ti), ~Immutable(ti)
    handled = false;
    return true;
  }
  
  assert(k->kind == ty_kvar);
  
  // *(k, tg, ti), Mut(ti)
  if(ins->isDeepMut()) {
    trail->subst(k, Type::Kmono);
    handled = true;
    return true;
  }

  /* U(*(k, tg, ti), *(k, tg, ti')), ti !=~= ti' */
  for(size_t c=0; c < cset->size(); c++) {
    GCPtr<Constraint> newCt = cset->Pred(c)->getType();
    if(newCt == ct)
      continue;
    
    if(newCt->isPcst() && newCt->CompType(0) == k) {
      GCPtr<Type> newIns = newCt->CompType(1);
      if(!ins->unifyWith(newIns)) {
	trail->subst(k, Type::Kpoly);
	handled = true;
	return true;
      }
    }
  }
  
  handled = false;
  return true;
}

static GCPtr<TypeScheme> 
handleSpecialPred(bool &errFree, std::ostream &errStream,
		  GCPtr<Typeclass> pred, bool &handled)
{
  GCPtr<TypeScheme> sTS = new TypeScheme(pred);
  
  // Special handling for ref-types
  // Safe to do name comparison, everyone includes the prelude.
  const std::string &ref_types = SpecialNames::spNames.sp_ref_types; 
  if(pred->defAst->s == ref_types) {
    handled = true;
    assert(pred->typeArgs->size() == 1);
    GCPtr<Type> it = pred->TypeArg(0)->getType();    
    if(it->isRefType())
      return sTS;
    else 
      return NULL;
  }
  
  handled = false;
  return NULL;
}


static GCPtr<TypeScheme> 
findInstance(bool &errFree, std::ostream &errStream,
	     GCPtr<Typeclass> pred, 
	     GCPtr<const Environment< CVector<GCPtr<Instance> > > > instEnv)
{
  // See if this is a special (built-in) typeclass constraint
  bool speciallyHandled = false;
  GCPtr<TypeScheme> sTS = handleSpecialPred(errFree, errStream,  
				      pred, speciallyHandled);
  
  if(speciallyHandled)
    return sTS;
  
  GCPtr<CVector<GCPtr<Instance> > > insts = 
    instEnv->getBinding(pred->defAst->fqn.asString());
  if(!insts)
    return NULL;
  
  for(size_t j=0; j < insts->size(); j++) {
    GCPtr<TypeScheme> ts = (insts->elem(j))->ts->ts_instance_copy();
    if(pred->equals(ts->tau))
      return ts;
#ifdef VERBOSE_SOLVE    
    errStream << "Not applicable: " << ts->asString()
               << std::endl;
#endif
  }  
  return NULL;  
}

static void
unifyWithInstance(std::ostream &errStream,
		  GCPtr<Typeclass> pred, GCPtr<TypeScheme> instScheme,
		  GCPtr<TCConstraints> removedTcc,		  
		  GCPtr<TCConstraints> tcc,
		  LexLoc &errLoc)
{  
  bool errFree = pred->unifyWith(instScheme->tau);
  assert(errFree);  
  
  if(instScheme->tcc)
    for(size_t c=0; c < instScheme->tcc->pred->size(); c++) {
      GCPtr<Typeclass> instPred = instScheme->tcc->Pred(c);
      if(!removedTcc->contains(instPred))
	tcc->addPred(instPred);
    }
}

/**********************************************************
                   THE Constraint solver 

    The input is a set of constraints, which the solver tries to solve
    based on pre-defined rules or known instances. It returns the set
    of residual constraints that:
      -- are known to be solvable
      -- do not constain only concrete types
 
    The predicate solver is a unification based algorthm
    here are the steps to follow:
    1) Handle special prediactes like ref-types as though apropriate
       instances are present. 

    2) Handle the polymorphic constraint as a special case: 
       If we find a constraint
          2.a) c = *(m, t, t1) then Unify(t = t1), c is satisfied
          2.b) c = *(p, t, t1) | Immutable(t1), then c is satisfied
          2.c) c = *(k, t, t1) | mutable(t1), then k = m
          2.d) c1 = *(k, t, t1) and c2 = *(k, t, t2) | ~Unify(t1 = t2)
                   then k = p.
          2.e) c = *(k, t, t1) | Immut(t1), keep the constraint
          2.f) c = *(p, t, t1) | ~Immutable(t1) then error

    3) If there exists a constraint such that c = T(t1,...,tm, ... tn),
       where types tm+1 ... tn are determined by functional
       dependencies, 

       3.a) If t1 != ... != tm != 'a for any 'a, 
              If there exists a unifying instance I, then Unify(c=I).
                 The constraint c is declared satisfied.
              Else fail.

       3.b) Otherwise, let {tm*} <= {t1, ..., tm} be type variables. 

            3.b.i) If there exists a unifying instance I, such that 
                      the unification Unify(c=I) succeeds when {tm*} 
                      are held rigid, then Unify(c=I).
                      the constraint c is declared satisfied.

           3.b.ii) Otherwise, 
                   If there is a unifying instance I, keep
                      the constraint as is (do not unify)
                   Else fail.

       In this case, maybe types aer considered constrained type
       variables. That is, T('a|t) === T('a) | copy-compat('a, t).

    4) If there exists two constraints such that 
             c1 = T(t1,...,tm, ... tn), and
             c2 = T(t1',...,tm', ... tn'), 
       where types tm+1 ... tn, tm+1' ... tn' are determined by
       functional dependencies, Unify(c1 = c2).

 *********************************************************/

bool
TypeScheme::solvePredicates(std::ostream &errStream,
 			    LexLoc &errLoc,
			    GCPtr< const Environment< CVector<GCPtr<Instance> > > >
			    instEnv)
{
  bool errFree = true;
  bool unifiedWithInstInThisPass = false;
  GCPtr<TCConstraints> removedTcc = new TCConstraints;
  
  markRigid(errStream, errLoc);
  
#ifdef VERBOSE_SOLVE
  errStream << std::endl;
  
  errStream << "Starting: " << this->asString(NULL)
	    << std::endl;
#endif

  do {
    unifiedWithInstInThisPass = false;

    for(size_t i=0; i < tcc->pred->size(); i++) {
      GCPtr<Typeclass> pred = tcc->Pred(i);
      GCPtr<TypeScheme> inst = findInstance(errFree, errStream, 
				      pred, instEnv);
      
#ifdef VERBOSE_SOLVE
      errStream << "For Predicate : " << pred->asString(NULL);
#endif		
      
      if(inst) {	
	// This predicate is a tautology	
	// Improve the type using the type of the instance 
	unifyWithInstance(errStream, pred, inst, 
			  removedTcc, tcc, errLoc);      
	unifiedWithInstInThisPass = true;

	removedTcc->addPred(pred);

	// Remove the constraint since we know that it is true.
	// This remove () _must_ be done immediately. The tcc must be
	// current, since further unifications with instances might
	// add other predicates and addPred() must know correct
	// information about current predicates.
	// clearPred OK since we break immediately.
	// We are NOT clearing in a loop.
	tcc->clearPred(i);	
    	
#ifdef VERBOSE_SOLVE
	errStream << " Instance Found. "
		  << " Scheme after Unification = "
		  << asString(NULL)
		  << std::endl;
#endif
	break;
      }
      else {
#ifdef VERBOSE_SOLVE
	errStream << "NoInstance Found" << std::endl;
#endif
      }
    }
  } while(unifiedWithInstInThisPass);


  ///// Continue
      errStream << errLoc << ": "
		<< "No Instance found for "
		<< pred->asString() 
		<< " in " << asString()
		<< std::endl; 

      errFree = false;

#ifdef VERBOSE_SOLVE
  errStream << "  - - - - - - - - - - - - - - - " 
	    << std::endl
	    << std::endl;
#endif
  return errFree;
}


#if 0
void 
TypeScheme::markRigid(std::ostream &errStream, LexLoc &errLoc)
{
  //  tvs that are determined by fnDeps
  GCPtr< CVector<GCPtr<Type> > > det = new CVector<GCPtr<Type> >; 

  // Mark all free Type variables as rigid
#ifdef VERBOSE_SOLVE
  Options::showAllTccs = true;
  errStream << errLoc << ": In " << asString(NULL) 
	    << " {" << tau->asString(NULL) << "} "
	    << " marking: ";
  Options::showAllTccs = false;
#endif

  for(size_t i=0; i < tcc->pred->size(); i++) {
    GCPtr<Typeclass> pred = tcc->Pred(i);

    
    if(pred->fnDeps)
      for(size_t fd = 0; fd < pred->fnDeps->size(); fd++) {
	GCPtr<Type> fndom = pred->FnDep(fd)->Args();
	fndom->collectAllftvs(det);
      }
  } 
  
  for(size_t i=0; i < ftvs->size(); i++) {
    GCPtr<Type> ftv = ftvs->elem(i)->getType();
    
    if(!det->contains(ftv)) {
      ftv->flags |= TY_RIGID;
#ifdef VERBOSE_SOLVE
    errStream << ftv->asString(NULL) << " ";
#endif
    }
  }
#ifdef VERBOSE_SOLVE
  errStream << std::endl;
#endif  
}
#endif

