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

void
TCConstraints::collectAllFnDeps(GCPtr<CVector<GCPtr<Type> > > fnDeps)
{
  for(size_t i=0; i < pred->size(); i++) {
    GCPtr<Typeclass> pr = Pred(i)->getType();    
    if(pr->fnDeps)
      for(size_t j=0; j < pr->fnDeps->size(); j++) {
	GCPtr<Type> fnDep = pr->FnDep(j)->getType();
	assert(fnDep->kind == ty_tyfn);
	fnDeps->append(fnDep);
      }
  }  
}

// Very important: if using pointer comparison, always add
// getType()s to closure and compare with getType()s only.
// I am relying on the fact that no unification happens
// at this stage. Otherwise, a equals() ir strictlyEquals()
// must be used.

void 
TCConstraints::close(GCPtr<CVector<GCPtr<Type> > > closure,
		     GCPtr<const CVector<GCPtr<Type> > > fnDeps)
{
  size_t newSize = 0; 
  size_t oldSize = 0;
  
  do {
    oldSize = newSize;    
    for(size_t i=0; i < fnDeps->size(); i++) {
      GCPtr<Type> fnDep = fnDeps->elem(i)->getType();
      GCPtr<Type> fnDepArgs = fnDep->CompType(0)->getType();
      GCPtr<Type> fnDepRet = fnDep->CompType(1)->getType();      
      GCPtr< CVector <GCPtr<Type> > > argTvs = new CVector <GCPtr<Type> >;
      GCPtr< CVector <GCPtr<Type> > > retTvs = new CVector <GCPtr<Type> >;
      fnDepArgs->collectAllftvs(argTvs);      
      bool foundAll = true;
      for(size_t j=0; j < argTvs->size(); j++) {
	GCPtr<Type> argTv = argTvs->elem(j);
	if(!closure->contains(argTv)) {
	  foundAll = false;
	  break;
	}
      }

      if(foundAll) {	
	fnDepRet->collectAllftvs(retTvs);	
	for(size_t j=0; j < retTvs->size(); j++) {
	  GCPtr<Type> retTv = retTvs->elem(j);
	  if(!closure->contains(retTv))
	    closure->append(retTv);
	}
      }	
    }
    newSize = closure->size();
  } while(newSize > oldSize);
}


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
TCConstraints::contains(GCPtr<Typeclass> tc) 
{
  for(size_t c = 0; c < pred->size(); c++) 
    if(Pred(c)->strictlyEquals(tc, false, true))
      return true;

  return false;
}
 
void 
TCConstraints::addPred(GCPtr<Typeclass> tc) 
{
  size_t c;
  for(c = 0; c < pred->size(); c++) 
    if(Pred(c)->strictlyEquals(tc, false, true)) {
      if(tc->flags & TY_CT_SUBSUMED)
	Pred(c)->flags |= TY_CT_SUBSUMED;
      break;
    }  

  if(c == pred->size()) {
    pred->append(tc);
  }
}

void 
TCConstraints::clearPred(size_t n) 
{
  pred = eliminate<GCPtr<Typeclass> >(pred, n);
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


bool
Typeclass::TCCconcrete(GCPtr<CVector<GCPtr<Type> > > ftvs)
{
  GCPtr<Type> t = getType();
  GCPtr<CVector<GCPtr<Type> > > tvs = new CVector<GCPtr<Type> >;
  t->collectAllftvs(tvs);

  // The real loop to be written here is :
  //for(size_t i=0; i < tvs->size(); i++)
  //  if(ftvs.contains(tvs->elem(i)))
  //    return false;
  //
  // However, we cannot gurantee that ftvs contains getType()s
  // Therefore we write it (equivalently) as:

  for(size_t i=0; i < ftvs->size(); i++) {
    GCPtr<Type> ftv = ftvs->elem(i)->getType();
    if(tvs->contains(ftv))
      return false;
  }
  
  //   std::cout << "Predicate: "
  //   	    << t->asString(NULL) 
  //   	    << "is Concrete."
  //   	    << std::endl;
  
  //   std::cout << "\t ftvs are: ";
  //   for(size_t i=0; i < ftvs->size(); i++)
  //     std::cout << ftvs->elem(i)->asString(NULL) << " ";
  //   std::cout << std::endl;  
  return true;
}

//#define VERBOSE_SOLVE
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
  
  const std::string& copy_compat =
    SpecialNames::spNames.sp_copy_compat;   
  if(pred->defAst->s == copy_compat) {
    handled = true;
    assert(pred->typeArgs->size() == 2);
    GCPtr<Type> t1 = pred->TypeArg(0)->getType();    
    GCPtr<Type> t2 = pred->TypeArg(1)->getType();    

    if(t1 == t2)
      return sTS;

    if(t1->copy_compatible_compat(t2)) {
      if(t1->isConcrete() && t2->isConcrete())
	return sTS;
      
      if(t1->allTvarsRigid() && t2->allTvarsRigid())
	if(t1->copy_compatible_eql(t2))
	  return sTS;
      
      return NULL;
    }
    else {
      errStream << pred->ast->loc << ": "
		<< "Unsolvable copy-compatibility constraint: "
		<< pred->asString()
		<< std::endl;
      errFree = false;
      return NULL;
    }
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
	GCPtr<Type> fndom = pred->FnDep(fd)->CompType(0);
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


/* Checking satisfiability of Typeclass constraints
   Here is the algorithm that is presently being used:
   1) Infer the type
   2) Generalize the type
   3) Reduce type-class constraints based on instance declarations 
   6) If there are unsatisfied fully concrete type-classes 
      constraints, report an error.
   4) Check for ambiguity
   5) If there is an ambiguous condition:
      For each such ambiguous predicate:
        If the ambiguious type-var appears by itself in the 
	constraint report ambiguity error.
	Otherwise, report "No instance found error" 
*/
 

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

  for(size_t i=0; i < tcc->pred->size(); i++) {
    GCPtr<Typeclass> pred = tcc->Pred(i);
    enum { pred_noerror, 
	   pred_ambiguousType, 
	   pred_noInstance } errType = pred_noerror; 	    
    
    if(pred->TCCconcrete(ftvs)) {
      errType = pred_noInstance;
    }
    else {      
      for(size_t j=0; j < ftvs->size(); j++) {
	GCPtr<Type> ftv = ftvs->elem(j);
	if(!tau->boundInType(ftv) && pred->boundInType(ftv)) {
	  for(size_t k=0; ((errType != pred_noerror) && 
			   (k < pred->typeArgs->size())); k++) {
	    GCPtr<Type> arg = pred->TypeArg(k)->getType();
	    if(arg == ftv)
	      errType = pred_ambiguousType;
	  }

	  errType = pred_noInstance; 	  
	  break;
	}
      }
      /* At the end of this loop, if no such variable is found,
	 there is no error */
    }

    switch(errType) {
    case pred_noerror:
      /* good */
      break;

    case pred_noInstance:
      errStream << errLoc << ": "
		<< "No Instance found for "
		<< pred->asString() 
		<< " in " << asString()
		<< std::endl; 

      errFree = false;
      break;
      
    case pred_ambiguousType:
      errStream << errLoc << ": "
		<< "Ambiguous type definition:"
		<< asString()
		<< std::endl;
      errFree = false;
      break;
    }
  }

#ifdef VERBOSE_SOLVE
  errStream << "  - - - - - - - - - - - - - - - " 
	    << std::endl
	    << std::endl;
#endif
  return errFree;
}

