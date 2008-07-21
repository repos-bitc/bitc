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
#include <sherpa/UExcept.hxx>
#include <sherpa/CVector.hxx>
#include <sherpa/avl.hxx>
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
  CHKERR(unifies, mySigma->solvePredicates(ss, ast->loc, 
					   instEnv, new Trail)); 
  
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
  LexLoc internalLocation;
  CHKERR(unifies, sigma->solvePredicates(ss, internalLocation, 
					 instEnv, new Trail));
  
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
  if(getType() != this)
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
      GCPtr<Type> fnDepArgs = fnDep->Args()->getType();
      GCPtr<Type> fnDepRet = fnDep->Ret()->getType();      
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
