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

bool 
Instance::equals(shared_ptr<Instance> ins, 
                 shared_ptr<const InstEnvironment > instEnv) const
{
  shared_ptr<TypeScheme> mySigma = ts->ts_instance();
  shared_ptr<TypeScheme> hisSigma = ins->ts->ts_instance();

  //std::cerr << mySigma->asString() << " vs " 
  //            << hisSigma->asString() 
  //            << std::endl;

  bool unifies = true;
  
  CHKERR(unifies, mySigma->tau->unifyWith(hisSigma->tau)); 
  
  if (!unifies)
    return false;
    
  assert(mySigma->tcc);
  assert(hisSigma->tcc);
  
  // This will also add self constraints.
  for (TypeSet::iterator itr = hisSigma->tcc->begin(); 
       itr != hisSigma->tcc->end(); ++itr) {
    shared_ptr<Typeclass> hisPred = (*itr);
    mySigma->tcc->addPred(hisPred);
  }
  
  std::stringstream ss;
  CHKERR(unifies, mySigma->solvePredicates(ss, ast->loc, 
                                           instEnv, Trail::make())); 
  
  if (!unifies)
    return false;
  
  if (mySigma->tcc->empty())
    return true;
  else
    return false;
}

// Check Instance overlapping:
// Currently, all instances must be absolutely non-overlapping --
// that is, non-unifiable.
//
// The operlapping check is different from equality check
// For example, consider a class ABC wherein, we have instances
//
// (definstance (forall ((IntLit 'a)) (ABC 'a))  ... )
// (definstance (forall ((FloatLit 'a)) (ABC 'a)) ...)
//
// The two instances are not equal, both have type (ABC 'a), and
// therefore are overlapping. 
//
// If we declare these instances as non-overlapping, in the constraint
// solver, if we have a constraint (ABC int32) The solver can 
// 1) First come across the (((FloatLit 'a)) (ABC 'a)) instance
// 2) Deem 'a unifiable with int32, assuming non-overlap
// 3) Add pre-condition (FloatLit int32) which is unsatisfiable.
//
// The solver unifies with the first unifiable instance. It is not a
// backtracking solver which tries other instances if the overall
// solving fails for an instance. 

bool 
Instance::overlaps(boost::shared_ptr<Instance> ins) const
{
  return ts->tau->equals(ins->ts->tau); 
}


bool 
Instance::satisfies(shared_ptr<Typeclass> pred,                     
                    shared_ptr<const InstEnvironment >
                    instEnv) const
{
  bool unifies = true;
  shared_ptr<TypeScheme> sigma = ts->ts_instance();

  CHKERR(unifies, sigma->tau->unifyWith(pred));   
  
  if (!unifies)
    return false;
    
  if (!sigma->tcc)
    return true;

  std::stringstream ss;
  LexLoc internalLocation;
  CHKERR(unifies, sigma->solvePredicates(ss, internalLocation, 
                                         instEnv, Trail::make()));
  
  if (!unifies)
    return false;
  
  if (sigma->tcc->empty())
    return true;
  else
    return false;
}

bool 
Typeclass::addFnDep(shared_ptr<Type> dep) 
{
  if (getType() != shared_from_this())
    return getType()->addFnDep(dep); // getType() OK
  
  if (typeTag != ty_typeclass)
    assert(false);

  if (dep->typeTag != ty_tyfn)
    assert(false);
  
  for (TypeSet::iterator itr = fnDeps.begin(); 
      itr != fnDeps.end(); ++itr) {
    if ((*itr)->strictlyEquals(dep, false, true))
      return false;
  }

  //   std::cout << "Adding fnDep " << dep->asString(NULL) 
  //               << " to " << this->asString(NULL) << "."
  //               << std::endl;
  fnDeps.insert(dep);
  return true;
}

void
TCConstraints::collectAllFnDeps(TypeSet& fnDeps)
{
  for (iterator itr = begin(); itr != end(); ++itr) {
    shared_ptr<Typeclass> pr = (*itr)->getType();    

    for (TypeSet::iterator itr_j=pr->fnDeps.begin();
        itr_j != pr->fnDeps.end(); ++itr_j) {
      shared_ptr<Type> fnDep = (*itr_j)->getType();
      assert(fnDep->typeTag == ty_tyfn);
      fnDeps.insert(fnDep);
    }
  }  
}

// Very important: if using pointer comparison, always add
// getType()s to closure and compare with getType()s only.
// I am relying on the fact that no unification happens
// at this stage. Otherwise, a equals() or strictlyEquals()
// must be used.

void 
TCConstraints::close(TypeSet& closure,
                     const TypeSet& fnDeps)
{
  size_t newSize = 0; 
  size_t oldSize = 0;
  
  do {
    oldSize = newSize;    
    for (TypeSet::iterator itr = fnDeps.begin();
        itr != fnDeps.end(); ++itr) {
      shared_ptr<Type> fnDep = (*itr)->getType();
      shared_ptr<Type> fnDepArgs = fnDep->Args()->getType();
      shared_ptr<Type> fnDepRet = fnDep->Ret()->getType();      
      TypeSet argTvs;
      TypeSet retTvs;
      fnDepArgs->collectAllftvs(argTvs);      
      bool foundAll = true;
      for (TypeSet::iterator itr_j = argTvs.begin();
          itr_j != argTvs.end(); ++itr_j) {
        shared_ptr<Type> argTv = (*itr_j);
        if (closure.find(argTv) == closure.end()) {
          foundAll = false;
          break;
        }
      }

      if (foundAll) {        
        fnDepRet->collectAllftvs(retTvs);        
        for (TypeSet::iterator itr_j = retTvs.begin();
            itr_j != retTvs.end(); ++itr_j) {
          shared_ptr<Type> retTv = (*itr_j);
          if (closure.find(retTv) == closure.end())
            closure.insert(retTv);
        }
      }        
    }
    newSize = closure.size();
  } while (newSize > oldSize);
}
