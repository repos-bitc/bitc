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
#include <assert.h>

#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"

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

void 
TCConstraints::normalize() 
{
  GCPtr<CVector<GCPtr<Typeclass> > > allPreds = pred;
  pred = new CVector<GCPtr<Typeclass> >;
  
  for(size_t c=0; c < allPreds->size(); c++)
    addPred((*allPreds)[c]);
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

