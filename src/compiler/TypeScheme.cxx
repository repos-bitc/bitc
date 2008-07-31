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

TypeScheme::TypeScheme(GCPtr<Type> _tau, GCPtr<AST> _ast, GCPtr<TCConstraints> _tcc)
{
  tau = _tau;
  ast = _ast;
  tcc = _tcc;
}

GCPtr<Type> 
TypeScheme::type_instance_copy()
{
  normalize();

  //std::cout << "Instantiating by copy " << this->asString();

  vector<GCPtr<Type> > cnftvs;
  vector<GCPtr<Type> > cftvs;
  
  for(TypeSet::iterator itr_i = ftvs.begin(); 
      itr_i != ftvs.end(); ++itr_i) {
    cftvs.push_back(*itr_i);
    cnftvs.push_back(newTvar());
  }
  
  GCPtr<Type> t = tau->TypeSpecialize(cftvs, cnftvs); 
  //std::cout << " to " << t->asString() << std::endl;

  return t;
}


GCPtr<Type> 
TypeScheme::type_instance()
{
  normalize();
  //std::cout << "Instantiating " << this->asString();

  GCPtr<Type> t;
  if(ftvs.empty())
    t = tau;
  else
    t = type_instance_copy();

  //std::cout << " to " << t->asString() << std::endl;

  return t;
}


GCPtr<TypeScheme> 
TypeScheme::ts_instance(bool fullCopy)
{
  normalize();

  GCPtr<TypeScheme> ts = TypeScheme::make(tau, ast);
  ts->tau = GC_NULL;

  vector<GCPtr<Type> > cnftvs;
  vector<GCPtr<Type> > cftvs;
  
  for(TypeSet::iterator itr_i = ftvs.begin(); 
      itr_i != ftvs.end(); ++itr_i) {
    GCPtr<Type> tv = newTvar();
    ts->ftvs.insert(tv);
    cftvs.push_back(*itr_i);
    cnftvs.push_back(tv);
  }
  
  if(fullCopy || ftvs.size())
    ts->tau = tau->TypeSpecializeReal(cftvs, cnftvs);  
  else 
    ts->tau = tau;
 
  if(tcc) {
    GCPtr<TCConstraints> _tcc = TCConstraints::make();
    addConstraints(_tcc);
    
    ts->tcc = TCConstraints::make();
    for(TypeSet::iterator itr = _tcc->begin();
	itr != _tcc->end(); ++itr) {
      GCPtr<Typeclass> pred;
      
      if(fullCopy || ftvs.size())
	pred = (*itr)->TypeSpecializeReal(cftvs, cnftvs);
      else
	pred = (*itr);
      
      ts->tcc->addPred(pred);
    }
  }

  tau->clear_sp();

  if(tcc)
    for(TypeSet::iterator itr = tcc->begin();
	itr != tcc->end(); ++itr)
      (*itr)->clear_sp();
  
  return ts;
}

GCPtr<TypeScheme> 
TypeScheme::ts_instance_copy() 
{
  return ts_instance(true);
}

void
TypeScheme::addConstraints(GCPtr<TCConstraints> _tcc) const
{
  if(!tcc)
    return;
  
  TypeSet allFtvs;
  tau->collectAllftvs(allFtvs);
  
  for(TypeSet::iterator itr = tcc->begin();
      itr != tcc->end(); ++itr) {
    for(TypeSet::iterator itr_j = allFtvs.begin();
	itr_j != allFtvs.end(); ++itr_j)
      if((*itr)->boundInType(*itr_j)) {
	_tcc->addPred(*itr);
	break;
      }
  }
  
  //if(tcc->pred.size()) {
  // std::cout << tau->ast->loc << "AddConstraints("
  //	      << tau->asString() << ", ";    
  // for(size_t i = 0; i < tcc->pred.size(); i++)
  //   std::cout << tcc->pred[i]->asString() << ", ";
  // std::cout << ") = ";
  // for(size_t i = 0; i < _tcc->pred.size(); i++)
  //   std::cout << _tcc->pred[i]->asString() << ", ";       
  // std::cout << "."
  //	      << std::endl;
  //}
}

bool
TypeScheme::normalize() 
{
  bool changed = false;

  TS_NORM_DEBUG
    std::cerr << "Considering: "
	      << asString(Options::debugTvP, false)
	      << std::endl;
  
  
  TypeSet newTvs;
  for(TypeSet::iterator itr_c = ftvs.begin();
      itr_c != ftvs.end(); ++itr_c) {
    GCPtr<Type> ftv = (*itr_c)->getType();
    
    if(ftv->kind == ty_tvar)
      newTvs.insert(ftv);
    else
      changed = true;
  }
  ftvs = newTvs;
  
  if(tcc) {
    set< GCPtr<Constraint> > allPreds = tcc->pred;
    tcc->pred.clear();
    
    for(TypeSet::iterator itr = allPreds.begin();
	itr != allPreds.end(); ++itr) {
      GCPtr<Constraint> ct = (*itr)->getType();
      if(!ct->isPcst()) {
	tcc->addPred(ct);
	continue;
      }
      
      GCPtr<Type> k = ct->CompType(0)->getType();
      GCPtr<Type> tg = ct->CompType(1)->getType();

      /* If k = M, the solver must have handled this case
	 and unified tg = ti
	               _
	 If k = P and |_|(tg), then the solver must have handled this
	 case and unified tg = I(tg) and ti = I(ti).
	 Actually, we can check Immut(tg) here;


	 In either case, drop this predicate. 
	 Otherwise, add it to newPred.                  */
      
      if((k == Type::Kmono) ||
	 ((k == Type::Kpoly) && tg->isConcretizable())) {
	changed = true;
      }
      else {
	tcc->addPred(ct);
      }
    }
  }
  
  TS_NORM_DEBUG
    if(changed)
      std::cerr << "\t\tNormalized to "
		<< asString(Options::debugTvP, false)
		<< std::endl;
  
  return changed;
}
