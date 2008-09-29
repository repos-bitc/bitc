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
#include <dirent.h>
#include <sstream>

#include "Type.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"
#include "AST.hxx"
#include "Environment.hxx"
#include "inter-pass.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;

template<class T>
shared_ptr<Binding<T> > 
Environment<T>::getLocalBinding(const std::string& nm) const
{
  const_iterator itr = bindings.find(nm);
  if (itr != bindings.end())
    return itr->second;

  return GC_NULL;
}

template<class T>
shared_ptr<Binding<T> > 
Environment<T>::doGetBinding(const std::string& nm,
			     shared_ptr<Environment<T> > outerLimit) const
{
  shared_ptr<Binding<T> > binding = getLocalBinding(nm);
  
  if (binding) 
    return binding;

  if (parent && parent != outerLimit)
    return parent->doGetBinding(nm, outerLimit);
  
  return GC_NULL;
}

template<class T>
std::string
Environment<T>::asString() const
{
  std::stringstream ss;

  if (parent)
    ss << parent->asString();
  
  for (const_iterator itr = begin(); itr != end(); ++itr)
    ss << itr->first << ": "
       << itr->second->val
       << std::endl;
  
  ss << std::endl;

  return ss.str();
}

template<class T>
void
Environment<T>::removeBinding(const std::string& nm)
{
  iterator itr = bindings.find(nm);
  if (itr != bindings.end()) {
    bindings.erase(itr);
    return;
  }

  if (parent)
    parent->removeBinding(nm);
}

template<class T>
void
Environment<T>::addBinding(const std::string& nm, 
			   shared_ptr<T> val, 
			   bool rebind)
{
  if (rebind) {
    iterator itr = bindings.find(nm);
    if (itr != bindings.end()){
      itr->second->val = val;
      itr->second->flags = 0;
      return;
    }
  }

  bindings[nm] = Binding<T>::make(nm, val);
  latest = bindings[nm]; 
}

template<class T>
void 
Environment<T>::updateKey(const std::string& from, 
			  const std::string& to)
{
  iterator itr = bindings.find(from);
  if (itr != bindings.end()) {
    boost::shared_ptr<Binding<T> > b = itr->second;
    bindings.erase(itr);
    bindings[to] = b;
    return;
  }

  if (parent)
    parent->updateKey(from, to);
}

template<class T>
shared_ptr<Environment<T> > 
Environment<T>::newScope()
{
  shared_ptr<Environment<T> > nEnv = Environment<T>::make(uocName);
  nEnv->parent = this->shared_from_this();
  nEnv->defEnv = defEnv;

  return nEnv;
}

template<class T>
shared_ptr<Environment<T> > 
Environment<T>::newDefScope()
{
  shared_ptr<Environment<T> > nEnv = newScope();
  nEnv->defEnv = nEnv;
  
  return nEnv;
}


template<class T>
bool 
Environment<T>::isAncestor(shared_ptr<Environment<T> > env)
{
  if (parent == env)
    return true;
  else if (!parent)
    return false;
  else
    return parent->isAncestor(env);
}

template<class T>
void
Environment<T>::mergeBindingsFrom(shared_ptr<Environment<T> > from, bool complete)
{
  for (iterator itr = from->begin();
      itr != from->end(); ++itr ) {
    const std::string& nm = itr->first;
    shared_ptr<T> val = itr->second->val;
    unsigned long flags = itr->second->flags;

    if (flags & BF_NO_MERGE)
      continue;

    if (complete)
      flags |= BF_COMPLETE;
    
    bool rebind = (flags & BF_REBIND) ? true : false;
    flags &= ~BF_REBIND;
    
    addBinding(nm, val, rebind);
    setFlags(nm, flags);
  }
}


template<class T>
Environment<T>::~Environment()
{
}

// EXPLICIT INSTANTIATIONS:

template shared_ptr<Binding<AST> > 
Environment<AST>::getLocalBinding(const std::string& nm) const;
template shared_ptr<Binding<TypeScheme> > 
Environment<TypeScheme>::getLocalBinding(const std::string& nm) const;
template shared_ptr<Binding<set<shared_ptr<Instance> > > >
InstEnvironment::getLocalBinding
(const std::string& nm) const;

template shared_ptr<Binding<AST> > 
Environment<AST>::doGetBinding(const std::string& nm, 
			       shared_ptr<Environment<AST> >) const;
template shared_ptr<Binding<TypeScheme> > 
Environment<TypeScheme>::doGetBinding(const std::string& nm,
			       shared_ptr<Environment<TypeScheme> >) const;
template shared_ptr<Binding<set<shared_ptr<Instance> > > >
InstEnvironment::doGetBinding
(const std::string& nm, shared_ptr<InstEnvironment>) const;

template void
Environment<AST>::addBinding(const std::string& nm, shared_ptr<AST> val,
			     bool rebind);
template void
Environment<TypeScheme>::addBinding(const std::string& nm, 
				    shared_ptr<TypeScheme> val,
				    bool rebind);
template void
InstEnvironment::addBinding
(const std::string& nm, shared_ptr<set<shared_ptr<Instance> > > val, bool rebind);


template void
Environment<AST>::removeBinding(const std::string& nm);
template void
Environment<TypeScheme>::removeBinding(const std::string& nm);
template void
InstEnvironment::removeBinding
(const std::string& nm);

template void
Environment<AST>::updateKey(const std::string& from,
			    const std::string& to);
template void
Environment<TypeScheme>::updateKey(const std::string& from,
				   const std::string& to);
template void
InstEnvironment::updateKey
(const std::string& from, const std::string& to);


template shared_ptr<Environment<AST> > 
Environment<AST>::newScope();
template shared_ptr<Environment<TypeScheme> > 
Environment<TypeScheme>::newScope();
template shared_ptr<InstEnvironment > 
InstEnvironment::newScope();


template shared_ptr<Environment<AST> > 
Environment<AST>::newDefScope();
template shared_ptr<Environment<TypeScheme> > 
Environment<TypeScheme>::newDefScope();
template shared_ptr<InstEnvironment > 
InstEnvironment::newDefScope();

template bool 
Environment<AST>::isAncestor(shared_ptr<Environment<AST> > env);
template bool 
Environment<TypeScheme>::isAncestor(shared_ptr<Environment<TypeScheme> > env);
template bool 
InstEnvironment::isAncestor
(shared_ptr<InstEnvironment > env);

template void
Environment<AST>::mergeBindingsFrom
(shared_ptr<Environment<AST> >  from,
 bool complete);
template void
Environment<TypeScheme>::mergeBindingsFrom
(shared_ptr<Environment<TypeScheme> > from,
 bool complete);
template void
InstEnvironment::mergeBindingsFrom
(shared_ptr<InstEnvironment >  from,
 bool complete);

template std::string
Environment<AST>::asString() const;
template std::string
Environment<TypeScheme>::asString() const;
template std::string
InstEnvironment::asString() const;

template
Environment<AST>::~Environment();
template
Environment<TypeScheme>::~Environment();
template
InstEnvironment::~Environment();
 
