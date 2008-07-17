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
#include <dirent.h>

#include "Type.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"
#include "AST.hxx"
#include "Environment.hxx"
#include <assert.h>
#include <sstream>
#include "inter-pass.hxx"

using namespace sherpa;

template<class T>
GCPtr<Binding<T> > 
Environment<T>::getLocalBinding(const std::string& nm) const
{
  for (size_t i = 0; i < bindings->size(); i++)
    if (bindings->elem(i)->nm == nm)
      return bindings->elem(i);

  return NULL;
}

template<class T>
GCPtr<Binding<T> > 
Environment<T>::doGetBinding(const std::string& nm) const
{
  GCPtr<Binding<T> > binding = getLocalBinding(nm);
  
  if (binding)
    return binding;

  if (parent)
    return parent->doGetBinding(nm);

  return NULL;
}

template<class T>
std::string
Environment<T>::asString() const
{
  std::stringstream ss;

  if (parent)
    ss << parent->asString();
  
  for(size_t i=0; i < bindings->size(); i++)
    ss << bindings->elem(i)->nm << ": "
       << bindings->elem(i)->val
       << std::endl;
  
  ss << std::endl;

  return ss.str();
}

template<class T>
void 
Environment<T>::unbind(size_t n)
{
  bindings = eliminate<GCPtr<Binding<T> > >(bindings, n);  
}

template<class T>
void
Environment<T>::removeBinding(const std::string& nm)
{
  for (size_t i = 0; i < bindings->size(); i++) {
    if (bindings->elem(i)->nm == nm) {
      unbind(i);
      return;
    }
  }

  if (parent)
    parent->removeBinding(nm);
}

template<class T>
void
Environment<T>::addBinding(const std::string& name, 
			   GCPtr<T> val, 
			   bool rebind)
{
  if (rebind) {
    GCPtr<Binding<T> > binding = doGetBinding(name);
    if (binding) {
      binding->val = val;
      binding->flags = 0;
      return;
    }
  }

  bindings->insert(0, new Binding<T>(name, val));
}

template<class T>
void 
Environment<T>::updateKey(const std::string& from, 
			  const std::string& to)
{
  for (size_t i = 0; i < bindings->size(); i++) {
    if (bindings->elem(i)->nm == from) {
      bindings->elem(i)->nm = to;
      return;
    }
  }

  if (parent)
    parent->updateKey(from, to);
}

template<class T>
GCPtr<Environment<T> > 
Environment<T>::newScope()
{
  GCPtr<Environment<T> > nEnv = new Environment<T>(uocName);
  nEnv->parent = this;
  nEnv->defEnv = defEnv;

  return nEnv;
}

template<class T>
GCPtr<Environment<T> > 
Environment<T>::newDefScope()
{
  GCPtr<Environment<T> > nEnv = newScope();
  nEnv->defEnv = nEnv;
  
  return nEnv;
}


template<class T>
bool 
Environment<T>::isAncestor(GCPtr<Environment<T> > env)
{
  if(parent == env)
    return true;
  else if(!parent)
    return false;
  else
    return parent->isAncestor(env);
}

template<class T>
void
Environment<T>::mergeBindingsFrom(GCPtr<Environment<T> > from, bool complete)
{
  for(size_t i = 0; i < from->bindings->size(); i++) {
    const std::string& nm = from->bindings->elem(i)->nm;
    GCPtr<T> val = from->bindings->elem(i)->val;
    unsigned long flags = from->bindings->elem(i)->flags;

    if (flags & BF_NO_MERGE)
      continue;

    if(complete)
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

template GCPtr<Binding<AST> > 
Environment<AST>::getLocalBinding(const std::string& nm) const;
template GCPtr<Binding<TypeScheme> > 
Environment<TypeScheme>::getLocalBinding(const std::string& nm) const;
template GCPtr<Binding< CVector<GCPtr<Instance> > > >
Environment< CVector<GCPtr<Instance> > >::getLocalBinding
(const std::string& nm) const;

template GCPtr<Binding<AST> > 
Environment<AST>::doGetBinding(const std::string& nm) const;
template GCPtr<Binding<TypeScheme> > 
Environment<TypeScheme>::doGetBinding(const std::string& nm) const;
template GCPtr<Binding< CVector<GCPtr<Instance> > > >
Environment< CVector<GCPtr<Instance> > >::doGetBinding
(const std::string& nm) const;

template void
Environment<AST>::addBinding(const std::string& nm, GCPtr<AST> val,
			     bool rebind);
template void
Environment<TypeScheme>::addBinding(const std::string& nm, 
				    GCPtr<TypeScheme> val,
				    bool rebind);
template void
Environment< CVector<GCPtr<Instance> > >::addBinding
(const std::string& nm, GCPtr<CVector<GCPtr<Instance> > > val, bool rebind);


template void 
Environment<AST>::unbind(size_t n);
template void 
Environment<TypeScheme>::unbind(size_t n);
template void 
Environment< CVector<GCPtr<Instance> > >::unbind(size_t n);

template void
Environment<AST>::removeBinding(const std::string& nm);
template void
Environment<TypeScheme>::removeBinding(const std::string& nm);
template void
Environment< CVector<GCPtr<Instance> > >::removeBinding
(const std::string& nm);

template void
Environment<AST>::updateKey(const std::string& from,
			    const std::string& to);
template void
Environment<TypeScheme>::updateKey(const std::string& from,
				   const std::string& to);
template void
Environment< CVector<GCPtr<Instance> > >::updateKey
(const std::string& from, const std::string& to);


template GCPtr<Environment<AST> > 
Environment<AST>::newScope();
template GCPtr<Environment<TypeScheme> > 
Environment<TypeScheme>::newScope();
template GCPtr<Environment< CVector<GCPtr<Instance> > > > 
Environment< CVector<GCPtr<Instance> > >::newScope();


template GCPtr<Environment<AST> > 
Environment<AST>::newDefScope();
template GCPtr<Environment<TypeScheme> > 
Environment<TypeScheme>::newDefScope();
template GCPtr<Environment< CVector<GCPtr<Instance> > > > 
Environment< CVector<GCPtr<Instance> > >::newDefScope();

template bool 
Environment<AST>::isAncestor(GCPtr<Environment<AST> > env);
template bool 
Environment<TypeScheme>::isAncestor(GCPtr<Environment<TypeScheme> > env);
template bool 
Environment< CVector<GCPtr<Instance> > >::isAncestor
(GCPtr<Environment< CVector<GCPtr<Instance> > > > env);

template void
Environment<AST>::mergeBindingsFrom
(GCPtr<Environment<AST> >  from,
 bool complete);
template void
Environment<TypeScheme>::mergeBindingsFrom
(GCPtr<Environment<TypeScheme> > from,
 bool complete);
template void
Environment< CVector<GCPtr<Instance> > >::mergeBindingsFrom
(GCPtr<Environment< CVector<GCPtr<Instance> > > >  from,
 bool complete);

template std::string
Environment<AST>::asString() const;
template std::string
Environment<TypeScheme>::asString() const;
template std::string
Environment< CVector<GCPtr<Instance> > >::asString() const;

template
Environment<AST>::~Environment();
template
Environment<TypeScheme>::~Environment();
template
Environment< CVector<GCPtr<Instance> > >::~Environment();
 
