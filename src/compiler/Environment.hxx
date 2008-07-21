#ifndef ENVIRONMENT_HXX
#define ENVIRONMENT_HXX

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

#include <sherpa/CVector.hxx>
#include <iostream>

using namespace sherpa;

// Type of (sub) environment, if any.
// Universal, In module scope, or in record scope
//enum envType {universal, mod, rec, none}; 
//enum BindType {bind_type, bind_value};


#define BF_PRIVATE   0x1u  /* Binding is a private binding */
#define BF_COMPLETE  0x2u  /* Binding def is completed */
#define BF_REBIND    0x4u  /* On merge, this binding should replace
			      any existing binding. */
#define BF_NO_MERGE  0x8u  /* This flag should not survive a merger */

/* The following two flags should only be found in the private per-UoC
   copy of the interface environment that is made in importIfBinding()
   in Symtab.cxx */
#define BF_PROVIDING 0x10u /* Interface binding that we are
			      providing. */

template <class T>
struct Binding : public Countable {
  std::string nm;
  GCPtr<T> val;
  unsigned flags;

  Binding(const std::string& _nm, GCPtr<T> _val)
  {
    nm = _nm;
    val = _val;
    flags = 0;
  }
};

template <class T>
struct Environment : public Countable {
  std::string uocName;
  GCPtr<Environment<T> > parent; // in the chain of environments
  GCPtr<Environment<T> > defEnv; // definition level env

  GCPtr< CVector<GCPtr<Binding<T> > > > bindings;

  GCPtr< Binding<T> >
  doGetBinding(const std::string& nm) const;

  GCPtr< Binding<T> >
  getLocalBinding(const std::string& nm) const;

  Environment(std::string _uocName)
  {
    bindings = new CVector<GCPtr< Binding<T> > >;
    uocName = _uocName;
    parent = 0;
    defEnv = 0;
  }

  ~Environment();

  void addBinding(const std::string& name, GCPtr<T> val, 
		  bool rebind = false);
  void
  addDefBinding(const std::string& name, GCPtr<T> val)
  {
    defEnv->addBinding(name, val);
  }

  void unbind(size_t n);
  void removeBinding(const std::string& name);

  // Updates the most-current binding.
  void updateKey(const std::string& from, const std::string& to);

  inline GCPtr<T>
  getBinding(const std::string& nm) const
  {
    GCPtr<const Binding<T> > binding = doGetBinding(nm);
    return (binding ? binding->val : NULL);
  }

  inline unsigned
  getFlags(const std::string& nm)
  {
    GCPtr<const Binding<T> > binding = doGetBinding(nm);
    return (binding ? binding->flags : 0);
  }

  inline void
  setFlags(const std::string& nm, unsigned long flags)
  {
    GCPtr<Binding<T> > binding = doGetBinding(nm);
    if (binding) binding->flags |= flags;
  }

  void mergeBindingsFrom(GCPtr<Environment<T> > from, bool complete=true);
  
  GCPtr<Environment<T> > newScope();

  GCPtr<Environment<T> > newDefScope();

  // Is env my ancestor?
  bool isAncestor(GCPtr<Environment<T> > env);

  std::string asString() const;
};
#endif /* ENVIRONMENT_HXX */
