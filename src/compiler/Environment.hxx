#ifndef ENVIRONMENT_HXX
#define ENVIRONMENT_HXX

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

#include <iostream>
#include <map>
#include <string>
#include <set>

#include <libsherpa/GCPtr.hxx>

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
struct Binding {
  std::string nm;
  sherpa::GCPtr<T> val;
  unsigned flags;

  Binding(const std::string& _nm, sherpa::GCPtr<T> _val)
  {
    nm = _nm;
    val = _val;
    flags = 0;
  }

  static inline sherpa::GCPtr<Binding>
  make (const std::string& _nm, sherpa::GCPtr<T> _val) {
    Binding *tmp = new Binding(_nm, _val);
    return sherpa::GCPtr<Binding>(tmp);
  }
};

template <class T>
struct Environment 
  : public sherpa::enable_shared_from_this<Environment<T> > {
  std::string uocName;
  sherpa::GCPtr<Environment<T> > parent; // in the chain of environments
  sherpa::GCPtr<Environment<T> > defEnv; // definition level env

  typedef typename std::map<std::string, sherpa::GCPtr<Binding<T> > >::iterator iterator;
  typedef typename std::map<std::string, sherpa::GCPtr<Binding<T> > >::const_iterator const_iterator;
  std::map<std::string, sherpa::GCPtr<Binding<T> > > bindings;

private:
  sherpa::GCPtr<Binding<T> > latest;
public:
  sherpa::GCPtr<Binding<T> > getLatest() {
    return latest;
  }

  iterator begin() {
    return bindings.begin();
  }
  iterator end() {
    return bindings.end();
  }
  iterator find(std::string s) {
    return bindings.find(s);
  }
  const_iterator find(std::string s) const {
    return bindings.find(s);
  }
  const_iterator begin() const {
    return bindings.begin();
  }
  const_iterator end() const {
    return bindings.end();
  }
  size_t size() const {
    return bindings.size();
  }


  sherpa::GCPtr< Binding<T> >
  doGetBinding(const std::string& nm) const;

  sherpa::GCPtr< Binding<T> >
  getLocalBinding(const std::string& nm) const;

  Environment(std::string _uocName)
  {
    uocName = _uocName;
    parent = sherpa::GC_NULL;
    defEnv = sherpa::GC_NULL;
  }

  static inline sherpa::GCPtr<Environment>
  make(std::string _uocName) {
    Environment *tmp = new Environment(_uocName);
    return sherpa::GCPtr<Environment>(tmp);
  }

  ~Environment();

  void addBinding(const std::string& name, sherpa::GCPtr<T> val, 
		  bool rebind = false);
  void
  addDefBinding(const std::string& name, sherpa::GCPtr<T> val)
  {
    defEnv->addBinding(name, val);
  }

  void removeBinding(const std::string& name);

  // Updates the most-current binding.
  void updateKey(const std::string& from, const std::string& to);

  inline sherpa::GCPtr<T>
  getBinding(const std::string& nm) const
  {
    sherpa::GCPtr<const Binding<T> > binding = doGetBinding(nm);
    if (binding) return binding->val;
    return sherpa::GC_NULL;
  }

  inline unsigned
  getFlags(const std::string& nm)
  {
    sherpa::GCPtr<const Binding<T> > binding = doGetBinding(nm);
    return (binding ? binding->flags : 0);
  }

  inline void
  setFlags(const std::string& nm, unsigned long flags)
  {
    sherpa::GCPtr<Binding<T> > binding = doGetBinding(nm);
    if (binding) binding->flags |= flags;
  }

  void mergeBindingsFrom(sherpa::GCPtr<Environment<T> > from, bool complete=true);
  
  sherpa::GCPtr<Environment<T> > newScope();

  sherpa::GCPtr<Environment<T> > newDefScope();

  // Is env my ancestor?
  bool isAncestor(sherpa::GCPtr<Environment<T> > env);

  std::string asString() const;
};

// A couple of kinds of environments that we will be defining
// elsewhere.

// In abstract, an InstEnvironment is a set. In future, we may use
// lexical resolution for instances, in which case we will either need
// to change this or we will need to make use of lexically nested
// instance environments.
struct Instance;
typedef Environment<std::set<sherpa::GCPtr<Instance> > > InstEnvironment;

struct AST;
typedef Environment<AST> ASTEnvironment;

struct TypeScheme;
typedef Environment<TypeScheme> TSEnvironment;

#endif /* ENVIRONMENT_HXX */
