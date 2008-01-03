#ifndef TYPECLASS_HXX
#define TYPECLASS_HXX

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

#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/avl.hxx>
#include "AST.hxx"
#include "Type.hxx"
#include "INOstream.hxx"

typedef Type Typeclass;
struct TypeScheme;

struct Instance : public Countable {
  GCPtr<TypeScheme> ts;
  GCPtr<AST> ast;
  
  Instance(GCPtr<TypeScheme> _ts, GCPtr<AST>_ins)
  {
    ts = _ts;
    ast = _ins;
  }
  
  bool equals(std::ostream &errStream, GCPtr<Instance> ins, 
	      GCPtr<const Environment< CVector<GCPtr<Instance> > > >
	      instEnv) const;
  bool satisfies(std::ostream &errStream, GCPtr<Typeclass> pred, 
		 GCPtr<const Environment< CVector<GCPtr<Instance> > > >
		 instEnv) const;
  std::string asString() const;
  std::string asXML() const;
  void asXML(INOstream &out) const;
};


/* Type class constraints */
 
struct TCConstraints : public Countable {
  // Type class predicates
  GCPtr<CVector<GCPtr<Typeclass> > > pred;
  
  TCConstraints()
  {
    pred = new CVector<GCPtr<Typeclass> >;
  }

  size_t size() { return pred->size(); }

  void addPred(GCPtr<Typeclass> tc);
  void clearPred(size_t n);
  void normalize();
  
  bool contains(GCPtr<Typeclass> tc);
  void collectAllFnDeps(GCPtr<CVector<GCPtr<Type> > > fnDeps);
  
  // Compute the closure of all functional dependencies 
  // supplied in the vector
  static void close(GCPtr<CVector<GCPtr<Type> > > closure,
		    GCPtr<const CVector<GCPtr<Type> > > fnDeps);
  void clearHintsOnPreds(GCPtr<Trail> trail);  

  /* PUBLIC Accessors (Conveniecnce Forms) */
  GCPtr<Type> & Pred(size_t i)
  {
    return (*pred)[i];
  }  
};

typedef Typeclass Constraint; 
typedef TCConstraints Constraints; 

#endif /* TYPECLASS_HXX */

