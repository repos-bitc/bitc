#ifndef TYPECLASS_HXX
#define TYPECLASS_HXX

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

#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <set>

#include "AST.hxx"
#include "Type.hxx"

struct TypeScheme;
struct TCConstraints;

typedef Type Typeclass;
typedef Typeclass Constraint; 
typedef TCConstraints Constraints; 

struct Instance : public sherpa::Countable {
  sherpa::GCPtr<TypeScheme> ts;
  sherpa::GCPtr<AST> ast;
  
  Instance(sherpa::GCPtr<TypeScheme> _ts, sherpa::GCPtr<AST>_ins)
  {
    ts = _ts;
    ast = _ins;
  }
  
  bool equals(std::ostream &errStream, sherpa::GCPtr<Instance> ins, 
	      sherpa::GCPtr<const InstEnvironment >
	      instEnv) const;
  bool satisfies(std::ostream &errStream, sherpa::GCPtr<Typeclass> pred, 
		 sherpa::GCPtr<const InstEnvironment >
		 instEnv) const;
  std::string asString();

  std::string asXML();
  void asXML(sherpa::INOstream &out);
};


/* Type class constraints */
 
struct TCConstraints : public sherpa::Countable {
  // Type class predicates
  std::set<sherpa::GCPtr<Typeclass> > pred;
  
  typedef std::set<sherpa::GCPtr<Typeclass> >::iterator iterator;

  TCConstraints()
  {
  }

  bool empty() const {
    return pred.empty();
  }

  size_t size() const {
    return pred.size();
  }

  void addPred(sherpa::GCPtr<Typeclass> tc);
  void clearPred(sherpa::GCPtr<Constraint> ct);

  void normalize();
  
  bool contains(sherpa::GCPtr<Typeclass> tc);

  void collectAllFnDeps(std::set<sherpa::GCPtr<Type> >& fnDeps);
  
  // Compute the closure of all functional dependencies 
  // supplied in the vector
  static void close(std::set<sherpa::GCPtr<Type> >& closure,
		    const std::set<sherpa::GCPtr<Type> >& fnDeps);
  void clearHintsOnPreds(sherpa::GCPtr<Trail> trail);  

  iterator begin() {
    return pred.begin();
  }
  iterator end() {
    return pred.end();
  }
};


#endif /* TYPECLASS_HXX */

