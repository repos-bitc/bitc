#ifndef TYPESCHEME_HXX
#define TYPESCHEME_HXX

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

#include "AST.hxx"
#include "Type.hxx"
#include "Typeclass.hxx"
#include "INOstream.hxx"

enum GeneralizeMode {gen_instance=0, gen_top=1, gen_local=2, 
		     gen_Hinstance=3, gen_Htop=4, gen_Hlocal=5};

struct TypeScheme : public sherpa::Countable {
  
  sherpa::GCPtr<Type> tau;
  sherpa::GCPtr<AST> ast; // Need to maintained the official version here,
                  // Type pointers get linked, typeschemes don't
  sherpa::GCPtr<sherpa::CVector<sherpa::GCPtr<Type> > > ftvs;
  
  // Type class constraints 
  // Note: Leave this as a pointer, not an embedded vector.
  // This helps tcc sharing for definitions that are 
  // defined within the same letrec, etc.
  sherpa::GCPtr<TCConstraints> tcc;

  // The constructor
  TypeScheme(sherpa::GCPtr<Type> _tau, sherpa::GCPtr<AST> _ast,
	     sherpa::GCPtr<TCConstraints> _tcc = NULL);
  
  // The generalizer
  bool generalize(std::ostream& errStream,
		  const sherpa::LexLoc &errLoc,
		  sherpa::GCPtr<const TSEnvironment > gamma,
		  sherpa::GCPtr<const InstEnvironment >instEnv, 
		  sherpa::GCPtr<const AST> expr, 
		  sherpa::GCPtr<TCConstraints >parentTCC,
		  sherpa::GCPtr<Trail> trail,
		  GeneralizeMode gen);
  
  // The function that actually makes a copy of the 
  // contained type. This function calls TypeSpecialize.
  // This function is called by type_instance()
  // and getDCopy() function in Type class.
  sherpa::GCPtr<Type> type_instance_copy();
  
  // Type Instance, if there are no ftvs, returns the original tau  
  sherpa::GCPtr<Type> type_instance();
  
  // Type scheme's instance (including TC constraints)
  sherpa::GCPtr<TypeScheme> ts_instance(bool copy=false);
  sherpa::GCPtr<TypeScheme> ts_instance_copy();
  
  // Read carefully:
  // Appends constraints that corrrespond to at least one
  // free variable in this scheme's ftvs to _tcc
  void addConstraints(sherpa::GCPtr<TCConstraints> _tcc) const;
  // or ones that correspond to any of the added predicates
  void TransAddConstraints(sherpa::GCPtr<TCConstraints> _tcc) const;
  
  //sherpa::GCPtr<Type> type_copy();
  std::string asString(sherpa::GCPtr<TvPrinter> tvP=new TvPrinter, 
		       bool norm=false);

  void asXML(sherpa::GCPtr<TvPrinter> tvP, INOstream &out);
  std::string asXML(sherpa::GCPtr<TvPrinter> tvP = new TvPrinter);
  
  // Collect all tvs wrt tau, and tcc->pred, but NOT tcc->fnDeps
  void collectAllFtvs();
  void collectftvs(sherpa::GCPtr<const TSEnvironment > gamma);
  bool removeUnInstFtvs();
  bool normalizeConstruction(sherpa::GCPtr<Trail> trail);

  bool solvePredicates(std::ostream &errStream,
		       const sherpa::LexLoc &errLoc,
		       sherpa::GCPtr<const InstEnvironment > instEnv,
		       sherpa::GCPtr<Trail> trail);
  
  bool checkAmbiguity(std::ostream &errStream, const sherpa::LexLoc &errLoc);
  bool migratePredicates(sherpa::GCPtr<TCConstraints> parentTCC);    
  
  // In the presence of PCSTs, the substitution within type schemes is
  // not capture avoiding. Therefore, there might be some
  // substitutions into the free type-variables. If so, they can be
  // removed from the list of type variables.
  // Returns true if any changes to the type scheme are made.
  // This also removes redundant constraints, or PCSTs that are
  // satisfied and no longer need to be present.
  bool normalize();

  /* FIX: THIS MUST NEVER BE USED IN lhs OF ASSIGNMENT! */
  /* PUBLIC Accessors (Convenience Forms) */
  sherpa::GCPtr<Type> & Ftv(size_t i)
  {
    return (*ftvs)[i];
  }  
  sherpa::GCPtr<Type>  Ftv(size_t i) const
  {
    sherpa::GCPtr<Type> t = (*ftvs)[i];
    return t;
  }  
};

#endif /* TYPESCHEME_HXX */


