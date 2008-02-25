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
#include <iostream>
#include <string>
#include <sstream>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "TypeInfer.hxx"
#include "TypeInferUtil.hxx"

using namespace sherpa;
using namespace std;

/**************************************************************/
/*                     Some Helper Functions                  */
/**************************************************************/

//#define VERBOSE

GCPtr<Type> 
obtainFullUnionType(GCPtr<Type> t)
{  
  t = t->getBareType();  
  assert(t->isUType());
  GCPtr<AST> unin = t->myContainer;  
  GCPtr<TypeScheme> uScheme = unin->scheme;
  GCPtr<Type> uType = uScheme->type_instance_copy()->getType();

  assert(uType->kind == ty_unionv || uType->kind == ty_unionr);
  assert(uType->typeArgs->size() == t->typeArgs->size());

  for(size_t c=0; c < uType->typeArgs->size(); c++)
    t->TypeArg(c)->unifyWith(uType->TypeArg(c));
  
  return uType;
}

size_t
nCtArgs(GCPtr<Type> t)
{
  assert(t->isUType() || t->isException());
  t = t->getBareType();
  
  size_t cnt=0;
  for(size_t i=0; i < t->components->size(); i++)
    if((t->CompFlags(i) & COMP_UNIN_DISCM) ==0)
      cnt++;

  return cnt;
}



/**************************************************************/
/*                    Environment handling                    */
/**************************************************************/

/* Use all bindings in the some other environment */
void
useIFGamma(const std::string& idName,
	   GCPtr<Environment<TypeScheme> > fromEnv, 
	   GCPtr<Environment<TypeScheme> > toEnv)
{
  for (size_t i = 0; i < fromEnv->bindings->size(); i++) {
    GCPtr<Binding<TypeScheme> > bdng = fromEnv->bindings->elem(i);

    if (bdng->flags & BF_PRIVATE)
      continue;

    std::string s = bdng->nm;
    GCPtr<TypeScheme> ts = bdng->val;

    if (idName.size())
      s = idName + "." + s;

    toEnv->addBinding(s, ts);
    toEnv->setFlags(s, BF_PRIVATE|BF_COMPLETE);
  }  
}

/* Use all instances from some other environment. Unlike Type
   environmen, this one needs explicit checking to make sure there are
   no collisions */

bool
useIFInsts(std::ostream &errStream,
	   LexLoc &errLoc,
	   GCPtr<Environment< CVector<GCPtr<Instance> > > >fromEnv, 
	   GCPtr<Environment< CVector<GCPtr<Instance> > > >toEnv,
	   unsigned long uflags)
{
  bool errFree = true;
  for (size_t i = 0; i < fromEnv->bindings->size(); i++) {
    GCPtr<Binding< CVector<GCPtr<Instance> > > >bdng =
      fromEnv->bindings->elem(i);
    
    if (bdng->flags & BF_PRIVATE)
      continue;
    
    std::string s = bdng->nm;
    GCPtr<CVector<GCPtr<Instance> > > fromInsts = bdng->val;
    GCPtr<CVector<GCPtr<Instance> > > toInsts = toEnv->getBinding(s); 

    if(!toInsts) {
      toInsts = new CVector<GCPtr<Instance> >;
      for (size_t j = 0; j < fromInsts->size(); j++)
	toInsts->append(fromInsts->elem(j));
      toEnv->addBinding(s, toInsts);
    }
    else {
      for (size_t j = 0; j < fromInsts->size(); j++) {
	size_t k;
	bool mustAppend = true;
	GCPtr<Instance> fromInst = fromInsts->elem(j);

	for (k = 0; k < toInsts->size(); k++) {
	  GCPtr<Instance> toInst = toInsts->elem(k);
	  
	  if(toInst == fromInst) {
	    // We are seeing the same AST as a matter of import from
	    // different interfaces. For example: diamond import,
	    // or, Interface X imports prelude, obviously, and
	    // exports all interfaces in the prelude. I have also
	    // already seen the prelude. So, this need not be
	    // re-considered, but this is not an error.
	    // It is safe to ignore this because we are thinking of
	    // the *same* instance defined at the same *AST* and	      
	    // thus, there is no conflict.
	    mustAppend = false;
	    break;
	  }
	  
	  if((uflags & ALL_INSTS_OK) == 0)
	    if(toInst->equals(errStream, fromInst, toEnv)) {
	      errStream << errLoc << ": "
			<< "Conflict in Instance declarations "
			<< " of type " 
			<< toInsts->elem(k)->asString()
			<< " defined at " 
			<< toInsts->elem(k)->ast->loc << " and " 
			<< fromInsts->elem(j)->ast->loc << "."
			<< std::endl;
	      errFree = false;
	      mustAppend = false;	    
	      break;				  
	    }
	}
	  
	if(mustAppend) 
	  toInsts->append(fromInsts->elem(j));
      }
    }
  }
  
  return errFree;
}
  
/* Initialize my environment */
bool
initGamma(std::ostream& errStream, 
	  GCPtr<Environment<TypeScheme> > gamma,
	  GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	  const GCPtr<AST> ast, unsigned long uflags)
{
  bool errFree = true;
  // Make sure I am not processing the prelude itself
  // cout << "Processing " << ast->child(1)->child(0)->s 
  //      << std::endl;
  if(ast->child(0)->astType == at_interface &&
     ast->child(0)->child(0)->s == "bitc.prelude") {
    // cout << "Processing Prelude " << std::endl;
    return true;
  }
  
  // "use" everything in the prelude
  GCPtr<Environment<TypeScheme> > preenv = 0;
  GCPtr<Environment< CVector<GCPtr<Instance> > > > preInsts = 0;
  
  size_t i;

  for(i=0; i < UocInfo::ifList->size(); i++) {
    if(UocInfo::ifList->elem(i)->uocName == "bitc.prelude") {
      preenv = UocInfo::ifList->elem(i)->gamma;
      preInsts = UocInfo::ifList->elem(i)->instEnv;
      break;
    }
  }

  if(i == UocInfo::ifList->size()) {
    errStream << ast->loc << ": "
	      << "Internal Compiler Error. "
	      << "Prelude has NOT been processed till " 
	      << "type inference."
	      << std::endl;
    return false;
  }
  
  if(!preenv || !preInsts) {
    errStream << ast->loc << ": "
	      << "Internal Compiler Error. "
	      << "Prelude's Gamma is NULL "
	      << std::endl;
    return false;
  }
  
  useIFGamma(std::string(), preenv, gamma);
  LexLoc internalLocation;
  CHKERR(errFree, useIFInsts(errStream, internalLocation, 
			     preInsts, instEnv, uflags));
  return errFree;
}


