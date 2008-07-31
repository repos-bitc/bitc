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

#include <stdint.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <sstream>
#include <libsherpa/UExcept.hxx>
#include "UocInfo.hxx"
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

GCPtr<Type> 
obtainFullUnionType(GCPtr<Type> t)
{  
  t = t->getBareType();  
  assert(t->isUType());
  GCPtr<AST> unin = t->myContainer;  
  GCPtr<TypeScheme> uScheme = unin->scheme;
  GCPtr<Type> uType = uScheme->type_instance_copy()->getType();

  assert(uType->kind == ty_unionv || uType->kind == ty_unionr);
  assert(uType->typeArgs.size() == t->typeArgs.size());

  for(size_t c=0; c < uType->typeArgs.size(); c++)
    t->TypeArg(c)->unifyWith(uType->TypeArg(c));
  
  return uType;
}

size_t
nCtArgs(GCPtr<Type> t)
{
  assert(t->isUType() || t->isException());
  t = t->getBareType();
  
  size_t cnt=0;
  for(size_t i=0; i < t->components.size(); i++)
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
	   GCPtr<TSEnvironment > fromEnv, 
	   GCPtr<TSEnvironment > toEnv)
{
  for(TSEnvironment::iterator itr = fromEnv->begin();
      itr != fromEnv->end(); ++itr) {
    GCPtr<Binding<TypeScheme> > bdng = itr->second;

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


void
useIFInsts(const std::string& idName,
	   GCPtr<InstEnvironment >fromEnv, 
	   GCPtr<InstEnvironment >toEnv)
{
  for(InstEnvironment::iterator itr = fromEnv->begin();
      itr != fromEnv->end(); ++itr) {
    GCPtr<Binding<set<GCPtr<Instance> > > > bdng = itr->second;
    
    if (bdng->flags & BF_PRIVATE)
      continue;
    
    std::string s = bdng->nm;
    GCPtr<set<GCPtr<Instance> > > insts = bdng->val;
    
    if (idName.size())
      s = idName + "." + s;
    
    toEnv->addBinding(s, insts);
    toEnv->setFlags(s, BF_PRIVATE|BF_COMPLETE);
  }
}
  
/* Initialize my environment */
bool
initGamma(std::ostream& errStream, 
	  GCPtr<TSEnvironment > gamma,
	  GCPtr<InstEnvironment > instEnv,
	  const GCPtr<AST> topAst, unsigned long uflags)
{
  bool errFree = true;
  // Make sure I am not processing the prelude itself
  // cout << "Processing " << ast->child(1)->child(0)->s 
  //      << std::endl;
  if(topAst->astType == at_interface &&
     topAst->child(0)->s == "bitc.prelude") {
    // cout << "Processing Prelude " << std::endl;
    return true;
  }
  
  // "use" everything in the prelude
  GCPtr<TSEnvironment > preenv = GC_NULL;
  GCPtr<InstEnvironment > preInsts = GC_NULL;
  
  size_t i;

  {
    UocMap::iterator itr = UocInfo::ifList.find("bitc.prelude");
    if (itr == UocInfo::ifList.end()) {
      errStream << topAst->loc << ": "
		<< "Internal Compiler Error. "
		<< "Prelude has NOT been processed till " 
		<< "type inference."
		<< std::endl;
      // GCFIX: Why does this return on error instead of exiting? This
      // is a FATAL compiler errors!
      return false;
    }

    preenv = itr->second->gamma;
    preInsts = itr->second->instEnv;
  }

  if(!preenv || !preInsts) {
    errStream << topAst->loc << ": "
	      << "Internal Compiler Error. "
	      << "Prelude's Gamma is NULL "
	      << std::endl;
    // GCFIX: Why does this return on error instead of exiting? This
    // is a FATAL compiler errors!
    return false;
  }
  
  useIFGamma(std::string(), preenv, gamma);
  useIFInsts(std::string(), preInsts, instEnv);
  return errFree;
}

