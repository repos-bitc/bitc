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

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "Eliminate.hxx"

using namespace std;
using namespace sherpa;

bool 
TCConstraints::contains(GCPtr<Typeclass> tc) 
{
  for(iterator itr = begin(); itr != end(); ++itr)
    if((*itr)->strictlyEquals(tc, false, true))
      return true;

  return false;
}
 
void 
TCConstraints::addPred(GCPtr<Typeclass> tc) 
{
  for(iterator itr = begin(); itr != end(); ++itr) {
    if((*itr)->strictlyEquals(tc, false, true)) {
      if(tc->flags & TY_CT_SUBSUMED)
	(*itr)->flags |= TY_CT_SUBSUMED;
      return;
    }  
  }

  pred.insert(tc);
}

void 
TCConstraints::clearPred(GCPtr<Constraint> ct) 
{
  ct =  ct->getType();
  for(iterator itr = begin(); itr != end(); ++itr) {
    GCPtr<Constraint> pr = (*itr)->getType();
    if(pr == ct) {
      pred.erase(itr);
      return;
    }
  }
}

void 
TCConstraints::normalize() 
{
  set<GCPtr<Typeclass> > allPreds = pred;
  pred.clear();
  
  for(iterator itr = allPreds.begin(); itr != allPreds.end(); ++itr)
    addPred((*itr));
}

