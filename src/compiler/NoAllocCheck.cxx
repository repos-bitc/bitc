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

#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "inter-pass.hxx"

using namespace boost;
using namespace sherpa;

// Return whether the expression returns a location or not.
static bool
AllocCheck(std::ostream &errStream, shared_ptr<AST> ast)
{
  bool errFree = true;

  switch (ast->astType) {
  case at_vector:
  case at_makevectorL:
    errStream << ast->loc << ": "
	      << "Expression requires dynamic allocation. "
	      << "Disallowed in NO-GC mode"
	      << std::endl;
    errFree = false;
    break;

  case at_struct_apply:
  case at_ucon_apply:
    if(ast->child(0)->symType->isRefType()) {
      errStream << ast->loc << ": "
		<< "Expression requires dynamic allocation. "
		<< "Disallowed in NO-GC mode"
		<< std::endl;
      errFree = false;
    }
    break;
    
  default:
    break;
  }
  
  for (size_t c = 0; c < ast->children.size(); c++)
    CHKERR(errFree, AllocCheck(errStream, ast->child(c)));
  
  return errFree;	   
}

bool
UocInfo::fe_noAllocCheck(std::ostream& errStream, 
			 bool init, unsigned long flags)
{
  /* This pass only recognizes syntactic structures that perform
     memory alocation. The requirement not performing closure
     conversion is checked in the clconv pass */

  bool errFree=true;

  if(Options::noAlloc)
    CHKERR(errFree, AllocCheck(errStream, uocAst));  
  return errFree;
}

