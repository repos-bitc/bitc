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
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/avl.hxx>
#include <assert.h>
#include "AST.hxx"
#include "Type.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

// Return whether the expression returns a location or not.
static bool
LocChk(std::ostream &errStream, bool &errFree, GCPtr<AST> ast, bool inSET)
{
  switch (ast->astType) {
  case at_ident:
    {
      return true;
    }
    
    //case at_array:
    //case at_vector:
  case at_vector_nth:
  case at_deref:
    {
      for (size_t c = 0; c < ast->children->size(); c++)
	LocChk(errStream, errFree, ast->child(c), false);
       
      return true;
    }

  case at_setbang:
    {
      bool isLoc = LocChk(errStream, errFree, ast->child(0), true);
      LocChk(errStream, errFree, ast->child(1), inSET);

      if(!isLoc) {
	errStream << ast->child(0)->loc << ": "
		  << "Non-location in set! context"
		  << std::endl;
	errFree = false;
      }

      return false;
    }

  case at_array_nth:
    {
      bool isLoc = LocChk(errStream, errFree, ast->child(0), inSET);
      if(inSET && !isLoc) {
	errStream << ast->child(0)->loc << ": "
		  << "Non-location in set! context"
		  << std::endl;
	errFree = false;
      }

      return true;
    }

  case at_fqCtr:
    {
      return true;
    }

  case at_select:
    {
      bool isLoc = LocChk(errStream, errFree, ast->child(0), inSET);
      
      if(inSET && !isLoc && !ast->child(0)->symType->isRefType()) {
	errStream << ast->child(0)->loc << ": "
		  << "Non-location in set! context"
		  << std::endl;
	errFree = false;
      }
      
      return true;
    }

  case at_sel_ctr:
    {
      bool isLoc = LocChk(errStream, errFree, ast->child(0), inSET);
      return false;
    }
    
  default:
    {
      for (size_t c = 0; c < ast->children->size(); c++)
	LocChk(errStream, errFree, ast->child(c), inSET);

      return false;
    }
  }
}

bool
UocInfo::fe_locCheck(std::ostream& errStream, 
		     bool init, unsigned long flags)
{
  bool errFree = true;  
  LocChk(errStream, errFree, ast, false);  
  return errFree;
}

