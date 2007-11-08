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

using namespace sherpa;

unsigned long tmpNameCounter = 0;

// Alpha rename is currently not being employed

void
AlphaRename(AST *ast)
{
  switch(ast->astType) {
  case at_ident:
    {
      if (ast->isGlobal())
	break;

      if (ast->symbolDef) {	// use occurrence, copy from defining
	assert(!ast->isGlobal());

	if (ast->symbolDef->identFlags & ID_IS_GENSYM) {
	  ast->s = ast->symbolDef->s;
	  ast->identFlags |= ID_IS_GENSYM;
	}
      }
      else {			// defining occurrence
	if (!ast->isGlobal()) {
	  std::stringstream ss;
	  ss << "an_" << tmpNameCounter++;
	  ast->s = ss.str();
	  ast->identFlags |= ID_IS_GENSYM;
	}
      }

      break;
    }

  default:
    break;
  }

  for (size_t i = 0; i < ast->children.size(); i++) {
    AlphaRename(ast->children[i]);
  }
}
