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

/* In order to successfully polyinstantiate instances that contain
   immediate lambdas, we need to hoist them and give them proper
   names so that the polyinstantiator has something to mangle. */

#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/avl.hxx>
#include <assert.h>
#include "AST.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

// Remove any wrapping BEGIN that has just one child.
static GCPtr<AST> 
beginSimp(GCPtr<AST> ast, std::ostream& errStream, bool *stillOK)
{
  for (size_t c = 0; c < ast->children->size(); c++)
    ast->child(c) = beginSimp(ast->child(c), errStream, stillOK);

  if (ast->astType == at_begin) {
    for (size_t c = 0; c < ast->children->size(); c++) {
      if (ast->child(c)->astType == at_define) {
	GCPtr<AST> def = ast->child(c);
	GCPtr<AST> letBinding = new AST(at_letbinding, def->loc, 
				  def->child(0), def->child(1));
	letBinding->Flags |= LB_REC_BIND;

	// The definition is not a global
	def->child(0)->child(0)->Flags &= ~ID_IS_GLOBAL;
	GCPtr<AST> body = new AST(at_begin, ast->child(c)->loc);

	for (size_t bc = c+1; bc < ast->children->size(); bc++)
	  body->addChild(ast->child(bc));

	if (body->children->size() == 0) {
	  // Error: define at end of BEGIN form
	}

	// Trim the remaining children of this begin:
	// while (ast->children->size() > c+1)
	//   ast->children->Remove(ast->children->size()-1);
	GCPtr<CVector<GCPtr<AST> > > newChildren = new CVector<GCPtr<AST> >;
	for(size_t i=0; i <= c; i++)
	  newChildren->append(ast->child(i));
	ast->children = newChildren;

	// Insert the new letrec:
	GCPtr<AST> theLetRec = 
	  new AST(at_letrec, def->loc, 
		  new AST(at_letbindings, def->loc, letBinding),
		  body,
		  def->child(2));
	ast->child(c) = beginSimp(theLetRec, errStream, stillOK);

	// Stop processing this begin:
	break;
      }
    }
  }

  if (ast->astType == at_begin && ast->children->size() == 1)
    return ast->child(0);

  return ast;
}

bool
UocInfo::fe_beginSimp(std::ostream& errStream,
		      bool init, unsigned long flags)
{ 
  BEG_SIMP_DEBUG if (isSourceUoc())
    PrettyPrint(errStream);
  
  BEG_SIMP_DEBUG std::cerr << "fe_beginSimp" << std::endl;
  bool stillOK = true;
  uocAst = beginSimp(uocAst, errStream, &stillOK);

  BEG_SIMP_DEBUG if (isSourceUoc())
    PrettyPrint(errStream);

  return true;
}
