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

/// @file REPR simplification pass.
///
/// For almost all purposes in the compiler, a DEFREPR form can be
/// treated as a discriminated union form. This pass converts a
/// DEFREPR into a closely corresponding DEFUNION, leaving a marker
/// behind to indicate that it was really a DEFREPR.

#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <errno.h>
#include "Version.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Environment.hxx"
#include "inter-pass.hxx"
#include "backend.hxx"

using namespace boost;
using namespace sherpa;

shared_ptr<AST>
reprXform(shared_ptr<AST> ast, std::ostream& errStream, bool &errFree)
{
  switch(ast->astType) {
  case at_defrepr:
    {
      shared_ptr<AST> unin = AST::make(ast, false);

      unin->astType = at_defunion;
      unin->flags |= UNION_IS_REPR;
      unin->addChild(ast->child(0)); // identifier
      unin->child(0)->flags |= UNION_IS_REPR;
      unin->addChild(AST::make(at_tvlist, ast->loc)); // empty tvlist
      unin->addChild(ast->child(1));  // category
      unin->addChild(reprXform(ast->child(2), errStream, errFree));  // declares
      unin->addChild(reprXform(ast->child(3), errStream, errFree)); // constructors
      unin->addChild(AST::make(at_constraints, ast->loc)); // constraints
      ast = unin;

      break;
    }

  case at_reprctrs:
    {
      shared_ptr<AST> ctrs = AST::make(at_constructors, ast->loc);
      for (size_t c=0; c < ast->children.size(); c++)
	ctrs->addChild(reprXform(ast->child(c), errStream, errFree));
      ast = ctrs;
      break;
    }

  case at_reprctr:
    {
      shared_ptr<AST> ctr = ast->child(0);

      for (size_t i=1; i < ast->children.size(); i++) {
	shared_ptr<AST> where = ast->child(i);
	bool found = false;

	for (size_t j=1; j < ctr->children.size(); j++) {
	  shared_ptr<AST> fld = ctr->child(j);
		
	  if (where->child(0)->s == fld->child(0)->s) {
	    found = true;
	
	    if (fld->flags & FLD_IS_DISCM) {
	      errStream << where->loc << ": "
			<< " Duplicate `where' label for "
			<< where->child(0)->s
			<< std::endl;
	
	      errFree = false;
	      break;
	    }
	
	    fld->flags |= FLD_IS_DISCM;	
	    fld->unin_discm = (size_t)(where->child(1)->litValue.i.as_uint64());
	  }
	}

	if (!found) {
	  errStream << where->loc << ": "
		    << " Unknown Field: "
		    << where->child(0)->s
		    << std::endl;
	
	  errFree = false;
	}
      }

      ast = ctr;
      break;
    }

  case at_declrepr:
    {
      shared_ptr<AST> unin = AST::make(ast, false);

      unin->astType = at_declunion;
      unin->flags |= UNION_IS_REPR;
      unin->addChild(ast->child(0)); // identifier
      unin->addChild(AST::make(at_tvlist)); // empty tvlist
      unin->addChild(ast->child(1));  // category
      unin->addChild(AST::make(at_tvlist, ast->loc)); // constraints
      ast = unin;
      break;
    }

  default:
    {
      // value definitions are ignored
      for (size_t c=0; c < ast->children.size(); c++)
	ast->child(c) = reprXform(ast->child(c), errStream, errFree);
      break;
    }
  }
  return ast;
}

bool
UocInfo::fe_reprSimp(std::ostream& errStream,
		     bool init, unsigned long flags)
{
  bool errFree = true;
  uocAst = reprXform(uocAst, errStream, errFree);
  return errFree;
}

