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

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

static void
cl_HoistInstLam(GCPtr<UocInfo> uoc)
{
  std::vector<GCPtr<AST> > outAsts;

  GCPtr<AST> modOrIf = uoc->uocAst;

  for (size_t c = 0;c < modOrIf->children.size(); c++) {
    GCPtr<AST> child = modOrIf->child(c);

    if (child->astType == at_definstance) {
      GCPtr<AST> methods = child->child(1);

      // FIX: This is **utterly failing** to hoist the constraints, so
      // any constraint that is not on an instantiated variable will
      // not do the right thing.
      for (size_t m = 0; m < methods->children.size(); m++) {
	GCPtr<AST> meth = methods->child(m);
	if (meth->astType != at_ident) {
	  // It's an expression. Need to hoist it into a new binding.

	  // FIX: redef or define?
	  GCPtr<AST> newDef = new AST(at_define, meth->loc);

	  GCPtr<AST> lamName = AST::genSym(meth, "lam");
	  lamName->identType = id_value;
	  lamName->Flags |= ID_IS_GLOBAL;

	  GCPtr<AST> lamPat = new AST(at_identPattern, meth->loc, lamName);
	  newDef->addChild(lamPat);
	  newDef->addChild(meth);
	  newDef->addChild(new AST(at_constraints));

	  outAsts.push_back(newDef);

	  GCPtr<AST> instName = lamName->Use();
	  GCPtr<AST> the = new AST(at_tqexpr);
	  the->addChild(instName);
	  the->addChild(meth->symType->asAST(meth->loc));

	  meth = the;
	}
	methods->child(m) = meth;
      }
    }

    outAsts.push_back(child);
  }
  
  modOrIf->children = outAsts;
}

#if 0
bool
UocInfo::be_HoistInstLam(std::ostream& errStream,
			 bool init, unsigned long flags)
{ 
  bool errFree = true;

  GCPtr<AST> &ast = UocInfo::linkedUoc.ast;

  ILH_DEBUG if (isSourceUoc)
    BitcPP(errStream, &UocInfo::linkedUoc);

  ILH_DEBUG std::cerr << "cl_HoistInstLam" << std::endl;
  cl_HoistInstLam(&UocInfo::linkedUoc);

  ILH_DEBUG if (isSourceUoc)
    BitcPP(errStream, &UocInfo::linkedUoc);

  ILH_DEBUG std::cerr << "RandT" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, RandT(errStream, &UocInfo::linkedUoc, true, POLY_SYM_FLAGS, POLY_TYP_FLAGS));
  assert(errFree);

  return true;
}
#else
bool
UocInfo::fe_HoistInstLam(std::ostream& errStream,
			 bool init, unsigned long flags)
{ 
  bool errFree = true;

  ILH_DEBUG if (isSourceUoc())
    PrettyPrint(errStream);

  ILH_DEBUG std::cerr << "cl_HoistInstLam" << std::endl;
  cl_HoistInstLam(this);

  ILH_DEBUG if (isSourceUoc())
    PrettyPrint(errStream);

  ILH_DEBUG std::cerr << "RandT" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, RandT(errStream, true, PI_SYM_FLAGS, PI_TYP_FLAGS));
  assert(errFree);

  return true;
}
#endif
