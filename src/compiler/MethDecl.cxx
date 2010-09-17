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

/* In order to successfully polyinstantiate instances that contain
   immediate lambdas, we need to hoist them and give them proper
   names so that the polyinstantiator has something to mangle. */

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include <libsherpa/UExcept.hxx>

#include "AST.hxx"
#include "inter-pass.hxx"

using namespace boost;
using namespace sherpa;
using namespace std;

/// @brief Insert function proclamations for structure and object
/// methods.
///
/// This pass examines structure and object definitions. For every
/// method m in structure/object type S, it introduces a proclaim for
/// a function named S.m that accepts an instance of S as an
/// argument. 
static void
insertMethDecls(shared_ptr<AST> ast, std::ostream& errStream, bool &errFree)
{
  assert(ast->astType == at_interface || ast->astType == at_module);

  // We rely here on the fact that the top-level AST must be
  // at_interface or at_module, and that DEFSTRUCT and DEFOBJECT can
  // only appear as children of these nodes.

  std::vector<shared_ptr<AST> > newChildren;

  for (size_t c = 0; c < ast->children.size(); c++) {
    shared_ptr<AST> theTypeDefn = ast->child(c);
    newChildren.push_back(theTypeDefn);

    if ((theTypeDefn->astType != at_defstruct) &&
        (theTypeDefn->astType != at_defobject))
      continue;

    // Typename might be an at_usesel here, but the parser has set
    // the "s" field in that case.
    shared_ptr<AST> typeName = theTypeDefn->child(0);

    shared_ptr<AST> fields = theTypeDefn->child(4);

    for (size_t f = 0; f < fields->children.size(); f++) {
      shared_ptr<AST> fld = fields->child(f);

      if (fld->astType != at_methdecl)
        continue;

      shared_ptr<AST> methName = fld->child(0);
      shared_ptr<AST> methFnType = fld->child(1)->getDeepCopy();
      methFnType->astType = at_fn;
      shared_ptr<AST> methFnArgs = methFnType->child(0);

      // Assume initially that this is a non-parameterize type. We
      // will fix that in a moment.
      shared_ptr<AST> structArgType = theTypeDefn->child(0)->getDeepCopy();

      // If the structure type is parameterized, it's tvList will be
      // non-empty, and we need to build an at_typeapp node:
      shared_ptr<AST> tvList = theTypeDefn->child(1);
      assert(tvList->astType == at_tvlist);

      if (tvList->children.size()) {
        structArgType = AST::make(at_typeapp, tvList->loc, structArgType);
        for (size_t tv = 0; tv < tvList->children.size(); tv++) {
          shared_ptr<AST> theTV = tvList->child(tv);
          structArgType->addChild(theTV->getDeepCopy());
        }
      }

      // If the structure is an unboxed type, we need to wrap the argument
      // type in a BY-REF. Check child(4):
      if (theTypeDefn->child(4)->astType == at_unboxedCat)
        structArgType = 
          AST::make(at_byRefType, structArgType->child(4)->loc,
                    structArgType);

      // Fetch the (possibly empty) constraint set from the original
      // structure:
      size_t nChildren = theTypeDefn->children.size();
      shared_ptr<AST> constraintSet = theTypeDefn->child(nChildren - 1);

      if (constraintSet->children.size()) {
        // There are constraints. Wrap the AST describing the type in
        // an at_qualtype node:

        structArgType =
          AST::make(at_qualType, constraintSet->loc,
                    constraintSet->getDeepCopy(), structArgType);
      }

      methFnArgs->children.insert(methFnArgs->children.begin(),
                                  structArgType);

      // We should probably build a at_usesel here, but the resolver
      // pass will simply smash that into an identifier anyway, so go
      // ahead and build the identifier directly:

      shared_ptr<AST> methFnIdentNode = 
        AST::make(at_usesel, 
                  methName->loc,
                  typeName->getDeepCopy(),
                  methName->getDeepCopy());

      shared_ptr<AST> mFnDecl = 
        AST::make(at_proclaim, fld->loc, 
                  methFnIdentNode,
                  methFnType,
                  constraintSet->getDeepCopy());

      mFnDecl->isDecl = true;

      // FIX: Not convinced that this is correct:
      mFnDecl->flags |= PROCLAIM_IS_INTERNAL;

      mFnDecl->printVariant = pf_IMPLIED;

      newChildren.push_back(mFnDecl);
    }
  }

  ast->children = newChildren;
}

bool
UocInfo::fe_methDecl(std::ostream& errStream,
                      bool init, unsigned long flags)
{
  DEBUG(METH_DECL) if (isSourceUoc())
    PrettyPrint(errStream);

  DEBUG(BEG_SIMP) std::cerr << "fe_beginSimp" << std::endl;
  bool errFree = true;
  insertMethDecls(uocAst, errStream, errFree);

  DEBUG(METH_DECL) if (isSourceUoc())
    PrettyPrint(errStream);

  return true;
}
