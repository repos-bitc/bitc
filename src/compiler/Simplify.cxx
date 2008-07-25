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
#include <assert.h>
#include "AST.hxx"
#include "Type.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

// This pass exists to support GC. The idea is to ensure that every
// temporary result has an entry in the stack frame so that it doesn't
// get lost in collection. We do this by "let introduction". That is,
// any time we see an application (including value constructions) such
// as:
//
//   (e1 e2 e3 e4)
//
// we rewrite it (recursively) into:
//
//   (let ((tmp (e1 e2 e3 e4))) tmp)
//
// Doing this naively would turn
//
//    (let ((v (e1 e2))) ...)
//
// into
//
//    (lst ((v (let ((tmp (e1 e2))) tmp))) ...)
//
// so we handle the initializer expression in a let as a special case.
//
// The purpose of this pass is procedure frame construction.  Here is
// an example. Consider:
//
//   (e1 (e2 e3)) =>
//
//   (let ((tmp1 (e1 (let ((tmp2 (e2 e3))) tmp2)))) tmp1) =>
//
//   (make-frame (tmp1 tmp2)
//     ...
//     (begin
//       (set! tmp1 (e1 (begin (set! tmp2 (e2 e3)) tmp2)))
//       tmp1)
//     ...)
//
// which is uglier than sin, but easily compiled (and reordered).
//
// For purposes of this rewrite, string literals are considered
// applications.
//
// NOTE that this pass takes a by-reference argument, and performs
// its re-writing IN PLACE.

static unsigned long ltmpCounter = 0;

static GCPtr<AST> 
LetWrap(GCPtr<AST> ast)
{
  std::stringstream ss;
  ss << "_ltmp" << ltmpCounter++;

  GCPtr<AST> let = new AST(at_let, ast->loc);
  GCPtr<AST> letBindings = new AST(at_letbindings, ast->loc);
  GCPtr<AST> binding = new AST(at_letbinding, ast->loc);
  GCPtr<AST> idPattern = new AST(at_identPattern, ast->loc);
  GCPtr<AST> id = new AST(at_ident, ast->loc);
  GCPtr<AST> useid = new AST(at_ident, ast->loc);

  let->addChild(letBindings);
  let->addChild(useid);
  let->addChild(new AST(at_constraints));

  letBindings->addChild(binding);

  binding->addChild(idPattern);
  binding->addChild(ast);

  idPattern->addChild(id);

  useid->s = id->s = ss.str();
  useid->Flags |=  ID_IS_GENSYM;
  id->Flags |= ID_IS_GENSYM;
  useid->identType = id->identType = id_value;
  assert(!ast->scheme);
  //  useid->scheme = id->scheme = ast->scheme;
  useid->symType = id->symType = ast->symType;
  useid->symbolDef = id;
  useid->fqn.ident = id->fqn.ident = id->s;

  return let;
}

// MakeFrame -- walk a lambda, looking for LET bindings. Hoist the
// locals into a call frame structure, and rewrite the LET using
// BEGIN and SET!.
void
MakeFrame(GCPtr<AST> ast, GCPtr<AST> frameBindings)
{
  switch(ast->astType) {
  case at_let:
    ast->astType = at_begin;
    break;

  case at_letbinding:
    {
      assert(ast->child(0)->astType = at_identPattern);

      // Add the identPattern entry to the frame -- just go ahead and
      // alias it.
      frameBindings->addChild(ast->child(0));

      // Re-write the letbinding as a set!:
      ast->astType = at_setbang;
      ast->symType = ast->child(0)->symType;
    }
  default:
    break;
  }

  // Do the children recursively
  for (size_t i = 0; i < ast->children.size(); i++) {
    MakeFrame(ast->child(i), frameBindings);
  }
}


void
LetInsert(GCPtr<AST> ast, bool skip = false)
{
  switch(ast->astType) {
  case at_apply:
  case at_struct_apply:
  case at_ucon_apply:
    {
      //      bool needRewrite = false;

      // First, figure out if we require a rewrite:
      for (size_t c = 0; c < ast->children.size(); c++) {
	GCPtr<AST> child = ast->child(c);
	LetInsert(child);
      }

      if (!skip)
	ast = LetWrap(ast);

      break;
    }

  case at_letbinding:
    {
      LetInsert(ast->child(1), 
		(ast->child(0)->astType == at_identPattern));
      break;
    }
  default:
    break;
  }

  for (size_t i = 0; i < ast->children.size(); i++) {
    LetInsert(ast->child(i));
  }
}
