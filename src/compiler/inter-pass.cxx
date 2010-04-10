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
#include <sstream>
#include <string>
#include <map>

#include <libsherpa/UExcept.hxx>

#include "Options.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;

bool
UocInfo::RandT(std::ostream& errStream,
               bool init,
               ResolverFlags rflags,
               TI_Flags tflags,
               std::string mesg)
{
  bool errFree = true;

  CHKERR(errFree, Resolve(errStream, init, rflags, mesg));

  if (errFree)
    CHKERR(errFree, TypeCheck(errStream, init, tflags, mesg));

  if (!errFree)
    errStream << "WHILE R&Ting:" << std::endl
              << uocAst->asString() << std::endl;

  return errFree;
}


void
UocInfo::wrapEnvs()
{
  env = env->newDefScope();
  gamma = gamma->newDefScope();
  instEnv = instEnv->newDefScope();
}

void
UocInfo::unwrapEnvs()
{
  env = env->defEnv;
  gamma = gamma->defEnv;
  instEnv = instEnv->defEnv;
}

// The following is used to R&T any sub-expression

bool
UocInfo::RandTexpr(std::ostream& errStream,
                   shared_ptr<AST> expr,
                   ResolverFlags rflags,
                   TI_Flags tflags,
                   std::string mesg,
                   bool keepResults,
                   shared_ptr<EnvSet> altEnvSet)
{
  bool errFree = true;
  shared_ptr<UocInfo> myUoc = UocInfo::make(shared_from_this());
  myUoc->uocAst = expr;

  if (altEnvSet) {
    myUoc->env = altEnvSet->env;
    myUoc->gamma = altEnvSet->gamma;
    myUoc->instEnv = altEnvSet->instEnv;
  }

  if (!keepResults)
    myUoc->wrapEnvs();

  CHKERR(errFree, myUoc->Resolve(errStream, false, rflags, mesg));
  if (errFree)
    CHKERR(errFree, myUoc->TypeCheck(errStream, false,
                                     tflags, mesg)); 
  
  if (!keepResults)
    myUoc->unwrapEnvs();

  if (!errFree)
    errStream << "WHILE R&Ting:" << std::endl
              << expr->asString() << std::endl;

  return errFree;
}


#define MARKDEF(ast, def) do {\
    assert(def);              \
    ast->defForm = def;              \
  } while (0);
//std::cout << "Marked " << ast->asString() << "->defForm = "
//        << def->asString() << std::endl;


/** @brief For every defining occurrence of an identifier, set up a
 * back pointer to the containing defining form.
 *
 * See the explanation in AST.ast*/
void
UocInfo::findDefForms(shared_ptr<AST> ast, shared_ptr<AST> local, shared_ptr<AST> top)
{
  bool processChildren = true;

  switch(ast->astType) {
  case at_let:
  case at_letrec:
  case at_letStar:
    {
      MARKDEF(ast, top);
      local = ast;
      break;
    }

  case at_letbindings:
    {
      assert(local != top);
      MARKDEF(ast, local);
      local = ast;
      break;
    }

  case at_letbinding:
    {
      MARKDEF(ast, local);
      shared_ptr<AST> id = ast->child(0)->child(0);
      MARKDEF(id, ast);
      break;
    }

  case at_define:
  case at_recdef:
    {
      shared_ptr<AST> id = ast->child(0)->child(0);
      MARKDEF(id, ast);
      top = ast;
      local = ast;
      break;
    }

  case at_declstruct:
  case at_declunion:
  case at_proclaim:
  case at_defstruct:
  case at_defexception:
    {
      shared_ptr<AST> id = ast->child(0);
      MARKDEF(id, ast);
      processChildren = false;
      break;
    }

  case at_defunion:
    {
      shared_ptr<AST> id = ast->child(0);
      MARKDEF(id, ast);
      shared_ptr<AST> ctrs = ast->child(4);
      for (size_t i=0; i < ctrs->children.size(); i++) {
        shared_ptr<AST> ctr = ctrs->child(i);
        shared_ptr<AST> ctrID = ctr->child(0);
        MARKDEF(ctrID, ast);
      }
      processChildren = false;
      break;
    }

  case at_deftypeclass:
    {
      shared_ptr<AST> id = ast->child(0);
      MARKDEF(id, ast);
        
      shared_ptr<AST> methods = ast->child(4);
      for (size_t i = 0; i < methods->children.size(); i++) {
        shared_ptr<AST> method = methods->child(i);
        shared_ptr<AST> mID = method->child(0);
        MARKDEF(mID, ast);
      }
      processChildren = false;
      break;
    }

  case at_do:
    {
      local = ast;
      break;
    }

  case at_dobindings:
    {
      assert(local != top);
      MARKDEF(ast, local);
      local = ast;
      break;
    }

  case at_dobinding:
    {
      MARKDEF(ast, local);
      shared_ptr<AST> id = ast->child(0)->child(0);
      MARKDEF(id, ast);
      break;
    }

  default:
    {
      break;
    }
  }

  if (processChildren)
    for (size_t c=0; c < ast->children.size(); c++)
      findDefForms(ast->child(c), local, top);        
}

/** @brief Make a pass over every AST, setting up back pointers to the
 * containing forms of all defining occurrences. */
void
UocInfo::findAllDefForms()
{
  for (UocMap::iterator itr = UocInfo::srcList.begin();
      itr != UocInfo::srcList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;
    puoci->findDefForms(puoci->uocAst);
  }

  for (UocMap::iterator itr = UocInfo::ifList.begin();
      itr != UocInfo::ifList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;
    puoci->findDefForms(puoci->uocAst);
  }
}

static void
addCandidates(shared_ptr<AST> mod)
{
  for (size_t c = 0; c < mod->children.size(); c++) {
    shared_ptr<AST> ast = mod->child(c);
    shared_ptr<AST> id = ast->getID();
    switch(ast->astType) {
    case at_proclaim: // proclaims needed to keep externalNames
    case at_define:
    case at_recdef:
    case at_defexception:
      if (id->symType->isConcrete())
        Options::entryPts.insert(id->fqn.asString());

      break;

    default:
        break;
    }
  }
}


void
UocInfo::addAllCandidateEPs()
{
  for (UocMap::iterator itr = UocInfo::ifList.begin();
      itr != UocInfo::ifList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;
    addCandidates(puoci->uocAst);
  }

  for (UocMap::iterator itr = UocInfo::srcList.begin();
      itr != UocInfo::srcList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;
    addCandidates(puoci->uocAst);
  }

  //for (size_t c=0; c < Options::entryPts.size(); c++)
  //  std::cerr << "Entry Point: " << Options::entryPts[c] << std::endl;
}

