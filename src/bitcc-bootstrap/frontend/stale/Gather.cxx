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
#include <assert.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <gmp.h>
#include <errno.h>

#include "Version.hxx"
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "inter-pass.hxx"
#include "Special.hxx"

using namespace sherpa;
using namespace std;
 
static void
fixWithFqns(AST *ast, string sname)
{
  switch(ast->astType) {
  case at_start:
    {
      fixWithFqns(ast->children[0], sname);
      break;
    }

  case at_module:
  case at_interface:
    {
      size_t c = (ast->astType == at_interface) ? 1 : 0;
      for (; c < ast->children.size(); c++) {
	fixWithFqns(ast->children[c], sname);
      }
      break;
    }
  
  case at_field:
    {
      fixWithFqns(ast->children[1], sname);
      break;
    }

  case at_declares:
    break;

  case at_select:
    {
      fixWithFqns(ast->children[0], sname);
      
      if((ast->Flags2 & SEL_FROM_UN_VAL) ||
	 (ast->Flags2 & SEL_FROM_UN_TYPE)) 
	fixWithFqns(ast->children[1], sname);
      
      break;
    }
  
  case at_use_case:
    {
      ast->children[0]->Flags |= ID_IS_GLOBAL;
      ast->children[0]->fqn = ast->children[1]->fqn;
      for (size_t c = 0; c < ast->children.size(); c++)
	fixWithFqns(ast->children[c], sname);      
      break;
    }
    
  case at_ident:
    {      
      AST *def = ast;
      if(ast->symbolDef != NULL)
	def = ast->symbolDef;

      if(def->isGlobal()) {
	ast->s = def->fqn.asString(".", sname);
      }
      break;
    }

  default:
    {
      for (size_t c = 0; c < ast->children.size(); c++) {
	fixWithFqns(ast->children[c], sname);
      }	
      break;
    }
  }
}

void
getAllDefs(AST *last, AST *ast)
{
  switch(ast->astType) {
  case at_start:
    {
      getAllDefs(last, ast->children[0]);
      break;
    }
  case at_module:
  case at_interface:
    {
      size_t c = (ast->astType == at_interface) ? 1 : 0;
      for (; c < ast->children.size(); c++) {
	switch(ast->children[c]->astType) {
	case at_import:
	case at_provide:
	case at_use:
	  break;
	  
	  // FIX: Why isn't this just default?
	case at_definstance:
	  last->children.append(ast->children[c]);
	  break;

	default:
	  last->children.append(ast->children[c]);
	  break;
	}
      }
      break;
    }
  default:
    {
      assert(false);
    }
  }
}

bool
markPreludeDefs(AST *prelude)
{

  AST *iface = prelude->children[0];
  assert(iface->astType == at_interface);
  for(size_t c=1; c < iface->children.size(); c++) {
    AST *ast = iface->children[c]->getID();
    if(ast != NULL) 
      ast->Flags |= DEF_IN_PRELUDE;
  }

  return true;
}


bool
FQNize()
{
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    UocInfo *uoc = UocInfo::ifList[i];
    fixWithFqns(uoc->ast, "");
  }
  
  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    UocInfo *uoc = UocInfo::srcList[i];    
    stringstream ss;
    ss << "src" << i;
    fixWithFqns(uoc->ast, ss.str());    
  }
  return true;
}

static bool 
reRandT(std::ostream &errStream, UocInfo *uoc)
{
   bool errFree = true;
   CHKERR(errFree, uoc->RandT(errStream,  
			 true, OP_SYM_FLAGS, OP_TYP_FLAGS, 
			 "Gather-stage error(s):\n"));
   //if(!errFree)
   //  errStream << " while RandTing " << uoc->ast->asString() << std::endl;
   
  return errFree;
}

bool
buildLinkedAST(std::ostream &errStream, UocInfo *luoc)
{
  bool errFree = true;

  sherpa::LexLoc loc; // internal location
  luoc->ast = new AST(at_start, loc);
  AST *start = luoc->ast;
  AST *verStr = AST::makeStringLit(LToken(loc, BITC_VERSION));
  AST *ver = new AST(at_version, loc, verStr);
  AST *mod = new AST(at_module, loc);
  start->children.append(mod);
  start->children.append(ver);
  
  // First process prelude
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    UocInfo *uoc = UocInfo::ifList[i];
    if(uoc->flags & UOC_IS_PRELUDE) {
      //markAlwaysEmit(uoc);
      getAllDefs(mod, uoc->ast); 
      break;
    }
  }

  // Next all interfaces
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    UocInfo *uoc = UocInfo::ifList[i];
    if(uoc->flags & UOC_IS_PRELUDE) 
      continue;
    
    getAllDefs(mod, uoc->ast);
  }

  // Next all source units.
  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    UocInfo *uoc = UocInfo::srcList[i];    
    getAllDefs(mod, uoc->ast);
  }

  CHKERR(errFree, reRandT(errStream, luoc));
  //BitcP(std::cout, start, false);
  //DisplayTypes(std::cout, luoc);
  return errFree;
}


#if 0
bool 
ensureDefnsFound(std::ostream &errStream, AST *start)
{
  bool errFree = true;
  AST *mod = start->children[0];
  
  for(size_t c = 0; c < mod->children.size(); c++) {
    AST *ast = mod->children[c];
    switch(ast->astType) {
    case at_declstruct:
    case at_declunion:    
    case at_declValue:
      {
	AST *def = ast->getID();
	if(def->defn == NULL && 
	   ((def->Flags & DEF_IS_EXTERNAL) == 0)) {
	  // Declaration Found, No definition.
	  errStream << ast->loc.asString()
		    << ": non-external symbol " << def->s 
		    << " proclaimed here, but is never defined."
		    << endl;
	  errFree = false;
	}
	break;
      }
    
    default:
      break;    
    }
  }
  return errFree;
}
#endif

bool
UocInfo::be_gather(std::ostream &errStream, 
		   bool init, unsigned long flags)
{
  bool errFree = true;
  
  UocInfo *prelude = NULL;
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    UocInfo *uoc = UocInfo::ifList[i];
    if(uoc->flags & UOC_IS_PRELUDE) {
      prelude = uoc;
      break;
    }
  }

  assert(prelude != NULL);
  AST *preludeAST = prelude->ast;

  CHKERR(errFree, markPreludeDefs(preludeAST));
  CHKERR(errFree, FQNize());
  SpecialNames::spNames.fixUpSpNames(prelude);
  CHKERR(errFree, buildLinkedAST(errStream, this));
  
  if(!ast->isValid()) {
    errStream << "PANIC: Invalid AST built for LINKED AST"
	      << "Please report this problem.\n"; 
    errStream << ast->asString() << std::endl;
    exit(1);
  }
  markDefForms(ast);
  
#if 0
  CHKERR(errFree, ensureDefnsFound(errStream, uoc->ast)); 
#endif

#if 0
  if((Options::backEnd->flags & BK_HDR_MODE) == 0) 
    CHKERR(errFree, checkForMain(errStream, uoc));
#endif
  
  return errFree;
}

