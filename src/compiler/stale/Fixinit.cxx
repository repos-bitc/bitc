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
#include "AST.hxx"
#include "Environment.hxx"
#include "Symtab.hxx"
#include "Unify.hxx"
#include "inter-pass.hxx"
#include <sstream>
 
using namespace sherpa;

bool
isTrivialInit(AST *ast)
{
  switch(ast->astType) {
    
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_unit:
  case at_lambda:
    {
      return true;
    }

  case at_ident:
    {
      return ast->symType->isAtomic();
    }

  case at_tqexpr:
    {
      return isTrivialInit(ast->children[0]);
    }
    
    //case at_pair:
    //case at_cpair:
    //case at_array:
    //case at_struct_apply:
    //case at_ucon_apply: 
  case at_begin:
    {
      for(size_t c = 0; c < ast->children.size(); c++)
	if(!isTrivialInit(ast->children[c]))
	  return false;
      return true;
    }

  default:
    return false;
  }
}

AST *
FixDefs(AST *mod)
{
  AST *newMod = new AST(mod->astType, mod->loc);
  for(size_t c = 0; c < mod->children.size(); c++) {    
    AST *ast = mod->children[c];
    switch(ast->astType) {
    case at_define:
      {
	AST *expr = ast->children[1];
	if(expr->astType != at_lambda) {
	  if(!isTrivialInit(expr)) {
	    
	    // While the following was true, the closure conversion
	    // also introduces a new proclaimation for everu top level
	    // definition, and this this one bacame redundant. 
	    //// With the new closure conversion scheme, a definition's
	    //// use of itself is no longer contained. So, I need to
	    //// emit a proclaim so that the initializer can use it. No
	    //// need to C-emit it.
	    //AST *defID = ast->children[0]->children[0];
	    //AST *proclaim = new AST(at_proclaim, ast->loc,
	    //			      defID->Use(),
	    //                        defID->symType->asAST(ast->loc),
	    //                        new AST(at_constraints,
	    //		                      ast->loc));
	    //newMod->children.append(proclaim);
	    //proclaim->Flags |= DEF_IS_INIT_AUX;
	    //ast->Flags2 |= PROCLAIM_IS_INTERNAL;

	    // Now build the AST that actually initializes the 
	    // value. This is called from C code only.
	    AST *id = AST::genSym(ast, "init");
	    AST *ip = new AST(at_identPattern, id->loc, id);
	    AST *argVec = new AST(at_argVec, ast->loc);
	    AST *lam = new AST(at_lambda, ast->loc, 
				     argVec, expr);
	    AST *initDef = new AST(at_define, ast->loc, ip, lam);
	    initDef->addChild(new AST(at_constraints));
	    newMod->children.append(initDef);
	    
	    // For now, have a definition that does the APPLY to
	    // globally initialize the value, but mark it
	    // auxiliary. The code generator will move all these
	    // application to the real main() routine.  I don't want
	    // to do it as an AST transform as that will mean that I
	    // can no longer RandT.
	    
	    AST *idUse = id->getDCopy();
	    AST *app = new AST(at_apply, ast->loc, idUse);
	    ast->children[1] = app;
	    ast->Flags |= DEF_IS_INIT_AUX;
	    app->Flags2 |= APP_IS_VALUE;
	    app->Flags2 |= APP_NATIVE_FNXN;
	    newMod->children.append(ast);
	  }
	  else {
	    if(expr->astType == at_begin) {
	      // If this is a begin block containing trivial 
	      // (i.e. non=side-affecting) contents, then 
	      // it can be replaced by the finall expression
	      ast->children[1] = expr->children[expr->children.size() - 1];	      
	    }
	    newMod->children.append(ast);
	  }
	}
	else {
	  newMod->children.append(ast);
	}
	break;
      }
    default:
      newMod->children.append(ast);
      break;
    }
  }
  return newMod;
}

bool
fixinit(std::ostream& errStream, UocInfo *uoc)
{
  bool errFree = true;  
  AST *mod = uoc->ast->children[0];
  uoc->ast->children[0] = FixDefs(mod);
  
  return errFree;
}


bool
UocInfo::be_fix4c(std::ostream& errStream,
		  bool init, unsigned long flags)
{
  bool errFree = true;
  UocInfo *uoc = &(UocInfo::linkedUoc);
  assert(uoc != NULL);

  CHKERR(errFree, fixinit(errStream, uoc));  

  errStream << "Fixinit AST = " << std::endl
           << uoc->ast->asString() << std::endl;

  CHKERR(errFree, RandT(errStream, uoc, true, 			
  			CL_SYM_FLAGS, CL_TYP_FLAGS));
  
  return errFree;
}
