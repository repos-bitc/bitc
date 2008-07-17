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
#include <sstream>
#include <string>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/avl.hxx>
#include <assert.h>
#include "AST.hxx"
#include "Type.hxx"
#include "TvPrinter.hxx"
#include "backend.hxx"
#include "inter-pass.hxx"
#include "Symtab.hxx"
#include "Unify.hxx"
#include "Special.hxx"

using namespace sherpa;

#define SHAPDEBUG if(0)

// Set to true if you want to hoist all lambdas unconditionally
#define HOISTALL false

// Set to true if you want to build procedure objects for all lambdas
// unconditionally.
#define CLOSEALL false

#define NULL_MODE  0x0u
#define LOCAL_MODE 0x2u  // Parameters
#define USE_MODE   0x3u
#define TYPE_MODE  0x4u
 
static void
markRecBound(AST *ast)
{
  for(size_t c=0; c < ast->children.size(); c++)
    markRecBound(ast->children[c]);
  if (ast->astType == at_ident)
    ast->Flags2 |= ID_IS_RECBOUND;
}

/**
 * @brief Mark all defining occurrences that are closed over so that
 * we can later rewrite them.
 */
static void
clearusedef(AST *ast)
{
  ast->Flags2 &= ~(ID_IS_DEF|ID_IS_USE|ID_IS_CLOSED|ID_IS_CAPTURED|ID_NEEDS_HEAPIFY|ID_IS_RECBOUND);

  for(size_t c=0; c < ast->children.size(); c++)
    clearusedef(ast->children[c]);
}

/**
 * @brief Mark all defining occurrences that are closed over so that
 * we can later rewrite them.
 */
static void
findusedef(AST *topAst, AST *ast, const int mode,
	   // list of vars that are bound within the lambda at the
	   // current point:
	   CVector<AST *> *boundVars,
	   // list of vars that lambda uses, but are not in
	   // boundVars. Globals are not entered into this.
	   CVector<AST *> *freeVars)
{
  switch(ast->astType) {

  case at_Null:
  case at_AnyGroup:
  case at_version:
  case agt_literal:
  case agt_tvar:
  case agt_var:
  case agt_definition:
  case agt_type:
  case agt_bindingPattern:
  case agt_expr:
  case agt_eform:
  case agt_type_definition:
  case agt_value_definition:
  case at_letbindings:
  case at_letbinding:
  case at_dobindings:
  case at_dobinding:
  case agt_CompilationUnit:
  case at_ifident:
  case at_localFrame:
  case at_frameBindings:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_category:
  case agt_ow:
  case agt_qtype:
  case agt_fielditem:
  case at_refCat:
  case at_valCat:
  case at_opaqueCat:
  case at_tcdecls:
  case at_tyfn:
  case at_usesel:
  case at_use_case:
  case at_identList:
  case at_container:
  case at_defrepr:
  case at_reprbody:
  case agt_reprbodyitem:
  case at_reprcase:
  case at_reprcaselegR:
  case at_reprtag:
  case at_unit:
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
  case at_bitfield:
  case at_declunion:
  case at_declstruct:
  case at_declrepr:
  case at_use:
  case at_import:
  case at_provide:
  case at_declares:
  case at_declare:
  case at_tvlist:
  case at_unitPattern:
  case at_docString:
    break;


  case at_ident:
    {
      //     errStream << ast->loc.asString() 
      //               << "astType = at_ident; mode = " << mode 
      // 	       << std::endl;

      switch(mode) {

      case TYPE_MODE:
	break;
	
      case LOCAL_MODE:
	boundVars->append(ast);
	ast->Flags2 |= ID_IS_DEF;
	break;
      
      case USE_MODE:
	{
	  if (ast->symbolDef == NULL)
	    std::cerr << "Warning: No definition for "
		      << ast->fqn << std::endl;

#if 0
	  assert(ast->symbolDef != NULL);
#endif

	  ast->Flags2 |= ID_IS_USE;

	  // could check ast->symbolDef->symType->isMutable()

	  if(ast->symbolDef->isGlobal())
	    break;

#if 1
	  if (ast->symbolDef->Flags2 & ID_IS_RECBOUND) {
	    ast->Flags2 |= (ID_NEEDS_HEAPIFY|ID_IS_CLOSED);
	    ast->symbolDef->Flags2 |= (ID_NEEDS_HEAPIFY|ID_IS_CAPTURED);
	  }
#endif
      
	  if(boundVars->contains(ast->symbolDef))
	    break;

	  ast->Flags2 |= ID_IS_CLOSED;
	  ast->symbolDef->Flags2 |= ID_IS_CAPTURED;

	  if (ast->symbolDef->getType()->needsCaptureConversion()) {
	    ast->Flags2 |= ID_NEEDS_HEAPIFY;
	    ast->symbolDef->Flags2 |= ID_NEEDS_HEAPIFY;
	  }


	  SHAPDEBUG std::cerr << "Append " << ast->symbolDef->fqn
			      << " to freeVars" << std::endl;

	  if (!freeVars->contains(ast->symbolDef))
	    freeVars->append(ast->symbolDef);
	  break;
	}      
      case NULL_MODE:
      default:
	break;
      }
      break;
    }

  case at_start:
    {
      for(size_t c=0; c < ast->children.size(); c++)
	findusedef(topAst, ast->children[c], NULL_MODE, boundVars, freeVars);
      break;
    }

  case at_deftypeclass:
    {
      findusedef(topAst, ast->children[3], TYPE_MODE, boundVars, freeVars);
      break;
    }

  case at_definstance:
    {
      findusedef(topAst, ast->children[0], TYPE_MODE, boundVars, freeVars);
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      findusedef(topAst, ast->children[2], TYPE_MODE, boundVars, freeVars);
      break;
    }

  case at_method_decl: 
    {
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_defunion:
  case at_defstruct:
    {
      findusedef(topAst, ast->children[4], TYPE_MODE, boundVars, freeVars);
      break;
    }
    
  case at_declValue:
    {      
      findusedef(topAst, ast->children[1], TYPE_MODE, boundVars, freeVars);
      break;
    }
    
  case at_fn: 
    {      
      findusedef(topAst, ast->children[0], TYPE_MODE, boundVars, freeVars);
      findusedef(topAst, ast->children[1], TYPE_MODE, boundVars, freeVars);
      break;
    }

  case at_define:
    {
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_identPattern:
    {
      findusedef(topAst, ast->children[0], mode, boundVars, freeVars);
      break;
    }

  case at_pairPattern:
    {
      findusedef(topAst, ast->children[0], mode, boundVars, freeVars);
      findusedef(topAst, ast->children[1], mode, boundVars, freeVars);
      break;
    }
    
  case at_tqexpr:
    {
      findusedef(topAst, ast->children[0], USE_MODE, boundVars, freeVars);
      findusedef(topAst, ast->children[1], TYPE_MODE, boundVars, freeVars);
      break;
    }

  case at_suspend:
    {
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      break;
    }
    
  case at_select:
    {
      findusedef(topAst, ast->children[0], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_lambda:
    {
      if (ast == topAst) {
	findusedef(topAst, ast->children[0], LOCAL_MODE, 
		   boundVars, freeVars);
	findusedef(topAst, ast->children[1], USE_MODE, 
		   boundVars, freeVars);
      }
      else {
	CVector<AST *> freeVars;
	CVector<AST *> boundVars;

	findusedef(topAst, ast->children[0], LOCAL_MODE, 
		   &boundVars, &freeVars);
	findusedef(topAst, ast->children[1], USE_MODE, 
		   &boundVars, &freeVars);
      }

      break;
    }
    
  case at_interface:
  case at_module:
    {
      for(size_t c=0; c < ast->children.size(); c++)
	findusedef(topAst, ast->children[c], NULL_MODE, boundVars, freeVars);
      break;
    }
  case at_methods:
    {
      for(size_t c=0; c < ast->children.size(); c++)
	findusedef(topAst, ast->children[c], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_method_decls:
  case at_tcapp:    
  case at_typeapp:
  case at_exceptionType:
  case at_arrayType:
  case at_vectorType:
  case at_refType:
  case at_valType:
  case at_primaryType:
  case at_pairType:
  case at_fnargVec:
  case at_mutableType:
  case at_qualType:
  case at_constraints:
  case at_constructors:
  case at_fields:
  case at_fill:
    {
      for(size_t c=0; c < ast->children.size(); c++)
	findusedef(topAst, ast->children[c], TYPE_MODE, boundVars, freeVars);
      break;
    }

  case at_constructor:
  case at_field:
  case at_defexception:
    {
      for(size_t c=1; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], TYPE_MODE, boundVars, freeVars);
      break;
    }

  case at_begin:
    {
      for(size_t c=0; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_argVec:
    {
      for(size_t c=0; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], LOCAL_MODE, boundVars, freeVars);
      break;
    }

  case at_and:
  case at_or:
  case at_not:
  case at_cond:
  case at_cond_legs:
  case at_dup:
  case at_deref:
  case at_switchR:
  case at_sw_legs:
  case at_otherwise:
  case at_tryR:
  case at_throw:
  case at_array_length:
  case at_vector_length:
  case at_array_nth:
  case at_vector_nth:
  case at_vector:
  case at_array:
  case at_pair:
  case at_mkclosure:
  case at_makevector:    
  case at_apply:
    {
      for(size_t c=0; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], USE_MODE, boundVars, freeVars);
      break;
    }
    
  case at_ucon_apply:
  case at_struct_apply:
    {
      for(size_t c=1; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_if:
    {
      for(size_t c=0; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_cond_leg:
    {
      findusedef(topAst, ast->children[0], USE_MODE, boundVars, freeVars);
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      break;
    }
    
  case at_setbang:
    {
      for(size_t c=0; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], USE_MODE, boundVars, freeVars);
      break;
    }
    
  case at_sw_leg:
    {
      findusedef(topAst, ast->children[0], LOCAL_MODE, boundVars, freeVars);
      for(size_t c=1; c<ast->children.size();c++)
	findusedef(topAst, ast->children[c], USE_MODE, boundVars, freeVars);
      break;
    }
    
  case at_do:
    {
      AST *dbs = ast->children[0];      
      for (size_t c = 0; c < dbs->children.size(); c++) {
	AST *db = dbs->children[c];
	
	findusedef(topAst, db->children[1], USE_MODE, boundVars, freeVars);
	findusedef(topAst, db->children[0], LOCAL_MODE, boundVars, freeVars);
	findusedef(topAst, db->children[2], USE_MODE, boundVars, freeVars);
      }
      
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      findusedef(topAst, ast->children[2], USE_MODE, boundVars, freeVars);
      break;
    }

  case at_dotest:
    {
      findusedef(topAst, ast->children[0], USE_MODE, boundVars, freeVars);
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      break;
    }    

  case at_letStar:
  case at_let:
  case at_letrec:
    {
      AST *lbs = ast->children[0];

      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children.size(); c++) {
	AST *lb = lbs->children[c];

	findusedef(topAst, lb->children[0], LOCAL_MODE, boundVars, freeVars);
	if (ast->astType == at_letrec)
	  markRecBound(lb->children[0]);
	findusedef(topAst, lb->children[1], USE_MODE, boundVars, freeVars);
      }
      
      findusedef(topAst, ast->children[1], USE_MODE, boundVars, freeVars);
      break;
    }
  }
}

AST *
cl_rewrite_captured_idents(AST *ast, AST *clenvName)
{
  for (size_t c = 0; c < ast->children.size(); c++)
    ast->children[c] = 
      cl_rewrite_captured_idents(ast->children[c], clenvName);

  switch(ast->astType) {
  case at_ident:
    {
      if (ast->Flags2 & ID_IS_CLOSED) {
	AST *clUse = new AST(at_select, ast->loc);
	clUse->addChild(clenvName->getSCopy(clenvName->loc));
	clUse->addChild(ast);

	ast = clUse;
      }

      break;
    }
  default:
    break;
  }

  return ast;
}

// Walk an AST. If it contains a lambda form that is going to require
// a closure record, fabricate the closure record and append it to outASTs
AST *
cl_convert_ast(AST *ast, CVector<AST *> *outAsts, bool shouldHoist)
{
  bool hoistChildren = true;

  if (ast->astType == at_define) {
    // match agt_expr
    if(ast->children[0]->astType == at_identPattern &&
       (ast->children[1]->astType == at_lambda))
      hoistChildren = HOISTALL;
  }

  for (size_t c = 0; c < ast->children.size(); c++)
    ast->children[c] = 
      cl_convert_ast(ast->children[c], outAsts, hoistChildren);

  switch(ast->astType) {
  case at_lambda:
    {
      CVector<AST *> boundVars;
      CVector<AST *> freeVars;

      AST *clenvName = 0;
      TvPrinter tvP;
      CVector<std::string> *tvs = 0;

      SHAPDEBUG std::cerr << "Processing lambda. " << std::endl;

      // Need to re-run this here, because we may have hoisted inner
      // lambdas and/or introduced a closure conversion, which will
      // have introduced new identifiers.
      findusedef(ast, ast, NULL_MODE, &boundVars, &freeVars);

      // defstruct = ident tvlist category declares fields constraints;
      if (freeVars.size() || CLOSEALL) {
	SHAPDEBUG std::cerr << "Need to generate closure struct. " << std::endl;
	AST *defStruct = new AST(at_defstruct, ast->loc);
	
	// Note defstruct does not use an identPattern
	clenvName = AST::genSym(ast, "clenv");
	clenvName->identType = id_type;
	clenvName->Flags |= (ID_IS_GLOBAL | ID_IS_CTOR);
	defStruct->addChild(clenvName);

	// FIX: if the free variables have alpha types, need to add
	// the alpha variables here, and I don't know how to do
	// that. For the moment, simply add an at_Null record as a
	// placeholder:
	AST *tvlist = new AST(at_tvlist, ast->loc);
	defStruct->addChild(tvlist);

	// env records are ref types
	defStruct->addChild(new AST(at_refCat));
	// no declares
	defStruct->addChild(new AST(at_declares));
	// Parent AST for fields:
	AST *fields = new AST(at_fields, ast->loc);
	defStruct->addChild(fields);

	// Add empty constraints subtree
	defStruct->addChild(new AST(at_constraints, ast->loc));

	for (size_t fv = 0; fv < freeVars.size(); fv++) {
	  assert(freeVars[fv]->astType == at_ident);
	  AST *field = new AST(at_field, ast->loc);
	  AST *ident = new AST(at_ident, ast->loc);
	  ident->s = ident->fqn.ident = freeVars[fv]->s;

	  field->addChild(ident);
	  field->addChild(freeVars[fv]->symType->asAST(ast->loc, &tvP));

	  fields->addChild(field);
	}

	// If the Type of arg contains a type variable,	  
	// then add to the tvlist.
	tvs = tvP.getAllTvarStrings();
	for(size_t i=0; i<tvs->size(); i++) {
	  AST *tv = new AST(at_ident, tvlist->loc);
	  tv->Flags |= ID_IS_TVAR;
	  tv->s = tv->fqn.ident = ((*tvs)[i]);
	  tvlist->children.append(tv);
	}

	// Okay. We have built the type declaration for the closure
	// record. Append it to outAsts
	outAsts->append(defStruct);
      }

      // Need to hoist if (a) we are supposed to, or (b) we just
      // introduced a closure.
      if (shouldHoist || clenvName) {
	SHAPDEBUG std::cerr << "Need to hoist this lambda. " << std::endl;

	SHAPDEBUG ast->PrettyPrint(std::cerr);

	// AST define = bindingPattern expr;
	AST *newDef = new AST(at_define, ast->loc);

	AST *lamName = AST::genSym(ast, "lam");
	lamName->identType = id_value;
	lamName->Flags |= ID_IS_GLOBAL;

	AST *lamType = ast->symType->asAST(ast->loc);

	AST *lamPat = new AST(at_identPattern, ast->loc, lamName, lamType);
	newDef->addChild(lamPat);
	newDef->addChild(ast);
	newDef->addChild(new AST(at_constraints));

	// See if we can re-use the existing lambda or we need to add
	// the closure argument:

	AST *clArgName = 0;

	if (clenvName) {
	  // Insert the extra closure argument and prepend the type to
	  // the attached function type signature

	  AST *argVec = ast->child(0);
	  AST *body = ast->child(1);
	  clArgName = new AST(at_ident, ast->loc);
	  clArgName->s = clArgName->fqn.ident = "__clArg";
	  AST *clPat = new AST(at_identPattern, ast->loc, clArgName);

	  AST *clType;

	  if(tvs->size()) {
	    AST *typeApp = new AST(at_typeapp, ast->loc);
	    typeApp->addChild(clenvName->Use());

	    for(size_t i=0; i<tvs->size(); i++) {
	      AST *tv = new AST(at_ident, ast->loc);
	      tv->Flags |= ID_IS_TVAR;
	      tv->s = tv->fqn.ident = ((*tvs)[i]);
	      typeApp->addChild(tv);
	    }

	    clType = typeApp;
	  }
	  else
	    clType = clenvName->Use();

	  lamType->children[0]->children.insert(0, clType);
	  clPat->addChild(clType->getDCopy());
	  argVec->children.insert(0, clPat);

	  ast->children[1] = cl_rewrite_captured_idents(body, clArgName);
	}

	// We have built the hoisted procedure. Emit that:
	outAsts->append(newDef);

	SHAPDEBUG ast->PrettyPrint(newDef);

	// If the lambda requires a closure, emit a make-closure, else
	// emit an identifier reference in place of the lambda:

	AST *lamUse = lamName->Use();
	lamUse->symbolDef = lamName;

	if (clenvName) {
	  AST *mkEnv = new AST(at_apply, ast->loc);
	  mkEnv->addChild(clenvName->Use());

	  for (size_t fv = 0; fv < freeVars.size(); fv++)
	    mkEnv->addChild(freeVars[fv]->Use());

	  AST *mkClo = new AST(at_mkclosure, ast->loc, mkEnv, lamUse);
	  ast = mkClo;
	}
	else {
	  ast = lamUse; 
	}

	SHAPDEBUG ast->PrettyPrint(ast);
      }

      return ast;
    }

  default:
    return ast;
  }
}

void
cl_convert(UocInfo *uoc)
{
  CVector<AST *> outAsts;

  AST *modOrIf = uoc->ast->child(0);

  for (size_t c = 0;c < modOrIf->children.size(); c++) {
    AST *child = modOrIf->children[c];

    child = cl_convert_ast(child, &outAsts, true);
    outAsts.append(child);
  }

  modOrIf->children = outAsts;
}

// Collect all of the at_ident ASTs that are defined in this argument
// binding pattern and are marked as captured.
static void
collectHeapifiedArgs(AST *ast, CVector<AST *> *capturedArgs)
{
  if (ast->astType == at_ident && (ast->Flags2 & ID_NEEDS_HEAPIFY)) {
    assert (ast->Flags2 & ID_IS_CAPTURED);

    capturedArgs->append(ast);
  }

  for(size_t i=0; i < ast->children.size(); i++)
    collectHeapifiedArgs(ast->child(i), capturedArgs);
}

// Simple re-writing pass. Takes all of the identifiers that were 
// identfied above as being closed over and re-writes them in such a
// way as to push them into the heap. In the following examples, the
// '*' after the bound identifier indicates that it is captured. The
// rewrites that we need to do on the defining occurrences are:
//
//  (let[rec] ((a* e-init) ...) <body>)
//  => (let[ref] ((a (dup e-init))) <body>)
//
//  (switch id* e <cases>) => (switch id (dup e) <cases>)
//
//  (lambda (a* ...) <body>)
//   => (lambda (a ...)
//         (let ((a (dup a)))
//            <body>))
//
// And then in the use-occurrences we simply need (at this stage) to
// wrap the use-occurrences with (__clmember id)
//
AST *
cl_heapify(AST *ast)
{
  switch(ast->astType) {
  case at_lambda:
    {
      // Proceed through the arguments. For each argument that is
      // captured, rewrite the body to be surrounded by a dup'ing LET
      // form.
      AST *args = ast->child(0);
      AST *body = ast->child(1);

      CVector<AST *> heapifiedArgs;
      // Captures the leaf idents:
      collectHeapifiedArgs(args, &heapifiedArgs);
      if (heapifiedArgs.size() == 0)
	break;

      AST *bindings = new AST(at_letbindings, body->loc);

      // Wrap the existing body in a LET binding:
      body = new AST(at_let, body->loc, bindings, body);
      body->addChild(new AST(at_constraints));
      ast->children[1] = body;

      // The RHS is not yet dup'd here. This will happen when this let
      // is processed in the at_letbinding handler.
      for (size_t i = 0; i < heapifiedArgs.size(); i++) {
	AST *arg = heapifiedArgs[i];
	AST *argExpr = new AST(at_ident, arg->loc);
	argExpr->s = argExpr->fqn.ident = arg->s;

	AST *argIdent = new AST(at_ident, arg->loc);
	argIdent->s = argIdent->fqn.ident = arg->s;
	AST *identPattern = new AST(at_identPattern, arg->loc, argIdent);

	// We have moved the point of capture into the let, which was
	// the point:
	arg->Flags2 &= ~(ID_IS_CAPTURED|ID_NEEDS_HEAPIFY);
	argIdent->Flags2 |= (ID_IS_CAPTURED|ID_NEEDS_HEAPIFY);

	AST *theBinding = new AST(at_letbinding, arg->loc, identPattern, argExpr);
	bindings->addChild(theBinding);
      }

      break;
    }

  case at_letbinding:
    {
      AST *bpattern = ast->child(0);
      AST *expr = ast->child(1);

      assert(bpattern->astType == at_identPattern);
      AST *ident = bpattern->child(0);

      SHAPDEBUG std::cerr << "Let binding for " << ident->s << std::endl;

      /* Must heapify EXPR unconditionally */
      expr = cl_heapify(expr);

      if (ident->Flags2 & ID_NEEDS_HEAPIFY) {
	// Process the RHS:

	SHAPDEBUG std::cerr << "  Needs dup " << ident->s << std::endl;
	
	assert (ident->Flags2 & ID_IS_CAPTURED);
	expr = new AST(at_dup, expr->loc, expr);
 
	// if the binding pattern was qualified by a type qualification,
	// wrap that in a REF:
	if (bpattern->children.size() == 2)
	  bpattern->children[1] = 
	    new AST(at_refType, bpattern->loc, bpattern->children[1]);
      }

      ast->children[1] = expr;

      return ast;
    }

  case at_ident:
    {
      // If this is a use occurrence of a heapified symbol, rewrite it
      // into a DEREF reference.

      SHAPDEBUG std::cerr << "Processing " << ast->s << std::endl;
      SHAPDEBUG if (ast->Flags2 & ID_NEEDS_HEAPIFY)
	std::cerr << "  needs heapify" << std::endl;
      SHAPDEBUG if (ast->Flags2 & ID_IS_CLOSED)
	std::cerr << "  closed" << std::endl;

      if (ast->Flags2 & ID_NEEDS_HEAPIFY) {
	SHAPDEBUG std::cerr << "Needs deref " << ast->s << std::endl;
	ast = new AST(at_deref, ast->loc, ast);

	// Intentionally skip any further recursion, since we don't
	// want to do this redundantly.
	return ast;
      }
    }
    
  default:
    break;
  }

  for (size_t c = 0; c < ast->children.size(); c++)
    ast->children[c] = cl_heapify(ast->children[c]);

  return ast;
}

#ifdef LATE_CLCONV
bool
UocInfo::be_clconv(std::ostream& errStream,
		   bool init, unsigned long flags)
{ 
  bool errFree = true;

  CVector<AST *> freeVars;
  CVector<AST *> boundVars;
  
  AST *&ast = UocInfo::linkedUoc.ast;

  SHAPDEBUG std::cerr << "findusedef 1" << std::endl;

  findusedef(ast, ast, NULL_MODE, &boundVars, &freeVars);

  SHAPDEBUG UocInfo::linkedUoc.PrettyPrint(errStream, true);

  SHAPDEBUG std::cerr << "cl_heapify" << std::endl;
  ast = cl_heapify(ast);

  SHAPDEBUG UocInfo::linkedUoc.PrettyPrint(errStream, true);

  SHAPDEBUG std::cerr << "RandT 1" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, RandT(errStream, &UocInfo::linkedUoc, true, POLY_SYM_FLAGS, POLY_TYP_FLAGS));
  assert(errFree);

  SHAPDEBUG std::cerr << "findusedef 2" << std::endl;

  // This *shouldn't* be necessary, but it doesn't hurt anything.
  clearusedef(ast);
  findusedef(ast, ast, NULL_MODE, &boundVars, &freeVars);

  SHAPDEBUG UocInfo::linkedUoc.PrettyPrint(errStream);

  SHAPDEBUG std::cerr << "cl_convert" << std::endl;
  cl_convert(&UocInfo::linkedUoc);

  SHAPDEBUG UocInfo::linkedUoc.PrettyPrint(errStream);

  SHAPDEBUG std::cerr << "RandT 2" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, RandT(errStream, &UocInfo::linkedUoc, true, POLY_SYM_FLAGS, POLY_TYP_FLAGS));
  assert(errFree);

  return true;
}
#else
bool
UocInfo::fe_clconv(std::ostream& errStream,
		   bool init, unsigned long flags)
{ 
  bool errFree = true;

  CVector<AST *> freeVars;
  CVector<AST *> boundVars;
  
  SHAPDEBUG std::cerr << "findusedef 1" << std::endl;

  findusedef(ast, ast, NULL_MODE, &boundVars, &freeVars);

  SHAPDEBUG if (isSourceUoc)
    PrettyPrint(errStream);

  SHAPDEBUG std::cerr << "cl_heapify" << std::endl;
  ast = cl_heapify(ast);

  SHAPDEBUG if (isSourceUoc)
    PrettyPrint(errStream);

  SHAPDEBUG std::cerr << "RandT 1" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, RandT(errStream, this, true, PP_SYM_FLAGS, PP_TYP_FLAGS));
  assert(errFree);

  SHAPDEBUG std::cerr << "findusedef 2" << std::endl;

  // This *shouldn't* be necessary, but it doesn't hurt anything.
  clearusedef(ast);
  findusedef(ast, ast, NULL_MODE, &boundVars, &freeVars);

  SHAPDEBUG if (isSourceUoc)
    PrettyPrint(errStream, true);

  SHAPDEBUG std::cerr << "cl_convert" << std::endl;

  cl_convert(this);

  SHAPDEBUG if (isSourceUoc)
    PrettyPrint(errStream, true);

  SHAPDEBUG std::cerr << "RandT 2" << std::endl;
  // Re-run the type checker to propagate the changes:
  CHKERR(errFree, RandT(errStream, this, true, PP_SYM_FLAGS, PP_TYP_FLAGS));
  assert(errFree);

  return true;
}
#endif
