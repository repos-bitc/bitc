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
#include <gmp.h>
#include <errno.h>
#include <sstream>
#include <assert.h>

#include "Version.hxx"
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"
#include "Special.hxx"


using namespace sherpa;
using namespace std;

//#define VERBOSE 200

#ifdef VERBOSE
#define STRICTLYEQUALS(x) strictlyCompatible(x, true) // verbose = true
#define DEBUG if(1)
#else
#define STRICTLYEQUALS(x) strictlyCompatible(x)
#define DEBUG if(0)
#endif

static bool 
reRandT(std::ostream &errStream, 
	UocInfo *uoc, AST *ast)
{
  stringstream ss;
  bool errFree = 
    uoc->RandTexpr(ss, ast, POLY_SYM_FLAGS, POLY_TYP_FLAGS,
		   "Polyinstantiation (s):\n");
  
  DEBUG if(!errFree)
    errStream << ss.str() << " while RandTing " << ast->asString() << std::endl;
  
  return errFree;
}

static bool 
reRandT(std::ostream &errStream, UocInfo *uoc,
	unsigned long rFlags = POLY_SYM_FLAGS, 
	unsigned long tFlags = POLY_TYP_FLAGS,
	std::string mess = "FINAL Polyinstantiation (s):\n")
{
  //   errStream << "AT " << mess << ": "
  // 	    << "Polyinstantiated AST is "
  // 	    << std::endl
  //  	    << uoc->ast->asString()
  // 	    << std::endl;
  
  bool errFree = 
    uoc->RandT(errStream, true, rFlags, tFlags, mess);
  if(!errFree)
    errStream << " while RandTing " 
	      << uoc->ast->asString() 
	      << std::endl;

  return errFree;
}

static bool
removeAndReRandT(std::ostream &errStream, UocInfo *uoc, AST *ast)
{
  bool removed = false;
  std::string nm = ast->getID()->s;
  for (size_t i = 0; i < uoc->env->bindings->size(); i++)
    if (uoc->env->bindings->elem(i)->nm == nm) {
      uoc->env->bindings->remove(i);
      removed = true;
      break;
    }
  
  assert(removed);

  removed = false;
  for (size_t i = 0; i < uoc->gamma->bindings->size(); i++)
    if (uoc->gamma->bindings->elem(i)->nm == nm) {
      uoc->gamma->bindings->remove(i);
      removed = true;
      break;
    }
  
  assert(removed);
  
  return reRandT(errStream, uoc, ast);
}

void
sub(AST *ast, AST *tv, 
    AST *newAst, Type *typ,
    AST *parent, size_t chno)
{
  if(ast->astType == at_ident) {
    assert(ast->children.size() == 0);
    if(ast->symbolDef == tv)
      parent->children[chno] = typ->asAST(ast->loc);
  }
  else {
    for(size_t c = 0; c < ast->children.size(); c++)
      sub(ast->children[c], tv, newAst->children[c], typ, newAst, c);
  }    
}

/* Namakaranam -- the ritual of giving a name */
#define NAMKARAN(ast, name) do {		\
    ast->s = name;				\
    ast->Flags2 |= IDENT_MANGLED;		\
  } while(0);

static void
substitute(AST *ast, AST *from, AST *to)
{
  if(ast->astType == at_ident && ast->s == from->s) {
    ast->symbolDef = to;
    NAMKARAN(ast, to->s);
  }
  
  for(size_t c = 0; c < ast->children.size(); c++)
    substitute(ast->children[c], from, to);
}


static inline void
add_todo(std::ostream &errStream, CVector<AST *> &todo,
	 AST *copy)
{
  // optimization for current name  
  DEBUG errStream << "Added to todo: " << copy->asString() << std::endl;
  todo.append(copy);
}

void
add_relevant_items_todo(std::ostream &errStream, 
			CVector<AST *> &todo, AST *ast)
{
  switch(ast->astType) {
  case at_define: 
  case at_letbinding:
    {
      add_todo(errStream, todo, ast->children[1]);      
      if(ast->children[0]->children[1] != NULL)
	add_todo(errStream, todo, ast->children[0]->children[1]);      
      break;
    }

  case at_proclaim: 
    {
      add_todo(errStream, todo, ast->children[1]);      
      break;
    }

  case at_defstruct:
    {
      add_todo(errStream, todo, ast->children[4]);	        
      break;
    }

  case at_defunion:
    {
      add_todo(errStream, todo, ast->children[4]);	        
      break;
    }

  case at_declstruct:
  case at_declunion:
  case at_defexception:
    {
      break;
    }

  default:
    {
      assert(false);
      break;
    }
  }    
}

static inline void
markReached(std::ostream &out, AST *ast)
{
  ast->reached = true;
  //DEBUG out << "Marked " << ast->asString() << std::endl;
}

static inline void
unmarkReached(std::ostream &out, AST *ast)
{
  ast->reached = false;
  //DEBUG out << "    unMarked " << ast->asString() << std::endl;
}


std::string
getNewName(AST *def, Type *typ)
{
  stringstream ss;
  Type *uAdjTyp = typ;
  
  if(typ->isUcon() || typ->isUval())
    uAdjTyp = typ->getUnionType();

  assert(uAdjTyp);
  ss << def->s;
  ss << "#" << uAdjTyp->mangledString();
  return ss.str();      
}


void
clearConstraints(AST *ast, AST *parent=NULL, size_t chno=0)
{
  switch(ast->astType) {
  case at_constraints:
    ast->children.erase();
    break;

  case at_qualType:
    assert(parent != NULL);
    clearConstraints(ast->children[1], ast, 1);
    parent->children[chno] = ast->children[1];
    break;
    
  default:
    for(size_t c=0; c < ast->children.size(); c++)
      clearConstraints(ast->children[c], ast, c);
    break;
  }
} 

AST *
Specialize(std::ostream &errStream,
	   bool &errF,
	   UocInfo *uoc,
	   AST *def, 
	   Type *typ,
	   AST *mod, 
	   CVector<AST *> &todo)  
{
  if(def->symbolDef != NULL)
    def = NULL;

  assert(def->symbolDef == NULL);
  assert(def->astType == at_ident);

  typ = typ->getTheType(true, false);
  //typ->clearAllMaybes();
  
  if(!typ->isConcrete()) {
    errStream << " Specialize called with NON-concrete Type: "
  	      << typ->asString() << " For  AST " 
  	      << def->asString() << " located at " << def->loc << "."
  	      << std::endl;
  }    
  assert(typ->isConcrete());
  
  DEBUG errStream << "Special: " << def->s << ": " 
	    << " WITH TYPE " << def->symType->asString()
	    << " FOR " << typ->asString()
	    << std::endl;
  
  if(def->isMethod()) {
    AST *typClass = def->defForm;
    AST *tcID = typClass->children[0];
    assert(tcID->astType == at_ident);
    //assert(typ->defAst != NULL);
    //assert(typ->myContainer != NULL);
    //assert(typ->myContainer == tcID);

    Typeclass *pred = tcID->scheme->type_instance_copy();
    size_t nthMethod = 0;
    bool found = false;
    for(size_t i = 0; i < pred->components.size(); i++) {
      Type *ithMethod = pred->components[i]->typ->getType();
      if(ithMethod->defAst == def) {
	nthMethod = i;
	found = true;
	break;
      }
    }
        
    if(!found) {
      errStream << def->loc << ": Internal compiler error. "
		<< "Method " << def->s 
		<< " could not be located within the type of "
		<< " its typeclass " << tcID->s 
		<< " by the polyinstantiator."
		<< std::endl;
      errF = false;
      return def;
    }

    // Unify the method with the necessary type
    // This should have enough information to 
    // uniquely identify the instance.
    BE_CHKERR(errF, pred->components[nthMethod]->typ->unifyWith(typ));	  
    
    CVector<Instance *> *insts = 
      uoc->instEnv->getBinding(tcID->fqn.asString());    
    
    found = false;
    size_t nthInstance = 0;
    for(size_t j=0; j < insts->size(); j++) {
      Instance *currInst = (*insts)[j];
      if(currInst->satisfies(errStream, pred, uoc->instEnv)) {
	nthInstance = j;
	found = true;
	break;
      }
    }
    
    if(!found) {
      errStream << def->loc << ": Internal compiler error. "
		<< "No Instance found for " 
		<< pred->asString() << " while polyinstantiating "
		<< def->s << "."
		<< std::endl;
      errF = false;
      return def;
    }

    AST *instAST = (*insts)[nthInstance]->ast;
    AST *theMethod = instAST->children[1]->children[nthMethod];

    // If an immediate lambda was present, then InstLamHoist hoisted
    // it and left us with an ID wrapped by a THE.
    assert (theMethod->astType == at_ident || theMethod->astType == at_tqexpr);
    if (theMethod->astType == at_tqexpr)
      theMethod = theMethod->children[0];

    AST *theMethodDef = theMethod->symbolDef;
    // FIX: Shap inserted this to get a reliable stopping point for
    // debugging.
    assert(theMethodDef);
    markReached(errStream, theMethodDef->defForm); 
    AST *altDef = Specialize(errStream, errF, uoc, 
			     theMethodDef, typ, mod, todo);
    return altDef;
  }
  else if(def->isUnionLeg()) {
    assert(!(def->isDecl));
    Type *t = typ->getUnionType();

    AST *unin = def->defForm;
    AST *unionID = unin->children[0];

    //     cout << "** def = " << def->asString() << "(" <<
    //       def->symType->asString() << ")" << endl 
    // 	 << "** defForm = " << unin->asString() << "(" <<
    //       unin->symType->asString() << ")" << endl 
    // 	 << "** unionID = " << unionID->asString() << "(" <<
    //       unionID->symType->asString() << ")" << endl 
    // 	 << "** t = " << t->asString() << endl;    
    //     cout << "## Calling Specialize on " << unionID->asString() 
    // 	 << "(" << unionID->symType->asString() << ")"
    // 	 << " with " << t->asString() << endl;

    AST *newUnionID = Specialize(errStream, errF, uoc, unionID, 
				 t, mod, todo);
    AST *newUnion = newUnionID->defForm;
    assert(newUnion->astType == at_defunion);
    //cout << "Obtained " << newUnion->asString() 
    //	 << "(" << newUnion->symType->asString() << ")" << endl;
    AST *ctrs = unin->children[4]; // oldUnion
    for(size_t i = 0; i < ctrs->children.size(); i++) {
      AST *ctr = ctrs->children[i];
      if(ctr->children[0] == def)
	return (newUnion->children[4]->children[i]->children[0]);
    }
    // Not reached.
    assert(false);
  }

  /* Skipped in new Poly */
  if(!def->isDecl) {
    if(def->symType->STRICTLYEQUALS(typ)) {
      add_relevant_items_todo(errStream, todo, def->defForm);
      return def;
    }
  }
  
  if(def->special != NULL) {
    for(size_t i=0; i<def->special->size(); i++) {
      spStruct *sp = (*def->special)[i];
      DEBUG errStream << "Comparing existing "
		      << sp->typ->asString() << " for " 
		      << typ->asString()
		      << std::endl;
      if(sp->typ->STRICTLYEQUALS(typ)) {
	
	DEBUG errStream << "FOUND, returning "
			<< sp->ast->getID()->asString()
			<< std::endl;
	return sp->ast->getID();
      }
    }
  }
  else {
    //     errStream << "No Match found" << std::endl;
    def->special = new CVector <spStruct *>;    
  }
  
  AST *defn = def->defForm;
  AST *copy = defn->getDCopy();
  // Mark up the defForms in the copy
  if(defn->astType == at_letbinding) {
    copy->Flags2 &= ~LB_MUST_GO;

    AST *lbs = defn->defForm;
    AST *let = lbs->defForm;
    AST *top = let->defForm;
    uoc->markDefForms(copy, lbs, top); 
  }
  else if(defn->astType == at_letbindings) {
    AST *let = defn->defForm;
    AST *top = let->defForm;
    uoc->markDefForms(copy, let, top);
  }
  else 
    uoc->markDefForms(copy);

  assert(copy->polyinst == false);
  clearConstraints(copy);
  AST *id = copy->getID();
  //id->defForm = copy;
  
  def->special->append(new spStruct(copy, typ));
  if(def->isDecl && (def->defn != NULL)) {
    DEBUG std::cout << "For declaration " << def->s 
		    << " Calling Specialize on definition "
		    << def->defn->defForm->asString()
		    << std::endl;

    AST *altDef = Specialize(errStream, errF, uoc, 
			     def->defn, typ, mod, todo);
    
    NAMKARAN(id, altDef->s);
    id->defn = altDef;
  }
  else {
    NAMKARAN(id, getNewName(id, typ));
  }
  
  switch(copy->astType) {
  case at_letbinding:
    {
      AST *ip = copy->children[0];
      assert(ip->astType == at_identPattern);
      AST *t = typ->asAST(copy->loc);
      if(ip->children.size() > 1) {
	ip->children[1] = t;	
      }
      else {
	ip->children.append(t);	
      }
      
      substitute(copy, defn->getID(), copy->getID()); 
      add_relevant_items_todo(errStream, todo, copy);
      
      DEBUG errStream << "Specialized\n\t" << defn->asString() 
		      << "   to \n\t"
		      << copy->asString() << std::endl;

      AST *letbinds = defn->defForm;
      AST *let = letbinds->defForm;
      AST *definition = let->defForm;

      letbinds->children.append(copy);
      // No, not
      //BE_CHKERR(errF, removeAndReRandT(errStream, uoc, definition));
      // There might be errors at this stage because the entire
      //expression is not changed in full.
      removeAndReRandT(errStream, uoc, definition);
      return id;
    }

  case at_define: 
    {
      AST *ip = copy->children[0];
      assert(ip->astType == at_identPattern);
      AST *t = typ->asAST(copy->loc);
      if(ip->children.size() > 1) {
	ip->children[1] = t;	
	// This should be sufficient as the type obtained from the
	// type record is at least, or more precise than what the user
	// wrote. I was previously doing:
	// 	AST *the = new AST(at_tqexpr, copy->loc, 
	// 			   copy->children[1], t);
	// 	copy->children[1] = the;
      }
      else {
	ip->children.append(t);	
      }

      break;
    }

  case at_proclaim: 
    {
      copy->children[1] = typ->asAST(copy->loc);
      break;
    }

  case at_declstruct:
    { 
      assert(typ->kind == ty_structv  ||
	     typ->kind == ty_structr);
      
      AST *tvlist = copy->children[1];
      tvlist->children.erase();
      break;	      
    }
     
  case at_defstruct:
    {
      assert(typ->kind == ty_structv ||
	     typ->kind == ty_structr);
      

      AST *tvlist = copy->children[1];
      AST *fields = copy->children[4];
      AST *oldtvlist = defn->children[1];
      AST *oldfields = defn->children[4];

      assert(typ->typeArgs.size() == tvlist->children.size());
      assert(fields->children.size() != 0); // This is a definition

      for(size_t i=0; i < oldtvlist->children.size(); i++) {
	AST *oldtv = oldtvlist->children[i];
	for(size_t j=0; j < oldfields->children.size(); j++) {
	  AST *oldfield = oldfields->children[j];
	  AST *field = fields->children[j];
	  if(field->astType == at_fill)
	    continue;
	  sub(oldfield->children[1], oldtv, field->children[1], 
	      typ->typeArgs[i], field, 1);
	}
      }

      tvlist->children.erase();
      break;
    }

  case at_declunion:
    {
      assert(typ->kind == ty_unionv ||
             typ->kind == ty_unionr);

      AST *tvlist = copy->children[1];
      tvlist->children.erase();
      break;	
    }
    
  case at_defunion:
    {
      assert(typ->kind == ty_unionv ||
	     typ->kind == ty_unionr || 
	     typ->kind == ty_uconv  || 
	     typ->kind == ty_uconr  ||
	     typ->kind == ty_uvalv  ||
	     typ->kind == ty_uvalr);
      
      AST *tvlist = copy->children[1];
      AST *ctrs = copy->children[4];
      AST *oldtvlist = defn->children[1];
      AST *oldctrs = defn->children[4];
      
      assert(typ->typeArgs.size() == tvlist->children.size());
      assert(ctrs->children.size() != 0); // This is a definition
      
      for(size_t i=0; i < oldtvlist->children.size(); i++) {
	AST *oldtv = oldtvlist->children[i];
	Type *newArg = typ->typeArgs[i];
	for(size_t j=0; j < oldctrs->children.size(); j++) {
	  AST *oldctr = oldctrs->children[j];
	  AST *ctr = ctrs->children[j];
	  for(size_t k=0; k < oldctr->children.size(); k++) {	  
	    AST *field = oldctr->children[k];
	    if(field->astType == at_fill)
	      continue;	    
	    
	    sub(oldctr->children[k], oldtv, ctr->children[k], 
		newArg, ctr, k);
	  }
	}
      }
      
      for(size_t i = 0; i < ctrs->children.size(); i++) {
	AST *ctr = ctrs->children[i];
	AST *ctrID = ctr->children[0];
	
	ctrID->defForm = copy;
	NAMKARAN(ctrID, getNewName(ctrID, typ));
      }
      
      tvlist->children.erase();
      break;
    }
    
  case at_defexception:
    {
      assert(false);
      return NULL;
    }
  default:
    {
      assert(false);
      return NULL;
    }
  }

  add_relevant_items_todo(errStream, todo, copy);
  
  DEBUG errStream << "Specialized\n\t" << defn->asString() 
		  << "   to \n\t"
		  << copy->asString() << std::endl;
  
  BE_CHKERR(errF, reRandT(errStream, uoc, copy));
  //reRandT(errStream, uoc, copy);


  if(!copy->isDecl)
    if(def->decl != NULL)
      Specialize(errStream, errF, uoc, def->decl, typ,
		 mod, todo);

  return id;
}

// Polymorphic local procedure bindings must be dropped if not
// used/polyinstantiated. This is because they may polymorphically
// invoke type-class methods, which must not survive after this pass.
static bool 
lbMustNotSurvive(const AST *lb)
{
  assert(lb->astType == at_letbinding);
  AST *lhs = lb->children[0]->children[0];

  // This check is subtle. No expression that can cause a side-effect
  // must ever be dropped. I initially thought that the check must be
  // made as to whether the AST is a lambda (possibly wrapped in any
  // number of type qualifications, but this is not enough because
  // (define (a253 x) (+ x 1))
  // ... (let ((b253 a253)) ... )
  // will still preserve the erroneous case. Thus this check is
  // correct. 
  // Since no non-value can be polymorphic, this check will ensure
  // that we do not drop any state change.
  return (lhs->symType->isFnxn() && !lhs->symType->isConcrete());
}

static void
setupTypApp(AST *ast) 
{  
  AST *copy = ast->getDCopy();
  AST *typ = ast->symType->asAST(ast->loc);
  AST *the = new AST(at_tqexpr, ast->loc, copy, typ);
  ast->set(the);
  ast->polyinst = true;
}

// Polymorphic types to concrete types.
bool
polyinst(std::ostream &errStream,
	 UocInfo *uoc,
	 AST *ast, AST *mod, 
	 CVector<AST *> &todo)
{
  bool errFree = true; bool errF= true;
  DEBUG errStream << "polyinst: " << ast->loc << ": "
		  << ast->atKwd() << ": "
		  << ast->asString()
		  << std::endl;
  
  if(ast->polyinst) {
    DEBUG errStream << "Already processed" << endl;
    return true;
  }
  ast->polyinst = true;

  switch(ast->astType) {
  case at_start:
  case at_module:
  case at_interface:
    {
      assert(false);
      break;
    }

  case at_typeapp:
    {
      for (size_t c = 0; c < ast->children.size(); c++) {
	BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[c], 
				 mod, todo));
      }

      ast->set(ast->children[0]);
      ast->polyinst = true;      

      break;
    }
    
  case at_field:
    {
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[1], 
			       mod, todo));
      break;
    }

  case at_select:
    {
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[0], 
			       mod, todo));

      if(ast->Flags2 & SEL_FROM_UN_VAL || ast->Flags2 & SEL_FROM_UN_TYPE)
	BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[1], 
				 mod, todo));
      break;
    }

  case at_declare:
    {
      if(ast->children.size() > 1)
	BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[1], 
				 mod, todo));
      
      break;
    }

  case at_bitfield:
    {
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[0], 
			       mod, todo));
      
      break;
    }
    
  case at_arrayType:
    {
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[0], 
				  mod, todo));
      break;
    }
    
  case at_tqexpr:
    {     
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[0], 
				  mod, todo));
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[1], 
				  mod, todo));

      if(ast->children[0]->astType == at_tqexpr) {	
	assert(ast->children[0]->children[0]->astType == at_intLiteral ||
	       ast->children[0]->children[0]->astType == at_floatLiteral);

	/* We fixed intlit's / floatlit's type twice */
	ast->children[1] = ast->children[0]->children[1];
	ast->children[0] = ast->children[0]->children[0];
      }
      break;
    }

  case at_floatLiteral:
  case at_intLiteral:
    {
      if(ast->symType->isConcrete())
	setupTypApp(ast);
      break;	
    }

  case at_let:
  case at_letrec:
  case at_letStar:
    {
      // First process the final expression.
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[1], 
				  mod, todo));
      
      // Then process each binding.
      BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[0], 
				  mod, todo));
      break;
    }

  case at_letbinding:
    {
      if(!lbMustNotSurvive(ast)) {
	DEBUG errStream << "LB with type " 
			<< ast->children[1]->symType->asString()
			<< " must survive." << std::endl;
	BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[0], 
				    mod, todo));
	BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[1], 
				    mod, todo));
      }
      else {
	ast->Flags2 |= LB_MUST_GO;
	DEBUG errStream << "Skipping LB with type " 
			<< ast->children[1]->symType->asString() << std::endl;
      }
      break;
    }

  case at_ident:
    {
      if(ast->symbolDef == NULL) {
	/* DEFINING OCCURRENCE */
	DEBUG errStream << "[Defining Occurence]" << std::endl;
	AST *defn = ast->defForm;
	if(defn != NULL)
	  add_relevant_items_todo(errStream, todo, defn); 
	
	// defn will not be NULL for top level definitions
	// and let-bindings. It will be NULL for local definitions
	// and tvar definitions.
	// For example:
	// (define R (case nil (nil (the int32 10)) 
	//          ((cons a b) 20) (otherwise 30)))
	// for `a' and `b' 		
      }
      else {
	/* USE OCCURENCE */
	AST *def = ast->symbolDef;
	DEBUG errStream << "[Use Occurence]" << std::endl;

	if(def->isGlobal()) {
	  // All globals MUST have defForm set
	  markReached(errStream, def->defForm);
	  if(def->defn != NULL)
	    markReached(errStream, def->defn->defForm);
	}	  

	if(!ast->symType->STRICTLYEQUALS(def->symType)) {
	  
	  if(!ast->symType->isConcrete())
	    ast->symType->SetTvarsToUnit();
	
	  AST *altDef = Specialize(errStream, errF, uoc, def, 
				   ast->symType, mod, todo);
	  BE_CHKERR(errFree, errF);
	 
	  NAMKARAN(ast, altDef->s);
	  ast->symbolDef = altDef;	      
	}
	else {
	  AST *defn = ((def->isUnionLeg()) ? 
		       (def->defForm->children[0]) :
		       (def));
	  
	  if(defn->defForm)
	    add_todo(errStream, todo, defn->defForm);
	  
	  if((defn->isDecl) && (defn->defn != NULL))
	    add_todo(errStream, todo, defn->defn->defForm);
	}
      }
      break;
    }
      
  case at_deftypeclass:
  case at_definstance:
    break;

  default:
    {
      for (size_t c = 0; c < ast->children.size(); c++) {
	BE_CHKERR(errFree, polyinst(errStream, uoc, ast->children[c], 
				 mod, todo));
      }	
      break;
    }
  }
  
  return errFree;
}

void
getLetintoLetStream(std::ostream &errStream, AST *ast)
{
  switch(ast->astType) {
  case at_letbindings:
    {
      AST *lbs = ast;
      for(size_t c = 0; c < lbs->children.size(); c++) {
	AST *lb = lbs->children[c];	
	AST *id = lb->getID();
	if(id->special != NULL && id->special->size() > 0) {
	  lbs->children.remove(c);
	  c--;
	}
	else if(lb->Flags2 & LB_MUST_GO) {
	  lbs->children.remove(c);
	  c--;
	}
	else {
	  getLetintoLetStream(errStream, lb->children[1]);
	}
      }
      break;
    }

  case at_let:
  case at_letrec:
    {
      AST *lbs = ast->children[0];
      AST *expr = ast->children[1];
      getLetintoLetStream(errStream, lbs);
      getLetintoLetStream(errStream, expr);
      if(lbs->children.size() == 0)
	ast->set(expr);
      break;
    }
      
  case at_letbinding:
    {
      assert(false);
      break;
    }
    
  default:
    {
      for(size_t c = 0; c < ast->children.size(); c++)
	getLetintoLetStream(errStream, ast->children[c]);
      break;
    }
  }
}

void
getValuesIntoMainStream(std::ostream &errStream, AST *mod) 
{
  for(size_t c=0; c < mod->children.size(); c++) {
    AST *def = mod->children[c];
    switch(def->astType) {
    case at_defstruct:
    case at_defunion:
    case at_declstruct:
    case at_declunion:
    case at_defexception:
    case at_deftypeclass:
    case at_definstance:
      break;
      
    case at_define:
    case at_proclaim:
      {
	AST *ast = def->getID();
	if(ast->special != NULL && ast->special->size() > 0) {
	  unmarkReached(errStream, def);
	  
	  for(size_t i=0; i < ast->special->size(); i++) {
	    spStruct *sp = (*ast->special)[i];
	    AST *altDef = sp->ast;
	    markReached(errStream, altDef);
	    
	    if(!sp->lifted) {
	      sp->lifted = true;
	      if(c == mod->children.size() - 1)
		mod->children.append(altDef);
	      else
		mod->children.insert(c+1, altDef);
	      c++;
	      
	      if(altDef->astType == at_define)
		getLetintoLetStream(errStream, altDef->children[1]);
	    } 
	  }
	}
	else {
	  if((def->astType == at_proclaim) &&
	     (ast->Flags & DEF_IS_EXTERNAL) &&
	     (!ast->symType->isConcrete()))
	    unmarkReached(errStream, def);
	}
	
 	if(def->astType == at_define)
	  getLetintoLetStream(errStream, def->children[1]);
	break;
      }

    default:
      assert(false);
      break;
    }
  }
}

void
getTypesIntoMainStream(std::ostream &errStream, AST *mod) 
{
  size_t n = mod->children.size();
  for(size_t c=0; c < n; c++) {
    AST *def = mod->children[c];
    switch(def->astType) {
    case at_defstruct:
    case at_defunion:
    case at_declstruct:
    case at_declunion:
    case at_defexception:
      {
	AST *ast = def->getID();
	if(ast->special != NULL && ast->special->size() > 0) {
	  unmarkReached(errStream, def);

	  for(size_t i=0; i < ast->special->size(); i++) {
	    spStruct *sp = (*ast->special)[i];
	    markReached(errStream, sp->ast);
	    
	    if(!sp->lifted) {
	      sp->lifted = true;
	      mod->children.append(sp->ast);
	    } 
	  }
	}
	break;
      }

    case at_define:
    case at_proclaim:
    case at_deftypeclass:
    case at_definstance:
      break;      

    default:
      assert(false);
      break;
    }
  }
}
 
void 
getIntoMainStream(std::ostream &errStream, AST *mod)
{
  getValuesIntoMainStream(errStream, mod);
  getTypesIntoMainStream(errStream, mod);
}
 
void
removeUnreached(AST *mod) 
{
  size_t c = (mod->astType == at_interface)?1:0; 
  for(; c < mod->children.size(); c++) {
    AST *def = mod->children[c];
    switch(def->astType) {
    case at_deftypeclass:
    case at_definstance:
      {
	mod->children.remove(c); 
	c--;	
	break;
      }

    default:
      {
	if(!def->reached) {
	  mod->children.remove(c); 
	  c--;
	}
      }      
    }
  }
}

void addType(AST *def, AST *mod, CVector<AST *> &deps);

void
addCompType(AST *ast, AST *mod, CVector<AST *> &deps)
{
  switch(ast->astType) {
  case at_ident:
    {
      AST *def = ast->symbolDef;
      if((def != NULL) && def->defForm)	
	addType(def->defForm, mod, deps);
      break;
    }
  default:
    {
      for(size_t c=0; c < ast->children.size(); c++)
	addCompType(ast->children[c], mod, deps);
    }
  }
}

void
addType(AST *def, AST *mod, CVector<AST *> &deps)
{
  if(!mod->children.contains(def) && !deps.contains(def)) {
    size_t n = deps.size();
    deps.append(def);
    switch(def->astType) {
    case at_defstruct:
    case at_defunion:
      addCompType(def->children[4], mod, deps);
      break;
	
    case at_defexception:
      for(size_t c = 1; c < def->children.size(); c++)
	addCompType(def->children[c], mod, deps);
      break;

    default:
      assert(false);
      break;
    }
    mod->children.append(def);
    assert(deps.size() == n + 1);
    deps.remove(n);
  }  
}

void
moveTypesUp(AST *start)
{
  AST *mod = start->children[0];
  AST *newMod = new AST(at_module, mod->loc);

  for(size_t c = 0; c < mod->children.size(); c++) {
    AST *def = mod->children[c];
    switch(def->astType) {
    case at_declstruct:
    case at_declunion:
      newMod->children.append(def);
      break;
      
    case at_define:
    case at_proclaim:
    case at_defstruct:
    case at_defunion:
    case at_defexception:      
      break;
      
    default:
      assert(false);
    }
  }

  // Types need to be added by honouring dependencies.
  // The original program order is NOT enough
  // because of type specializations.
  CVector<AST *> dep;
  for(size_t c = 0; c < mod->children.size(); c++) {
    AST *def = mod->children[c];
    switch(def->astType) {
    case at_defstruct:
    case at_defunion:
    case at_defexception:
      {
	assert(dep.size() == 0);
	addType(def, newMod, dep);
	break;
      }
      
    case at_declstruct:
    case at_declunion:
    case at_define:
    case at_proclaim:
      {
	break;
      }

    default:
      {
	assert(false);
      }
    }
  }

  for(size_t c = 0; c < mod->children.size(); c++) {
    AST *def = mod->children[c];
    switch(def->astType) {      
    case at_define:
    case at_proclaim:
      newMod->children.append(def);
      break;
    case at_defstruct:
    case at_defunion:
    case at_declstruct:
    case at_declunion:
    case at_defexception:
      break;
      
    default:
      assert(false);    
    }
  }

  start->children[0] = newMod;
}

bool 
checkInfTypedValues(AST *ast, std::ostream &errStream)
{
  bool errFree = true;

  
  switch(ast->astType) {    

  case at_ident:
    {
      // To the best of my knowledge, it is impossible to construct an 
      // infinite type without using an identifier. So, this case is
      // sufficiently checked within the ident case. 
      // Of course, I can write this check outside the case, but I am
      // writing it here because, if there is a counter example, I
      // would like to know.

      if(ast->symType &&
	 ast->symType->isOfInfiniteType()) {	
	errStream << ast->loc << ": " 
		  << "This expression has an infinite Type: "
		  << ast->symType->asString()
		  << ", and cannot be instantiated."
		  << std::endl;
	errFree = false;
      }

      break;
    }

  case at_field:			       
    {
      BE_CHKERR(errFree, checkInfTypedValues(ast->children[1], 
					  errStream));
      break;
    }

  case at_select:
    {
      BE_CHKERR(errFree, checkInfTypedValues(ast->children[0],
					  errStream));
      break;
    }

  case at_define:
  case at_proclaim:
    {
      for(size_t c = 0; errFree && (c < ast->children.size()); c++)
	BE_CHKERR(errFree, checkInfTypedValues(ast->children[c],
					    errStream));
      break;
    }
    
  case at_defstruct:
  case at_defunion:
  case at_declstruct:
  case at_declunion:
  case at_defexception:
    {
      break;
    }

  default:
    {
      for(size_t c = 0; errFree && (c < ast->children.size()); c++)
	BE_CHKERR(errFree, checkInfTypedValues(ast->children[c], 
					    errStream));
      break;
    }    
  }

  return errFree;
}

void
mangleIDs(AST *ast)
{
  switch(ast->astType) {
  case at_ident:
    {
      ast->s = ast->mangledString();
      break;
    }
    
  case at_declares:
    break;
    
  default:
    {
      for(size_t c = 0; c < ast->children.size(); c++)
	mangleIDs(ast->children[c]);
      break;
    }    
  }
}
 
static void
proclaimsAboveDefs(AST *ast)
{
  // Temporary fix so that I can run test cases, introduced
  // because of the new proclaimation rule. 

  // This is a really bad implementation, fix is comming ...
     
  AST *mod = ast->children[0];
  AST *newMod = new AST(at_module, mod->loc);

  for(size_t c=0; c < mod->children.size(); c++) {
    AST *ast = mod->children[c];
    switch(ast->astType) {
    case at_define:
    case at_defexception:
    case at_defstruct:
    case at_defunion:
      {
	for(size_t j=c+1; j < mod->children.size(); j++) {
	  AST *thisDef = mod->children[j];
	  if(ast->getID()->s == thisDef->getID()->s) {
	    newMod->children.append(thisDef);
	    mod->children.remove(j); j--;
	  }
	}
	    
	newMod->children.append(ast);
	break;
      }

    case at_proclaim:
    case at_declstruct:
    case at_declunion:      
      {
	newMod->children.append(ast);
	break;
      }
      
    default:
      {
	assert(false);
	break;
      }
    }
  }
}

bool
UocInfo::be_polyinstantiate(std::ostream &errStream, 
			    bool init, unsigned long flags)
{
  bool errFree = true;

  UocInfo *uoc = this;
  assert(uoc != NULL);
  AST *mod = uoc->ast->children[0]; 
  CVector<AST *> todo;
  
#if 0
  AST *main = lookForMain(uoc);
  if(main == NULL) 
    assert(Options::backEnd->flags & BK_HDR_MODE);
  
  if(main != NULL) {
    main = main->defForm;
    assert(main != NULL);
    markReached(errStream, main);
    add_todo(errStream, todo, main);
  }
#else
  for (size_t ep = 0; ep < Options::entryPts->size(); ep++) {
    std::string& epName = (*Options::entryPts)[ep];

    AST *mod = uoc->ast->children[0];
    for(size_t c = 0; c < mod->children.size(); c++) {
      if(mod->children[c]->astType != at_define)
	continue;

      AST *def = mod->children[c]->getID();
      if (def->fqn.asString() == epName) {
	def = def->defForm;
	assert(def != NULL);

	if (epName == "bitc.main.main")
	  UocInfo::mainIsDefined = true;

	markReached(errStream, def);
	add_todo(errStream, todo, def);
      }
    }
  }
#endif
  
  BE_CHKERR(errFree, checkInfTypedValues(mod, errStream));
  if(!errFree)
    return false;
    
  mangleIDs(mod);
  SpecialNames::spNames.fixUpSpNames(uoc);
  BE_CHKERR(errFree, reRandT(errStream, uoc, 
			  OP_SYM_FLAGS, OP_TYP_FLAGS, 
			  "Post mangle"));

  /* Top-level side-effecting expressions must not be dropped. There
     is talk about dis-allowing such expressions, and an analysis pass
     must be performed to check all such cases. For now, we must keep
     everything that is concrete as roots

     This will add some things like main to the todo list twice, 
     but ... */
  bool INCLUDE_ALL_CONCRETE_VALUES = true;

  for(size_t c = 0; c < mod->children.size(); c++) {
    AST *ast = mod->children[c];
    switch(ast->astType) {
    case at_proclaim:
      if((ast->getID()->Flags & DEF_IS_EXTERNAL) || 
	 (INCLUDE_ALL_CONCRETE_VALUES &&
	  ast->getID()->symType->isConcrete())) {
	markReached(errStream, ast);
	add_todo(errStream, todo, ast);
      }      
      break;

    case at_define:      
      {
	if(INCLUDE_ALL_CONCRETE_VALUES &&
	   ast->getID()->symType->isConcrete()) {	  
	  markReached(errStream, ast);
	  add_todo(errStream, todo, ast);
	}
	break;
      }
      
    case at_defexception:
      {
	markReached(errStream, ast);
	add_todo(errStream, todo, ast);
	break;
      }

    case at_declstruct:
    case at_declunion:
    case at_defstruct:
    case at_defunion:
      break;
      
    case at_deftypeclass:
    case at_definstance:
      break;

    default:
      {
	assert(false);
	break;
      }
    }
  }
 
  DEBUG {
    uoc->ast->asString();
    uoc->ShowTypes(std::cout);
    
    std::cout << "_______________________________________________"
	      << std::endl;
  }
  
  while(todo.size() > 0) {
    AST *ast = todo[todo.size() - 1];
    assert(ast != NULL);
    DEBUG std::cout << "Now Processing: " << ast->asString() << std::endl;
    todo.remove(todo.size() - 1);

    BE_CHKERR(errFree, polyinst(errStream, uoc, ast, mod, todo)); 
    DEBUG if(!errFree) {
      errStream << "Breaking because of an error " << std::endl;
      break;		
    }	   
    
    DEBUG {
      std::cout << "- - - - - - - - - - - - - - - - - - - - - - - -"
		<< std::endl;
      
      uoc->ast->asString();
      uoc->ShowTypes(std::cout);
    
      std::cout << "_______________________________________________"
		<< std::endl;
    }
  }

  getIntoMainStream(errStream, mod);
  removeUnreached(mod);
  /* mod changes in this area, pointer may be stale, use uoc->ast */
  moveTypesUp(uoc->ast);  
  clearConstraints(uoc->ast);
  proclaimsAboveDefs(uoc->ast);  
  if(errFree) 
    BE_CHKERR(errFree, reRandT(errStream, uoc));
  //DisplayTypes(std::cout, uoc);
  
  return errFree;
}


 
