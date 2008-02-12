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
#include <libsherpa/avl.hxx>
#include <assert.h>

#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "Unify.hxx"

using namespace sherpa;
using namespace std;

bool
Type::boundInType(GCPtr<Type> tv)
{
  GCPtr<Type> t = getType();
  
  if(t == tv->getType())
    return true;
   
  if(t->mark & MARK1)
    return false;
  
  t->mark |= MARK1;
  bool bound = false;
  
  for(size_t i=0; (!bound) && (i < t->components->size()); i++) 
    bound = t->CompType(i)->boundInType(tv);

  // To consider cases like (define aNil nil)
  for(size_t i=0; (!bound) && (i < t->typeArgs->size()); i++) 
    bound = t->TypeArg(i)->boundInType(tv);

  // Deal with fnDeps if present
  if(t->fnDeps)
    for(size_t i=0; (!bound) && (i < t->fnDeps->size()); i++) 
      bound = t->FnDep(i)->boundInType(tv);
  
  t->mark &= ~MARK1;
  return bound;
}

bool
Type::boundInGamma(GCPtr<const Environment<TypeScheme> > gamma)
{
  GCPtr<Type> tvar = getType();
  while (gamma) {
    for(size_t i = 0; i < gamma->bindings->size(); i++) {
      GCPtr<TypeScheme> sigma = gamma->bindings->elem(i)->val;

      for (size_t tv = 0; tv < sigma->ftvs->size(); tv++) {
	if (sigma->Ftv(tv)->uniqueID == tvar->uniqueID)
	  return true;
      }
      
      if(sigma->tau->boundInType(tvar))
	return true;
    }
    
    gamma = gamma->parent;
  }
  
  return false;
}

// Collect ALL ftvs regardless of gamma
// This APPENDS TO the vector `tvs'. IT IS NOT NECESSARY THAT
// `tvs' BE EMPTY TO START WITH. Don't rely on this.
void
Type::collectAllftvs(GCPtr<CVector<GCPtr<Type> > > tvs)
{
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK3)
    return;

  t->mark |= MARK3;
  
  if(t->kind == ty_tvar) {
    if(!(tvs->contains(t))) {
      tvs->append(t);
    }
  }      
  else {
    for(size_t i=0; i < t->components->size(); i++)
      t->CompType(i)->collectAllftvs(tvs);

    for(size_t i=0; i < t->typeArgs->size(); i++)
      t->TypeArg(i)->collectAllftvs(tvs);

    if(t->fnDeps)
      for(size_t i=0; i < t->fnDeps->size(); i++)
	t->FnDep(i)->collectAllftvs(tvs);
  }

  t->mark &= ~MARK3;
}
 
// Collects ftvs wrt the basic type and TC predicates 
void
TypeScheme::collectAllFtvs()
{
  tau->collectAllftvs(ftvs);  
  if(tcc) {
    for(size_t i=0; i < tcc->pred->size(); i++)
      tcc->Pred(i)->collectAllftvs(ftvs);      
  }  
}

// Collect the Free Type Variables in a type
// that are unbound in gamma
void
Type::collectftvsWrtGamma(GCPtr<CVector<GCPtr<Type> > > tvs,
			  GCPtr<const Environment<TypeScheme> > gamma)
{   
  GCPtr<Type> t = getType();

  if(t->mark & MARK2)
    return;

  t->mark |= MARK2;

  if(t->kind == ty_tvar) {
    assert(t->components->size() == 0);
    if(!t->boundInGamma(gamma) && !(tvs->contains(t))) 
      tvs->append(t);
  }
  else {
    for(size_t i=0; i < t->components->size(); i++)      
      t->CompType(i)->collectftvsWrtGamma(tvs, gamma);
    
    for(size_t i=0; i < t->typeArgs->size(); i++)
      t->TypeArg(i)->collectftvsWrtGamma(tvs, gamma);

    if(t->fnDeps)
      for(size_t i=0; i < t->fnDeps->size(); i++) 
	t->FnDep(i)->collectftvsWrtGamma(tvs, gamma);
  }

  t->mark &= ~MARK2;
}
 

// Remove Free Type Variables that are indirectly influenced
// by type variablles that are bound in Gamma through
// Functional Dependencies
static void
remftvsWrtFnDeps(GCPtr<CVector<GCPtr<Type> > > &ftvs,
		 const GCPtr<CVector<GCPtr<Type> > > fnDeps,
		 GCPtr<const Environment<TypeScheme> > gamma)
{
  // closure wrt tvs in fnDeps influenced by Gamma.
  GCPtr<CVector<GCPtr<Type> > > closure = 
    new CVector<GCPtr<Type> >;

  for(size_t i=0; i < fnDeps->size(); i++) {
    GCPtr<Type> fnDep = fnDeps->elem(i);
    GCPtr<CVector<GCPtr<Type> > > tvs =
      new CVector<GCPtr<Type> >;
    fnDep->collectAllftvs(tvs);
    for(size_t j=0; j < tvs->size(); j++) {
      GCPtr<Type> tv = (*tvs)[j];
      if(!closure->contains(tv) && tv->boundInGamma(gamma))
	closure->append(tv);
    }
  }

  TCConstraints::close(closure, fnDeps);
  
  GCPtr<CVector<GCPtr<Type> > > newFtvs = new CVector<GCPtr<Type> >;
  for(size_t i=0; i < ftvs->size(); i++) {
    GCPtr<Type> ftv = ftvs->elem(i)->getType();
    if(!closure->contains(ftv))
      newFtvs->append(ftv);
  }

  ftvs = newFtvs;
}

// Collect the Free Type Variables in a type
// that are unbound in gamma
void
TypeScheme::collectftvs(GCPtr<const Environment<TypeScheme> > gamma)
{
  tau->collectftvsWrtGamma(ftvs, gamma);  
  std::cerr << "tau = "
	    << tau->asString(Options::debugTvP)
	    << std::endl
	    << "tau's ftvs = ";

  for(size_t i=0; i < ftvs->size(); i++)
    std::cerr << Ftv(i)->asString(Options::debugTvP);
  
  std::cerr << std::endl;
  
  if(tcc) {    
    for(size_t i=0; i < tcc->pred->size(); i++) {
      GCPtr<Typeclass> pred = tcc->Pred(i);
      pred->collectftvsWrtGamma(ftvs, gamma);  
      
      std::cerr << "pred = "
		<< pred->asString(Options::debugTvP)
		<< std::endl
		<< "SUM ftvs = ";
      
      for(size_t i=0; i < ftvs->size(); i++)
	std::cerr << Ftv(i)->asString(Options::debugTvP);
      
      std::cerr << std::endl;
    }
 
    GCPtr<CVector<GCPtr<Type> > > allFnDeps = new CVector<GCPtr<Type> >;
    tcc->collectAllFnDeps(allFnDeps);
    remftvsWrtFnDeps(ftvs, allFnDeps, gamma);
  }
}

bool isExpansive(std::ostream& errStream, 
		GCPtr<const Environment<TypeScheme> > gamma,
		GCPtr<const AST> ast);
bool isExpansive(std::ostream& errStream, 
		 GCPtr<const Environment<TypeScheme> > gamma,
		 GCPtr<Type> typ);



/**********************************************************
                   THE Type Generalizer 

   The generalizer returns true if all free-type-variables could be
   completely generalized, false otherwise.

   Here is the Type generalization algorithm:
   Input is a type t and a set of constraints C, wrt to 
   the current let expression let(k) x = e in ...

   1) Add the polymorphic instantiation constraint *(k, t, t) to C
      This step is not performed for instance generalizations.      

   2) Solve predicates: let (t', C') = SolvePredicates(C)
      The constraint set C' contains residual constraints. It cannot
      contain any constraints over concrete types.
      This step is not performed for instance generalizations.      

   3) In case of top-level definitions, make a choice for all
      *-constraints: The type is made immutable upto the function
      boundary, and all 'ks are resolved to polymorphic.

   4) Determine the set of generalizable type variables:
      'a* = (FTVS(t') U FTVS(C')) \ FTVS(gamma)

   5) Check for value restriction: 
      exp = isExpansive(e) || isExpansive(t')

   6) If expansive, remove the set of non-generalizable type
      variables from 'a*.
      let 'b* = 'a* \ remove-restricted('a*)
      Here, we follow Ocaml-like relaxed-value restriction
      rule. Otherwise, {'b*} = {}

   7) Migrate appropriate constraints to parent's TCC, if one exists
      let C'' = migrate(parent-sigma, C')
      --> Constraints purely over monomorphic type variables can be
          migrated to the containing scope.

   8) Check for ambiguous types. If there exists a 'a such that 
      'a in {'b*} and 'a in FTVS(C'') and 'a not in FTVS(t'), then the
      type is ambiguous. 
    
   9) The generalized type is forall 'b*, t' \ C''

 *********************************************************/

bool
TypeScheme::generalize(std::ostream& errStream, 
		       LexLoc &errLoc,
		       GCPtr<const Environment<TypeScheme> > gamma,
		       GCPtr<const Environment<CVector<GCPtr<Instance> > > >
		       instEnv, 
		       GCPtr<const AST> expr, 
		       GCPtr<TCConstraints> parentTCC,
		       GCPtr<Trail> trail,
		       const GeneralizeMode gen_mode)
{
  bool errFree = true;

  
  GEN_DEBUG errStream << "[0] To Generalize " 
		      << asString(Options::debugTvP)
		      << " for expression "
		      << expr->asString() 
		      << std::endl;

  if(gen_mode != gen_instance) {
    // Step 1
    GCPtr<Type> pcst = new Constraint(ty_pcst, tau->ast); 
    pcst->components->append(new comp(new Type(ty_kvar, tau->ast)));
    pcst->components->append(new comp(tau)); // General Type
    pcst->components->append(new comp(tau)); // Instantiation Type
    tcc->addPred(pcst);
    
    GEN_DEBUG errStream << "[1] With Pcst: " 
			<< asString(Options::debugTvP)
			<< std::endl;

    // Step 2
    if(tcc)
      CHKERR(errFree, solvePredicates(errStream, errLoc, 
				      instEnv, trail)); 
    
    GEN_DEBUG errStream << "[2] Solve: " 
			<< asString(Options::debugTvP)
			<< std::endl;
  }

  if(gen_mode == gen_top) {
    // Step 3
    tau->adjMaybe(trail, true);
    
    for(size_t i=0; i < tcc->size(); i++) {
      GCPtr<Type> pred = tcc->Pred(i);
      if(!pred->isPcst())
	continue;

      GCPtr<Type> k = pred->CompType(0)->getType();
      GCPtr<Type> ins = pred->CompType(2)->getType();
      if(k->kind == ty_kvar) {
	ins->adjMaybe(trail, true);
	trail->subst(k, Type::Kpoly);
      }
    }
    
    CHKERR(errFree, solvePredicates(errStream, errLoc, 
				    instEnv, trail)); 

    for(size_t i=0; i < tcc->size(); i++) {
      GCPtr<Type> pred = tcc->Pred(i);
      if(pred->isPcst()) {
	errStream << "REMAINING PCST!!!"
		  << std::endl;
      }
    }
   
    GEN_DEBUG errStream << "[3] Top-Fix: " 
			<< asString(Options::debugTvP)
			<< std::endl;
  }
  
  // Step 4
  collectftvs(gamma);
  
  GEN_DEBUG errStream << "[4] CollectFtvs: " 
		      << asString(Options::debugTvP)
		      << std::endl;    
  
  // Step 5
  bool exprExpansive = isExpansive(errStream, gamma, expr);
  bool typExpansive = isExpansive(errStream, gamma, tau);
  bool fullyGeneral = true;

  GEN_DEBUG if(exprExpansive)
    errStream << "[5] " << expr->asString() 
	      << " is expansive"
	      << std::endl;
  
  GEN_DEBUG if(typExpansive)
    errStream << "[5] " << tau->asString(Options::debugTvP) 
	      << "is expansive"
	      << std::endl;

  // Step 6
  GCPtr<CVector<GCPtr<Type> > > removedFtvs = 
    new CVector<GCPtr<Type> >;
  if(exprExpansive || typExpansive) 
    tau->removeRestricted(ftvs, false, removedFtvs);
  
  if(removedFtvs->size()) 
    fullyGeneral = false;
  
  if ((gen_mode == gen_top) && !fullyGeneral) {
    for(size_t i=0; i < removedFtvs->size(); i++) {
      GCPtr<Type> ftv = (*removedFtvs)[i]->getType();
      if(ftv->kind == ty_tvar)
	ftv->link = new Type(ty_dummy, ftv->ast);
    }      
    
    errStream << errLoc << ": WARNING: The type of"
	      << " this toplevel definition "
	      << expr->asString() << " "
	      << " cannot be fully generalized"
	      << " due to the value restriction."
	      << " The type obtained is: "
	      << tau->asString() << "."
	      << std::endl;    
  }

  GEN_DEBUG errStream << "[6] Value Restriction: " 
		      << asString(Options::debugTvP)
		      << std::endl;

  // Step 7
  migratePredicates(parentTCC);
  
  GEN_DEBUG errStream << "[7] Migrated Constraints: " 
		      << asString(Options::debugTvP)
		      << std::endl;
  
  // Step 8
  CHKERR(errFree, checkAmbiguity(errStream, errLoc));
  
  // Step 9
  GEN_DEBUG errStream << "FINAL: " 
		      << asString(Options::debugTvP)
		      << std::endl 
		      << std::endl;
  return errFree;
}


/* THE Type Specializer */     

// The following function has the distinction of being the most
// debugged (and most vulnerale) function.
GCPtr<Type> 
Type::TypeSpecializeReal(GCPtr<CVector<GCPtr<Type> > > ftvs,
			 GCPtr<CVector<GCPtr<Type> > > nftvs)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> theType = new Type(t);
  theType->flags &= ~TY_SP_MASK;
  theType->typeArgs->erase();
  theType->components->erase();
  theType->fnDeps = NULL;
  // Note `hints' is linked to the ORIGINAL VALUE here.
  GCPtr<Type> retType = theType;
  
  INS_DEBUG std::cout << "To Specialize " 
		      << '`' << ast->s << '\''
		      << this->asString()  
		      << std::endl;  

  if(t->sp)
    retType = t->sp;
  else {    
    t->sp = retType;
  
    switch(t->kind) {    
    case ty_kvar:
      {
	retType = t;
	break;
      }
    case ty_pcst:
      {
	// the let-kind and generic type are added as is.
	theType->components->append(new comp(t->CompType(0)));
	theType->components->append(new comp(t->CompType(1)));
	// The instance of the constraint is specialized.
	GCPtr<Type> ins = t->CompType(2)->TypeSpecializeReal(ftvs, nftvs);
	theType->components->append(new comp(ins));
	break;
      }
    case ty_tvar:
      {
	size_t i=0;
	for(i=0; i<ftvs->size(); i++) {
	  GCPtr<Type> ftv = ftvs->elem(i)->getType();	  
	  if(ftv->kind == ty_tvar && t->uniqueID == ftv->uniqueID) {
	    theType->link = nftvs->elem(i); 
	    break;
	  }
	}
	
	// If the variable was NOT in ftv list, then 
	// we should link it to the original, in order to honor
	// variable capture
	if(i == ftvs->size())
	  theType->link = t;
      	break;
      }

    default:
      {      
	/* Deal with Type-args */
	for(size_t i=0; i<t->typeArgs->size(); i++) {
	  GCPtr<Type> arg = t->TypeArg(i)->getType();
	  GCPtr<Type> newArg = arg->TypeSpecializeReal(ftvs, nftvs);
	  
	  theType->typeArgs->append(newArg);
	}
            
	/* Deal with Components */
	for(size_t i=0; i<t->components->size(); i++) {
	  comp *nComp = 
	    new comp(t->CompName(i),
		     t->CompType(i)->TypeSpecializeReal(ftvs, nftvs),
		     t->CompFlags(i));
	  theType->components->append(nComp);
	}

	/* Deal with fnDeps if any */
	if(t->fnDeps) {
	  theType->fnDeps = new CVector<GCPtr<Type> >;

	  for(size_t i=0; i<t->fnDeps->size(); i++) {
	    GCPtr<Type> fnDep = t->FnDep(i)->TypeSpecializeReal(ftvs, nftvs);
	    theType->addFnDep(fnDep);
	  }
	}
	
	break;
      }      
    }
    //t->sp =  NULL;
  }
  
  INS_DEBUG std::cout << "\t Specialized " 
		      << '`' << ast->s << '\''
		      << getType()->asString(NULL) 
		      << " to " 
		      << retType->getType()->asString(NULL) 
		      << std::endl;
  
  return retType;
}

// Clear the sp (specialization) field of type records recursively.
void
Type::clear_sp()
{
  GCPtr<Type> t = getType();
  if(!t->sp)
    return;

  t->sp = NULL;

  for(size_t i=0; i<t->typeArgs->size(); i++)
    t->TypeArg(i)->clear_sp();

  for(size_t i=0; i<t->components->size(); i++)
    t->CompType(i)->clear_sp();

  if(t->fnDeps)
    for(size_t i=0; i<t->fnDeps->size(); i++)
      t->FnDep(i)->clear_sp();
}


GCPtr<Type> 
Type::TypeSpecialize(GCPtr<CVector<GCPtr<Type> > > ftvs,
		     GCPtr<CVector<GCPtr<Type> > > nftvs)
{
  GCPtr<Type> specializedType = TypeSpecializeReal(ftvs, nftvs);
  clear_sp();
  return specializedType;
}


/* Helper routines to generalize a pattern */
void
updateSigmas(GCPtr<const AST> bp, GCPtr<CVector<GCPtr<Type> > > ftvs,
	     GCPtr<TCConstraints> tcc)
{
  switch(bp->astType) {
  case at_identPattern:
    {
      GCPtr<AST> ident = bp->child(0);
      assert(ident->scheme);
      for(size_t i=0; i<ftvs->size(); i++) {
	if(ident->symType->boundInType(ftvs->elem(i)))
	  ident->scheme->ftvs->append(ftvs->elem(i));
      }
      ident->scheme->tcc = tcc;
      break;
    }
    
  case at_letGather:
    {
      for (size_t c = 0; c < bp->children->size(); c++)
	updateSigmas(bp->child(c), ftvs, tcc);
      break;
    }

  default:
    {
      assert(false);
      break;
    }
  }
}


/* This is the routine that any definition involving a pattern
   biunding, including identPattern must call. */
bool
generalizePat(std::ostream& errStream,
	      LexLoc &errLoc,
	      GCPtr<Environment<TypeScheme> > gamma,
	      GCPtr<const Environment< CVector<GCPtr<Instance> > > > instEnv,
	      GCPtr<AST> bp, GCPtr<AST> expr,
	      GCPtr<TCConstraints> tcc,
	      GCPtr<TCConstraints> parentTCC,
	      GCPtr<Trail> trail)
{
  bool errFree = true;

  // Make a temporary typeScheme for the pattern.
  // Individual identifiers' TypeScheme will be updated after the 
  // pattern is generalized as a whole.
  GCPtr<TypeScheme> sigma = new TypeScheme(bp->symType, tcc);
  
  CHKERR(errFree, 
	 sigma->generalize(errStream, errLoc, 
			   gamma, instEnv, expr, parentTCC,
			   trail, gen_local));
  
  updateSigmas(bp, sigma->ftvs, tcc);

  // Puts the letgather type here.
  expr->symType = bp->symType = sigma->tau;
  expr->scheme = bp->scheme = sigma;

  return errFree;
}
