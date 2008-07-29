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
#include <string>
#include <sstream>

#include <libsherpa/UExcept.hxx>

#include "Options.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "Unify.hxx"

using namespace std;
using namespace sherpa;

static GCPtr< CVector< GCPtr<Type> > > 
getDomain(GCPtr<Typeclass> t)
{
  GCPtr< CVector< GCPtr<Type> > > dom = new CVector< GCPtr<Type> >;
  
  for(size_t i=0; i < t->typeArgs.size(); i++)
    dom->append(t->TypeArg(i));
  
  for(TypeSet::iterator itr = t->fnDeps.begin(); 
      itr != t->fnDeps.end(); ++itr) {
    GCPtr<Type> fdep = (*itr);
    GCPtr<Type> ret = fdep->Ret();
      
    GCPtr< CVector< GCPtr<Type> > > newDom = 
      new CVector< GCPtr<Type> >;
      
    for(size_t i=0; i < dom->size(); i++)
      if(dom->elem(i)->getType() != ret->getType())
	newDom->append(dom->elem(i));
      
    dom = newDom;
  }
  
  return dom;
}

static GCPtr< CVector< GCPtr<Type> > >
getDomVars(GCPtr< CVector< GCPtr<Type> > > dom)
{
  GCPtr< CVector< GCPtr<Type> > > vars = new CVector< GCPtr<Type> >;
  
  for(size_t i=0; i < dom->size(); i++) {
    GCPtr<Type> arg = dom->elem(i)->getType();
    arg->collectAllftvs(vars);
  }
  
  return vars;
}

static bool
mustSolve(GCPtr< CVector< GCPtr<Type> > > dom)
{
  for(size_t i=0; i < dom->size(); i++) {
    GCPtr<Type> arg = dom->elem(i)->getType();
    if(arg->isTvar())
      return false;
  }
  
  return true;
}

static void
rigidify(GCPtr< CVector< GCPtr<Type> > > vars)
{  
  for(size_t i=0; i < vars->size(); i++) {
    GCPtr<Type> arg = vars->elem(i)->getType();
    assert(arg->kind == ty_tvar);
    arg->flags |= TY_RIGID;
  }
}

static void
unrigidify(GCPtr< CVector< GCPtr<Type> > > vars)
{  
  for(size_t i=0; i < vars->size(); i++) {
    GCPtr<Type> arg = vars->elem(i)->getType();
    assert(arg->kind == ty_tvar);
    arg->flags &= ~TY_RIGID;
  }
}


bool
handlePcst(std::ostream &errStream, GCPtr<Trail> trail,
	   GCPtr<Constraint> ct, GCPtr<Constraints> cset, 
	   bool &handled, bool &handlable)
{
  if(ct->isPcst()) {
    handlable = true;
  }
  else {
    handlable = false;
    handled = false;
    return true;
  }

  PCST_DEBUG errStream << "\t\tTrying PCST: " 
		       << ct->asString(Options::debugTvP)
		       << std::endl;
  
  GCPtr<Type> k = ct->CompType(0)->getType();
  GCPtr<Type> gen = ct->CompType(1)->getType();
  GCPtr<Type> ins = ct->CompType(2)->getType();
  
  // *(m, tg, ti)
  if(k == Type::Kmono) {
    PCST_DEBUG errStream << "\t\tCase *(m, tg, ti), CLEAR." 
			 << std::endl;
    cset->clearPred(ct);
    handled = true;
    bool unifies = ins->unifyWith(gen, false, trail, errStream);
    return unifies;
  }
  
  if (k == Type::Kpoly) {
    //                _
    // *(p, tg, ti), |_|(tg)
    if(gen->isConcretizable()) {
      PCST_DEBUG errStream << "\t\tCase *(p, tg, ti), [](tg), CLEAR."
			   << std::endl;
      cset->clearPred(ct);
      handled = true;
      GCPtr<Type> tgg = gen->minimizeDeepMutability();
      GCPtr<Type> tii = ins->minimizeDeepMutability();
      bool errFree = true;
      CHKERR(errFree, gen->unifyWith(tgg, false, trail, errStream));
      CHKERR(errFree, ins->unifyWith(tii, false, trail, errStream));
      return errFree;
    }
    
    // *(p, tg, ti),  Mut(ti)
    if (ins->isDeepMut()) {
      PCST_DEBUG errStream << "\t\tCase *(p, tg, ti), Mut(ti) ERROR." 
			   << std::endl;
      cset->clearPred(ct);
      handled = true;
      return false;
    }
    
    PCST_DEBUG errStream << "\t\tCase *(p, tg, ti), " 
			 << "Immut(ti) KEEP." 
			 << std::endl;
    
    // *(p, tg, ti), ~Immut(ti), ~Mut(ti)
    handled = false;
    return true;
  }
  
  assert(k->kind == ty_kvar);
  
  // *(k, tg, ti), Mut(ti)
  if(ins->isDeepMut()) {
    PCST_DEBUG errStream << "\t\tCase *(k, tg, ti), " 
			 << "Mut(ti) [k |-> m]." 
			 << std::endl;
    trail->subst(k, Type::Kmono);
    handled = true;
    return true;
  }

  // *(k, tg, ti), Immut(tg)
  if(gen->isDeepImmut()) {
    PCST_DEBUG errStream << "\t\tCase *(k, tg, ti), Immut(tg)" 
			 << "Immut(tg) [k |-> p]." 
			 << std::endl;
    trail->subst(k, Type::Kpoly);
    handled = true;
    return true;
  }

  /* U(*(k, tg, ti), *(k, tg, ti')), ti !=~= ti' */
  for (TypeSet::iterator itr = cset->begin(); 
       itr != cset->end(); ++itr) {
    GCPtr<Constraint> newCt = (*itr)->getType();
    if(newCt == ct)
      continue;

    if(!newCt->isPcst())
      continue;
    
    GCPtr<Type> newK = newCt->CompType(0)->getType();
    GCPtr<Type> newGen = newCt->CompType(1)->getType();
    GCPtr<Type> newIns = newCt->CompType(2)->getType();
    
    if(newK == k && !ins->equals(newIns)) {
      PCST_DEBUG errStream << "\t\tCase *(k, tg, ti), *(k, tg, tj)" 
			   << " ti !~~ tj, [k |-> p]." 
			   << std::endl;
      
      trail->subst(k, Type::Kpoly);
      handled = true;
      return true;
    }
  }
  
  PCST_DEBUG errStream << "\t\tCase *(k, tg, ti) KEEP." 
		       << std::endl;
  
  handled = false;
  return true;
}

bool
handleSpecialPred(std::ostream &errStream, GCPtr<Trail> trail,
		  GCPtr<Constraint> pred, GCPtr<Constraints> cset, 
		  bool &handled, bool &handlable)
{
  pred = pred->getType();
  if(pred->kind != ty_typeclass) {
    // This must be a pcst constraint;
    handlable = false;
    handled = false;
    return true;
  }
  
  // Special handling for ref-types
  // Safe to do name comparison, everyone includes the prelude.
  const std::string &ref_types = SpecialNames::spNames.sp_ref_types; 
  if(pred->defAst->s == ref_types) {
    handlable = true;
    assert(pred->typeArgs.size() == 1);
    GCPtr<Type> it = pred->TypeArg(0)->getType();

    SPSOL_DEBUG errStream << "\t\tCase RefTypes for "
			  << pred->asString(Options::debugTvP)
			  << std::endl;
    
    if(it->isRefType()) {
      SPSOL_DEBUG errStream << "\t\t ... Satisfied, CLEAR"
			    << pred->asString(Options::debugTvP)
			    << std::endl;
      cset->clearPred(pred);
      handled = true;
      return true;
    }
    
    if(it->isTvar()) { // checks beyond mutability, maybe-ness
      SPSOL_DEBUG errStream << "\t\t ... Variable, KEEP"
			    << pred->asString(Options::debugTvP)
			    << std::endl;
      handled = false;
      return true;
    }
    
    /* Value Type */
    SPSOL_DEBUG errStream << "\t\t ... Unboxed-type, ERROR"
			  << pred->asString(Options::debugTvP)
			  << std::endl;
    cset->clearPred(pred);
    handled = true;
    return false;
  }
  
  handlable = false;
  handled = false;
  return true;
}

bool
handleTCPred(std::ostream &errStream, GCPtr<Trail> trail,
	     GCPtr<Typeclass> pred, GCPtr<TCConstraints> tcc, 
	     GCPtr<const InstEnvironment > instEnv,
	     bool must_solve, bool trial_mode, bool &handled)
{
  TCSOL_DEBUG errStream << "\t\tInstance Solver for: "
			<< pred->asString(Options::debugTvP)
			<< (trial_mode ? " [TRIAL]" : "")
			<< std::endl;

  GCPtr<CVector<GCPtr<Instance> > > insts = 
    instEnv->getBinding(pred->defAst->fqn.asString());
  
  if(!insts) {
    TCSOL_DEBUG errStream << "\t\t ... No Instances in Environment"
			  << std::endl;
    if(must_solve) {
      tcc->clearPred(pred);
      handled = true;
      return false;
    }
    else {
      handled = false;
      return true;
    }
  }

  GCPtr<TypeScheme> instScheme = NULL;  
  for(size_t j=0; j < insts->size(); j++) {
    GCPtr<TypeScheme> ts = (insts->elem(j))->ts->ts_instance_copy();
    GCPtr<Type> inst = ts->tau;

    
    // FIX: This step must be performed ONLY for those
    // arguments that are used only at copy-compatible
    // positions. That is, if an argument is used in a 
    // method within a reference, this step must be 
    // skipped on that argument. 
    for(size_t c=0; c < inst->typeArgs.size(); c++)
      inst->TypeArg(c) = MBF(inst->TypeArg(c));
    
    if(pred->equals(inst)) {
      instScheme = ts;
      break;
    }
  }

  if(!instScheme) {
    TCSOL_DEBUG errStream << "\t\t ... No Suitable Instance found"
			  << std::endl;
    if(must_solve) {
      tcc->clearPred(pred);
      handled = true;
      return false;
    }
    else {
      handled = false;
      return true;
    }
  }
  
  if(trial_mode) {
    handled = false;
    return true;
  }
  
  bool errFree = pred->unifyWith(instScheme->tau);

  TCSOL_DEBUG errStream << "\t\t .. Post Unification with Instance: "
			<< pred->asString(Options::debugTvP)
			<< " CLEAR."
			<< std::endl;
  
  assert(errFree);  
  tcc->clearPred(pred);
  
  if(instScheme->tcc)
    for (TypeSet::iterator itr = instScheme->tcc->begin(); 
	 itr != instScheme->tcc->end(); ++itr) {
      GCPtr<Typeclass> instPred = (*itr);

      // Add all preconditions, except for the self-condition
      // added to all instances. Remember that the 
      // type specializer clears the TY_SELF flag.
      if(!pred->equals(instPred)) {
	tcc->addPred(instPred);
	TCSOL_DEBUG errStream << "\t\t .. Adding pre-condition: "
			      << instPred->asString(Options::debugTvP)
			      << std::endl;
      }
    }
  
  handled = true;
  return true;
}

bool
handleEquPreds(std::ostream &errStream, GCPtr<Trail> trail,
	       GCPtr<Typeclass> pred, GCPtr<TCConstraints> tcc, 
	       GCPtr< CVector< GCPtr<Type> > > vars,
	       bool &handled)
{
  // Equality of domain types in two type class predicated is achieved
  // by testing for unification wherein the type variables in the
  // domain are held rigid.
  handled = false;
  rigidify(vars);
  for (TypeSet::iterator itr = tcc->begin(); 
       itr != tcc->end(); ++itr) {
    GCPtr<Constraint> newCt = (*itr)->getType();
    if(newCt == pred)
      continue;
    
    if(pred->equals(newCt)) {
      TCSOL_DEBUG errStream << "\t\t EquPreds: "
			    << pred->asString(Options::debugTvP)
			    << " === "
			    << newCt->asString(Options::debugTvP)
			    << " UNIFY, CLEAR1."
			    << std::endl;
      
      pred->unifyWith(newCt);
      tcc->clearPred(pred);
      handled=true;
      break;
    }
  }
  unrigidify(vars);
  
  return true;
}


/**********************************************************
                   THE Constraint solver 

    The input is a set of constraints, which the solver tries to solve
    based on pre-defined rules or known instances. It returns the set
    of residual constraints that:
      -- are known to be solvable
      -- do not constain only concrete types
 
    The predicate solver is a unification based algorthm
    here are the steps to follow:
    1) Handle special prediactes like ref-types as though apropriate
       instances are present. 

    2) Handle the polymorphic constraint as a special case: 
       If we find a constraint
          2.a) c = *(m, t, t1) then Unify(t = t1), c is satisfied
          2.b) c = *(p, t, t1) | Immutable(t1), then c is satisfied
          2.c) c = *(k, t, t1) | mutable(t1), then k = m
          2.d) c1 = *(k, t, t1) and c2 = *(k, t, t2) | ~Unify(t1 = t2)
                   then k = p.
          2.e) c = *(k, t, t1) | Immut(t1), keep the constraint
          2.f) c = *(p, t, t1) | ~Immutable(t1) then error

    3) If there exists a constraint such that c = T(t1,...,tm, ... tn),
       where types tm+1 ... tn are determined by functional
       dependencies, 

       3.a) If t1 != ... != tm != 'a for any 'a, 
              If there exists a unifying instance I, then Unify(c=I).
                 The constraint c is declared satisfied.
              Else fail.

       3.b) Otherwise, let {tm*} <= {t1, ..., tm} be type variables. 

            3.b.i) If there exists a unifying instance I, such that 
                      the unification Unify(c=I) succeeds when {tm*} 
                      are held rigid, then Unify(c=I).
                      the constraint c is declared satisfied.

           3.b.ii) Otherwise, 
                   If there is a unifying instance I, keep
                      the constraint as is (do not unify)
                   Else fail.

       In this case, maybe types aer considered constrained type
       variables. That is, T('a|t) === T('a) | copy-compat('a, t).

    4) If there exists two constraints such that 
             c1 = T(t1,...,tm, ... tn), and
             c2 = T(t1',...,tm', ... tn'), 
       where types tm+1 ... tn, tm+1' ... tn' are determined by
       functional dependencies, Unify(c1 = c2).

 *********************************************************/


bool
TypeScheme::solvePredicates(std::ostream &errStream, const LexLoc &errLoc,
			    GCPtr< const InstEnvironment > instEnv,
			    GCPtr<Trail> trail)
{
/* handled: Signifies any changes to the tcc individual handler
   functions might have performed.
   
   errFree/errFreeNow: Decides correctness of current constraints

   In the case of and error, the rpedicate is removed, and handles
   must be true */
  bool errFree = true;
  bool handled = false;
  
  SOL_DEBUG errStream << "\tTo Solve: " 
		      << asString(Options::debugTvP)
		      << std::endl;
  
  do {
    handled = false;
    GCPtr<Typeclass> errPred = NULL;
    bool errFreeNow = true;
    
    for (TypeSet::iterator itr = tcc->begin(); 
	 itr != tcc->end(); ++itr) {
      GCPtr<Typeclass> pred = (*itr);
      errPred = pred;
      bool handlable = false;
      
      // Step 1
      CHKERR(errFreeNow, handleSpecialPred(errStream, trail, 
					   pred, tcc, 
					   handled, handlable));

      SOL_DEBUG errStream << "\t[Sol 1] (special): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;
      
      if(handled)
	break;
      if(handlable)
	continue;

      // Step 2
      CHKERR(errFreeNow, handlePcst(errStream, trail, 
				    pred, tcc, handled, handlable));
      
      SOL_DEBUG errStream << "\t[Sol 2] (pcst): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;

      if(handled)
	break;
      if(handlable)
	continue;
      
      GCPtr< CVector< GCPtr<Type> > > dom = getDomain(pred);
      GCPtr< CVector< GCPtr<Type> > > vars = getDomVars(dom);
      bool ms = mustSolve(dom);
      if(ms) {
 	// Step 3.a
	CHKERR(errFreeNow, handleTCPred(errStream, trail, pred, tcc,
					instEnv, ms, false, handled));
	SOL_DEBUG errStream << "\t[Sol 3.a] (must-solve): " 
			    << asString(Options::debugTvP)
			    << (handled ? " [HANDLED]" : "")
			    << (handlable ? " [HANDLABLE]" : "")
			    << (!errFreeNow ? " [ERROR]" : "")
			    << std::endl;

	if(handled)
	  break;
      }
      
      // Step 3.b.i
      rigidify(vars);
      handleTCPred(errStream, trail, pred, tcc,
		   instEnv, ms, false, handled);
      unrigidify(vars);
      SOL_DEBUG errStream << "\t[Sol 3.b.i] (exact): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;
      if(handled)
	break;
      
      // Step 3.b.ii
      CHKERR(errFreeNow, handleTCPred(errStream, trail, pred, tcc,
				      instEnv, false, true, handled));
      SOL_DEBUG errStream << "[Sol 3.b.ii] (solvability): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;
      if(handled)
	break;
      
      // Step 4
      CHKERR(errFreeNow, handleEquPreds(errStream, trail, 
					pred, tcc, vars, handled));
      SOL_DEBUG errStream << "[Sol 4] (equ-pred): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;      
      if(handled)
	break;
      
      if(!errFreeNow)
	assert(false);
    }
    
    if(!errFreeNow) {
      assert(handled);
      assert(errPred);
      errStream << errLoc << ": "
		<< "Unsatisfiable Constraint: "
		<< errPred->asString() 
		<< std::endl; 
    }
    
    CHKERR(errFree, errFreeNow);
    
  } while(handled);
  
  return errFree;
}
