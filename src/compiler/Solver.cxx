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
using namespace boost;
using namespace sherpa;

static TypeSet
getDomain(shared_ptr<Typeclass> t)
{
  TypeSet dom;
  
  for (size_t i=0; i < t->typeArgs.size(); i++)
    dom.insert(t->TypeArg(i));
  
  for (TypeSet::iterator itr = t->fnDeps.begin(); 
      itr != t->fnDeps.end(); ++itr) {
    shared_ptr<Type> fdep = (*itr);
    shared_ptr<Type> ret = fdep->Ret();
      
    TypeSet newDom;
      
    for (TypeSet::iterator itr_d = dom.begin(); itr_d != dom.end(); ++itr_d)
      if ((*itr_d)->getType() != ret->getType())
	newDom.insert(*itr_d);
      
    dom = newDom;
  }
  
  return dom;
}

static TypeSet
getDomVars(const TypeSet& dom)
{
  TypeSet vars;
  
  for (TypeSet::iterator itr = dom.begin(); itr != dom.end(); ++itr) {
    shared_ptr<Type> arg = (*itr)->getType();
    arg->collectAllftvs(vars);
  }
  
  return vars;
}

static bool
mustSolve(const TypeSet& dom)
{
  for (TypeSet::iterator itr = dom.begin(); itr != dom.end(); ++itr) {
    shared_ptr<Type> arg = (*itr)->getType();
    if (arg->isVariable())
      return false;
  }
  
  return true;
}

static void
rigidify(TypeSet& vars)
{  
  for (TypeSet::iterator itr = vars.begin(); itr != vars.end(); ++itr) {
    shared_ptr<Type> arg = (*itr)->getType();
    assert(arg->kind == ty_tvar);
    arg->flags |= TY_RIGID;
  }
}

static void
unrigidify(TypeSet& vars)
{  
  for (TypeSet::iterator itr = vars.begin(); itr != vars.end(); ++itr) {
    shared_ptr<Type> arg = (*itr)->getType();
    assert(arg->kind == ty_tvar);
    arg->flags &= ~TY_RIGID;
  }
}


bool
handlePcst(std::ostream &errStream, shared_ptr<Trail> trail,
	   shared_ptr<Constraint> ct, shared_ptr<Constraints> cset, 
	   bool &handled, bool &handlable)
{
  if (ct->isPcst()) {
    handlable = true;
  }
  else {
    handlable = false;
    handled = false;
    return true;
  }

  DEBUG(PCST) errStream << "\t\tTrying PCST: " 
		       << ct->asString(Options::debugTvP)
		       << std::endl;
  
  shared_ptr<Type> k = ct->CompType(0)->getType();
  shared_ptr<Type> gen = ct->CompType(1)->getType();
  shared_ptr<Type> ins = ct->CompType(2)->getType();
  
  // *(m, tg, ti)
  if (k == Type::Kmono) {
    DEBUG(PCST) errStream << "\t\tCase *(m, tg, ti), CLEAR." 
			 << std::endl;
    cset->clearPred(ct);
    handled = true;
    bool unifies = ins->unifyWith(gen, false, trail, errStream);
    return unifies;
  }
  
  if (k == Type::Kpoly) {
    //                _
    // *(p, tg, ti), |_|(tg)
    if (gen->isConcretizable()) {
      DEBUG(PCST) errStream << "\t\tCase *(p, tg, ti), [](tg), CLEAR."
			   << std::endl;
      cset->clearPred(ct);
      handled = true;
      shared_ptr<Type> tgg = gen->minimizeDeepMutability();
      shared_ptr<Type> tii = ins->minimizeDeepMutability();
      bool errFree = true;
      CHKERR(errFree, gen->unifyWith(tgg, false, trail, errStream));
      CHKERR(errFree, ins->unifyWith(tii, false, trail, errStream));
      return errFree;
    }
    
    // *(p, tg, ti),  Mut(ti)
    if (ins->isDeepMut()) {
      DEBUG(PCST) errStream << "\t\tCase *(p, tg, ti), Mut(ti) ERROR." 
			   << std::endl;
      cset->clearPred(ct);
      handled = true;
      return false;
    }
    
    DEBUG(PCST) errStream << "\t\tCase *(p, tg, ti), " 
			 << "Immut(ti) KEEP." 
			 << std::endl;
    
    // *(p, tg, ti), ~Immut(ti), ~Mut(ti)
    handled = false;
    return true;
  }
  
  assert(k->kind == ty_kvar);
  
  // *(k, tg, ti), Mut(ti)
  if (ins->isDeepMut()) {
    DEBUG(PCST) errStream << "\t\tCase *(k, tg, ti), " 
			 << "Mut(ti) [k |-> m]." 
			 << std::endl;
    trail->subst(k, Type::Kmono);
    handled = true;
    return true;
  }

  // *(k, tg, ti), Immut(tg)
  if (gen->isDeepImmut()) {
    DEBUG(PCST) errStream << "\t\tCase *(k, tg, ti), Immut(tg)" 
			 << "Immut(tg) [k |-> p]." 
			 << std::endl;
    trail->subst(k, Type::Kpoly);
    handled = true;
    return true;
  }

  /* U(*(k, tg, ti), *(k, tg, ti')), ti !=~= ti' */
  for (TypeSet::iterator itr = cset->begin(); 
       itr != cset->end(); ++itr) {
    shared_ptr<Constraint> newCt = (*itr)->getType();
    if (newCt == ct)
      continue;

    if (!newCt->isPcst())
      continue;
    
    shared_ptr<Type> newK = newCt->CompType(0)->getType();
    shared_ptr<Type> newGen = newCt->CompType(1)->getType();
    shared_ptr<Type> newIns = newCt->CompType(2)->getType();
    
    if (newK == k && !ins->equals(newIns)) {
      DEBUG(PCST) errStream << "\t\tCase *(k, tg, ti), *(k, tg, tj)" 
			   << " ti !~~ tj, [k |-> p]." 
			   << std::endl;
      
      trail->subst(k, Type::Kpoly);
      handled = true;
      return true;
    }
  }
  
  DEBUG(PCST) errStream << "\t\tCase *(k, tg, ti) KEEP." 
		       << std::endl;
  
  handled = false;
  return true;
}

bool
handleSpecialPred(std::ostream &errStream, shared_ptr<Trail> trail,
		  shared_ptr<Constraint> pred, shared_ptr<Constraints> cset, 
		  bool &handled, bool &handlable)
{
  bool errFree = true;
  pred = pred->getType();

  do{//dummy loop, so that we can break in between

    if (pred->kind != ty_typeclass) {
      handlable = false;
      handled = false;
      break;
    }

    // Safe to do name comparison, everyone includes the prelude.
    const std::string &ref_types = SpecialNames::spNames.sp_ref_types; 
    const std::string &has_field = SpecialNames::spNames.sp_has_field; 
    
    if (pred->defAst->s == ref_types) {
      handlable = true;
      DEBUG(SPSOL) errStream << "\t\tCase RefTypes for "
			    << pred->asString(Options::debugTvP)
			    << std::endl;

      shared_ptr<Type> it = pred->TypeArg(0)->getType();
      if (it->isVariable()) { // checks beyond mutability, maybe-ness
	DEBUG(SPSOL) errStream << "\t\t ... Variable, KEEP"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
	handled = false;
	break;
      }

      handled = true;
      if (it->isRefType()) {
	DEBUG(SPSOL) errStream << "\t\t ... Satisfied, CLEAR"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
	break;
      }

      /* Otherwise, we have a Value Type */
      DEBUG(SPSOL) errStream << "\t\t ... Unboxed-type, ERROR"
			    << pred->asString(Options::debugTvP)
			    << std::endl;
      
      errFree = false;      
      break;
    }
    
    if(pred->defAst->s == has_field) {
      handlable = true;
      DEBUG(SPSOL) errStream << "\t\tCase has-field for "
			    << pred->asString(Options::debugTvP)
			    << std::endl;
      
      shared_ptr<Type> st = pred->TypeArg(0)->getBareType();
      shared_ptr<Type> fName = pred->TypeArg(1)->getType();
      shared_ptr<Type> fType = pred->TypeArg(2)->getType();
    
      if(st->isVariable()) {
	DEBUG(SPSOL) errStream << "\t\t ... Variable, KEEP"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
	handled = false;
	break;
      }
      
      handled = true;
      if (st->kind != ty_structv && st->kind != ty_structr) {
	DEBUG(SPSOL) errStream << "\t\t ... Non-structure type, ERROR"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
	errFree = false;
	break;
      }
      
      if (fName->kind != ty_field) {
	DEBUG(SPSOL) errStream << "\t\t ... Non-field type, ERROR"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
	errFree = false;
	break;
      }
      
      shared_ptr<Type> fld = GC_NULL;
      for (size_t i=0; i < st->components.size(); i++)
	if (st->CompName(i) == fName->litValue.s)
	  if((st->CompFlags(i) & COMP_INVALID) == 0) {
	    fld = st->CompType(i)->getType();
	    break;
	  }
      
      
      for (size_t i=0; i < st->methods.size(); i++)
	if (st->MethodName(i) == fName->litValue.s)
	  if((st->MethodFlags(i) & COMP_INVALID) == 0) {
	    fld = st->MethodType(i)->getType()->getDCopy();

	    // Form a function type for unification with constraint.
	    assert(fld->kind == ty_method);
	    fld->kind = ty_fn;
	    break;
	  }
      
      if(!fld) {
	DEBUG(SPSOL) errStream << "\t\t ... Field/Method not found, ERROR"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
	errFree = false;
	break;
      }
      
      CHKERR(errFree, fType->unifyWith(fld));
      if(!errFree)
	DEBUG(SPSOL) errStream << "\t\t ... Field Unification failure, ERROR"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
      else
	DEBUG(SPSOL) errStream << "\t\t ... Field found, CLEAR"
			      << pred->asString(Options::debugTvP)
			      << std::endl;
      break;
    }
    
    // This constraint is non of the above checked special type
    // classes.
    handlable = false;
    handled = false;
    break;
  }while(false);
  
  if(handled)
    cset->clearPred(pred);

  return errFree;
}

bool
handleTCPred(std::ostream &errStream, shared_ptr<Trail> trail,
	     shared_ptr<Typeclass> pred, shared_ptr<TCConstraints> tcc, 
	     shared_ptr<const InstEnvironment > instEnv,
	     bool must_solve, bool trial_mode, bool &handled)
{
  DEBUG(TCSOL) errStream << "\t\tInstance Solver for: "
			<< pred->asString(Options::debugTvP)
			<< (trial_mode ? " [TRIAL]" : "")
			<< std::endl;

  shared_ptr<set<shared_ptr<Instance> > > insts = 
    instEnv->getBinding(pred->defAst->fqn.asString());
  
  if (!insts) {
    DEBUG(TCSOL) errStream << "\t\t ... No Instances in Environment"
			  << std::endl;
    if (must_solve) {
      tcc->clearPred(pred);
      handled = true;
      return false;
    }
    else {
      handled = false;
      return true;
    }
  }

  shared_ptr<TypeScheme> instScheme = GC_NULL;  
  for (set<shared_ptr<Instance> >::iterator itr_j = insts->begin();
      itr_j != insts->end(); ++itr_j) {
    shared_ptr<TypeScheme> ts = (*itr_j)->ts->ts_instance();
    shared_ptr<Type> inst = ts->tau;

    
    // FIX: This step must be performed ONLY for those
    // arguments that are used only at copy-compatible
    // positions. That is, if an argument is used in a 
    // method within a reference, this step must be 
    // skipped on that argument. 
    for (size_t c=0; c < inst->typeArgs.size(); c++)
      inst->TypeArg(c) = MBF(inst->TypeArg(c));
    
    if (pred->equals(inst)) {
      instScheme = ts;
      break;
    }
  }

  if (!instScheme) {
    DEBUG(TCSOL) errStream << "\t\t ... No Suitable Instance found"
			  << std::endl;
    if (must_solve) {
      tcc->clearPred(pred);
      handled = true;
      return false;
    }
    else {
      handled = false;
      return true;
    }
  }
  
  if (trial_mode) {
    handled = false;
    return true;
  }
  
  bool errFree = pred->unifyWith(instScheme->tau);

  DEBUG(TCSOL) errStream << "\t\t .. Post Unification with Instance: "
			<< pred->asString(Options::debugTvP)
			<< " CLEAR."
			<< std::endl;
  
  assert(errFree);  
  tcc->clearPred(pred);
  
  if (instScheme->tcc)
    for (TypeSet::iterator itr = instScheme->tcc->begin(); 
	 itr != instScheme->tcc->end(); ++itr) {
      shared_ptr<Typeclass> instPred = (*itr);

      // Add all preconditions, except for the self-condition
      // added to all instances. Remember that the 
      // type specializer clears the TY_SELF flag.
      if (!pred->equals(instPred)) {
	tcc->addPred(instPred);
	DEBUG(TCSOL) errStream << "\t\t .. Adding pre-condition: "
			      << instPred->asString(Options::debugTvP)
			      << std::endl;
      }
    }
  
  handled = true;
  return true;
}

static bool
handleEquPreds(std::ostream &errStream, shared_ptr<Trail> trail,
	       shared_ptr<Typeclass> pred, shared_ptr<TCConstraints> tcc, 
	       TypeSet& vars,
	       bool &handled)
{
  // Equality of domain types in two type class predicated is achieved
  // by testing for unification wherein the type variables in the
  // domain are held rigid.
  handled = false;
  rigidify(vars);
  for (TypeSet::iterator itr = tcc->begin(); 
       itr != tcc->end(); ++itr) {
    shared_ptr<Constraint> newCt = (*itr)->getType();
    if (newCt == pred)
      continue;
    
    if (pred->equals(newCt)) {
      DEBUG(TCSOL) errStream << "\t\t EquPreds: "
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
			    shared_ptr< const InstEnvironment > instEnv,
			    shared_ptr<Trail> trail)
{
/* handled: Signifies any changes to the tcc individual handler
   functions might have performed.
   
   errFree/errFreeNow: Decides correctness of current constraints

   In the case of and error, the pedicate is removed, and handles
   must be true */
  bool errFree = true;
  bool handled = false;
  
  DEBUG(SOL) errStream << "\tTo Solve: " 
		      << asString(Options::debugTvP)
		      << std::endl;
  
  do {
    handled = false;
    shared_ptr<Typeclass> errPred = GC_NULL;
    bool errFreeNow = true;
    
    for (TypeSet::iterator itr = tcc->begin(); 
	 itr != tcc->end(); ++itr) {
      shared_ptr<Typeclass> pred = (*itr);
      errPred = pred;
      bool handlable = false;
      
      // Step 1
      CHKERR(errFreeNow, handleSpecialPred(errStream, trail, 
					   pred, tcc, 
					   handled, handlable));

      DEBUG(SOL) errStream << "\t[Sol 1] (special): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;
      
      if (handled)
	break;
      if (handlable)
	continue;

      // Step 2
      CHKERR(errFreeNow, handlePcst(errStream, trail, 
				    pred, tcc, handled, handlable));
      
      DEBUG(SOL) errStream << "\t[Sol 2] (pcst): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;

      if (handled)
	break;
      if (handlable)
	continue;
      
      TypeSet dom = getDomain(pred);
      TypeSet vars = getDomVars(dom);
      bool ms = mustSolve(dom);
      if (ms) {
 	// Step 3.a
	CHKERR(errFreeNow, handleTCPred(errStream, trail, pred, tcc,
					instEnv, ms, false, handled));
	DEBUG(SOL) errStream << "\t[Sol 3.a] (must-solve): " 
			    << asString(Options::debugTvP)
			    << (handled ? " [HANDLED]" : "")
			    << (handlable ? " [HANDLABLE]" : "")
			    << (!errFreeNow ? " [ERROR]" : "")
			    << std::endl;

	if (handled)
	  break;
      }
      
      // Step 3.b.i
      rigidify(vars);
      handleTCPred(errStream, trail, pred, tcc,
		   instEnv, ms, false, handled);
      unrigidify(vars);
      DEBUG(SOL) errStream << "\t[Sol 3.b.i] (exact): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;
      if (handled)
	break;
      
      // Step 3.b.ii
      CHKERR(errFreeNow, handleTCPred(errStream, trail, pred, tcc,
				      instEnv, false, true, handled));
      DEBUG(SOL) errStream << "[Sol 3.b.ii] (solvability): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;
      if (handled)
	break;
      
      // Step 4
      CHKERR(errFreeNow, handleEquPreds(errStream, trail, 
					pred, tcc, vars, handled));
      DEBUG(SOL) errStream << "[Sol 4] (equ-pred): " 
			  << asString(Options::debugTvP)
			  << (handled ? " [HANDLED]" : "")
			  << (handlable ? " [HANDLABLE]" : "")
			  << (!errFreeNow ? " [ERROR]" : "")
			  << std::endl;      
      if (handled)
	break;
      
      if (!errFreeNow)
	assert(false);
    }
    
    if (!errFreeNow) {
      assert(handled);
      assert(errPred);
      errStream << errLoc << ": "
		<< "Unsatisfiable Constraint: "
		<< errPred->asString() 
		<< std::endl; 
    }
    
    CHKERR(errFree, errFreeNow);
    
  } while (handled);
  
  return errFree;
}
