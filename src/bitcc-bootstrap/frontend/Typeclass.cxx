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
Instance::equals(std::ostream &errStream, GCPtr<Instance> ins, 
		 GCPtr<const Environment< sherpa::CVector<GCPtr<Instance> > > >
		 instEnv) const
{
  GCPtr<TypeScheme> mySigma = ts->ts_instance_copy();
  GCPtr<TypeScheme> hisSigma = ins->ts->ts_instance_copy();

  //std::cerr << mySigma->asString() << " vs " 
  //	    << hisSigma->asString() 
  //	    << std::endl;

  bool unifies = true;
  
  CHKERR(unifies, mySigma->tau->unifyWith(hisSigma->tau)); 
  
  if(!unifies)
    return false;
    
  assert(mySigma->tcc);
  assert(hisSigma->tcc);
  
  // This will also add self constraints.
  for(size_t j=0; j < hisSigma->tcc->pred->size(); j++) {
    GCPtr<Typeclass> hisPred = hisSigma->tcc->Pred(j);      
    mySigma->tcc->addPred(hisPred);
  }
  
  std::stringstream ss;
  CHKERR(unifies, mySigma->solvePredicates(ss, ast->loc, 
					   instEnv, new Trail)); 
  
  if(!unifies)
    return false;
  
  if (mySigma->tcc->pred->size() == 0)
    return true;
  else
    return false;
}

bool 
Instance::satisfies(std::ostream &errStream,
		    GCPtr<Typeclass> pred, 		    
		    GCPtr<const Environment< sherpa::CVector<GCPtr<Instance> > > >
		    instEnv) const
{
  bool unifies = true;
  GCPtr<TypeScheme> sigma = ts->ts_instance_copy();

  CHKERR(unifies, sigma->tau->unifyWith(pred));   
  
  if(!unifies)
    return false;
    
  if(!sigma->tcc)
    return true;

  std::stringstream ss;
  CHKERR(unifies, sigma->solvePredicates(ss, pred->ast->loc, 
					 instEnv, new Trail));
  
  if(!unifies)
    return false;
  
  if (sigma->tcc->pred->size() == 0)
    return true;
  else
    return false;
}

bool 
Typeclass::addFnDep(GCPtr<Type> dep) 
{
  if(link)
    return getType()->addFnDep(dep); // getType() OK
  
  size_t c;
  
  if(kind != ty_typeclass)
    assert(false);

  if(dep->kind != ty_tyfn)
    assert(false);
  
  if(!fnDeps)
    fnDeps = new CVector<GCPtr<Type> >;

  for(c = 0; c < fnDeps->size(); c++)
    if(FnDep(c)->strictlyEquals(dep, false, true))
      return false;

  //   std::cout << "Adding fnDep " << dep->asString(NULL) 
  //   	    << " to " << this->asString(NULL) << "."
  //   	    << std::endl;
  fnDeps->append(dep);
  return true;
}

/* Check if a type scheme s = forall 'a*. t\C is ambiguous.
   If there exists a 'a such that 
      'a in {'a*}, 
      'a in FTVS(C) and 
      'a not in FTVS(t')
    then s is ambiguous.

   For example: 
     read:  forall 'a. () -> 'a \ {Readable('a)}
     write: forall 'a. 'a -> () \ {Writable('a)}

   What about write(read ()) ?
  
     write(read ()): forall 'a. () \ Readable('a), Writable('a).

   This case is traditionally declared and error because there is no
   way to instantiate the 'a at the use location.

   It is not clear that "ambiguous" typing is an error. In fact, it
   does not break subject reduction, and execution can continue by
   picking any instantiation of the variable. In particular, it is (in
   a way) necessary in the case of polymorphic consrtaints.

   For example, consider:
   
    let(k1) id = \x.x 
    Ignoring the internal maybe types at function argument and return
    positions, we can write:
    
    id: forall 'a,'b. 'b|'a->'a  \ {*(k1, 'b|'a->'a, 'b|'a->'a)}
    
    This type is not ambiguous. Now, if we write:

    let(k2) id2 = \y.(id y),

    the type if id2 will be:

    id2: forall 'c,'d,'e. 'd|'c->'c  \ {*(k1, 'b|'a->'a, 'e|'c->'c),
                                        *(k2, 'd|'c->'c, 'd|'c->'c)}

    Here, the type of id2 will be declared ambiguous, which we cannot
    accept. 
    
   It seems that we can take a middle ground where ambiguity check is
   performed only for type-class constraints. However, in the presence
   of copy-compatibility, even this is insufficient. Consider the
   following case:

   (deftypeclass (Tc 'a)
      mt: (fn ('a) bool))

   (define (f x) (mt x))
   f: forall 'a,'b,'c,'d. (fn ('a|'b) 'c|bool) \ Tc('d|'b)

   Actually, the top-level mutability on the type-class does not
   matter (as long as the argument) is not used in a reference
   context, and can be ignored in the ambiguity check. However,
   it seems that the correct solution is to turn off the ambiguity
   check. There is now only a stub-code for the ambiguty check.
 */


bool 
TypeScheme::checkAmbiguity(std::ostream &errStream, LexLoc &errLoc)
{
#if 0
  bool errFree =true;
  for(size_t j=0; j < ftvs->size(); j++) {
    GCPtr<Type> ftv = ftvs->elem(j);
    
    if(!tau->boundInType(ftv)) {
      // ftv must be bound in some predicate.

      for(size_t c=0; c < tcc->size(); c++) {
	GCPtr<Typeclass> pred = tcc->Pred(c);
	if(pred->isPcst())
	  continue;
	
	// The ftv is bound in a type-class predicate.
	if(pred->boundInType(ftv)) {
	  errStream << errLoc << ": "
		    << "Type variable "
		    << ftv->asString(Options::debugTvP)
		    << " unbound in "
		    << tau->asString(Options::debugTvP)
		    << " wrt "
		    << asString(Options::debugTvP)
		    << std::endl;
	  
	  errFree = false;
	  break;
	}
      }
    }
  }

  if(!errFree)
    errStream << errLoc << ": "
	      << "Ambiguous type definition:"
	      << asString()
	      << std::endl;
  return errFree;
#else
  return true;
#endif
}

/* Migrate appropriate constraints to parent's TCC, if one exists
   let C'' = migrate(parent-sigma, C')
      --> Constraints purely over monomorphic type variables can be
          migrated to the containing scope.
   
  This function returns true if at least one predicate was migrated to
  the containing scope, false otherwise. 

  Suppose the current tye scheme s = forall 'a*.t\C

  For each predicate p in C,

  1) FTV(p) = 0 CANNOT HAPPEN. The prediate is concrete, and must be
     have been solved at this step.

  2) If FTVS(p) intersection 'a* = {}, then this predicate can be
     migrated. 
*/


bool
TypeScheme::migratePredicates(GCPtr<TCConstraints> parentTCC)
{
  if(!parentTCC)
    return false;
  
  bool migrated = false;
  GCPtr<CVector<GCPtr<Typeclass> > > newPred =
    new CVector<GCPtr<Typeclass> >;
  
  for(size_t i=0; i < tcc->pred->size(); i++) {
    GCPtr<Typeclass> pred = tcc->Pred(i)->getType();
    GCPtr< CVector< GCPtr<Type> > > allFtvs = new CVector<GCPtr<Type> >;
    pred->collectAllftvs(allFtvs);
    
    assert(allFtvs->size() != 0);
    
    bool hasFtv = false;
    for(size_t j=0; j < allFtvs->size(); j++) {
      GCPtr<Type> ftv = allFtvs->elem(j)->getType();
      
      if(ftvs->contains(ftv)) {
	hasFtv = true;
	break;
      }
    }
    
    if(hasFtv) {
      newPred->append(pred);
    }
    else {
      parentTCC->addPred(pred);
      migrated = true;
    }
  }
    
  tcc->pred = newPred;
  return migrated;
}


static GCPtr< CVector< GCPtr<Type> > > 
getDomain(GCPtr<Typeclass> t)
{
  GCPtr< CVector< GCPtr<Type> > > dom = new CVector< GCPtr<Type> >;
  
  for(size_t i=0; i < t->typeArgs->size(); i++)
    dom->append(t->TypeArg(i));
  
  if(t->fnDeps)
    for(size_t fd = 0; fd < t->fnDeps->size(); fd++) {
      GCPtr<Type> fdep = t->FnDep(fd);
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
    if(arg->kind == ty_tvar || 
       arg->kind == ty_mbTop || arg->kind == ty_mbFull)
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
    
    // *(p, tg, ti), Immutable(ti)
    if(ins->isDeepImmutable()) {
      PCST_DEBUG errStream << "\t\tCase *(p, tg, ti), "
			   << "Immutable(ti) CLEAR." 
			   << std::endl;
      cset->clearPred(ct);
      handled = true;
      return true;
    }
    
    // *(p, tg, ti), ~Immut(ti) (type variables OK here)
    if (!ins->isDeepImmut()) {
      PCST_DEBUG errStream << "\t\tCase *(p, tg, ti), " 
			   << "~Immut(ti) ERROR." 
			   << std::endl;
      cset->clearPred(ct);
      handled = true;
      return false;
    }
    
    PCST_DEBUG errStream << "\t\tCase *(p, tg, ti), " 
			 << "Immut(ti) KEEP." 
			 << std::endl;
    
    // *(p, tg, ti), Immut(ti), ~Immutable(ti)
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

  /* U(*(k, tg, ti), *(k, tg, ti')), ti !=~= ti' */
  for(size_t c=0; c < cset->size(); c++) {
    GCPtr<Constraint> newCt = cset->Pred(c)->getType();
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
    assert(pred->typeArgs->size() == 1);
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
	     GCPtr<const Environment< CVector<GCPtr<Instance> > > > instEnv,
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
    for(size_t c=0; c < inst->typeArgs->size(); c++)
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
    for(size_t c=0; c < instScheme->tcc->pred->size(); c++) {
      GCPtr<Typeclass> instPred = instScheme->tcc->Pred(c);

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
  for(size_t c=0; c < tcc->size(); c++) {
    GCPtr<Constraint> newCt = tcc->Pred(c)->getType();
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
TypeScheme::solvePredicates(std::ostream &errStream, LexLoc &errLoc,
			    GCPtr< const Environment< CVector<GCPtr<Instance> > > > instEnv,
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
    
    for(size_t i=0; i < tcc->pred->size(); i++) {
      GCPtr<Typeclass> pred = tcc->Pred(i);
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


#if 0
void 
TypeScheme::markRigid(std::ostream &errStream, LexLoc &errLoc)
{
  //  tvs that are determined by fnDeps
  GCPtr< CVector<GCPtr<Type> > > det = new CVector<GCPtr<Type> >; 

  // Mark all free Type variables as rigid
#ifdef VERBOSE_SOLVE
  Options::showAllTccs = true;
  errStream << errLoc << ": In " << asString(NULL) 
	    << " {" << tau->asString(NULL) << "} "
	    << " marking: ";
  Options::showAllTccs = false;
#endif

  for(size_t i=0; i < tcc->pred->size(); i++) {
    GCPtr<Typeclass> pred = tcc->Pred(i);

    
    if(pred->fnDeps)
      for(size_t fd = 0; fd < pred->fnDeps->size(); fd++) {
	GCPtr<Type> fndom = pred->FnDep(fd)->Args();
	fndom->collectAllftvs(det);
      }
  } 
  
  for(size_t i=0; i < ftvs->size(); i++) {
    GCPtr<Type> ftv = ftvs->elem(i)->getType();
    
    if(!det->contains(ftv)) {
      ftv->flags |= TY_RIGID;
#ifdef VERBOSE_SOLVE
    errStream << ftv->asString(NULL) << " ";
#endif
    }
  }
#ifdef VERBOSE_SOLVE
  errStream << std::endl;
#endif  
}
#endif

