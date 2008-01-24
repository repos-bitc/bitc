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
#include <iostream>
#include <string>
#include <sstream>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <assert.h>
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "TypeEqInfer.hxx"
#include "TypeInferCommon.hxx"

/* This file implements an equational unification algorithm that
   (partially) solves the constraints generated through the inference
   algorithm coded in TypeEqInfer.cxx */

/**********************************************************************/
/*                 Constraint Set (Transitive) Closure                */
/**********************************************************************/


// Note: This function uses pointer comparison for transitive closure.
// The only important case for closure is the constraints of the form
// 'a <: t and t <: 'a, for which this algorithm suffices.
// This algorithm will not close constraints of the form:
// tx <: t1 -> t2 , t1 -> t2 <: ty

// Returns true if the constraint set has changed, otherwise false.
bool
TransClose(GCPtr<Constraints> cset)
{
  size_t start = cset->pred->size();
  size_t pass_start=0;
  do {
    pass_start = cset->pred->size();
    
    for(size_t i=0; i < cset->size(); i++) {
      GCPtr<Constraint> cti = cset->Pred(i)->getType();
      if(cti->kind != ty_subtype)
	continue;
      
      for(size_t j=i+1; j < cset->size(); j++) {
	GCPtr<Constraint> ctj = cset->Pred(j)->getType();
	if(ctj->kind != ty_subtype)
	  continue;
	
	// If we have cti = a <: b and ctj = b <: c, then add a <: c
	if(cti->CompType(1) == ctj->CompType(0))
	  addSubCst(cti->ast, cti->CompType(0), 
		    ctj->CompType(1), cset);

	// If we have cti = a <: b and ctj = c <: a, then add c <: a
	if(cti->CompType(0) == ctj->CompType(1))
	  addSubCst(ctj->ast, ctj->CompType(0), 
		    cti->CompType(1), cset);
      }
    }
  } while(cset->pred->size() > pass_start);
  
  size_t norm = cset->pred->size();
  cset->normalize();
  
  size_t final = cset->pred->size();
  return ((start != norm) || (norm != final));
}


// For debugging only.
extern GCPtr<TvPrinter> debugTvp;
void
printCset(std::ostream& out, GCPtr<Constraints> cset)
{
  if(cset->size()) {
    out << "{";
    for(size_t i=0; i < cset->size(); i++) {
      if(i > 0)
	out << ", ";
      out << cset->Pred(i)->asString(debugTvp);
    }
    out << "}";
  }
}


static bool
typeError(std::ostream& errStream, GCPtr<Constraint> ct)
{
  errStream << ct->ast->loc << ": Type Error."
	    << " Unsatiafiable constraint: "
	    << ct->asString();
  
  // MUST always return false.
  return false;
}


#define CMPSET(var, val) \
  do {			 \
    if(var != true)	 \
      var = val;	 \
  } while(0);		 


bool
EqUnify(std::ostream& errStream, GCPtr<Constraints> cset, 
	GCPtr<Trail> trail)
{
  bool cset_changed = false;
  bool errFree = true;
  
  TransClose(cset);
  do {
    cset_changed = false;
    
    for(size_t i=0; i < cset->size(); i++) {
      bool unified_in_this_iteration = true;
      
      /* U({}) */
      // Implementation is implicit
      
      GCPtr<Constraint> ct = cset->Pred(i)->getType();
      if(ct->flags & CT_REMOVE)
	continue;
      
      switch(ct->kind) {
      case ty_subtype:
	{
	  GCPtr<Type> lhs = ct->CompType(0)->getType();
	  GCPtr<Type> rhs = ct->CompType(1)->getType();

	  /* U(t <: t) */
	  if(lhs == rhs) {
	    ct->flags |= CT_REMOVE;
	    break;
	  }
	  
	  if((lhs->kind == rhs->kind) && lhs->isBaseConstType()) {
	    ct->flags |= CT_REMOVE;
	    break;
	  }

	  /* U(Mt <: t) */
	  if((lhs->kind == ty_mutable) &&
	     (lhs->CompType(0)->getType() == rhs)) {
	    ct->flags |= CT_REMOVE;
	    break;
	  }
	  
	  /* U('a <: t, t <: 'a) */
	  if(lhs->kind == ty_tvar) {
	    GCPtr<Type> reverse = new Constraint(ty_subtype, ct->ast,
						 rhs, lhs);
	    if(cset->contains(reverse)) {
	      ct->flags |= CT_REMOVE;
	      trail->subst(lhs, rhs);
	      break;
	    }
	  }
	  
	  /* U(k = x) */
	  // This case is implicit. We don't generate k=x constraints, 
	  // but perform the substitution immediately.

	  /* U('a <: t), t = maxz(t) */
	  if((lhs->kind == ty_tvar) && rhs->isMaxMutable()) {
	    ct->flags |= CT_REMOVE;
	    trail->subst(lhs, rhs);
	    break;
	  }
	  
	  /* U(t <: 'a), t = minz(t), forall 'b, t != 'b */
	  if((rhs->kind == ty_tvar) && (lhs->kind != ty_tvar) &&
	     lhs->isMinMutable()) {
	    ct->flags |= CT_REMOVE;
	    trail->subst(rhs, lhs);
	    break;
	  }
	  
	  /* U(ta1->tr1 <: ta2->tr2 */ 
	  if((lhs->kind == ty_fn) && (rhs->kind == ty_fn)) {
	    GCPtr<Type> arg1 = lhs->CompType(0)->getType();
	    GCPtr<Type> arg2 = rhs->CompType(0)->getType();
	    GCPtr<Type> ret1 = lhs->CompType(1)->getType();
	    GCPtr<Type> ret2 = rhs->CompType(1)->getType();
	    
	    if(arg1->components->size() == arg2->components->size()) {
	      ct->flags |= CT_REMOVE;
	      
	      for(size_t a=0; a < arg1->components->size(); a++)
		addEqCst(ct->ast, arg1->CompType(a), 
			 arg2->CompType(a), cset); 
	      
	      addEqCst(ct->ast, ret1, ret2, cset);
	      break;
	    }
	  }
	  
	  /* U(^t1 <: ^t2 */ 
	  if((lhs->kind == ty_ref) && (rhs->kind == ty_ref)) {
	    ct->flags |= CT_REMOVE;
	    
	    addEqCst(ct->ast, lhs->CompType(0), 
		     rhs->CompType(0), cset);
	    break;
	  }
	  
	  /* U(Mt1 <: Mt2 */ 
	  if((lhs->kind == ty_mutable) && (rhs->kind == ty_mutable)) {
	    ct->flags |= CT_REMOVE;
	    
	    addSubCst(ct->ast, lhs->CompType(0), 
		      rhs->CompType(0), cset);
	    break;
	  }
	  
	  /* U(Mt1 <: Mt2 */ 
	  if((lhs->kind == ty_mutable) && (rhs->kind != ty_tvar)) {
	    assert(rhs->kind != ty_mutable);
	    ct->flags |= CT_REMOVE;
	    
	    addSubCst(ct->ast, lhs->CompType(0), rhs, cset);
	    break;
	  }

	  /* U(t11xt12 <: t21xt22 */ 
	  /* Pairs tbd */
	  
	  /* Subtype constraint must remain, or is an error */
	  unified_in_this_iteration = false;
	  break;
	}
	
      case ty_pcst:
	{
	  GCPtr<Type> k = ct->CompType(0)->getType();
	  GCPtr<Type> gen = ct->CompType(1)->getType();
	  GCPtr<Type> ins = ct->CompType(2)->getType();
	  
	  /* U(*(m, tg, ti)) */
	  if(k == Type::Kmono) {
	    ct->flags |= CT_REMOVE;
	    addEqCst(ct->ast, gen, ins, cset);
	    break;
	  }

	  /* U(*(p, tg, ti)), Immutable(ti) */
	  if (k == Type::Kpoly && ins->isDeepImmutable()) {
	    ct->flags |= CT_REMOVE;
	    break;
	  }
	  /* U(*(k, tg, ti)), Mut(ti) */
	  if(k->kind == ty_kvar && ins->isDeepMut()) {
	    trail->subst(k, Type::Kmono);
	    break;
	  }
	  
	  /* U(*(k, tg, ti), *(k, tg, ti')), ti !=~= ti' */
	  if (k->kind == ty_kvar) {
	    GCPtr<Constraints> newC = new Constraints();	      
	    bool more_found = false;
	    for(size_t c=0; c < cset->size(); c++) {
	      GCPtr<Constraint> newCt = cset->Pred(c)->getType();
	      
	      if(newCt->flags & CT_REMOVE)
		continue;
	      
	      if(newCt->kind == ty_pcst && newCt->CompType(0) == k) {
		if(newCt != ct)
		  more_found = true;
		
		addEqCst(ct->ast, ct->CompType(2),
			 newCt->CompType(2), newC); 
	      }
	      else
		newC->addPred(newCt);
	    }
	    
	    if(more_found) {
	      GCPtr<Trail> tr = new Trail();
	      bool unifies = EqUnify(errStream, newC, tr);
	      tr->rollBack();
	      
	      if(!unifies) {
		trail->subst(k, Type::Kpoly);
		break;
	      }
	    }
	  }

	  unified_in_this_iteration = false;
	  break;
	}
	
      default:
	{
	  assert(false);
	  break;
	}
      } /* switch constraint */ 
      
      CMPSET(cset_changed, unified_in_this_iteration);
    } /* for each constraint */
    
    if(cset_changed) {
      GCPtr<Constraints> newC = new Constraints();	      
      for(size_t c=0; c < cset->size(); c++) {
	GCPtr<Constraint> ct = cset->Pred(c)->getType();
	
	if((ct->flags & CT_REMOVE) == 0)
	  newC->addPred(ct);
      }
      
      cset->pred = newC->pred;
      
      //errStream << "After this pass: ";
      //printCset(errStream, cset);
      //errStream << endl;
	
      TransClose(cset);

      //errStream << "After close: ";
      //printCset(errStream, cset);
      //errStream << endl;
    }
    
  } while(cset_changed);
  
  /* Error detection */
  for(size_t i=0; i < cset->size(); i++) {
    GCPtr<Constraint> ct = cset->Pred(i)->getType();

    /* Constraints that can remain unsolved */
    switch(ct->kind) {
    case ty_subtype:
      {
	GCPtr<Type> lhs = ct->CompType(0)->getType();
	GCPtr<Type> rhs = ct->CompType(1)->getType();
	
	/* 'a <: t and t <: 'a */
	if(lhs->kind == ty_tvar || rhs->kind == ty_tvar)
	  break;
    
	errFree = typeError(errStream, ct);
	break;
      }
    case ty_pcst:
      {
	GCPtr<Type> k = ct->CompType(0)->getType();
	GCPtr<Type> gen = ct->CompType(1)->getType();
	GCPtr<Type> ins = ct->CompType(2)->getType();
	
	/* U(*(k, tg, ti)) */
	if (k->kind == ty_kvar)
	  break;
	
	/* U(*(p, tg, ti)), Immut(ti) */
	if (k == Type::Kpoly && ins->isDeepImmut())
	  break;
	
	errFree = typeError(errStream, ct);
	break;
      }
      
    default:
      {
	assert(false);
	break;
      }
    }
  }
  
  return errFree;
}
