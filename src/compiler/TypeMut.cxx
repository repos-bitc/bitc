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

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "TypeScheme.hxx"
#include "TypeMut.hxx"
#include "Typeclass.hxx"
#include "inter-pass.hxx"
#include "Unify.hxx"
#include "Options.hxx"

using namespace boost;
using namespace sherpa;
using namespace std;


/********************************************************************/
/*
                        HANDLING MUTABILITY 
			*******************
         The following are some implementation level Observations
              in implementing mutability rules in BitC. 

1) All tvars MUST have a maybe-wrapper around them. For example:

(define (f x y)
  (if #t x y)  
  x:int32
  y:(mutable int32))

  if x and y are just given the types 'a and 'b, then they are linked at
  the if expression, and subsequent steps will fail to type check.

  Since:
    - when the use writes 'a, he means any type whether mutable or
      immutable, and 
    - we do not have an immutable type qualifier (yet),
   type variables are always given a maybe-wrapper at binding
   time. These are discarded at the let-boundary.

   There are 2 positions we can take on Type Variables:
   i) Maybe-types are a *part of the type-variuable itself*.
      Two type-variables with different maybe-wrappers are different
      types with copy-compatibility constraints. Therefore, they must
      always co-exist, even in the type-schemes.

   ii) May-be wrappers are actually a *part of the constraint*
       requiring copy-compatibility. Therefore, eventhough we need
       them while we are working (just like any other type), they need
       not exist in the type-scheme. While instantiating, they should
       be instantiated with maybe-wrappers.


2) The following type-records should never have a maybe-wrapper:
      ty_fnarg, ty_letgather, ty_typeclass, ty_tyfn.

 
3) All type expressions (except in the case of a type variable) are
   stand for their immutable versions, so, they are given a type
   without any wrapper.
 
4) At top level, all monomorphic type variables are instantiated to
   dummy types.

 
2) We perform exact Unification in the following circumstances:
  i) Type Qualification
  ii) Unification past a ref-boundary.


3) We need to unify maybe-type-records even after contents have unified.
   Otherwise:

(lambda (x:(vector 'a) y:(vector 'b))
  (== x y) ;; succeeds
  x:(vector int32)
  y:(vector (mutable int32))
  (== x y)  ;; fails
)
   
4) TypeSpecialize must return ``hints'' as is:

-- Rationale: For something that is frozen at a let and put in the 
   environment, there is no maybeHint 
-- Otherwise, we are workingon some type, and it is reasonable to 
   use the same hint. 

5) Type-class constraints: 
   While the value-restriction requies that we clear maybe-wrappers on  
   types, this is not true for constraints. I think we can let the
   constraints have maybe-types (with undecided mutability status) even 
   on polymorphic types with no effect on soundness. This will give more
   expressiveness in cases like:
    
(deftypeclass (CL 'a)
  MTD: (fn ((vector 'a)) ()))

(define (p x) (MTD (vector x)))
p: (CL (maybe-1 'a)) => (fn ('a) ())

   Then, an instance of (CL (mutable bool)) can satisfy use of p with
   bool. 
 
   This issue goes away if we require all arguments of a type-class to
   not have typ-level mutability, as opposed to only those used in
   copy-positions in methods.  
   
   However, Prof.Shapiro has suggested that we fix all maybes
   including those in constraints for now. 

6) Whereas the type of definitions and declarations must ordinarily
   match exactly, if the definition in question is a function, the
   definition and declaration shall be deemed compatible if all the
   arguments return type of the definition and declaration are
   copy-compatible. 

7) Implementation level issue: Since closure conversion introduces
   new Refs, copy-compatibility at letbindings must be adjusted to
   work beyond these refs.
                   
8) getDCopy() on types actually does the right thing. It does not
   create duplicate maybe-records in the wrong places and create
   unwanted copy-compatibilities. This is because it ultimately calls
   TypeSpecialize, which not only links tvars to original ones, but
   also remembers the specialized type of a particular type record,
   and always returns the same thing. For this reason, it is
   imperative that TypeSpecialize be the **only** routine that
   duplicates type records deeply.
                                                                    */
/********************************************************************/  

/********************************************************************
         Operators for Minimizing/Maximizing Mutability
  [ Name of the operator in formalization document written in braces]

 *******************************************************************/


/* Maximize Mutability at the top-most level [\blacktriangle] */
shared_ptr<Type> 
Type::maximizeTopMutability(shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  shared_ptr<Type> rt = GC_NULL;
  
  switch(t->kind) {
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      rt = t->Core()->maximizeTopMutability(trail);
      break;
    }

  case ty_mutable:
    {
      rt = t->Base()->maximizeTopMutability(trail);
    }

  case ty_letGather:
    {
      assert(false);
      break;
    }
    
  default:
    {
      rt = Mutable(t->getDCopy());
      break;
    }
  }

  return rt;
}

/* Minimize Mutability at the top-most level [\blacktriangledown] */
shared_ptr<Type> 
Type::minimizeTopMutability(shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  shared_ptr<Type> rt = GC_NULL;
  
  switch(t->kind) {
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      rt = t->Core()->minimizeTopMutability(trail);
      break;
    }
    
  case ty_mutable:
    {
      rt = t->Base()->minimizeTopMutability(trail);
      break;
    }

  case ty_letGather:
    {
      assert(false);
      break;
    }
        
  default:
    {
      rt = t;
      break;
    }
  }
  
  return rt;
}

/* Maximize Mutability up to copy boundary [\triangle] */
shared_ptr<Type> 
Type::maximizeMutability(shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  shared_ptr<Type> rt = GC_NULL;

  if (t->mark & MARK_MAXIMIZE_MUTABILITY)
    return t;
  
  t->mark |= MARK_MAXIMIZE_MUTABILITY;  
  
  switch(t->kind) {
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      rt = t->Core()->maximizeMutability(trail);
      break;
    }

  case ty_mutable:
    {
      rt = t->Base()->maximizeMutability(trail);
      break;
    }
    
  case ty_array:
    {
      rt = Type::make(t);
      rt->Base() = t->Base()->maximizeMutability(trail);
      rt = Mutable(t);      
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
    {
      rt = t->getDCopy();
      for (size_t i=0; i < rt->typeArgs.size(); i++) {
	shared_ptr<Type> arg = rt->TypeArg(i)->getType();

	if (rt->argCCOK(i)) {
	  shared_ptr<Type> argMax =
	    arg->maximizeMutability(trail)->getType(); 
	  if (arg != argMax)
	    trail->link(arg, argMax);
	}
      }
      
      rt = Mutable(rt);
      break;
    } 
    
  case ty_letGather:
    {
      rt = t->getDCopy();
      for (size_t i=0; i < t->components.size(); i++)
	rt->CompType(i) = t->CompType(i)->maximizeMutability(trail); 
      break;
    }
    
  default:
    {
      rt = Mutable(t->getDCopy());
      break;
    }
  }
  
  t->mark &= ~MARK_MAXIMIZE_MUTABILITY;
  return rt;
}

/* Minimize Mutability up to copy boundary [\triangledown] */
shared_ptr<Type> 
Type::minimizeMutability(shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  shared_ptr<Type> rt = GC_NULL;
  
  if (t->mark & MARK_MINIMIZE_MUTABILITY)
    return t;
  
  t->mark |= MARK_MINIMIZE_MUTABILITY;  
  
  switch(t->kind) {
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      rt = t->Core()->minimizeMutability(trail);
      break;
    }

  case ty_mutable:
    {
      rt = t->Base()->minimizeMutability(trail);
      break;
    }

  case ty_array:
    {
      rt = Type::make(t);
      rt->Base() = t->Base()->minimizeMutability(trail);
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
    {
      rt = t->getDCopy();
      for (size_t i=0; i < rt->typeArgs.size(); i++) {
	shared_ptr<Type> arg = rt->TypeArg(i)->getType();
	if (rt->argCCOK(i)) {
	  shared_ptr<Type> argMin =
	    arg->minimizeMutability(trail)->getType(); 
	  if (arg != argMin)
	    trail->link(arg, argMin);
	}
      }
      break;
    }

  case ty_letGather:
    {
      rt = t->getDCopy();
      for (size_t i=0; i < t->components.size(); i++) 
	rt->CompType(i) = t->CompType(i)->minimizeMutability(trail);
      break;
    }
    
  default:
    {
      rt = t;
      break;
    }
  }
  
  t->mark &= ~MARK_MINIMIZE_MUTABILITY;
  return rt;
}

/* Minimize Mutability up to function boundary [\mathfrak{I}] */
shared_ptr<Type> 
Type::minimizeDeepMutability(shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  shared_ptr<Type> rt = GC_NULL;
  
  if (t->mark & MARK_MINIMIZE_DEEP_MUTABILITY)
    return t;
  
  t->mark |= MARK_MINIMIZE_DEEP_MUTABILITY;  
  
  switch(t->kind) {
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      rt = t->Core()->minimizeDeepMutability(trail);
      break;
    }

  case ty_mutable:
    {
      rt = t->Base()->minimizeDeepMutability(trail);
      break;
    }

  case ty_ref:
  case ty_vector:
  case ty_array:
    {
      rt = Type::make(t);
      rt->Base() = t->Base()->minimizeDeepMutability(trail);
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_structr:
  case ty_unionr: 
  case ty_uvalr: 
  case ty_uconr: 
    {
      rt = t->getDCopy();
      for (size_t i=0; i < rt->typeArgs.size(); i++) {
	shared_ptr<Type> arg = rt->TypeArg(i)->getType();
	shared_ptr<Type> argMin =
	  arg->minimizeDeepMutability(trail)->getType(); 
	
	if (arg != argMin)
	  trail->link(arg, argMin);
      }
      break;
    } 
    
  case ty_letGather:
    {
      rt = t->getDCopy();
      for (size_t i=0; i < t->components.size(); i++) {
	rt->CompType(i) =
	  t->CompType(i)->minimizeDeepMutability(trail);
      }
      break;
    }

  default:
    {
      // Concrete types and function types enter this case
      rt = t;
      break;
    }
  }
  
  t->mark &= ~MARK_MINIMIZE_DEEP_MUTABILITY;
  return rt;
}


/* Get the minimally-mutable version of this type, but interpret
   const-meta-constructors at this step. This function is useful to
   construct a maybe(full) type, since in 'a|p, p need not  
   preserve const-ness. 

   This function is similar to [\triangledown], except for the
   handling  of const  */
shared_ptr<Type> 
Type::minMutConstless(shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  shared_ptr<Type> rt = GC_NULL;
  
  if (t->mark & MARK_MIN_MUT_CONSTLESS)
    return t;
  
  t->mark |= MARK_MIN_MUT_CONSTLESS;  
  
  switch(t->kind) {
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      rt = t->Core()->minMutConstless(trail);
      break;
    }

  case ty_const:
  case ty_mutable:
    {
      rt = t->Base()->minMutConstless(trail);
      break;
    }

  case ty_array:
    {
      rt = Type::make(t);
      rt->Base() = t->Base()->minMutConstless(trail);
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
    {
      rt = t->getDCopy();
      for (size_t i=0; i < rt->typeArgs.size(); i++) {
	shared_ptr<Type> arg = rt->TypeArg(i)->getType();
	if (rt->argCCOK(i)) {
	  shared_ptr<Type> argMin =
	    arg->minMutConstless(trail)->getType(); 
	  if (arg != argMin)
	    trail->link(arg, argMin);
	}
      }
      break;
    }

  case ty_letGather:
    {
      rt = t->getDCopy();
      for (size_t i=0; i < t->components.size(); i++) 
	rt->CompType(i) = t->CompType(i)->minMutConstless(trail);
      break;
    }
    
  default:
    {
      rt = t;
      break;
    }
  }
  
  t->mark &= ~MARK_MIN_MUT_CONSTLESS;
  return rt;
}



/********************************************************************
      Mutability Propagation inwards for unboxed structures 
             and mutability consistency checking
 *******************************************************************/

bool
Type::checkMutConsistency(bool inMut, bool seenMut)
{
  bool errFree = true;
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_CHECK_MUT_CONSISTENCY)
    return errFree;
  
  t->mark |= MARK_CHECK_MUT_CONSISTENCY;
  
  switch(t->kind) {
  case ty_tvar:
    {
      // Should we enforce:  
      //CHKERR(errFree, !inMut || seenMut); ??
      break;
    }

  case ty_mbTop:
    {
      CHKERR(errFree, !inMut);
      CHKERR(errFree, t->Core()->checkMutConsistency(inMut, false));
      break;
    }
    
  case ty_mbFull:
    {
      shared_ptr<Type> var = t->Var()->getType();
      shared_ptr<Type> inner = t->Core()->getType();
    
      bool varMutable = var->isMutable();
    
      CHKERR(errFree, !inMut || varMutable);
    
      if(varMutable) {
	shared_ptr<Type> innerMax = inner->maximizeMutability();
	CHKERR(errFree, innerMax->checkMutConsistency(false, false));
      }
      else {
	CHKERR(errFree, inner->checkMutConsistency(false, false));
      }
      
      break;
    }
    
  case ty_mutable:
    {
      CHKERR(errFree, t->Base()->checkMutConsistency(true, true));
      break;
    }
    
  case ty_array:
    {
      CHKERR(errFree, !inMut || seenMut);
      CHKERR(errFree, t->Base()->checkMutConsistency(inMut, false)); 
      break;
    }
    
  case ty_structv:
    {
      CHKERR(errFree, !inMut || seenMut);
      for (size_t i=0; i < t->components.size(); i++) {
	shared_ptr<Type> component = t->CompType(i);
	CHKERR(errFree, component->checkMutConsistency(inMut, false));
      }
      break;
    }

  default:
    {
      CHKERR(errFree, !inMut || seenMut);
      for (size_t i=0; i < t->components.size(); i++) {
	shared_ptr<Type> component = t->CompType(i);
	CHKERR(errFree, component->checkMutConsistency(false, false));
      }
      break;
    }
  }
  
  t->mark &= ~MARK_CHECK_MUT_CONSISTENCY;
  return errFree;
}

/* Mutability Propagation [\mathbb{M}] */
bool
Type::propagateMutability(boost::shared_ptr<Trail> trail, 
			  const bool inMutable)
{
  bool errFree = true;
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_PROPAGATE_MUTABILITY)
    return errFree;
  
  t->mark |= MARK_PROPAGATE_MUTABILITY;  
  
  switch(t->kind) {
    
  case ty_tvar:
    {
      errFree = false;
      break;
    }
    
  case ty_mbTop:    
    {
      shared_ptr<Type> var = t->Var()->getType();
      shared_ptr<Type> inner = t->Core()->getType();
      if(!inner->isMutable())
	inner = Type::make(ty_mutable, inner);
      
      CHKERR(errFree, inner->propagateMutability(trail, false));

      if(errFree)
	trail->subst(var, inner);

      break;
    }
    
  case ty_mbFull:    
    {
      shared_ptr<Type> var = t->Var()->getType();
      shared_ptr<Type> inner = t->Core()->getType();
      
      if(!var->isMutable())
	trail->subst(var, Type::make(ty_mutable, newTvar()));

      CHKERR(errFree, t->checkMutConsistency());
      
      break;
    }
    
  case ty_mutable:
    {
      CHKERR(errFree, t->Base()->propagateMutability(trail, true));
      break;
    }

  case ty_array:
    {
      CHKERR(errFree, inMutable);
      CHKERR(errFree, t->Base()->propagateMutability(trail, false)); 
      break;
    }
    
  case ty_structv:
    {
      CHKERR(errFree, inMutable);
      for (size_t i=0; i < t->components.size(); i++) {
	shared_ptr<Type> component = t->CompType(i);
	CHKERR(errFree, component->propagateMutability(trail, false)); 
      }
      break;
    } 
    
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
    {
      CHKERR(errFree, inMutable);
      break;
    }
    
  case ty_letGather:
    {
      assert(false);
      break;
    }
    
    // concrete types, function type and reference types.
  default:
    {
      CHKERR(errFree, inMutable);
      break;
    }
  }
  
  t->mark &= ~MARK_PROPAGATE_MUTABILITY;
  return errFree;
}

/********************************************************************
                      Maybe normalization
 *******************************************************************/

/* Normalize types such as 
   ((mutable 'a)|bool) to (mutable bool) [\mathfrac{M}] */
void
Type::normalize_mbFull(boost::shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_NORMALIZE_MBFULL)
    return;
  
  t->mark |= MARK_NORMALIZE_MBFULL;  
  
  switch(t->kind) {
  case ty_mbFull:    
    {
      shared_ptr<Type> var = t->Var()->getType();
      shared_ptr<Type> inner = t->Core()->getType();
      
      inner->normalize_mbFull(trail);
      
      if(var->isMutable() && inner->isShallowConcretizable())
	trail->subst(var->Base(), inner->maximizeMutability(trail));
      
      break;
    }
    
  default:
    {
      for (size_t i=0; i < t->components.size(); i++)
	t->CompType(i)->normalize_mbFull(trail);
      
      for (size_t i=0; i < t->typeArgs.size(); i++)
	t->TypeArg(i)->normalize_mbFull(trail);
      
      for (TypeSet::iterator itr = t->fnDeps.begin();
	   itr != t->fnDeps.end(); ++itr)
	(*itr)->normalize_mbFull(trail);
      
      break;
    }
  }
  
  t->mark &= ~MARK_NORMALIZE_MBFULL;
  return;
}

/********************************************************************
                      Const normalization
 *******************************************************************/

/* Normalize types such as (const bool) to bool*/

void
Type::normalize_const(boost::shared_ptr<Trail> trail)
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_NORMALIZE_CONST)
    return;
  
  t->mark |= MARK_NORMALIZE_CONST;  
  
  for (size_t i=0; i < t->components.size(); i++)
    t->CompType(i)->normalize_const(trail);
  
  for (size_t i=0; i < t->typeArgs.size(); i++)
    t->TypeArg(i)->normalize_const(trail);
  
  for (TypeSet::iterator itr = t->fnDeps.begin();
       itr != t->fnDeps.end(); ++itr)
    (*itr)->normalize_const(trail);

  if((t->kind == ty_const) && t->Base()->isConstReducible())
    trail->link(t, t->Base()->minimizeMutability());
  
  t->mark &= ~MARK_NORMALIZE_CONST;
}


/********************************************************************
          Determining Copy Compatibility of unboxed structures
 *******************************************************************/

// See if nth typeArg is a CCC based on the TY_CCC flag markings
bool
Type::argCCOK(size_t argN)
{
  shared_ptr<Type> t = getType();
  assert(t->isValType());
  assert(argN < t->typeArgs.size());
  assert(t->defAst);

  // Be REALLY careful about bt. It is the type in the scheme.
  // NEVER unify it with anyhing.
  shared_ptr<Type> bt = t->defAst->symType;  
  if (bt->TypeArg(argN)->getType()->flags & TY_CCC)
    return true;
  else
    return false;
}


/* Determine Candidacy for Copy-Compatibility For type variables only,
   argument is a composite-type that is searched to determine ccc-ness */ 

bool
Type::determineCCC(shared_ptr<Type> t, bool inRefType)
{ 
  if (shared_from_this() != getType())
    return getType()->determineCCC(t);
  
  t = t->getType();

  if (t->mark & MARK_PREDICATE)
    return true;
  
  t->mark |= MARK_PREDICATE;  
  bool cccOK = true;
  
  switch(t->kind) {
  case ty_tvar:				       
    {
      if ((t == shared_from_this()) && (inRefType))
	cccOK = false;
      break;
    }

  case ty_typeclass:
  case ty_tyfn:
    {
      assert(false);
      break;
    }

  default:
    {
      for (size_t i=0; cccOK && (i<t->typeArgs.size()); i++) 
	cccOK = determineCCC(t->TypeArg(i), 
			     inRefType || t->isRefType() || 
			     (!t->argCCOK(i)));
      
      for (size_t i=0; cccOK && (i<t->components.size()); i++)
	cccOK = determineCCC(t->CompType(i), t->isRefType());
      
      // I think no need to process fnDeps here.
      break;
    }
  }

  t->mark &= ~MARK_PREDICATE;
  return cccOK;
}


/********************************************************************
                      Maybe Type coercion
 *******************************************************************/

static void 
coerceMaybe(shared_ptr<Type> t, shared_ptr<Trail> trail, 
	    bool minimize)
{
  //std::cerr << "Coercing: " << t->asString(Options::debugTvP)
  //	    << " to ";
  
  shared_ptr<Type> core = t->Core()->getType();
  shared_ptr<Type> var = t->Var()->getType();

  if((t->kind == ty_mbFull) && var->isMutable()) {
    var = var->Base()->getType();
    core = core->maximizeMutability()->getType();
  }
  else {
    if (minimize && (t->kind == ty_mbFull))
      core = core->minimizeMutability()->getType();
    else
      core = core->minimizeTopMutability()->getType();
  }
  
  if (core->kind != ty_tvar)
    trail->subst(var, core);
  else
    trail->link(t, core);
}

void
Type::adjMaybe(shared_ptr<Trail> trail, bool markedOnly, 
	       bool minimize, bool adjFn) 
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_ADJ_MAYBE)
    return;
  
  t->mark |= MARK_ADJ_MAYBE;
    
  switch(t->kind) {
  case ty_mbFull:
    {
      t->Core()->adjMaybe(trail, markedOnly, minimize, adjFn);
      if (!markedOnly || (t->Var()->getType()->flags & TY_COERCE))
	coerceMaybe(t, trail, minimize);
      break;
    }
    
  case ty_mbTop:
    {
      t->Core()->adjMaybe(trail, markedOnly, minimize, adjFn);
      if (!markedOnly || (t->Var()->getType()->flags & TY_COERCE))
	coerceMaybe(t, trail, minimize);
      break;
    }

  case ty_fn:
    {
      if (!adjFn)
	break;
      // otherwise, fall through
    }

  default:
    {
      for (size_t i=0; i < t->typeArgs.size(); i++) 
	t->TypeArg(i)->adjMaybe(trail, markedOnly, minimize, adjFn);
      
      for (size_t i=0; i<t->components.size(); i++)
	t->CompType(i)->adjMaybe(trail, markedOnly, minimize, adjFn);
      
      for (TypeSet::iterator itr = t->fnDeps.begin();
	  itr != t->fnDeps.end(); ++itr)
	(*itr)->adjMaybe(trail, markedOnly, minimize, adjFn);
      
      break;
    }
  }
  
  t->mark &= ~MARK_ADJ_MAYBE;
}


// Mark significant MB-tvars.
// Mb-Tvars that need not be preserved semantically are:
//  (1) at a copy position of a function argument or return type.
//  (2) at a copy-argument-position of typeclass argument.
// (1) is detected automatically, for (2) pass cppos-true at start.
// Actually what this does is an "unmark" on the TY_COERCE flag, not
// a new mark. The idea is that only generalizable FTVs should be
// marked this way. So, mark all generalizable TVs with TY_COERCE,
// and this routine will unmark all those coercions that will alter
// semantic meaning.
void 
Type::markSignMbs(bool cppos)
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_SIGN_MBS)
    return;
  
  t->mark |= MARK_SIGN_MBS;  
  
  switch(t->kind) {
    
  case ty_tvar:
    {
      t->flags &= ~TY_COERCE;
      break;
    }
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      if (!cppos)
	t->Var()->markSignMbs(cppos);
      
      t->Core()->markSignMbs(cppos);
      break;
    }

  case ty_mutable:
  case ty_array:
    {
      t->Base()->markSignMbs(cppos);
      break;
    }
    
  case ty_vector:
  case ty_ref:
  case ty_byref:
    {
      t->Base()->markSignMbs(false);
      break;
    }

  case ty_fn:
    {
      t->Args()->markSignMbs(true);
      t->Ret()->markSignMbs(true);
      break;
    }    
    
  case ty_fnarg:
    {
      for (size_t i=0; i < t->components.size(); i++)
	if(t->CompFlags(i) & COMP_BYREF)
	  t->CompType(i)->markSignMbs(false);
	else
	  t->CompType(i)->markSignMbs(true);

      break;
    }
    
  case ty_letGather:
    {
      for (size_t i=0; i < t->components.size(); i++)
	t->CompType(i)->markSignMbs(cppos);
      break;
    }
    
  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
    {
      for (size_t i=0; i < t->typeArgs.size(); i++) {
	shared_ptr<Type> arg = t->TypeArg(i)->getType();
	if (t->argCCOK(i))
	  arg->markSignMbs(cppos);
	else
	  arg->markSignMbs(false);
      }
      break;
    }
  case ty_structr:
  case ty_unionr: 
  case ty_uvalr: 
  case ty_uconr: 
    {
      for (size_t i=0; i < t->typeArgs.size(); i++) {
	shared_ptr<Type> arg = t->TypeArg(i)->getType();
	arg->markSignMbs(false);
      }
      break;
    }
    
  default:
    {
      break;
    }
  }
  
  t->mark &= ~MARK_SIGN_MBS;
  return;
}

void
Type::fixupFnTypes()
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_FIXUP_FN_TYPES)
    return;
  
  t->mark |= MARK_FIXUP_FN_TYPES;
  
  switch(t->kind) {
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      t->Core()->fixupFnTypes();
      break;
    }
    
  case ty_mutable:
  case ty_array:
  case ty_vector:    
  case ty_ref:
  case ty_byref:
    {
      t->Base()->fixupFnTypes();
      break;
    }
  
  case ty_fn:
    {
      t->Args()->fixupFnTypes();
      t->Ret()->fixupFnTypes();
      shared_ptr<Type> ret = t->Ret()->getType();
      if (ret->kind != ty_mbFull)
	t->Ret() = MBF(ret);
      break;
    }

  case ty_fnarg:
    {
      for (size_t i=0; i < t->components.size(); i++) {
	t->CompType(i)->fixupFnTypes();
	shared_ptr<Type> arg = t->CompType(i)->getType();
	if ((t->CompFlags(i) & COMP_BYREF) == 0) {
	  if (arg->kind != ty_mbFull)
	    t->CompType(i) = MBF(arg);
	}
      }
      
      break;
    }
  
  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_structr:
  case ty_unionr: 
  case ty_uvalr: 
  case ty_uconr: 
    {
      for (size_t i=0; i < t->typeArgs.size(); i++)
	t->TypeArg(i)->fixupFnTypes();
      
      break;
    }

  case ty_letGather:
    {
      for (size_t i=0; i < t->components.size(); i++) 
	t->CompType(i)->fixupFnTypes();
      break;
    }
    
  default:
    {
      break;
    }
  }
  
  t->mark &= ~MARK_FIXUP_FN_TYPES;
}
