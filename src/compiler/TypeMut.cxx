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

bool 
Type::copy_compatible(GCPtr<Type> t, bool verbose, std::ostream &errStream)
{
  return MBF(shared_from_this())->equals(MBF(t), verbose, errStream);
}

bool 
Type::copy_compatibleA(GCPtr<Type> t, bool verbose, std::ostream &errStream)
{
  return MBF(shared_from_this())->equalsA(MBF(t), verbose, errStream);
}

static inline GCPtr<Type> 
addMutable(GCPtr<Type> t)
{
  return Type::make(ty_mutable, t->getBareType());
}

GCPtr<Type> 
Type::maximizeTopMutability(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> rt = sherpa::GC_NULL;
  
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
      rt = addMutable(t->getDCopy());
      break;
    }
  }

  return rt;
}

GCPtr<Type> 
Type::minimizeTopMutability(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> rt = sherpa::GC_NULL;
  
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

GCPtr<Type> 
Type::maximizeMutability(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> rt = sherpa::GC_NULL;

  if(t->mark & MARK17)
    return t;
  
  t->mark |= MARK17;  
  
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
      rt = addMutable(t);      
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_reprv:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < rt->typeArgs.size(); i++) {
	GCPtr<Type> arg = rt->TypeArg(i)->getType();

	if(rt->argCCOK(i)) {
	  GCPtr<Type> argMax =
	    arg->maximizeMutability(trail)->getType(); 
	  if(arg != argMax)
	    trail->link(arg, argMax);
	}
      }
      
      rt = addMutable(rt);
      break;
    } 
    
  case ty_letGather:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < t->components.size(); i++)
	rt->CompType(i) = t->CompType(i)->maximizeMutability(trail); 
      break;
    }
    
  default:
    {
      rt = addMutable(t->getDCopy());
      break;
    }
  }
  
  t->mark &= ~MARK17;
  return rt;
}

GCPtr<Type> 
Type::minimizeMutability(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> rt = sherpa::GC_NULL;
  
  if(t->mark & MARK19)
    return t;
  
  t->mark |= MARK19;  
  
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
  case ty_reprv:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < rt->typeArgs.size(); i++) {
	GCPtr<Type> arg = rt->TypeArg(i)->getType();
	if(rt->argCCOK(i)) {
	  GCPtr<Type> argMin =
	    arg->minimizeMutability(trail)->getType(); 
	  if(arg != argMin)
	    trail->link(arg, argMin);
	}
      }
      break;
    }

  case ty_letGather:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < t->components.size(); i++) 
	rt->CompType(i) = t->CompType(i)->minimizeMutability(trail);
      break;
    }
    
  default:
    {
      rt = t;
      break;
    }
  }
  
  t->mark &= ~MARK19;
  return rt;
}

GCPtr<Type> 
Type::minimizeDeepMutability(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> rt = sherpa::GC_NULL;
  
  if(t->mark & MARK24)
    return t;
  
  t->mark |= MARK24;  
  
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
  case ty_reprv:
  case ty_structr:
  case ty_unionr: 
  case ty_uvalr: 
  case ty_uconr: 
  case ty_reprr:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < rt->typeArgs.size(); i++) {
	GCPtr<Type> arg = rt->TypeArg(i)->getType();
	GCPtr<Type> argMin =
	  arg->minimizeDeepMutability(trail)->getType(); 
	
	if(arg != argMin)
	  trail->link(arg, argMin);
      }
      break;
    } 
    
  case ty_letGather:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < t->components.size(); i++) {
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
  
  t->mark &= ~MARK24;
  return rt;
}



bool 
Type::isMaxMutable()
{
  return strictlyEquals(maximizeMutability());
}

extern GCPtr<TvPrinter> debugTvp;
bool
Type::isMinMutable()
{
  return strictlyEquals(minimizeMutability());
}


static void 
coerceMaybe(GCPtr<Type> t, GCPtr<Trail> trail, bool minimize)
{
  //std::cerr << "Coercing: " << t->asString(Options::debugTvP)
  //	    << " to ";
  
  GCPtr<Type> core = t->Core()->getType();
  if(minimize)
    core = core->minimizeTopMutability()->getType();
  
  GCPtr<Type> var = t->Var()->getType();
  if(core->kind != ty_tvar)
    trail->subst(var, core);
  else
    trail->link(t, core);

  //  std::cerr << t->asString(Options::debugTvP)
  //    << std::endl;
}

void
Type::adjMaybe(GCPtr<Trail> trail, bool markedOnly, 
	       bool minimize, bool adjFn) 
{
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK15)
    return;
  
  t->mark |= MARK15;
    
  switch(t->kind) {
  case ty_mbFull:
    {
      t->Core()->adjMaybe(trail, markedOnly, minimize, adjFn);
      if(!markedOnly || (t->Var()->getType()->flags & TY_COERCE))
	coerceMaybe(t, trail, minimize);
      break;
    }
    
  case ty_mbTop:
    {
      t->Core()->adjMaybe(trail, markedOnly, minimize, adjFn);
      if(!markedOnly || (t->Var()->getType()->flags & TY_COERCE))
	coerceMaybe(t, trail, minimize);
      break;
    }

  case ty_fn:
    {
      if(!adjFn)
	break;
      // otherwise, fall through
    }

  default:
    {
      for(size_t i=0; i < t->typeArgs.size(); i++) 
	t->TypeArg(i)->adjMaybe(trail, markedOnly, minimize, adjFn);
      
      for(size_t i=0; i<t->components.size(); i++)
	t->CompType(i)->adjMaybe(trail, markedOnly, minimize, adjFn);
      
      for(TypeSet::iterator itr = t->fnDeps.begin();
	  itr != t->fnDeps.end(); ++itr)
	(*itr)->adjMaybe(trail, markedOnly, minimize, adjFn);
      
      break;
    }
  }
  
  t->mark &= ~MARK15;
}


// See if nth typeArg is a CCC based on the TY_CCC flag markings
bool
Type::argCCOK(size_t argN)
{
  GCPtr<Type> t = getType();
  assert(t->isValType());
  assert(argN < t->typeArgs.size());
  assert(t->defAst);

  // Be REALLY careful about bt. It is the type in the scheme.
  // NEVER unify it with anyhing.
  GCPtr<Type> bt = t->defAst->symType;  
  if(bt->TypeArg(argN)->getType()->flags & TY_CCC)
    return true;
  else
    return false;
}


/* Determine Candidacy for Copy-Compatibility For type variables only,
   argument is a composite-type that is searched to determine ccc-ness */ 

bool
Type::determineCCC(GCPtr<Type> t, bool inRefType)
{ 
  if (GCPtr<Type>(this) != getType())
    return getType()->determineCCC(t);
  
  t = t->getType();

  if(t->mark & MARK18)
    return true;
  
  t->mark |= MARK18;  
  bool cccOK = true;
  
  switch(t->kind) {
  case ty_tvar:				       
    {
      if((t == shared_from_this()) && (inRefType))
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
      for(size_t i=0; cccOK && (i<t->typeArgs.size()); i++) 
	cccOK = determineCCC(t->TypeArg(i), 
			     inRefType || t->isRefType() || 
			     (!t->argCCOK(i)));
      
      for(size_t i=0; cccOK && (i<t->components.size()); i++)
	cccOK = determineCCC(t->CompType(i), t->isRefType());
      
      // I think no need to process fnDeps here.
      break;
    }
  }

  t->mark &= ~MARK18;
  return cccOK;
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
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK25)
    return;
  
  t->mark |= MARK25;  
  
  switch(t->kind) {
    
  case ty_tvar:
    {
      t->flags &= ~TY_COERCE;
      break;
    }
    
  case ty_mbFull:    
  case ty_mbTop:    
    {
      if(!cppos)
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
      for(size_t i=0; i < t->components.size(); i++)
	t->CompType(i)->markSignMbs(true);
      break;
    }
    
  case ty_letGather:
    {
      for(size_t i=0; i < t->components.size(); i++)
	t->CompType(i)->markSignMbs(cppos);
      break;
    }
    
  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_reprv:
    {
      for(size_t i=0; i < t->typeArgs.size(); i++) {
	GCPtr<Type> arg = t->TypeArg(i)->getType();
	if(t->argCCOK(i))
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
  case ty_reprr:
    {
      for(size_t i=0; i < t->typeArgs.size(); i++) {
	GCPtr<Type> arg = t->TypeArg(i)->getType();
	arg->markSignMbs(false);
      }
      break;
    }
    
  default:
    {
      break;
    }
  }
  
  t->mark &= ~MARK25;
  return;
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
Type::fixupFnTypes()
{
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK26)
    return;
  
  t->mark |= MARK26;
  
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
      GCPtr<Type> ret = t->Ret()->getType();
      if(ret->kind != ty_mbFull)
	t->Ret() = MBF(ret);
      break;
    }

  case ty_fnarg:
    {
      for(size_t i=0; i < t->components.size(); i++) {
	t->CompType(i)->fixupFnTypes();
	GCPtr<Type> arg = t->CompType(i)->getType();
	if((t->CompFlags(i) & COMP_BYREF) == 0) {
	  if(arg->kind != ty_mbFull)
	    t->CompType(i) = MBF(arg);
	}
      }
      
      break;
    }
  
  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_reprv:
  case ty_structr:
  case ty_unionr: 
  case ty_uvalr: 
  case ty_uconr: 
  case ty_reprr:
    {
      for(size_t i=0; i < t->typeArgs.size(); i++)
	t->TypeArg(i)->fixupFnTypes();
      
      break;
    }

  case ty_letGather:
    {
      for(size_t i=0; i < t->components.size(); i++) 
	t->CompType(i)->fixupFnTypes();
      break;
    }
    
  default:
    {
      break;
    }
  }
  
  t->mark &= ~MARK26;
}
