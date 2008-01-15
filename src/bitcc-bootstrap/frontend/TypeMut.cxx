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
 


// Gives the type of the copy, introduces explicit maybe-mutability.
GCPtr<Type> 
Type::addShallowMaybe()
{
  GCPtr<Type> t = getType();
  GCPtr<Type> bt = t->getBareType();
  GCPtr<Type> mt = new Type(ty_maybe, bt);
  
  mt->hints = new Type(ty_hint, t);
  return mt;
}

GCPtr<Type> 
Type::addShallowMutable()
{
  GCPtr<Type> bt = getBareType();
  GCPtr<Type> mt = new Type(ty_mutable, bt);

  return mt;
}

GCPtr<Type> 
Type::TypeOfCopy()
{
  if(Options::topMutOnly)
    return addShallowMaybe();

  GCPtr<Type> t = getType();
  GCPtr<Type> rt = NULL;

  if(t->mark & MARK17)
    return t;
  
  t->mark |= MARK17;  
  
  switch(t->kind) {

  case ty_maybe:
  case ty_mutable:
    {
      rt = t->CompType(0)->TypeOfCopy()->getType();
      assert(rt->kind == ty_maybe);
      assert(rt->hints);
      assert(rt->hints->components->size() == 1);
      rt->hints->CompType(0) = t;
      break;
    }

  case ty_fnarg:
  case ty_byref:
  case ty_typeclass:
  case ty_letGather:
  case ty_hint:
  case ty_subtype:
  case ty_pcst:
  case ty_kvar:
  case ty_kfix:
    assert(false);
    
  case ty_tvar:
  case ty_unit:
  case ty_bool:
  case ty_char:
  case ty_string:
  case ty_int8:
  case ty_int16:
  case ty_int32:
  case ty_int64:
  case ty_uint8:
  case ty_uint16:
  case ty_uint32:
  case ty_uint64:
  case ty_word:
  case ty_float:
  case ty_double:
  case ty_quad:
  case ty_dummy:
  case ty_tyfn:
#ifdef KEEP_BF
  case  ty_bitfield:
#endif
    {
      rt = t->addShallowMaybe();
      break;
    }
    
  case ty_fn:
  case ty_vector:
  case ty_ref:
  case ty_structr:
  case ty_unionr:
  case ty_uvalr:
  case ty_uconr:
  case ty_reprr:
  case ty_exn:
    {
      rt = t->addShallowMaybe();
      break;
    }

  case ty_array:
    {
      rt = new Type(t);
      rt->CompType(0) = t->CompType(0)->TypeOfCopy();
      rt = rt->addShallowMaybe();
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_reprv:
    {
      rt = t->defAst->scheme->type_instance_copy();
      rt->kind = t->kind; // Sometime, we need ucon/uval fixup

      for(size_t i=0; i < rt->typeArgs->size(); i++) {
	GCPtr<Type> tArg = t->TypeArg(i)->getType();

	assert(tArg->kind != ty_tvar); // We should never be dealing
	              // with bare tvars, except in the type-schemes. 
	
	if(rt->argCCOK(i))
	  rt->TypeArg(i)->link = tArg->TypeOfCopy();
	else
	  rt->TypeArg(i)->link = tArg;	
      }

      // One should NEVER make a DCopy() here.
      //rt = t->getDCopy();
      //for(size_t i=0; i < rt->typeArgs->size(); i++) {
      // ...
      //}

      rt = rt->addShallowMaybe();
      break;
    }
  }
  
  t->mark &= ~MARK17;
  return rt;
}

bool 
Type::copy_compatible_compat(GCPtr<Type> t, bool verbose, std::ostream &errStream)
{
  GCPtr<Type> copy = TypeOfCopy();
  return copy->compatible(t->TypeOfCopy(), verbose, errStream); 
}

bool 
Type::copy_compatible_eql(GCPtr<Type> t, bool verbose, std::ostream &errStream)
{
  GCPtr<Type> copy = TypeOfCopy();
  return copy->equals(t->TypeOfCopy(), verbose, errStream); 
}

GCPtr<Type> 
Type::maximizeMutability(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  GCPtr<Type> rt = NULL;

  if(t->mark & MARK17)
    return t;
  
  t->mark |= MARK17;  
  
  switch(t->kind) {
    
  case ty_maybe:    
  case ty_mutable:
    {
      rt = t->CompType(0)->maximizeMutability(trail);
      break;
    }

  case ty_array:
    {
      rt = new Type(t);
      rt->CompType(0) = 
	t->CompType(0)->maximizeMutability(trail);
      rt = t->addShallowMutable();      
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_reprv:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < rt->typeArgs->size(); i++) {
	GCPtr<Type> arg = rt->TypeArg(i)->getType();
	assert(arg->kind != ty_tvar);
	if(rt->argCCOK(i))	  
	  trail->link(arg, arg->maximizeMutability(trail));       
      }
      rt = rt->addShallowMutable();
      break;
    } 
    
  default:
    {
      rt = t->getDCopy()->addShallowMutable();
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
  GCPtr<Type> rt = NULL;
  
  if(t->mark & MARK19)
    return t;
  
  t->mark |= MARK19;  
  
  switch(t->kind) {
    
  case ty_maybe:    
  case ty_mutable:
    {
      rt = t->CompType(0)->minimizeMutability(trail);
      break;
    }

  case ty_array:
    {
      rt = new Type(t);
      rt->CompType(0) = 
	t->CompType(0)->minimizeMutability(trail);
      break;
    }

  case ty_structv:
  case ty_unionv: 
  case ty_uvalv: 
  case ty_uconv: 
  case ty_reprv:
    {
      rt = t->getDCopy();
      for(size_t i=0; i < rt->typeArgs->size(); i++) {
	GCPtr<Type> arg = rt->TypeArg(i)->getType();
	assert(arg->kind != ty_tvar);
	if(rt->argCCOK(i))	  
	  trail->link(arg, arg->minimizeMutability(trail));       
      }
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
  //   GCPtr<Type> min = minimizeMutability();
  //   std::cout << "**  t = " << asString(debugTvp) 
  // 	    << std::endl;
  //   std::cout << "  min = " << min->asString(debugTvp) 
  // 	    << std::endl;
  //   bool isMin = strictlyEquals(min);
  //   if(isMin)
  //     std::cout << "   [MINIMAL]" << min->asString(debugTvp) 
  // 	      << std::endl;
  //   else
  //     std::cout << "   [NOT MINIMAL]" << min->asString(debugTvp)
  // 	      << std::endl;
  //   return isMin;
}

  


void 
Type::addHint(GCPtr<Type> theHint)
{
  GCPtr<Type> t = getType();
  theHint = theHint->getType();
  assert(t->kind == ty_hint);
  assert(theHint->kind != ty_hint);
  
  bool found = false;
  for(size_t i=0; i < t->components->size(); i++) {
    GCPtr<Type> thisHint = t->CompType(i)->getType();
    if(thisHint == theHint) {
      found = true;
      break;
    }
  }

  if(!found)
    t->components->append(new comp(theHint));
}

void
Type::clearHints(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK14)
    return;
  
  t->mark |= MARK14;
  
  switch(t->kind) {
  case ty_maybe:
    if((t->hints) && t->hints->components->size())
      trail->link(t->hints, new Type(ty_hint, t->hints->ast));
    // fall through
    
  default:
    for(size_t i=0; i < t->typeArgs->size(); i++) 
      t->TypeArg(i)->clearHints(trail);
    
    for(size_t i=0; i<t->components->size(); i++)
      t->CompType(i)->clearHints(trail);
    
    if(t->fnDeps)
      for(size_t i=0; i < t->fnDeps->size(); i++)
	t->FnDep(i)->clearHints(trail);
    
    break;
  }

  t->mark &= ~MARK14;
}

void 
TCConstraints::clearHintsOnPreds(GCPtr<Trail> trail)
{
  for(size_t i=0; i < pred->size(); i++)
    Pred(i)->clearHints(trail);
}

void 
TypeScheme::adjMaybes(GCPtr<Trail> trail)
{
  tau->adjMaybe(trail);
  if(tcc)
    tcc->clearHintsOnPreds(trail);
}

void
Type::groundMaybe(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();
  switch(t->kind) {
  case ty_maybe:
    trail->link(t, t->CompType(0)->getType()); 
    break;    

  default:
    break;
  }
}

static inline GCPtr<Type> 
hintTvar(GCPtr<AST> ast)
{
  return newBareTvar(ast);
}

GCPtr<Type> 
Type::simplifiedHint()
{
  GCPtr<Type> t = getType();

  GCPtr<Type> rt = NULL;
  //   LexLoc _loc;
  //   GCPtr<AST> dummy = new AST(at_Null, _loc);  

  if(t->mark & MARK16)
    return newBareTvar(ast);

  t->mark |= MARK16;

  switch(t->kind) {

  case ty_maybe:
    {
      if(t->hints)
	rt =  t->hints->simplifiedHint();
      else
	rt = new Type(ty_maybe, 
		      t->CompType(0)->simplifiedHint());
      break;
    }

  case ty_hint:
    {
      GCPtr<Type> immut = NULL;
      GCPtr<Type> mut = NULL;
      GCPtr<Type> maybe = NULL;
      for(size_t i=0; i < t->components->size(); i++) {
	GCPtr<Type> hint = t->CompType(i);
	hint = hint->simplifiedHint();

 	if(hint->isMaybe())
	  maybe = hint;
	else if(hint->isMutable())
	  mut = hint;
	else
	  immut = hint;
      }

      // Pincking maybe over mutable may seem like we are losing
      // information. but consider: 
      // (define (f x:'a y:(mutable 'b)) (if #t x y)) 
      if(immut)
	rt = immut;
      else if (maybe)
	rt = maybe;
      else if (mut)
	rt = mut;
      else
	rt = hintTvar(ast);
      break;
    }

  case ty_mutable:
    {
      rt = new Type(ty_mutable, newBareTvar(ast));
      break;
    }

  case ty_fnarg:
  case ty_byref:
  case ty_typeclass:
  case ty_subtype:
  case ty_pcst:
  case ty_kvar:
  case ty_kfix:
  case ty_letGather:
    assert(false);
    
  case ty_tvar:
  case ty_unit:
  case ty_bool:
  case ty_char:
  case ty_string:
  case ty_int8:
  case ty_int16:
  case ty_int32:
  case ty_int64:
  case ty_uint8:
  case ty_uint16:
  case ty_uint32:
  case ty_uint64:
  case ty_word:
  case ty_float:
  case ty_double:
  case ty_quad:
  case ty_dummy:
  case ty_tyfn:
#ifdef KEEP_BF
  case  ty_bitfield:
#endif
    {
      rt = t;
      break;
    }
    
  case ty_fn:
    {
      GCPtr<Type> oldFnArgs = t->CompType(0);
      GCPtr<Type> newFnArgs = new Type(ty_fnarg, ast);
      for(size_t i=0; i < oldFnArgs->components->size(); i++)
	newFnArgs->components->append(new comp(hintTvar(ast),
					       oldFnArgs->CompFlags(i)));
      
      rt = new Type(ty_fn, newFnArgs, hintTvar(ast));
      break;
    }
    
  case ty_array:
  case ty_vector:
  case ty_ref:
    {
      rt = new Type(t);
      for(size_t i=0; i < t->components->size(); i++)
	rt->CompType(i) = hintTvar(ast);      
      break;
    }
   
  case ty_structv:
  case ty_structr:
  case ty_unionv: 
  case ty_unionr:
  case ty_uvalv: 
  case ty_uvalr:
  case ty_uconv: 
  case ty_uconr:
  case ty_reprr:
  case ty_reprv:
  case ty_exn:
    {
      // For Structur and union base types, myContainer = defAst.
      rt = t->myContainer->scheme->type_instance_copy();
      rt->kind = t->kind; // Sometime, we need ucon/uval fixup

      GCPtr<Trail> trail = new Trail;
      for(size_t i=0; i < rt->typeArgs->size(); i++)
	rt->TypeArg(i)->groundMaybe(trail);
      break;
    }
  }
  
  assert(rt);

  t->mark &= ~MARK16;
  return rt;
}

void
Type::adjMaybe(GCPtr<Trail> trail)
{
  GCPtr<Type> t = getType();

  if(t->mark & MARK15)
    return;

  t->mark |= MARK15;
    
  switch(t->kind) {
  case ty_maybe:
    {
      t->CompType(0)->adjMaybe(trail);
      if(t->hints) {
	GCPtr<Type> sh = t->hints->simplifiedHint();
// 	std::cerr << "Trying to Unify: " 
// 		  << t->asString(0) << sh->asString(0)
// 		  << std::endl;
	t->unifyWith(sh, false, trail);
// 	std::cerr << "Result = " 
// 		  << t->asString(0)
// 		  << std::endl;	
      }
      t->groundMaybe(trail);
      break;
    }

  default:
    {
      for(size_t i=0; i < t->typeArgs->size(); i++) 
	t->TypeArg(i)->adjMaybe(trail);
      
      for(size_t i=0; i<t->components->size(); i++)
	t->CompType(i)->adjMaybe(trail);
      
      if(t->fnDeps)
	for(size_t i=0; i < t->fnDeps->size(); i++)
	  t->FnDep(i)->adjMaybe(trail);
      
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
  assert(argN < t->typeArgs->size());
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
      if((t == this) && (inRefType))
	cccOK = false;
      break;
    }

  case ty_typeclass:
  case ty_tyfn:
  case ty_hint:
    {
      assert(false);
      break;
    }

  default:
    {
      for(size_t i=0; cccOK && (i<t->typeArgs->size()); i++) 
	cccOK = determineCCC(t->TypeArg(i), 
			     inRefType || t->isRefType() || 
			     (!t->argCCOK(i)));
      
      for(size_t i=0; cccOK && (i<t->components->size()); i++)
	cccOK = determineCCC(t->CompType(i), t->isRefType());
      
      // I think no need to process fnDeps here.
      break;
    }
  }

  t->mark &= ~MARK18;
  return cccOK;
}

