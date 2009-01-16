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
#include "Typeclass.hxx"
#include "machine-dep.hxx"

using namespace boost;
using namespace sherpa;
using namespace std;

unsigned long long Type::typeCount=0;
shared_ptr<Type> Type::Kmono = Type::make(ty_kfix);
shared_ptr<Type> Type::Kpoly = Type::make(ty_kfix);

static struct {
  const char *nm;
  bool isPrimary;
  bool isSimpleTypeForC;
  bool isAtomic;
  bool isScalarType;
  bool isRefType;
  bool isPrimInt;
  bool isPrimFloat;
} kindInfo[] = {
#define DEFKIND(nm, prim, csimp, atom, scalar, ref, primInt, primFloat)	\
  { #nm, prim, csimp, atom, scalar, ref, primInt, primFloat },
#include "kind.def"
};

const char *
KindName(Kind k)
{
  return kindInfo[k].nm;
}

Kind
Type::LookupKind(const std::string& s)
{
  // The check for isPrimary is completely unnecessary, but
  // it makes the fail cases faster.
  for (size_t i = 0; i < (sizeof(kindInfo) / sizeof(kindInfo[0])); i++)
    if (kindInfo[i].isPrimary && kindInfo[i].nm == s)
      return (Kind) i;

  assert(false);
}


Kind 
Type::getValKind(const Kind refKind)
{
  switch(refKind) {
  case ty_structr:
    return ty_structv;

  case ty_unionr:
    return ty_unionv;

  case ty_uconr:
    return ty_uconv;

  case ty_uvalr:
    return ty_uvalv;

  case ty_ref:
    return ty_tvar;

  default:
    assert(false);
    return ty_tvar;
  }
}

Kind 
Type::getRefKind(const Kind valKind)
{
  switch(valKind) {
  case ty_structv:
    return ty_structr;

  case ty_unionv:
    return ty_unionr;

  case ty_uconv:
    return ty_uconr;

  case ty_uvalv:
    return ty_uvalr;

  case ty_tvar:
    return ty_ref;

  default:
    assert(false);
    return ty_ref;
  }
}


shared_ptr<const Type> 
Type::getTypePrim() const
{ 
  shared_ptr<const Type> curr = shared_from_this();
  while (curr->link)
    curr = curr->link;
  
  return curr;
}

shared_ptr<Type> 
Type::getTypePrim()
{ 
  shared_ptr<Type> curr = shared_from_this();
  while (curr->link)
    curr = curr->link;
  
  return curr;
}


// Normalize Mutability Constructor Idempotence
// Deal with the fact that 
// (mutable (mutable t)) == (mutable t)
// We cannot avoid this type of linkage because of a structure
// getting parametrized over a mutable type while having a mutable
// wrapper at the field level.  
shared_ptr<Type> 
Type::normalize_mut()
{ 
  shared_ptr<Type> t = getTypePrim();
  shared_ptr<Type> in = t;
  
  while (in->kind == ty_mutable) {
    t = in;
    in = in->Base()->getTypePrim();
  }
  
  return t;
}
 
shared_ptr<Type> 
Type::getType()
{ 
  shared_ptr<Type> t = normalize_mut();
  
  // Maybe-types must be handled with special care:
  // mbTop can only be of the form 'a!t where the Var() part is a
  // type variable. If the Var() part of this mbTop type is not a
  // type variable, then, it has unified with some other type,
  // and we must follow that link.
  // mbFull can be of the form 'a!t or M'a|t. Otherwise, we follow
  // the link to whatever type the Var() part has unified with.
  // Maybe types may not be recursively nested.
  // The Var() part of an mbTop must only be linked to 
  // (be substituted by) an unconstrained type./
  // The Var() part of an mbFull can be linked to an mbTop type or an
  // unconstrained type.

  if (t->kind == ty_mbFull) {
    shared_ptr<Type> in = t->Var()->normalize_mut();
    shared_ptr<Type> within = ((in->kind == ty_mutable) ?
			       in->Base()->getTypePrim(): in);
    
    if(within->kind != ty_tvar)
      t = in;
  }

  if(t->kind == ty_mbTop) {
    shared_ptr<Type> in = t->Var()->normalize_mut();
    if(in->kind != ty_tvar)
      t = in;
  }

  if (t->kind == ty_mbFull || t->kind == ty_mbTop) {
    shared_ptr<Type> var = t->Var()->normalize_mut();
    shared_ptr<Type> core = t->Core()->normalize_mut();
    
    if (var == core)
      t = core;
  }

  return t;
}

shared_ptr<const Type> 
Type::getType() const
{ 
  shared_ptr<const Type> t = getTypePrim();
  
  if (t->kind == ty_mutable) {    
    shared_ptr<Type> in = t->components[0]->typ->getTypePrim();
    while (in->kind == ty_mutable) {
      t = in;
      in = t->components[0]->typ->getTypePrim();
    }
  }
  
  return t;
}

shared_ptr<Type> 
Type::getBareType()
{ 
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_GET_BARE_TYPE)
    return t;
  
  t->mark |= MARK_GET_BARE_TYPE;

  shared_ptr<Type> retType = t;

  if (t->isMaybe())
    retType = t->Core()->getBareType();  
  
  if (t->isMutable())
    retType = t->Base()->getBareType();  

  t->mark &= ~MARK_GET_BARE_TYPE;
  return retType;
}

shared_ptr<Type> 
Type::getTheType(bool mutableOK, bool maybeOK)
{ 
  shared_ptr<Type> t = getType();  

  if (t->mark & MARK_GET_THE_TYPE)
    return t;
  
  t->mark |= MARK_GET_THE_TYPE;

  shared_ptr<Type> retType = t;
  
  if ((t->kind == ty_mutable) && !mutableOK)
    retType = t->Base()->getTheType(mutableOK, maybeOK);
  else if (t->isMaybe() && !maybeOK)
    retType = t->Core()->getTheType(mutableOK, maybeOK);
  
  t->mark &= ~MARK_GET_THE_TYPE;
  return retType;
}

bool
Type::isTvar()
{
  shared_ptr<Type> t = getType();  
  return (t->kind == ty_tvar);
}

bool
Type::isVariable()
{
  shared_ptr<Type> t = getBareType();  
  return (t->kind == ty_tvar);
}

bool
Type::isUnifiableVar(UnifyFlags uflags)
{
  shared_ptr<Type> t = getType();
  shared_ptr<Type> var = t;
  
  switch(t->kind) {
  case ty_tvar:
    var = t;
    break;

  case ty_mbTop:
    var = t->Var()->getType();
    break;

  case ty_mbFull:
    var = t->Var()->getType();
  
    if(var->isMutable())
      var = var->Base()->getType();
    break;
    
  default:
    return false;
  }

  if (uflags & UFLG_UN_IGN_RIGIDITY)
    return true;
  
  return ((var->flags & TY_RIGID) == 0);
}

bool 
Type::isAtomic()
{
  shared_ptr<Type> t = getBareType();  
  return kindInfo[t->kind].isAtomic;
}

bool 
Type::isSimpleTypeForC()
{
  shared_ptr<Type> t = getBareType();
  return kindInfo[t->kind].isSimpleTypeForC;
}

bool 
Type::isScalar()
{
  shared_ptr<Type> t = getBareType();  
  return kindInfo[t->kind].isScalarType;
}

bool 
Type::isRefType()
{
  shared_ptr<Type> t = getBareType();
  return kindInfo[t->kind].isRefType;
}

bool 
Type::isValType()
{
  return !isRefType();
}

bool 
Type::isByrefType()
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_byref);
}

bool 
Type::isNullableType()
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_unionv &&
	  (t->defAst->flags & NULLABLE_UN));
}


// Is the current type constrained by (ref-types t) 
// within the constraint set tcc?
bool 
Type::isConstrainedToRefType(boost::shared_ptr<TCConstraints> tcc)
{
  shared_ptr<Type> t = getType();
  
  for (TypeSet::iterator itr = tcc->begin(); itr != tcc->end(); ++itr) {
    shared_ptr<Typeclass> pred = (*itr);
    
    const std::string &ref_types = SpecialNames::spNames.sp_ref_types; 
    
    if (pred->defAst->s == ref_types) {
      shared_ptr<Type> arg = pred->TypeArg(0)->getType();
      if (strictlyEquals(arg, false, true))
	return true;
    }
  }

  return false;
}


bool 
Type::isFnxn()
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_fn);
}

bool 
Type::isBaseConstType()
{
  switch(getType()->kind) {
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

    // FIX: case ty_lit:
    return true;
  default:
    return false;
  }
}


//#if 0
bool 
Type::isClosure()
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_fn);
}
//#endif

bool 
Type::isMutable()
{
  shared_ptr<Type> t = getType();
  return t->kind == ty_mutable;
}

bool 
Type::isMutType()
{
  shared_ptr<Type> t = getType();
  return(t->isMutable() ||
	 (t->isMbFull() && t->Var()->isMutable()));
}

bool 
Type::isConst()
{
  shared_ptr<Type> t = getType();
  return t->kind == ty_const;
}

bool 
Type::isMaybe()
{
  shared_ptr<Type> t = getType();
  return (t->kind == ty_mbTop || t->kind == ty_mbFull);
}

bool 
Type::isMbFull()
{
  shared_ptr<Type> t = getType();
  return (t->kind == ty_mbFull);
}

bool 
Type::isMbTop()
{
  shared_ptr<Type> t = getType();
  return (t->kind == ty_mbTop);
}
 
bool 
Type::isMaxMutable()
{
  return strictlyEquals(maximizeMutability());
}

extern shared_ptr<TvPrinter> debugTvp;
bool
Type::isMinMutable()
{
  return strictlyEquals(minimizeMutability());
}

bool 
Type::isPrimInt()
{
  shared_ptr<Type> t = getBareType();
  return (kindInfo[t->kind].isPrimInt);
}

bool 
Type::isPrimFloat()
{
  shared_ptr<Type> t = getBareType();
  return (kindInfo[t->kind].isPrimFloat);
}

bool 
Type::isPrimaryType()
{
  shared_ptr<Type> t = getBareType();
  return (kindInfo[t->kind].isPrimary);
}
bool 
Type::isInteger()
{
  shared_ptr<Type> t = getBareType();
  switch(t->kind) {
  case ty_int8:
  case ty_int16:
  case ty_int32:
  case ty_int64:
  case ty_uint8:
  case ty_uint16:
  case ty_uint32:
  case ty_uint64:
#ifdef KEEP_BF
  case ty_bitfield:
#endif
    return true;
  default:
    return false;
  }
}

bool 
Type::isbool()
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_bool);
}

bool 
Type::isIntegral()
{
  shared_ptr<Type> t = getBareType();
  switch(t->kind) {
  case ty_int8:
  case ty_int16:
  case ty_int32:
  case ty_int64:
  case ty_uint8:
  case ty_uint16:
  case ty_uint32:
  case ty_uint64:
#ifdef KEEP_BF
  case ty_bitfield:
#endif
  case ty_word:
    return true;
  default:
    return false;
  }
}


size_t 
Type::nBits()
{
  shared_ptr<Type> t = getBareType();
  switch(t->kind) {
  case ty_bool:
    return 1;

  case ty_int8:
  case ty_uint8:
    return 8;
  case ty_int16:
  case ty_uint16:
    return 16;

  case ty_int32:
  case ty_uint32:
    return 32;

  case ty_int64:
  case ty_uint64:
    return 64;
    
#ifdef KEEP_BF
  case ty_bitfield:
    return t->Isize;
#endif

  case ty_word:
    return TARGET_WORD_SIZE;

  default:
    assert(false);
  }
}


bool 
Type::isFloat()
{
  shared_ptr<Type> t = getBareType();
  switch(t->kind) {
  case ty_float:
  case ty_double:
  case ty_quad:
    return true;
  default:
    return false;
  }
}

bool
Type::isTypeClass()
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_typeclass);    
}

bool
Type::isPcst()
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_pcst);    
}

// Returns true if this is a union or a structure definition 
// respectively
bool 
Type::isUnion(bool ignMut) 
{
  shared_ptr<Type> t = ((ignMut) ? getBareType() : getType());
  return (((t->kind == ty_unionv) || (t->kind == ty_unionr)) &&
	  (t->components.size()));
}

bool 
Type::isUcon(bool ignMut) 
{
  shared_ptr<Type> t = ((ignMut) ? getBareType() : getType());
  return ((t->kind == ty_uconv) || (t->kind == ty_uconr));
}

bool 
Type::isUval(bool ignMut) 
{
  shared_ptr<Type> t =  ((ignMut) ? getBareType() : getType());
  return ((t->kind == ty_uvalv) || (t->kind == ty_uvalr));
}

bool 
Type::isULeg(bool ignMut) 
{
  return(isUcon(ignMut) || isUval(ignMut));
}

bool 
Type::isUType(bool ignMut) 
{
  return(isUnion(ignMut) || isUcon(ignMut) || isUval(ignMut));
}

bool
Type::isDeepMut()
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_PREDICATE)
    return true;
  
  t->mark |= MARK_PREDICATE;
  
  bool mut = false;
  
  switch(t->kind) {
  case ty_mutable:
    mut = true;
    break;
    
  case ty_fn:
    break;

  default:
    for (size_t i=0;!mut &&  i < t->components.size(); i++)
      mut = t->CompType(i)->isDeepMut();
   
    for (size_t i=0; !mut && i < t->typeArgs.size(); i++)
      mut = t->TypeArg(i)->isDeepMut();

    // No need to check functional dependencies
    // May lead to error if checked because functional dependencies
    // might be on types that are used within a function constructor
    break;
  }
  
  t->mark &= ~MARK_PREDICATE;
  return mut;
}
  
bool 
Type::isDeepImmut()
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_PREDICATE)
    return true;
  
  t->mark |= MARK_PREDICATE;
  
  bool immut = true;
  
  switch(t->kind) {
  case ty_mutable:
  case ty_tvar:
    immut = false;
    break;
 
  case ty_fn:
    break;
    
  default:
    for (size_t i=0; immut && i < t->components.size(); i++)
      immut = t->CompType(i)->isDeepImmut();
    
    for (size_t i=0; immut && i < t->typeArgs.size(); i++)
      immut = t->TypeArg(i)->isDeepImmut();

    // No need to check functional dependencies
    // May lead to error if checked because functional dependencies
    // might be on types that are used within a function constructor
    
    break;
  }

  t->mark &= ~MARK_PREDICATE;
  return immut;  
}

bool 
Type::isConcretizable()
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_PREDICATE)
    return true;
  
  t->mark |= MARK_PREDICATE;
  
  bool concretizable = true;
  
  switch(t->kind) {
  case ty_tvar:
    concretizable = false;
    break;

  case ty_mbTop:
  case ty_mbFull:
    concretizable = t->Core()->isConcretizable();
    
  case ty_fn:
    break;
    
  default:
    for (size_t i=0; concretizable && i < t->typeArgs.size(); i++)
      concretizable = t->TypeArg(i)->isConcretizable();
    break;
  }
  
  t->mark &= ~MARK_PREDICATE;
  return concretizable;  
}

bool 
Type::isShallowConcretizable()
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_PREDICATE)
    return true;
  
  t->mark |= MARK_PREDICATE;
  
  bool concretizable = true;
  
  switch(t->kind) {
  case ty_tvar:
    concretizable = false;
    break;

  case ty_mbTop:
  case ty_mbFull:
    concretizable = t->Core()->isShallowConcretizable();
    
  case ty_mutable:
  case ty_array:
    concretizable = t->Base()->isShallowConcretizable();
    break;
    
  case ty_structv:
  case ty_unionv:
  case ty_uvalv:
  case ty_uconv:
    for (size_t i=0; concretizable && i < t->typeArgs.size(); i++)
      concretizable = t->TypeArg(i)->isShallowConcretizable();
    break;

  default:
    break;
  }
  
  t->mark &= ~MARK_PREDICATE;
  return concretizable;  
}

bool 
Type::isEffectivelyConst()
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_PREDICATE)
    return true;
  
  t->mark |= MARK_PREDICATE;
  
  bool effConst = true;
  
  switch(t->kind) {
  case ty_tvar:
  case ty_mbTop:
  case ty_mbFull:
  case ty_mutable:
    effConst = false;
    
  case ty_array:
  case ty_structv:
  case ty_unionv:
  case ty_uvalv:
  case ty_uconv:
    for (size_t i=0; effConst && i < t->components.size(); i++)
      effConst = t->CompType(i)->isEffectivelyConst();
    break;
    
  default:
    break;
  }
  
  t->mark &= ~MARK_PREDICATE;
  return effConst;
}

bool 
Type::isConstReducible()
{
  shared_ptr<Type> t = getType();
  
  if (t->mark & MARK_PREDICATE)
    return true;
  
  t->mark |= MARK_PREDICATE;
  
  bool constred = true;
  
  switch(t->kind) {
  case ty_tvar:
  case ty_mbTop:
  case ty_mbFull:
    constred = false;
    
  case ty_mutable:
  case ty_array:
  case ty_structv:
  case ty_unionv:
  case ty_uvalv:
  case ty_uconv:
    for (size_t i=0; constred && i < t->components.size(); i++)
      constred = t->CompType(i)->isConstReducible();
    break;

  default:
    break;
  }
  
  t->mark &= ~MARK_PREDICATE;
  return constred;
}

void 
Type::normalize(boost::shared_ptr<Trail> trail)
{
  normalize_mbFull(trail);
}


/* Produce Type ty_union[rv] from ty_ucon[rv] or ty_uval[rv]
   ONLY typeArgs are populated */
shared_ptr<Type> 
Type::getUnionType()
{
  shared_ptr<Type> uType = getType();
  assert(uType->isUType());
  shared_ptr<Type> uCopy = uType->getType()->getDCopy();
  shared_ptr<Type> t = uCopy->getBareType();
  t->kind = (uType->isRefType() ? ty_unionr : ty_unionv);
  t->defAst = t->myContainer;
  t->components.clear();
  return uCopy;
}

bool 
Type::isException() 
{
  shared_ptr<Type> t = getBareType();
  return (t->kind == ty_exn);
}

bool 
Type::isStruct()
{
  shared_ptr<Type> t = getBareType();

  // WAS  return (((t->kind == ty_structv) || 
  //               (t->kind == ty_structr)) &&
  //           	  (t->components->size() != 0));
  return ((t->kind == ty_structv) || 
	  (t->kind == ty_structr));
}

bool 
Type::isDecl()
{
  shared_ptr<Type> t = getBareType();
  switch(t->kind) {
  case ty_unionv:
  case ty_unionr:
  case ty_structv:
  case ty_structr:
    return (t->components.empty());

  default:
    return false;
  }  
}

bool
Type::isOfInfiniteType()
{
  bool infType = false;

  if (getType() != shared_from_this())
    return getType()->isOfInfiniteType();

  if (mark & MARK_PREDICATE) 
    return true;

  mark |= MARK_PREDICATE;

  switch(kind) {
  case ty_tvar:
  case ty_kvar:
  case ty_kfix:
  case ty_dummy:
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
#ifdef KEEP_BF
  case ty_bitfield:
#endif
    {
      infType = false;
      break;
    }

  case ty_mbFull:
  case ty_mbTop:
  case ty_array:
  case ty_vector:
  case ty_ref:
  case ty_byref:
  case ty_mutable:
  case ty_const:
  case ty_fn:
    {
      for (size_t i=0; !infType && (i < components.size()); i++)
      	if (CompType(i)->isOfInfiniteType())
      	  infType = true;
      break;
    }

  case ty_letGather:
  case ty_tyfn:
  case ty_fnarg:
  case ty_structv:
  case ty_structr:
  case ty_uconv:
  case ty_uconr:
  case ty_uvalv:
  case ty_uvalr:
  case ty_unionv:
  case ty_unionr:
  case ty_typeclass:
  case ty_exn:
  case ty_pcst:
    {      
      for (size_t i=0; !infType && (i < typeArgs.size()); i++)
      	if (TypeArg(i)->isOfInfiniteType())
      	  infType = true;

      break;
    }
  }
  
 mark &= ~MARK_PREDICATE;
 return infType;
}

// Return true iff a binding of this type requires capture conversion
// (by migrating it into the heap). Rules are as follows:
//
//   1. Mutable types *always* require capture 
//   2. Otherwise, scalar and reference types do not require
//      capture conversion.
//   3. Otherwise, the type requires capture conversion. This
//      is conservative, but correct.
//   4. Function types need heapification! This case is handled
//      separately since functions are marked scalars in kind.def
//      (they actually are, except for this case). The issue here is
//      that we are talking about a function *pointer* which must be a
//      real location (alloc-ref) before the code for the closure can
//      be produced, and later we can set the function pointer to the
//      correct value (copy-ref).  

bool
Type::needsCaptureConversion()
{
  shared_ptr<Type> t = getType();
  if (t->isMutable())
    return true;
  if (t->isFnxn())
    return true;
  else if (t->isRefType())
    return false;
  else if (t->isScalar())
    return false;
  
  // All other cases require conversion. 
  // This is somewhat conservative.
  return true;
}


bool 
Type::isConcrete()
{
  TypeSet tvs;
  collectAllftvs(tvs);
  return (tvs.empty());
}

void 
Type::SetTvarsTo(shared_ptr<Type> t)
{
  TypeSet tvs;
  collectAllftvs(tvs);
  
  for (TypeSet::iterator itr = tvs.begin(); itr != tvs.end(); ++itr)
    (*itr)->getType()->link = t;
}

void 
Type::SetTvarsToUnit()
{
  TypeSet tvs;
  collectAllftvs(tvs);
  
  for (TypeSet::iterator itr = tvs.begin(); itr != tvs.end(); ++itr) {
    shared_ptr<Type> ftv = (*itr)->getType();
    shared_ptr<Type> unit = Type::make(ty_unit);
    ftv->link = unit;
  }
}
  
comp::comp(shared_ptr<Type> t, CompFlagSet _flags) 
{
  name = "";
  typ = (shared_ptr<Type> )t;
  flags=_flags;
}
  
comp::comp(const std::string s, shared_ptr<Type> t, CompFlagSet _flags) 
{
  name = s;
  typ = t;
  flags = _flags;
}

// comp::comp(const comp &c)
// {
//   name = c.name;
//   typ = c.typ;
//   flags = c.flags;  
// }

#define TYPE_CTR_INIT(k) do {			\
    kind = k;					\
    defAst = GC_NULL;				\
    arrLen = ArrLen::make(0);			\
    Isize = 0;					\
    minSignedRep = 0;				\
    minUnsignedRep = 0;				\
    mark = MARK_NONE;				\
    pMark = 0;					\
    sp = GC_NULL;				\
    myContainer = GC_NULL;			\
    link = GC_NULL;				\
    flags = TY_NO_FLAGS;			\
  } while (0);


Type::Type(const Kind k)
  : uniqueID(genTypeID())
{
  TYPE_CTR_INIT(k);
}

Type::Type(const Kind k, shared_ptr<Type> child)
  : uniqueID(genTypeID())
{
  TYPE_CTR_INIT(k);
  components.push_back(comp::make(child));
}

Type::Type(const Kind k, shared_ptr<Type> child1, shared_ptr<Type> child2)
  : uniqueID(genTypeID())
{
  TYPE_CTR_INIT(k);
  components.push_back(comp::make(child1));
  components.push_back(comp::make(child2));
}

// Copy constructor, except distinct uniqueID
Type::Type(shared_ptr<Type>  t)
  : uniqueID(genTypeID())
{
  assert(t);
  kind = t->kind;
  defAst = t->defAst;
  myContainer = t->myContainer;
  link = t->link;    
  arrLen = t->arrLen; // Copies the indirection
  Isize = t->Isize;
  minSignedRep = t->minSignedRep;
  minUnsignedRep = t->minUnsignedRep;

  typeArgs = t->typeArgs;
  fnDeps = t->fnDeps;
    
  for (size_t i=0; i<t->components.size(); i++)
    components.push_back(comp::make(t->CompName(i), t->CompType(i), t->CompFlags(i)));


  mark = MARK_NONE;
  pMark = 0;  
  sp = GC_NULL;
  flags = t->flags;
}

// Makes a deep copy , but ** LINKS TVARS TO ORIGINAL ONES ** 
shared_ptr<Type> 
Type::getDCopy()
{
  shared_ptr<Type> t = getType();
  shared_ptr<TypeScheme> sigma = TypeScheme::make(t, GC_NULL);
  // sigma's ftvs are empty, therefore, TypeSpecialize will link
  // all type-variables to the original ones
  shared_ptr<Type> newTyp = sigma->type_instance();
  newTyp->flags = flags;
  return newTyp;
}

// Returns true of the type `t' is structurally equal to `this'
// under alpha renaming -- modulo:
// i)   mutabality 
// ii)  declarations unify with definitions
// iii) Imprecise integer/floating point types unify with 
//          compatible primitive/prelude typres
// 
// However, it should be noted that this is rarely an issue 
// if the function used once the inference process is OVER.
//
// If one is still not satisfied, he can use the next function
// strictlyEquals which removes the above two restrictions.
bool
Type::eql(shared_ptr<Type> t, bool verbose, std::ostream &errStream,
	  UnifyFlags uflags, bool keepSub,
	  shared_ptr<Trail> trail)
{
  std::stringstream ss;  
  LexLoc internalLocation;
  bool errFree = unify(ss, trail, internalLocation, 
		       shared_from_this(), t, uflags);
  
  if (!keepSub)
    trail->rollBack();

  if (verbose) {
    if (errFree)
      errStream << asString() << " === " << t->asString()
		<< std::endl;
    else
      errStream << asString() << " !== " << t->asString()
		<< " because " << ss.str() 
		<< std::endl;
  }

  return errFree;
}

bool
Type::equals(shared_ptr<Type> t, bool verbose, std::ostream &errStream)
{
  return eql(t, verbose, errStream, UFLG_NO_FLAGS, false);
}

bool 
Type::strictlyEquals(shared_ptr<Type> t, bool verbose,
		     bool noAlphaRename,
		     std::ostream &errStream)
{
  UnifyFlags uflags = UFLG_UNIFY_STRICT;
  if (noAlphaRename)
    uflags |= UFLG_UNIFY_STRICT_TVAR;
  return eql(t, verbose, errStream, uflags, false);
}

bool
Type::unifyWith(shared_ptr<Type> t, bool verbose, 
		shared_ptr<Trail> trail, ostream &errStream)
{
  return eql(t, verbose, errStream, UFLG_NO_FLAGS, true, trail);
}

bool 
Type::forcedUnify(shared_ptr<Type> t, bool verbose, std::ostream &errStream)
{
  return eql(t, verbose, errStream, UFLG_UN_IGN_RIGIDITY, true);
}

bool
Type::equalsA(shared_ptr<Type> t, bool verbose, std::ostream &errStream)
{
  return eql(t, verbose, errStream, UFLG_UN_IGN_RIGIDITY, false);
}

bool 
Type::strictlyEqualsA(shared_ptr<Type> t, bool verbose,
		      std::ostream &errStream)
{
  return eql(t, verbose, errStream, 
	     UFLG_UNIFY_STRICT | UFLG_UN_IGN_RIGIDITY, false);
}

bool 
Type::copy_compatible(shared_ptr<Type> t, bool verbose, std::ostream &errStream)
{
  return MBF(shared_from_this())->equals(MBF(t), verbose, errStream);
}

bool 
Type::copy_compatibleA(shared_ptr<Type> t, bool verbose, std::ostream &errStream)
{
  return MBF(shared_from_this())->equalsA(MBF(t), verbose, errStream);
}

bool
Type::allTvarsRigid()
{
  TypeSet ftvs;
  getType()->collectAllftvs(ftvs);
  for (TypeSet::iterator itr = ftvs.begin(); itr != ftvs.end(); ++itr) {
    if (((*itr)->flags & TY_RIGID) == 0)
      return false;
  }
  return true;
}
