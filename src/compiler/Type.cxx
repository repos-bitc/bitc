/**************************************************************************
" *
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

using namespace sherpa;
using namespace std;

unsigned long long Type::typeCount=0;
GCPtr<Type> Type::Kmono = new Type(ty_kfix);
GCPtr<Type> Type::Kpoly = new Type(ty_kfix);

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


GCPtr<const Type> 
Type::getTypePrim() const
{ 
  GCPtr<const Type> curr = this;
  while(curr->link)
    curr = curr->link;
  
  return curr;
}

GCPtr<Type> 
Type::getTypePrim()
{ 
  GCPtr<Type> curr = this;
  while(curr->link)
    curr = curr->link;
  
  return curr;
}
 

GCPtr<Type> 
Type::getType()
{ 
  GCPtr<Type> t = getTypePrim();
  
  // Deal with the fact that 
  // (mutable (mutable t)) == (mutable t)
  // We cannot avoid this type of linkage because of a structure
  // getting parametrized over a mutable type while having a mutable
  // wrapper at the field level.  
  if(t->kind == ty_mutable) {    
    GCPtr<Type> in = t->Base()->getTypePrim();
    while(in->kind == ty_mutable) {
      t = in;
      in = t->Base()->getTypePrim();
    }
  }
  
  // Maybe types may not be recursively nested, But a MbFull
  // might contain an MbTop.
  if(t->kind == ty_mbFull || t->kind == ty_mbTop) {
    GCPtr<Type> in = t->Var()->getTypePrim();

    assert(in->kind != ty_mbFull);
    
    if(in->kind == ty_mbTop) {
      t = in;
      in = in->Var()->getTypePrim();
    }
    
    assert(in->kind != ty_mbFull && in->kind != ty_mbTop);
    
    if(in->kind != ty_tvar)
      t = in;
  }

  if(t->kind == ty_mbFull || t->kind == ty_mbTop) {
    GCPtr<Type> var = t->Var()->getTypePrim();
    GCPtr<Type> core = t->Core()->getTypePrim();
    
    if (var == core)
      t = core;
  }

  return t;
}

GCPtr<const Type> 
Type::getType() const
{ 
  GCPtr<const Type> t = getTypePrim();
  
  if(t->kind == ty_mutable) {    
    GCPtr<Type> in = t->components->elem(0)->typ->getTypePrim();
    while(in->kind == ty_mutable) {
      t = in;
      in = t->components->elem(0)->typ->getTypePrim();
    }
  }
  
  return t;
}

GCPtr<Type> 
Type::getBareType()
{ 
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK10)
    return t;
  
  t->mark |= MARK10;

  GCPtr<Type> retType = t;

  if(t->isMaybe())
    retType = t->Core()->getBareType();  
  
  if(t->isMutable())
    retType = t->Base()->getBareType();  

  t->mark &= ~MARK10;
  return retType;
}

GCPtr<Type> 
Type::getTheType(bool mutableOK, bool maybeOK)
{ 
  GCPtr<Type> t = getType();  

  if(t->mark & MARK11)
    return t;
  
  t->mark |= MARK11;

  GCPtr<Type> retType = t;
  
  if((t->kind == ty_mutable) && !mutableOK)
    retType = t->Base()->getTheType(mutableOK, maybeOK);
  else if(t->isMaybe() && !maybeOK)
    retType = t->Core()->getTheType(mutableOK, maybeOK);
  
  t->mark &= ~MARK11;
  return retType;
}

bool
Type::isTvar()
{
  GCPtr<Type> t = getBareType();  
  return (t->kind == ty_tvar);
}

bool
Type::isUnifiableTvar(size_t flags)
{
  GCPtr<Type> t = getType();
  if(t->kind != ty_tvar)
    return false;
  
  if(flags & UN_IGN_RIGIDITY)
    return true;
  
  if((t->flags & TY_RIGID) == 0)
    return true;

  return false;
}

bool
Type::isUnifiableMbTop(size_t flags)
{
  GCPtr<Type> t = getType();
  if(t->kind != ty_mbTop)
    return false;
  
  return t->Var()->isUnifiableTvar(flags);
}

bool
Type::isUnifiableMbFull(size_t flags)
{
  GCPtr<Type> t = getType();
  if(t->kind != ty_mbFull)
    return false;
  
  return t->Var()->isUnifiableTvar(flags);
}

bool 
Type::isAtomic()
{
  GCPtr<Type> t = getBareType();  
  return kindInfo[t->kind].isAtomic;
}

bool 
Type::isSimpleTypeForC()
{
  GCPtr<Type> t = getBareType();
  return kindInfo[t->kind].isSimpleTypeForC;
}

bool 
Type::isScalar()
{
  GCPtr<Type> t = getBareType();  
  return kindInfo[t->kind].isScalarType;
}

bool 
Type::isRefType()
{
  GCPtr<Type> t = getBareType();
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
  GCPtr<Type> t = getBareType();
  return (t->kind == ty_byref);
}

bool 
Type::isNullableType()
{
  GCPtr<Type> t = getBareType();
  return (t->kind == ty_unionv &&
	  (t->defAst->Flags & NULLABLE_UN));
}


// Is the current type constrained by (ref-types t) 
// within the constraint set tcc?
bool 
Type::isConstrainedToRefType(sherpa::GCPtr<TCConstraints> tcc)
{
  GCPtr<Type> t = getType();
  
  for(TypeSet::iterator itr = tcc->begin(); itr != tcc->end(); ++itr) {
    GCPtr<Typeclass> pred = (*itr);
    
    const std::string &ref_types = SpecialNames::spNames.sp_ref_types; 
    
    if(pred->defAst->s == ref_types) {
      GCPtr<Type> arg = pred->TypeArg(0)->getType();
      if(strictlyEquals(arg, false, true))
	return true;
    }
  }

  return false;
}


bool 
Type::isFnxn()
{
  GCPtr<Type> t = getBareType();
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
    return true;
  default:
    return false;
  }
}


//#if 0
bool 
Type::isClosure()
{
  GCPtr<Type> t = getBareType();
  return (t->kind == ty_fn);
}
//#endif

bool 
Type::isMutable()
{
  GCPtr<Type> t = getType();
  return t->kind == ty_mutable;
}

bool 
Type::isMaybe()
{
  GCPtr<Type> t = getType();
  return (t->kind == ty_mbTop || t->kind == ty_mbFull);
}
 
bool 
Type::isMbVar()
{
  GCPtr<Type> t = getType();
  TYPE_ACC_DEBUG assert(kind == ty_mbTop || kind == ty_mbFull);
  GCPtr<Type> v = t->Var();
  TYPE_ACC_DEBUG 
    if(t->kind == ty_mbFull)
      assert(v->kind != ty_mbFull);
    else
      assert(v->kind != ty_mbFull && v->kind != ty_mbTop);
  
  return (v->kind == ty_tvar);
}

bool 
Type::isPrimInt()
{
  GCPtr<Type> t = getBareType();
  return kindInfo[t->kind].isPrimInt;
}

bool 
Type::isPrimFloat()
{
  GCPtr<Type> t = getBareType();
  return kindInfo[t->kind].isPrimFloat;
}

bool 
Type::isPrimaryType()
{
  GCPtr<Type> t = getBareType();
  return (kindInfo[t->kind].isPrimary);
}
bool 
Type::isInteger()
{
  GCPtr<Type> t = getBareType();
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
  GCPtr<Type> t = getBareType();
  return (t->kind == ty_bool);
}

bool 
Type::isIntegral()
{
  GCPtr<Type> t = getBareType();
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
  GCPtr<Type> t = getBareType();
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
  GCPtr<Type> t = getBareType();
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
  GCPtr<Type> t = getBareType();
  return (t->kind == ty_typeclass);    
}

bool
Type::isPcst()
{
  GCPtr<Type> t = getBareType();
  return (t->kind == ty_pcst);    
}

// Returns true if this is a union or a structure definition 
// respectively
bool 
Type::isUnion(bool ignMut) 
{
  GCPtr<Type> t = ((ignMut) ? getBareType() : getType());
  return (((t->kind == ty_unionv) || (t->kind == ty_unionr)) &&
	  (t->components->size() != 0));
}

bool 
Type::isUcon(bool ignMut) 
{
  GCPtr<Type> t = ((ignMut) ? getBareType() : getType());
  return ((t->kind == ty_uconv) || (t->kind == ty_uconr));
}

bool 
Type::isUval(bool ignMut) 
{
  GCPtr<Type> t =  ((ignMut) ? getBareType() : getType());
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
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK22)
    return true;
  
  t->mark |= MARK22;
  
  bool mut = false;
  
  switch(t->kind) {
  case ty_mutable:
    mut = true;
    break;
    
  case ty_fn:
    break;

  default:
    for(size_t i=0;!mut &&  i < t->components->size(); i++)
      mut = t->CompType(i)->isDeepMut();
   
    for(size_t i=0; !mut && i < t->typeArgs->size(); i++)
      mut = t->TypeArg(i)->isDeepMut();

    // No need to check functional dependencies
    // May lead to error if checked because functional dependencies
    // might be on types that are used within a function constructor
    break;
  }
  
  t->mark &= ~MARK22;
  return mut;
}
  
bool 
Type::isDeepImmut()
{
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK23)
    return true;
  
  t->mark |= MARK23;
  
  bool immut = true;
  
  switch(t->kind) {
  case ty_mutable:
  case ty_tvar:
    immut = false;
    break;
 
  case ty_fn:
    break;
    
  default:
    for(size_t i=0; immut && i < t->components->size(); i++)
      immut = t->CompType(i)->isDeepImmut();
    
    for(size_t i=0; immut && i < t->typeArgs->size(); i++)
      immut = t->TypeArg(i)->isDeepImmut();

    // No need to check functional dependencies
    // May lead to error if checked because functional dependencies
    // might be on types that are used within a function constructor
    
    break;
  }

  t->mark &= ~MARK23;
  return immut;  
}

bool 
Type::isConcretizable()
{
  GCPtr<Type> t = getType();
  
  if(t->mark & MARK7)
    return true;
  
  t->mark |= MARK7;
  
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
    for(size_t i=0; concretizable && i < t->components->size(); i++)
      concretizable = t->CompType(i)->isConcretizable();
    
    for(size_t i=0; concretizable && i < t->typeArgs->size(); i++)
      concretizable = t->TypeArg(i)->isConcretizable();
    
    // No need to check functional dependencies
    // May lead to error if checked because functional dependencies
    // might be on types that are used within a function constructor
    break;
  }
  
  t->mark &= ~MARK7;
  return concretizable;  
}

/* Produce Type ty_union[rv] from ty_ucon[rv] or ty_uval[rv]
   ONLY typeArgs are populated */
GCPtr<Type> 
Type::getUnionType()
{
  GCPtr<Type> uType = getType();
  assert(uType->isUType());
  GCPtr<Type> uCopy = uType->getType()->getDCopy();
  GCPtr<Type> t = uCopy->getBareType();
  t->kind = (uType->isRefType() ? ty_unionr : ty_unionv);
  t->defAst = t->myContainer;
  t->components->erase();
  return uCopy;
}

bool 
Type::isException() 
{
  GCPtr<Type> t = getBareType();
  return (t->kind == ty_exn);
}

bool 
Type::isStruct()
{
  GCPtr<Type> t = getBareType();

  // WAS  return (((t->kind == ty_structv) || 
  //               (t->kind == ty_structr)) &&
  //           	  (t->components->size() != 0));
  return ((t->kind == ty_structv) || 
	  (t->kind == ty_structr));
}

bool 
Type::isDecl()
{
  GCPtr<Type> t = getBareType();
  switch(t->kind) {
  case ty_unionv:
  case ty_unionr:
  case ty_structv:
  case ty_structr:
    return (t->components->size() == 0);

  default:
    return false;
  }  
}

bool
Type::isOfInfiniteType()
{
  bool infType = false;

  if(getType() != this)
    return getType()->isOfInfiniteType();

  if(mark & MARK6) 
    return true;

  mark |= MARK6;

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
    {
      return Core()->isOfInfiniteType();
      break;
    }

  case ty_letGather:
  case ty_fn:
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
  case ty_reprr:
  case ty_reprv:
  case ty_typeclass:
  case ty_array:
  case ty_vector:
  case ty_ref:
  case ty_byref:
  case ty_mutable:
  case ty_exn:
  case ty_subtype:
  case ty_pcst:
    {      
      for(size_t i=0; !infType && (i < typeArgs->size()); i++)
      	if(TypeArg(i)->isOfInfiniteType())
      	  infType = true;

      break;
    }
  }
  
 mark &= ~MARK6;
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
  GCPtr<Type> t = getType();
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
  GCPtr< CVector<GCPtr<Type> > > tvs = new CVector<GCPtr<Type> >;
  collectAllftvs(tvs);
  return (tvs->size() == 0);
}

void 
Type::SetTvarsTo(GCPtr<Type> t)
{
  GCPtr< CVector<GCPtr<Type> > > tvs = new CVector<GCPtr<Type> >;
  collectAllftvs(tvs);
  
  for(size_t i=0; i < tvs->size(); i++)
    (*tvs)[i]->getType()->link = t;
}

void 
Type::SetTvarsToUnit()
{
  GCPtr< CVector<GCPtr<Type> > > tvs = new CVector<GCPtr<Type> >;
  collectAllftvs(tvs);
  
  for(size_t i=0; i < tvs->size(); i++) {
    GCPtr<Type> ftv = (*tvs)[i]->getType();
    GCPtr<Type> unit = new Type(ty_unit);
    ftv->link = unit;
  }
}
  
comp::comp(GCPtr<Type> t, unsigned long _flags) 
{
  name = "";
  typ = (GCPtr<Type> )t;
  flags=_flags;
}
  
comp::comp(const std::string s, GCPtr<Type> t, unsigned long _flags) 
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
    defAst = GCPtr<AST>(0);			\
    arrlen = new ArrLen(0);			\
    Isize = 0;					\
    minSignedRep = 0;				\
    minUnsignedRep = 0;				\
    mark = 0;					\
    pMark = 0;					\
    sp = NULL;					\
    myContainer = NULL;				\
    link = 0;					\
    flags = 0;					\
    components = new CVector<GCPtr<comp> >;     \
    typeArgs = new CVector<GCPtr<Type> >;       \
  } while(0);


Type::Type(const Kind k)
  : uniqueID(genTypeID())
{
  TYPE_CTR_INIT(k);
}

Type::Type(const Kind k, GCPtr<Type> child)
  : uniqueID(genTypeID())
{
  TYPE_CTR_INIT(k);
  components->append(new comp(child));
}

Type::Type(const Kind k, GCPtr<Type> child1, GCPtr<Type> child2)
  : uniqueID(genTypeID())
{
  TYPE_CTR_INIT(k);
  components->append(new comp(child1));
  components->append(new comp(child2));
}

// Copy constructor, except distinct uniqueID
Type::Type(GCPtr<Type>  t)
  : uniqueID(genTypeID())
{
  assert(t);
  kind = t->kind;
  defAst = t->defAst;
  myContainer = t->myContainer;
  link = t->link;    
  arrlen = t->arrlen; // Copies the indirection
  Isize = t->Isize;
  minSignedRep = t->minSignedRep;
  minUnsignedRep = t->minUnsignedRep;

  components = new CVector<GCPtr<comp> >;
  typeArgs = new CVector<GCPtr<Type> >;

  for(size_t i=0; i<t->typeArgs->size(); i++)
    typeArgs->append(t->TypeArg(i));
  
    
  for(size_t i=0; i<t->components->size(); i++)
    components->append(new comp(t->CompName(i), t->CompType(i), t->CompFlags(i)));

  fnDeps = t->fnDeps;

  mark = 0;
  pMark = 0;  
  sp = NULL;
  flags = t->flags;
}

// Makes a deep copy , but ** LINKS TVARS TO ORIGINAL ONES ** 
GCPtr<Type> 
Type::getDCopy()
{
  GCPtr<Type> t = getType();
  GCPtr<TypeScheme> sigma = new TypeScheme(t, NULL);
  // sigma's ftvs are empty, therefore, TypeSpecialize will link
  // all type-variables to the original ones
  GCPtr<Type> newTyp = sigma->type_instance_copy();
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
Type::eql(GCPtr<Type> t, bool verbose, std::ostream &errStream,
	  unsigned long uflags, bool keepSub,
	  GCPtr<Trail> trail)
{
  std::stringstream ss;  
  LexLoc internalLocation;
  bool errFree = unify(ss, trail, internalLocation, this, t, uflags);
  
  if(!keepSub)
    trail->rollBack();

  if(verbose) {
    if(errFree)
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
Type::equals(GCPtr<Type> t, bool verbose, std::ostream &errStream)
{
  return eql(t, verbose, errStream, UNIFY_TRY, false);
}

bool 
Type::strictlyEquals(GCPtr<Type> t, bool verbose,
		     bool noAlphaRename,
		     std::ostream &errStream)
{
  unsigned long uflags = UNIFY_TRY | UNIFY_STRICT;
  if(noAlphaRename)
    uflags |= UNIFY_STRICT_TVAR;
  return eql(t, verbose, errStream, uflags, false);
}

bool
Type::unifyWith(GCPtr<Type> t, bool verbose, 
		GCPtr<Trail> trail, ostream &errStream)
{
  return eql(t, verbose, errStream, 0, true, trail);
}

bool 
Type::forcedUnify(GCPtr<Type> t, bool verbose, std::ostream &errStream)
{
  return eql(t, verbose, errStream, 
	     UN_IGN_RIGIDITY, true);
}

bool
Type::equalsA(GCPtr<Type> t, bool verbose, std::ostream &errStream)
{
  return eql(t, verbose, errStream, 
	     UNIFY_TRY | UN_IGN_RIGIDITY, false);
}

bool 
Type::strictlyEqualsA(GCPtr<Type> t, bool verbose,
		      std::ostream &errStream)
{
  return eql(t, verbose, errStream, 
	     UNIFY_TRY | UNIFY_STRICT | UN_IGN_RIGIDITY, false);
}

bool
Type::allTvarsRigid()
{
  GCPtr< CVector<GCPtr<Type> > > ftvs = new CVector<GCPtr<Type> >;
  getType()->collectAllftvs(ftvs);
  for(size_t i=0; i < ftvs->size(); i++) 
    if((ftvs->elem(i)->flags & TY_RIGID) == 0)
      return false;
  return true;
}
