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
#include <sstream>
#include <string>
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
#include "WorkList.hxx"
#include "Unify.hxx"

using namespace boost;
using namespace sherpa;

static bool
typeError(std::ostream& errStream, const LexLoc &errLoc,
	  shared_ptr<Type> t1, shared_ptr<Type> t2)
{
  errStream << errLoc << ": Type Error."
	    << "Expected " << t1->asString(GC_NULL) 
	    << ", Obtained " << t2->asString(GC_NULL)
	    << std::endl;
  
    if (errStream == std::cerr)
      errStream << "Real Error!" << std::endl;
  
  // MUST always return false.
  return false;
}

// Get an instance of a primary type defined in the Prelude.
bool
unifyPrim(std::ostream& errStream,
	  shared_ptr<Trail> trail, const LexLoc &errLoc,
	  shared_ptr<Type> tau, const std::string ptype) 
{
  bool errFree = true;

  shared_ptr<Type> primType = Type::make(Type::LookupKind(ptype));
  CHKERR(errFree, unify(errStream, trail, errLoc, primType, tau, UFLG_NO_FLAGS));
  return errFree;
}

static bool
Unify(std::ostream& errStream,
      shared_ptr<Trail> trail, 
      const LexLoc &errLoc,
      shared_ptr<Type> ft, shared_ptr<Type> st, 
      UnifyFlags uflags);

// Handle unification of struct/union decl+decl or decl+def
static bool 
UnifyDecl(std::ostream& errStream,
	  shared_ptr<Trail> trail,
	  const LexLoc &errLoc,
	  shared_ptr<Type> t1, shared_ptr<Type> t2,
	  UnifyFlags uflags) 
{
  bool errFree = true;

  UNIFY_DEBUG std::cout << "UnifyDecl " 
			<< t1->asString(Options::debugTvP) 
			<< " ==? " 
			<< t2->asString(Options::debugTvP)
			<< std::endl;

  assert(t1->defAst == t2->defAst);
  assert(t1->kind != ty_uconv || t1->kind != ty_uconr);
  assert(t1->kind != ty_uvalv || t1->kind != ty_uvalr);

  if (t1->typeArgs.size() != t2->typeArgs.size())
    return typeError(errStream, errLoc, t1, t2);
    
  for (size_t i=0; i<t1->typeArgs.size(); i++)
    CHKERR(errFree, Unify(errStream, trail, errLoc, t1->TypeArg(i), 
			  t2->TypeArg(i), uflags)); 
    
  return errFree;
}
    

static bool 
UnifyStructUnion(std::ostream& errStream,
		 shared_ptr<Trail> trail,
		 const LexLoc &errLoc,
		 shared_ptr<Type> t1, shared_ptr<Type> t2,
		 UnifyFlags uflags) 
{
  bool errFree = true;

  UNIFY_DEBUG std::cout << "UnifyStructUnion " 
			<< t1->asString(Options::debugTvP) 
			<< " ==? "
			<< t2->asString(Options::debugTvP)
			<< std::endl;

  if (t1->isULeg() || t2->isULeg()) {
    if (t1->myContainer != t2->myContainer)
      return typeError(errStream, errLoc, t1, t2);
  }
  else {
    if (t1->defAst != t2->defAst)
      return typeError(errStream, errLoc, t1, t2);

    if (t1->components.size() == 0 || t2->components.size() == 0) 
      return UnifyDecl(errStream, trail, errLoc, t1, t2, uflags);
  }
  
  size_t n = trail->snapshot();
  trail->link(t1, t2);  
  
  assert(t1->typeArgs.size() == t2->typeArgs.size());  
  for (size_t i=0; i<t1->typeArgs.size(); i++) 
    CHKERR(errFree, Unify(errStream, trail, errLoc, t1->TypeArg(i), 
			  t2->TypeArg(i), uflags));
  
  if (!errFree)
    trail->rollBack(n);
  
  return errFree;
}

static bool 
UnifyMbCt(std::ostream& errStream, shared_ptr<Trail> trail,
	  shared_ptr<Type> mb, shared_ptr<Type> ct)
{
  mb = mb->getType();
  ct = ct->getType();
  shared_ptr<Type> var = mb->Var()->getType();
  shared_ptr<Type> core = mb->Core()->getType();
  
  trail->subst(var, ct);
  
  return true;
}

// While unifying two fnArgs, it is better to report the entire
// function type rather than the argument types (if we know the 
// full function type).
// Therefore, this function is called from the Unifier
// from both ty_fn case (in which case the errt1 and errt2
// types are the full function types) and the
// ty_fnarg case )in thich case errt1 = t1 and errt2=t2)
static bool 
UnifyFnArgs(std::ostream& errStream, shared_ptr<Trail> trail,
	    const LexLoc &errLoc,
 	    shared_ptr<Type> errt1, shared_ptr<Type> errt2,
	    shared_ptr<Type> t1, shared_ptr<Type> t2,
	    UnifyFlags uflags)
{
  bool errFree = true;
  t1 = t1->getType();
  t2 = t2->getType();
  assert(t1->kind == ty_fnarg);
  assert(t2->kind == ty_fnarg);
  
  if (t1->components.size() != t2->components.size())
    return typeError(errStream, errLoc, errt1, errt2);
  
  for (size_t i=0; i< t1->components.size(); i++) {
    
    if ((t1->CompFlags(i) & COMP_BYREF_P) &&
	((t2->CompFlags(i) & COMP_BYREF_P) == 0)) {
      t1->CompFlags(i) &= ~COMP_BYREF_P;
      t1->CompFlags(i) |= t2->CompFlags(i) & COMP_BYREF;
    }
    else if ((t2->CompFlags(i) & COMP_BYREF_P) &&
	     ((t1->CompFlags(i) & COMP_BYREF_P) == 0)) {
      t2->CompFlags(i) &= ~COMP_BYREF_P;
      t2->CompFlags(i) |= t1->CompFlags(i) & COMP_BYREF;
    }
    else if (t1->CompFlags(i) != t2->CompFlags(i)) {
      errFree = typeError(errStream, errLoc, errt1, errt2);
      break;
    }
    
    CHKERR(errFree, Unify(errStream, trail, errLoc,
			  t1->CompType(i), 
			  t2->CompType(i), uflags));
  }

  return errFree;
}

// Wrapper over the propagateMutability member function,
// which includes printing error message.
static bool
PropagateMutability(std::ostream& errStream,
		    shared_ptr<Trail> trail, 
		    const LexLoc &errLoc,
		    shared_ptr<Type> t) 
{
  bool errFree = true;
  CHKERR(errFree, t->propagateMutability(trail));
  if(!errFree)
    errStream << errLoc << ": Unsound Mutable type: "
	      << t->asString()
	      << std::endl;
  // else
  //     errStream << errLoc << ": Mutability propagation okay for: "
  // 	      << t->asString()
  // 	      << std::endl;
  return errFree;
}

static bool
Unify(std::ostream& errStream,
      shared_ptr<Trail> trail, 
      const LexLoc &errLoc,
      shared_ptr<Type> ft, shared_ptr<Type> st, 
      UnifyFlags uflags) 
{
  shared_ptr<Type> t1 = ft->getType();
  shared_ptr<Type> t2 = st->getType();
  bool errFree = true;

  UNIFY_DEBUG std::cerr << "Unifier: " 
			<< ft->asString(Options::debugTvP)
			<< " ==? " 
			<< st->asString(Options::debugTvP)
			<< std::endl;  
  
  if (t1->uniqueID == t2->uniqueID)
    return true;

  bool unifingSameKind = (t1->kind == t2->kind);

  switch(unifingSameKind) {
  case false:
    if (t1->isUType(false) && t2->isUType(false) && 
	t1->isRefType() == t2->isRefType()) {
      CHKERR(errFree, UnifyStructUnion(errStream, trail, errLoc, 
				       t1, t2, uflags));
      break;
    }
    
    if (uflags & UFLG_UNIFY_STRICT) {
      errFree = typeError(errStream, errLoc, t1, t2);
      break;
    }
      
    /* 1. Handle the case of Type Variables unifying with 
          another type
       2. If no such unification is possible, handle the case of
          a mbFull unifying with a mbTop or other non-maybe type
       3. If no such unification is possible, handle the case of 
          a mbTop unifying with a non-maybe type.      */
      
    if(t1->isTvar() || t2->isTvar()) {
      shared_ptr<Type> var = t1->isTvar() ? t1 : t2;
      shared_ptr<Type> other = t1->isTvar() ? t2 : t1;

      // The current unification case is 'a = t. 
      // If 'a is a rigid variable, we  ust immediately return an
      // error, since unification through other cases will be less
      // precise (that is, if t is a maybe type, we will get a type
      // whole variability wrt mutability is lost.


      if(!var->isUnifiableVar()) {
	errStream << errLoc << " Rigid variable "
		  << var->asString() << " cannot be unified with "
		  << other->asString() << std::endl;
	errFree = false;
	break;
      }

      // Now, we check to make sure that substitution of t for 'a
      // does not lead to cyclic substitution.
      // If 'a does not occur in t, we can substitute t for 'a and
      // declare victory. 
      // 
      // However, if 'a is bound in t, we cannot immediately declare
      // an error, but must try other options. For example, this case
      // arises in the solver and generalizer where a type is unified
      // with its (deeply) most immutable version.
      // One of the possibilities we can end up here is 'a = 'b|'a,
      // where the intension really is to ensure that the mutability
      // should be fixed to the immutable version.

      if(!other->boundInType(var)) {
	trail->subst(var, other);
	break;
      }
      // Otherwise, try other options ...
    }

    if (t1->isMbFull() || t2->isMbFull()) {
      shared_ptr<Type> mb = t1->isMbFull() ? t1 : t2;
      shared_ptr<Type> other = t1->isMbFull() ? t2 : t1;
      
      if(!mb->isUnifiableVar()) {
	errStream << errLoc << "Rigid type "
		  << mb->asString() << " cannot be unified with "
		  << other->asString() << std::endl;
	errFree = false;
	break;
      }

      // Handle the special case: U(s|'b == M'a):
      // Under the normal unification rules, this will result in 
      // the type s|'b = M'a = M'a|'a, 
      // but what we want is s|'b = M'a = M'c|'b
      // This kind of unification constraint can only arise out of
      // explicit qualification, and must therefore be handled
      // specially (theory does not mention it). We cannot interpret
      // all M'a types as M'a|'b, since such qualifications might be
      // within composite type definitions.

      if(t2->isMutable() && t2->Base()->isTvar()) {
	CHKERR(errFree, Unify(errStream, trail, errLoc, mb->Var(),
			      Type::make(ty_mutable, newTvar()), uflags));
	
	trail->link(other, mb);
      }
      else {
	CHKERR(errFree, Unify(errStream, trail, errLoc, 
			      mb->minimizeMutability(), 
			      other->minimizeMutability(), uflags));
	
	CHKERR(errFree, Unify(errStream, trail, errLoc, 
			      mb->Var(), other, uflags));
      }
      
      
      if(errFree && (other->isMutable() || mb->Var()->isMutable()))
	CHKERR(errFree, PropagateMutability(errStream, trail, 
					    errLoc, mb));
      
      break;
    }

    if (t1->isMbTop() || t2->isMbTop()) {
      shared_ptr<Type> mb = t1->isMbTop() ? t1 : t2;
      shared_ptr<Type> other = t1->isMbTop() ? t2 : t1;
      
      if(!mb->isUnifiableVar()) {
	errStream << errLoc << "Rigid type "
		  << mb->asString() << " cannot be unified with "
		  << other->asString() << std::endl;
	errFree = false;
	break;
      }
      
      CHKERR(errFree, Unify(errStream, trail, errLoc, 
			    mb->minimizeTopMutability(), 
			    other->minimizeTopMutability(), uflags));
      
      CHKERR(errFree, Unify(errStream, trail, errLoc, 
			    mb->Var(), other, uflags));
      break;
    }

    /* Certain types have multiple equivalent representations. 
       For example,
        1) (array (mutable int32)) == (mutable (array (mutable int32)))
	2) An unboxed structure is mutable as a whole if all of its
           components are mutable.  
       
       Therefore, (array T) must potentially unify with 
       (mutable T) and (mutable 'a)|t.  The following rule
       checks to see if such a match is possible */
    
    if(t1->isMutType() || t2->isMutType()) {
      shared_ptr<Type> mut = t1->isMutType() ? t1 : t2;
      shared_ptr<Type> other = t1->isMutType() ? t2 : t1;
      
      assert(!other->isMutType() &&  // Otherwise, it would be 
	     !other->isMaybe());     // handled already.  
      
      if(other->kind == ty_array || other->kind == ty_structv) {
	CHKERR(errFree, Unify(errStream, trail, errLoc, 
			      mut, Mutable(other), uflags));
	break;
      }
    }

    /* Const equivalent representation handling
       For example: (const (bool, bool)) == (bool, bool) */

    if(t1->isConst() || t2->isConst()) {
      shared_ptr<Type> constType = t1->isConst() ? t1 : t2;
      shared_ptr<Type> other = t1->isConst() ? t2 : t1;

      if(other->isEffectivelyConst()) {
	CHKERR(errFree,
	       Unify(errStream, trail, errLoc, 
		     t1->Base()->minimizeMutability(), 
		     t2->Base()->minimizeMutability(), uflags));
	break;
      }
    }
    
    errFree = typeError(errStream, errLoc, t1, t2);
    break;
    
  case true:
    switch(t1->kind) {
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
      break;

    case ty_tvar:
      {		
	if (uflags & UFLG_UNIFY_STRICT_TVAR) {
	  errFree = typeError(errStream, errLoc, t1, t2);
	  break;
	}

	if ((t1->flags & TY_RIGID) && (t2->flags & TY_RIGID) &&
	    ((uflags & UFLG_UN_IGN_RIGIDITY) == 0)) {
	  errFree = typeError(errStream, errLoc, t1, t2);
	  break;
	}

	// One of the types is not rigid, or we are ignoring rigidity.
	if (t1->flags & TY_RIGID)
	  trail->subst(t2, t1);
	else
	  trail->subst(t1, t2);
	
	break;
      }

    case ty_dummy:
      {
	break;
      }
    
#ifdef KEEP_BF
    case ty_bitfield:
      {	
	CHKERR(errFree, Unify(errStream, trail, errLoc,
			      t1->CompType(0), 
			      t2->CompType(0), uflags));
	
	if (!errFree)
	  break;
	
	if (t1->Isize == t2->Isize)
	  break;

	errStream << errLoc << ": "
		  << "Incompatibility in integer types "
		  << t1->asString() << " and " << t2->asString() 
		  << "." << std::endl;

	errFree = false;
	break;
      } 
#endif     

    case ty_tyfn:
    case ty_fn:
      {
	shared_ptr<Type> t1Args = t1->Args();
	shared_ptr<Type> t2Args = t2->Args();
	CHKERR(errFree, UnifyFnArgs(errStream, trail, errLoc, 
				    t1, t2, t1Args, t2Args, uflags));
      
	CHKERR(errFree, 
	       Unify(errStream, trail, errLoc, t1->Ret(), 
		     t2->Ret(), uflags));
	break;
      }

    case ty_fnarg:
      {
	CHKERR(errFree, UnifyFnArgs(errStream, trail, errLoc, 
				    t1, t2, t1, t2, uflags));
	break;
      }

    case ty_structv:
    case ty_structr:
    case ty_unionv:
    case ty_unionr:
    case ty_uconr:
    case ty_uconv:
    case ty_uvalr:
    case ty_uvalv:
      {
	CHKERR(errFree,
	       UnifyStructUnion(errStream, trail, errLoc, t1, t2, uflags));
	break;
      }

    case ty_letGather:
      {      
	if (t1->components.size() != t2->components.size()) {
	  errFree = typeError(errStream, errLoc, t1, t2);
	  break;
	}

	for (size_t i=0; i<t1->components.size(); i++) 
	  CHKERR(errFree, 
		 Unify(errStream, trail, errLoc, t1->CompType(i), 
		       t2->CompType(i), uflags));
	
	break;
      }
      
    case ty_array:
      {
	CHKERR(errFree, 
	       Unify(errStream, trail, errLoc, t1->Base(), 
		     t2->Base(), uflags));

	if (t1->arrLen->len == t2->arrLen->len)
	  break;
      
	// Array lengths did not Unify
	if (t1->arrLen->len == 0) {
	  t1->arrLen->len = t2->arrLen->len;
	  break;
	}
	else if (t2->arrLen->len == 0) {
	  t2->arrLen->len = t1->arrLen->len;
	  break;
	}
	else {
	  errStream << errLoc 
		    << ": Array lengths do not match. "
		    << t1->arrLen->len
		    << " vs " 
		    << t2->arrLen->len
		    << std::endl;
	  errFree = false;
	}
      
	break;
      }
      
    case ty_vector:
      {
	CHKERR(errFree, 
	       Unify(errStream, trail, errLoc, t1->Base(), 
		     t2->Base(), uflags));
	break;
      }
    
    case ty_mbTop:
      {
	CHKERR(errFree,
	       Unify(errStream, trail, errLoc, 
		     t1->Core()->minimizeTopMutability(), 
		     t2->Core()->minimizeTopMutability(), 
		     uflags));
      
	if (!errFree)
	  break;
      
	CHKERR(errFree, Unify(errStream, trail, errLoc, 
			      t1->Var(), t2->Var(), uflags));
	break;
      }
    
    case ty_mbFull:
      {
	CHKERR(errFree,
	       Unify(errStream, trail, errLoc, 
		     t1->Core()->minimizeMutability(), 
		     t2->Core()->minimizeMutability(), 
		     uflags));
      
	if (!errFree)
	  break;
      
	shared_ptr<Type> var1 = t1->Var()->getType();
	shared_ptr<Type> var2 = t2->Var()->getType();
      
	if(var1->isMutable() && var2->isMutable())
	  CHKERR(errFree, Unify(errStream, trail, errLoc, 
				var1->Base(), var2->Base(), uflags));
	else
	  CHKERR(errFree, Unify(errStream, trail, errLoc, 
				var1, var2, uflags));
      
	if(errFree && t1->Var()->isMutable())
	  CHKERR(errFree, PropagateMutability(errStream, trail, 
					      errLoc, t1));
      
	break;
      }

    case ty_mutable:
      {
	CHKERR(errFree,
	       Unify(errStream, trail, errLoc, t1->Base(), 
		     t2->Base(), uflags));

	if(!errFree)
	  break;
      
	CHKERR(errFree, PropagateMutability(errStream, trail, 
					    errLoc, t1));
	break;
      }

    case ty_const:
      {
	CHKERR(errFree,
	       Unify(errStream, trail, errLoc, 
		     t1->Base()->minimizeMutability(), 
		     t2->Base()->minimizeMutability(), uflags));
	break;
      }
    
    case ty_ref:
    case ty_byref:
      {
	CHKERR(errFree,
	       Unify(errStream, trail, errLoc, t1->Base(), 
		     t2->Base(), uflags));
	break;
      }

    case ty_exn:
      {
	// All exceptions belong to the same sum type.	
	break;
      }

    case ty_typeclass:
      {
	if (t1->defAst != t2->defAst) {
	  errFree = typeError(errStream, errLoc, t1, t2);
	  break;
	}
	
	if (t1->typeArgs.size() != t2->typeArgs.size()) {
	  errFree = typeError(errStream, errLoc, t1, t2);
	  break;
	}
	
	for (size_t i = 0; i < t1->typeArgs.size(); i++) {
	  
	  CHKERR(errFree, Unify(errStream, trail, errLoc, 
				t1->TypeArg(i), t2->TypeArg(i),
				uflags));
	}
	
	break;
      }
      
      // The following cases are filled in so that strictlyEquals()
      // function works correctly.
    case ty_pcst:
      {
	assert(t1->components.size() == t2->components.size());
	for (size_t i=0; i < t1->components.size(); i++) 
	  CHKERR(errFree,
		 Unify(errStream, trail, errLoc, t1->CompType(i), 
		       t2->CompType(i), uflags));
	break;
      }
      
    case ty_kvar:
    case ty_kfix:
      {
	// This check will never unify, since the following check has
	// already failed at the start of the unification algorithm.
	if (t1->uniqueID != t2->uniqueID)
	  errFree = typeError(errStream, errLoc, t1, t2);
	break;
      }
    }
  }
  
  UNF_RES_DEBUG errStream << "\t Result: " 
			  << ft->asString(Options::debugTvP)
			  << " == " 
			  << st->asString(Options::debugTvP)
			  << "{" << (errFree?"OK":"ERR") << "}"
			  << std::endl;  
  
  return errFree;
}

bool
acyclic(std::ostream& errStream,
	const LexLoc &errLoc,
	shared_ptr<Type> typ, 
	WorkList<shared_ptr<Type> >& worklist, // Types currently visiting
 	DoneList<shared_ptr<Type> >& donelist, // Types Known to be OK 
	bool inref=false)
{
  assert(typ);

  shared_ptr<Type> t = typ->getType();
  bool errFree = true;

  //std::cout << "Acyclic: Processing: " << t->asString(Options::debugTvP)
  //	    << std::endl;
  
  if (donelist.contains(t))
    return true;
  
  if (worklist.contains(t)) {
    
    if (t->kind == ty_structr ||
       t->kind == ty_unionr || 
       t->kind == ty_uconr ||
       t->kind == ty_uvalr)
      return true;
    
    if (inref)
      return true;
    
    std::cerr << errLoc << ": " 
	      << "Cyclic Type Definitions among the following " 
	      << worklist.size() << " types:" << std::endl;
    
    // By now, we know that the current type is already 
    // in the worklist
    std::cerr << t->asString(GC_NULL, false) 
    	      << std::endl;
    for (WorkList<shared_ptr<Type> >::iterator itr = worklist.begin();
	itr != worklist.end(); ++itr)
      std::cerr << (*itr)->asString(GC_NULL, false)
		<< std::endl;
    fatal();
    return false;
  }
	
  assert(!worklist.contains(t));
  worklist.insert(t);

  for (size_t i=0; i < t->components.size(); i++)
    CHKERR(errFree, acyclic(errStream, errLoc, t->CompType(i),
			    worklist, donelist,
			    (inref || t->kind == ty_ref)));

//   for (size_t i=0; i < t->typeArgs.size(); i++) {
//     CHKERR(errFree, acyclic(errStream, errLoc, t->TypeArg(i),  
// 			    worklist, donelist,
// 			    (inref || t->kind == ty_ref)));
//   }
  
  worklist.erase(t);   
  if (errFree)
    donelist.insert(t);
  
  //std::cout << "--"
  //          << std::endl;
  
  return errFree;		
}

bool
unify(std::ostream& errStream,
      shared_ptr<Trail> trail,
      const LexLoc &errLoc,
      shared_ptr<Type> ft, shared_ptr<Type> st, 
      UnifyFlags uflags) 
{
  bool errFree = true;

  assert((uflags & UFLG_UN_MBFULL_VAR) == 0);
  CHKERR(errFree, Unify(errStream, trail, errLoc, ft, st, uflags));
  
  WorkList<shared_ptr<Type> > worklist;
  DoneList<shared_ptr<Type> > donelist;
  CHKERR(errFree, acyclic(errStream, errLoc, ft, worklist, donelist));
  if(errFree)
    ft->normalize(trail);
  
  return errFree;
}

