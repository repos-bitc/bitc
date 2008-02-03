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
#include <sstream>
#include <string>
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
#include "WorkList.hxx"
#include "DoneList.hxx"

using namespace sherpa;

//#define VERBOSE_UNIFY

static bool
typeError(std::ostream& errStream,
	  GCPtr<const AST> errAst, GCPtr<Type> t1, GCPtr<Type> t2)
{
  errStream << errAst->loc << ": Type Error."
	    << "Expected " << t1->asString(NULL) 
	    << ", Obtained " << t2->asString(NULL)
	    << std::endl;

  if(t1->isUType()) {
    errStream << "##" << std::endl;
  }

  // MUST always return false.
  return false;
}


static bool Unify(std::ostream& errStream,
		  GCPtr<Trail> trail,
		  GCPtr<const AST> errAst, GCPtr<Type> ft, 
		  GCPtr<Type> st, unsigned long flags);

// Get an instance of a primary type defined in the Prelude.
bool
unifyPrim(std::ostream& errStream,
	  GCPtr<Trail> trail,
	  GCPtr<AST> errAst, GCPtr<Type> tau, 
	  const std::string ptype) 
{
  bool errFree = true;

  GCPtr<Type> primType = new Type(Type::LookupKind(ptype), errAst);
  CHKERR(errFree, unify(errStream, trail, errAst, primType, tau, 0));
  return errFree;
}


// Handle unification of struct/union decl+decl or decl+def
static bool 
UnifyDecl(std::ostream& errStream,
	  GCPtr<Trail> trail,
	  GCPtr<const AST> errAst,
	  GCPtr<Type> t1, GCPtr<Type> t2,
	  unsigned long flags) 
{
  bool errFree = true;

#ifdef VERBOSE_UNIFY
  std::cout << "UnifyDecl " 
  	    << t1->asString(NULL) << " and "
  	    << t2->asString(NULL)
  	    << std::endl;
#endif

  assert(t1->defAst == t2->defAst);
  assert(t1->kind != ty_uconv || t1->kind != ty_uconr);
  assert(t1->kind != ty_uvalv || t1->kind != ty_uvalr);

  if(t1->typeArgs->size() != t2->typeArgs->size())
    return typeError(errStream, errAst, t1, t2);
    
  for(size_t i=0; i<t1->typeArgs->size(); i++)
    CHKERR(errFree, Unify(errStream, trail, errAst, t1->TypeArg(i), 
			  t2->TypeArg(i), flags)); 
    
  return errFree;
}
    

static bool 
UnifyStruct(std::ostream& errStream,
	    GCPtr<Trail> trail,
	    GCPtr<const AST> errAst,
	    GCPtr<Type> t1, GCPtr<Type> t2,
	    unsigned long flags) 
{
  bool errFree = true;

#ifdef VERBOSE_UNIFY
  std::cout << "UnifyStruct " 
  	    << t1->asString() << " and "
  	    << t2->asString()
  	    << std::endl;    
#endif

  if(t1->defAst != t2->defAst)
    return typeError(errStream, errAst, t1, t2);
  
  if (t1->components->size() == 0 || t2->components->size() == 0) 
    return UnifyDecl(errStream, trail, errAst, t1, t2, flags);

  GCPtr<Type> bt = t1->defAst->scheme->type_instance_copy();

  if(bt->components->size() != t1->components->size()) {
    errStream << errAst->loc
	      << ": Incorrect Number of Fields for structure "
	      << bt->asString()
	      << " Expected " << bt->components->size()
	      << " Obtained " << t1->components->size() << "."
	      << std::endl;
    errFree = false;
    return false;
  }
   
  if(bt->components->size() != t2->components->size()) {
    errStream << errAst->loc
	      << ": Incorrect Number of Fields for structure "
	      << bt->asString()
	      << " Expected " << bt->components->size()
	      << " Obtained " << t2->components->size() << "."
	      << std::endl;

    errFree = false;
    return false;
  }

  size_t n = trail->snapshot();
  trail->link(t1, t2);

  for(size_t i=0; i<bt->components->size(); i++) 
    CHKERR(errFree, Unify(errStream, trail, errAst, 
			  t1->CompType(i), 
			  t2->CompType(i), flags));
  

  if(!errFree) {
    trail->rollBack(n);
    return false;
  }

  trail->link(bt, t1);  
  // Just to be really sure:
  for(size_t i=0; i<bt->components->size(); i++) 
    CHKERR(errFree, Unify(errStream, trail, errAst, 
			  bt->CompType(i), 
			  t1->CompType(i), 
			  (flags & ~UNIFY_STRICT)));
  
  if(!errFree) {
    trail->rollBack(n);
    return false;
  }
  
  assert(t1->typeArgs->size() == t2->typeArgs->size());
  
  // This is redundant; someday I should delete it.
  // Careful, it is NOT redundant in unions, don't 
  // delete it there out of symetry.
  for(size_t i=0; i<t1->typeArgs->size(); i++) 
    CHKERR(errFree, Unify(errStream, trail, errAst, t1->TypeArg(i), 
			  t2->TypeArg(i), flags));

  if(!errFree)
    trail->rollBack(n);

  return errFree;
}

static bool 
UnifyUnion(std::ostream& errStream,
	    GCPtr<Trail> trail,
	    GCPtr<const AST> errAst,
	    GCPtr<Type> t1, GCPtr<Type> t2,
	    unsigned long flags)
{
  bool errFree = true;

#ifdef VERBOSE_UNIFY
  std::cout << "UnifyUnion" 
  	    << t1->asString(NULL)
  	    << t2->asString(NULL)
  	    << std::endl;
#endif
  
  // Note: Following check also serves to prevent unification of reprs with 
  // unions.
  if(t1->defAst != t2->defAst)
    return typeError(errStream, errAst, t1, t2);
  
  if (t1->components->size() == 0 || t2->components->size() == 0) 
    return UnifyDecl(errStream, trail, errAst, t1, t2, flags);  
  
  GCPtr<Type> bt = t1->defAst->scheme->type_instance_copy();
  
  if(bt->components->size() != t1->components->size()) {
    errStream << errAst->loc
	      << ": Incorrect Number of legs for the Union type "
	      << bt->asString()
	      << " Expected " << bt->components->size()
	      << " Obtained " << t1->components->size() << "."
	      << std::endl;
    
    errFree = false;
    return false;
  }
  
  if(bt->components->size() != t2->components->size()) {
    errStream << errAst->loc
	      << ": Incorrect Number of legs for the Union type "
	      << bt->asString()
	      << " Expected " << bt->components->size()
	      << " Obtained " << t2->components->size() << "."
	      << std::endl;
    
    errFree = false;
    return false;
  }
  
  //   for(size_t i=0; i<bt->components->size(); i++) 
  //     CHKERR(errFree, Unify(errStream, trail, errAst, bt->CompType(i), 
  //   			  t1->CompType(i), flags));
  
  //   for(size_t i=0; i<bt->components->size(); i++) 
  //     CHKERR(errFree, Unify(errStream, trail, errAst, bt->CompType(i), 
  //   			  t2->CompType(i), flags));
  
  //if(!errFree)
  //  return false;
  
  assert(t1->typeArgs->size() == t2->typeArgs->size());

  size_t n = trail->snapshot();
  trail->link(t1, t2);
  
  for(size_t i=0; i<t1->typeArgs->size(); i++) {
    CHKERR(errFree, Unify(errStream, trail, errAst, t1->TypeArg(i), 
			  t2->TypeArg(i), flags)); 
  }
  
  if(!errFree)
    trail->rollBack(n);
  
  return errFree;
}

/* Strict unification of typeArgs of t1 and t2
   should happen first.
   component unifications wiith bt, if at all done, 
   should be done without the UNIFY_STRICT flag.
*/
static bool 
UnifyUcon(std::ostream& errStream,
	   GCPtr<Trail> trail,
	   GCPtr<const AST> errAst,
	   GCPtr<Type> t1, GCPtr<Type> t2,
	   unsigned long flags)
{
  bool errFree = true;
  
#ifdef VERBOSE_UNIFY
  std::cout << "UnifyUcon" 
	    << t1->asString(NULL)
	    << t2->asString(NULL)
	    << std::endl;
#endif

  if(t1->myContainer != t2->myContainer) {
    typeError(errStream, errAst, t1, t2);
    return false;
  }  
  
  //  GCPtr<Type> utyp = t1->myContainer->symType->getType();
  //  errStream << "utype = " << utyp->asString() << std::endl;
  
  assert(t1->typeArgs->size() == t2->typeArgs->size());
  
  for(size_t i=0; i<t1->typeArgs->size(); i++) {
    CHKERR(errFree, Unify(errStream, trail, errAst, t1->TypeArg(i), 
			  t2->TypeArg(i), flags)); 
  }
  
//   errStream << "utype = " << utyp->asString() << std::endl;  
//   errStream << "Result [" << ((errFree)?"true":"false") << "] = " 
//        << t1->asString(NULL)
//        << t2->asString(NULL)
//        << std::endl;
  
  return errFree;
}

static bool 
UnifyUval(std::ostream& errStream,
	   GCPtr<Trail> trail,
	   GCPtr<const AST> errAst,
	   GCPtr<Type> t1, GCPtr<Type> t2,
	   unsigned long flags)
{
  bool errFree = true;
  
#ifdef VERBOSE_UNIFY
  std::cout << "UnifyUval" 
	    << t1->asString(NULL)
	    << t2->asString(NULL)
	    << std::endl;
#endif

  if(t1->myContainer != t2->myContainer)
    return typeError(errStream, errAst, t1, t2);
  
  assert(t1->typeArgs->size() == t2->typeArgs->size());

  size_t n = trail->snapshot();
  trail->link(t1, t2);
  
  for(size_t i=0; i<t1->typeArgs->size(); i++) {
    CHKERR(errFree, Unify(errStream, trail, errAst, t1->TypeArg(i), 
			  t2->TypeArg(i), flags)); 
  }
  
  if(!errFree)
    trail->rollBack(n);

  
  return errFree;
}


static bool 
UnifyUTVC(std::ostream& errStream,
	  GCPtr<Trail> trail,
	  GCPtr<const AST> errAst,
	  GCPtr<Type> ut1, GCPtr<Type> ut2,
	  unsigned long flags) 
{
  bool errFree = true;
  
#ifdef VERBOSE_UNIFY
  std::cout << "UnifyUTVC" 
	    << ut1->asString(NULL) << " and "
	    << ut2->asString(NULL)
	    << std::endl;
#endif
  
  if(ut1->myContainer != ut2->myContainer)
    return typeError(errStream, errAst, ut1, ut2);
  
  assert(ut1->typeArgs->size() == ut2->typeArgs->size());

  size_t n = trail->snapshot();
  trail->link(ut1, ut2);  

  for(size_t i=0; i<ut1->typeArgs->size(); i++) {
    CHKERR(errFree, Unify(errStream, trail, errAst, ut1->TypeArg(i), 
			  ut2->TypeArg(i), flags)); 
  }

  // If error, rollback. 
  // Otherwise, single remove to break the unnecessary link. 
  if(!errFree)
    trail->rollBack(n);
  else
    trail->release(n, ut1);

  //   errStream << "Result [" << ((errFree)?"true":"false") << "] = " 
  // 	    << ut->asString(NULL) << " and "
  // 	    << uv->asString(NULL)
  // 	    << std::endl;

  return errFree;
}


#define RET_UNIFY do{   \
    return true;        \
  }while(0)


#define RET_FAIL do{    \
    return false;       \
  }while(0) 


static bool
Unify(std::ostream& errStream,
      GCPtr<Trail> trail,
      GCPtr<const AST> errAst, GCPtr<Type> ft, GCPtr<Type> st, 
      unsigned long flags) 
{
  GCPtr<Type> t1 = ft->getType();
  GCPtr<Type> t2 = st->getType();
  bool errFree = true;

  
#ifdef VERBOSE_UNIFY
  std::cout << "Unifier: " 
  	    << ft->asString()
  	    << " vs " 
  	    << st->asString()
  	    << std::endl;  
#endif
    
  if (t1->uniqueID == t2->uniqueID)
    RET_UNIFY;

  if(t1->kind != t2->kind) {
    
    if(t1->kind == ty_tvar || t2->kind == ty_tvar) {
      /* Handle the case of Type Variables unifying with another type */
      GCPtr<Type> tv = (t1->kind == ty_tvar) ? t1 : t2;
      GCPtr<Type> typ = (t1->kind == ty_tvar) ? t2 : t1;
      
      if(typ->boundInType(tv)) {
	// This case is not really wrong: 'a = 'b|'a
        // but I want to find out if we get here.
	assert(false);
      }
      
      // This condition is not hoisted up because, we want the
      // execution to enter the type variable case and fail, 
      // rather then go to other cases.
      // Why should maybe-case exempt from UNIFY_STRICT? 
      if((flags & UNIFY_STRICT) == 0)
	if(((flags & UN_IGN_RIGIDITY) || ((tv->flags & TY_RIGID) == 0))) {
	  trail->subst(tv, typ);
	  RET_UNIFY;
	}
    }
    else if (t1->kind == ty_mbFull || t2->kind == ty_mbFull) {
      GCPtr<Type> mb = (t1->kind == ty_mbFull) ? t1 : t2;
      GCPtr<Type> typ = (t1->kind == ty_mbFull) ? t2 : t1;

      Unify(errStream, trail, errAst, mb->minimizeMutability(), 
	    typ->minimizeMutability(), flags);
      
      if(errFree)
	trail->link(mb->Var(), typ);
      return errFree;
    }
    else if(t1->kind == ty_mbTop || t2->kind == ty_mbTop) {      
      // ONLY one of the types is a maybe type.
      GCPtr<Type> mb = (t1->kind == ty_mbTop) ? t1 : t2;
      GCPtr<Type> typ = (t1->kind == ty_mbTop) ? t2 : t1;
      assert(typ->kind != ty_mbFull);
      
      Unify(errStream, trail, errAst, mb->minimizeMutability(), 
	    typ->minimizeMutability(), flags);
      
      if(errFree)
	trail->link(mb->Var(), typ);
      return errFree;
    }
    else if(t1->isUType() && t2->isUType() && 
	    (t1->isRefType() == t2->isRefType())) {
      return UnifyUTVC(errStream, trail, errAst, t1, t2, flags);
    }
    else {
      errFree = typeError(errStream, errAst, t1, t2);
      RET_FAIL;
    }
    
    // Must not reach here
    assert(false);
  }
  
  // Here, t1->kins == t2->kind.
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
      if(flags & UNIFY_STRICT_TVAR) {
	errFree = typeError(errStream, errAst, t1, t2);
	break;
      }

      if((t1->flags & TY_RIGID) && (t2->flags & TY_RIGID) &&
	 ((flags & UN_IGN_RIGIDITY) == 0)) {
	errFree = typeError(errStream, errAst, t1, t2);
	break;
      }

      // One of the types is not rigid, or we are ignoring rigidity.
      if(t1->flags & TY_RIGID)
	trail->subst(t2, t1);
      else
	trail->subst(t1, t2);
	
      break;
    }

  case ty_dummy:
    {
      // FIX?: For NOW, all dummy types are co-equal
      // I have done this so that instantiation is simple
      // And, it does not matter in practice.
      // Once we have an input mechanism for dummy types, 
      // we *may* want to treat dummy types like tvars by comparing
      // their  uniqueIDs
      break;
    }

#ifdef KEEP_BF
  case ty_bitfield:
    {	
      CHKERR(errFree, Unify(errStream, trail, errAst,
			    t1->CompType(0), 
			    t2->CompType(0), flags));
	
      if(!errFree)
	break;
	
      if(t1->Isize == t2->Isize)
	break;

      errStream << errAst->loc << ": "
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
      assert(t1->components->size() == 2);
      assert(t2->components->size() == 2);

      // I have repeated this code here and in
      // ty_fnarg because, in the presence of a type-error,
      // it is better to report the entire function type
      // rather than the argument types.
      GCPtr<Type> t1Args = t1->Args();
      GCPtr<Type> t2Args = t2->Args();

      if (t1Args->components->size() != t2Args->components->size()) {
	errFree = typeError(errStream, errAst, t1, t2);
	break;	// no point unifying the args, might segfault
      }

      for(size_t i=0; i< t1Args->components->size(); i++)
	CHKERR(errFree, Unify(errStream, trail, errAst,
			      t1Args->CompType(i), 
			      t2Args->CompType(i), flags));

      CHKERR(errFree, 
	     Unify(errStream, trail, errAst, t1->Ret(), 
		   t2->Ret(), flags));
      break;
    }

  case ty_fnarg:
    {
      if (t1->components->size() != t2->components->size()) {
	errFree = typeError(errStream, errAst, t1, t2);
	break;
      }
	
      for(size_t i=0; i< t1->components->size(); i++) {

	if ((t1->CompFlags(i) & COMP_BYREF_P) &&
	    ((t2->CompFlags(i) & COMP_BYREF_P) == 0)) {
	  t1->CompFlags(i) &= ~COMP_BYREF_P;
	  t1->CompFlags(i) |= t1->CompFlags(i) & COMP_BYREF;
	}
	else if ((t2->CompFlags(i) & COMP_BYREF_P) &&
		 ((t1->CompFlags(i) & COMP_BYREF_P) == 0)) {
	  t2->CompFlags(i) &= ~COMP_BYREF_P;
	  t2->CompFlags(i) |= t2->CompFlags(i) & COMP_BYREF;
	}
	else if(t1->CompFlags(i) != t2->CompFlags(i)) {
	  errFree = typeError(errStream, errAst, t1, t2);
	  break;
	}
	  	    
	CHKERR(errFree, Unify(errStream, trail, errAst,
			      t1->CompType(i), 
			      t2->CompType(i), flags));
      }
      break;
    }

  case ty_structv:
  case ty_structr:
    {
      CHKERR(errFree,
	     UnifyStruct(errStream, trail, errAst, t1, t2, flags));
      break;
    }

  case ty_reprv:
  case ty_reprr:
    {
      CHKERR(errFree,
	     UnifyUnion(errStream, trail, errAst, t1, t2, flags));
      break;
    }

  case ty_unionv:
  case ty_unionr:
    {
      CHKERR(errFree,
	     UnifyUnion(errStream, trail, errAst, t1, t2, flags));
      break;
    }

  case ty_uconr:
  case ty_uconv:
    {
      CHKERR(errFree,
	     UnifyUcon(errStream, trail, errAst, t1, t2, flags));
      break;
    }

  case ty_uvalr:
  case ty_uvalv:
    {
      CHKERR(errFree,
	     UnifyUval(errStream, trail, errAst, t1, t2, flags));
      break;
    }

  case ty_letGather:
    {      
      assert(t1->components->size() == t2->components->size());
	
      for(size_t i=0; i<t1->components->size(); i++) 
	CHKERR(errFree, 
	       Unify(errStream, trail, errAst, t1->CompType(i), 
		     t2->CompType(i), flags));
	
      break;
    }
      
  case ty_array:
    {
      CHKERR(errFree, 
	     Unify(errStream, trail, errAst, t1->Base(), 
		   t2->Base(), flags));

      if(t1->arrlen->len == t2->arrlen->len)
	break;
      
      // Array lengths did not Unify
      if(t1->arrlen->len == 0) {
	t1->arrlen->len = t2->arrlen->len;
	break;
      }
      else if(t2->arrlen->len == 0) {
	t2->arrlen->len = t1->arrlen->len;
	break;
      }
      else {
	errStream << errAst->loc 
		  << ": Array Lengths do not match. "
		  << t1->arrlen->len
		  << " vs " 
		  << t2->arrlen->len
		  << std::endl;
	errFree = false;
      }
      
      break;
    }
      
  case ty_vector:
    {
      CHKERR(errFree, 
	     Unify(errStream, trail, errAst, t1->Base(), 
		   t2->Base(), flags));
      break;
    }
    
  case ty_mbTop:
    {
      CHKERR(errFree,
	     Unify(errStream, trail, errAst, 
		   t1->Core()->minimizeTopMutability(), 
		   t2->Core()->minimizeTopMutability(), 
		   flags));
      
      trail->link(t1->Var(), t2->Var());
      break;
    }
    
  case ty_mbFull:
    {
      CHKERR(errFree,
	     Unify(errStream, trail, errAst, 
		   t1->Core()->minimizeMutability(), 
		   t2->Core()->minimizeMutability(), 
		   flags));
	
      trail->link(t1->Var(), t2->Var());
      break;
    }

  case ty_mutable:
  case ty_ref:
  case ty_byref:
    {
      assert(t1->components->size() == 1);
      assert(t2->components->size() == 1);
      CHKERR(errFree,
	     Unify(errStream, trail, errAst, t1->Base(), 
		   t2->Base(), flags));
      break;
    }

  case ty_exn:
    {
      // All exceptions belong to the same sum type.	
      break;
    }

  case ty_typeclass:
    {
      if(t1->defAst != t2->defAst) {
	errFree = typeError(errStream, errAst, t1, t2);
	break;
      }
	
      if(t1->typeArgs->size() != t2->typeArgs->size()) {
	errFree = typeError(errStream, errAst, t1, t2);
	break;
      }
	
      for(size_t i = 0; i < t1->typeArgs->size(); i++) {
	  
	CHKERR(errFree, Unify(errStream, trail, errAst, 
			      t1->TypeArg(i), t2->TypeArg(i),
			      flags));
      }
	
      break;
    }
      
    // The following cases are filled in so that strictlyEquals()
    // function works correctly.
  case ty_subtype:
  case ty_pcst:
    {
      assert(t1->components->size() == t2->components->size());
      for(size_t i=0; i < t1->components->size(); i++) 
	CHKERR(errFree,
	       Unify(errStream, trail, errAst, t1->CompType(i), 
		     t2->CompType(i), flags));
      break;
    }
      
  case ty_kvar:
  case ty_kfix:
    {
      // This check will never unify, since the following check has
      // already failed at the start of the unification algorithm.
      if (t1->uniqueID != t2->uniqueID)
	errFree = typeError(errStream, errAst, t1, t2);
      break;
    }
  }
  
#ifdef VERBOSE_UNIFY
  errStream << "\t Result: " 
	    << ft->asString(NULL)
	    << " == " 
	    << st->asString(NULL)
	    << std::endl;  
#endif
  
  if(errFree)
    RET_UNIFY;
  else
    RET_FAIL;
}

bool
acyclic(std::ostream& errStream,
	GCPtr<Type> typ, 
	GCPtr< WorkList<GCPtr<Type> > > worklist, // Types currently visiting
 	GCPtr< DoneList<GCPtr<Type> > > donelist, // Types Known to be OK 
	bool inref=false)
{
  assert(typ);

  GCPtr<Type> t = typ->getType();
  bool errFree = true;

  //std::cout << "Acyclic: Processing: " << t->asString(NULL)
  //	    << std::endl;
  
  if (donelist->contains(t))
    return true;
  
  if(worklist->contains(t)) {
    
    if(t->kind == ty_structr ||
       t->kind == ty_unionr || 
       t->kind == ty_uconr ||
       t->kind == ty_uvalr)
      return true;
    
    if(inref)
      return true;
    
    std::cerr << "Cyclic Type Definitions among the following " 
	      << worklist->size() << " types:" << std::endl;
    
    // By now, we know that the current type is already 
    // in the worklist
    std::cerr << "\t" << t->ast->loc << ": " 
    	      << t->asString(NULL, false) 
    	      << std::endl;
    for(size_t i=0; i<worklist->size(); i++)
      std::cerr << "\t" << worklist->elem(i)->ast->loc
		<< ": " << worklist->elem(i)->asString(NULL, false)
		<< std::endl;
    fatal();
    return false;
  }
	
  assert(worklist->add(t));
  for(size_t i=0; i < t->components->size(); i++) {
    CHKERR(errFree, acyclic(errStream, t->CompType(i), worklist, 
			    donelist,
			    (inref || t->kind == ty_ref)));
  }

//   for(size_t i=0; i < t->typeArgs->size(); i++) {
//     CHKERR(errFree, acyclic(errStream, t->TypeArg(i), worklist, 
// 			    donelist,
// 			    (inref || t->kind == ty_ref)));
//   }

  worklist->done(t);   
  if(errFree)
    donelist->add(t);
  
  //std::cout << "--"
  //          << std::endl;
  
  return errFree;		
}

bool
unify(std::ostream& errStream,
      GCPtr<Trail> trail,
      GCPtr<AST> errAst, GCPtr<Type> ft, GCPtr<Type> st, 
      unsigned long flags) 
{
  bool errFree = true;
  GCPtr< WorkList<GCPtr<Type> > > worklist = new WorkList<GCPtr<Type> >;
  GCPtr< DoneList<GCPtr<Type> > > donelist = new DoneList<GCPtr<Type> >;
  CHKERR(errFree, Unify(errStream, trail, errAst, ft, st, flags));
  CHKERR(errFree, acyclic(errStream, ft, worklist, donelist));
#ifdef VERBOSE_UNIFY
  std::cout << "____________________________"
	    << std::endl << std::endl;
#endif

  return errFree;
}


// Old TY_RIGID handling in ty_tvat case of the Unifier.
// 	   RIGID and RESTRICTED flags:
// 	   RIGID happens past generalization, and 
// 	   RESTRICTED occurs before generalization
// 	   These should never be present concurrently */

// 	if((t1->flags & TY_RIGID) && (t1->flags & TY_RESTRICTED))
// 	  assert(false);
	
// 	if((t2->flags & TY_RIGID) && (t2->flags & TY_RESTRICTED))
// 	  assert(false);
 	
// 	if((t1->flags & TY_RIGID) && (t2->flags & TY_RESTRICTED))
// 	  assert(false);
 	
// 	if((t2->flags & TY_RIGID) && (t1->flags & TY_RESTRICTED))
// 	  assert(false);

// 	if(t1->flags & TY_RIGID)
// 	  trail->subst(t2, t1);
// 	else if (t1->flags & TY_RESTRICTED)
// 	  trail->subst(t2, t1);
// 	else
// 	  trail->subst(t1, t2);
	
