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
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/avl.hxx>
#include <assert.h>
#include <sstream>

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

string
Type::toString()
{
  stringstream ss;

  if(link)
    return getType()->toString();

  if(pMark >= 2)
    return " ... ";
  else 
    pMark++;

  
  switch(kind) {
  case ty_tvar:
    ss << "'a" << uniqueID;
    break;
  
  case ty_kvar:
      ss << "'k" << uniqueID;
      break;
  
  case ty_dummy:
    ss << "#DUMMY#";
    //ss << "#X" << uniqueID;
    break;

  case ty_unit:
    ss << "()";
    break;

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
    {
      ss << kindName();
      break;
    }

#ifdef KEEP_BF
  case  ty_bitfield:
    ss << "(bitfield "
       << CompType(0)->toString()
       << " "
       << Isize
       << ")";
    break;
#endif
    
  case ty_fn:
    assert(components->size() == 2);
    ss << "(fn " << CompType(0)->toString() 
       <<  " " << CompType(1)->toString() << ")";
    break;

  case ty_fnarg:
    ss << "(";
    for(size_t i=0; i<components->size(); i++) {
      if (i > 0) ss << " ";
      if(CompFlags(i) & COMP_BYREF)
	ss << "(by-ref " << CompType(i)->toString() << ")";
      else
	ss << CompType(i)->toString();      
    }
    ss << ")";
    break;

  case ty_letGather:
    ss << "(__letGather ";
    for(size_t i=0; i<components->size(); i++) {
      if (i > 0) ss << " ";
      ss << CompType(i)->toString();
    }
    ss << ")";
    
    break;

  case ty_structv:
    ss << "(struct " <<defAst->s << " - ";
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->toString() << " ";
    ss << ")";
    break;
    
  case ty_structr:
    ss <<  "(structR " << defAst->s << " - ";
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->toString() << " ";
    ss << ")";
    break;
    
  case ty_unionv:
    ss << "(union " << defAst->s;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ") [";
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->getType()->toString();
    ss << "]";
    break;

  case ty_unionr:
    ss << "(unionR " << defAst->s;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ") [";
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->getType()->toString();
    ss << "]";
    break;

  case ty_uconv:
    ss << "(union-con " << defAst->s;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ") ["; 
      
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->getType()->toString();
    ss << "]";
    break;

  case ty_uconr:
    ss << "(unionR-con " << defAst->s;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ") ["; 
      
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->getType()->toString();
    ss << "]";
    break;

  case ty_uvalv:
    ss << "(union-val " << defAst->s;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ") [";
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->getType()->toString();
    ss << "]";
    break;

  case ty_uvalr:
    ss << "(unionR-val " << defAst->s;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ") [";
    for(size_t i=0; i<components->size(); i++)
      ss << CompName(i) << ":" 
	 << CompType(i)->getType()->toString();
    ss << "]";
    break;

  case ty_reprv:
    ss << "(repr " << defAst->s;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ")";
    //    ss << " [";
    //    for(size_t i=0; i<components->size(); i++)
    //      ss << CompType(i)->getType()->toString();
    //    ss << "]";
    break;

  case ty_reprr:
    ss << "(reprR " << defAst->s ;
    for(size_t i=0; i<typeArgs->size(); i++)
      ss << TypeArg(i)->getType()->toString();
    ss << ")";
    //    ss << " [";
    //    for(size_t i=0; i<components->size(); i++)
    //      ss << CompType(i)->getType()->toString();
    //    ss << "]";
    break;
    
  case ty_typeclass:    
    ss << "(Typeclass " << defAst->s;
    for(size_t i=0; i < typeArgs->size(); i++)
      ss << " " << TypeArg(i)->toString();
    ss << ")";
    break;

  case ty_array:
    ss <<  "(array "
       << CompType(0)->toString()
       << " " << arrlen
       << ")";
    break;
    
  case ty_vector:
    ss << "(vector ";
    for(size_t i=0; i<components->size(); i++) {
      if (i > 0) ss << " ";
      ss << CompType(i)->toString();
    }
    ss << ")";
    break;

  case ty_ref:
    assert(components->size() == 1);
    ss << "(ref " << CompType(0)->toString() << ")";
    break;


  case ty_byref:
    assert(components->size() == 1);
    ss << "(by-ref " << CompType(0)->toString() << ")";
    break;

  case ty_mutable:
    assert(components->size() == 1);
    ss << "(mutable " << CompType(0)->toString() << ")";
    break;

  case ty_exn:
    if(defAst)
      ss << "(exn " << defAst->s << ")"; 
    else
      ss << "exception";     
    break;

  case ty_tyfn:
    assert(components->size() == 2);
    ss << "(tyfn (" << CompType(0)->toString() 
       <<  ") " << CompType(1)->toString() << ")";
    break;

  case ty_maybe:
    ss << "(maybe " << CompType(0)->toString();      
    if(hints)
      ss << " " << hints->toString();
    ss  << ")";
    break;    

  case ty_hint:
    ss << "(hints ";
    for(size_t i=0; i<components->size(); i++)
      ss << CompType(i)->getType()->toString();
    ss << ")";
    break;

  case ty_subtype:
    {
      ss << CompType(0)->toString() 
	 << " < "
	 << CompType(1)->toString();
      break;
    }
    
  case ty_pcst:
    {
      ss << "(*";
      for(size_t i=0; i<components->size(); i++)
	ss << CompType(i)->toString();
      break;
    }

  case ty_kfix:
    {
      GCPtr<Type> t = this; // To satisfy libsherpa
      if(t == Type::Kmono)
	ss << "m";
      else if(t == Type::Kpoly)
	ss << "P";
      else
	assert(false);
      break;
    }
  }

  pMark--;
  return ss.str();
}
