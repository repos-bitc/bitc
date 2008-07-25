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

/* arg1: Ignore all mutability in name 
   arg2: Ignore TopLevel mutability and top-level mutability at 
         the (first) toplevel fn-arg-ret positions in name */
string
Type::mangledString(bool igMut, bool igTlMut, bool maxArgMut)
{
  // Reserved start characters: (Do not use)
  // "__" "VS"

  stringstream ss;
  
  if(getType() != this)
    return getType()->mangledString(igMut, igTlMut, maxArgMut);

  if(mark & MARK5) {
    // Encountered Infinite Type
    assert(false);
  }

  mark |= MARK5;

  switch(kind) {
  case ty_tvar:
    {
      stringstream avss;
      avss << "'a" << uniqueID;

      ss << "_" << avss.str().size() << avss.str();
      break;
    }
    
  case ty_dummy:
    {
      ss << "D";
      break;
    }

  case ty_unit:
    ss << "_4unit";
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
      const char *tn = kindName();
      ss << "_" << strlen(tn) << tn;
      break;
    }

#ifdef KEEP_BF
  case  ty_bitfield:
    ss << "BF" << Isize << CompType(0)->mangledString(igMut, false);
    break;
#endif

  case ty_fn:
    assert(components->size() == 2);
    
    ss << "FN" 
       << Args()->mangledString(igMut, true,maxArgMut)
       << Ret()->mangledString(igMut, true, maxArgMut);

    break;

  case ty_fnarg:
    ss << components->size();
    for(size_t i=0; i<components->size(); i++) {
      if(CompFlags(i) & COMP_BYREF)
	ss << "Z";
      ss << CompType(i)->mangledString(igMut, true, 
				       maxArgMut);
    }
    break;

  case ty_letGather:
    ss << "LG";
    for(size_t i=0; i<components->size(); i++) {
      ss << CompType(i)->mangledString(igMut, igTlMut, 
					      maxArgMut);
    }
    
    break;

  case ty_structv:
  case ty_structr:
    {
      ss << "S"
	 << ((kind == ty_structv) ? "V" : "R")
	 << typeArgs->size() 
	 << "_" << defAst->s.size() << defAst->s;

      for(size_t i=0; i < typeArgs->size(); i++)
	ss << TypeArg(i)->mangledString(igMut, false, 
					 maxArgMut);
      break;
    }

  case ty_typeclass:    
    {
      ss << "TC"
	 << "_" << defAst->s.size() << defAst->s;
      for(size_t i=0; i < typeArgs->size(); i++)
	ss << TypeArg(i)->mangledString(igMut, false, 
					 maxArgMut);
      
      break;
    }

  case ty_uconv:
  case ty_uconr:
  case ty_uvalv:
  case ty_uvalr:
  case ty_unionv:
  case ty_unionr:
    {
      ss << "U"
	 << (isRefType() ? "R" : "V")
	 << typeArgs->size() 
	 << "_" << myContainer->s.size() << myContainer->s;

      for(size_t i=0; i < typeArgs->size(); i++)
	ss << TypeArg(i)->mangledString(igMut, false, maxArgMut);
    }
    break;

  case ty_reprv:
  case ty_reprr:
    {
      ss << "D"
	 << (isRefType() ? "R" : "V")
	 << typeArgs->size() 
	 << "_" << myContainer->s.size() << myContainer->s;

      for(size_t i=0; i < typeArgs->size(); i++)
	ss << TypeArg(i)->mangledString(igMut, false, maxArgMut);
    }
    break;

  case ty_array:
    {
      assert(components->size() == 1);
      ss << "J" << Base()->mangledString(igMut, false, maxArgMut)
	 << "__" << arrlen->len;
      break;
    }
    
  case ty_vector:
    {
      assert(components->size() == 1);
      ss << "K" << Base()->mangledString(igMut, false, maxArgMut);
      break;
    }

  case ty_ref:
    assert(components->size() == 1);
    ss << "R" << Base()->mangledString(igMut, false, maxArgMut);
    break;

  case ty_byref:
    assert(components->size() == 1);
    ss << "Z" << Base()->mangledString(igMut, false, maxArgMut);
    break;

  case ty_mutable:
    assert(components->size() == 1);
    if(!igMut && !igTlMut)
      ss << "M";
    ss << Base()->mangledString(igMut, false, maxArgMut);
    break;
    
  case ty_exn:
    {
      string s = "exception";
      ss << "_" << s.size() << s;
      //ss << "X" << defAst->s.size() << defAst->s;
      break;
    }
    
    /* MAYBE handling right? */
  case ty_mbFull:
  case ty_mbTop:
    {
      ss << Core()->mangledString(igMut, false, maxArgMut);
      break;
    }

  case ty_tyfn:
  case ty_subtype:
  case ty_pcst:
  case ty_kvar:
  case ty_kfix:
    {
      assert(false);
      break;
    }
  }
  
  mark &= ~MARK5;
  return ss.str();
}
