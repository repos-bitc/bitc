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

#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "machine-dep.hxx"

static size_t
calc_struct_size(const GCPtr<Type> t)
{
  size_t start=0;
  size_t sz = 0;
  GCPtr<AST> base = NULL;

  if(t->kind == ty_structv) {
    start = 0;
    base = t->defAst->defForm->child(4);
  }
  else if (t->kind == ty_uconv) {
    start = 1;
    base = t->defAst;
  }
  else
    assert(false);
  

  // Fill elements and repr discriminators are not a part of the type.
  // So, we need a separate count for the type components.
  size_t compCnt=0;
  for(size_t i=start; i < base->children->size(); i++) {
    GCPtr<AST> fld = base->child(i);
    
    switch(fld->astType) {
    case at_fill:
    case at_reserved:
      {
	sz += fld->field_bits;
	break;
      }
      
    case at_field:
      {
	if(fld->field_bits != 0) {
	  assert(fld->symType->isIntegral());
	  sz += fld->field_bits;
	}
	else {
	  sz+= t->CompType(compCnt)->size();
	}

	if((fld->Flags2 & FLD_IS_DISCM) == 0)
	  compCnt++;
	break;
      }
      
    default:
      {
	assert(false);
	break;
      }
    }
  }
    
  return sz;
}


static size_t
calc_unin_size(const GCPtr<Type> t)
{
  assert(t->kind == ty_unionv);
  GCPtr<AST> base = t->defAst->defForm;
  
  size_t max=0;
  for(size_t i=0; i < base->children->size(); i++) {
    GCPtr<AST> ctr = base->child(i);

    // Constructors like `nil' having no arguments do not count.
    if(ctr->symType->isUval())
      continue;
    
    assert(ctr->symType->isUcon());
    size_t sz = calc_struct_size(ctr->symType);
    
    if(max < sz)
      max = sz;
  }
   
  size_t tag=0;
  if((base->Flags2 & ENUM_UN) || (base->Flags2 & SINGLE_LEG_UN) || 
     (base->Flags2 & CARDELLI_UN) || (base->Flags2 & UNION_IS_REPR))
    tag=0;
  else 
    tag = base->tagType->size();

  return (max + tag);
}


size_t
Type::size() 
{ 
  if(link)
    return link->getType()->size();

  if(mark & MARK21)
    assert(false);

  mark |= MARK21;

  size_t theSize=0;
  
  switch(kind) {
  case ty_unit:
  case ty_bool:
  case ty_char:
    theSize = 8;
    break;

  case ty_int8:
  case ty_uint8:
  case ty_int16:
  case ty_uint16:
  case ty_int32:
  case ty_uint32:
  case ty_int64:
  case ty_uint64:
    theSize = nBits();
    break;

  case ty_word:
    theSize = TARGET_WORD_SIZE;
    break;

  case ty_float:
    theSize = TARGET_FLOAT_SIZE;
    break;

  case ty_double:
    theSize = TARGET_DOUBLE_SIZE;
    break;

  case ty_quad:
    theSize = TARGET_QUAD_SIZE;
    break;
    
#ifdef KEEP_BF
  case  ty_bitfield:
    theSize = Isize;
    break;
#endif

  case ty_fn:
  case ty_string:
  case ty_dummy:
  case ty_structr:
  case ty_unionr:
  case ty_uvalr:
  case ty_uconr:
  case ty_vector:
  case ty_ref:
  case ty_byref:
  case ty_exn:
    theSize = TARGET_QUAD_SIZE;
    break;

  case ty_tvar:
  case ty_fnarg:
  case ty_tyfn:
  case ty_letGather:
  case ty_typeclass:    
  case ty_reprv:
  case ty_reprr:
  case ty_subtype:
  case ty_pcst:
  case ty_kvar:
  case ty_kfix:
    assert(false);
    break;
    
  case ty_structv:
    {
      //       // Fix: inner bit-fields
      //       for(size_t i=0; i < components->size(); i++)
      // 	theSize += CompType(i)->size();
      //       theSize += defAst->total_fill;
      //       break;
      
      theSize = calc_struct_size(this);
      break;
    }

  case ty_unionv: 
    {
      theSize = calc_unin_size(this);
      break;
    }

  case ty_uvalv: 
  case ty_uconv: 
    {
      theSize = getUnionType()->size();
      break;
    }
    
  case ty_array:
    {
      theSize = Base()->size() * arrlen;
      break;
    }
    
  case ty_mbTop:
  case ty_mbFull:
    {
      theSize = Core()->size();
    }
    
  case ty_mutable:
    {
      theSize = Base()->size();      
      break;
    }
  }
  
  mark &= ~MARK21;
  return theSize;
}

