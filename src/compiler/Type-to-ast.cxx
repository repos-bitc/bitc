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

using namespace boost;
using namespace sherpa;
using namespace std;


shared_ptr<AST> 
Type::asAST(const sherpa::LexLoc &loc,
            shared_ptr<TvPrinter> tvP)
{ 
  shared_ptr<AST> ast = GC_NULL;
  shared_ptr<Type> t = getType();  
  
  if (t->pMark >= 1) {
    ast = AST::make(at_ident, loc);
    ast->s = tvP->tvName(t);
    ast->identType = id_tvar;
    
    return ast;
  }
  else {
    t->pMark++;
  }

  switch(t->typeTag) {
  case ty_unit:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "unit";
      break;
    }
  case ty_bool:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "bool";
      break;
    }
  case ty_char:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "char";
      break;
    }
  case ty_string:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "string";
      break;
    }
  case ty_int8:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "int8";
      break;
    }
  case ty_int16:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "int16";
      break;
    }
  case ty_int32:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "int32";
      break;
    }
  case ty_int64:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "int64";
      break;
    }
  case ty_uint8:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "uint8";
      break;
    }
  case ty_uint16:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "uint16";
      break;
    }
  case ty_uint32:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "uint32";
      break;
    }
  case ty_uint64:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "uint64";
      break;
    }
  case ty_word:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "word";
      break;
    }
  case ty_float:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "float";
      break;
    }
  case ty_double:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "double";
      break;
    }
  case ty_quad:
    {
      ast = AST::make(at_primaryType, loc);
      ast->s = "quad";
      break;
    }
  case ty_tvar:
    {
      ast = AST::make(at_ident, loc);
      ast->s = tvP->tvName(t);
      ast->identType = id_tvar;
      break;
    }

  case ty_mbTop:
    {
      ast = t->Core()->asAST(loc, tvP);
      break;
    }

  case ty_mbFull:
    {
      if(t->Var()->isMutable())
        ast = t->Core()->maximizeMutability()->asAST(loc, tvP);
      else
        ast = t->Core()->asAST(loc, tvP);
        
      break;
    }

  case ty_dummy:
    {
      ast = AST::make(at_dummyType, loc);
      break;
    }


#ifdef KEEP_BF
  case  ty_bitfield:
    {
      shared_ptr<AST> typ = t->CompType(0)->asAST(loc, tvP);
      shared_ptr<AST> intLit = AST::make(at_intLiteral, loc);
      mpz_init_set_ui(intLit->litValue.i, t->Isize);
      intLit->litBase = 10;
      ast = AST::make(at_bitfield, loc, typ, intLit);
      break;
    }
#endif

  case ty_method:
    {
      assert(t->components.size() == 2);
      shared_ptr<AST> arg = t->Args()->asAST(loc, tvP);
      shared_ptr<AST> ret = t->Ret()->asAST(loc, tvP);
      ast = AST::make(at_methType, loc, arg, ret);
      break;
    }

  case ty_fn:
    {
      assert(t->components.size() == 2);
      shared_ptr<AST> arg = t->Args()->asAST(loc, tvP);
      shared_ptr<AST> ret = t->Ret()->asAST(loc, tvP);
      ast = AST::make(at_fn, loc, arg, ret);
      break;
    }

  case ty_fnarg:
    {
      ast = AST::make(at_fnargVec, loc);
      for (size_t i=0; i < t->components.size(); i++) {
        shared_ptr<AST> arg = t->CompType(i)->asAST(loc, tvP);
        if (t->CompFlags(i) & COMP_BYREF)
          arg = AST::make(at_byRefType, arg->loc, arg);
        
        ast->children.push_back(arg);
      }
      break;
    }
    
  case ty_tyfn:
    {
      assert(false);
      break;
    }    

  case ty_structv:
  case ty_structr:
  case ty_unionv: 
  case ty_unionr:
    {
      ast = t->defAst->Use();
      //ast = AST::make(at_ident, loc);
      //ast->s = t->defAst->s;
      //ast->symbolDef = t->defAst;
      if (t->typeArgs.size() > 0) {
        ast = AST::make(at_typeapp, loc, ast);
        for (size_t i=0; i < t->typeArgs.size(); i++)
          ast->children.push_back(t->TypeArg(i)->asAST(loc, tvP));
      }
      break;
    }
    
  case ty_field:
    {
      shared_ptr<AST> fld = AST::make(at_ident, loc);
      fld->s = t->litValue.s;
      ast = AST::make(at_fieldType, fld->loc, fld);
      break;;
    }
    
  case ty_uconv: 
  case ty_uconr:
  case ty_uvalv: 
  case ty_uvalr:
    {
      ast = t->myContainer->Use();
      //ast = AST::make(at_ident, loc);
      //ast->s = t->myContainer->s;
      if (t->typeArgs.size()) {
        ast = AST::make(at_typeapp, loc, ast);
        for (size_t i=0; i < t->typeArgs.size(); i++)
          ast->children.push_back(t->TypeArg(i)->asAST(loc, tvP));
      }
      break;
    }
    
  case ty_typeclass:
    assert(false);

  case ty_array:
    {
      shared_ptr<AST> typ = t->Base()->asAST(loc, tvP);
      shared_ptr<AST> intLit = AST::make(at_intLiteral, loc);
      intLit->litValue.i = t->arrLen->len;
      intLit->litBase = 10;
      intLit->s = intLit->litValue.i.asString(10);
      ast = AST::make(at_arrayType, loc, typ, intLit);
      break;
    }

  case ty_vector:
    {
      shared_ptr<AST> typ = t->Base()->asAST(loc, tvP);
      ast = AST::make(at_vectorType, loc, typ);
      break;
    }

  case ty_ref:
    {
      shared_ptr<AST> typ = t->Base()->asAST(loc, tvP);
      ast = AST::make(at_boxedType, loc, typ);
      break;
    }

  case ty_byref:
    {
      shared_ptr<AST> typ = t->Base()->asAST(loc, tvP);
      ast = AST::make(at_byRefType, loc, typ);
      break;
    }

  case ty_array_ref:
    {
      shared_ptr<AST> typ = t->Base()->asAST(loc, tvP);
      ast = AST::make(at_arrayRefType, loc, typ);
      break;
    }

  case ty_mutable:
    {
      shared_ptr<AST> typ = t->Base()->asAST(loc, tvP);
      ast = AST::make(at_mutableType, loc, typ);
      break;
    }

  case ty_const:
    {
      shared_ptr<AST> typ = t->Base()->asAST(loc, tvP);
      ast = AST::make(at_constType, loc, typ);
      break;
    }

  case ty_exn:
    {
      ast = AST::make(at_exceptionType, loc);
      break;
    }
   
  case ty_letGather:
  case ty_pcst:
  case ty_kvar:
  case ty_kfix:
    assert(false);
    break;
  }

  t->pMark--;  
  assert(ast);
  return ast;
}
