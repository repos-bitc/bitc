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

//#define DEBUG_SHOW_ALL_LINKS
//#define DEBUG_SHOW_RIGIDITY

static string
printName(GCPtr<AST> ast)
{
  if(!ast)
    return "NULL";
  
  return ast->s;
}

string
Type::asString(GCPtr<TvPrinter> tvP, bool traverse) 
{ 

  if(Options::rawTvars)
    tvP = 0;

  GCPtr<Type> t;
  if(traverse)
    t = getType();
  else
    t = this;
  
  if(t->pMark >= 2)
    return " ... ";
  else 
    t->pMark++;

  stringstream ss;
  
#ifdef DEBUG_SHOW_ALL_LINKS  
  if(traverse) {
    GCPtr<Type> t1 = this;
    ss << "[";
    while(t1->link) {
      ss << "'a" << t1->uniqueID;
      ss << "->";
      t1 = t1->link;
    }
    ss << "'a" << t1->uniqueID;
    ss << "]";
  }
#endif
    
  switch(t->kind) {
  case ty_unit:
    {
      ss << "()";
      break;
    }

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
      ss << t->kindName();
      break;
    }

  case ty_tvar:
    {
      if(!tvP) {
	ss << "'a" << t->uniqueID;
	if(t->flags & TY_RIGID) 
	  ss << 'R';
      }
      else {
	ss << tvP->tvName(t);
      }
      

      break;
    }

  case ty_kvar:
    {
      ss << "'K" << t->uniqueID;
      break;
    }

  case ty_dummy:
    {
      ss << "#DUMMY#";
      //ss << "#X" << t->uniqueID;
      break;
    }

#ifdef KEEP_BF
  case  ty_bitfield:
    {
      ss << "(bitfield "
	 << t->CompType(0)->toString()
	 << " "
	 << t->Isize
	 << ")";
      break;
    }
#endif

  case ty_fn:
    {
      assert(t->components->size() == 2);
      ss << "(fn " 
	 << t->Args()->asString(tvP, traverse) 
	 << " " 
	 << t->Ret()->asString(tvP, traverse) 
	 << ")";
      break;
    }

  case ty_fnarg:
    {
      ss <<  "(";
      for(size_t i=0; i < t->components->size(); i++) {
	if (i > 0) 
	  ss << " ";
	if(t->CompFlags(i) & COMP_BYREF)
	  ss << "(byref " 
	     << t->CompType(i)->asString(tvP, traverse)
	     << ")";
	else	   
	  ss << t->CompType(i)->asString(tvP, traverse);

      }
      ss << ")";
      break;
    }

  case ty_tyfn:
    {
      assert(t->components->size() == 2);
      ss << "(tyfn " 
	 <<  t->Args()->asString(tvP, traverse) 
	 << " " 
	 << t->Ret()->asString(tvP, traverse) 
	 << ")";
      break;          
    }

  case ty_letGather:
    {
      ss <<  "(__letGather ";
      for(size_t i=0; i < t->components->size(); i++) {
	if (i > 0) ss << " ";
	ss << t->CompType(i)->asString(tvP, traverse);
      }
      ss << ")";
      break;
    }

  case ty_structv:
  case ty_structr:
    {
      if(t->typeArgs->size() == 0)
	ss << printName(t->defAst);
      else {
	ss << "(" << printName(t->defAst);
	for(size_t i=0; i < t->typeArgs->size(); i++)
	  ss << " " << t->TypeArg(i)->asString(tvP, traverse);
	ss << ")";
      }
      
      break;
    }

  case ty_reprv:
  case ty_reprr:
  case ty_unionv: 
  case ty_unionr:
  case ty_uvalv: 
  case ty_uvalr:
  case ty_uconv: 
  case ty_uconr:
    {
      if(t->typeArgs->size() == 0)
	ss << printName(t->myContainer);
      else {
	ss << "(" << printName(t->myContainer);
	for(size_t i=0; i < t->typeArgs->size(); i++)
	  ss << " " << t->TypeArg(i)->asString(tvP, traverse);
	ss << ")";
      }
      
      break;
    }

  case ty_typeclass:    
    if(t->typeArgs->size() == 0)
      ss << printName(t->defAst);
    else {
      ss << "(" << printName(t->defAst);
	for(size_t i=0; i < t->typeArgs->size(); i++)
	  ss << " " << t->TypeArg(i)->asString(tvP, traverse);
	ss << ")";
    }
    break;

  case ty_array:
    {
      ss << "(array "
	 << t->Base()->asString(tvP, traverse)
	 << " "
	 << t->arrlen->len
	 << ")";
      break;
    }

  case ty_vector:
    {
      ss << "(vector " 
	 << t->Base()->asString(tvP, traverse) 
	 <<  ")";
      break;
    }
    
  case ty_ref:
    {
      assert(t->components->size() == 1);
      ss << "(ref "
	 << t->Base()->asString(tvP, traverse) 
	 << ")";
      break;
    }

  case ty_byref:
    {
      assert(t->components->size() == 1);
      ss << "(by-ref "
	 << t->Base()->asString(tvP, traverse) 
	 << ")";
      break;
    }

  case ty_mbFull:
  case ty_mbTop:
    {
      ss << t->Var()->asString(tvP, traverse)
	 << ((t->kind == ty_mbFull) ? "|" : "!")
	 << ((t->Core()->kind == ty_fn)?"(":"")
	 << t->Core()->asString(tvP, traverse)
	 << ((t->Core()->kind == ty_fn)?")":"");
      break;
    }

  case ty_mutable:
    {
      ss << "(mutable " 
	 << t->Base()->asString(tvP, traverse) 
	 << ")";
      break;  
    }

  case ty_subtype:
    {
      ss << t->CompType(0)->asString(tvP, traverse) 
	 << " < "
	 << t->CompType(1)->asString(tvP, traverse);
      break;
    }
    
  case ty_pcst:
    {
      ss << "*(";
      for(size_t i=0; i<t->components->size(); i++) {
	if(i > 0)
	  ss << ", ";
	ss << t->CompType(i)->asString(tvP, traverse);
      }
      ss << ")";
      break;
    }

  case ty_kfix:
    {
      if(t == Type::Kmono)
	ss << "m";
      else if(t == Type::Kpoly)
	ss << "P";
      else
	assert(false);
      break;
    }

  case ty_exn:
    {
      ss << "exception"; 
      break;
    }
  }
  
  t->pMark--;
  return ss.str();
}


std::string
TypeScheme::asString(GCPtr<TvPrinter> tvP, bool norm)
{
  std::stringstream ss; 
  bool forall = false;

  if(norm)
    normalize();
  
  if(Options::FQtypes)
    if(ftvs->size()) {
      ss << "(forall";
      forall = true;
      for(size_t i=0; i < ftvs->size(); i++)      
	ss << " " << Ftv(i)->asString(tvP);      
      ss << " ";
    }

  if(tcc) {
    if(Options::showAllTccs) {
      if(tcc->pred->size()) {
	if(!forall) {
	  ss << "(forall";
	  forall = true;
	}

	ss << " (";
	for(size_t i=0; i < tcc->pred->size(); i++) {
	  GCPtr<Typeclass> pred = tcc->Pred(i)->getType();
	  ss << pred->asString(tvP) << " ";
	  
	  if(pred->fnDeps)	  
	    for(size_t j=0; j < pred->fnDeps->size(); j++) {
	      ss << pred->FnDep(j)->asString(tvP) << " ";
	    }
	}
	ss << ") ";
      }
    }
    else { 
      //GCPtr<TCConstraints> _tcc = new TCConstraints;
      //addConstraints(_tcc);
      GCPtr<TCConstraints> _tcc = tcc;

      if(_tcc->pred->size()) {
 	for(size_t i=0; i < _tcc->pred->size(); i++) {
	  if(((_tcc->Pred(i)->flags & TY_CT_SUBSUMED) == 0) && 
	     ((_tcc->Pred(i)->flags & TY_CT_SELF) == 0)) {
	    if(!forall) {
	      ss << "(forall (";
	      forall = true;
	    }
	    ss << _tcc->Pred(i)->asString(tvP) << " ";	  
	  }
	}

	if(forall)
	  ss << ") ";
      }
    }
  }
  
  ss << tau->asString(tvP);
  if(forall)
    ss << ")";
  
  return ss.str();
}

std::string 
Instance::asString()
{
  return ts->tau->asString();
}

