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

using namespace boost;
using namespace sherpa;
using namespace std;

static string
printName(shared_ptr<AST> ast)
{
  if (!ast)
    return "NULL";
  
  return ast->s;
}

void
Type::asXML(shared_ptr<TvPrinter> tvP, INOstream &out) 
{ 
  if (Options::rawTvars)
    tvP = GC_NULL;

  shared_ptr<Type> t = getType();

  if (t->pMark >= 2) {
    out << "<infinity/>" << endl;
    return;
  }
  else {
    t->pMark++;
  }
  
  switch(t->kind) {
  case ty_unit:
    {
      out << "<unit/>" << endl;
      break;
    }

  case ty_bool:    
  case ty_char:
  case ty_string:
  case ty_word:
  case ty_float:
  case ty_double:
  case ty_quad:
    {
      out << "<" << t->kindName() << "/>" << endl;
      break;
    }    
  case ty_int8:
    {
      out << "<int sz='8'/>" << endl;
      break;
    }
  case ty_int16:
    {
      out << "<int sz='16'/>" << endl;
      break;
    }
  case ty_int32:
    {
      out << "<int sz='32'/>" << endl;
      break;
    }
  case ty_int64:
    {
      out << "<int sz='64'/>" << endl;
      break;
    }
  case ty_uint8:
    {
      out << "<uint sz='8'/>" << endl;
      break;
    }
  case ty_uint16:
    {
      out << "<uint sz='16'/>" << endl;
      break;
    }
  case ty_uint32:
    {
      out << "<uint sz='32'/>" << endl;
      break;
    }
  case ty_uint64:
    {
      out << "<uint sz='64'/>" << endl;
      break;
    }

  case ty_field:
    {
      out << "<field name='" << t->litValue.s << "'/>";
      break;
    }

  case ty_tvar:
    {
      string name;
      if (tvP)
	name = tvP->tvName(t);
      
      if (name == "'a")
	out << "<tvar name='alpha'/>" << endl; 
      else if (name == "'b")
	out << "<tvar name='beta'/>" << endl; 
      else if (name == "'c")
	out << "<tvar name='gamma'/>" << endl; 
      else if (name == "'d")
	out << "<tvar name='delta'/>" << endl; 
      else
	out << "<tvar name='alpha' num='"<< t->uniqueID << "'/>" << endl; 
      break;
    }

  case ty_kvar:
    {      
      out << "<lKind k='var' num='"<< t->uniqueID << "'/>" << endl; 
      break;
    }
    
  case ty_dummy:
    {
      out << "<dummy/>" << endl;
      break;
    }

#ifdef KEEP_BF
  case  ty_bitfield:
    {
      assert(false);
      break;
    }
#endif

  case ty_method:
  case ty_fn:
    {
      std::string s = (t->kind == ty_fn)?"fn":"method";
      assert(t->components.size() == 2);
      out << "<" << s << ">" << endl;
      out.more();
      out << "<tuple>" << endl;
      out.more();
      t->Args()->asXML(tvP, out);
      out.less();
      out << "</tuple>" << endl;
      t->Ret()->minimizeMutability()->asXML(tvP, out);
      out.less();
      out << "</" << s << ">" << endl;
      break;
    }

  case ty_fnarg:
    {
      for (size_t i=0; i < t->components.size(); i++) {
	if (t->CompFlags(i) & COMP_BYREF) {
	  out << " <byref> ";
	  t->CompType(i)->asXML(tvP, out);
	  out << "<byref> ";
	}
	else {
	  t->CompType(i)->minimizeMutability()->asXML(tvP, out);
	}
      }
      break;
    }
    
  case ty_tyfn:
    {
      assert(t->components.size() == 2);
      out << "<tyfn>" << endl;
      out.more();
      out << "<tuple>" << endl;
      out.more();
      t->Args()->asXML(tvP, out);
      out.less();
      out << "</tuple>" << endl;
      t->Ret()->minimizeMutability()->asXML(tvP, out);
      out.less();
      out << "</tyfn>" << endl;
      break;
    }

  case ty_letGather:
    {
      assert(false);
      break;
    }

  case ty_structv:
  case ty_structr:
    {
      out << "<struct inline='yes' name='" << printName(t->defAst) <<"'>" << endl; 
      out.more();
      for (size_t i=0; i < t->typeArgs.size(); i++)
	t->TypeArg(i)->asXML(tvP, out);
      out.less();
      out << "</struct>" << endl;
	break;
    }

  case ty_unionv: 
  case ty_unionr:
  case ty_uvalv: 
  case ty_uvalr:
  case ty_uconv: 
  case ty_uconr:
    {
      out << "<union inline='yes' name='" << printName(t->myContainer) <<"'>" << endl; 
      out.more();
      for (size_t i=0; i < t->typeArgs.size(); i++)
	t->TypeArg(i)->asXML(tvP, out);
      out.less();
      out << "</union>" << endl;
      break;
    }

  case ty_typeclass:    
    {
      out << "<typeclass name='" << printName(t->defAst) <<"'>" << endl; 
      out.more();
      for (size_t i=0; i < t->typeArgs.size(); i++)
	t->TypeArg(i)->asXML(tvP, out);
      out.less();
      out << "</typeclass>" << endl;
      break;
    }

  case ty_array:
    {
      out << "<array sz='" << t->arrLen->len  <<"'>" << endl;
      out.more();
      t->Base()->asXML(tvP, out);
      out.less();
      out << "</array>" << endl; 
      break;
    }

  case ty_vector:
    {
      out << "<vector>" << endl;
      out.more();
      t->Base()->asXML(tvP, out);
      out.less();       
      out << "</vector>" << endl; 
      break;
    }

  case ty_ref:
    {
      out << "<ref>" << endl;
      out.more();
      t->Base()->asXML(tvP, out);
      out.less();       
      out << "</ref>" << endl; 
      break;
    }

  case ty_byref:
    {
      out << "<byref>" << endl;
      out.more();
      t->Base()->asXML(tvP, out);
      out.less();       
      out << "</byref>" << endl; 
      break;
    }

  case ty_array_ref:
    {
      out << "<array-byref>" << endl;
      out.more();
      t->Base()->asXML(tvP, out);
      out.less();       
      out << "</array-byref>" << endl; 
      break;
    }

  case ty_mbTop:
  case ty_mbFull:
    {
      out << ((t->kind == ty_mbFull) ? "<MBPAIR>" : "<mbpair>") 
	  << endl;
      out.more();
      t->Var()->asXML(tvP, out);
      t->Core()->asXML(tvP, out);
      out << ((t->kind == ty_mbFull) ? "</MBPAIR>" : "</mbpair>") 
	  << endl;
      break;
    }

  case ty_mutable:
    {
      out << "<mutable>" << endl;
      out.more();
      t->Base()->asXML(tvP, out);
      out.less();       
      out << "</mutable>" << endl; 
      break;  
    }

  case ty_const:
    {
      out << "<const>" << endl;
      out.more();
      t->Base()->asXML(tvP, out);
      out.less();       
      out << "</const>" << endl; 
      break;  
    }

  case ty_pcst:
    {
      out << "<pcst>" << endl;
      out.more();
      t->CompType(0)->asXML(tvP, out);
      t->CompType(1)->asXML(tvP, out);
      out << "</pcst>" << endl; 
      for (size_t i=0; i<t->components.size(); i++)
	t->CompType(i)->asXML(tvP, out);
      break;
    }

  case ty_kfix:
    {
      if (t == Type::Kmono)
	out << "<lKind k='mono'/>" << endl; 
      else if (t == Type::Kpoly)
	out << "<lKind k='poly'/>" << endl; 
      else
	assert(false);
      break;
    }

  case ty_exn:
    {
      out << "<exception/>" << endl; 
      break;
    }
  }
  t->pMark--;
}


string
Type::asXML(shared_ptr<TvPrinter> tvP) 
{
  std::stringstream ss;      
  INOstream out(ss);
  asXML(tvP, out);
  return ss.str(); 
}

static inline bool
mustShowPred(shared_ptr<Typeclass> pred)
{
  if ((Options::showAllTccs) ||
     (((pred->flags & TY_CT_SUBSUMED) == 0) && 
      ((pred->flags & TY_CT_SELF) == 0)))
    return true;
  else
    return false;
}

void
TypeScheme::asXML(shared_ptr<TvPrinter> tvP, INOstream &out)
{
  normalize();
  shared_ptr<TCConstraints> _tcc = TCConstraints::make();
  out << "<TS>" << endl;
  out.more();
  
  for (TypeSet::iterator itr_i = ftvs.begin();
      itr_i != ftvs.end(); ++itr_i)
    out << (*itr_i)->asXML(tvP);      
  
  if (tcc) {    
    if (Options::showAllTccs)
      _tcc = tcc;
    else
      addConstraints(_tcc);      
  }

  out << "<CType>" << endl;
  out.more();
  if (_tcc->size()) {
    for (TypeSet::iterator itr = _tcc->begin();
	itr != _tcc->end(); ++itr)
      if (mustShowPred((*itr)))
	(*itr)->asXML(tvP);
  }
  
  tau->asXML(tvP, out);
  out.less();
  out << "</CType>" << endl;
  out.less();
  out << "</TS>" << endl;  
}

std::string
TypeScheme::asXML(shared_ptr<TvPrinter> tvP)
{
  normalize();
  std::stringstream ss; 
  INOstream out(ss);
  asXML(tvP, out);  
  return ss.str();
}

void 
Instance::asXML(INOstream &out)
{
  ts->asXML(GC_NULL, out);
}

std::string 
Instance::asXML()
{
  return ts->asXML();
}
