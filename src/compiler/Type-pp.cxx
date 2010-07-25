/**************************************************************************
 *
 * Copyright (C) 2008, Johns Hopkins University.
 * Copyright (C) 2010, Jonathan S. Shapiro.
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

//#define DEBUG_SHOW_ALL_LINKS
//#define DEBUG_SHOW_RIGIDITY

static string
printName(shared_ptr<AST> ast)
{
  if (!ast)
    return "NULL";
  
  return ast->s;
}

string
Type::asSexprString(shared_ptr<TvPrinter> tvP, PrintOptions options) 
{ 
  stringstream ss;
  
  if (options & PO_SHOW_LINKS)
    options |= PO_NO_TRAVERSE;

  if (Options::rawTvars)
    tvP = GC_NULL;

  shared_ptr<Type> t = 
    (options & PO_NO_TRAVERSE) ? shared_from_this() : getType();
  
  // Bound the display recursion artificially. there really needs to be a
  // better way to do this, but I don't have a quick solution, and
  // this choice has worked well in practice so far.
  if (t->pMark >= 2)
    return " ... ";
  else 
    t->pMark++;

  // Debugging support for type variable linkage display:
  if (options & PO_SHOW_LINKS) {
    shared_ptr<Type> t1 = shared_from_this();
    ss << "[";
    while (t1->link) {
      ss << "'a" << t1->uniqueID;
      ss << "->";
      t1 = t1->link;
    }
    ss << "'a" << t1->uniqueID;
    ss << "]";
  }
    
  switch(t->kind) {
  case ty_tvar:
    if (!tvP) {
      ss << "'a" << t->uniqueID;
      if (t->flags & TY_RIGID) 
        ss << 'R';
    }
    else {
      ss << tvP->tvName(t);
    }
      
    break;

  case ty_kvar:
    ss << "'K" << t->uniqueID;
    break;

  case ty_dummy:
      ss << "#DUMMY#";
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
    ss << t->kindName();
    break;

  case ty_field:
    ss << t->litValue.s;
    break;

#ifdef KEEP_BF
  case  ty_bitfield:
    ss << "(bitfield "
       << t->CompType(0)->toString()
       << " "
       << t->Isize
       << ")";
    break;
#endif

  case ty_method:
    assert(t->components.size() == 2);
    ss << "(method " << t->Args()->asSexprString(tvP, options) 
       << " -> " << t->Ret()->asSexprString(tvP, options) << ")";
    break;

  case ty_fn:
    assert(t->components.size() == 2);
    ss << "(fn " << t->Args()->asSexprString(tvP, options) 
       << " -> " << t->Ret()->asSexprString(tvP, options) << ")";
    break;

  case ty_fnarg:
    for (size_t i=0; i < t->components.size(); i++) {
      if (i > 0) ss << " ";
      if (t->CompFlags(i) & COMP_BYREF)
        ss << "(by-ref " << t->CompType(i)->asSexprString(tvP, options)
           << ")";
      else           
        ss << t->CompType(i)->asSexprString(tvP, options);
    }
    break;

  case ty_tyfn:
    assert(t->components.size() == 2);
    ss << "(tyfn " <<  t->Args()->asSexprString(tvP, options) 
       << " -> " << t->Ret()->asSexprString(tvP, options) << ")";
    break;          

  case ty_letGather:
    ss <<  "(__letGather ";
    for (size_t i=0; i < t->components.size(); i++) {
      if (i > 0) ss << " ";
      ss << t->CompType(i)->asSexprString(tvP, options);
    }
    ss << ")";

    break;

  case ty_structv:
  case ty_structr:
    {
      // The debug version dumps the structure body in detail. These
      // should be reconciled more cleanly when we switch the type
      // syntax over.
      if (options & PO_SHOW_FIELDS) {
        ss << "(" << ((t->kind == ty_structr) ? "struct " : "structR ")
           << printName(t->defAst) << " - ";
        for (size_t i=0; i<components.size(); i++)
          ss << CompName(i) << ":" 
             << CompType(i)->asSexprString(tvP, options) << " ";
        ss << ")";
      }
      else {
        if (t->typeArgs.size() == 0)
          ss << printName(t->defAst);
        else {
          ss << "(" << printName(t->defAst);
          for (size_t i=0; i < t->typeArgs.size(); i++)
            ss << " " << t->TypeArg(i)->asSexprString(tvP, options);
          ss << ")";
        }
      }
      
      break;
    }

  case ty_unionv: 
  case ty_unionr:
  case ty_uvalv: 
  case ty_uvalr:
  case ty_uconv: 
  case ty_uconr:
    // The debug version dumps the structure body in detail. These
    // should be reconciled more cleanly when we switch the type
    // syntax over.
    if (options & PO_SHOW_FIELDS) {
      const char *dbName;
      switch(t->kind) {
      case ty_unionv: 
        dbName = "union";
        break;
      case ty_unionr:
        dbName = "unionR";
        break;
      case ty_uvalv: 
        dbName = "union-val";
        break;
      case ty_uvalr:
        dbName = "unionR-val";
        break;
      case ty_uconv: 
        dbName = "union-con";
        break;
      case ty_uconr:
        dbName = "unionR-con";
        break;
      }
      ss << "(" << dbName << " " << printName(t->defAst);
      for (size_t i=0; i<typeArgs.size(); i++)
        ss << TypeArg(i)->getType()->asSexprString(tvP, options);
      ss << ") [";
      for (size_t i=0; i<components.size(); i++)
        ss << CompName(i) << ":" 
           << CompType(i)->getType()->asSexprString(tvP, options);
      ss << "]";
    }
    else {
      if (t->typeArgs.size() == 0)
        ss << printName(t->myContainer);
      else {
        ss << "(" << printName(t->myContainer);
        for (size_t i=0; i < t->typeArgs.size(); i++)
          ss << " " << t->TypeArg(i)->asSexprString(tvP, options);
        ss << ")";
      }
    }
      
    break;

  case ty_typeclass:    
    // The debug version dumps the structure body in detail. These
    // should be reconciled more cleanly when we switch the type
    // syntax over.
    if (options & PO_SHOW_FIELDS) {
      ss << "(Typeclass " << printName(t->defAst);
      for (size_t i=0; i < typeArgs.size(); i++)
        ss << " " << TypeArg(i)->asSexprString(tvP, options);
      ss << ")";
    }
    else {
      if (t->typeArgs.size() == 0)
        ss << printName(t->defAst);
      else {
        ss << "(" << printName(t->defAst);
        for (size_t i=0; i < t->typeArgs.size(); i++)
          ss << " " << t->TypeArg(i)->asSexprString(tvP, options);
        ss << ")";
      }
    }
    break;

  case ty_array:
    ss << "(array " << t->Base()->asSexprString(tvP, options)
       << " " << t->arrLen->len << ")";
    break;

  case ty_vector:
    ss << "(vector " << t->Base()->asSexprString(tvP, options) <<  ")";
    break;
    
  case ty_ref:
    ss << "(ref " << t->Base()->asSexprString(tvP, options) << ")";
    break;

  case ty_byref:
    ss << "(by-ref " << t->Base()->asSexprString(tvP, options) << ")";
    break;

  case ty_array_ref:
    ss << "(array-ref " << t->Base()->asSexprString(tvP, options) << ")";
    break;

 case ty_mbFull:
 case ty_mbTop:
   ss << t->Var()->asSexprString(tvP, options)
      << ((t->kind == ty_mbFull) ? "|" : "!")
      << ((t->Core()->kind == ty_fn)?"(":"")
      << t->Core()->asSexprString(tvP, options)
      << ((t->Core()->kind == ty_fn)?")":"");
   break;

  case ty_mutable:
    ss << "(mutable " << t->Base()->asSexprString(tvP, options) << ")";
    break;  

  case ty_const:
    ss << "(const " << t->Base()->asSexprString(tvP, options) << ")";
    break;

  case ty_exn:
    ss << "exception"; 
    break;

  case ty_pcst:
    {
      ss << "*(";
      for (size_t i=0; i<t->components.size(); i++) {
        if (i > 0)
          ss << ", ";
        ss << t->CompType(i)->asSexprString(tvP, options);
      }
      ss << ")";
      break;
    }

  case ty_kfix:
    {
      if (t == Type::Kmono)
        ss << "m";
      else if (t == Type::Kpoly)
        ss << "P";
      else
        assert(false);
      break;
    }

  }
  
  t->pMark--;
  return ss.str();
}

string
Type::asBlockString(shared_ptr<TvPrinter> tvP, PrintOptions options)
{
  return asBlockStringProducer(tvP, options, false);
}

string
Type::asString(shared_ptr<TvPrinter> tvP, PrintOptions options)
{
  return asBlockString(tvP, options) + " OR " + asSexprString(tvP, options);
}

// Return the precedence (in the grammar's view) of the production
// that produces a given type.
static int typePrecedence(Kind k)
{
  switch(k) {
  case ty_fn:
  case ty_tyfn:
  case ty_method:
    return 3;

  case ty_array:
  case ty_vector:
  case ty_ref:
    return 2;

  case ty_mutable:
  case ty_const:
    return 1;

  default:
    return 0;
  }
}

static bool shouldParenWrap(Kind parent, Kind child)
{
  return typePrecedence(parent) < typePrecedence(child);
  // return true;
}

string
Type::asBlockStringProducer(shared_ptr<TvPrinter> tvP, PrintOptions options, 
                       bool parenWrap) 
{ 
  stringstream ss;
  
  if (options & PO_SHOW_LINKS)
    options |= PO_NO_TRAVERSE;

  if (Options::rawTvars)
    tvP = GC_NULL;

  shared_ptr<Type> t = 
    (options & PO_NO_TRAVERSE) ? shared_from_this() : getType();
  
  // Bound the display recursion artificially. there really needs to be a
  // better way to do this, but I don't have a quick solution, and
  // this choice has worked well in practice so far.
  if (t->pMark >= 2)
    return " ... ";
  else 
    t->pMark++;

  if (parenWrap) ss << '(';

  // Debugging support for type variable linkage display:
  if (options & PO_SHOW_LINKS) {
    shared_ptr<Type> t1 = shared_from_this();
    ss << "[";
    while (t1->link) {
      ss << "'a" << t1->uniqueID;
      ss << "->";
      t1 = t1->link;
    }
    ss << "'a" << t1->uniqueID;
    ss << "]";
  }
    
  switch(t->kind) {
  case ty_tvar:
    if (!tvP) {
      ss << "'a" << t->uniqueID;
      if (t->flags & TY_RIGID) 
        ss << 'R';
    }
    else {
      ss << tvP->tvName(t);
    }
      
    break;

  case ty_kvar:
    ss << "'K" << t->uniqueID;
    break;

  case ty_dummy:
    ss << "#DUMMY#";
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
    ss << t->kindName();
    break;

  case ty_field:
    ss << t->litValue.s;
    break;

#ifdef KEEP_BF
  case  ty_bitfield:
    ss << t->CompType(0)->toString()
       << "("
       << t->Isize
       << ")";
    break;
#endif

  case ty_method:
    assert(t->components.size() == 2);
    ss << "method (" << t->Args()->asBlockStringProducer(tvP, options, false) 
       << ") -> " << t->Ret()->asBlockStringProducer(tvP, options, false);
    break;

  case ty_fn:
    // No paren wrapping here. While the return value is not fully
    // bracketed by ty_fn itself, the bracketing decision must be made
    // by our container.
    assert(t->components.size() == 2);
    ss << "fn (" << t->Args()->asBlockStringProducer(tvP, options, false) 
       << ") -> " << t->Ret()->asBlockStringProducer(tvP, options, false);
    break;

  case ty_fnarg:
    // No paren wrapping here. Function arguments are fully bracketed,
    // and ByRef is always outermost and lowest precedence.
    for (size_t i=0; i < t->components.size(); i++) {
      if (i > 0) ss << ", ";
      if (t->CompFlags(i) & COMP_BYREF)
        ss << "ByRef " << t->CompType(i)->asBlockStringProducer(tvP, options, false)
           << "";
      else           
        ss << t->CompType(i)->asBlockStringProducer(tvP, options, false);
    }
    break;

  case ty_tyfn:
    // No paren wrapping here. Same argument as for ty_fn
    assert(t->components.size() == 2);
    ss << "tyfn (" <<  t->Args()->asBlockStringProducer(tvP, options, false)
       << ", -> " << t->Ret()->asBlockStringProducer(tvP, options, false);
    break;          

  case ty_letGather:
    //HERE
    ss <<  "(__letGather ";
    for (size_t i=0; i < t->components.size(); i++) {
      shared_ptr<Type> compType = t->CompType(i);

      if (i > 0) ss << " ";
      ss << compType->asBlockStringProducer(tvP, options, shouldParenWrap(t->kind, compType->kind));

    }
    ss << ")";

    break;

  case ty_structv:
  case ty_structr:
    {
      // No paren wrapping here. Everything here is fully bracketed,
      // and [type] application has about the highest precedence of anything.
      ss << printName(t->defAst);
      if (t->kind == ty_structr) ss << '^';

      if (t->typeArgs.size() > 0) {
        ss << '(';
        for (size_t i=0; i < t->typeArgs.size(); i++) {
          if (i > 0) ss << ", ";
          ss << t->TypeArg(i)->asBlockStringProducer(tvP, options, false);
        }
        ss << ')';
      }
        
      if (options & PO_SHOW_FIELDS) {
        ss << "{ ";
        for (size_t i=0; i<components.size(); i++)
          ss << CompName(i) << ":" 
             << CompType(i)->asBlockStringProducer(tvP, options, false) << "; ";
        ss << "}";
      }
      
      break;
    }

  case ty_unionv: 
  case ty_unionr:
  case ty_uvalv: 
  case ty_uvalr:
  case ty_uconv: 
  case ty_uconr:
    {
      // No paren wrapping here. Everything here is fully bracketed,
      // and [type] application has about the highest precedence of anything.
      ss << printName(t->myContainer);
      if ((t->kind == ty_unionr) ||
          (t->kind == ty_uvalr) ||
          (t->kind == ty_uconr)) 
        ss << '^';

      if (t->typeArgs.size() > 0) {
        ss << '(';
        for (size_t i=0; i < t->typeArgs.size(); i++) {
          if (i > 0) ss << ", ";
          ss << t->TypeArg(i)->asBlockStringProducer(tvP, options, false);
        }
        ss << ")";
      }

      if (options & PO_SHOW_FIELDS) {
        const char *dbName;
        switch(t->kind) {
        case ty_unionv: 
          dbName = "union";
          break;
        case ty_unionr:
          dbName = "union^";
          break;
        case ty_uvalv: 
          dbName = "union-val";
          break;
        case ty_uvalr:
          dbName = "union^-val";
          break;
        case ty_uconv: 
          dbName = "union-con";
          break;
        case ty_uconr:
          dbName = "union^-con";
          break;
        }

        ss << dbName << "{ ";
        for (size_t i=0; i<components.size(); i++)
          ss << CompName(i) << ":" 
             << CompType(i)->getType()->asBlockStringProducer(tvP, options, false)
             << "; ";
        ss << "}";
      }
      break;
    }
  case ty_typeclass:    
    {
      // No paren wrapping here. Everything here is fully bracketed,
      // and [type] application has about the highest precedence of anything.
      ss << printName(t->defAst);
      if (t->typeArgs.size() != 0) {
        ss << "(";
        for (size_t i=0; i < t->typeArgs.size(); i++) {
          if (i > 0) ss << ", ";
          ss << t->TypeArg(i)->asBlockStringProducer(tvP, options, false);
        }
        ss << ")";
      }

      break;
    }

  case ty_array:
    {
      shared_ptr<Type> base = t->Base();
      bool wrap = shouldParenWrap(t->kind, base->kind);

      ss << base->asBlockStringProducer(tvP, options, wrap);
      ss << '[' << t->arrLen->len << "]";
      break;
    }

  case ty_vector:
    {
      shared_ptr<Type> base = t->Base();
      bool wrap = shouldParenWrap(t->kind, base->kind);

      ss << base->asBlockStringProducer(tvP, options, wrap);
      ss <<  "[]";
      break;
    }

  case ty_ref:
    {
      shared_ptr<Type> base = t->Base();
      bool wrap = shouldParenWrap(t->kind, base->kind);

      ss << base->asBlockStringProducer(tvP, options, wrap);
      ss << " reference";
      break;
    }
    break;

  case ty_byref:
    // Always outermost, so never needs paren wrapping
    ss << "ByRef " << t->Base()->asBlockStringProducer(tvP, options, false);
    break;

  case ty_array_ref:
    // Always outermost, so never needs paren wrapping
    ss << "ArrayRef " << t->Base()->asBlockStringProducer(tvP, options, false);
    break;

  case ty_mbFull:
  case ty_mbTop:
    ss << t->Var()->asBlockStringProducer(tvP, options, false)
       << ((t->kind == ty_mbFull) ? "|" : "!")
       << ((t->Core()->kind == ty_fn)?"(":"")
       << t->Core()->asBlockStringProducer(tvP, options, false)
       << ((t->Core()->kind == ty_fn)?")":"");
    break;

  case ty_mutable:
    {
      shared_ptr<Type> base = t->Base();
      bool wrap = shouldParenWrap(t->kind, base->kind);

      ss << "mutable "
         << base->asBlockStringProducer(tvP, options, wrap);
      break;
    }

  case ty_const:
    {
      shared_ptr<Type> base = t->Base();
      bool wrap = shouldParenWrap(t->kind, base->kind);

      ss << "const "
         << base->asBlockStringProducer(tvP, options, wrap);
      break;
    }

  case ty_exn:
    ss << "exception"; 
    break;

  case ty_pcst:
    {
      ss << "*(";
      for (size_t i=0; i<t->components.size(); i++) {
        if (i > 0) ss << ", ";
        ss << t->CompType(i)->asBlockStringProducer(tvP, options, true);
      }
      ss << ")";
      break;
    }

  case ty_kfix:
    {
      if (t == Type::Kmono)
        ss << "m";
      else if (t == Type::Kpoly)
        ss << "P";
      else
        assert(false);
      break;
    }

  }
  
  t->pMark--;

  if (parenWrap) ss << ')';

  return ss.str();
}

std::string
TypeScheme::asString(shared_ptr<TvPrinter> tvP, bool norm)
{
  std::stringstream ss; 
  bool forall = false;

  if (norm)
    normalize();
  
  if (Options::FQtypes)
    if (ftvs.size()) {
      ss << "(forall";
      forall = true;
      for (TypeSet::iterator itr_i = ftvs.begin(); 
          itr_i != ftvs.end(); ++itr_i)
        ss << " " << (*itr_i)->asString(tvP);      
      ss << " ";
    }

  if (tcc) {
    if (Options::showAllTccs) {
      if (tcc->size()) {
        if (!forall) {
          ss << "(forall";
          forall = true;
        }

        ss << " (";
        for (TypeSet::iterator itr = tcc->begin();
            itr != tcc->end(); ++itr) {
          shared_ptr<Typeclass> pred = (*itr)->getType();
          ss << pred->asString(tvP) << " ";
          
          for (TypeSet::iterator itr_j = pred->fnDeps.begin();
              itr_j != pred->fnDeps.end(); ++itr_j)
            ss << (*itr_j)->asString(tvP) << " ";
        }
        ss << ") ";
      }
    }
    else { 
      //shared_ptr<TCConstraints> _tcc = new TCConstraints;
      //addConstraints(_tcc);
      shared_ptr<TCConstraints> _tcc = tcc;

      if (_tcc->size()) {
        for (TypeSet::iterator itr = _tcc->begin();
            itr != _tcc->end(); ++itr) {
          if ((((*itr)->flags & TY_CT_SUBSUMED) == 0) && 
             (((*itr)->flags & TY_CT_SELF) == 0)) {
            if (!forall) {
              ss << "(forall (";
              forall = true;
            }
            ss << (*itr)->asString(tvP) << " ";          
          }
        }

        if (forall)
          ss << ") ";
      }
    }
  }
  
  ss << tau->asString(tvP);
  if (forall)
    ss << ")";
  
  return ss.str();
}

std::string 
Instance::asString()
{
  return ts->tau->asString();
}

