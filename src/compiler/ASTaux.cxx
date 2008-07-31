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

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"
#include "FQName.hxx"

using namespace sherpa;

GCPtr<Type> 
AST::getType()
{
  return symType->getType();
}

GCPtr <const Type> 
AST::getType() const
{
  return symType->getType();
}

GCPtr<AST> 
AST::genIdent(const char *label, const bool isTV)
{
  GCPtr<AST> id = AST::make(at_ident);

  std::stringstream ss;
  if(isTV)
    ss << "'__";
  else
    ss << "__";
  ss << label << id->ID;

  id->s = ss.str();

  return id;
}

GCPtr<AST> 
AST::genSym(GCPtr<AST> ast, const char *label, const bool isTV)
{
  GCPtr<AST> id = genIdent(label, isTV);
  // FQN to be set by the next call to the ersolver 

  id->identType = ast->identType;
  id->Flags = ast->Flags | ID_IS_GENSYM;
  id->symType = ast->symType;
  id->scheme = ast->scheme;

  return id;
}

std::string
AST::asString() const
{
  std::stringstream ss;
  PrettyPrint(ss, false, false);
  return ss.str();
}

std::string
AST::mangledString() const
{
  std::stringstream ss;
  if(astType == at_ident)
    ss << "_" << s.size() << s;
  else
    ss << "_##" << astTypeName() << "##";
  return ss.str();
}

bool
AST::isFnxn()
{
  return (symType->getBareType()->kind == ty_fn);
}

size_t
AST::nBits()
{
  if(field_bits != 0)
    return field_bits;
  else    
    return tagType->nBits();
}


GCPtr<AST> 
AST::Use()
{
  assert(astType == at_ident);
  assert(!symbolDef || (Flags & ID_IS_TVAR));
  GCPtr<AST> idUse = getDCopy();
  idUse->Flags  &= ~MASK_FLAGS_FROM_USE;
  idUse->Flags2 &= ~MASK_FLAGS2_FROM_USE;    
  idUse->symbolDef = shared_from_this();
  if(symType)
    idUse->symType = symType->getDCopy();
  return idUse;
}


AST::AST(GCPtr<AST> ast, bool shallowCopyChildren)
{
  astType = ast->astType;
  ID = ++(AST::astCount);  
  identType = ast->identType;
  s = ast->s;
  loc = ast->loc;
  fqn = ast->fqn;
  Flags = ast->Flags;
  Flags2 = ast->Flags2;
  externalName = ast->externalName;
  symbolDef = ast->symbolDef;
  defn = ast->defn;
  decl = ast->decl;
  symType = ast->symType;
  scheme = ast->scheme;
  envs = ast->envs;
  polyinst = ast->polyinst;
  reached = ast->reached;
  defForm = ast->defForm;
  defbps = ast->defbps;
  litValue = ast->litValue;
  litBase = ast->litBase;
  isDecl = ast->isDecl;
  ifName = ast->ifName;
  printVariant = ast->printVariant;
  tagType = ast->tagType;
  field_bits = ast->field_bits;
  unin_discm = ast->unin_discm;
  total_fill = ast->total_fill;

  if(shallowCopyChildren)
    children = ast->children;
}

 
GCPtr<AST> 
AST::getTrueCopy()
{  
  GCPtr<AST> to = AST::make(shared_from_this(), false);
  
  for(size_t i=0; i < children.size(); i++)
    to->children.push_back(child(i)->getTrueCopy());
  
  return to;
}

GCPtr<AST> 
AST::getDCopy()
{  
  GCPtr<AST> to = AST::make(shared_from_this(), false);
  to->symbolDef = GC_NULL;
  to->defn = GC_NULL;
  to->decl = GC_NULL;
  to->symType = GC_NULL;
  to->scheme = GC_NULL;
  to->envs = envs;
  to->polyinst = false;
  to->reached = false;
  to->defForm = GC_NULL;
  to->defbps = GC_NULL;

  for(size_t i=0; i<children.size(); i++)
    to->children.push_back(child(i)->getDCopy());
  return to;
}

/* */

void
AST::set(GCPtr<AST> ast)
{  
  astType = ast->astType;
  s = ast->s;
  loc = ast->loc;
  identType = ast->identType;
  Flags = ast->Flags;
  Flags2 = ast->Flags2;
  externalName = ast->externalName;
  symbolDef = ast->symbolDef;
  defn = ast->defn;
  decl = ast->decl;
  symType = ast->symType;
  scheme = ast->scheme;
  envs = ast->envs;
  polyinst = ast->polyinst;
  reached = ast->reached;
  defForm = ast->defForm;
  defbps = ast->defbps;
  litValue = ast->litValue;
  litBase = ast->litBase;
  isDecl = ast->isDecl;
  fqn = ast->fqn;
  ifName = ast->ifName;
  field_bits = ast->field_bits;
  unin_discm = ast->unin_discm;
  total_fill = ast->total_fill;

  children = ast->children;
}
