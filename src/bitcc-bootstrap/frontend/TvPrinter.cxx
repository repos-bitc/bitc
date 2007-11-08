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
#include "AST.hxx"
#include "Type.hxx"
#include <sstream>

using namespace sherpa;
using namespace std;

TvPrinter::TvPrinter(const bool pp, const std::string& _pfx)
{
  prettyPrint = pp;
  pfx = _pfx;
  count = 0;
}

string 
TvPrinter::newTvName()
{
  stringstream ss;
  
  if(count <= (unsigned long long) ('z' - 'a')) {
    // Wierd: ss << ('a' + count) did do the right thing !?
    char c = 'a' + count;
    ss << pfx << c;
  }
  else {
    ss << pfx << "a" << count;
  }

  count++;
  return ss.str();
}
  
string
TvPrinter::tvName(GCPtr<const Type> tvar)
{
  if(!prettyPrint) {
    stringstream ss;
    ss << pfx << "a" << tvar->uniqueID;
    return ss.str();
  }
  
  GCPtr< AvlMapNode<unsigned long long, string> > tvMapNode;
  tvMapNode = tvMap.lookup(tvar->uniqueID);

  if(!tvMapNode) {
    string s = newTvName();
    tvMap.insert(tvar->uniqueID, s);
    return s;
  }
  else
    return tvMapNode->value;      
};


bool
iterateOverTvMapS(GCPtr<AvlMapNode<unsigned long long, string> >node,
		  const void *aux)
{ 
  CVector<std::string> *vec = (CVector<std::string> *) aux;
  vec->append(node->value);  
  return true;
}

GCPtr<CVector<std::string> >
TvPrinter::getAllTvarStrings()
{
  CVector<std::string> *vec = new CVector<std::string>;
  GCPtr<CVector<std::string> > gcVec = vec;
  tvMap.iterate(iterateOverTvMapS, vec);
  return gcVec;
}
