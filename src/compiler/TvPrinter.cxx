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
#include <libsherpa/UExcept.hxx>
#include "AST.hxx"
#include "Type.hxx"
#include <sstream>

using namespace std;
using namespace sherpa;

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
  unsigned long long nchars = 'z' - 'a' + 1;

  char c = 'a' + (count % nchars);
  ss << pfx << c;

  unsigned long long ndx = count / nchars;
  if(ndx > 0)
    ss << ndx;
  
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
  
  TvMap::iterator itr = tvMap.find(tvar->uniqueID);

  if (itr == tvMap.end()) {
    string s = newTvName();
    tvMap[tvar->uniqueID] = s;
    return s;
  }
  else
    return itr->second;
}

std::vector<std::string>
TvPrinter::getAllTvarStrings()
{
  std::vector<std::string> vec;

  for(TvMap::iterator itr = tvMap.begin();
      itr != tvMap.end(); ++itr)
    vec.push_back(itr->second);

  return vec;
}
