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

/// @file
///
/// @brief Back end to emit BitC "object" files.
///
/// Calling this a back end is moderately laughable. All it does is
/// pretty-print the original UoC.

#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include "Version.hxx"
#include "Options.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"

using namespace boost;
using namespace sherpa;

bool
EmitBitO(std::ostream &optStream, std::ostream &errStream)
{
  std::ofstream out(Options::outputFileName.c_str(),
		    std::ios_base::out|std::ios_base::trunc);

  if (!out.is_open()) 
    errStream << "Couldn't open output file \""
	      << Options::outputFileName
	      << "\" -- "
	      << strerror(errno)
	      << std::endl;


  out << "(bitc-version \"" << BITC_VERSION << "\")" << std::endl;

  for (UocMap::iterator itr = UocInfo::srcList.begin();
      itr != UocInfo::srcList.end(); ++itr) {
    shared_ptr<UocInfo> uoc = itr->second;

    uoc->PrettyPrint(out, false);
  }

  return true;
}

