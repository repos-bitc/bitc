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

#include "Options.hxx"

using namespace std;
using namespace boost;
using namespace boost;
using namespace sherpa;

namespace Options {

  bool showParse = false;
  bool showTypes = false;
  bool xmlTypes = false;
  bool showLex = false;
  bool useStdInc = true;
  bool useStdLib = true;
  bool advisory = false;
  bool rawTvars = false;
  bool showMaybes = false;
  bool FQtypes = true;
  bool showAllTccs = false;
  bool showPasses = false;
  bool noPrelude = false; 
  bool dumpAfterMidEnd = false;
  bool dumpTypesAfterMidEnd = false;
  set<string> showTypesUocs;
  set<string> xmlTypesUocs;
  bool ppFQNS = false;
  bool ppDecorate = false;
  unsigned verbose = 0;
  set<string> entryPts;
  BackEnd *backEnd = 0;
  string outputFileName;
  vector<filesystem::path> libDirs;
  vector<filesystem::path> inputs;
  bool Wall = false;
  bool noGC = false;
  bool noAlloc = false;
  shared_ptr<TvPrinter> debugTvP = TvPrinter::make();
  bool heuristicInference = false;

  vector<string> LinkPreOptionsGCC;
  vector<string> CompilePreOptionsGCC;
  vector<string> LinkPostOptionsGCC;

  vector<filesystem::path> SystemDirs;

}
