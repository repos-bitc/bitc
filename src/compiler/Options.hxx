#ifndef OPTIONS_HXX
#define OPTIONS_HXX

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

#include <set>
#include <boost/filesystem/path.hpp>
#include "backend.hxx"
#include "FQName.hxx"
#include "Special.hxx"
#include "TvPrinter.hxx"

/* Flags set from command line option */
namespace Options {
  extern bool showParse;
  extern bool showLex;
  extern bool showTypes;
  extern bool xmlTypes;
  extern bool showPP;
  extern bool useStdInc;
  extern bool useStdLib;
  extern bool advisory;
  extern bool rawTvars;
  extern bool showMaybes;
  extern bool FQtypes;
  extern bool showAllTccs;
  extern bool showPasses;
  extern bool ppFQNS;
  extern bool ppDecorate;
  extern unsigned verbose;	// 0 = no, 1 = show exec, 2 = forward

  /// @brief Suppress load of the prelude.
  ///
  /// This is an internal testing option to suppress loading the
  /// prelude while working on incomplete parts of the compiler.  This
  /// option should not be exposed to users.
  ///
  extern bool noPrelude;
  extern bool dumpAfterMidEnd;
  extern bool dumpTypesAfterMidEnd;
  extern std::set<std::string> showTypesUocs;
  extern std::set<std::string> xmlTypesUocs;
  extern std::set<FQName> entryPts;
  extern BackEnd *backEnd;
  extern std::string outputFileName;
  extern std::vector<boost::filesystem::path> libDirs;
  extern std::vector<boost::filesystem::path> inputs;

  /// @brief Options and files that should be passed to GCC @em before
  /// we insert the C file produced by bitcc.
  extern std::vector<std::string> LinkPreOptionsGCC;
  extern std::vector<std::string> CompilePreOptionsGCC;

  /// @brief Options and files that should be passed to GCC @em after
  /// we insert the C file produced by bitcc.
  extern std::vector<std::string> LinkPostOptionsGCC;

  extern std::vector<boost::filesystem::path> SystemDirs;

  extern bool Wall; // All Warnings are errors.
  extern bool noGC; // no garbage collection mode
  extern bool noAlloc; // statically reject heap-allocating constructs
  extern bool heuristicInference;

  /// @brief TvPrinter to support debugging output.
  ///
  /// This is not actually an option, but it used by some options. The
  /// purpose of this TvPrinter is to provide a globally consistent
  /// mapping from type variables to names. Through this mapping, it
  /// becomes possible for several passes to print their type variable
  /// names consistently, which makes understanding the transformation
  /// that was performed easier.
  extern boost::shared_ptr<TvPrinter> debugTvP;
} /* namespace Options */;

#endif /* OPTIONS_HXX */
