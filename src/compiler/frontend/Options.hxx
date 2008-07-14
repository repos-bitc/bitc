#ifndef OPTIONS_HXX
#define OPTIONS_HXX

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

#include <dirent.h>
#include <libsherpa/Path.hxx>
#include <libsherpa/CVector.hxx>
#include "backend.hxx"
#include "Special.hxx"
#include "TvPrinter.hxx"

/* Flags set from command line option */
struct Options {
  static bool showParse;
  static bool showLex;
  static bool showTypes;
  static bool xmlTypes;
  static bool showPP;
  static bool useStdInc;
  static bool useStdLib;
  static bool advisory;
  static bool rawTvars;
  static bool showMaybes;
  static bool FQtypes;
  static bool showAllTccs;
  static bool showPasses;
  static bool ppFQNS;
  static bool ppDecorate;
  /** @brief Suppress load of the prelude.
   *
   * This is an internal testing option to suppress loading the
   * prelude while working on incomplete parts of the compiler.  This
   * option should not be exposed to users.
   */
  static bool noPrelude;
  static bool dumpAfterMidEnd;
  static bool dumpTypesAfterMidEnd;
  static GCPtr<CVector<std::string> > showTypesUocs;
  static GCPtr<CVector<std::string> > xmlTypesUocs;
  static GCPtr<CVector<std::string> > entryPts;
  static BackEnd *backEnd;
  static std::string outputFileName;
  static GCPtr<CVector<GCPtr<Path> > > libDirs;
  static GCPtr<CVector<std::string> > inputs;
  static bool Wall; // All Warnings are errors.
  static bool nogc; // no garbage collection mode
  static GCPtr<TvPrinter> debugTvP;
  static bool heuristicInference;
};

#endif /* OPTIONS_HXX */
