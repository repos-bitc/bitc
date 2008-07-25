#ifndef SEXPRLEXER_HXX
#define SEXPRLEXER_HXX

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

#include <iostream>

#include "ParseType.hxx"

typedef long ucs4_t;

struct SexprLexer {
  sherpa::LexLoc here;
  int num_errors;
  bool debug;
  bool isRuntimeUoc;
  bool ifIdentMode;
  bool isCommandLineInput;
  std::istream& inStream;
  std::ostream& errStream;

  unsigned nModules;

  int nDigits;
  int radix;

  std::string thisToken;

  ucs4_t putbackChar;		// -1 or UCS4

  long digitValue(ucs4_t);

  ucs4_t getChar();
  void ungetChar(ucs4_t);

  SexprLexer(std::ostream& _err, std::istream& _in, 
	     const std::string& origin,
	     bool commandLineInput);

  void ReportParseError(const sherpa::LexLoc& where, std::string  /* msg */);
  void ReportParseWarning(const sherpa::LexLoc& where, std::string  /* msg */);

  void ReportParseError();
  void ReportParseError(std::string msg)
  {
    ReportParseError(here, msg);
  }
  inline void ReportParseWarning(std::string msg)
  {
    ReportParseWarning(here, msg);
  }

  inline void setDebug(bool showlex)
  {
    debug = (showlex ? true : false);
  }

  inline void setIfIdentMode(bool arg)
  {
    ifIdentMode = arg;
  }

  int lex(ParseType *yylvalp);

  int kwCheck(const char *s);

  ~SexprLexer() {}
};

#endif /* SEXPRLEXER_HXX */

