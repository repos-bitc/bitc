#ifndef BITCLEXER_HXX
#define BITCLEXER_HXX

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

#include <libsherpa/Path.hxx>
#include "ParseType.hxx"

/* We need to include this file into the lex file so that the class
 * declaration for IdlLexer2 is available. In all other inclusions we
 * are going to need FlexLexer.h, but if we include that in the lexer
 * we will get a redundantly defined lexer class.
 *
 * It proves that FlexLexer.h is included into the lex.l output after
 * YYSTATE is defined, so use the presence or absence of YYSTATE to
 * decide whether to include FlexLexer.h.
 */
#ifndef YYSTATE

#undef yyFlexLexer
#define yyFlexLexer bitcFlexLexer
#include <FlexLexer.h>

#endif /* YYSTATE */

struct BitcLexer : bitcFlexLexer {
  sherpa::LexLoc here;
  int num_errors;
  bool isRuntimeUoc;
  bool ifIdentMode;
  std::ostream& errStream;

  BitcLexer(std::ostream& _err, std::istream& in, 
	    sherpa::Path *inputPath);

  void ReportParseError();
  void ReportParseError(std::string  /* msg */);
  void ReportParseWarning(std::string  /* msg */);
  void setDebug(bool);

  inline void setIfIdentMode(bool arg)
  {
    ifIdentMode = arg;
  }

  int lex(ParseType *yylvalp);

  int kwCheck(const char *s);

  ~BitcLexer() {}
};

#endif /* BITCLEXER_HXX */

