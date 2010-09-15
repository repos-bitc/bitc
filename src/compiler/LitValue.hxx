#ifndef LITVALUE_HXX
#define LITVALUE_HXX

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

#include <inttypes.h>
#include <string>
#include <libsherpa/BigNum.hxx>

#include "ucs.hxx"

enum LiteralType {
  lt_none,
  lt_bool,
  lt_char,
  lt_int,
  lt_float,
  lt_string
};

struct LitValue {
  LiteralType litType;

  bool   b;                        /* boolean Values */
  unsigned long c;                 /* utf32 code points */
  sherpa::BigNum i;                /* large precision integers */
  double d;                        /* doubles, floats          */

  static bool valid_char_printable(ucs4_t ucs4);
  static bool valid_charpoint(ucs4_t ucs4);
  static bool valid_charpunct(ucs4_t ucs4);
  static unsigned validate_string(const char *s);

  /** @brief If @p c is a digit character in radix @p radix, return
   * its decimal value */
  static long digitValue(ucs4_t, unsigned radix);

  // FIX: (shap) the original input is being saved in AST.s for replay
  // purposes. String literals need to be stored here as a vector of
  // character representations.
  std::string s;                /* String Literals          */

  struct EscapedLiteral {
    const char *s;
    ucs4_t codePoint;
  };

  static EscapedLiteral EscapedLiteralMap[];
  static const size_t EscapedLiteralMapLength;
  static ucs4_t GetEscapedCodePoint(const char *escapedLiteral);

  static ucs4_t DecodeNumericCharacter(const char *s, const char **next);
  static ucs4_t DecodeBlockCharacter(const char *s);
  static ucs4_t DecodeStringCharacter(const char *s, const char **next);
  static ucs4_t DecodeCharacter(const std::string&);

  std::string asString() const;

  LitValue() {
    litType = lt_none;
  }
};

inline
std::ostream& operator<<(std::ostream& strm, const LitValue& lv)
{
  strm << lv.asString();
  return strm;
}

#endif /* LITVALUE_HXX */
