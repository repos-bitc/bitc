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
#include <string.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include <unicode/uchar.h>

#include <libsherpa/utf8.hxx>

#include "LitValue.hxx"

#define DEBUG_DECODE false

long 
LitValue::digitValue(ucs4_t ucs4, unsigned radix)
{
  long l = -1;

  if (ucs4 >= '0' && ucs4 <= '9')
    l = ucs4 - '0';
  if (ucs4 >= 'a' && ucs4 <= 'f')
    l = ucs4 - 'a' + 10;
  if (ucs4 >= 'A' && ucs4 <= 'F')
    l = ucs4 - 'A' + 10;

  if (l > radix)
    l = -1;
  return l;
}

bool
LitValue::valid_char_printable(ucs4_t ucs4)
{
  switch (ucs4) {
  case '_':
    return true;

    // This should match the list TransitionLexer::valid_ident_punct():
  case '!':
  case '$':
  case '%':
  case '&':
  case '|':
  case '*':
  case '+':
  case '-':
  case '/':
  case '<':
  case '>':
  case '=':
  case '?':
  case '@':
  case '~':
    return true;

    // Other characters that can appear "naked" in a character constant:
  case '^':
  case '.':
  case ',':
  case ':':
  case ';':
  case '[':
  case ']':
  case '\'':
  case '#':
  case '`':
  case '(':
  case ')':
    return true;
  default:
    return false;
  }
}

bool
LitValue::valid_charpoint(ucs4_t ucs4)
{
  if (valid_char_printable(ucs4))
    return true;

  return u_isgraph(ucs4);
}

bool
LitValue::valid_charpunct(ucs4_t ucs4)
{
  if (strchr("!\"#$%&'()*+,-./:;{}<=>?@[\\]^_`|~", ucs4))
    return true;
  return false;
}

unsigned
LitValue::validate_string(const char *s)
{
  const char *spos = s;
  ucs4_t c;

  assert(*spos == '"');
  spos++;

  while (*spos != '"') {
    const char *snext;
    c = DecodeStringCharacter(spos, &snext); //&OK
    if (c < 0)
      return spos - s;

    spos = snext;
  }

  return 0;
}

/// @brief Map from all of the named, escaped literals to the
/// corresponding code points.
///
/// There were getting to be too many of these to keep in sync, so I
/// wanted one place to maintain them.
///
/// This table is only used to validate lexically well-formed
/// escapes. The format for in-string, sexpr-char, and block-char
/// escapes are mutually non-overlapping, so we just stick them all in
/// a single table.
///
/// The multi-character literals appear in three forms:
///
/// S-expression: #\formfeed
/// Block character escape: '\formfeed'
/// Embedded in a string: "...\{formfeed}..."
///
/// The single character versions do not have short-form S-expression
/// character variants, because this would interfere with the normal
/// S-expression character syntax.

#define ESCAPED_LITERAL(s, cp)  \
  { "\\{" s "}" , cp },         \
  { "'\\" s "'", cp }
#define SINGLE_LITERAL(s, cp)  \
  { "\\" s , cp },             \
  { "'\\" s "'", cp }

LitValue::EscapedLiteral
LitValue::EscapedLiteralMap[] = {
  ESCAPED_LITERAL("space", ' '),
  ESCAPED_LITERAL("linefeed", '\n'),
  SINGLE_LITERAL("n", '\n' ),
  ESCAPED_LITERAL("return", '\r'),
  SINGLE_LITERAL("r", '\r' ),
  ESCAPED_LITERAL("tab", '\t'),
  SINGLE_LITERAL("t", '\t' ),
  ESCAPED_LITERAL("backspace", '\b'),
  SINGLE_LITERAL("b", '\b' ),
  ESCAPED_LITERAL("formfeed", '\f'),
  SINGLE_LITERAL("f", '\f' ),
  SINGLE_LITERAL("s", ' ' ),
  ESCAPED_LITERAL("backslash", '\\'),
  SINGLE_LITERAL("\\", '\\' ),
  ESCAPED_LITERAL("dquote", '\"'),
  SINGLE_LITERAL("\"", '\"' ),
  ESCAPED_LITERAL("squote", '\''),
  SINGLE_LITERAL("\'", '\'' )
};
const size_t LitValue::EscapedLiteralMapLength = 
  (sizeof(LitValue::EscapedLiteralMap) /
   sizeof(LitValue::EscapedLiteralMap[0]));

ucs4_t
LitValue::GetEscapedCodePoint(const char *escapedLiteral)
{
  for (size_t i = 0; i < EscapedLiteralMapLength; i++) {
    if (strcmp(EscapedLiteralMap[i].s, escapedLiteral) == 0) {
      return EscapedLiteralMap[i].codePoint;
    }
  }

  return -1;
}

ucs4_t
LitValue::DecodeNumericCharacter(const char *s, const char **snext)
{
  ucs4_t codePoint = 0;
  unsigned radix = 10;

  // Could be unicode escape:
  if (s[0] == 'U' && s[1] == '+') {
    s = s + 2;
    radix = 16;
  }
  else if ((s[0] == '0') && (s[1] == 'x')) { // hexadecimal
    s = s + 2;
    radix = 16;
  }
  else if ((s[0] == '0') && (s[1] == 'o')) { // octal
    s = s + 2;
    radix = 8;
  }
  else if ((s[0] == '0') && (s[1] == 'b')) { // binary
    s = s + 2;
    radix = 2;
  }
  else if (s[0] == '0') {       // C-style octal
    s = s + 1;
    radix = 8;
  }
  else if (!isdigit(s[0])) {
    if (snext) *snext = s;
    return -1;
  }

  for (;;) {
    long dv = digitValue(*s, 16);
    if (dv < 0)                 // exits on NUL, ', or }
      break;

    codePoint *= 16;
    codePoint += dv;

    if (codePoint > UCHAR_MAX_VALUE) {
      if (snext) *snext = s;
      return -1;
    }
    s++;
  }

  if (snext) *snext = s;
  return codePoint;
}

ucs4_t
LitValue::DecodeStringCharacter(const char *s, const char **next)
{
  const char *sBegin = s;
  const char *snext = s + 1;

  ucs4_t c = sherpa::utf8_decode(s, &snext);

  if (c == ' ') {
    if (next) *next = snext;
    if (DEBUG_DECODE)
      std::cerr << "DecodeStringChar handles { } giving "
                << (ucs4_t)' '
                << std::endl;
    return ' ';
  }
  else if (c != '\\') {
    if (!u_isgraph(c)) {
      if (DEBUG_DECODE)
        std::cerr << "DecodeStringChar handles {"
                  << (char) c
                  << "} giving -1"
                  << std::endl;
      return -1;
    }

    if (DEBUG_DECODE)
      std::cerr << "DecodeStringChar handles {"
                << (char) c
                << "} giving "
                << (ucs4_t)c
                << std::endl;

    if (next) *next = snext;
    return c;
  }
  else {
    s = snext;
    c = sherpa::utf8_decode(s, &snext);

    if (c == '{' ) {
      while (c != '}')
        c = sherpa::utf8_decode(s, &snext);
    }

    std::string theEscape(sBegin, snext - sBegin);
    ucs4_t codePoint = GetEscapedCodePoint(theEscape.c_str());
    if (codePoint < 0) {
      if (DEBUG_DECODE)
        std::cerr << "DecodeStringChar handles {"
                  << theEscape
                  << "} giving -1"
                  << std::endl;
      return -1;
    }

    if (DEBUG_DECODE)
      std::cerr << "DecodeStringChar handles {"
                << theEscape
                << "} giving "
                << (ucs4_t)codePoint
                << std::endl;

    if (next) *next = snext;
    return codePoint;
  }
}

ucs4_t
LitValue::DecodeBlockCharacter(const char *s)
{
  const char *snext;

  ucs4_t codePoint = GetEscapedCodePoint(s);
  if (codePoint >= 0)
    return codePoint;

  if (s[1] == '\\') {
    // Any remaining escape must be numeric:

    s = s + 2;
    codePoint = DecodeNumericCharacter(s, &snext);
    if (codePoint < 0)
      return codePoint;

    if (snext == s) {
      // It wasn't a numeric escape:
      return -1;
    }

    return codePoint;
  }

  else {
    s = s + 1;                  // skip the '\'

    // This is a non-escaped character:
    codePoint = sherpa::utf8_decode(s, &snext); //&OK
    if (codePoint < 0 || snext == s)
      return -1;

    if (valid_charpoint(codePoint) || valid_charpunct(codePoint))
      return codePoint;
  }

  return -1;
}

ucs4_t
LitValue::DecodeCharacter(const std::string& s)
{
  const char *str = s.c_str();

  ucs4_t codePoint = DecodeBlockCharacter(str);

#if 0
  std::cerr << "Decoding character {" << s << "} gives " 
            << codePoint << std::endl;
#endif

  return codePoint;
}

static bool
needsBackslashEscape(uint32_t c)
{
  return (c == '"' || c == '\'' || c == '\\');
}

static bool
asciiPrintableCharacter(uint32_t c)
{
  /* ASCII printable glyphs are in the range [0x20,0x7e], but a few
     of these require special escaping. */
  return (c >= 0x20 && c < 0x7f);
}

std::string
LitValue::asString() const
{
  std::stringstream ss;

  switch(litType) {
  case lt_bool:
    return (b == true) ? "true" : "false";

  case lt_char:
    {
      if (asciiPrintableCharacter(c))
        ss << (needsBackslashEscape(c) ? "'\\" : "'")
           << (unsigned char) c << "'";
      else
        ss << (unsigned long)(c);

      return ss.str();
    }
  case lt_int:
    ss << i;                    // defer to bignum printer
    return ss.str();

  case lt_float:
    {
      char buf[256];
      snprintf(buf, sizeof(buf), " %f\n", d);
      return buf;
    }

  case lt_string:
    return s;

  default:
    return "BAD LITERAL TYPE";
  }
}
