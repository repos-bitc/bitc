/**************************************************************************
 *
 * Copyright (C) 2010, Jonathan S. Shapiro
 * Portions Copyright (C) 2008, Johns Hopkins University
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
#include <string.h>
#include <string>

#include <unicode/uchar.h>

#include <libsherpa/utf8.hxx>
#include <libsherpa/LexLoc.hxx>
#include <libsherpa/utf8.hxx>

#include "BUILD/TransitionParser.hxx"

using namespace sherpa;

#include "TransitionLexer.hxx"

bool
TransitionLexer::valid_char_printable(uint32_t ucs4)
{
  if (strchr("!#$%&`()*+-.,/:;<>=?@_|~^[]'", ucs4))
    return true;
  return false;
}

bool
TransitionLexer::valid_ident_punct(uint32_t ucs4, bool allowExtended)
{
  if (ucs4 == '_')
    return true;
  if (allowExtended && strchr("!$%&|*+-/<>=?@~", ucs4))
    return true;
  return false;
}

bool
TransitionLexer::valid_ident_start(uint32_t ucs4)
{
  // Extended characters are only permitted as the first
  // identifier character in lisp identifier mode.
  bool allowExtended = (currentLang & lf_LispIdents) ? true : false;
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_START)
          || valid_ident_punct(ucs4, allowExtended));
}

bool
TransitionLexer::valid_ident_continue(uint32_t ucs4)
{
  // For the moment, extended characters are permitted as 
  // continue characters.
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE) ||
          valid_ident_punct(ucs4, true));
}

bool
TransitionLexer::valid_ifident_start(uint32_t ucs4)
{
  return (isalpha(ucs4) || ucs4 == '_');
  //  return (u_hasBinaryProperty(ucs4,UCHAR_XID_START));
}

bool
TransitionLexer::valid_ifident_continue(uint32_t ucs4)
{
  return (isalpha(ucs4) || isdigit(ucs4) || ucs4 == '_' || ucs4 == '-');
  //  return (u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE) ||
  //valid_ifident_punct(ucs4));
}

bool
TransitionLexer::valid_tv_ident_start(uint32_t ucs4)
{
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_START) || 
          ucs4 == '_');
}

bool
TransitionLexer::valid_tv_ident_continue(uint32_t ucs4)
{
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE) ||
          ucs4 == '_');
}
bool
TransitionLexer::valid_charpoint(uint32_t ucs4)
{
  if (valid_char_printable(ucs4))
    return true;

  return u_isgraph(ucs4);
}

bool
TransitionLexer::valid_charpunct(uint32_t ucs4)
{
  if (strchr("!\"#$%&'()*+,-./:;{}<=>?@[\\]^_`|~", ucs4))
    return true;
  return false;
}

unsigned
TransitionLexer::validate_string(const char *s)
{
  const char *spos = s;
  uint32_t c;

  while (*spos) {
    const char *snext;
    c = sherpa::utf8_decode(spos, &snext); //&OK

    if (c == ' ') {                /* spaces are explicitly legal */
      spos = snext;
    }
    else if (c == '\\') {        /* escaped characters are legal */
      const char *escStart = spos;
      spos++;
      switch (*spos) {
      case 'n':
      case 't':
      case 'r':
      case 'b':
      case 's':
      case 'f':
      case '"':
      case '\\':
        {
          spos++;
          break;
        }
      case '{':
        {
          if (*++spos != 'U')
            return (spos - s);
          if (*++spos != '+')
            return (spos - s);
          {
            uint32_t codePoint = 0;

            while (*++spos != '}') {
              if (!isxdigit(*spos))
                return (spos - s);
              codePoint *= 16;
              codePoint += TransitionLexer::digitValue(*spos, 16);
              if (codePoint > UCHAR_MAX_VALUE)
                return (spos - s);
            }
          }
        }
        spos++;
        break;
      default:
        // Bad escape sequence
        return (escStart - s);
      }
    }
    else if (u_isgraph(c)) {
      spos = snext;
    }
    else return (spos - s);
  }

  return 0;
}

struct TransitionLexer::KeyWord TransitionLexer::keywords[] = {
  { "and",              lf_transition,        tk_AND },
  { "apply",            lf_transition,        tk_APPLY },
  { "array",            lf_transition,        tk_ARRAY },
  { "array-length",     lf_transition,        tk_ARRAY_LENGTH },
  { "array-ref",        lf_transition,        tk_ARRAY_REF },
  { "array-ref-length", lf_transition,        tk_ARRAY_REF_LENGTH },
  { "as",               lf_transition,        tk_AS },
  { "assert",           lf_transition,        tk_ReservedWord },
  { "begin",            lf_transition,        tk_BEGIN },
  { "bitc",             lf_version,           tk_BITC },
  { "bitfield",         lf_transition,        tk_BITFIELD },
  { "bitsizeof",        lf_transition,        tk_BITSIZEOF },
  { "block",            lf_transition,        tk_BLOCK },
  { "bool",             lf_transition,        tk_BOOL },
  { "break",            lf_transition,        tk_ReservedWord },
  { "by-ref",           lf_transition,        tk_BY_REF },
  { "case",             lf_transition,        tk_CASE },
  { "catch",            lf_transition,        tk_CATCH },
  { "char",             lf_transition,        tk_CHAR },
  { "check",            lf_transition,        tk_ReservedWord },
  { "closed",           lf_transition,        tk_CLOSED },
  { "coindset",         lf_transition,        tk_ReservedWord },
  { "cond",             lf_transition,        tk_COND },
  { "const",            lf_transition,        tk_CONST },
  { "constrain",        lf_transition,        tk_ReservedWord },
  { "continue",         lf_transition,        tk_CONTINUE },
  { "declare",          lf_transition,        tk_DECLARE },
  { "deep-const",       lf_transition,        tk_ReservedWord },
  { "defequiv",         lf_transition,        tk_ReservedWord },
  { "defexception",     lf_transition,        tk_DEFEXCEPTION },
  { "define",           lf_transition,        tk_DEFINE },
  { "definstance",      lf_transition,        tk_DEFINSTANCE },
  { "definvariant",     lf_transition,        tk_ReservedWord },
  { "defobject",        lf_transition,        tk_DEFOBJECT },
  { "defrefine",        lf_transition,        tk_ReservedWord },
  { "defrepr",          lf_transition,        tk_DEFREPR },
  { "defstruct",        lf_transition,        tk_DEFSTRUCT },
  { "deftheory",        lf_transition,        tk_ReservedWord },
  { "defthm",           lf_transition,        tk_DEFTHM },
  { "deftypeclass",     lf_transition,        tk_DEFTYPECLASS },
  { "defunion",         lf_transition,        tk_DEFUNION },
  { "defvariant",       lf_transition,        tk_ReservedWord },
  { "deref",            lf_transition,        tk_DEREF },
  { "disable",          lf_transition,        tk_ReservedWord },
  { "do",               lf_transition,        tk_DO },
  { "do*",              lf_transition,        tk_ReservedWord },
  { "double",           lf_transition,        tk_DOUBLE },
  { "dup",              lf_transition,        tk_DUP },
  { "enable",           lf_transition,        tk_ReservedWord },
  { "exception",        lf_transition,        tk_EXCEPTION },
  { "external",         lf_transition,        tk_EXTERNAL },
  { "fill",             lf_transition,        tk_FILL },
  { "float",            lf_transition,        tk_FLOAT },
  { "fn",               lf_transition,        tk_FN },
  { "forall",           lf_transition,        tk_FORALL },
  { "if",               lf_transition,        tk_IF },
  { "import",           lf_transition,        tk_IMPORT },
  { "import!",          lf_transition,        tk_ReservedWord },
  { "impure",           lf_transition,        tk_IMPURE },
  { "indset",           lf_transition,        tk_ReservedWord },
  { "inner-ref",        lf_transition,        tk_INNER_REF },
  { "int16",            lf_transition,        tk_INT16 },
  { "int32",            lf_transition,        tk_INT32 },
  { "int64",            lf_transition,        tk_INT64 },
  { "int8",             lf_transition,        tk_INT8 },
  { "interface",        lf_transition,        tk_INTERFACE },
  { "lambda",           lf_transition,        tk_LAMBDA },
  { "let",              lf_transition,        tk_LET },
  { "let*",             lf_transition,        tk_ReservedWord },
  { "letrec",           lf_transition,        tk_LETREC },
  { "location",         lf_transition,        tk_ReservedWord },
  //  { "make-vector",      lf_transition,        tk_MAKE_VECTOR },
  { "make-vector",      lf_transition,        tk_MAKE_VECTORL },
  { "member",           lf_transition,        tk_MEMBER },   /* REDUNDANT */
  { "method",           lf_transition,        tk_METHOD },
  { "module",           lf_transition,        tk_MODULE },
  { "mutable",          lf_transition,        tk_MUTABLE },
  { "namespace",        lf_transition,        tk_ReservedWord },
  { "nth",              lf_transition,        tk_NTH },
  { "opaque",           lf_transition,        tk_OPAQUE },
  { "or",               lf_transition,        tk_OR },
  { "otherwise",        lf_transition,        tk_OTHERWISE },
  { "proclaim",         lf_transition,        tk_PROCLAIM },
  { "provide",          lf_transition,        tk_PROVIDE },
  { "provide!",         lf_transition,        tk_ReservedWord },
  { "pure",             lf_transition,        tk_PURE },
  { "quad",             lf_transition,        tk_QUAD },
  { "read-only",        lf_transition,        tk_ReservedWord },
  { "ref",              lf_transition,        tk_REF },
  { "require",          lf_transition,        tk_ReservedWord },
  { "reserved",         lf_transition,        tk_RESERVED },
  { "return",           lf_transition,        tk_RETURN },
  { "return-from",      lf_transition,        tk_RETURN_FROM },
  { "sensory",          lf_transition,        tk_ReservedWord },
  { "set!",             lf_transition,        tk_SET },
  { "sizeof",           lf_transition,        tk_SIZEOF },
  { "string",           lf_transition,        tk_STRING },
  { "super",            lf_transition,        tk_ReservedWord },
  { "suspend",          lf_transition,        tk_SUSPEND },  
  { "switch",           lf_transition,        tk_SWITCH },
  { "tag",              lf_transition,        tk_TAG },
  { "the",              lf_transition,        tk_THE },
  { "throw",            lf_transition,        tk_THROW },
  { "try",              lf_transition,        tk_TRY },
  { "tycon",            lf_transition,        tk_ReservedWord },
  { "tyfn",             lf_transition,        tk_TYFN },
  { "uint16",           lf_transition,        tk_UINT16 },
  { "uint32",           lf_transition,        tk_UINT32 },
  { "uint64",           lf_transition,        tk_UINT64 },
  { "uint8",            lf_transition,        tk_UINT8 },
  { "using",            lf_transition,        tk_ReservedWord },
  { "val",              lf_transition,        tk_VAL },
  { "value-at",         lf_transition,        tk_ReservedWord },
  { "vector",           lf_transition,        tk_VECTOR },
  { "vector-length",    lf_transition,        tk_VECTOR_LENGTH },
  { "version",          lf_version,           tk_VERSION },
  { "when",             lf_transition,        tk_WHEN },
  { "where",            lf_transition,        tk_WHERE },
  { "word",             lf_transition,        tk_WORD }
};

static int
kwstrcmp(const void *vKey, const void *vCandidate)
{
  const char *key = ((const TransitionLexer::KeyWord *) vKey)->nm;
  const char *candidate = ((const TransitionLexer::KeyWord *) vCandidate)->nm;

  return strcmp(key, candidate);
}

int
TransitionLexer::kwCheck(const char *s)
{
  if (ifIdentMode) {
    if (!valid_ifident_start(*s))
      return tk_ReservedWord;

    for (++s; *s; s++)
      if (!valid_ifident_continue(*s))
        return tk_ReservedWord;

    return tk_Ident;
  }

  KeyWord key = { s, lf_transition, 0 };
  KeyWord *entry = 
    (KeyWord *)bsearch(&key, keywords, // &OK
                       sizeof(keywords)/sizeof(keywords[0]), 
                       sizeof(keywords[0]), kwstrcmp);

  // If it is in the token table, and it is accepted in the prevailing
  // language variant, return the indicated token type. Note a trick
  // here that the very first token accepted may be accepted under the
  // lf_version sub-language. Once the first token has been accepted,
  // we disable that sub-language.
  if (entry) {
    if (currentLang & entry->whichLang) {
      currentLang &= ~LangFlags(lf_version);

      return entry->tokValue;
    }
  }

  // Otherwise, check for various reserved words:

  // Things starting with "__":
  if (s[0] == '_' && s[1] == '_') {
    if (!isRuntimeUoc)
      return tk_ReservedWord;
  }

  // Things starting with "def":
  if (s[0] == 'd' && s[1] == 'e' && s[2] == 'f')
    return tk_ReservedWord;

  // Things starting with "#":
  if (s[0] == '#')
    return tk_ReservedWord;

  return tk_Ident;
}

void
TransitionLexer::ReportParseError()
{
  errStream << here
            << ": syntax error (via yyerror)" << '\n';
  num_errors++;
}
 
void
TransitionLexer::ReportParseError(const LexLoc& where, std::string msg)
{
  errStream << where
            << ": "
            << msg << std::endl;

  num_errors++;
}

void
TransitionLexer::ReportParseWarning(const LexLoc& where, std::string msg)
{
  errStream << where
            << ": "
            << msg << std::endl;
}

TransitionLexer::TransitionLexer(std::ostream& _err, std::istream& _in, 
                       const std::string& origin,
                       bool commandLineInput)
  :here(origin, 1, 0), inStream(_in), errStream(_err)
{
  inStream.unsetf(std::ios_base::skipws);

  // Don't accept block syntax keywords until we see the new version syntax,
  // which is accepted under lf_version
  currentLang = lf_sexpr | lf_version | lf_LispIdents | lf_LispComments;
  num_errors = 0;
  isRuntimeUoc = false;
  ifIdentMode = false;
  isCommandLineInput = commandLineInput;
  debug = false;
  putbackChar = -1;
  nModules = 0;
}

long 
TransitionLexer::digitValue(ucs4_t ucs4, unsigned radix)
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

ucs4_t
TransitionLexer::getChar()
{
  char utf[8];
  unsigned char c;

  long ucs4 = putbackChar;

  if (putbackChar != -1) {
    putbackChar = -1;
    utf8_encode(ucs4, utf);
    goto checkDigit;
  }

  memset(utf, 0, 8);

  utf[0] = inStream.get();
  c = utf[0];
  if (utf[0] == EOF)
    return EOF;

  if (c <= 127)
    goto done;

  utf[1] = inStream.get();
  if (utf[1] == EOF)
    return EOF;

  if (c <= 223)
    goto done;

  utf[2] = inStream.get();
  if (utf[2] == EOF)
    return EOF;

  if (c <= 239)
    goto done;

  utf[3] = inStream.get();
  if (utf[3] == EOF)
    return EOF;

  if (c <= 247)
    goto done;
 
  utf[4] = inStream.get();
  if (utf[4] == EOF)
    return EOF;

  if (c <= 251)
    goto done;

  utf[5] = inStream.get();
  if (utf[5] == EOF)
    return EOF;

 done:
  ucs4 = utf8_decode(utf, 0);
 checkDigit:
  thisToken += utf;

  return ucs4;
}

void
TransitionLexer::ungetChar(ucs4_t c)
{
  char utf[8];
  assert(putbackChar == -1);
  putbackChar = c;

  unsigned len = utf8_encode(c, utf);
  thisToken.erase( thisToken.length() - len);
}

static bool
isCharDelimiter(ucs4_t c)
{
  switch (c) {
  case ' ':
  case '\t':
  case '\n':
  case '\r':
  case ')':
    return true;
  default:
    return false;
  }
}

int
TransitionLexer::lex(ParseType *lvalp)
{
  ucs4_t c;

 startOver:
  thisToken.erase();

  c = getChar();

  switch (c) {
  case '/':
    c = getChar();
    if (c == '/') {
      do {
        c = getChar();
      } while (c != '\n' && c != '\r');
      ungetChar(c);
      here.updateWith(thisToken);
      goto startOver;
    }
    else if (c == '*') {
      for (;;) {
        c = getChar();
        if (c == '*') {
          c = getChar();
          if (c == '/') {
            break;
          }
          else if (c == EOF)
            return EOF;
          else
            ungetChar(c);
        }
        else if (c == EOF)
          return EOF;
      }

      here.updateWith(thisToken);
      goto startOver;
    }
    else {
      ungetChar(c);
      if (currentLang & lf_LispIdents)
        goto identifier;
      else {
#if 1
        return EOF;                // temporary
#else
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return '/':
#endif
      }
    }

  case '*':
  case '+':
    {
      if (currentLang & lf_LispIdents)
        goto identifier;
      else {
#if 1
        return EOF;                // temporary
#else
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return c:
#endif
      }
    }

  case ';':                        // Comments
    do {
      c = getChar();
    } while (c != '\n' && c != '\r');
    // FALL THROUGH 

  case ' ':                        // White space
  case '\t':
  case '\n':
  case '\r':
    here.updateWith(thisToken);
    goto startOver;


  case '.':                        // Single character tokens
  case ',':
  case '[':
  case ']':
  case '(':
  case ')':
  case ':':
  case '^':
    lvalp->tok = LToken(here, thisToken);
    here.updateWith(thisToken);
    return c;


  case '"':                        // String literal
    {
      do {
        c = getChar();

        if (c == '\\') {
          (void) getChar();        // just ignore it -- will validate later
        }
      }        while (c != '"');
      
      unsigned badpos = validate_string(thisToken.c_str());

      if (badpos) {
        LexLoc badHere = here;
        badHere.offset += badpos;
        errStream << badHere.asString()
                  << ": Illegal (non-printing) character in string '"
                  << thisToken << "'\n";
        num_errors++;
      }

      LexLoc tokStart = here;
      here.updateWith(thisToken);
      lvalp->tok = LToken(here, thisToken.substr(1, thisToken.size()-2));
      return tk_String;
    }


  case '#':                        // character and boolean literals
    {
      c = getChar();
      switch(c) {
      case 't':
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return tk_TRUE;
      case 'f':
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return tk_FALSE;
      case '\\':
        {
          c = getChar();

          if (valid_charpunct(c)) {
            lvalp->tok = LToken(here, thisToken);
            here.updateWith(thisToken);
            return tk_Char;
          }
          else if (c == 'U') {
            uint32_t codePoint = 0;

            c = getChar();
            if (c == '+') {
              for(;;) {
                c = getChar();
                long dv = digitValue(c, 16);
                if (dv < 0)
                  break;
                codePoint *= 16;
                codePoint += dv;
                if (codePoint > UCHAR_MAX_VALUE)
                  return EOF;
              }
              
              if (!isCharDelimiter(c)) {
                ungetChar(c);
                return EOF;
              }
            }
            ungetChar(c);

            lvalp->tok = LToken(here, thisToken);
            here.updateWith(thisToken);
            return tk_Char;
          }
          else if (valid_charpoint(c)) {
            // Collect more characters in case this is a named character
            // literal.
            do {
              c = getChar();
            } while (isalpha(c));
            ungetChar(c);

            if (thisToken.size() == 3 || // '#' '\' CHAR
                thisToken == "#\\space" ||
                thisToken == "#\\linefeed" ||
                thisToken == "#\\return" ||
                thisToken == "#\\tab" ||
                thisToken == "#\\backspace" ||
                thisToken == "#\\lbrace" ||
                thisToken == "#\\rbrace") {
              lvalp->tok = LToken(here, thisToken);
              here.updateWith(thisToken);
              return tk_Char;
            }
          }

          // FIX: this is bad input
          return EOF;
        }
      default:
        // FIX: this is bad input
        return EOF;
      }
    }

  case '\'':                        // Type variables
    {
      int tokType = tk_TypeVar;

      c = getChar();

      if (c == '%') {
        tokType = tk_EffectVar;
        c = getChar();
      }

      if (!valid_tv_ident_start(c)) {
        // FIX: this is bad input
        ungetChar(c);
        return EOF;
      }

      do {
        c = getChar();
      } while (valid_tv_ident_continue(c));
      ungetChar(c);

      here.updateWith(thisToken);
      lvalp->tok = LToken(here, thisToken);
      return tokType;
    }

    // Hyphen requires special handling. If it is followed by a digit
    // then it is the beginning of a numeric literal, else it is part
    // of an identifier.
  case '-':
    c = getChar();
    if (c == '>')
      return tk_FNARROW;

    if (digitValue(c, 10) < 0) {
      ungetChar(c);
      if (currentLang & lf_LispIdents)
        goto identifier;
      else {
#if 1
        return EOF;                // temporary
#else
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return '-':
#endif
      }
    }

    /* ELSE fall through to digit processing */
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    {
      int radix = 10;

      do {
        c = getChar();
      } while (digitValue(c, 10) >= 0);

      /* At this point we could either discover a radix marker 'r', or
         a decimal point (indicating a floating poing literal or a
         language version). If it is a radix marker, change the radix
         value here so that we match the succeeding digits in the
         correct radix. */
      if (c == 'r') {
        radix = strtol(thisToken.c_str(), 0, 10);
        if (radix < 0) radix = -radix; // leading sign not part of radix

        long count = 0;
        do {
          c = getChar();
          count++;
        } while (digitValue(c, radix) >= 0);
        count--;
        /* FIX: if count is 0, number is malformed */
      }

      /* We are either done with the literal, in which case it is an
         integer literal, or we are about to see a decimal point, in
         which case it is either a floating point literal or a
         language version. */
      if (c != '.') {
        ungetChar(c);
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return tk_Int;
      }

      if (currentLang & lf_version) {
        /* Looking for a language version number. */
        long count = 0;
        do {
          c = getChar();
          count++;
        } while (digitValue(c, 10) >= 0);
        count--;
        ungetChar(c);
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return tk_VersionNumber;
      }
      else {
        /* We have seen a decimal point, so from here on it must be a
           floating point literal. */
        long count = 0;
        do {
          c = getChar();
          count++;
        } while (digitValue(c, radix) >= 0);
        count--;
        /* FIX: if count is 0, number is malformed */
      }

      /* We are either done with this token or we are looking at a '^'
         indicating start of an exponent. */
      if (c != '^') {
        ungetChar(c);
        lvalp->tok = LToken(here, thisToken);
        here.updateWith(thisToken);
        return tk_Float;
      }

      /* Need to collect the exponent. Revert to radix 10 until
         otherwise proven. */
      c = getChar();
      radix = 10;

      if (c != '-' && digitValue(c, radix) < 0) {
        // FIX: Malformed token
      }

      do {
        c = getChar();
      } while (digitValue(c, 10) >= 0);

      /* Check for radix marker on exponent */
      if (c == 'r') {
        radix = strtol(thisToken.c_str(), 0, 10);
        if (radix < 0) radix = -radix; // leading sign not part of radix

        long count = 0;
        do {
          c = getChar();
          count++;
        } while (digitValue(c, radix) >= 0);
        count--;
        /* FIX: if count is 0, number is malformed */
      }

      ungetChar(c);
      lvalp->tok = LToken(here, thisToken);
      here.updateWith(thisToken);
      return tk_Float;
    }

  case EOF:
    return EOF;

  default:
    if (valid_ident_start(c))
      goto identifier;

    // FIX: Malformed token
    return EOF;
  }

 identifier:
  do {
    c = getChar();
  } while (valid_ident_continue(c));
  ungetChar(c);
  lvalp->tok = LToken(here, thisToken);
  here.updateWith(thisToken);
  return kwCheck(thisToken.c_str());
}
