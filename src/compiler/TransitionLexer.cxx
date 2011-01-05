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

#include "BUILD/TransitionParser.hxx"

using namespace sherpa;

#include "TransitionLexer.hxx"
#include "LitValue.hxx"

extern const char *TransitionTokenName(int lexTokenNumber);

bool
TransitionLexer::valid_ident_start(ucs4_t ucs4)
{
  // '_' is now handled as a special case in the tokenizer, because it
  // has behavioral significance for mixfix identifiers
  return u_hasBinaryProperty(ucs4,UCHAR_XID_START);
}

bool
TransitionLexer::valid_ident_continue(ucs4_t ucs4)
{
  // '_' is now handled as a special case in the tokenizer, because it
  // has behavioral significance for mixfix identifiers
  return u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE);
}

bool
TransitionLexer::valid_ascii_symbol(ucs4_t ucs4)
{
  switch (ucs4) {
  case '_':
    // '_' is now handled as a special case in the tokenizer, because
    // it has behavioral significance for mixfix identifiers
    return false;

  case '#':                     // thunked hole marker is "#_"
    return false;

  case '@':                     // spacer for non-hole quasi-keywords
    return false;

  case ':':                     // this just broke too much stuff
    return false;

  case '!':
  case '$':
  case '%':
  case '&':
  case '*':
  case '+':
  case '-':
  case '/':
  case '<':
  case '>':
  case '=':
  case '?':
  case '^':
  case '|':
  case '~':
    return true;

  default:
    return false;
  }
}

bool
TransitionLexer::valid_operator_start(ucs4_t ucs4)
{

  // Extended characters are only permitted as the first
  // identifier character in lisp identifier mode.
  return (valid_ascii_symbol(ucs4));
}

bool
TransitionLexer::valid_operator_continue(ucs4_t ucs4)
{
  // For the moment, extended characters are permitted as 
  // continue characters.
  return (valid_ascii_symbol(ucs4));
}

bool
TransitionLexer::valid_ifident_start(ucs4_t ucs4)
{
  return (isalpha(ucs4) || ucs4 == '_');
  //  return (u_hasBinaryProperty(ucs4,UCHAR_XID_START));
}

bool
TransitionLexer::valid_ifident_continue(ucs4_t ucs4)
{
  return (isalpha(ucs4) || isdigit(ucs4) || ucs4 == '_' || ucs4 == '-');
  //  return (u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE) ||
  //valid_ifident_punct(ucs4));
}

bool
TransitionLexer::valid_tv_ident_start(ucs4_t ucs4)
{
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_START) || 
          ucs4 == '_');
}

bool
TransitionLexer::valid_tv_ident_continue(ucs4_t ucs4)
{
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE) ||
          ucs4 == '_');
}

TransitionLexer::KeyWord::KeyWord(const char *_nm, LangFlags _whichLang, int _tokValue)
{
  nm = _nm;
  whichLang = _whichLang;
  tokValue = _tokValue;
}

/// @brief Whether keyword table has been sorted.
///
/// I added this because the emacs sort-lines function doesn't
/// do the right thing for trailing punctuation in keywords. This is
/// true because '!' sorts lower than '"', so
///
///    "set"  sorts lower than
///    "set!
///
/// I finally got tired of forgetting to fix this by hand.
static bool keywords_sorted = false;

struct TransitionLexer::KeyWord TransitionLexer::keywords[] = {
  TransitionLexer::KeyWord( "=",                lf_block,        '=' ),
//  TransitionLexer::KeyWord( "!=",               lf_block,             tk_NOTEQUALS ),
  TransitionLexer::KeyWord( "->",               lf_block,        tk_FNARROW ),
  TransitionLexer::KeyWord( "==",               lf_block,        tk_EQUALS ),
  TransitionLexer::KeyWord( "and",              lf_block,        tk_AND ),
  TransitionLexer::KeyWord( "apply",            lf_block,        tk_APPLY ),
  TransitionLexer::KeyWord( "array",            lf_block,        tk_ARRAY ),
  TransitionLexer::KeyWord( "ArrayRef",         lf_block,        tk_ARRAY_REF ),
  TransitionLexer::KeyWord( "as",               lf_block,        tk_AS ),
  TransitionLexer::KeyWord( "assert",           lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "begin",            lf_block,        tk_BEGIN ),
  TransitionLexer::KeyWord( "bitc",             lf_version,      tk_BITC ),
  TransitionLexer::KeyWord( "bitfield",         lf_block,        tk_BITFIELD ),
  TransitionLexer::KeyWord( "bitsizeof",        lf_block,        tk_BITSIZEOF ),
//  TransitionLexer::KeyWord( "block",            lf_block,        tk_BLOCK ),
  TransitionLexer::KeyWord( "bool",             lf_block,        tk_BOOL ),
  TransitionLexer::KeyWord( "boxed",            lf_block,        tk_BOXED ),
  TransitionLexer::KeyWord( "break",            lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "ByRef",            lf_block,        tk_BY_REF ),
  TransitionLexer::KeyWord( "case",             lf_block,        tk_CASE ),
  TransitionLexer::KeyWord( "catch",            lf_block,        tk_CATCH ),
  TransitionLexer::KeyWord( "char",             lf_block,        tk_CHAR ),
  TransitionLexer::KeyWord( "check",            lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "closed",           lf_block,        tk_CLOSED ),
  TransitionLexer::KeyWord( "cond",             lf_block,        tk_COND ),
  TransitionLexer::KeyWord( "const",            lf_block,        tk_CONST ),
  TransitionLexer::KeyWord( "constrain",        lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "continue",         lf_block,        tk_CONTINUE ),
  TransitionLexer::KeyWord( "declare",          lf_block,        tk_DECLARE ),
  TransitionLexer::KeyWord( "deep",             lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "def",              lf_block,        tk_DEF ),
  TransitionLexer::KeyWord( "deref",            lf_block,        tk_DEREF ),
  TransitionLexer::KeyWord( "disable",          lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "do",               lf_block,        tk_DO ),
  TransitionLexer::KeyWord( "do*",              lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "double",           lf_block,        tk_DOUBLE ),
  TransitionLexer::KeyWord( "dup",              lf_block,        tk_DUP ),
  TransitionLexer::KeyWord( "else",             lf_block,        tk_ELSE ),
  TransitionLexer::KeyWord( "enable",           lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "exception",        lf_block,        tk_EXCEPTION ),
  TransitionLexer::KeyWord( "external",         lf_block,        tk_EXTERNAL ),
  TransitionLexer::KeyWord( "false",            lf_block,        tk_FALSE ),
  TransitionLexer::KeyWord( "fill",             lf_block,        tk_FILL ),
  TransitionLexer::KeyWord( "float",            lf_block,        tk_FLOAT ),
  TransitionLexer::KeyWord( "fn",               lf_block,        tk_FN ),
  TransitionLexer::KeyWord( "forall",           lf_block,        tk_FORALL ),
  TransitionLexer::KeyWord( "from",             lf_block,        tk_FROM ),
//  TransitionLexer::KeyWord( "giving",           lf_block,        tk_GIVING ),
  TransitionLexer::KeyWord( "if",               lf_block,        tk_IF ),
  TransitionLexer::KeyWord( "import",           lf_block,        tk_IMPORT ),
  TransitionLexer::KeyWord( "impure",           lf_block,        tk_IMPURE ),
  TransitionLexer::KeyWord( "in",               lf_block,        tk_IN ),
  TransitionLexer::KeyWord( "InnerRef",         lf_block,        tk_INNER_REF ),
  TransitionLexer::KeyWord( "instance",         lf_block,        tk_INSTANCE ),
  TransitionLexer::KeyWord( "int16",            lf_block,        tk_INT16 ),
  TransitionLexer::KeyWord( "int32",            lf_block,        tk_INT32 ),
  TransitionLexer::KeyWord( "int64",            lf_block,        tk_INT64 ),
  TransitionLexer::KeyWord( "int8",             lf_block,        tk_INT8 ),
  TransitionLexer::KeyWord( "interface",        lf_block,        tk_INTERFACE ),
  TransitionLexer::KeyWord( "is",               lf_block,        tk_IS ),
  TransitionLexer::KeyWord( "label",            lf_block,        tk_LABEL ),
  TransitionLexer::KeyWord( "lambda",           lf_block,        tk_LAMBDA ),
  TransitionLexer::KeyWord( "let",              lf_block,        tk_LET ),
  TransitionLexer::KeyWord( "let*",             lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "letrec",           lf_block,        tk_LETREC ),
  TransitionLexer::KeyWord( "location",         lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "loop",             lf_block,        tk_LOOP ),
  TransitionLexer::KeyWord( "MakeVector",       lf_block,        tk_MAKE_VECTOR ),
  TransitionLexer::KeyWord( "member",           lf_block,        tk_MEMBER ),   /* REDUNDANT */
  TransitionLexer::KeyWord( "method",           lf_block,        tk_METHOD ),
  TransitionLexer::KeyWord( "mixfix",           lf_block,        tk_MIXFIX ),
  TransitionLexer::KeyWord( "module",           lf_block,        tk_MODULE ),
  TransitionLexer::KeyWord( "mutable",          lf_block,        tk_MUTABLE ),
  TransitionLexer::KeyWord( "namespace",        lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "object",           lf_block,        tk_OBJECT ),
  TransitionLexer::KeyWord( "opaque",           lf_block,        tk_OPAQUE ),
  TransitionLexer::KeyWord( "or",               lf_block,        tk_OR ),
  TransitionLexer::KeyWord( "otherwise",        lf_block,        tk_OTHERWISE ),
//   TransitionLexer::KeyWord( "proclaim",         lf_block,        tk_PROCLAIM ),
  TransitionLexer::KeyWord( "provide",          lf_block,        tk_PROVIDE ),
  TransitionLexer::KeyWord( "provide!",         lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "pure",             lf_block,        tk_PURE ),
  TransitionLexer::KeyWord( "quad",             lf_block,        tk_QUAD ),
  TransitionLexer::KeyWord( "read-only",        lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "reference",        lf_block,        tk_PTR ),
  TransitionLexer::KeyWord( "repr",             lf_block,        tk_REPR ),
  TransitionLexer::KeyWord( "require",          lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "reserved",         lf_block,        tk_RESERVED ),
  TransitionLexer::KeyWord( "return",           lf_block,        tk_RETURN ),
  TransitionLexer::KeyWord( "sensory",          lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "sizeof",           lf_block,        tk_SIZEOF ),
  TransitionLexer::KeyWord( "string",           lf_block,        tk_STRING ),
  TransitionLexer::KeyWord( "struct",           lf_block,        tk_STRUCT ),
  TransitionLexer::KeyWord( "super",            lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "suspend",          lf_block,        tk_SUSPEND ),  
  TransitionLexer::KeyWord( "switch",           lf_block,        tk_SWITCH ),
  TransitionLexer::KeyWord( "tag",              lf_block,        tk_TAG ),
//  TransitionLexer::KeyWord( "the",              lf_block,        tk_THE ),
  TransitionLexer::KeyWord( "then",             lf_block,        tk_THEN ),
  TransitionLexer::KeyWord( "throw",            lf_block,        tk_THROW ),
  TransitionLexer::KeyWord( "trait",            lf_block,        tk_TRAIT ),
  TransitionLexer::KeyWord( "true",             lf_block,        tk_TRUE ),
  TransitionLexer::KeyWord( "try",              lf_block,        tk_TRY ),
  TransitionLexer::KeyWord( "tycon",            lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "tyfn",             lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "typecase",         lf_block,        tk_TYPECASE ),
  TransitionLexer::KeyWord( "uint16",           lf_block,        tk_UINT16 ),
  TransitionLexer::KeyWord( "uint32",           lf_block,        tk_UINT32 ),
  TransitionLexer::KeyWord( "uint64",           lf_block,        tk_UINT64 ),
  TransitionLexer::KeyWord( "uint8",            lf_block,        tk_UINT8 ),
  TransitionLexer::KeyWord( "unboxed",          lf_block,        tk_UNBOXED ),
  TransitionLexer::KeyWord( "union",            lf_block,        tk_UNION ),
  TransitionLexer::KeyWord( "unless",           lf_block,        tk_UNLESS ),
  TransitionLexer::KeyWord( "until",            lf_block,        tk_UNTIL ),
  TransitionLexer::KeyWord( "using",            lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "value-at",         lf_block,        tk_ReservedWord ),
  TransitionLexer::KeyWord( "vector",           lf_block,        tk_VECTOR ),
  TransitionLexer::KeyWord( "version",          lf_version,      tk_VERSION ),
  TransitionLexer::KeyWord( "when",             lf_block,        tk_WHEN ),
  TransitionLexer::KeyWord( "where",            lf_block,        tk_WHERE ),
  TransitionLexer::KeyWord( "word",             lf_block,        tk_WORD )
};

static int
kwstrcmp(const void *vKey, const void *vCandidate)
{
  const char *key = ((const TransitionLexer::KeyWord *) vKey)->nm;
  const char *candidate = ((const TransitionLexer::KeyWord *) vCandidate)->nm;

  return strcmp(key, candidate);
}

int
TransitionLexer::kwCheck(const char *s, int identType)
{
  if (ifIdentMode) {
    if (!valid_ifident_start(*s))
      return tk_ReservedWord;

    for (++s; *s; s++) {
      if (!valid_ifident_continue(*s))
        return tk_ReservedWord;
    }

    return tk_BlkIdent;
  }

  KeyWord key = KeyWord(s, lf_block, 0);
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

  // Things starting with "def" are reserved:
  if (s[0] == 'd' && s[1] == 'e' && s[2] == 'f')
    return tk_ReservedWord;

  else return identType;
}

void
TransitionLexer::ReportParseError()
{
  errStream << lastToken.loc
            << ": syntax error (via yyerror)" << std::endl;
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

  if (!keywords_sorted) {
    qsort(keywords,
         sizeof(keywords)/sizeof(keywords[0]), 
         sizeof(keywords[0]), kwstrcmp);
    keywords_sorted = true;
  }

  // Don't accept block syntax keywords until we see the new version syntax,
  // which is accepted under lf_version
  currentLang = lf_block | lf_version;
  num_errors = 0;
  isRuntimeUoc = false;
  ifIdentMode = false;
  isCommandLineInput = commandLineInput;
  debug = false;
  nModules = 0;

  showNextError = true;

  lastTokType = EOF;
  lastToken = LToken(EOF, "end of file");
}

ucs4_t
TransitionLexer::getChar()
{
  char utf[8];
  unsigned char c;

  long ucs4 = pushBackStack.pop();

  if (ucs4 != -1) {
    utf8_encode(ucs4, utf);
    goto checkDigit;
  }

  memset(utf, 0, 8);

  utf[0] = inStream.get();
  c = utf[0];
  if (inStream.eof())
    return EOF;

  if (c <= 127)
    goto done;

  utf[1] = inStream.get();
  if (inStream.eof())
    return EOF;

  if (c <= 223)
    goto done;

  utf[2] = inStream.get();
  if (inStream.eof())
    return EOF;

  if (c <= 239)
    goto done;

  utf[3] = inStream.get();
  if (inStream.eof())
    return EOF;

  if (c <= 247)
    goto done;
 
  utf[4] = inStream.get();
  if (inStream.eof())
    return EOF;

  if (c <= 251)
    goto done;

  utf[5] = inStream.get();
  if (inStream.eof())
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

  // Never bother to push back EOF, since it is reproduced by the
  // input.
  if (c == EOF)
    return;

  pushBackStack.push(c);

  unsigned len = utf8_encode(c, utf);
  thisToken.erase( thisToken.length() - len);
}

void
TransitionLexer::ungetThisToken()
{
  ucs4_t ucsToken[8];
  const char *s = thisToken.c_str();
  const char *snext = s;
  size_t i = 0;

  for( ; *snext && i < 8; i++) {
    ucsToken[i] = utf8_decode(s, &snext);
    s = snext;
  }

  // If the following assert fails, we are backing out something
  // excessively large...
  assert(*snext == 0);

  for (; i > 0; i--) {
    ucs4_t c = ucsToken[i-1];
    pushBackStack.push(c);
  }

  thisToken.erase();
}

static bool
isWhiteSpace(ucs4_t c)
{
  switch (c) {
  case ' ':
  case '\t':
  case '\n':
  case '\r':
    return true;
  default:
    return false;
  }
}

static bool
isCharDelimiter(ucs4_t c)
{
  if (c == ')') return true;

  return isWhiteSpace(c);
}

void
TransitionLexer::showToken(std::ostream& errStream, const LToken& tok)
{
  const char *tokTypeName = TransitionTokenName(tok.tokType);
  const char *prevTokTypeName = TransitionTokenName(tok.prevTokType);

  errStream << tok.tokType << ": " << tok.loc << ' '
            << tokTypeName;

  switch(tok.tokType) {
  case tk_TypeVar:
  case tk_EffectVar:
    errStream << " (" << tok.str << ")";
    break;
  case tk_BlkIdent:
  case tk_NegativeInt:
  case tk_Nat:
  case tk_Float:
  case tk_VersionNumber:
    errStream << " (" << tok.str << ")";
    break;

  case tk_Char:
    errStream << " ('" << tok.str << "')";
    break;
  case tk_String:
    errStream << " (\"" << tok.str << "\")";
    break;
  default:
    break;
  }

  errStream << " followed " << prevTokTypeName;

  if (tok.flags) {
    errStream << " [";

    if (tok.flags & TF_INSERTED)
      errStream << "INSERTED";

    if (tok.flags & TF_BY_PARSER)
      errStream << " BY PARSER";

    if (tok.flags & TF_AT_FIRST)
      errStream << " AT FIRST";

    if (tok.flags & TF_FIRST_ON_LINE) {
      if (tok.flags & TF_INSERTED)
        errStream << ", ";
      errStream << "FIRST-ON-LINE";
    }

    if (tok.flags & TF_REPROCESS) {
      if (tok.flags & (TF_INSERTED | TF_FIRST_ON_LINE))
        errStream << ", ";
      errStream << "REPROCESS";
    }
    errStream << "]";
  }
}

int
TransitionLexer::lex(ParseType *lvalp)
{
  LToken tok = getNextToken();
  assert (tok.prevTokType == lastTokType);

  if (debug) {
    errStream << "TOKEN ";
    showToken(errStream, tok);
    errStream << std::endl;
  }

  lastTokType = tok.tokType;
  lastToken = tok;
  
  lvalp->tok = tok;
  here = tok.endLoc;

  return tok.tokType;
}

void
TransitionLexer::beginBlock(const LToken& tok)
{
  bool inserted = (tok.flags & TF_INSERTED);

#ifdef LAYOUT_BLOCK_DEBUG
  errStream << "  LAYOUT: " << here 
            << (inserted ? ": inserted " : ": explicit ")
            << "beginBlock()" << std::endl;
#endif

  // Until we find the first token and update it, the semicolon
  // comment for a new block is the same as it's parent.
  unsigned column = layoutStack ? layoutStack->column : 0;

  boost::shared_ptr<LayoutFrame> lf = 
    LayoutFrame::make(lastToken.tokType, inserted, column, tok);

  //  expectingLeftBrace = false; // we're about to return it.
  //  learnBlockIndent = true;

  lf->next = layoutStack;
  layoutStack = lf;
}

void
TransitionLexer::endBlock(const LToken& tok)
{
  bool inserted = (tok.flags & TF_INSERTED);

  if (inserted && ! layoutStack->inserted) {
    std::stringstream ss;
    ss << "Inserted close brace balances explicit open brace at " 
       << layoutStack->tok.loc
       << ".";
    ReportParseError(tok.loc, ss.str());
  }
  else if (!inserted && layoutStack->inserted) {
    std::stringstream ss;
    ss << "Explicit close brace balances inserted open brace at "
       << layoutStack->tok.loc
       << ".";
    ReportParseError(tok.loc, ss.str());
  }

  // considerLineStart = false; // we've done it
  layoutStack = layoutStack->next;

#ifdef LAYOUT_BLOCK_DEBUG
  errStream << "  LAYOUT: " << here 
            << (inserted ? ": inserted " : ": explicit ")
            << "endBlock()";
  if (layoutStack)
    errStream << " leaving indent at " << layoutStack->column;
  errStream << std::endl;
#endif
}

bool
TransitionLexer::closeToOffset(unsigned offset)
{
  // Note that in contrast to closeToOpeningToken, this can safely
  // (and usefully) be called multiple times.

  // It is possible for us to be called on the first token in the
  // file, or on junk tokens after end of module. In both cases we
  // won't have a layout stack yet.
  if (!layoutStack)
    return false;

  if (!layoutStack->inserted)
    return false;

  if (offset >= layoutStack->column)
    return false;

#ifdef LAYOUT_BLOCK_DEBUG
  unsigned count = 0;

  errStream << "  LAYOUT: " << here 
            << ": closeToOffset inserted '}'" << std::endl;
#endif
    
  return true;
}

bool
TransitionLexer::conditionallyInsertSemicolon(unsigned offset)
{
  // It is possible for us to be called on the first token in the
  // file, or on junk tokens after end of module. In both cases we
  // won't have a layout stack yet.
  if (!layoutStack)
    return false;

  assert(layoutStack);
  if (here.offset <= layoutStack->column)
    return true;
  return false;
}

LexLoc
TransitionLexer::skipWhiteSpaceAndComments()
{
  ucs4_t c;

  LexLoc pos = here;

 startOver:
  thisToken.erase();

  c = getChar();

  if (c == '/') {
    c = getChar();
    if (c == '/') {
      do {
        c = getChar();
      } while (c != '\n' && c != '\r');
      // Back out the EOL. We'll handle that with white space
      // processing below to simplify layout processing.
      ungetChar(c);
      pos.updateWith(thisToken);
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
          else if (c == EOF) {
            pos.updateWith(thisToken);
            return pos;
          }
          else
            ungetChar(c);
        }
        else if (c == '\n' || c == '\r') {
          /* If a multi-line comment spans lines, the next token after
             the comment is the first token on that line */
          atBeginningOfLine = true;
        }
        else if (c == EOF) {
          pos.updateWith(thisToken);
          return pos;
        }
      }

      pos.updateWith(thisToken);
      goto startOver;
    }
    else {
      ungetChar(c);
      c = '/';
    }
  }

  if (isWhiteSpace(c)) {
    while (isWhiteSpace(c)) {
      if (c == '\n' || c == '\r') {
        atBeginningOfLine = true;
      }

      c = getChar();
    }
    ungetChar(c);

    pos.updateWith(thisToken);
    goto startOver;
  }

  ungetChar(c);

  return pos;
}

void
TransitionLexer::pushTokenBack(const LToken& tok, bool verbose)
{
  if (verbose && debug) {
    errStream << "PARSER PUSHED BACK TOKEN ";
    showToken(errStream, tok);
    errStream << std::endl;
  }
  pushbackTokens.push_back(tok);

  lastTokType = tok.prevTokType;

  if (verbose && debug) {
    errStream << "  Pushback Stack Is:\n";

    for (size_t i = pushbackTokens.size(); i > 0; i--) {
      LToken theTok = pushbackTokens[i-1];
      errStream << "  " << i-1 << " ";
      showToken(errStream, theTok);
      errStream << std::endl;
    }
  }
}

#if 0
#define RETURN_INSERTED(tok) do {  \
    LToken _tok = tok;             \
    _tok.flags |= TF_INSERTED;     \
    return _tok;                   \
  } while(false)
#endif

LToken
TransitionLexer::getNextToken()
{
  LToken tok = havePushbackToken() ? popToken() : getNextInputToken();

  LexLoc startLoc = here;
  LexLoc endLoc = here;

  // Rule 1: A left curly-brace is automatically inserted after
  // certain tokens:
  //
  // FIX: Once I clean up the surface syntax, this should also be done
  // for tk_DO.
  bool curlyRequired = ((lastTokType == EOF)        // beginning of file
                        // These are braced for bindings:
                        || (lastTokType == tk_LET)
                        || (lastTokType == tk_LETREC)
                        || (lastTokType == tk_LOOP)
                        || (lastTokType == tk_SWITCH)
                        // These are braced for blocks:
                        || (lastTokType == tk_IN)
                        || (lastTokType == tk_IS)
                        || (lastTokType == tk_DO)
                        || (lastTokType == tk_TRY)
                        || (lastTokType == tk_THEN)
                        || (lastTokType == tk_OTHERWISE)
                        || (lastTokType == tk_ELSE)
                        || false);

#ifdef LAYOUT_BLOCK_DEBUG
  if (lastTokType < 256 && isprint(lastTokType)) {
    errStream << "  LAYOUT: " << startLoc 
              << ": last token was '" << (char)lastTokType << "'." << std::endl;
  }
  else {
    errStream << "  LAYOUT: " << startLoc 
              << ": last token was type " << lastTokType << std::endl;
  }
#endif

  if (curlyRequired && (tok.tokType != '{')) {
    pushTokenBack(tok);

    LToken newTok = LToken('{', startLoc, endLoc, "{");
    newTok.flags |= TF_INSERTED;
    newTok.prevTokType = tok.prevTokType;

    beginBlock(newTok);
    
    // No position update for inserted token.
    return newTok;
  }

  // Rule 2: If the last token was a '{', then the next token is
  // processed specially. Either:
  //
  //   a) The next token is more indented than the prevailing indent
  //      level, in which case it establishes the indent level for the
  //      newly introduced block, OR
  //   b) The next token is NOT more indented, in which case the
  //      block is presumed to have been empty and a '} is immediately
  //      inserted. 
  if (lastTokType == '{') {
    assert(layoutStack);

#ifdef LAYOUT_BLOCK_DEBUG
    errStream << "  LAYOUT: " << startLoc 
              << ": Processing post-{..." << std::endl;
#endif

    if ((startLoc.offset > layoutStack->column) ||
        // Top-level context needs special handling if the curly brace
        // was insertedly inserted:
        (layoutStack->inserted && !layoutStack->next)) {
      // Valid indent. Establish indent level for new block:
      layoutStack->column = startLoc.offset;

#ifdef LAYOUT_BLOCK_DEBUG
      errStream << "  LAYOUT: " << startLoc 
                << ": set indent to "
                << startLoc.offset
                << ", disable check first token." << std::endl;
#endif
    }
    else if (layoutStack->inserted && layoutStack->next) {
#ifdef LAYOUT_BLOCK_DEBUG
      errStream << "  LAYOUT: " << startLoc 
                << " : close empty inserted block." << std::endl;
#endif
      // First token after '{' is at the previous (or earlier) indent
      // level, and it was an inserted open block. Close it immediately:
      pushTokenBack(tok);

      LToken newTok = LToken('}', startLoc, endLoc, "}");
      newTok.flags |= TF_INSERTED;
      newTok.prevTokType = tok.prevTokType;

      endBlock(newTok);

      return newTok;
    }
  }

  if (tok.flags & TF_FIRST_ON_LINE) {
    // Rule 3: If this is the first token on a line, then WHILE the
    // current indent level is less than the prevailing indent level,
    // close inserted blocks.

    // Consider possible right curly insertion and/or semicolon
    // insertion.

#ifdef LAYOUT_BLOCK_DEBUG
    errStream << "  LAYOUT: " << startLoc 
              << ": processing first token..." << std::endl;
#endif

    // This hack provided especially for printf-like things. Let indent
    // rules be violated for long printf strings:
    bool autoCloseOK = !(lastTokType == ','
                         || lastTokType == '('
                         || tok.tokType == ','
                         || tok.tokType == ')'
                         || false);

    if (autoCloseOK && closeToOffset(startLoc.offset)) {
      pushTokenBack(tok);

      LToken newTok = LToken('}', startLoc, endLoc, "}");
      newTok.flags |= TF_INSERTED|TF_AT_FIRST;
      newTok.prevTokType = tok.prevTokType;

      endBlock(newTok);
      return newTok;
    }
  }

  // Rule 4: At end of file, any outstanding inserted blocks are closed.
  if (tok.tokType == EOF) {
    if (layoutStack && layoutStack->inserted) {

      pushTokenBack(tok);

      LToken newTok = LToken('}', startLoc, endLoc, "}");
      newTok.flags |= TF_INSERTED|TF_AT_FIRST;
      newTok.prevTokType = tok.prevTokType;

      endBlock(newTok);
      return newTok;
    }

    return tok;
  }

  // Rule 5: If this is the first token on a line, and the indent
  // level is the SAME as the prevailing indent level, insert a
  // semicolon UNLESS:
  //   a) we just saw one, or
  //   b) we are closing the block explicitly.

  if (tok.flags & TF_FIRST_ON_LINE) {
    bool wantAutoSemi = !(lastTokType == ';' ||
                          lastTokType == '{' ||
                          lastTokType == ',' ||
                          lastTokType == '(' ||
                          lastTokType == '[' ||
                          lastTokType == '=' ||
                          lastTokType == tk_ASSIGN ||
                          tok.tokType == ')' ||
                          tok.tokType == ']' ||
                          tok.tokType == ';' ||
                          tok.tokType == '}' ||
                          tok.tokType == tk_THEN ||
                          tok.tokType == tk_ELSE ||
                          tok.tokType == tk_CASE ||
                          tok.tokType == tk_CATCH ||
                          tok.tokType == tk_OTHERWISE ||
                          tok.tokType == tk_UNTIL ||
                          tok.tokType == tk_IN ||
                          tok.tokType == tk_IS ||
                          tok.tokType == tk_DO ||
                          false);

    if (wantAutoSemi) {
      if (conditionallyInsertSemicolon(startLoc.offset)) {
#ifdef LAYOUT_BLOCK_DEBUG
        errStream << "  LAYOUT: " << startLoc 
                  << ": semicolon inserted at offset " 
                  << startLoc.offset << std::endl;
#endif
        pushTokenBack(tok);

        LToken newTok = LToken(';', startLoc, endLoc, ";");
        newTok.flags |= TF_INSERTED|TF_AT_FIRST;
        newTok.prevTokType = tok.prevTokType;

        return newTok;
      }
    }
  }

  // Open and close layout contexts when we see explicit open/close
  // curly braces:
  if (tok.tokType == '{')
    beginBlock(tok);

  if (tok.tokType == '}') {
    if (!(tok.flags & TF_INSERTED)) {
      // Following will stop closing blocks when it reaches the most
      // recent explicit block, which is what we want.
      if (closeToOffset(0)) {
        pushTokenBack(tok);

        LToken newTok = LToken('}', startLoc, endLoc, "}");
        newTok.flags |= TF_INSERTED|TF_AT_FIRST;
        newTok.prevTokType = tok.prevTokType;

        endBlock(newTok);
        return newTok;
      }
    }

    endBlock(tok);
  }

  return tok;
}

LToken
TransitionLexer::popToken()
{
  assert(havePushbackToken());
  
  LToken pbTok = pushbackTokens[pushbackTokens.size()-1];
  pushbackTokens.pop_back();
  pbTok.prevTokType = lastTokType;
  return pbTok;
}

#define RETURN_TOKEN(tok) do {               \
    LToken _tok = tok;                       \
    _tok.prevTokType = lastTokType;          \
    if (atBeginningOfLine) {                 \
      _tok.flags |= TF_FIRST_ON_LINE;        \
      atBeginningOfLine = false;             \
    }                                        \
    return _tok;                             \
  } while(false)

LToken
TransitionLexer::getNextInputToken()
{
  // For numbers:
  int radix = 10;

  here = skipWhiteSpaceAndComments();

  thisToken.erase();

  ucs4_t c = getChar();

  /////////////////////////////////////////////////////////
  // We've now eaten comments and white space, so whatver
  // location this is, this is where the new token starts:
  /////////////////////////////////////////////////////////
  LexLoc startLoc = here;
  LexLoc endLoc = here;

  switch (c) {
  case ':':
    {
      int tokID = ':';

      ucs4_t c2 = getChar();
      if (c2 == '=')
        tokID = tk_ASSIGN;
      else if (c2 == ':') {
        tokID = tk_BlkIdent;
      }
      else
        ungetChar(c2);

      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(tokID, startLoc, endLoc, thisToken));
    }
  case ';':
    {
      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(c, startLoc, endLoc, thisToken));
    }

  case '{':
    {
      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(c, startLoc, endLoc, thisToken));
    }
  case '}':
    {
      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(c, startLoc, endLoc, thisToken));
    }

  case '.':                        // Single character tokens
  case ',':
  case '[':
  case ']':
  case '(':
  case ')':
    {
      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(c, startLoc, endLoc, thisToken));
    }

  case '"':                        // String literal
    {
      do {
        c = getChar();

        if (c == '\\') {
          (void) getChar();        // just ignore it -- will validate later
        }
      } while (c != '"' && c != EOF);

      if (c == EOF) {
        errStream << startLoc
                  << ": Unterminated string constant. Missing end quote?"
                  << std::endl;
        num_errors++;
        RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
      }
      
      unsigned badpos = LitValue::validate_string(thisToken.c_str());

      if (badpos) {
        LexLoc badHere = startLoc;
        badHere.offset += badpos;
        errStream << badHere.asString()
                  << ": Illegal (non-printing) character in string '"
                  << thisToken << "'" << std::endl;
        num_errors++;
      }

      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(tk_String, startLoc, endLoc, 
                          thisToken.substr(1, thisToken.size()-2)));
    }

  case '\'':                        // Type variable or character literal.
    {
      // This can signal a type variable or a UCS4 codepoint literal,
      // depending on whether a close-single-quote is present.
      int tokType = tk_TypeVar;

      int c1 = getChar();       // first character after '
      int c2 = getChar();       // second character after '

      if (c1 == EOF || c2 == EOF) {
        endLoc.updateWith(thisToken);
        RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
      }

      // Check for simple, one-codepoint character:
      if (c2 == '\'') {
        /* This is of the form '??', where ?? is a single character
           that is not "'" or "\": */
        switch (c1) {
        case '\'':
        case '\\':
          {
            // These are valid_charpunct() but not actually legal in
            // non-escaped characters:
            ungetChar(c2);
            ungetChar(c1);
            endLoc.updateWith(thisToken);
            RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
          }
        default:
          {
            if (LitValue::DecodeCharacter(thisToken) >= 0) {
              endLoc.updateWith(thisToken);
              RETURN_TOKEN(LToken(tk_Char, startLoc, endLoc, thisToken));
            }

            ungetChar(c2);
            ungetChar(c1);
            endLoc.updateWith(thisToken);
            RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
          }          
        }
      }
      else if (c1 == '\\') {
        // Escaped character. We have already consumed the next
        // character into c2. Scan forward to the matching '\''
        // and then test the result by calling DecodeCharacter to
        // validate it:
        do {
          c = getChar();
          if (c == EOF) {
            endLoc.updateWith(thisToken);
            RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
          }
        } while (c != '\'' && c != EOF);

        if (c == EOF) {
          errStream << startLoc
                    << ": Unterminated character constant. Missing end quote?"
                    << std::endl;
          num_errors++;
          RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
        }

        if (LitValue::DecodeCharacter(thisToken) >= 0) {
          endLoc.updateWith(thisToken);
          RETURN_TOKEN(LToken(tk_Char, startLoc, endLoc, thisToken));
        }

        endLoc.updateWith(thisToken);
        RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
      }

      // Otherwise it is a type variable.

      // It must be a type variable:

      ungetChar(c2);

      if (c1 == '%') {
        tokType = tk_EffectVar;
        c1 = getChar();
      }

      if (!valid_tv_ident_start(c1)) {
        // FIX: this is bad input
        ungetChar(c1);
        endLoc.updateWith(thisToken);
        RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
      }

      do {
        c = getChar();
      } while (valid_tv_ident_continue(c));
      ungetChar(c);

      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(tokType, startLoc, endLoc, thisToken));
    }

    // Leading '-' no longer requires special handling now that we are
    // getting rid of the S-expression syntax. Unary negation is
    // handled in the parser in the new syntax.
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
      if (c == '0') {
        ucs4_t c2 = getChar();
        switch(c2) {
        case 'b':
          radix = 2;
          break;
        case 'x':
          radix = 16;
          break;
        case 'o':
          radix = 8;
          break;
        default:
          ungetChar(c2);
          // 0 followed by legal octal digit is a number:
          if (LitValue::digitValue(c2, 8) >= 0) {
            radix = 8;
            break;
          }
          // c still holds 0, which is okay.
        }
      }

      do {
        c = getChar();
      } while (LitValue::digitValue(c, radix) >= 0);

      /* We are either done with the literal, in which case it is an
         integer literal, or we are about to see a decimal point, in
         which case it is either a floating point literal or a
         language version. */
      if (c != '.') {
        ungetChar(c);
        int tokType = (thisToken[0] == '-') ? tk_NegativeInt : tk_Nat;

        endLoc.updateWith(thisToken);
        RETURN_TOKEN(LToken(tokType, startLoc, endLoc, thisToken));
      }

      // Language version number?
      if (currentLang & lf_version) {
        if (radix != 10) {
          ReportParseError(startLoc, "Language version number must be decimal.");
          RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
        }

        /* Looking for a language version number. */
        long count = 0;
        do {
          c = getChar();
          count++;
        } while (LitValue::digitValue(c, radix) >= 0);
        count--;
        ungetChar(c);
        endLoc.updateWith(thisToken);
        RETURN_TOKEN(LToken(tk_VersionNumber, startLoc, endLoc, thisToken));
      }

      // It's a floating point literal.
      {
        if (radix != 10) {
          ReportParseError(startLoc, "Floating point literals must be base 10.");
          RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
        }

        long count = 0;
        do {
          c = getChar();
          count++;
        } while (LitValue::digitValue(c, radix) >= 0);
        count--;
        /* FIX: if count is 0, number is malformed */
      }

      /* We are either done with this token or we are looking at a 'e'
         indicating start of an exponent. */
      if (c != 'e') {
        ungetChar(c);
        endLoc.updateWith(thisToken);
        RETURN_TOKEN(LToken(tk_Float, startLoc, endLoc, thisToken));
      }

      /* Need to collect the exponent. Revert to radix 10 until
         otherwise proven. */
      c = getChar();
      radix = 10;

      if (c != '-' && LitValue::digitValue(c, radix) < 0) {
        // FIX: Malformed token
      }

      do {
        c = getChar();
      } while (LitValue::digitValue(c, 10) >= 0);

      /* Check for radix marker on exponent */
      if (c == 'r') {
        radix = strtol(thisToken.c_str(), 0, 10);
        if (radix < 0) radix = -radix; // leading sign not part of radix

        long count = 0;
        do {
          c = getChar();
          count++;
        } while (LitValue::digitValue(c, radix) >= 0);
        count--;
        /* FIX: if count is 0, number is malformed */
      }

      ungetChar(c);
      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(tk_Float, startLoc, endLoc, thisToken));
    }

  case EOF:
    {
      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
    }

  default:
    if (valid_ident_start(c) || valid_operator_start(c) || c == '_') {
      goto identifier_or_operator;
    }

    // FIX: Malformed token
    endLoc.updateWith(thisToken);
    RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
  }

 identifier_or_operator:
  /// With the introduction of mixfix, the definition of a valid
  /// identifier has to change. Previously, it was basically a choice
  /// between "a classical alpha ident" and "stuff made of
  /// punctuation". Mixfix introduces two new wrinkles:
  ///
  ///  - We now need to accept '_' as legal for either type of
  ///    identifier 
  ///  - A mixfix definition effectively introduces multiple
  ///    quasi-keywords, and it is the individual keywords that need
  ///    to be recognizable as either "a classical alpha ident" or
  ///    "stuff made of punctuation".
  ///
  /// So the new rule for identifiers is that it consists of a
  /// sequence of "classical alpha ident" chunks and "stuff made of
  /// punctuation chunks" that are separated by a single underscore,
  /// may be optionally preceded by multiple underscores, and are
  /// optionally trailed by a single underscore.
  while(c == '_')
    c = getChar();

  // Match leading parsed hole
  if (c == '#') {
    switch(c = getChar()) {     // syntactic category
    case 'e':
      {
        c = getChar();
        switch (c) {
        case '_':               // no modifier
          ungetChar(c);
          break;
        case 'T':               // Thunk modifier
          break;
        default:
          goto malformed_ident;
        }
        break;
      }
    case 't':                   // no legal modifiers for now
    case 'k':
      break;
    default:
      goto malformed_ident;
    }

    c = getChar();
    if (c != '_') {
      goto malformed_ident;
    }

    c = getChar();
  }

  while (valid_ident_start(c) || valid_operator_start(c) || c == '_') {
    // Grab an alpha chunk or a punctuation chunk:
    if (valid_ident_start(c)) {
      do {
        c = getChar();
      } while (valid_ident_continue(c));
    }
    else if (valid_operator_start(c)) {
      do {
        c = getChar();
      } while (valid_operator_continue(c));
    }

    // We need special transitional handling to recognize set! for
    // use in S-expressions. This violates the longest match rule, but
    // note that it does so on an otherwise illegal token.
    //
    // This will go away soon.
    if (thisToken == "set!") {
      int tokType = kwCheck(thisToken.c_str(), tk_BlkIdent);
      endLoc.updateWith(thisToken);
      RETURN_TOKEN(LToken(tokType, startLoc, endLoc, thisToken));
    }

    if ((c != '#') && (c != '_') && (c != '@'))
      goto ident_done;

    // Match internal or trailing separator*
    if (c == '#') {
      switch(c = getChar()) {     // syntactic category
      case 'e':
        {
          c = getChar();
          switch (c) {
          case '_':               // no modifier
            ungetChar(c);
            break;
          case 'T':               // Thunk modifier
            break;
          default:
            goto malformed_ident;
          }
          break;
        }
      case 't':                   // no legal modifiers for now
      case 'k':
        break;
      default:
        goto malformed_ident;
      }

      c = getChar();
      if (c != '_') {
        goto malformed_ident;
      }
    }
    c = getChar();
  }
 ident_done:

  // We have gone one too far.
  ungetChar(c);

  // Might have matched trailing '@' in error. If so, back it out:
  if (thisToken[thisToken.size()-1] == '@') {
    ReportParseError(startLoc, thisToken + 
                     " is not a well-formed identifier. Trailing '@' is not valid.");
    RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
  }
  else {
    int tokType = kwCheck(thisToken.c_str(), tk_BlkIdent);
    endLoc.updateWith(thisToken);
    RETURN_TOKEN(LToken(tokType, startLoc, endLoc, thisToken));
  }

 malformed_ident:
  ReportParseError(startLoc, thisToken + 
                   " is not a well-formed identifier.");
  RETURN_TOKEN(LToken(EOF, startLoc, endLoc, "end of file"));
}
