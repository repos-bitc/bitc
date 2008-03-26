/* -*- Mode: c -*- */
/*
 * Copyright (C) 2006, The EROS Group, LLC.
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
 */

%{

#include <sys/types.h>
#include <gmp.h> /* GNU multiple precision library: */
#include <string.h>
#include <assert.h>
#include <dirent.h>
#include <unicode/uchar.h>
 
#include <string>

#include <libsherpa/utf8.hxx>
#include "BitcLexer.hxx"

#include "BitcParser.hxx"
#define YY_NO_UNPUT

#define UPDATE_POSITION() \
                do {                                    \
		  if ( yy_flex_debug )                  \
                    errStream << "Position was "	\
                      << here << "\n";                  \
                  here.updateWith(yytext);              \
		  if ( yy_flex_debug )                  \
                    errStream << "Position now " 	\
                      << here << "\n";                  \
                } while (0)

#define STOK(x,begin,len)  do { \
                  std::string bs = yytext;		\
                  bs = bs.substr(begin, len);           \
                  lvalp->tok = LToken(here, bs);        \
                  UPDATE_POSITION();                    \
                  return x; } while (0)

#define TOK(x)  do { \
                  lvalp->tok = LToken(here, yytext);    \
                  UPDATE_POSITION();                    \
                  return x; } while (0)

#define IGNORE() \
                do {                                    \
                  UPDATE_POSITION();                    \
                } while (0)

#undef YY_DECL
#define YY_DECL int BitcLexer::lex(ParseType* lvalp)

#define YY_MAIN 0

  // Flex skeleton code is broken, so we need to open up the std:: namespace.
using namespace std;
using namespace sherpa;

static bool
valid_char_printable(uint32_t ucs4)
{
  if (strchr("!#$%&`()*+-.,/:;<>=?@_|~^[]'", ucs4))
    return true;
  return false;
}

#if 0
static bool
valid_ident_punct(uint32_t ucs4)
{
  if (strchr("!$%&*+-/<>=?@_~", ucs4))
    return true;
  return false;
}

static bool
valid_ident_start(uint32_t ucs4)
{
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_START) || 
	  valid_ident_punct(ucs4));
}

static bool
valid_ident_continue(uint32_t ucs4)
{
  return (u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE) ||
	  valid_ident_punct(ucs4));
}
#endif
static bool
valid_ifident_start(uint32_t ucs4)
{
  return (isalpha(ucs4) || ucs4 == '_');
  //  return (u_hasBinaryProperty(ucs4,UCHAR_XID_START));
}

static bool
valid_ifident_continue(uint32_t ucs4)
{
  return (isalpha(ucs4) || isdigit(ucs4) || ucs4 == '_' || ucs4 == '-');
  //  return (u_hasBinaryProperty(ucs4,UCHAR_XID_CONTINUE) ||
  //valid_ifident_punct(ucs4));
}

static bool
valid_charpoint(uint32_t ucs4)
{
  if (ucs4 == (uint32_t) '{')
    return false;

  if (ucs4 == (uint32_t) '}')
    return false;

  if (valid_char_printable(ucs4))
    return true;

  return u_isgraph(ucs4);
}

static long
validate_string(const char *s)
{
  const char *spos = s;
  uint32_t c;

  while (*spos) {
    const char *snext;
    c = sherpa::utf8_decode(spos, &snext);

    if (c == ' ') {		/* spaces are explicitly legal */
      spos = snext;
    }
    else if (c == '\\') {	/* escaped characters are legal */
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
	  while (*spos != '}')
	    spos++;
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

%}

%option c++
%option yyclass="BitcLexer"
%option noyywrap
%option prefix="bitc"

BINDIGIT       [01]
DECDIGIT       [0-9]   
OCTDIGIT       [0-7]
HEXDIGIT       [0-9a-fA-F]
ALPHA          [A-Za-z]
ALNUM          [A-Za-z0-9]
PRNTPUNCT      [!#$%&`()*+\-.,/\:;<>=?@_|~^\[\]\' \t]

/* Matcher for UTF-8 unicode characters. Trailing _N is number of
   bytes in this encoding. The 5 and 6 byte encodings are officially
   illegal now, but may still be used by some legacy inputs for
   private code point spaces.
*/
/* Single byte encodings 0 to 127. This excludes three code points
        \042 (double quote)
        \134 (backslash)
        \173 (left curly)
   because these have escaped significance within strings or
   characters.
*/
UTF8_1       ([\000-\041\043-\133\135-\172\174])
/* First byte 128..223: */
UTF8_2       ([\200-\337]([\200-\277]{1}))
/* First byte 224..239: */
UTF8_3       ([\340-\357]([\200-\277]{2}))
/* First byte 240..247: */
UTF8_4       ([\360-\367]([\200-\277]{3}))
/* First byte 248..251: */
UTF8_5       ([\370-\373]([\200-\277]{4}))
/* First byte 252..253: */
UTF8_6       ([\374-\375]([\200-\277]{5}))

/* Note that if we consider only the single-byte UTF-8 encodings, then
   
       UNICODE ID_Start is    {ALPHA}
       UNICODE ID_Continue is {ALNUM} + '_'

   Since '_' is part of PRNTPUNCT, we don't worry about it here.

   Multibyte encodings may not match the various required constraints,
   but they are invalid in any context EXCEPT identifiers, strings,
   and character literals.

   So we exploit a sleazy trick. We only match the single-byte
   sequences carefully at the lexer level. We match multibyte
   sequences promiscuously and then check them in the action rule.
*/

 /* Valid code points within a string, excluding the ones that require
    escaping: */
S_UTF8       ({UTF8_1}|{UTF8_2}|{UTF8_3}|{UTF8_4}|{UTF8_5}|{UTF8_6})
 /* Valid characters, excluding the ones that require escaping: */
C_UTF8       ({ALNUM}|{PRNTPUNCT}|{UTF8_2}|{UTF8_3}|{UTF8_4}|{UTF8_5}|{UTF8_6})

/* IDENTIFIERS: 
 VERY IMPORTANT: If the list of special characters changes, must
 update the (de)mangling routines in c-gen.cxx. 
 // c-gen is not currently being used. 
*/

SPECIAL        [!$%&*+\-/<>=?@_~]
ID_START       ({ALPHA}|{SPECIAL})
ID_CH          ({ALNUM}|{SPECIAL})
IDENT          ({ID_START}{ID_CH}*)

MOD_ID_START   ({ALPHA}| [_\-])
MOD_ID_CH      ({ALPHA}|{DECDIGIT}| [_\-])
MOD_IDENT      ({MOD_ID_START}{MOD_ID_CH}*)


BINNUMBER      (2r{BINDIGIT}+)
OCTNUMBER      (8r{OCTDIGIT}+)
HEXNUMBER      (16r{HEXDIGIT}+)
DECNUMBER      ((10r)?{DECDIGIT}+)

BINFLOAT       (2r{BINDIGIT}+\.{BINDIGIT}+)
OCTFLOAT       (8r{OCTDIGIT}+\.{OCTDIGIT}+)
DECFLOAT       ((10r)?{DECDIGIT}+\.{DECDIGIT}+)
HEXFLOAT       (16r{HEXDIGIT}+\.{HEXDIGIT}+)


NATURAL        ({BINNUMBER}|{OCTNUMBER}|{HEXNUMBER}|{DECNUMBER})
INTEGER        ((\-)?{NATURAL})

MANTISSA       ({BINFLOAT}|{OCTFLOAT}|{HEXFLOAT}|{DECFLOAT})
FLOAT          ((\-)?{MANTISSA}(^{INTEGER})?)

/* Encodings for character literals */
C_SPACE        (\{space\}|space)
C_TAB          (\{tab\}|tab)
C_LF           (\{linefeed\}|linefeed)
C_CR           (\{return\}|return)
C_BS           (\{backspace\}|backspace)
C_LBRACE       (\{lbrace\}|lbrace)
C_RBRACE       (\{rbrace\}|rbrace)
C_DQUOTE       (\")
C_CCODE        (\{{NATURAL}\})
C_UCODE        (\{U\+{HEXDIGIT}+\})

CHAR_WS        ({C_SPACE}|{C_TAB}|{C_LF}|{C_CR}|{C_BS})
CHAR_OTHER     ({C_LBRACE}|{C_RBRACE}|{C_DQUOTE}|{C_CCODE}|{C_UCODE})

S_LF           (\\n|\\\{linefeed\})
S_CR           (\\r|\\\{return\})
S_TAB          (\\t|\\\{tab\})
S_BS           (\\b|\\\{backspace\})
S_SPACE        (\\s|\\\{space\})
S_FF           (\\f)
S_BSLASH       ("\\")
S_CURLY        ("{")
S_DQUOTE       (\\\")
S_UCODE        (\\\{U\+{HEXDIGIT}+\})

S_WS           ({S_LF}|{S_CR}|{S_TAB}|{S_BS}|{S_SPACE}|{S_FF})
S_OTHER        ({S_DQUOTE}|{S_BSLASH}|{S_UCODE}|{S_CURLY})
SCHAR          ({S_UTF8}|{S_WS}|{S_OTHER})

%%
[\;][^\n\r]*            {  IGNORE();                   }
[ \t\n\r]+              {  IGNORE();                   }
\.	                {  TOK('.');                   }
\,	                {  TOK(',');                   }
\[	                {  TOK('[');                   }
\]	                {  TOK(']');                   }
\(	                {  TOK('(');                   }
\)	                {  TOK(')');                   }
\:	                {  TOK(':');                   }
"^"                     {  TOK('^');                   }

"#f"                    {  TOK(tk_TRUE);               }
"#t"                    {  TOK(tk_FALSE);              }
{FLOAT}                 {  TOK(tk_Float);              }
{INTEGER}               {  TOK(tk_Int);                }

{IDENT}                 {  TOK(kwCheck(yytext)); }
\'{IDENT}               {  TOK(tk_TypeVar);            }

\#\\{CHAR_WS}           {  TOK(tk_Char);               }
\#\\{CHAR_OTHER}        {  TOK(tk_Char);               }
\#\\{C_UTF8}            {  uint32_t codePoint = 
                             sherpa::utf8_decode(yytext+2, 0);
                           if (!valid_charpoint(codePoint)) {
			     errStream << here.asString()
				       << ": Illegal character literal '"
				       << yytext << "'\n";
			     num_errors++;
			     // But let processing continue...
			   }
                           TOK(tk_Char);               }
\"{SCHAR}*\"            {  unsigned badpos = validate_string(yytext);
                           if (badpos) {
			     LexLoc badHere = here;
			     badHere.offset += badpos;
			     errStream << badHere.asString()
				       << ": Illegal (non-printing) character in string '"
				       << yytext << "'\n";
			     num_errors++;
	                   }
                           STOK(tk_String,1,yyleng-2); }

.              { /* all else has failed */
                 errStream << here.asString()
			   << ": Illegal input character '"
			   << yytext[0] << "'\n";
		 exit(1);
	       }
%%
void
BitcLexer::ReportParseError()
{
  errStream << here
	    << ": syntax error (via yyerror)" << '\n';
  num_errors++;
}
 
void
BitcLexer::ReportParseError(std::string msg)
{
  errStream << here
	    << ": "
	    << msg << std::endl;

  num_errors++;
}

void
BitcLexer::ReportParseWarning(std::string msg)
{
  errStream << here
	    << ": "
	    << msg << std::endl;
}

BitcLexer::BitcLexer(std::ostream& err, std::istream& in, Path *inFile)
  : bitcFlexLexer(&in), errStream(err)
{
  here = LexLoc(inFile, 1, 0);
  num_errors = 0;
  isRuntimeUoc = false;
  ifIdentMode = false;
#if 0
  yyrestart(in);
  yy_init = 1;		/* whether we need to initialize */
  yy_start = 0;	/* start state number */
  yy_flush_buffer();
#endif
}

void 
BitcLexer::setDebug(bool showlex)
{
  set_debug(showlex ? 1 : 0);
}

struct KeyWord {
  const char *nm;
  int  tokValue;
} keywords[] = {
  { "and",              tk_AND },
  { "apply",            tk_APPLY },
  { "array",            tk_ARRAY },
  { "array-length",     tk_ARRAY_LENGTH },
  { "array-nth",        tk_ARRAY_NTH },
  { "assert",           tk_Reserved },
  { "begin",            tk_BEGIN },
  { "bitc-version",     tk_BITC_VERSION },
  { "bitfield",         tk_BITFIELD },
  { "bool",             tk_BOOL },
  { "case",             tk_CASE },
  { "catch",            tk_CATCH },
  { "char",             tk_CHAR },
  { "check",            tk_Reserved },
  { "coindset",         tk_Reserved },
  { "cond",             tk_COND },
  { "constrain",        tk_Reserved },
  { "declare",          tk_DECLARE },
  { "defequiv",         tk_Reserved },
  { "defexception",     tk_DEFEXCEPTION },
  { "define",           tk_DEFINE },
  { "definstance",      tk_DEFINSTANCE },
  { "definvariant",     tk_Reserved },
  { "defobject",        tk_Reserved },
  { "defrefine",        tk_Reserved },
  { "defrepr",          tk_DEFREPR },
  { "defstruct",        tk_DEFSTRUCT },
  { "defthm",           tk_DEFTHM },
  { "deftheory",        tk_Reserved },
  { "deftypeclass",     tk_DEFTYPECLASS },
  { "defunion",         tk_DEFUNION },
  { "defvariant",       tk_Reserved },
  { "deref",            tk_DEREF },
  { "disable",          tk_Reserved },
  { "do",               tk_DO },
  { "do*",              tk_Reserved },
  { "double",           tk_DOUBLE },
  { "dup",              tk_DUP },
  { "enable",           tk_Reserved },
  { "exception",        tk_EXCEPTION },
  { "external",         tk_EXTERNAL },
  { "fill",             tk_FILL },
  { "float",            tk_FLOAT },
  { "fn",               tk_FN },
  { "forall",           tk_FORALL },
  { "hide",             tk_HIDE },
  { "if",               tk_IF },
  { "import",           tk_IMPORT },
  { "import!",          tk_Reserved },
  { "indset",           tk_Reserved },
  { "int16",            tk_INT16 },
  { "int32",            tk_INT32 },
  { "int64",            tk_INT64 },
  { "int8",             tk_INT8 },
  { "interface",        tk_INTERFACE },
  { "label",            tk_Reserved },
  { "lambda",           tk_LAMBDA },
  { "let",              tk_LET },
  { "let*",             tk_Reserved },
  { "letrec",           tk_LETREC },
  { "literal",          tk_Reserved },
  { "location",         tk_Reserved },
  { "make-vector",      tk_MAKE_VECTOR },
  { "member",           tk_MEMBER },   /* REDUNDANT */
  { "method",           tk_METHOD },
  { "mutable",          tk_MUTABLE },
  { "namespace",        tk_Reserved },
  { "not",              tk_NOT },
  { "nth",              tk_Reserved },
  { "opaque",           tk_OPAQUE },
  { "or",               tk_OR },
  { "otherwise",        tk_OTHERWISE },
  { "pair",             tk_PAIR },
  { "proclaim",         tk_PROCLAIM },
  { "provide",          tk_PROVIDE },
  { "provide!",         tk_Reserved },
  { "quad",             tk_QUAD },
  { "read-only",        tk_Reserved },
  { "ref",              tk_REF },
  { "sensory",          tk_Reserved },
  { "set!",             tk_SET },
  { "size-of",          tk_Reserved },
  { "string",           tk_STRING },
  { "super",            tk_Reserved },
  { "suspend",          tk_SUSPEND },  
  { "switch",           tk_SWITCH },
  { "tag",              tk_TAG },
  { "the",              tk_THE },
  { "throw",            tk_THROW },
  { "try",              tk_TRY },
  { "tycon",            tk_Reserved },
  { "tyfn",             tk_TYFN },
  { "uint16",           tk_UINT16 },
  { "uint32",           tk_UINT32 },
  { "uint64",           tk_UINT64 },
  { "uint8",            tk_UINT8 },
  { "use",              tk_USE },
  { "using",            tk_Reserved },
  { "val",              tk_VAL },
  { "value-at",         tk_Reserved },
  { "vector",           tk_VECTOR },
  { "vector-length",    tk_VECTOR_LENGTH },
  { "vector-nth",       tk_VECTOR_NTH },
  { "word",             tk_WORD }
  
  //{ "immutable",        tk_IMMUTABLE },
  //{ "module",           tk_MODULE },
  //{ "mutual-recursion", tk_MUTUAL_RECURSION },
  //{ "nat",              tk_NAT },
  //{ "package",          tk_PACKAGE }, 
  //{ "restricted-ref",   tk_RESTRICTEDREF },
  //{ "sequence-length",  tk_SEQUENCELENGTH },
  //{ "sequence-ref",     tk_SEQUENCEREF }, /* REDUNDANT */

};

static int
kwstrcmp(const void *vKey, const void *vCandidate)
{
  const char *key = ((const KeyWord *) vKey)->nm;
  const char *candidate = ((const KeyWord *) vCandidate)->nm;

  return strcmp(key, candidate);
}

int
BitcLexer::kwCheck(const char *s)
{
  if (ifIdentMode) {
    if (!valid_ifident_start(*s))
      return tk_Reserved;

    for (++s; *s; s++)
      if (!valid_ifident_continue(*s))
	return tk_Reserved;

    return tk_Ident;
  }

  KeyWord key = { s, 0 };
  KeyWord *entry = 
    (KeyWord *)bsearch(&key, keywords,
		       sizeof(keywords)/sizeof(keywords[0]), 
		       sizeof(keywords[0]), kwstrcmp);

  // If it is in the token table, return the indicated token type:
  if (entry)
    return entry->tokValue;

  // Otherwise, check for various reserved words:

  // Things starting with "__":
  if (s[0] == '_' && s[1] == '_') {
    if(!isRuntimeUoc)
      return tk_Reserved;
  }

  // Things starting with "def":
  if (s[0] == 'd' && s[1] == 'e' && s[2] == 'f')
    return tk_Reserved;

  // Things starting with "#":
  if (s[0] == '#')
    return tk_Reserved;

  return tk_Ident;
}
