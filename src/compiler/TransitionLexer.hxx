#ifndef TRANSITIONLEXER_HXX
#define TRANSITIONLEXER_HXX

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

#include <iostream>

#include "ParseType.hxx"
#include "libsherpa/EnumSet.hxx"

typedef long ucs4_t;

class PushBack {
  // Needs to be the size of the largest token that we might have to
  // push back due to layout, but not less than four.
  enum { maxPushBack = 8 };

  long stack[maxPushBack];
  unsigned depth;

public:
  inline void push(ucs4_t c) {
    assert (depth < maxPushBack);
    stack[depth] = c;
    depth++;
  }

  inline long pop() {
    if (depth == 0)
      return -1;
    depth--;
    return stack[depth];
  }

  PushBack() {
    depth = 0;
  }
};

enum LayoutFlagValues {
  /// @brief A left brace is required as the next token, and we should
  /// insert one if not found.
  NEED_LBRACE = 0x1u,

  /// @brief We have taken a newline and might need to insert a
  /// right brace[s] and or a semicolon before the next token
  /// according to its indent level.
  CHECK_FIRST_TOKEN = 0x2u,

  /// @brief Set iff we are currently unwinding the layout stack.
  TRIMMING_LAYOUT_STACK = 0x4u,
};
typedef sherpa::EnumSet<LayoutFlagValues> LayoutFlags;


struct LayoutFrame : public boost::enable_shared_from_this<LayoutFrame> {
  bool implicit;              // true IFF left curly was implicit
  unsigned column;            // column of first token after '{'
  int precedingToken;         // token that preceded this '{'
  bool dead;

  boost::shared_ptr<LayoutFrame> next;

  static inline boost::shared_ptr<LayoutFrame>
  make(int _precedingToken, bool _implicit, unsigned _column) {
    LayoutFrame *lf = new LayoutFrame;

    lf->implicit = _implicit;
    lf->precedingToken = _precedingToken;
    lf->column = _column;
    lf->dead = false;

    return boost::shared_ptr<LayoutFrame>(lf);
  }
};

/** @brief Hand-crafted transitional syntax lexer.
 *
 * TransitionLexer is yet another variant of shap's generic hand-crafted
 * lexer. The main job of TransitionLexer is to generate tokens (instances
 * of LToken) and keep track of input positions in the input stream
 * (via LexLoc). In addition, TransitionLexer serves as a holding box for
 * some meta-information about the current unit of compilation.
 *
 * Note that the @p inStream and @p errStream slots are by-reference,
 * which means that a TransitionLexer should not survive the streams that
 * it is processing.
 */
struct TransitionLexer {
private:
  int lastToken;

  boost::shared_ptr<LayoutFrame> layoutStack;
  LayoutFlags layoutFlags;

  void beginBlock(bool implicit);
  void endBlock(bool implicit);

  /// @brief Mark layout stack frames dead until (a) we reach one that
  /// was explicit, or (b) we reach one preceded by a bracketing match
  /// for the indicated token.
  void closeToOpeningToken(int closingToken);
  void closeToOffset(unsigned offset);
  bool conditionallyInsertSemicolon(unsigned offset);

  /// @brief If the top layout stack entry is dead, drop it, update
  /// the layout context accordingly, and return true.
  bool trimLayoutStack();

public:
  enum LangFlagsValues {
    lf_sexpr = 0x1u,
    lf_block = 0x2u,
    lf_transition = 0x3u,
    lf_version = 0x4u
  };

  int lispParenDepth;

  typedef sherpa::EnumSet<LangFlagsValues> LangFlags;

  LangFlags currentLang;

  /** @brief Start position of current token. */
  sherpa::LexLoc here;

  /** @brief Number of parse errors incurred.
   *
   * The current parser is not particularly good at context recovery,
   * and I don't really see much point to improving the error handling
   * when we are likely to abandon the LISP-style surface syntax
   * entirely. In a hypothetical future parser, we would want to check
   * num_errors before admitting a unit of compilation into the
   * compile phase.*/
  int num_errors;
  /** @brief Whether lexer debugging output is enabled.
   *
   * If @p debug is true, then the lexer will dump each token that it
   * sees to the error stream. */
  bool debug;
  /** @brief True @em iff the current unit of compilation is part of
   * the BitC runtime.
   *
   * This alters the criteria for identifier admission. If the unit is
   * part of the BitC runtime, it is entitled to admit symbols
   * starting with double underscore. This is set from the parser.
   */
  bool isRuntimeUoc;
  /** @brief Whether we are presently looking for identifiers that
   * satisfy the interface identifier name restrictions.
   *
   * Initially false. Toggled from the parser. */
  bool ifIdentMode;
  /** @brief Whether we are looking at a unit of compilation that was
   * admitted from the command line, as opposed to one that we
   * imported.
   *
   * @bug This is misnamed, since input from stdin in an interactive
   * implementation should have this true as well. It should probably
   * be called isImportInput instead (sense invert).
   */
  bool isCommandLineInput;
  /** @brief The input stream that we are processing */
  std::istream& inStream;
  /** @brief The error stream to which errors should be reported */
  std::ostream& errStream;

  /** @brief Number of modules that we have seen in the current
   * parse.
   *
   * Initialized to zero. This is parser state that is placed here for
   * convenience. It is used to give every source module a unique name
   * when multiple modules appear in a single source unit of compilation.
   */
  unsigned nModules;

private:
  /** @brief Collected characters for current token.
   *
   * These are accumulated by getChar() and released (if appropriate)
   * by ungetChar() */
  std::string thisToken;

  PushBack pushBackStack;

  /** @brief Fetch next character from input stream. */
  ucs4_t getChar();
  /** @brief Push a lookahead character back onto the input stream. */
  void ungetChar(ucs4_t);
  void ungetThisToken();

  bool valid_ascii_symbol(ucs4_t ucs4);

  bool valid_operator_start(ucs4_t ucs4);
  bool valid_operator_continue(ucs4_t ucs4);

  bool valid_ident_start(ucs4_t ucs4);
  bool valid_ident_continue(ucs4_t ucs4);
  bool valid_ifident_start(ucs4_t ucs4);
  bool valid_ifident_continue(ucs4_t ucs4);
  bool valid_tv_ident_start(ucs4_t ucs4);
  bool valid_tv_ident_continue(ucs4_t ucs4);
public:

  /** @brief Constructor
   *
   * Instantiate a new TransitionLexer drawing input from @p inStream and
   * reporting errors to @p errStream. Use @p origin as the name of
   * the containing "file" for any errors. Set @p commandLineInput to
   * false if this unit of compilation was imported rather than
   * processed from the command line or the user */
  TransitionLexer(std::ostream& errStream, std::istream& inStream, 
             const std::string& origin,
             bool commandLineInput);

  /** @brief Report parse error @p msg attributed to a particular
   * location @p loc. */
  void ReportParseError(const sherpa::LexLoc& loc, std::string  msg);
  /** @brief Report parse warning @p msg attributed to a particular
   * location @p loc. */
  void ReportParseWarning(const sherpa::LexLoc& loc, std::string msg);

  /** @brief Report generic syntax error attributed to current input location. */
  void ReportParseError();
  /** @brief Report parse error @p msg attributed to current input location. */
  void ReportParseError(std::string msg)
  {
    ReportParseError(here, msg);
  }
  /** @brief Report parse warning @p msg attributed to current input location. */
  inline void ReportParseWarning(std::string msg)
  {
    ReportParseWarning(here, msg);
  }

  /** @brief Issue debugging output on iff @p showlex is true */
  inline void setDebug(bool showlex)
  {
    debug = (showlex ? true : false);
  }

  /** @brief Restrict identifiers to interface identifiers iff @p arg
      is true. */
  inline void setIfIdentMode(bool arg)
  {
    ifIdentMode = arg;
  }

  /** @brief Fetch next token, return result via @p yylvalp. */
  int lex(ParseType *yylvalp);

  /** @brief Destructor */
  ~TransitionLexer() {}

  /** @brief Structure type for keyword table.
   *
   * Public so that it can be accessed from the comparison function
   * within TransitionLexer.cxx.  */
  struct KeyWord {
    const char *nm;
    /** @brief Which language variants accept this keyword.
     *
     */
    LangFlags whichLang;
    int sexprTokValue;
    int blockTokValue;

    KeyWord(const char *_nm, LangFlags _whichLang, int _tokValue);
    KeyWord(const char *_nm, LangFlags _whichLang, int _lispTokValue, int _blockTokValue);
  };

private:
  /** @brief Sorted List of keywords and their token numbers. */
  static KeyWord keywords[];

  /** @brief Return appropriate token number for argument string @p s.
   *
   * If @p s is a keyword, this will return the appropriate keyword
   * token number by consulting @p keywords. If the string does not
   * appear in @p keywords, return the token number meaning
   * identifier.
   */
  int kwCheck(const char *s, int identType);

  /** @brief Consume any comments and white space that precede the
   * next token.
   *
   * This can have the side effect of setting the CHECK_FIRST_TOKEN
   * flag in the LayoutFlags, because it handles newline processing.
   */
  sherpa::LexLoc skipWhiteSpaceAndComments();

  bool havePushbackToken;
  sherpa::LToken pushbackToken;

  /** @brief Fetch next token to hand to the parser.
   *
   * This calls getNextInputToken(), applies layout rules, and either
   * returns the results or pushes the input token back and
   * substitutes a token generated according to the layout rules.
   */
  sherpa::LToken getNextToken();

  /** @brief Push a token back onto the input stream.
   *
   * Used in some cases by layout processing.
   */
  void pushTokenBack(sherpa::LToken& tok);

  /** @brief Fetch next token from the input stream.
   *
   * This is the actual work-horse procedure. The one below is a
   * wrapper for debugging purposes.
   */
  sherpa::LToken getNextInputToken();
};

#endif /* TRANSITIONLEXER_HXX */

