/**************************************************************************
 *
 * Copyright (C) 2010, Jonathan S. Shapiro
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

/** @file
 * 
 * @brief Implementation of mixfix mechanism and associated parsing. See
 * @ref MixFixProcessing
 * for a detailed discussion of how BitC
 * handles MixFix.
 * 
 * This is a PLACEHOLDER implementation. I'm using a fixed
 * operator table while I get this debugged, and there is a bunch of
 * global state in this file that needs to be gathered into a suitable
 * object/structure once I sort out the implementation.
 * 
 * Part of the question at hand is that it's not clear how early the
 * mixfix stuff needs to be taken into account. Some questions in my
 * mind at the moment:
 * 
 * - Does the lexer need to know about mixfix, or can we do late
 *   processing of syntactic categories here, after symbol resolution
 *   has happened?
 * 
 * - Does the @em parser need to know about mixfix, or can we do late
 *   processing of syntactic categories here, after symbol resolution
 *   has happened?
 * 
 * - When this is all over, will we still need the `op` syntax at
 *   all? I hope not, but if so, what syntactic category should it be
 *   given?
 */

#include <limits.h>
#include <iostream>
#include "shared_ptr.hxx"

#include "Options.hxx"
#include "AST.hxx"
#include "MixFix.hxx"
#include "UocInfo.hxx"
#include "libsherpa/INOstream.hxx"

#include "BUILD/TransitionParser.hxx"

// Debug levels:
// 5 = show non-candidate rules
// 4 = rule matching
// 3 = shifting
// 2 = reduce actions
// 1 = Summary input and result
#define MIXDEBUG(n) if (Options::mixfixDebug >= (n))

using namespace std;
using namespace boost;
using namespace sherpa;

typedef shared_ptr<AST> ASTPtr;
typedef std::vector<ASTPtr> ASTVec;
typedef std::vector<std::string> StringVec;

// We are going to use a vector as a stack here, because the
// std::stack implementation does not provide indexed access, and we
// are going to need that.

template<class T>
static inline
const T& top(const std::vector<T>& vec)
{
  assert(vec.size() >= 1);
  return vec[vec.size()-1];
}

template<class T>
static inline
T& top(std::vector<T>& vec)
{
  assert(vec.size() >= 1);
  return vec[vec.size()-1];
}

template<class T>
static inline
const T& topNext(const std::vector<T>& vec)
{
  assert(vec.size() >= 2);
  return vec[vec.size()-2];
}

template<class T>
static inline
void push(std::vector<T>& vec, const T& elem)
{
  vec.push_back(elem);
}

template<class T>
static inline
T pop(std::vector<T>& vec)
{
  T elem = top(vec);
  vec.pop_back();
  return elem;
}


struct QuasiKeyword {
  /// @brief Number of times this quasi-keyword appears in a rule.
  ///
  /// If it appears twice in a given rule, it's counted twice.
  int nOccur;

  /// @brief Number of rules where this quasi-keyword is the first
  /// element of a rule.
  int nFirst;

  /// @brief Number of rules where this quasi-keyword is used just
  /// before a trailing hole.
  int nPre;

  /// @brief Number of rules where this quasi-keyword is used just
  /// after a leading hole.
  int nPost;

  QuasiKeyword()
  {
    nOccur = nPre = nPost = nFirst = 0;
  }
};

struct QuasiKeywordMap {
  std::map<std::string, QuasiKeyword> theMap;

  QuasiKeywordMap()
  {
  }

  void add(std::string qkwd);
  void remove(std::string qkwd);

  QuasiKeyword& operator[](std::string theKey)
  {
    return theMap[theKey];
  }

  bool contains(std::string theKey)
  {
    return (theMap.find(theKey) != theMap.end());
  }

  size_t erase(std::string theKey)
  {
    return theMap.erase(theKey);
  }
};

static const std::string ThunkMarker = "#_";
static const std::string HoleMarker = "_";
static const std::string KwdSeparator = "@";

struct MixRule : public boost::enable_shared_from_this<MixRule> {
  /// @brief Summary name of this mix rule
  std::string name;

  /// @brief Exploded elements of the right-hand side of the production.
  StringVec rhs;

  /// @brief Fixity - extracted mainly for doc and debug purposes
  int fixity;

  /// @brief Precedence for this production (infix only).
  int prec;

  /// @brief Associativity for this rule.
  ///
  /// This can only be assoc_left or assoc_right for infix rules.
  Associativity assoc;

  /// @brief Return true iff this is an infix rule.
  bool isInfix()   const { return fixity == infix; }
  bool isPrefix()  const { return fixity == prefix; }
  bool isClosed()  const { return fixity == closed; }
  bool isPostfix() const { return fixity == postfix; }

  bool isInitialHole(size_t pos) {
    return ((pos == rhs.size() - 1) &&
            (rhs[pos] == HoleMarker ||
             rhs[pos] == ThunkMarker));
  }

  bool isFinalHole(size_t pos) {
    return ((pos == rhs.size() - 1) &&
            (rhs[pos] == HoleMarker ||
             rhs[pos] == ThunkMarker));
  }

  bool isHole(size_t pos) {
    return (rhs[pos] == HoleMarker ||
            rhs[pos] == ThunkMarker);
  }

  bool isKwd(size_t pos) {
    return (rhs[pos] != HoleMarker &&
            rhs[pos] != ThunkMarker);
  }

  bool hasLeadingHole() 
  {
    return (fixity == infix) || (fixity == postfix);
  }
  bool hasTrailingHole() 
  {
    return (fixity == infix) || (fixity == prefix);
  }

private:
  /// @brief Helper to be used during construction
  void extract();

public:
  MixRule(const std::string& _nm, int _prec, Associativity _assoc = assoc_none)
  {
    name = _nm;
    prec = _prec;
    assoc = _assoc;
    extract();
  }

  static shared_ptr<MixRule> 
  make(const std::string& _nm, int _prec, Associativity _assoc = assoc_none)
  {
    MixRule *tmp = new MixRule(_nm, _prec, _assoc);
    return boost::shared_ptr<MixRule>(tmp);
  }

  void PrettyPrint(INOstream& out) const;
};
typedef shared_ptr<MixRule> MixRulePtr;

INOstream& operator<<(INOstream& out, MixRule& rule)
{
  rule.PrettyPrint(out);
  return out;
}


struct MixFixNode {
  /// @brief AST for this node.
  ASTPtr ast;

  /// @brief Rule that produced this node.
  MixRulePtr rule;

  MixFixNode(ASTPtr _ast, MixRulePtr _rule)
  {
    ast = _ast;
    rule = _rule;
  }

  bool matchesKwd(const std::string& kwd) const {
    assert(ast);
    return (ast->astType == at_ident &&
            ast->s == kwd);
  }

  void PrettyPrint(sherpa::INOstream& out, bool showTypes = false, bool endline = true)
  {
    if (ast->astType != at_ident)
      out << '{';

    ast->PrettyPrint(out, showTypes, false);

    if (ast->astType != at_ident)
      out << '}';

    if (endline)
      out << endl;
  }

  bool isKwd() const
  { return (rule) ? false : true; }

  bool isExpr() const 
  { return (rule) ? true : false; }
};

typedef std::vector<MixFixNode> MixInput;

struct MixContext {
  typedef std::set<MixRulePtr> RuleSet;
  RuleSet rules;
  QuasiKeywordMap kwMap;

  void add(MixRulePtr rule);
  void remove(MixRulePtr rule);

  MixRulePtr
  ParseOneMixFix(sherpa::INOstream& errStream, MixInput& origInput, 
                 MixRulePtr parentRule);
                  
  MixRulePtr
  ParseMixFix(sherpa::INOstream& errStream, MixInput& origInput, 
              MixRulePtr parentRule);
                  
  bool isKwd(const MixFixNode& mixNode);

  /// @brief Return true if this rule definitely cannot match the
  /// beginning of the lookahead stream.
  ///
  /// This is for use as an early filter. It does not consider the
  /// possibility that reductions occurring at the head of the input
  /// stream might later lead to a match being possible.
  bool cannotPossiblyMatch(sherpa::INOstream& errStream,
                           MixRulePtr rule,
                           const MixInput& mixExpr);

} TheMixContext;

void
MixRule::PrettyPrint(INOstream& out) const
{
  switch(fixity) {
  case infix:
    out << "infix";

    switch(assoc) {
    case assoc_left:   out << 'l'; break;
    case assoc_right:  out << 'r'; break;
    case assoc_none:   out << 'n'; break;
    }

    break;
  case prefix:
    out << "prefix";
    break;
  case postfix:
    out << "postfix";
    break;
  case closed:
    out << "closed";
    break;
  }

  out << " "
      << name
      << "("
      << prec
      << ")";
}

/// @brief Extract the RHS elements of the rule from its name.
void
MixRule::extract()
{
  std::string rule = name;
  
  while(rule.size()) {
    if (rule[0] == HoleMarker[0]) {
      rhs.push_back(HoleMarker);
      rule = rule.substr(1);
    }
    else if (rule.substr(0, 2) == ThunkMarker) {
      rhs.push_back(ThunkMarker);
      rule = rule.substr(2);
    }
    else {
      if (rule[0] == KwdSeparator[0])
        rule = rule.substr(1);

      // Find the end of this quasi-keyword, or end-of-rule:
      size_t nxt = rule.find_first_of("#_@");

      if (nxt == string::npos) {
        rhs.push_back(rule);
        rule.clear();
      }
      else {
        rhs.push_back(rule.substr(0, nxt));
        rule = rule.substr(nxt);
      }
    }
  }

  if (isHole(0) && isHole(rhs.size()-1))
    fixity = infix;
  else if (isHole(0))
    fixity = postfix;
  else if (isHole(rhs.size()-1))
    fixity = prefix;
  else
    fixity = closed;
}

bool
MixContext::isKwd(const MixFixNode& node)
{
  if (node.ast->astType != at_ident)
    return false;

  return kwMap.contains(node.ast->s);
}

bool 
MixContext::cannotPossiblyMatch(sherpa::INOstream& errStream,
                                MixRulePtr rule, 
                                const MixInput& input)
{
  StringVec& rhs = rule->rhs;

  // Are enough components left?
  if (input.size() < rhs.size())
    return true;

  // If this rule has a leading hole, the first thing must be a hole
  // and the next thing must match:
  if (rule->hasLeadingHole()) {
    bool might = (top(input).isExpr()
                  && ((rule->rhs.size() == 1)
                      || topNext(input).matchesKwd(rule->rhs[1])));

    MIXDEBUG(might ? 4 : 5)
      errStream << (*rule)
                << (might ? " might" : " cannot") << " match."
                << endl;
    
    if (might)
      return false;

    return true;
  }
  
  // For closed and prefix expressions, the initial keyword must
  // match, but if the next position is a keyword it may ultimately
  // reduce to an expression, so we can't conclude anything from that.
  bool might = top(input).matchesKwd(rule->rhs[0]);
  if (might)
    return false;

  MIXDEBUG(might ? 4 : 5)
    errStream << (*rule)
              << (might ? " might" : " cannot") << " match."
              << endl;

  return true;
}

/// @brief Given a keyword, make sure there is a kwMap entry for it.
void
QuasiKeywordMap::add(std::string qkwd)
{
  if (!contains(qkwd)) {
    QuasiKeyword qks;
    theMap.insert(std::pair<std::string, QuasiKeyword>(qkwd,qks));
  }

  theMap[qkwd].nOccur++;
}

/// @brief Remove a keyword occurrence, deleting the entry if this is
/// the last such occurrence.
void
QuasiKeywordMap::remove(std::string qkwd)
{
  if (!contains(qkwd))
    return;

  theMap[qkwd].nOccur--;

  if (theMap[qkwd].nOccur == 0)
    theMap.erase(qkwd);
}

/// @brief Add a new mixfix rule to the ruleset.
void 
MixContext::add(MixRulePtr rule)
{
  StringVec& rhs = rule->rhs;
  
  for (size_t i = 0; i < rhs.size(); i++)
    if (!rule->isHole(i))
      kwMap.add(rhs[i]);

  // Closed and prefix rules start with a token:
  if (!rule->hasLeadingHole())
    kwMap[rhs[0]].nFirst++;

  // Prefix and infix rules end with a hole:
  if (rule->hasTrailingHole() && (rhs.size() != 1))
    kwMap[rhs[rhs.size()-2]].nPre++;

  // Postfix and infix rules start with a hole:
  if (rule->hasLeadingHole() && (rhs.size() != 1))
    kwMap[rhs[1]].nPost++;

  rules.insert(rule);
}

/// @brief Remove an existing mixfix rule from the ruleset.
void
MixContext::remove(MixRulePtr rule)
{
  StringVec& rhs = rule->rhs;

  // Closed and prefix rules start with a token:
  if (!rule->hasLeadingHole())
    kwMap[rhs[0]].nFirst--;

  // Prefix and infix rules end with a hole:
  if (rule->hasTrailingHole())
    kwMap[rhs[rhs.size()-2]].nPre--;

  // Postfix and infix rules start with a hole:
  if (rule->hasLeadingHole())
    kwMap[rhs[1]].nPost--;

  for (size_t i = 0; i < rhs.size(); i++)
    if (!rule->isHole(i))
      kwMap.remove(rhs[i]);

  rules.erase(rule);
}

/// @brief Rewrite the parse tree produced by ParseMixFix to
/// handle some odd cases.
static ASTPtr
CleanMixFix(INOstream& errStream, ASTPtr ast)
{
  if (ast->astType == at_apply) {
    std::string fn = ast->child(0)->s;

    // This is the start rule - take all of these out.
    if (fn == "_")
      ast = ast->child(1);

    if (fn == "_._")
      ast = AST::make(at_select, ast->loc, ast->child(1), ast->child(2));

    if (fn == "(_)")
      ast = ast->child(1);

    if (fn == "_(_)") {
      shared_ptr<AST> argAst = ast->child(2);

      // Rotate the applied function into the proper position
      ast->children[0] = ast->children[1];
      ast->children.erase(ast->children.begin()+1, ast->children.end());

      while((argAst->astType == at_apply) && (argAst->child(0)->s == "_,_")) {
        ast->addChild(argAst->child(1));
        argAst = argAst->child(2);
      }
      ast->addChild(argAst);
    }

    if (fn == "_(@)") {
      // Rotate the applied function into the proper position
      ast->children[0] = ast->children[1];
      ast->children.erase(ast->children.begin()+1, ast->children.end());
    }

    if (fn == "(@)") {
      ast->astType = at_unit;
      ast->children.clear();
    }

    // Array indexing:
    if (fn == "_[_]")
      ast = AST::make(at_nth, ast->loc, ast->child(1), ast->child(2));

#if 0
    // Check for ([a, b, c]) convenience syntax. Otherwise eliminate
    // "(_)" nodes.
    if (fn == "(_)") {
      if ((ast->child(1)->astType == at_apply) &&
          (ast->child(1)->child(0)->s == "[_]")) {
        
        ast = argListToConsList(ast->child(1)->child(1));

        // Re-write the argument chain into a chain of CONS applications:
        shared_ptr<AST> argAst = inAST->child(1)->child(1);
        while((argAst->astType == at_apply) && (argAst->child(0)->s == "_,_")) {
          argAst->children[0] = 
            argAst = argAst->child(2);
        }
        ast->addChild(argAst);
      }
      else {
        ast = ast->child(1);
      }
    }
#endif

    if (fn == "[_]")
      ast = AST::make(at_nth, ast->loc, ast->child(1), ast->child(2));

    if (fn == "_and_" || fn == "_&&_") {
      ast->astType = at_and;
      ast->children.erase(ast->children.begin());
    }

    if (fn == "_or_" || fn == "_||_") {
      ast->astType = at_or;
      ast->children.erase(ast->children.begin());
    }
  }

  for (size_t c = 0; c < ast->children.size(); c++)
    ast->children[c] = CleanMixFix(errStream, ast->children[c]);

  return ast;
}

static ASTPtr
CheckMixFix(INOstream& errStream, ASTPtr ast)
{
  return ast;
}

static MixRulePtr MixNoRuleFound = MixRule::make("_", INT_MIN, assoc_none);
static MixRulePtr MixStartRule = MixRule::make("_", INT_MIN, assoc_none);
static MixRulePtr MixInputRule = MixRule::make("(_)", INT_MIN, assoc_none);

/// @brief Wrapper for ParseMixFix.
///
/// We need to shift/pushback on the input, so we implement it as a
/// stack such that the top element is the <em>first</em> element of
/// the input.
shared_ptr<AST>
ProcessMixFix(std::ostream& err, shared_ptr<AST> mixAST)
{
  INOstream errStream(err);

  if (TheMixContext.rules.empty())
    errStream << "Error! No Rules!" << endl;

  ASTVec& children = mixAST->children;

  MixInput input;
  for(ASTVec::reverse_iterator itr = children.rbegin();
      itr != children.rend();
      itr++) {
    push(input, MixFixNode(*itr, MixStartRule));

    // Input quasi-keywords should not be marked by a rule.
    if (TheMixContext.isKwd(top(input)))
      top(input).rule = GC_NULL;
  }

  MIXDEBUG(1) {
    errStream << "Processing mixfix expression at "
              << mixAST->loc << endl;
    errStream.more();
    for(MixInput::reverse_iterator itr = input.rbegin();
        itr != input.rend();
        itr++) {
      if (itr != input.rbegin()) errStream << " ";
      (*itr).PrettyPrint(errStream, false, false);
    }
    errStream << endl;
    errStream.less();
  }


  while( input.size() > 1 
         && TheMixContext.ParseMixFix(errStream, input, MixStartRule))
    ;

  if (input.size() != 1) {
    MIXDEBUG(1)
      errStream << "Yields Incomplete Parse" << endl;
    return GC_NULL;
  }

  MIXDEBUG(2) {
    errStream << "Produced: ";
    input[0].PrettyPrint(errStream, false, true);
  }

  shared_ptr<AST> result = CleanMixFix(errStream, input[0].ast);
  result = CheckMixFix(errStream, result);

  MIXDEBUG(1) {
    errStream << "Giving: ";
    result->PrettyPrint(errStream, false, true);
  }

  return result;
}

static MixFixNode
reduce(INOstream& errStream, MixInput& shunt, MixRulePtr rule)
{
  // Children under shunt are now what we want. Build an apply node
  // for them, and push them back onto the front of the input vector.
  // Don't build unnecessary apply nodes for the catch-all expression
  // rule.

  shared_ptr<AST> result = shunt[0].ast; // until proven otherwise

  shared_ptr<AST> fnName = 
    AST::make(at_ident, LToken(tk_BlkIdent, rule->name));

  result = AST::make(at_apply, shunt[0].ast->loc, fnName);

  assert(rule->rhs.size() == shunt.size());

  for(size_t i = 0; i < shunt.size(); i++) {
    if (rule->rhs[i] == HoleMarker) {
      assert(!shunt[i].isKwd());
      result->addChild(shunt[i].ast);
    }
    else if (rule->rhs[i] == ThunkMarker) {
      assert(!shunt[i].isKwd());

      /// @bug If the thunkified code contains a return, this doesn't
      /// do the right thing at all, so there is a hygiene failure. We
      /// need to change the lambda mechanism around so that we can
      /// let the proper return label be captured as part of the
      /// thunk's closure.
      shared_ptr<AST> iRetBody = 
        AST::make(at_block, shunt[i].ast->loc,
                  AST::make(at_ident, LToken(tk_BlkIdent, "__return")),
                  shunt[i].ast);

      shared_ptr<AST> thunk = 
        AST::make(at_lambda, shunt[i].ast->loc,
                  AST::make(at_argVec, shunt[i].ast->loc), // empty
                  iRetBody);

      result->addChild(thunk);
    }
  }

  MIXDEBUG(2) {
    errStream << "Reduced " <<  rule->name << " giving ";
    result->PrettyPrint(errStream, false, true);
  }

  return MixFixNode(result, rule);
}

/** @brief Perform one leading reduction on the input.
 *
 *  The ``one'' should be taken with salt, because ParseOneMixFix has
 *  to deal with precedence inversion at the right, and can therefore
 *  end up performing what amounts to multiple reductions. Given the
 *  rules and input:
 *
 * @verbatim
infix 3 _+_
infix 4 _*_

1 * 2 + 3
@endverbatim
 *
 * The parser initially proceeds left-to-right looking for the
 * matching rule with highest precedence that can be made to ``fit''
 * on the left. In this case, that's <code>_*_</code>. When it goes to
 * parse the trailing hole, it reduces _+_ and then reduces
 * _+_. Ordinarily, this would produce:
 *
 * @verbatim
1 * ( 2 + 3 )
@endverbatim
 *
 * which is clearly wrong. The reduce action, however, is aware of
 * precedence inversion on the right, and performs a tree-rotate at
 * reduce time. Note that this issue cannot arise on the left, because
 * we always seek the reduction with the highest possible precedence
 * from the left.
 */
MixRulePtr
MixContext::ParseOneMixFix(INOstream& errStream, MixInput& origInput, 
                           MixRulePtr parentRule)
{
  errStream.more();

  MixInput shunt;

  struct BestRule {
    MixRulePtr rule;
    MixInput result;
    bool tie;
  } best = { GC_NULL, MixInput(), false };

  MIXDEBUG(4) {
    errStream << "Parsing:";

    for(MixInput::reverse_iterator itr = origInput.rbegin();
        itr != origInput.rend();
        itr++) {
      errStream << " ";
      (*itr).PrettyPrint(errStream, false, false);
    }
    errStream << " in context of "<< (*parentRule);
    errStream << endl;
  }

  // We are proceding through all of the parse rules, trying to find
  // one that reduces the input without regard to operator
  // precedence. We'll fix up operator precedence later using a tree
  // re-writing.

  for(RuleSet::iterator itr = rules.begin();
      itr != rules.end();
      ++itr) {
    MixRulePtr rule = *itr;

    // ParentRule is actually telling us the hole precedence:
    if (rule->prec < parentRule->prec) {
      MIXDEBUG(5)
        errStream << (*rule)
                  << "rejected (below required precedence)"
                  << endl;
      continue;
    }
    
    if (rule->prec == parentRule->prec) {
      if ((rule->assoc != assoc_right) ||
          (parentRule->assoc != assoc_right)) {
        MIXDEBUG(5)
          errStream << (*rule)
                    << "rejected (prec OK, not right-assoc)"
                    << endl;
        continue;
      }
    }
    
    // If the candidate rule has lower precedence than the one we actually
    // found, the current best rule would be preferred.
    if (best.rule && rule->prec < best.rule->prec)
      continue;
    
    if (cannotPossiblyMatch(errStream, rule, origInput))
      continue;

    // The parse processing is destructive, so we need to operate on a
    // copy:
    MixInput input = origInput;
    shunt = MixInput();

    MIXDEBUG(4) {
      errStream << "Attempting rule " << rule->name
                << " on";

        for(MixInput::reverse_iterator itr = input.rbegin();
            itr != input.rend();
            itr++) {
          errStream << " ";
          (*itr).PrettyPrint(errStream, false, false);
        }
      errStream << endl;
    }

    errStream.more();

    // If we survived cannotPossiblyMatch(), then the first element
    // matches, so shift that:
    MIXDEBUG(3) {
      errStream << "Shifting first element: ";
      top(input).PrettyPrint(errStream, false, true);
    }
    push(shunt, pop(input));

    for (size_t pos = 1; pos < rule->rhs.size(); pos++) {
      // Parsing of expressions can cause the remaining input to
      // become too small for this rule to succeed. Need to check here
      // to avoid a range overrun on the input:
      if (input.size() < (rule->rhs.size() - pos))
        break;

      // Note that if matchesKwd() passes then we know the node
      // actually *was* a keyword by virtue of the fact that we are
      // matching a keyword position in the rule. There is no need
      // to check that redundantly.
      //
      // We handle right-most holes specially because they have to
      // have higher precedence than their parent rule, and when we
      // recurse we are only looking for potential child nodes.

      if (rule->isKwd(pos) && top(input).matchesKwd(rule->rhs[pos])) {
        MIXDEBUG(3) {
          errStream << "Shifting: ";
          top(input).PrettyPrint(errStream, false, true);
        }

        push(shunt, pop(input));
      }
      // Note: Want to do this whether the next thing in the input is
      // an expr or a kwd! Consider _-_ matching 1 - (2) 
      else if (rule->isHole(pos) &&
               ParseMixFix(errStream, input, 
                           rule->isFinalHole(pos) ? rule : MixInputRule)) {
        MIXDEBUG(3) {
          errStream << "Shifting: ";
          top(input).PrettyPrint(errStream, false, true);
        }

        push(shunt, pop(input));
      }
      else if(rule->isHole(pos) && top(input).isExpr()) {
        // No way to reduce. Shift this element because it is an
        // pre-existing expression constructed by the initial parser.
        MIXDEBUG(3) {
          errStream << "Shifting: ";
          top(input).PrettyPrint(errStream, false, true);
        }

        push(shunt, pop(input));
      }
      else {
        // Rule did not match.
        break;
      }
    }

    // Did we get a complete match for this rule? If so, compute the
    // reduction:
    MIXDEBUG(4) {
      errStream << "Rule " << rule->name
                << " (size " << rule->rhs.size()
                << ")";
      if (rule->rhs.size() == shunt.size()) {
        errStream << " matched stack " << shunt.size();

        if (best.rule) {
          if (rule->prec > best.rule->prec)
            errStream << " and is preferred";
          else
            errStream << " but existing rule is better";
        }
      }
      else
        errStream << " did not match stack " << shunt.size();

      errStream << endl;
    }

    errStream.less();

    if (rule->rhs.size() == shunt.size()) {
      MixFixNode nd = reduce(errStream, shunt, rule);
      
      assert(nd.ast);

      if (best.rule && rule->prec == best.rule->prec) {
        // FIX: Somehow need to track the alternatives here for
        // reporting purposes.
        best.tie = true;
        continue;
      }

      push(input, nd);
      best.rule = rule;
      best.result = input;
      best.tie = false;
    }
  }

  errStream.less();

  /// @bug We should probably keep track of what the candidates were
  /// and report them in a useful error message.
  if (best.tie)
    return GC_NULL;

  if (!best.rule)               // no rule fonud
    return GC_NULL;

  origInput = best.result;
  return best.rule;
}

/// @brief Trivial wrapper on ParseOneMixFix.
/// 
/// When ParseOneMixFix succeeds at performing a reduction, the input
/// has changed, and we need to try repeatedly until no further
/// reductions are possible. The final call to ParseOneMixFix will
/// fail, which is expected and okay.
MixRulePtr
MixContext::ParseMixFix(INOstream& errStream, MixInput& origInput,
                        MixRulePtr parentRule)
{
  MixRulePtr best = GC_NULL;
  MixRulePtr p = GC_NULL;

  while (p = ParseOneMixFix(errStream, origInput, parentRule)) {
    best = p;
  }

  return best;
}

/// @brief List of built-in mixfix rules.
///
/// For the moment, this is actually @em all of the mixfix rules. Once
/// I get the mixfix mechanism debugged and the syntax for mixfix
/// introduction in place many of these will move to the prelude.
///
/// User-defined mixfix rules have precedence between 0 and 15
/// (inclusive). Application and array indexing have precedence -1,
/// while constant definition syntax has precedence -3 (but is closed,
/// so that doesn't really matter.
MixRulePtr MixRules[] =  {
  //////////////////////////////////////////////////////////
  // Beginning of user-defined mixfix range
  /////////////////////////////////////////////////////////

  MixRule::make("_or_",  0, assoc_left), // lazy OR (syntax)
  MixRule::make("_||_",  0, assoc_left), // lazy OR (syntax)
  MixRule::make("_and_", 1, assoc_left), // lazy AND (syntax)
  MixRule::make("_&&_",  1, assoc_left), // lazy AND (syntax)
  MixRule::make("_!=_",  2, assoc_left),
  MixRule::make("_==_",  2, assoc_left),
  MixRule::make("_<_",   3, assoc_left),
  MixRule::make("_<=_",  3, assoc_left),
  MixRule::make("_>_",   3, assoc_left),
  MixRule::make("_>=_",  3, assoc_left),
  MixRule::make("_::_",  4, assoc_left), // infix cons
  MixRule::make("_|_",   5, assoc_left),
  MixRule::make("_^_",   6, assoc_left),
  MixRule::make("_&_",   7, assoc_left),
  MixRule::make("_<<_",  8, assoc_left),
  MixRule::make("_>>_",  8, assoc_left),
  MixRule::make("_+_",   9, assoc_left),
  MixRule::make("_-_",   9, assoc_left),
  MixRule::make("_%_",  10, assoc_left),
  MixRule::make("_*_",  10, assoc_left),
  MixRule::make("_/_",  10, assoc_left),

  MixRule::make("_**_",    11, assoc_left), // exponentiation
  MixRule::make("_**_+_",  11, assoc_left),  // hypothetical mul-add for testing

  MixRule::make("-_",   12, assoc_right), // Unary negation
  MixRule::make("!_",   13, assoc_right), // Boolean inverse
  MixRule::make("not_", 13, assoc_right), // Boolean inverse
  MixRule::make("~_",   14, assoc_right), // Bitwise inverse

  //////////////////////////////////////////////////////////
  // End of user-defined mixfix range
  /////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////
  // Rules associated with built-in structure start here.
  //
  // These productions yield (after massage) syntactic forms rather
  // than applications.
  /////////////////////////////////////////////////////////

  // The precedence of these next two doesn't really matter, because
  // (a) they are closed, and (b) the related forms below don't have
  // the same first token.

  // cpair convenience and precedence override:
  MixRule::make("(_)",  128, assoc_none),

  // list convenience syntax:
  MixRule::make("[_]",  128, assoc_none),

  // The design note called for:
  //   MixRule::make("__",  0, assoc_left),
  // but I'm giving these a try instead:

  MixRule::make("_,_",  -1, assoc_right), // arg assembly, cpair assembly

  MixRule::make("(@)",   129, assoc_left), // unit constructor

  MixRule::make("_(@)",  130, assoc_left), // nullary application
  MixRule::make("_(_)",  130, assoc_left), // application
  MixRule::make("_[_]",  130, assoc_left), // array/vector subscript
  MixRule::make("_._",   130, assoc_left), // dot notation (select or usesel)

  // Which might quite possibly be a better approach
};
static size_t nMixRules = sizeof(MixRules) / sizeof(MixRules[0]);

void
mixfix_init()
{
  for (size_t i = 0; i < nMixRules; i++) {
    TheMixContext.add(MixRules[i]);
  }
}

static bool 
HandleMixFix(std::ostream& errStream, shared_ptr<AST> ast)
{
  bool errFree = true;

  // In this pass, we proceed bottom-up
  for (size_t c = 0; c < ast->children.size(); c++)
    errFree = errFree && HandleMixFix(errStream, ast->child(c));

  if (ast->astType == at_mixExpr) {
    shared_ptr<AST> newAst = ProcessMixFix(errStream, ast);

    if (!newAst)
      errFree = false;

    if (newAst && newAst != ast) {
      // Clobber the old node in-place:
      ast->s = newAst->s;
      ast->astType = newAst->astType;
      ast->identType = newAst->identType;
      ast->litValue = newAst->litValue;
      ast->litBase = newAst->litBase;
      ast->flags = newAst->flags;
      ast->children = newAst->children;
      ast->fqn = newAst->fqn;
    }
  }

  return errFree;
}

bool
UocInfo::fe_mixfix(std::ostream& errStream,
                   bool init, unsigned long flags)
{
  return HandleMixFix(errStream, uocAst);
}
