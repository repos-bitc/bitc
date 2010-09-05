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

#include "AST.hxx"
#include "MixFix.hxx"
#include "UocInfo.hxx"

#include "BUILD/TransitionParser.hxx"

// Debug levels:
// 4 = detailed rule matching
// 3 = recursion
// 2 = shift/reduce
// 1 = Summary input and result
#define MIXFIX_DEBUG_LEVEL 3
#define MIXDEBUG(n) if (MIXFIX_DEBUG_LEVEL >= (n))

using namespace std;
using namespace boost;
using namespace sherpa;
using namespace mixfix;

typedef shared_ptr<AST> ASTPtr;
typedef std::vector<ASTPtr> ASTVec;
typedef std::vector<std::string> StringVec;

static const std::string HoleMarker = "_";

struct MixRule : public boost::enable_shared_from_this<MixRule> {
  /// @brief Summary name of this mix rule
  std::string name;
  /// @brief Exploded elements of the right-hand side of the production.
  StringVec rhs;

  /// @brief Fixity - extracted mainly for doc and debug purposes
  int fixity;

  /// @brief Precedence for this production (infix only).
  int precedence;

  /// @brief Associativity for this rule.
  ///
  /// This can only be assoc_left or assoc_right for infix rules.
  Associativity assoc;

  /// @brief Return true iff this is an infix rule.
  bool isInfix() { return fixity == infix; }
  bool isPrefix() { return fixity == prefix; }
  bool isClosed() { return fixity == closed; }
  bool isPostfix() { return fixity == postfix; }

  static bool isHoleMarker(const std::string& s) {
    return (s == HoleMarker);
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
  MixRule(const std::string& _nm, int _prec, int _assoc = assoc_none)
  {
    name = _nm;
    precedence = _prec;
    extract();
  }

  static shared_ptr<MixRule> 
  make(const std::string& _nm, int _prec, int _assoc = assoc_none)
  {
    MixRule *tmp = new MixRule(_nm, _prec, _assoc);
    return boost::shared_ptr<MixRule>(tmp);
  }
};
typedef shared_ptr<MixRule> MixRulePtr;

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

struct MixContext {
  typedef std::set<MixRulePtr> RuleSet;
  RuleSet rules;
  QuasiKeywordMap kwMap;

  void add(MixRulePtr rule);
  void remove(MixRulePtr rule);

  bool
  ParseMixFixRule(std::ostream& errStream,
                  ASTVec& input,
                  MixRulePtr rule);
  bool
  ParseMixFix(std::ostream& errStream, ASTVec& input, 
              int withPrecedenceAbove);

  bool isKwd(shared_ptr<AST> ast);

  /// @brief Return true if this rule definitely cannot match the
  /// beginning of the lookahead stream.
  ///
  /// This is for use as an early filter. It does not consider the
  /// possibility that reductions occurring at the head of the input
  /// stream might later lead to a match being possible.
  bool cannotPossiblyMatch(std::ostream& errStream,
                           MixRulePtr rule,
                           ASTVec mixExpr);

} TheMixContext;

/// @brief Extract the RHS elements of the rule from its name.
void
MixRule::extract()
{
  std::string rule = name;
  
  while(rule.size()) {
    if (rule[0] == HoleMarker[0]) {
      rhs.push_back(rule.substr(0,1));
      rule = rule.substr(1);
    }
    else {
      // Find the end of this quasi-keyword, or end-of-rule:
      size_t nxtHole = rule.find(HoleMarker);
      if (nxtHole == string::npos) {
        rhs.push_back(rule);
        rule.clear();
      }
      else {
        rhs.push_back(rule.substr(0, nxtHole));
        rule = rule.substr(nxtHole);
      }
    }
  }

  // All well-formed rules have at least two elements. Note that I may
  // need to withdraw this check in order to add a "_" baseline rule
  // to normalize processing below.
  assert(rhs.size() > 1);

  if (isHoleMarker(rhs[0]) && isHoleMarker(rhs[rhs.size()-1]))
    fixity = infix;
  else if (isHoleMarker(rhs[0]))
    fixity = postfix;
  else if (isHoleMarker(rhs[rhs.size()-1]))
    fixity = prefix;
  else
    fixity = closed;
}

bool
MixContext::isKwd(shared_ptr<AST> ast)
{
  if (ast->astType != at_ident)
    return false;

  return kwMap.contains(ast->s);
}

bool 
MixContext::cannotPossiblyMatch(std::ostream& errStream,
                                MixRulePtr rule, ASTVec mixExpr)
{
  StringVec& rhs = rule->rhs;

  // Are enough components left?
  if (mixExpr.size() < rhs.size())
    return true;

  // If this rule has a leading hole, the first thing must be a hole
  // and the next thing must match:
  if (rule->hasLeadingHole()) {
    bool might = (!isKwd(mixExpr[0])
                  && isKwd(mixExpr[1])
                  && mixExpr[1]->s == rhs[1]);

    MIXDEBUG(4) errStream << (rule->isInfix() ? "infix" : "postfix")
                       << " rule " << rule->name 
                       << (might ? " might" : " cannot") << " match."
                       << endl;
    
    if (might)
      return false;

    return true;
  }
  
  // For closed and prefix expressions, the initial keyword must
  // match, but if the next position is a keyword it may ultimately
  // reduce to an expression, so we can't conclude anything from that.
  bool might = isKwd(mixExpr[0]) && (mixExpr[0]->s == rhs[0]);
  if (might)
    return false;

  MIXDEBUG(4) errStream << (rule->isClosed() ? "closed" : "prefix")
                     << " rule " << rule->name
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
    if (!rule->isHoleMarker(rhs[i]))
      kwMap.add(rhs[i]);

  // Closed and prefix rules start with a token:
  if (!rule->hasLeadingHole())
    kwMap[rhs[0]].nFirst++;

  // Prefix and infix rules end with a hole:
  if (rule->hasTrailingHole())
    kwMap[rhs[rhs.size()-2]].nPre++;

  // Postfix and infix rules start with a hole:
  if (rule->hasLeadingHole())
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
    if (!rule->isHoleMarker(rhs[i]))
      kwMap.remove(rhs[i]);

  rules.erase(rule);
}

template<class T>
static inline
T& top(std::vector<T> vec)
{
  return vec[vec.size()-1];
}

bool
MixContext::ParseMixFixRule(std::ostream& errStream,
                            ASTVec& input,
                            MixRulePtr rule)
{
  ASTVec shunt;

  if (cannotPossiblyMatch(errStream, rule, input))
    return false;

  MIXDEBUG(4) errStream << "Attempting rule " << rule->name << endl;

  // If we survivied cannotPossiblyMatch(), then the first element
  // simply shifts:
  MIXDEBUG(2) {
    errStream << "Shifting: ";
    input[0]->PrettyPrint(errStream, false, true);
  }

  shunt.push_back(input.front());
  input.erase(input.begin());

  for (size_t pos = 1; pos < rule->rhs.size(); pos++) {
    if (rule->isHoleMarker(rule->rhs[pos])) {
      int precedenceFloor = 
        (pos == (rule->rhs.size()-1)) ? rule->precedence : INT_MIN;

      // The following *may* create or replace a leading subexpression
      // in the input
      ParseMixFix(errStream, input, precedenceFloor);

      if (!isKwd(input[0])) {
        // There is a leading subexpression to shift now.
        MIXDEBUG(2) {
          errStream << "Shifting: " ;
          input[0]->PrettyPrint(errStream, false, true);
        }

        shunt.push_back(input.front());
        input.erase(input.begin());
      }
      else {
        // We didn't get any rule to match, 
        return false;
      }
    }
    else if (isKwd(input[0])) {
      // Not necessary to check if rule->rhs here isKwd(), because
      // *everything* in rule->rhs is either HoleMarker or
      // keyword, and HoleMarker is *never* a keyword.
      if (input[0]->s == rule->rhs[pos]) {
        MIXDEBUG(2) {
          errStream << "Shifting: ";
          input[0]->PrettyPrint(errStream, false, true);
        }

        shunt.push_back(input.front());
        input.erase(input.begin());
      }
      else {
        // Rule did not match.
        return false;
      }
    }
    else {
      // Rule did not match.
      return false;
    }
  }

#if 0
  errStream << "Reducing " <<  rule->name << " with";
  for(size_t i = 0; i < shunt.size(); i++) {
    errStream << " ";
    shunt[i]->PrettyPrint(errStream, false, false);
  }
  errStream << endl;
#endif

  // Children under shunt are now what we want. Build an apply node
  // for them, and push them back onto the front of the input vector.

  shared_ptr<AST> ident = 
    AST::make(at_ident, LToken(tk_BlkIdent, rule->name));

  shared_ptr<AST> result = AST::make(at_apply, shunt[0]->loc, ident);
  for(size_t i = 0; i < shunt.size(); i++) {
    if(!isKwd(shunt[i]))
      result->addChild(shunt[i]);
  }

  MIXDEBUG(2) {
    errStream << "Reduced " <<  rule->name << " giving ";
    result->PrettyPrint(errStream, false, true);
  }

  input.insert(input.begin(), result);

  return true;
}

/// ParseMixFix has to be sensitive about precedence behavior when it
/// is called to process a trailing expression of a "parent" rule that
/// has a trailing hole. The concern over precedence lies in the fact
/// that precedence <em>determines</em> whether that rule is or is not
/// a parent rule.
///
/// The issue arises when two adjacent rules are joined by a common
/// hole, as in:
///
///   _*_ _+_ => (a * b) + c    (by precedence)
///   _+_ _*_ => a + (b * c)    (by precedence)
///   _+_ _+_ => (a + b) + c    (by associativity)
///   _+_ _!  => a + (b!)       (by precedence)
///
/// Basically, if we have a trailing hole on the left, and a leading
/// hole on the right, we want to go ahead and (recursively) reduce
/// anything on the right that has <em>higher</em> precedence than the
/// rule on the left.
///
/// The @p withPrecedenceAbove argument to ParseMixFix is ignored if
/// the rule on the right does <em>not</em> have a leading hole. That
/// is: if the rule on the right is prefix or closed. The intuition is
/// that things like:
///
///    a + -b         _+_  -_   infix then prefix
///    a + (b)        _+_  (_)  infix then closed
///    not (a and b)  !_   (_)  prefix then closed
///
/// have only one sensible parse.
///
/// The tricky bit is dealing with associativity, and I haven't
/// addressed that quite yet.
///
/// Texas law (truth!): When two trains meet each other at a
/// railroad crossing, each shall come to a full stop, and
/// neither shall proceed until the other has gone.
/// 
/// Okay. Perhaps the relevance of that law is questionable...
bool
MixContext::ParseMixFix(std::ostream& errStream,
                        ASTVec& input,
                        int withPrecedenceAbove)
{
  struct BestRule {
    MixRulePtr theRule;
    ASTVec result;
    int precedence;
  } best = { GC_NULL, ASTVec(), -2 };

  MIXDEBUG(3) {
    errStream << "Parsing leading expr:";
    for (size_t i = 0; i < input.size(); i++) {
      errStream << " ";
      input[i]->PrettyPrint(errStream, false, false);
    }
    errStream << endl;
  }

  if (input.size() == 1)
    return true;

  if (rules.empty())
    errStream << "Error! No Rules!" << endl;

  for(RuleSet::iterator itr = rules.begin();
      itr != rules.end();
      ++itr) {
    MixRulePtr rule = *itr;

    if (rule->hasLeadingHole() && (rule->precedence <= withPrecedenceAbove)) {
      MIXDEBUG(4) errStream << "Skipping rule " << rule->name << " (precedence)" << endl;
      continue;
    }

    ASTVec cleanInput = input;
    if (ParseMixFixRule(errStream, cleanInput, rule)) {
      if (rule->precedence > best.precedence) {
        best.theRule = rule;
        best.result = cleanInput;
        best.precedence = rule->precedence;
      }
    }
  }

  if (best.theRule) {
    /// @bug Need to deal with attachment precedence here.
    input = best.result;
    return true;
  }

  return false;
}

/** Wrapper for ParseMixFix.
 */
shared_ptr<AST>
mixfix::ProcessMixFix(std::ostream& errStream, 
                      shared_ptr<AST> mixAST)
{
  MIXDEBUG(2) {
    errStream << "Processing mixfix expression ";
    mixAST->PrettyPrint(errStream, false, true);
  }

  ASTVec input = mixAST->children;

  while (input.size() > 1 &&
         TheMixContext.ParseMixFix(errStream, input, INT_MIN))
    ;

  if (input.size() > 1) {
    MIXDEBUG(1)  {
      errStream << "Mixfix expression ";
      mixAST->PrettyPrint(errStream, false, true);
      errStream << "Yields Incomplete parse" << endl;
    }
    return GC_NULL;
  }

  MIXDEBUG(1) {
    errStream << "Mixfix expression ";
    mixAST->PrettyPrint(errStream, false, true);
    errStream << "Yields: ";
    input[0]->PrettyPrint(errStream, false, true);
  }

  return input[0];
}


#if 0


struct MixParseResult {
  shared_ptr<AST> tree;
  int precedence;
  int newPos;
  
  MixParseResult(shared_ptr<AST> _tree, int _prec, int _newPos)
  {
    tree = _tree;
    precedence = _prec;
    newPos = _newPos;
  }
};

static MixParseResult MixParseError(GC_NULL, -2, 0);
static MixParseResult MixParseBest(GC_NULL, 0, 0);

typedef std::vector<shared_ptr<AST> > MixParseStack;

void
shift(MixParseStack& stack, shared_ptr<AST> ast)
{
  stack.push_back(ast);
}
shared_ptr<AST>
top(MixParseStack& stack)
{
  return stack.back();
}

static bool
validPrefix(MixRule rule, std::string prefix)
{
  return (prefix == rule.name.substr(0, prefix.size()));
}

static bool
isMixOp(const shared_ptr<AST>& ast)
{
  size_t astLen = ast->s.size();

  if (ast->astType != at_ident)
    return false;

  // This is an utterly horrible temporary expedient. We really need
  // this classification to be done for us in either the lexer or the
  // resolver.
  for (size_t i = 0; i < nMixRules; i++) {
    MixRule& rule = MixRules[i];

    for (size_t pos = 0; pos < rule.name.size(); pos++) {
      if (rule.name[pos] == '_')
        continue;

      if (ast->s == rule.name.substr(pos, ast->s.size())) {
        if ((pos + astLen) == rule.name.size())
          return true;

        if (rule.name[pos+astLen] == '_')
          return true;

        return false;
      }
    }
  }
}

static bool
isExpr(const shared_ptr<AST>& ast)
{
  return !isMixOp(ast);
}

static shared_ptr<AST>
reduce(std::ostream& errStream, MixParseStack& parseStack, MixRule rule)
{
  errStream << " Reducing by rule: " << rule.name << endl;

  if (parseStack.size() != rule.rhs.size()) {
    errStream << "   ** invalid parse stack for " << rule.name << endl;
    
    return GC_NULL;
  }

  shared_ptr<AST> ident = 
    AST::make(at_ident, LToken(tk_BlkIdent, rule.name));

  errStream << "   Making apply node using ";
  ident->PrettyPrint(errStream, false, true);

  shared_ptr<AST> result = AST::make(at_apply, parseStack[0]->loc, ident);
  for(size_t i = 0; i < parseStack.size(); i++)
    if(!isMixOp(parseStack[i])) {
      errStream << "   Inserting child ";
      parseStack[i]->PrettyPrint(errStream, false, true);
      result->addChild(parseStack[i]);
    }

  errStream << "   Result tree is ";
  result->PrettyPrint(errStream, false, true);

  return result;
}

/** Process a sequential list of tokens and expressions into a
 * conventional AST.
 *
 * When ParseMixFix is called, the children of the argument @p mixAST
 * are a linear list of operators and expression nodes. We are asked
 * to parse beginning at position @p pos, considering only
 * expressions that have precedence greater than @prec. We use a
 * back-tracking recursive descent parser to do this.
 */
static MixParseResult
ParseMixFix(std::ostream& errStream, 
            shared_ptr<AST> mixAST, 
            int pos);

static MixParseResult
ParseMixFixRule(std::ostream& errStream, 
                shared_ptr<AST> mixAST, 
                MixRule rule,
                std::string prefix,
                MixParseStack& parseStack,
                size_t pos)
{
  size_t startPos = pos;

  for(;;) {
    if (!validPrefix(rule, prefix)) {
      errStream << " Invalid prefix." << endl;
      return MixParseError;
    }

    errStream << " Considering rule: " << rule.name << endl;
    errStream << " Prefix " << prefix << " is valid" << endl;
    errStream << " Against: ";
    for(size_t i = startPos; i < mixAST->children.size(); i++) {
      if (i == pos)
        errStream << ". ";
      mixAST->child(i)->PrettyPrint(errStream, false, false);
      errStream << " ";
    }
    if (pos == mixAST->children.size())
      errStream << ".";
    errStream << endl;

    shared_ptr<AST> lookAhead = GC_NULL;
    if (pos < mixAST->children.size())
      lookAhead = mixAST->child(pos);

    // Check if we are done with the present rule. If so, reduce
    // without regard to further lookahead:
    if (prefix == rule.name) {
      errStream << " Fully matched." << endl;
      return MixParseResult(reduce(errStream, parseStack, rule), 
                            rule.precedence, pos);
    }

    // If there isn't any lookAhead, we can't make further progress, so
    // we better have done a reduce above.
    if(!lookAhead) {
      errStream << " No lookahead and incomplete match." << endl;
      return MixParseError;
    }

    // If the parse stack is empty, shift. This is correct because
    // eta-productions are syntactically disallowed:
    if (parseStack.empty()) {
      errStream << " Empty stack: shifting." << endl;
      shift(parseStack, lookAhead);
      prefix += isExpr(lookAhead) ? HoleMarker : lookAhead->s;
      errStream << " Prefix is now: " << prefix << endl;
      pos = pos + 1;
      continue;
    }

    // Two expressions cannot be adjacent:
    if (isExpr(lookAhead) && isExpr(top(parseStack))) {
      errStream << " Expression would follow expression." << endl;
      return MixParseError;
    }

    // Optimization: if only one lookahead item remains, and we
    // haven't reduced, the only possible thing to do with it is shift
    // it:
    if ((pos + 1) == mixAST->children.size()) {
      errStream << " One Lookahead: shifting." << endl;
      shift(parseStack, lookAhead);
      prefix += isMixOp(lookAhead) ? lookAhead->s : HoleMarker;
      errStream << " Prefix is now: " << prefix << endl;
      pos = pos + 1;
      continue;
    }

    if (isExpr(top(parseStack))) {
      // Expression on top of stack, we haven't reduced, and the next
      // thing is not an expression:

      errStream << " Unreduced expr on top of stack: shifting." << endl;

      shift(parseStack, lookAhead);
      prefix += isMixOp(lookAhead) ? lookAhead->s : HoleMarker;
      errStream << " Prefix is now: " << prefix << endl;
      pos = pos + 1;
      continue;
    }

    // OK. The parse stack is non-empty, and we have taken care of the
    // easy cases. :-)

    assert(isMixOp(top(parseStack)));

    errStream << " Checking recursive expressions at this position...." << endl;

    // Since we didn't reduce above, and we're still working on a
    // valid prefix, the next thing HAS to be an expression.
    // Further, we know from above that the lookahead holds at least two
    // elements:
    std::string newPrefix = isMixOp(lookAhead) ? lookAhead->s : HoleMarker;

    MixParseResult best = ParseMixFix(errStream, mixAST, pos);
      
    // If we are looking at an expression, decide what to do based on
    // precedence:

    bool justShift = true;

    if (best.tree) {
      if (best.precedence > rule.precedence) {
        // Accept that reduction, shift it, update the
        // prefix, and continue:
        shift(parseStack, best.tree);
        prefix += HoleMarker;
        errStream << " Prefix is now: " << prefix << endl;
        pos = best.newPos;
        continue;
      }
    } else {
      // No forward parse.
      shift(parseStack, lookAhead);
      prefix += isMixOp(lookAhead) ? lookAhead->s : HoleMarker;
      errStream << " Prefix is now: " << prefix << endl;
      pos = pos + 1;
      continue;
    }

    // No expression to be found, so parse fails:
    return MixParseError;
  }
}

static MixParseResult
ParseMixFix(std::ostream& errStream, 
            shared_ptr<AST> mixAST, 
            int pos)
{
  MixParseResult best = MixParseError;

  for(size_t i = 0; i < nMixRules; i++) { 
    MixRule& rule = MixRules[i];
    MixParseStack newStack;

    if (rule.precedence < best.precedence)
      continue;

    MixParseResult mpr = 
      ParseMixFixRule(errStream, mixAST, rule, "", newStack, pos);

    if (!mpr.tree)
      continue;

    if (!best.tree) {
      errStream << "This is best so far" << endl;
      best = mpr;
      continue;
    }

    if (mpr.precedence > best.precedence) {
      errStream << "Higher precedence than previous best" << endl;
      best = mpr;
      continue;
    }

    if (mpr.precedence < best.precedence)
      continue;

    errStream << "Same precedence than previous best" << endl;

    // Same precedence, both valid: error
    return MixParseError;
  }

  return best;
}

/** Wrapper for ParseMixFix.
 */
shared_ptr<AST>
mixfix::ProcessMixFix(std::ostream& errStream, 
                      shared_ptr<AST> mixAST)
{
  errStream << "Processing mixfix ";
  mixAST->PrettyPrint(errStream, false, true);

  if (mixAST->children.size() == 1) {
    errStream << "Has single child. Returning ";
    mixAST->child(0)->PrettyPrint(errStream, false, true);

    return mixAST->child(0);
  }

  MixParseResult best = ParseMixFix(errStream, mixAST, 0);

  if (best.newPos != mixAST->children.size()) {
    errStream << "Incomplete parse" << endl;
    return GC_NULL;
  }

  if (best.tree) {
    errStream << "Got: ";
    best.tree->PrettyPrint(errStream, false, true);
    return best.tree;
  }

  // Mixfix processing error
  return GC_NULL;
}


#endif

// Temporary expedient: since I don't have a syntax for mixfix
// introduction yet, provide a fixed table of built-in mixfix rules
// for testing purposes:
MixRulePtr MixRules[] =  {
  //   MixRule::make("_",    -1, assoc_none),
  MixRule::make("_(#A)",  -10, assoc_left),
  MixRule::make("_or_",  0, assoc_left),
  MixRule::make("_||_",  0, assoc_left),
  MixRule::make("_and_", 1, assoc_left),
  MixRule::make("_&&_", 1, assoc_left),
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

  MixRule::make("-_",   12, assoc_left), // Unary negation
  MixRule::make("!_",   13, assoc_left), // Boolean inverse
  MixRule::make("not_", 13, assoc_left), // Boolean inverse
  MixRule::make("~_",   14, assoc_left), // Bitwise inverse
};
static size_t nMixRules = sizeof(MixRules) / sizeof(MixRules[0]);

void
mixfix::init()
{
  for (size_t i = 0; i < nMixRules; i++) {
    TheMixContext.add(MixRules[i]);
  }
}
