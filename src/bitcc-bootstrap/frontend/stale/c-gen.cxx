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

#include <stdlib.h>
#include <errno.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <assert.h>
#include <sstream>
#include "AST.hxx"
#include "Type.hxx"
#include "Options.hxx"
#include "inter-pass.hxx"
using namespace sherpa;
using namespace std;

extern void AlphaRename(GCPtr<AST> ast);
/* FIXME later (LOOP2LETREC) */
extern GCPtr<AST> loop2letrec(GCPtr<AST>);

enum DeclsOrDefs {
  WantForward,
  WantDecl,
  WantGC,
  WantConstructors,
  WantDefs
};

// This is a completely sleazy way of making an automatically indenting 
// stream.
struct INOstream {
  long depth;
  bool needIndent;
  std::ostream &ostrm;
  void doIndent();

  INOstream(std::ostream &os) 
    :ostrm(os)
  {
    depth = 0;
    needIndent = true;
  }

  inline void indent(int i)
  {
    depth += i;
  }

  inline void more()
  {
    indent(2);
  }

  inline void less()
  {
    indent(-2);
  }
};

void
INOstream::doIndent()
{
  if (needIndent) {
    for (int i = 0; i < depth; i++) {
      ostrm << ' ';
    }
  }
  needIndent = false;
}

// This is where the magic happens. The way that a C++ stream recognizes
// things like std::endl is by overloading this argument type. std::endl 
// is actually a procedure!
inline
INOstream& operator<<(INOstream& inostrm, ostream& (*pf)(ostream&))
{
  inostrm.doIndent();
  inostrm.ostrm << pf;
  if (pf == (ostream& (*)(ostream&)) std::endl)
    inostrm.needIndent = true;
  return inostrm;
}

inline
INOstream& operator<<(INOstream& inostrm, const char *s)
{
  inostrm.doIndent();
  for (size_t i = 0; i < strlen(s); i++) {
    if (s[i] != '\n')
      inostrm.ostrm << s[i];
    else {
      inostrm << std::endl;
    }
  }

  return inostrm;
}

inline
INOstream& operator<<(INOstream& inostrm, const uint64_t ull)
{
  inostrm.doIndent();
  inostrm.ostrm << ull;

  return inostrm;
}

inline
INOstream& operator<<(INOstream& inostrm, const std::string& s)
{
  inostrm.doIndent();
  for (size_t i = 0; i < s.size(); i++) {
    if (s[i] != '\n')
      inostrm.ostrm << s[i];
    else {
      inostrm << std::endl;
    }
  }

  return inostrm;
}

template<typename T>
inline
INOstream& operator<<(INOstream& inostrm, T ob)
{
  inostrm.doIndent();
  inostrm.ostrm << ob;
  return inostrm;
}

///////////////////////////////////////////////////////////////////
//
// Check restrictions imposed by the bootstrap backend
//
///////////////////////////////////////////////////////////////////

// Take in a binding pattern and fill a vector
// with the names of all new identifiers being defined
// in the pattern.

bool
getDefs(std::ostream& errStream, 
	GCPtr<AST> bp, GCPtr< CVector< GCPtr<AST> > > defs)
{
  bool errFree = true;
  switch(bp->astType) {
  case at_identPattern:
    {
      // match agt_var   // match agt_type?
      defs->append(bp->children[0]);
      break;
    }

  case at_unitPattern:
    break;
  case at_cpairPattern:
  case at_pairPattern:
    {
      // match agt_valuePattern*
      for (size_t c = 0; c < bp->children.size(); c++)
	CHKERR(errFree, getDefs(errStream, bp->children[c], defs));
      break;
    }

  default:
    {
      errStream << bp->loc << ": "
		<< "Internal Compiler Error." 
		<< "Unexpected Binding Pattern type " 
		<< bp->astTypeName()
		<< " obtained by getDefs() routine."
		<< std::endl;
      errFree = false;
      break;
    }
  }
  return errFree;
}


void
getDefNames(GCPtr<AST> bp, CVector<std::string>& names)
{
  GCPtr< CVector < GCPtr<AST> > > defs = new CVector< GCPtr<AST> >;

  getDefs(std::cerr, bp, defs);

  for (size_t i = 0; i < defs->size(); i++) {
    GCPtr<AST> ident = (*defs)[i];

    names.append(ident->s);
  }
}

// Return true if, wherever /nm/ is free in /ast/, it appears exclusively
// as the first argument of an application in tail position.
bool
pureLoop(std::ostream& errStream, const GCPtr<AST>& ast, 
	 std::string nm, bool isTail)
{
  bool result = true;

  switch(ast->astType) {
  case at_Null:
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
    return true;

  case at_ident:
    {
      if (!isTail && ast->s == nm) {
	errStream << ast->loc.asString()
		  << ": error: loop name "
		  << nm
		  << " appears in non-tail position."
		  << std::endl;
       
	result = false;
      }

      break;
    }

  case at_usesel:
    // Since the resolver passed, we know that the loop name
    // cannot be used in an at_usesel
    return true;

  case at_begin:
  case at_ibegin:
  {
    for (size_t i = 0; i < ast->children.size() - 1; i++) {
      GCPtr<AST> child = ast->children[i];
      if (!pureLoop(errStream, child, nm, false))
	result = false;
    }
    GCPtr<AST> child = ast->children[ast->children.size()-1];
    if (!pureLoop(errStream, child, nm, isTail))
      result = false;
    break;
  }

  case at_if:
    {
      GCPtr<AST> tst = ast->children[0];
      GCPtr<AST> thn = ast->children[1];
      GCPtr<AST> els = ast->children[2];

      if (!pureLoop(errStream, tst, nm, false))
	result = false;
      if (!pureLoop(errStream, thn, nm, isTail))
	result = false;
      if (!pureLoop(errStream, els, nm, isTail))
	result = false;

      break;
    }

  case at_apply:
    {
      for (size_t i = 0; i < ast->children.size(); i++) {
	GCPtr<AST> child = ast->children[i];
	if (!pureLoop(errStream, child, nm, (i > 0) ? false : isTail))
	  result = false;
      }
      break;
    }

  case at_try:
    {
      // Try expr tail if try is tail
      if (!pureLoop(errStream, ast->children[0], nm, isTail))
	result = false;
      // case_legs tail if try is tail:
      if (!pureLoop(errStream, ast->children[1], nm, isTail))
	result = false;
      // otherwise tail if try is tail:
      if (!pureLoop(errStream, ast->children[2], nm, isTail))
	result = false;
      break;
    }

  case at_loop:
    {
      /* FIXME later (LOOP2LETREC) */
      GCPtr<AST> last = loop2letrec(ast);
      GCPtr<AST> bindings = last->children[0];
      GCPtr<AST> binding = bindings->children[0];
      GCPtr<AST> looplambda = binding->children[1];

      if (!pureLoop(errStream, looplambda->children[1], nm, isTail))
	result = false;
      break;
    }

  case at_let:
  case at_letrec:
    {
      // all of these have the form "letbindings body"
      GCPtr<AST> bindings = ast->children[0];
      GCPtr<AST> body = ast->children[1];

      CVector<std::string> names;

      // Collect all of the names defined in this binding form:
      for (size_t i = 0; i < bindings->children.size(); i++) {
	GCPtr<AST> binding = bindings->children[i];
	GCPtr<AST> bp = binding->children[0];

	getDefNames(bp, names);
      }

      bool shadowed = false;

      // See if the identifier of interest is being shadowed:
      for (size_t i = 0; i < names.size(); i++) {
	if (names[i] == nm) {
	  shadowed = true;
	  break;
	}
      }

      // If this is a letrec, or if the loop identifier is not
      // shadowed, need to check the defining expressions.
      if (ast->astType == at_letrec || !shadowed) {
	for (size_t i = 0; i < bindings->children.size(); i++) {
	  GCPtr<AST> binding = bindings->children[i];
	  GCPtr<AST> expr = binding->children[1];

	  if (!pureLoop(errStream, expr, nm, false))
	    result = false;
	}
      }

      // If the name is not being shadowed, need to check the body
      // recursively:
      if (!shadowed && !pureLoop(errStream, body, nm, isTail));
	result = false;

      break;
    }

  case at_case:
    {
      // FIX: This is wrong!
      if (!pureLoop(errStream, ast->children[0], nm, false))
	result = false;
      // case_legs tail if case is tail:
      if (!pureLoop(errStream, ast->children[1], nm, isTail))
	result = false;
      // otherwise tail if case is tail:
      if (!pureLoop(errStream, ast->children[2], nm, isTail))
	result = false;
      break;
    }

  case at_cond:
    {
      // case_legs tail if case is tail:
      if (!pureLoop(errStream, ast->children[0], nm, isTail))
	result = false;
      // otherwise tail if case is tail:
      if (!pureLoop(errStream, ast->children[1], nm, isTail))
	result = false;
      break;
    }

  case at_cond_legs:
  case at_case_legs:
    {
      // Children tail if parent is tail.
      for (size_t i = 0; i < ast->children.size(); i++) {
	GCPtr<AST> child = ast->children[i];
	if (!pureLoop(errStream, child, nm, isTail))
	  result = false;
      }
      break;
    }
  case at_cond_leg:
  case at_case_leg:
    {
      // FIX: Need to check shadowing in the case leg value pattern.
      // Use in condition is not tail
      if (!pureLoop(errStream, ast->children[0], nm, false))
	result = false;
      // Use in expr tail if parent is tail
      if (!pureLoop(errStream, ast->children[1], nm, isTail))
	result = false;
      break;
    }
  case at_otherwise:
    {
      // Use in expr tail if parent is tail
      if (!pureLoop(errStream, ast->children[0], nm, isTail))
	result = false;
      break;
    }

  // If it appears under any of these, it's not in tail position:
  case at_and:
  case at_or:

  case at_throw:
  case at_setbang:
  case at_deref:
    //  case at_tuple:
  case at_makevector:
  case at_vector:
  case at_array:
    //  case at_tupleref:
  case at_select:
  case at_array_ref:
  case at_vector_ref:
  case at_array_length:
  case at_vector_length:
    {
      for (size_t i = 0; i < ast->children.size(); i++) {
	GCPtr<AST> child = ast->children[i];
	if (!pureLoop(errStream, child, nm, false))
	  result = false;
      }
      break;
    }

  case at_lambda:
  case at_ilambda:
    {
      errStream << ast->loc.asString()
		<< ": error: Something is wrong. This lambda should not have"
		<< " survived to this pass."
		<< std::endl;
      exit(1);
      break;
    }
  default:
    {
      errStream << ast->loc.asString()
		<< ": error: Unhandled case in pureLoop()."
		<< std::endl;
      exit(1);
      break;
    }
  }

  return result;
}

bool
hasPureLoops(std::ostream& errStream, const GCPtr<AST>& ast)
{
  bool result = true;

  switch(ast->astType) {
  case at_loop:
    {
      /* FIXME later (LOOP2LETREC) */
      GCPtr<AST> last = loop2letrec(ast);
      GCPtr<AST> bindings = last->children[0];
      GCPtr<AST> binding = bindings->children[0];
      GCPtr<AST> idPattern = binding->children[0];
      GCPtr<AST> id = idPattern->children[0];
      GCPtr<AST> looplambda = binding->children[1];
      GCPtr<AST> body = last->children[1];

      if (!pureLoop(errStream, looplambda->children[1], id->s, true))
	result = false;
      if (!hasPureLoops(errStream, looplambda))
	result = false;

      break;
    }      

  default:
    {
      for (size_t i = 0; i < ast->children.size(); i++) {
	if (!hasPureLoops(errStream, ast->children[i]))
	  result = false;
      }
      break;
    }
  }


  return result;
}

bool
hasLambda(std::ostream& errStream, const GCPtr<AST>& ast)
{
  bool result = false;

  if (ast->astType == at_lambda || ast->astType == at_ilambda) {
    errStream << ast->loc
	      << ": Error: bootstrap backend does not support "
	      << ast->atKwd()
	      << " in this context."
	      << std::endl;
    result = true;
  }

  // Singleton lambda in the at_loop special form is okay.
  if (ast->astType == at_loop) {
    /* FIXME later (LOOP2LETREC) */
    GCPtr<AST> last = loop2letrec(ast);
    GCPtr<AST> bindings = last->children[0];
    GCPtr<AST> binding = bindings->children[0];
    GCPtr<AST> looplambda = binding->children[1];
    GCPtr<AST> body = last->children[1];

    for (size_t i = 0; i < looplambda->children.size(); i++) {
      if (hasLambda(errStream, looplambda->children[i]))
	result = true;
    }
    if (hasLambda(errStream, body))
      result = true;
  }
  else {
    for (size_t i = 0; i < ast->children.size(); i++) {
      if (hasLambda(errStream, ast->children[i]))
	result = true;
    }
  }

  return result;
}

bool
hasInnerLambdas(std::ostream& errStream, const GCPtr<AST>& ast)
{
  bool result = false;

  switch(ast->astType) {
  case at_define:
    {
      // First child is ID, second child is value or (permitted) LAMBDA 
      // or ILAMBDA. Children of 2nd child should not have a lambda

      GCPtr<AST> expr = ast->children[1];

      for (size_t i = 0; i < expr->children.size(); i++) {
	if (hasLambda(errStream, expr->children[i]))
	  result = true;
      }

      break;
    }      

  default:
    break;
  }

  for (size_t i = 0; i < ast->children.size(); i++) {
    if (hasInnerLambdas(errStream, ast->children[i]))
      result = true;
  }

  return result;
}

bool
hasPatternBindings(std::ostream& errStream, const GCPtr<AST>& ast)
{
  bool result = false;

  switch(ast->astType) {
  case at_define:
    {
      GCPtr<AST> bp = ast->children[0];

      if (ast->getType()->kind == ty_fn) {
      }
    }

  case at_letbinding:
    {
      GCPtr<AST> bp = ast->children[0];
      if (bp->astType != at_identPattern) {
	errStream << bp->loc.asString()
		  << ": Error: bootstrap backend does not support "
		  << "binding patterns."
		  << std::endl;
	result = true;
      }
      break;
    }
  default:
    break;
  }

  for (size_t i = 0; i < ast->children.size(); i++) {
    if (hasPatternBindings(errStream, ast->children[i]))
      result = true;
  }

  return result;
}

// Simplify certain constructs so that the code generator
// has an easier time of it.
static bool
SimplifyAST(std::ostream& errStream, const GCPtr<AST>& ast)
{
  bool result = true;

  switch(ast->astType) {
  case at_and:
  case at_or:
    {
      // Reduce and/or to max of two arguments.
      if (ast->children.size() > 2) {
	GCPtr<AST> newAST = new AST(ast->astType, ast->loc);

	newAST->symType = ast->symType;
	newAST->scheme = ast->scheme;
	newAST->uocInfo = ast->uocInfo;  
	newAST->expr = ast->expr;  
	newAST->isDecl = false;

	for (size_t ndx = 1; ndx < ast->children.size(); ndx++)
	  newAST->children.append(ast->child(ndx));

	while(ast->children.size() > 2)
	  ast->children.remove(1);

	ast->children[1] = newAST;
      }

      break;
    }

  default:
    break;
  }

  for (size_t i = 0; i < ast->children.size(); i++) {
    if (!SimplifyAST(errStream, ast->children[i]))
      result = false;
  }

  return result;
}

// Very important to keep this sorted (excluding the guard entry),
// since it is searched with bsearch.
static char *kwd_blacklist[] = {
  "BitcArray",			// bitc runtime
  "BitcDouble_t",		// bitc runtime
  "BitcFloat_t",		// bitc runtime
  "BitcInt16_t",		// bitc runtime
  "BitcInt32_t",		// bitc runtime
  "BitcInt64_t",		// bitc runtime
  "BitcInt8_t",			// bitc runtime
  "BitcPair",			// bitc runtime
  "BitcQuad_t",			// bitc runtime
  "BitcString",			// bitc runtime
  "BitcUnit_t",			// bitc runtime
  "BitcUns16_t",		// bitc runtime
  "BitcUns32_t",		// bitc runtime
  "BitcUns64_t",		// bitc runtime
  "BitcUns8_t",			// bitc runtime
  "BitcVector",			// bitc runtime
  "BitcWord_t",			// bitc runtime
  "asm",			// c++
  "auto",
  "bool",			// c++, later ANSI C
  "break",
  "case",
  "catch",			// c++
  "cdecl",			// c++
  "char",
  "class",			// c++
  "const",
  "const_cast",			// c++
  "continue",
  "default",
  "defined",			// some extensions
  "delete",			// c++
  "do",
  "double",
  "dynamic_cast",		// c++
  "else",
  "entry",			// some extensions
  "enum",
  "explicit",			// c++
  "extern",
  "false",			// c++, C99
  "far",			// MS-DOS
  "float",
  "for",
  "fortran",			// VMS extension
  "friend",			// c++
  "generic",			// ???
  "globaldef",			// ???
  "globalref",			// ???
  "globalvalue",		// ???
  "goto",
  "huge",			// MS-DOS
  "if",
  "inline",
  "int",
  "int16",			// BSD, POSIX
  "int32",			// BSD, POSIX
  "int64",			// BSD, POSIX
  "int8",			// BSD, POSIX
  "interface",			// c++ (MS)
  "long",
  "mutable",			// c++
  "namespace",
  "near",			// MS-DOS
  "new",			// C++
  "operator",			// C++
  "pascal",			// VMS extension
  "pragma",			// various
  "private",			// c++
  "protected",			// c++
  "public",			// c++
  "quad",			// shap
  "readonly",			// various
  "register",
  "reinterpret_cast",		// c++
  "return",
  "short",
  "signed",
  "sizeof",
  "static",
  "static_cast",		// c++
  "string",			// shap
  "struct",
  "super",			// c++ (MS)
  "switch",
  "template",			// c++
  "this",			// c++
  "throw",			// c++
  "true",			// c++, C99
  "try",			// c++
  "typedef",
  "typeid",			// c++
  "typename",			// c++
  "uint16",			// BSD, POSIX
  "uint32",			// BSD, POSIX
  "uint64",			// BSD, POSIX
  "uint8",			// BSD, POSIX
  "union",
  "unsigned",
  "use",
  "using",
  "virtual",			// c++
  "void",
  "volatile",
  "while",
  "word",			// just begging for trouble


  0				// GUARD ENTRY
};
enum { nkwd = (sizeof(kwd_blacklist)/sizeof(kwd_blacklist[0])) - 1 };

int istrcmp(const void *vp1, const void *vp2)
{
  const char *s1 = *((const char **) vp1);
  const char *s2 = *((const char **) vp2);
  return strcmp(s1, s2);
}

bool is_kwd(const std::string& s)
{
  const char *cs = s.c_str();

  if (bsearch(&cs, kwd_blacklist, nkwd, sizeof(kwd_blacklist[0]), istrcmp))
    return true;
  return false;
}

const struct MangleMap {
  char c;
  const char *s;
} mangleMap[] = {
  { '#',  "_SH" },
  { '!',  "_EX" },
  { '$',  "_DL" },
  { '%',  "_PC" },
  { '*',  "_ST" },
  { '+',  "_PL" },
  { '\\', "_RS" },
  { '-',  "_HY" },
  { '/',  "_FS" },
  { '<',  "_LT" },
  { '>',  "_GT" },
  { '=',  "_EQ" },
  { '?',  "_QU" },
  { '@',  "_AT" },
  { '_',  "_US" },
  { '~',  "_TL" },

  // Following are introduced as a side effect of dotted names and fqns.
  { '.',  "::" },		// separator within IFNAME
#ifdef NAMESPACE
  { ':',  ":" },		// handles IFNAME::IDENT
#else
  { ':',  "__" },		// note special pattern
#endif
};
enum { nSpecial = sizeof(mangleMap) / sizeof(MangleMap) };

std::string
mangleComponent(const std::string& s)
{
  stringstream ss;

  for (size_t i = 0; i < s.size(); i++) {
    if (isalnum(s[i])) {
      ss << s[i];
    }
    else {
      size_t sp;
      for (sp = 0; sp < nSpecial; sp++) {
	if (mangleMap[sp].c == s[i]) {
	  ss << mangleMap[sp].s;
	  break;
	}
      }

      // We should have found it.
      assert(sp != nSpecial);
    }
  }

  std::string os = ss.str();
#if 0
  return (is_kwd(os) ? ('_' + os) : os);
#else
  return 'I' + os;
#endif
}

static std::string
xmangle(std::string s, const char *pfx)
{
  stringstream ss;

  size_t pos;

  while((pos = s.find(".")) != std::string::npos) {
    ss << mangleComponent(s.substr(0, pos));
#ifdef NAMESPACE
    ss << "::";
#else
    ss << "__";
#endif
    s = s.substr(pos+1);
  }

  ss << pfx;
  ss << mangleComponent(s);

  return ss.str();
}

static inline std::string
xmangle(const FQName& fqn, const char *pfx)
{
  return xmangle(fqn.asString("."), pfx);
}

static std::string 
idname(GCPtr<AST> ast, const char *pfx = "")
{
  if (ast->isGlobal())
    return xmangle(ast->fqn, pfx);

  if (ast->identFlags & ID_IS_GENSYM)
    return ast->s;
  
  return xmangle(ast->s, pfx);
}

#define CD_FNCONST   1u
#define CD_IN_STRUCT 2u
std::string
c_decl(TvPrinter& tvp, GCPtr<Type> ty, const std::string& id,
       unsigned flags = 0)
{
  stringstream ss;
  std::string sep = " ";

  ty = ty->getType();

  if (ty->kind == ty_mutable) {
    ss << c_decl(tvp, ty->components[0]->typ, id, flags & ~CD_FNCONST);
    return ss.str();
  }

  // Stuff that shows up BEFORE the identifier:
  switch(ty->kind) {

  case ty_bool:
    ss << "bool";
    break;
  case ty_char:
    ss << "BitcChar_t";
    break;
  case ty_string:
    ss << "BitcString_t *";
    break;
  case ty_int8:
    ss << "BitcInt8_t";
    break;
  case ty_int16:
    ss << "BitcInt16_t";
    break;
  case ty_int32:
    ss << "BitcInt32_t";
    break;
  case ty_int64:
    ss << "BitcInt64_t";
    break;
  case ty_uint8:
    ss << "BitcUns8_t";
    break;
  case ty_uint16:
    ss << "BitcUns16_t";
    break;
  case ty_uint32:
    ss << "BitcUns32_t";
    break;
  case ty_uint64:
    ss << "BitcUns64_t";
    break;
  case ty_word:
    ss << "BitcWord_t";
    break;
  case ty_float:
    ss << "BitcFloat_t";
    break;
  case ty_double:
    ss << "BitcDouble_t";
    break;
  case ty_quad:
    ss << "BitcQuad_t";
    break;

  case ty_bitfield:
    {
      ss << c_decl(tvp, ty->components[0]->typ, "");
      break;
    }
    
  case ty_tvar:
    ss << tvp.tvName(ty);
    break;

  case ty_unionv:
  case ty_unionr:
  case ty_structv:
  case ty_structr:
  case ty_closure:    
    {
      GCPtr<AST> defAst = ty->defAst;
      ss << idname(defAst);

      if (ty->typeArgs.size()) {
	ss << "< ";
	
	for (size_t tparam = 0; tparam < ty->typeArgs.size(); tparam++) {
	  GCPtr<Type> targ = ty->typeArgs[tparam]->getType();

	  if (tparam > 0)
	    ss << ", ";
	  ss << c_decl(tvp, targ, "");
	}

	ss << " >";
      }
      if (ty->kind == ty_unionr || 
	  ty->kind == ty_structr ||
	  ty->kind == ty_closure)
	ss << " *";
    }
    break;

  case ty_ref:
    {
      assert(ty->components.size() == 1);
      ss << "BitcRef< " << c_decl(tvp, ty->components[0]->typ, "")
	 << " >";
      // ss << c_decl(tvp, ty->components[0]->typ, "*");
      break;
    }

  case ty_vector:
    {
      assert(ty->components.size() == 1);
      ss << "BitcVector< " << c_decl(tvp, ty->components[0]->typ, "")
	 << " > *";
      break;
    }

  case ty_array:
    {
      assert(ty->components.size() == 1);
      ss << "BitcArray< " << c_decl(tvp, ty->components[0]->typ, "")
	 << ", "
	 << ty->components[0]->typ->getType()->arrlen
	 << " >";
      break;
    }

  case ty_pair:
    {
      assert(ty->components.size() == 2);
      ss << "BitcPair< " 
	 << c_decl(tvp, ty->components[0]->typ, "")
	 << ", "
	 << c_decl(tvp, ty->components[1]->typ, "")
	 << " >";
      break;
    }

  case ty_fn:
    {
      // There is a problem here. The issue is that ty_fn is used 
      // both for function pointers and for actual (constant) functions, 
      // and we need to decipher which one we are looking at. Part
      // of the problem is that some type information has been lost here.
      //
      // There are two issues at hand:
      //
      // 1. It is perfectly legal for the user to write something like
      //
      //      (define f (mutable (lambda (x) ...)))
      //
      //    This form REQUIRES a pointer, but on the positive side
      //    the value restriction guarantees that it has no type scheme.
      //    This means that we can actually declare it correctly to
      //    C++
      //
      // 2. Unfortunately, it is ALSO perfectly legal to write something
      //    like
      //
      //      (define f (lambda (x) x))
      //
      //    The good news is that this is a procedure constant and can
      //    be declared as a normal C procedure, but the bad news is
      //    that it may have a type scheme.
      //
      // Ultimately, I decided to resolve this by demanding some context
      // from the AST.

      assert(ty->components.size() == 2);

      // Emit the return type first:
      ss << c_decl(tvp, ty->components[1]->typ, "");

      ss << ((flags & CD_FNCONST) ? " " : " (*");
      sep = "";

      break;
    }

  default:
    ss << "/* UNHANDLED TYPE " << ty->kindName() << " */";
    break;
  }

  if (id.size())
    ss << sep << id;

  // Stuff that shows up AFTER the identifier:

  switch(ty->kind) {
  case ty_bitfield:
    {
      if (flags & CD_IN_STRUCT)
	ss << ":" << ty->Isize;
      break;
    }
    
  case ty_fn:
    {
      ss << ((flags & CD_FNCONST) ? "(" : ")(");

      GCPtr<Type> fnargs = ty->components[0]->typ->getType();

      for (size_t i = 0; i < fnargs->components.size(); i++) {
	if (i > 0) ss << ", ";
	ss << c_decl(tvp, fnargs->components[i]->typ, "");
      }

      ss << ")";

      break;
    }

  case ty_bool:
  case ty_char:
  case ty_string:
  case ty_int8:
  case ty_int16:
  case ty_int32:
  case ty_int64:
  case ty_uint8:
  case ty_uint16:
  case ty_uint32:
  case ty_uint64:
  case ty_word:
  case ty_float:
  case ty_double:
  case ty_quad:

  case ty_tvar:
  case ty_unionv:
  case ty_unionr:
  case ty_structv:
  case ty_structr:
  case ty_closure:
  case ty_ref:
  case ty_vector:
  case ty_pair:
    // No post-decl output
    break;

  default:
    ss << "/* UNHANDLED POST-DECL " << ty->kindName() << " */";
    break;
  }

  return ss.str();
}

#if 0
std::string
c_name(GCPtr<AST> ast)
{
  stringstream ss;

#if 0
  if (ast->defn) 
    assert(ast = ast->defn);
#endif

#if 0
  GCPtr<Type> ty = ast->getType();

  if (ast->isGlobal) {
    switch(ty->kind) {
    case ty_tvar:
      ss << "T_";
      break;
    case ty_fn:
      ss << "F_";
      break;
    case ty_structv:
      ss << "S_";
      break;
    case ty_unionv:
      ss << "U_";
      break;
    case ty_structr:
    case ty_closure:
      ss << "RS_";
      break;
    case ty_unionr:
      ss << "RU_";
      break;
    case ty_array:
      ss << "V_";
      break;
    case ty_vector:
      ss << "V_";
      break;
    case ty_ref:
      ss << "V_";
      break;
    default:
      ss << "UNK_";
      break;
    }
  }
#endif

  ss << mangle(ast);

  return ss.str();
}
#endif

static std::string
EmitTypeScheme(TvPrinter& tvp, const GCPtr<AST>& id, DeclsOrDefs what)
{
  stringstream ss;

  const GCPtr<TypeScheme>& scm = id->scheme;
  if (!scm || scm->ftvs.size() == 0)
    return "";

  ss << "template<";
  for (size_t i = 0; i < scm->ftvs.size(); i++) {
    GCPtr<Type> ty = scm->ftvs[i]->getType();
    assert(ty->kind == ty_tvar);

    if (i > 0) ss << ", ";
    ss << "typename " << tvp.tvName(ty);
  }
  ss << ">";

  if (what == WantDecl)
    ss << "\n";
  else
    ss << " ";

  return ss.str();
}

std::string
EmitTypeExpansion(TvPrinter& tvp, const GCPtr<AST>& id, DeclsOrDefs what)
{
  stringstream ss;

  const GCPtr<TypeScheme>& scm = id->scheme;
  if (!scm || scm->ftvs.size() == 0)
    return "";

  ss << "<";
  for (size_t i = 0; i < scm->ftvs.size(); i++) {
    GCPtr<Type> ty = scm->ftvs[i]->getType();
    assert(ty->kind == ty_tvar);

    if (i > 0) ss << ", ";
    ss << tvp.tvName(ty);
  }
  ss << ">";
  return ss.str();
}

static void
EmitExpr(INOstream& ino, TvPrinter& tvp, GCPtr<AST> ast)
{
  switch(ast->astType) {
  case at_ibegin:
  case at_begin:
    {
      ino << "({ ";
      ino.indent(3);

      for (size_t i = 0; i < ast->children.size(); i++) {
	if (i > 0) ino << ", ";
	EmitExpr(ino, tvp, ast->child(i));
      }

      ino.indent(-3);
      ino << " ;})";

      break;
    }
#if 0
  case at_identPattern:
    {
      EmitExpr(ino, tvp, ast->child(0));
      break;
    }
#endif

  case at_ident:
    {
      ino << idname(ast);
      break;
    }

  case at_if:
    {
      ino << "(";
      EmitExpr(ino, tvp, ast->child(0));
      ino << std::endl;
      ino.more();
      ino << "? ";
      EmitExpr(ino, tvp, ast->child(1));
      ino << std::endl;
      ino << ": ";
      EmitExpr(ino, tvp, ast->child(2));
      ino << ")";
      ino << std::endl;

      ino.less();
      break;
    }

  case at_and:
    {
      assert(ast->children.size() == 2);

      ino << "({ ";
      ino.indent(3);

      ino << c_decl(tvp, ast->child(0)->getType(), "_tmp", 0);
      ino << " = ";
      EmitExpr(ino, tvp, ast->child(0));
      ino << ";" << std::endl;

      ino << "(_tmp ? ";
      EmitExpr(ino, tvp, ast->child(1));
      ino << std::endl;
      ino.indent(6);
      ino << ": _tmp) ;})";
      ino << std::endl;
      ino.indent(-6);
      ino.indent(-3);

      break;
    }

  case at_or:
    {
      assert(ast->children.size() == 2);

      ino << "({ ";
      ino.indent(3);

      ino << c_decl(tvp, ast->child(0)->getType(), "_tmp", 0);
      ino << " = ";
      EmitExpr(ino, tvp, ast->child(0));
      ino << ";" << std::endl;

      ino << "(_tmp ? _tmp" << std::endl;
      ino.indent(6);
      ino << ": ";
      EmitExpr(ino, tvp, ast->child(1));
      ino << ") ;})";
      ino << std::endl;
      ino.indent(-6);
      ino.indent(-3);

      break;
    }

  case at_pair:
  case at_cpair:
    {
      ino << "BitcPair< " 
	  << c_decl(tvp, ast->child(0)->getType(), "")
	  << ", "
	  << c_decl(tvp, ast->child(1)->getType(), "")
	  << " >(";
      EmitExpr(ino, tvp, ast->child(0));
      ino << ", ";
      EmitExpr(ino, tvp, ast->child(1));
      ino << ")";
      break;
    }

  case at_vector:
    {
      ino << "({ "
	  << c_decl(tvp, ast->getType(), "")
	  << " *_tmp;" << std::endl;

      ino.indent(3);
      ino << "void (*_markfn)("
	  << c_decl(tvp, ast->getType(), "")
	  << " *&) = gcMark;" << std::endl;

      ino << "_tmp = " 
	  << std::endl;
      ino.more();
      ino << "( " << c_decl(tvp, ast->getType(), "") 
	  << " *)";
      ino << "  allocate(sizeof(*_tmp) + sizeof("
	  << c_decl(tvp, ast->child(0)->getType(), "")
	  << ") * " << ast->children.size() 
	  << ", "
	  << "(void (*)(...))_markfn"
	  << ");" << std::endl;
      ino.less();

      ino << "_tmp->length = " 
	  << ast->children.size() << ";"
	  << std::endl;
      
      for (size_t c = 0; c < ast->children.size(); c++) {
	ino << "_tmp->elements[" << c << "] = ";
	EmitExpr(ino, tvp, ast->child(c));
	ino << ";" << std::endl;
      }

      ino << "_tmp ;})";
      ino.indent(-3);
      break;
    }

  case at_makevector:
    {
      ino << "({ "
	  << c_decl(tvp, ast->child(0)->getType(), "_len")
	  << " = ";
      EmitExpr(ino, tvp, ast->child(0));
      ino << ";" << std::endl;

      ino.indent(3);

      ino << c_decl(tvp, ast->child(1)->getType(), "_e")
	  << " = ";
      EmitExpr(ino, tvp, ast->child(1));
      ino << ";" << std::endl;

      ino << c_decl(tvp, ast->getType(), "")
	  << " *_tmp;" << std::endl;

      ino << "void (*_markfn)("
	  << c_decl(tvp, ast->getType(), "")
	  << " *&) = gcMark;" << std::endl;

      ino << "_tmp = " 
	  << std::endl;
      ino.more();
      ino << "( " << c_decl(tvp, ast->getType(), "") 
	  << " *)";
      ino << "  allocate(sizeof(*_tmp) + sizeof("
	  << c_decl(tvp, ast->child(1)->getType(), "")
	  << ") * _len, "
	  << "(void (*)(...))_markfn"
	  << ");" << std::endl;
      ino.less();

      ino << "_tmp->length = _len;"
	  << std::endl;
      
      ino << "for(BitcWord_t ndx = 0; ndx < _len; ndx++)"
	  << std::endl;
      ino.more();
      ino << "_tmp->elements[ndx] = _e;" << std::endl;
      ino.less();
      
      ino << "_tmp ;})";
      ino.indent(-3);
      break;
    }

  case at_array:
    {
      ino << "({ "
	  << c_decl(tvp, ast->getType(), "")
	  << " _tmp;" << std::endl;

      ino.indent(3);

      for (size_t c = 0; c < ast->children.size(); c++) {
	ino << "_tmp.elements[" << c << "] = ";
	EmitExpr(ino, tvp, ast->child(c));
	ino << ";" << std::endl;
      }

      ino << "_tmp ;})";
      ino.indent(-3);
      break;
    }

  case at_array_length:
    {
      GCPtr<Type> ty = ast->child(0)->getType();

      ino << "({ ";
      EmitExpr(ino, tvp, ast->child(0));
      ino << "," << std::endl;

      ino << ty->arrlen
	  << " ;})";
      break;
    }

  case at_vector_length:
    {
      ino << "({ "
	  << c_decl(tvp, ast->child(0)->getType(), " *_tmp = ")
	  << std::endl;
      ino.indent(3);
      ino.more();
      EmitExpr(ino, tvp, ast->child(0));
      ino << ";" << std::endl;
      ino.less();
      ino << "_tmp->length ;})";
      ino.indent(-3);
      break;
    }
  case at_array_ref:
    {
      EmitExpr(ino, tvp, ast->child(0));
      ino << ".elements[";
      EmitExpr(ino, tvp, ast->child(1));
      ino << "]";
      break;
    }
  case at_vector_ref:
    {
      EmitExpr(ino, tvp, ast->child(0));
      ino << "->elements[";
      EmitExpr(ino, tvp, ast->child(1));
      ino << "]";
      break;
    }

  case at_deref:
    {
      ino << "*(";
      EmitExpr(ino, tvp, ast->child(0));
      ino << ")";
      break;
    }

  case at_select:
    {
      EmitExpr(ino, tvp, ast->child(0));
      ino << "."
	  << idname(ast->child(1));
      break;
    }

  case at_setbang:
    {
      ino << "({ "
	  << c_decl(tvp, ast->child(0)->getType(), "& _target")
	  << " = ";
      EmitExpr(ino, tvp, ast->child(0));
      ino << ";" << std::endl;
      ino.indent(3);
      ino << "_target = ";
      EmitExpr(ino, tvp, ast->child(1));
      ino << "; _target ;})" << std::endl;
      ino.indent(-3);
      break;
    }

  case at_charLiteral:
    {
      std::streamsize w = ino.ostrm.width();
      char fillChar = ino.ostrm.fill('0');
      
      ino.ostrm << right << oct;

      ino << "'\\";
      ino << ast->litValue.c;
      ino.ostrm << dec;
      ino.ostrm.width(w);
      ino.ostrm.fill(fillChar);
      ino << "'";

      break;
    }

  case at_boolLiteral:
    {
      ino << (ast->litValue.b
	      ? "true" 
	      : "false");
      break;
    }

  case at_intLiteral:
    {
      // An int literal is going to show up as either a ty_int
      // or it is one of the magical ty_struct types defined in the prelude.
      // In the latter case, component[0] is the type of the first
      // field, which is the "v" field, and *that* tells us how to
      // generate this.

      GCPtr<Type> ty = ast->getType();

      char valStr[mpz_sizeinbase(ast->litValue.i, 10)];
      mpz_get_str(valStr, 10, ast->litValue.i);

      assert(ty->kind != ty_structv); // legacy issue
      ino << valStr;

      break;
    }

  case at_floatLiteral:
    {
      GCPtr<Type> ty = ast->getType();

      // FIX: (shap) how to tell which kind?
      // FIX: (shap) This is flatly wrong!
      double d = mpf_get_d(ast->litValue.d);

      assert(ty->kind != ty_structv); // legacy issue
      ino << d;

      break;
    }
  case at_stringLiteral:
    // This one is going to need a helper function in the runtime.
    {
      // needs to initialize the vector properly...
      const char *s = ast->litValue.s.c_str();
      const char *sbegin = s;

      ino << "mkStringLiteral(\"";
      
      std::streamsize w = ino.ostrm.width();
      char fillChar = ino.ostrm.fill('0');
      
      ino.ostrm << right << oct;

      while ((size_t)(s - sbegin) < ast->litValue.s.size())
	ino << "\\" << LitValue::DecodeRawCharacter(s, &s);

      ino.ostrm << dec;
      ino.ostrm.width(w);
      ino.ostrm.fill(fillChar);

      ino << "\")";

      break;
    }

  case at_case:
  case at_cond:
  case at_let:
  case at_letrec:
  case at_loop:
  case at_throw:
  case at_try:
  case at_unit:
  case at_apply:
  case at_struct_apply:
  case at_ucon_apply:
  default:
    {
      ino << "/* AST " << ast->ID << ": " << ast->astTypeName() 
	  << " */" << std::endl;
      ino << ast->astTypeName() << " IS NOT IMPLEMENTED IN EmitExpr YET"
	  << std::endl;
      assert(false);
      break;
    }
  }
}

static void
EmitDeclValue(INOstream& ino, GCPtr<AST> ast, DeclsOrDefs what)
{
  if (what != WantForward)
    return;

  GCPtr<AST> id = ast->child(0);

  TvPrinter tvp("TV_");

  // If it is global and mutable, then c_decl will recursively call
  // itself in the ty_mutable handler in such a way as to suppress the
  // isFnConst argument, so it should be okay to pass it blindly here.
  ino << EmitTypeScheme(tvp, id, what)
      << "extern " << c_decl(tvp, id->getType(), idname(id), 
			     id->isGlobal() ? CD_FNCONST : 0)
      << ";" << std::endl;
}

static void
EmitDefine(INOstream& ino, GCPtr<AST> ast, DeclsOrDefs what)
{
  GCPtr<AST> idPattern = ast->child(0);
  GCPtr<AST> id = idPattern->child(0);

  TvPrinter tvp("TV_");

  if (what == WantDefs) {
    ino << "/* AST " << ast->ID << " " << ast->astTypeName() 
	<< " " << id->s << " (" << id->fqn.asString() << ")"
	<< " */" << std::endl;

    switch(id->getType()->kind) {
    case ty_fn:
      {
	GCPtr<AST> lambda = ast->child(1);
	GCPtr<AST> args = lambda->child(0);
	GCPtr<AST> body = lambda->child(1);

	ino << EmitTypeScheme(tvp, id, what)
	    << c_decl(tvp, id->getType()->components[1]->typ, "")
	    << std::endl << idname(id)
	    << "(";

	for (size_t arg = 0; arg < args->children.size(); arg++) {
	  GCPtr<AST> idPattern = args->child(arg);
	  GCPtr<AST> id = idPattern->child(0);

	  if (arg > 0) ino << ", ";
	  ino << c_decl(tvp, id->getType(), idname(id));
	}

	ino << ")" << std::endl;
	ino << "{" << std::endl;
	ino.more();

	ino << "return ";
	ino.indent(7);

	EmitExpr(ino, tvp, body);
	ino << ";" << std::endl;
	ino.indent(-7);

	ino.less();
	ino << "}" << std::endl;

	ino << std::endl;
	break;
      }
    default:
      {
	GCPtr<AST> body = ast->child(1);
	ino << c_decl(tvp, id->getType(), idname(id), 
		      id->isGlobal() ? CD_FNCONST : 0)
	    << " = ";
	EmitExpr(ino, tvp, body);
	ino << ";" << std::endl;
	break;
      }
    }
  }
  else if (what == WantDecl) {
    // If it is global and mutable, then c_decl will recursively call
    // itself in the ty_mutable handler in such a way as to suppress the
    // isFnConst argument, so it should be okay to pass it blindly here.
    ino << EmitTypeScheme(tvp, id, what)
	<< "extern " << c_decl(tvp, id->getType(), idname(id), 
			       id->isGlobal() ? CD_FNCONST : 0)
	<< ";" << std::endl;
  }
}

static void
EmitDefexcept(INOstream& ino, GCPtr<AST> ast, DeclsOrDefs what)
{
  GCPtr<AST> id = ast->child(0);

  // Exceptions are ALWAYS reference types!
  assert(id->symType->isRefType());

  // Note assert swapped by Swaroop
  // assert(id->scheme == 0);
  assert(id->scheme->ftvs.size() == 0);

  TvPrinter tvp("TV_");

  if (what == WantForward) {
    ino << "struct " << idname(id) << ";" << std::endl;
  }
  else if (what == WantDecl) {
    ino << "struct " << idname(id) << ":public BitcException {"
	<< " /* " << id->fqn.asString() << " */" << std::endl;

    ino.more();

    ino << "static BitcWord_t tagValue;" << std::endl;

    ino << idname(id) << "()" << std::endl;
    ino << ": BitcException(&tagValue)" << std::endl;
    ino << "{ }" << std::endl;

    for (size_t i = 0; i < ast->children.size() - 1; i++) {
      GCPtr<AST> fld = ast->child(i+1);

      stringstream argName;
      argName << "arg" << i;

      ino << c_decl(tvp, fld->getType(), argName.str())
	  << ";" << std::endl;
    }

    ino.less();
    ino << "};" << std::endl;
  }
  else if (what == WantGC) {
    // Emit mark routine:
    ino << "inline void gcMark(" << idname(id)
	<< "*& pob"		// always a reference type
	<< ")" << std::endl
	<< "{" << std::endl;
    ino.more();

    if (ast->children.size() > 1) {
      ino << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "& ob __attribute__((unused)) = *pob;" << std::endl;

      for (size_t ndx = 0; ndx < ast->children.size() - 1; ndx++) {
	GCPtr<AST> fld = ast->child(ndx+1);

	if (!fld->symType->isAtomic())
	  ino << "gcMark(ob.arg" << ndx << ");" << std::endl;
      }
    }

    ino.less();
    ino << "}" << std::endl;
  }
  else if (what == WantConstructors) {
    // Emit the static, one-time declaration for the tag word:
    ino << "BitcWord_t " << idname(id)
	<< "::tagValue = 0;" << std::endl;

    // For arity zero exceptions, we will simply use a statically
    // declared instance, but we do need to instantiate it:
    if (ast->children.size() == 1) {
      ino << idname(id)
	  << " the" << idname(id)
	  << ";" << std::endl;

      return;
    }

    // Below here we are handling arity > 0. 

    ino << "inline " << idname(id)
	<< "* C" << idname(id) << "(";

    for (size_t ndx = 0; ndx < ast->children.size()-1; ndx++) {
      GCPtr<AST> arg = ast->child(ndx+1);

      stringstream ss;
      ss << "arg" << ndx;

      if (ndx > 0) ino << ", ";
      ino << c_decl(tvp, arg->getType(), ss.str());
    }

    ino << ")" << std::endl;

    ino << "{" << std::endl;
    ino.more();

    ino << "void (*_markfn)(" << idname(id)
	<< EmitTypeExpansion(tvp, id, what)
	<< "*&) = gcMark;" << std::endl;

    ino << idname(id)
	<< "* _pInstance = (" 
	<< idname(id)
	<< "*)" << std::endl;

    ino << "  allocate(sizeof("
	<< idname(id)
	<< "), "
	<< "(void (*)(...))_markfn"
	<< ");" << std::endl;

    ino << idname(id)
	<< "& _instance = *_pInstance;" << std::endl << std::endl;

    // Dynamically allocated BitcExceptions never get their C++
    // constructors run, so we need to set the tag pointer by hand.
    ino << "_instance.tag = &"
	<< idname(id)
	<< "::tagValue;" << std::endl;

    for (size_t ndx = 0; ndx < ast->children.size()-1; ndx++) {
      GCPtr<AST> arg = ast->child(ndx);

      ino << "_instance.arg" << ndx
	  << " = " << "arg" << ndx
	  << ";" << std::endl;
    }

    ino << "return &_instance;" << std::endl;

    ino.less();
    ino << "}" << std::endl;
  }
}

static void
EmitDefstruct(INOstream& ino, GCPtr<AST> ast, DeclsOrDefs what)
{
  GCPtr<AST> id = ast->child(0);

  //ino << "/* AST " << ast->ID << ": " << ast->astTypeName() 
  //<< " " << c_name(id)
  //<< " */" << std::endl;

  TvPrinter tvp("TV_");

  if (what == WantForward) {
    ino << EmitTypeScheme(tvp, id, what)
	<< "struct " << idname(id) << ";" << std::endl;

    ino << EmitTypeScheme(tvp, id, what)
	<< "extern " << idname(id,"C") << "(";

    GCPtr<AST> fields = ast->children[4];
    for (size_t i = 0; i < fields->children.size(); i++) {
      GCPtr<AST> field = fields->children[i];
      GCPtr<AST> fname = field->children[0];

      if (i > 0) ino << ", ";
      ino << c_decl(tvp, fname->getType(), "");
    }

    ino << ");" << std::endl;
    return;
  }
  else if (what == WantDecl) {
    ino << EmitTypeScheme(tvp, id, what)
	<< "struct " << idname(id) << " {"
	<< " /* " << id->fqn.asString() << " */" << std::endl;

    ino.more();

    GCPtr<AST> fields = ast->children[4];
    for (size_t i = 0; i < fields->children.size(); i++) {
      GCPtr<AST> field = fields->children[i];
      GCPtr<AST> fname = field->children[0];
      ino << c_decl(tvp, fname->getType(), idname(fname), CD_IN_STRUCT)
	  << ";" << std::endl;
    }
    ino.less();

    ino << "};" << std::endl;
  }
  else if (what == WantGC) {
    GCPtr<AST> fields = ast->children[4];

    // Emit mark routine:
    ino << EmitTypeScheme(tvp, id, what)
	<< "inline void gcMark(" << idname(id)
	<< EmitTypeExpansion(tvp, id, what)
	<< (id->symType->isRefType()? "*& pob" : "& ob")
	<< ")" << std::endl
	<< "{" << std::endl;
    ino.more();

    if (id->symType->isRefType()) {
      ino << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "& ob __attribute__((unused)) = *pob;" << std::endl;
    }

    for (size_t i = 0; i < fields->children.size(); i++) {
      GCPtr<AST> field = fields->children[i];
      GCPtr<AST> fname = field->children[0];

      if (!fname->symType->isAtomic())
	ino << "gcMark(ob." << idname(fname) << ");" << std::endl;
    }
    ino.less();
    ino << "}" << std::endl;
  }
  else if (what == WantConstructors) {
    GCPtr<AST> fields = ast->children[4];

    // Emit constructor routine:

    ino << EmitTypeScheme(tvp, id, what)
	<< "inline " << idname(id)
	<< EmitTypeExpansion(tvp, id, what)
	<< (id->symType->isRefType() ? "*" : "")
	<< " "
	<< idname(id,"C") << "(";

    for (size_t i = 0; i < fields->children.size(); i++) {
      GCPtr<AST> field = fields->children[i];
      GCPtr<AST> fname = field->children[0];
      if (i > 0) ino << ", ";
      ino << c_decl(tvp, fname->getType(), idname(fname));
    }

    ino << ")" << std::endl;
    ino << "{" << std::endl;
    ino.more();

    if (id->symType->isRefType()){
      ino << "void (*_markfn)(" << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "*&) = gcMark;" << std::endl;

      ino << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "* _pInstance = (" 
	  << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "*)" << std::endl;
      ino << "  allocate(sizeof("
	  << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "), "
	  << "(void (*)(...))_markfn"
	  << ");" << std::endl;

      ino << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "& _instance = *_pInstance;" << std::endl;
    }
    else {
      ino << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << " _instance;" << std::endl;
    }

    for (size_t i = 0; i < fields->children.size(); i++) {
      GCPtr<AST> field = fields->children[i];
      GCPtr<AST> fname = field->children[0];
      ino << "_instance." << idname(fname)
	  << "= " << idname(fname) << ";" << std::endl;
    }

    ino << "return "
	<< (id->symType->isRefType() ? "&" : "")
	<< "_instance;" << std::endl;

    ino.less();
    ino << "}" << std::endl;
  }
}

static void
EmitDefunion(INOstream& ino, GCPtr<AST> ast, DeclsOrDefs what)
{
  GCPtr<AST> id = ast->child(0);
  GCPtr<AST> legs = ast->children[4];

  //  ino << "/* AST " << ast->ID << ": " << ast->astTypeName() 
  //      << " " << c_name(id)
  //      << " */" << std::endl;

  TvPrinter tvp("TV_");

  if (what == WantForward) {
    ino << EmitTypeScheme(tvp, id, what)
	<< "struct " << idname(id) << ";" << std::endl;

    // Swaroop had a clever idea, which was to simply predeclare all
    // of the arity zero constants. Unfortunately, C++ does not know
    // how to do templated values, so I have resorted here to the less
    // desirable alternative of templated procedures taking no
    // arguments. The constructors themselves can, at least, return
    // references to statically allocated instances.
    //
    // As long as they inline properly, they should all dissappear.
    for (size_t i = 0; i < legs->children.size(); i++) {
      GCPtr<AST> leg = legs->children[i];
      GCPtr<AST> cname = leg->children[0];

      ino << EmitTypeScheme(tvp, id, what)
	  << "extern struct "
	  << idname(id) 
	  << ((leg->children.size() == 1) ? "& C" : " C")
	  << idname(cname) << "(";

      if (leg->children.size() > 1) {
	for (size_t ndx = 1; ndx < leg->children.size(); ndx++) {
	  GCPtr<AST> arg = leg->child(ndx);

	  if (ndx > 0) ino << ", ";
	  ino << c_decl(tvp, arg->getType(), "");
	}
      }

      ino << ");" << std::endl;
    }

    return;
  }
  else if (what == WantDecl) {
    ino << EmitTypeScheme(tvp, id, what)
	<< "struct " << idname(id) << " {"
	<< " /* " << id->fqn.asString() << " */" << std::endl;

    bool needUnion = false;

    ino.more();

    // Print the tag value enumeration:
    ino << "enum { " << std::endl;

    ino.more();

    for (size_t i = 0; i < legs->children.size(); i++) {
      GCPtr<AST> leg = legs->children[i];
      GCPtr<AST> cname = leg->children[0];
      if (leg->children.size() > 1)
	needUnion = true;

      // ino << "    " << mangle(cname->fqn.asString());
      ino << "tag" << idname(cname);
      if (i < (legs->children.size() - 1))
	ino << ",  ";
      else
	ino << "   ";
      ino << "/* "
	  << cname->s
	  << " */"
	  << std::endl;
    }

    ino.less();

    ino << "} tag;" << std::endl;

    //ino << "  "
    //<< mangle("prelude::word")
    //<< " tag;" << std::endl;

    // In order to actually emit a union, we need to know if
    // any of the legs have members.

    if (needUnion) {
      ino << "union {" << std::endl;

      // Children of union node are it's legs:

      ino.more();

      for (size_t i = 0; i < legs->children.size(); i++) {
	GCPtr<AST> leg = legs->children[i];
	GCPtr<AST> cname = leg->children[0];
	if (leg->children.size() > 1) {
	  ino << "struct {" << std::endl;
	  ino.more();

	  for (size_t ndx = 0; ndx < leg->children.size()-1; ndx++) {
	    GCPtr<AST> arg = leg->child(ndx+1);

	    stringstream argName;
	    argName << "arg" << ndx;

	    ino << c_decl(tvp, arg->getType(), argName.str(), CD_IN_STRUCT)
		<< ";" << std::endl;
	  }
	  ino.less();
	  ino << "} "<< idname(cname) << ";" << std::endl;
	}
      }

      ino.less();
      ino << "} u;" << std::endl;
    }

    // Implement the static singleton instances for the arity zero
    // value constructors:
    for (size_t i = 0; i < legs->children.size(); i++) {
      GCPtr<AST> leg = legs->children[i];
      GCPtr<AST> cname = leg->children[0];

      if (leg->children.size() == 1)
	ino << "static " << idname(id) << " " 
	    << idname(cname) << ";" << std::endl;
    }

    ino.less();
    ino << "};" << std::endl;
  }
  else if (what == WantGC) {
    GCPtr<AST> fields = ast->children[4];

    // Emit mark routine:
    ino << EmitTypeScheme(tvp, id, what)
	<< "inline void gcMark(" << idname(id)
	<< EmitTypeExpansion(tvp, id, what)
	<< (id->symType->isRefType() ? "*& pob" : "& ob")
	<< ")" << std::endl;
    ino << "{" << std::endl;
    ino.more();
    if (id->symType->isRefType()) {
      ino << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "& ob __attribute__((unused))= *pob;" << std::endl;
    }

    ino << "switch(ob.tag) {" << std::endl;

    for (size_t i = 0; i < legs->children.size(); i++) {
      GCPtr<AST> leg = legs->children[i];
      GCPtr<AST> cname = leg->children[0];

      ino << "case " << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "::tag" << idname(cname) << ":" << endl;
      ino.more();
      ino << "{" << std::endl;
      ino.more();

      if (leg->children.size() > 1) {
	for (size_t ndx = 0; ndx < leg->children.size()-1; ndx++) {
	  GCPtr<AST> arg = leg->child(ndx+1);

	  stringstream argName;
	  argName << "arg" << ndx;

	  if (!arg->symType->isAtomic())
	    ino << "gcMark(ob.u." << idname(cname) << "."
		<< argName.str() << ");" << std::endl;
	}
      }
      ino << "break;" << std::endl;

      ino.less();
      ino << "}" << std::endl;
      ino.less();
    }

    ino << "}" << std::endl;

    ino.less();
    ino << "}" << std::endl;

  }
  else if (what == WantConstructors) {
    for (size_t i = 0; i < legs->children.size(); i++) {
      GCPtr<AST> leg = legs->children[i];
      GCPtr<AST> cname = leg->children[0];

      // For arity zero constructors, we will simply use
      // the statically declared instance, but we do need
      // to instantiate it:
      if (leg->children.size() == 1) {
	ino << EmitTypeScheme(tvp, id, what)
	    << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << " " << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << "::" << idname(cname) 
	    << " = { "
	    << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << "::tag" << idname(cname)
	    << " };" << std::endl;

	continue;
      }

      // Below here we are handling arity > 0

      ino << EmitTypeScheme(tvp, id, what)
	  << "inline " << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << (id->symType->isRefType() ? "*" : "")
	  << " C" << idname(cname) << "(";

      for (size_t ndx = 0; ndx < leg->children.size()-1; ndx++) {
	GCPtr<AST> arg = leg->child(ndx+1);

	stringstream ss;
	ss << "arg" << ndx;

	if (ndx > 0) ino << ", ";
	ino << c_decl(tvp, arg->getType(), ss.str());
      }

      ino << ")" << std::endl;

      ino << "{" << std::endl;
      ino.more();

      if (id->symType->isRefType()){
	ino << "void (*_markfn)(" << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << "*&) = gcMark;" << std::endl;

	ino << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << "* _pInstance = (" 
	    << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << "*)" << std::endl;

	ino << "  allocate(sizeof("
	    << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << "), "
	    << "(void (*)(...))_markfn"
	    << ");" << std::endl;

	ino << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << "& _instance = *_pInstance;" << std::endl << std::endl;
      }
      else {
	ino << idname(id)
	    << EmitTypeExpansion(tvp, id, what)
	    << " _instance;" << std::endl << std::endl;
      }

      ino << "_instance.tag = "
	  << idname(id)
	  << EmitTypeExpansion(tvp, id, what)
	  << "::tag" << idname(cname)
	  << ";" << std::endl;

      for (size_t ndx = 0; ndx < leg->children.size()-1; ndx++) {
	GCPtr<AST> arg = leg->child(ndx);

	ino << "_instance.u." << idname(cname) << ".arg" << ndx
	    << " = " << "arg" << ndx
	    << ";" << std::endl;
      }

      if (id->symType->isRefType())
	ino << "return &_instance;" << std::endl;
      else
	ino << "return _instance;" << std::endl;

      ino.less();
      ino << "}" << std::endl;
    }
  }
}

static void
EmitSUDecl(INOstream& ino, GCPtr<AST> ast, DeclsOrDefs what)
{
  if (what != WantDecl)
    return;
  // EmitDecl must recognize certain standard prelude types
  // as special cases.

  GCPtr<AST> id = ast->child(0);
  ino << "/* AST " << ast->ID << ": " << ast->astTypeName() 
      << " " << idname(id)
      << " */" << std::endl;
}

static void
EmitAst(INOstream& ino, GCPtr<AST> ast, DeclsOrDefs what)
{
  switch(ast->astType) {
  case at_defexception:
    {
      EmitDefexcept(ino, ast, what);
      break;
    }
  case at_defstruct:
    {
      EmitDefstruct(ino, ast, what);
      break;
    }
  case at_defunion:
    {
      EmitDefunion(ino, ast, what);
      break;
    }
  case at_define:
    {
      EmitDefine(ino, ast, what);
      break;
    }

  case at_declValue:
    {
      EmitDeclValue(ino, ast, what);
      break;
    }

  case at_declunionr:
  case at_declstructr:
    {
      EmitSUDecl(ino, ast, what);
      break;
    }

  default:
    {
      ino << "/* AST " << ast->ID << ": " << ast->astTypeName() 
	  << " */" << std::endl;
      break;
    }
  }
}

static void
GenerateInterface(INOstream& ino, const GCPtr<UocInfo> &uoc, 
		  DeclsOrDefs what)
{
  assert(uoc->ast->children.size() == 2);
  assert(uoc->ast->child(0)->astType == at_version);

  GCPtr<AST> srcmod = uoc->ast->child(1);

  assert((srcmod->astType == at_module) ||
	 (srcmod->astType == at_interface));

  // An at_interface's first child is an at_ident giving the if_name.
  // Skip that.
  for (size_t i = (srcmod->astType == at_interface) ? 1 : 0;
       i < srcmod->children.size(); i++) {
    GCPtr<AST> ast = srcmod->child(i);
    
    EmitAst(ino, ast, what);
  }
}

bool
CheckBootstrapConstraints(std::ostream& errStream, const GCPtr<UocInfo> &uoc)
{
  bool ok = true;

  if (hasPatternBindings(errStream, uoc->ast))
    ok = false;

  if(hasInnerLambdas(errStream, uoc->ast))
    ok = false;

  if(!hasPureLoops(errStream, uoc->ast))
    ok = false;

  return ok;
}

#if 0
void
GenerateC(std::ostream& out, const GCPtr<UocInfo> &uoc)
{
#if 0
  if(!escapeCheck(uoc->ast)) {
    out << "Exiting due to errors during escape analysis." 
	<< std::endl;
    exit(1);
  }
#endif

  INOstream ino(out);

  ino << "namespace " << mangle(uoc->ifName) << " {" << std::endl;
  ino.more();

  GenerateInterface(ino, uoc, WantDecls);

  ino.less();
  ino << "} /* namespace " << mangle(uoc->ifName) << " */;" << std::endl;

  GenerateInterface(ino, uoc, WantDefs);
}
#endif

bool
GenerateC()
{
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::ifList[i];

    if (!CheckBootstrapConstraints(std::cerr, uoc)) {
      std::cerr << "Errors found during bootstrap constraint check of " 
		<< uoc->path
		<< std::endl;
      return false;
    }

    if (!SimplifyAST(std::cerr, uoc->ast)) {
      std::cerr << "Unable to simplify AST for code generation in " 
		<< uoc->path
		<< std::endl;
      return false;
    }

    AlphaRename(uoc->ast);
  }

  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::srcList[i];

    if (!CheckBootstrapConstraints(std::cerr, uoc)) {
      std::cerr << "Errors found during bootstrap constraint check of " 
		<< uoc->path
		<< std::endl;
      return false;
    }

    if (!SimplifyAST(std::cerr, uoc->ast)) {
      std::cerr << "Unable to simplify AST for code generation in " 
		<< uoc->path
		<< std::endl;
      return false;
    }

    AlphaRename(uoc->ast);
  }

  if (Options::outputFileName.size() == 0)
    Options::outputFileName = "/dev/stdout";

  std::ofstream out(Options::outputFileName.c_str(),
		    std::ios_base::out|std::ios_base::trunc);
  
  if (!out.is_open()) {
    std::cerr << "Couldn't open output file \""
	      << Options::outputFileName
	      << "\" -- "
	      << strerror(errno)
	      << "\n";
    exit(1);
  }

  INOstream ino(out);

  ino << "#include \""
      << BITCCDIR
      << "/BitcRuntime.hxx\"" << std::endl;

#if 0
  // Emit forward declarations for all interfaces:
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::ifList[i];

    ino << "namespace " << mangle(uoc->ifName) << " {" << std::endl;
    ino.more();

    GenerateInterface(ino, uoc, WantForward);

    ino.less();
    ino << "} /* namespace " << mangle(uoc->ifName) << " */;" << std::endl;
  }

  // .. and for all sources:
  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::srcList[i];

    ino << "namespace " << mangle(uoc->ifName) << " {" << std::endl;
    ino.more();

    GenerateInterface(ino, uoc, WantForward);

    ino.less();
    ino << "} /* namespace " << mangle(uoc->ifName) << " */;" << std::endl;
  }
#endif

  // Emit declarations for all interfaces and sources:
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::ifList[i];

#ifdef NAMESPACE
    ino << "namespace " << xmangle(uoc->ifName) << " {" << std::endl;
    ino.more();
#endif

    GenerateInterface(ino, uoc, WantDecl);

#ifdef NAMESPACE
    ino.less();
    ino << "} /* namespace " << xmangle(uoc->ifName) << " */;" << std::endl;
#endif
  }

  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::srcList[i];

#ifdef NAMESPACE
    ino << "namespace " << xmangle(uoc->ifName) << " {" << std::endl;
    ino.more();
#endif

    GenerateInterface(ino, uoc, WantDecl);

#ifdef NAMESPACE
    ino.less();
    ino << "} /* namespace " << xmangle(uoc->ifName) << " */;" << std::endl;
#endif
  }

  // Emit GC routines for all interfaces and sources:
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::ifList[i];

    GenerateInterface(ino, uoc, WantGC);
  }

  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::srcList[i];

    GenerateInterface(ino, uoc, WantGC);
  }

  // Emit constructor implementations for all interfaces and sources:
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::ifList[i];

#ifdef NAMESPACE
    ino << "namespace " << xmangle(uoc->ifName) << " {" << std::endl;
    ino.more();
#endif

    GenerateInterface(ino, uoc, WantConstructors);

#ifdef NAMESPACE
    ino.less();
    ino << "} /* namespace " << xmangle(uoc->ifName) << " */;" << std::endl;
#endif
  }

  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::srcList[i];

#ifdef NAMESPACE
    ino << "namespace " << xmangle(uoc->ifName) << " {" << std::endl;
    ino.more();
#endif

    GenerateInterface(ino, uoc, WantConstructors);

#ifdef NAMESPACE
    ino.less();
    ino << "} /* namespace " << xmangle(uoc->ifName) << " */;" << std::endl;
#endif
  }

  // FIX: This needs to move into the runtime support library.
  ino << "/* HELPER ROUTINE */" << std::endl;
  // Emit a special helper routine to deal with string literals:
  ino << "BitcString_t *" << std::endl;
  ino << "mkStringLiteral(const char *s)" << std::endl;
  ino << "{" << std::endl;
  ino.more();
  ino << "size_t len = strlen(s);" << std::endl;
  ino << "void (*markfn)(BitcString_t *&) = gcMark;" << std::endl;
  ino << std::endl;
  ino << "BitcString_t *tmp =" << std::endl;
  ino.more();
  ino << "(BitcString_t *) allocate(sizeof(BitcString_t) "
      << "+ sizeof(tmp->s[0]) * len," 
      << std::endl;
  ino << "                  (void (*)(...)) markfn);" << std::endl;
  ino.less();

  ino << "tmp->length = len;" << std::endl;
  ino << "for(size_t i = 0; i < len; i ++)" << std::endl;
  ino.more();
  ino << "tmp->s[i] = s[i];" << std::endl;
  ino.less();

  ino << "return tmp;" << std::endl;

  ino.less();
  ino << "}";

  // Emit definitions for all interfaces and sources:
  for(size_t i = 0; i < UocInfo::ifList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::ifList[i];

    GenerateInterface(ino, uoc, WantDefs);
  }

  for(size_t i = 0; i < UocInfo::srcList.size(); i++) {
    GCPtr<UocInfo> uoc = UocInfo::srcList[i];

    GenerateInterface(ino, uoc, WantDefs);
  }

  // Just for testing:
#ifdef NAMESPACE
  ino << "using namespace Iprelude;" << std::endl;
  ino << std::endl;
#endif
  ino << "int main(int argc, char *argv[])" << std::endl;
  ino << "{" << std::endl;
  ino.more();
  ino << "BitcInt32_t i = 1;" << std::endl;
  ino << "BitcChar_t  c = 'c';" << std::endl;
  ino << "BitcFloat_t f = 3.414;" << std::endl;
  ino << std::endl;
#ifdef NAMESPACE
  ino << "(void) Icgen::CIstr3(i, c, f);" << std::endl;
#else
  ino << "(void) Icgen__CIstr3(i, c, f);" << std::endl;
#endif
  ino << std::endl;
  ino << "majorGC();" << std::endl;
  ino.less();
  ino << "}" << std::endl;
  return true;
}
	  
