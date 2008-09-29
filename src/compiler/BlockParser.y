%{
  /*
   * Copyright (C) 2008, The EROS Group, LLC.
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

#include <sys/fcntl.h>
#include <sys/stat.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <dirent.h>
#include <iostream>

#include <string>

#include "Version.hxx"
#include "AST.hxx"
#include "ParseType.hxx"
#include "UocInfo.hxx"
#include "Options.hxx"
  
using namespace boost;
using namespace sherpa;
using namespace std;  

#define YYSTYPE ParseType
#define YYLEX_PARAM (BlockLexer *)lexer
#undef yyerror
#define yyerror(lexer, s) lexer->ReportParseError(s)

#include "BlockLexer.hxx"

#define SHOWPARSE(s) \
  do { \
    if (Options::showParse) \
      lexer->errStream << (s) << std::endl;		\
  } while (false);
#define SHOWPARSE1(s,x) \
  do { \
    if (Options::showParse) \
      lexer->errStream << (s) << " " << (x) << std::endl;	\
  } while (false);


inline int block_lex(YYSTYPE *lvalp, BlockLexer *lexer)
{
  return lexer->lex(lvalp);
}

// If the passed exprSeq has a documentation string at the front,
// remove it. Note that if the exprSeq has length 1 then the string is
// the expression value and not a documentation comment.
static shared_ptr<AST> 
stripDocString(shared_ptr<AST> exprSeq)
{
  if (exprSeq->children.size() > 1 &&
      exprSeq->child(0)->astType == at_stringLiteral)
    exprSeq->disown(0);

  return exprSeq;
}

%}

%pure-parser
%parse-param {BlockLexer *lexer}

%token <tok> tk_Reserved	/* reserved words */

/* Categorical terminals: */
%token <tok> tk_Ident
%token <tok> tk_TypeVar
%token <tok> tk_EffectVar
%token <tok> tk_Int
%token <tok> tk_Float
%token <tok> tk_Char
%token <tok> tk_String


/* Primary types and associated hand-recognized literals: */
%token <tok> '(' ')' ','	/* unit */
%token <tok> '[' ']'	/* unit */
%token <tok> tk_BOOL
%token <tok> tk_TRUE   /* #t */
%token <tok> tk_FALSE  /* #f */
%token <tok> tk_CHAR
%token <tok> tk_STRING
%token <tok> tk_FLOAT
%token <tok> tk_DOUBLE
%token <tok> tk_DUP
%token <tok> tk_INT8
%token <tok> tk_INT16
%token <tok> tk_INT32
%token <tok> tk_INT64
%token <tok> tk_UINT8
%token <tok> tk_UINT16
%token <tok> tk_UINT32
%token <tok> tk_UINT64
%token <tok> tk_WORD

%token <tok> tk_BITFIELD
%token <tok> tk_FILL
%token <tok> tk_RESERVED // not to be confused with tk_Reserved
%token <tok> tk_WHERE
  
%token <tok> tk_BITC_VERSION

%token <tok> tk_PURE
%token <tok> tk_IMPURE
%token <tok> tk_CONST

%token <tok> tk_THE
%token <tok> tk_IF
%token <tok> tk_WHEN
%token <tok> tk_AND
%token <tok> tk_OR
%token <tok> tk_NOT
%token <tok> tk_COND
%token <tok> tk_SWITCH
%token <tok> tk_CASE
%token <tok> tk_OTHERWISE

%token <tok> tk_BLOCK tk_RETURN_FROM
%token <tok> tk_RETURN tk_CONTINUE

%token <tok> tk_PAIR
%token <tok> tk_VECTOR
%token <tok> tk_ARRAY
 //%token <tok> tk_MAKE_VECTOR
%token <tok> tk_MAKE_VECTORL
%token <tok> tk_ARRAY_LENGTH
%token <tok> tk_VECTOR_LENGTH
%token <tok> tk_ARRAY_NTH
%token <tok> tk_VECTOR_NTH

%token <tok> tk_REF
%token <tok> tk_VAL

%token <tok> tk_SET
%token <tok> tk_TAG

%token <tok> tk_DEREF
%token <tok> tk_LAMBDA

%token <tok> tk_STRUCT
%token <tok> tk_UNION
%token <tok> tk_REPR
%token <tok> tk_EXCEPTION
%token <tok> tk_TYPECLASS
%token <tok> tk_INSTANCE

%token <tok> tk_TRY
%token <tok> tk_CATCH
%token <tok> tk_THROW

%token <tok> tk_DO
%token <tok> tk_WHILE

%token <tok> tk_METHOD
%token <tok> tk_FORALL

%token <tok> tk_LET
%token <tok> tk_LETREC
%token <tok> tk_OPAQUE
%token <tok> tk_PROCLAIM
%token <tok> tk_MUTABLE
%token <tok> tk_EXTERNAL
%token <tok> tk_INTERFACE
%token <tok> tk_MODULE
// token tk_USESEL historic significance only 
// %token <tok> tk_USESEL
%token <tok> tk_PACKAGE
%token <tok> tk_IMPORT
%token <tok> tk_PROVIDE

%token <tok> tk_TYFN
//%token <tok> tk_SUPER
%token <tok> tk_SUSPEND

%type <ast> ident defident useident
%type <ast> exident
%type <tok> ifident
%type <ast> intLit floatLit charlit strLit boolLit  

%%

// Parser built for version 0.9
// Section Numbers indicated within []

// COMPILATION UNITS [2.5]
// This definition od start mus be changed as it ignores
// junk after the body.

start: version uoc_body {
  SHOWPARSE("start -> version uoc_body");
  return 0;
};

start: uoc_body {
  SHOWPARSE("start -> uoc_body");
  return 0;
};

uoc_body: {
}

//start: interface {
//  SHOWPARSE("start -> interface");
//
//  return 0;
//};

//uoc_body: interface {
//  SHOWPARSE("uocbody -> interface");
//}
//
//uoc_body: implicit_module {
//  SHOWPARSE("uocbody -> implicit_module");
//}
//
//uoc_body: module_seq {
//  SHOWPARSE("uocbody -> module_seq");
//}

// VERSION [2.5]

// We cannot do optversion, because that would require two token look-ahead.
version: tk_BITC_VERSION strLit ';' {
  SHOWPARSE("version -> BITC-VERSION strLit ;");
  if (!CheckVersionCompatibility($2->s)) {
    std::string s = ": Warning: BitC version conflict " + $2->s + " vs " + Version();
    lexer->ReportParseWarning($2->loc, s);
  }
}; 

ifident: {
    lexer->setIfIdentMode(true);
  } ident {
  lexer->setIfIdentMode(false);
  $$ = LToken($2->loc, $2->s);
};
ifident: ifident '.' {
    lexer->setIfIdentMode(true);
  } tk_Ident {
  lexer->setIfIdentMode(false);
  $$ = LToken($1.loc, $1.str + "." + $4.str);
};

// External identifiers are not subject to reserved word restrictions...
exident: tk_Ident {
  SHOWPARSE("exident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

exident: tk_Reserved {
  SHOWPARSE("exident -> <Reserved " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

// IDENTIFIERS [2.2]
ident: tk_Ident {
  SHOWPARSE("ident -> <Ident " + $1.str + ">");
  $$ = AST::make(at_ident, $1);
};

ident: tk_Reserved {
  SHOWPARSE("ident -> <RESERVED=" + $1.str + ">");
  cerr << $1.loc.asString() << ": The token \"" << $1.str 
       << "\" is reserved for future use.\n";
  lexer->num_errors++;
  $$ = AST::make(at_ident, $1);
};

useident: ident {
  SHOWPARSE("useident -> ident");
  $$ = $1;
};

useident: ident '.' ident { 
  SHOWPARSE("useident -> ident . ident");
  $$ = AST::make(at_usesel, $1->loc, $1, $3);
};

//defident: ident {
//  SHOWPARSE("defident -> ident");
//  $1->flags |= (ID_IS_GLOBAL);
//  $$ = $1;
//};

defident: useident {
  SHOWPARSE("defident -> useident");
  $1->flags |= (ID_IS_GLOBAL);
  $$ = $1;
};

// TYPE VARIABLES [3.3]
typevar: tk_TypeVar {
  SHOWPARSE("typevar -> <TypeVar=" + $1.str + ">");
  $$ = AST::make(at_ident, $1);
  $$->identType = id_tvar;
}; 

// /* Literal Value Representations */

boolLit: tk_TRUE {
  SHOWPARSE("boolLit -> <Bool=" + $1.str +">");
  $$ = AST::makeBoolLit($1);
};
boolLit: tk_FALSE {
  SHOWPARSE("boolLit -> <Bool=" + $1.str +">");
  $$ = AST::makeBoolLit($1);
};

charlit: tk_Char {
  SHOWPARSE("charlit -> <Char=" + $1.str +">");
  $$ = AST::makeCharLit($1);
};

intLit: tk_Int {
  SHOWPARSE("intLit -> <Int=" + $1.str +">");
  $$ = AST::makeIntLit($1);
};

floatLit: tk_Float {
  SHOWPARSE("floatLit -> <Float=" + $1.str +">");
  $$ = AST::makeFloatLit($1);
};

strLit: tk_String {
  SHOWPARSE("strLit -> <String=" + $1.str +">");
  $$ = AST::makeStringLit($1);
};

%%

