#ifndef INTER_PASS_HXX
#define INTER_PASS_HXX

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

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"

/* Definitions of mode used in Symbol Resolution 
   and Type-inference */
#define NULL_MODE   0x0u
#define DEF_MODE    0x1u
#define REDEF_MODE  0x2u
#define USE_MODE    0x3u
#define DECL_MODE   0x4u

#define CHKERR(noerr, exp) \
  do {			   \
    bool ans = (exp);	   \
    if (ans == false)	   \
      (noerr) = false;	   \
  }while (0)

#define BE_CHKERR(noerr, exp)	 \
  do {				 \
    bool ans = (exp);		 \
    assert(ans);		 \
    if (ans == false)		 \
      (noerr) = false;		 \
  }while (0)

// Final expression of a let
#define FEXPR(let) (let)->child(1)
// Identifier in a identPattern let
#define IDENT(let) (let)->child(0)->child(0)

// Ignore identifier at the following positions:
// at_switch *ident* expr sw_legs ow
// at_try expr *ident* sw_legs ow
#define IGNORE(ast) ((size_t)(((ast)->astType == at_switch)?0:1))

void BitcP(std::ostream& out, 
	   const boost::shared_ptr<AST> ast, 
	   bool showTypes);

void addDecl(boost::shared_ptr<AST> decl);
extern void fatal();

#endif /* INTER_PASS_HXX */
