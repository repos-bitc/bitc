#ifndef TYPEEQINFER_HXX
#define TYPEEQINFER_HXX
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

#include <iostream>
#include <sstream>
#include <libsherpa/CVector.hxx>
#include "AST.hxx"
#include "Type.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"

void
addSubCst(GCPtr<AST> errAst, GCPtr<Type> t1, GCPtr<Type> t2,
	  GCPtr<Constraints> tcc);

void
addEqCst(GCPtr<AST> errAst, GCPtr<Type> t1, GCPtr<Type> t2,
	 GCPtr<Constraints> tcc);
	
void
addCcCst(GCPtr<AST> errAst, GCPtr<Type> t1, GCPtr<Type> t2,
	 GCPtr<Constraints> tcc);
void
addPcst(GCPtr<AST> errAst, GCPtr<Type> t, GCPtr<Constraints> tcc);

bool
EqUnify(std::ostream& errStream, GCPtr<Constraints> cset);

#endif /* TYPEEQINFER_HXX */
