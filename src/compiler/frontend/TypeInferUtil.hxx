#ifndef TYPEINFERCOMMON_HXX
#define TYPEINFERCOMMON_HXX
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

#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "Pair.hxx"
#include "Type.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"

GCPtr<Type>
obtainFullUnionType(GCPtr<Type> t);

bool
initGamma(std::ostream& errStream, 
	  GCPtr<Environment<TypeScheme> > gamma,
	  GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	  const GCPtr<AST> ast, unsigned long uflags);
bool
checkImpreciseTypes(std::ostream& errStream, 
		    const GCPtr<Environment<TypeScheme> > gamma,
		    GCPtr<CVector<GCPtr<Pair<GCPtr<Type>, GCPtr<AST> > > > > impTypes);
void
useIFGamma(const std::string& idName,
	   GCPtr<Environment<TypeScheme> > fromEnv, 
	   GCPtr<Environment<TypeScheme> > toEnv);

void
useIFInsts(const std::string& idName,
	   GCPtr<Environment< CVector<GCPtr<Instance> > > >fromEnv, 
	   GCPtr<Environment< CVector<GCPtr<Instance> > > >toEnv);

bool
initGamma(std::ostream& errStream, 
	  GCPtr<Environment<TypeScheme> > gamma,
	  GCPtr<Environment< CVector<GCPtr<Instance> > > > instEnv,
	  const GCPtr<AST> ast, unsigned long uflags);

size_t
nCtArgs(GCPtr<Type> t);

#endif /* TYPEINFERCOMMON_HXX */
