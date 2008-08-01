#ifndef TYPEINFERUTIL_HXX
#define TYPEINFERUTIL_HXX
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
#include "Options.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeScheme.hxx"
#include "Typeclass.hxx"

boost::shared_ptr<Type>
obtainFullUnionType(boost::shared_ptr<Type> t);

bool
initGamma(std::ostream& errStream, 
	  boost::shared_ptr<TSEnvironment > gamma,
	  boost::shared_ptr<InstEnvironment > instEnv,
	  const boost::shared_ptr<AST> ast, unsigned long uflags);

void
useIFGamma(const std::string& idName,
	   boost::shared_ptr<TSEnvironment > fromEnv, 
	   boost::shared_ptr<TSEnvironment > toEnv);

void
useIFInsts(const std::string& idName,
	   boost::shared_ptr<InstEnvironment >fromEnv, 
	   boost::shared_ptr<InstEnvironment >toEnv);

bool
initGamma(std::ostream& errStream, 
	  boost::shared_ptr<TSEnvironment > gamma,
	  boost::shared_ptr<InstEnvironment > instEnv,
	  const boost::shared_ptr<AST> ast, unsigned long uflags);

size_t
nCtArgs(boost::shared_ptr<Type> t);

#endif /* TYPEINFERUTIL_HXX */
