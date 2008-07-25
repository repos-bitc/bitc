#ifndef TYPEMUT_HXX
#define TYPEMUT_HXX

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

#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include "AST.hxx"
#include "Type.hxx"
 
/* Different options for creating type records. Thsese helper
   functions are in place so that all maybe-records are created at one
   place. Some functions exist so that we cah flip copy behavious in
   some cases easily */

 
/* All type-variables are maybe-types by default, 
   We use bare tvars in some cases (ex: when the sole purpose of
   creating the type variable is unification of the types across all
   legs of a datastructure or expression.*/

static inline sherpa::GCPtr<Type> 
newTvar()
{
  return new Type(ty_tvar);
}

static inline sherpa::GCPtr<Type> 
MBF(sherpa::GCPtr<Type> t)
{
  return new Type(ty_mbFull, newTvar(), t->minimizeMutability());
}

static inline sherpa::GCPtr<Type> 
MBT(sherpa::GCPtr<Type> t)
{
  return new Type(ty_mbTop, newTvar(), t->getBareType());
}

#endif /* TYPEMUT_HXX */
