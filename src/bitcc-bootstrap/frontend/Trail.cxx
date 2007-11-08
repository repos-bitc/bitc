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
#include "Type.hxx"
#include "Trail.hxx"
#include <assert.h>
#include <libsherpa/CVector.hxx>
#include "Eliminate.hxx"

using namespace sherpa;

void
Trail::link(GCPtr<Type> from, GCPtr<Type> to)
{  
  from->link = to; 
  vec->append(from);   
} 

void
Trail::subst(GCPtr<Type> from, GCPtr<Type> to)
{ 
  // This assert() is only theoritically true. 
  // In practice we wrap type variables in maybes and
  // therefore this does not hold.
  // assert(from->kind == ty_tvar);
  // NOT from->isTvar()
  
  link(from, to);
} 

void
Trail::rollBack(size_t upto)
{
  assert(upto <= vec->size());
  GCPtr< CVector<GCPtr<Type> > > newVec = new CVector<GCPtr<Type> >;
  
  for(size_t i = 0; i < upto; i++)
    newVec->append(vec->elem(i));

  for(size_t i = upto; i < vec->size(); i++)
    vec->elem(i)->link = NULL;

  vec = newVec;
}

void
Trail::release(const size_t n, GCPtr<Type> rel)
{
  assert(vec->elem(n) == rel); // not getType()
  
  rel->link = NULL;
  vec = eliminate(vec, n);
}
