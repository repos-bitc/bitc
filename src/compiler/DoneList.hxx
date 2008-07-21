#ifndef DONELIST_HXX
#define DONELIST_HXX

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
#include <iostream>
#include <string>
#include <sherpa/CVector.hxx>
#include <sherpa/GCPtr.hxx>
#include <Eliminate.hxx>
#include <assert.h>

template <class T>
struct DoneList : public Countable {
  GCPtr< sherpa::CVector<T> > vec;
  
  DoneList()
  {
    vec = new sherpa::CVector<T>;
  }

  bool 
  contains(const T &probe) const
  {
    return vec->contains(probe);
  }

  size_t 
  pos(const T& probe) const
  {
    for (size_t i = 0; i < vec->size(); i++)
      if (vec->elem(i) == probe) 
	return i;
    
    assert(false);
  }

  T
  elem(const size_t n) const
  {
    return vec->elem(n);
  }

  size_t 
  size() const
  {
    return vec->size();
  }
  
  bool 
  add(T &probe)
  {
    if(contains(probe))
      return false;

    vec->append(probe);
    return true;
  }
};

#endif /* DONELIST_HXX */
