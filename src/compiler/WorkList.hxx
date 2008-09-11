#ifndef WORKLIST_HXX
#define WORKLIST_HXX

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
#include <assert.h>
#include <iostream>
#include <string>
#include <vector>
#include "shared_ptr.hxx"

// This is intentionally not a subclass of Donelist. 
// There should be no compatibility.

template <class T, const bool isDoneSet>
struct BaseWorkList {
  typedef std::set<T> WorkSet;
  WorkSet set;
  
  typedef typename WorkSet::iterator iterator;

  BaseWorkList()
  {
  }

  bool 
  contains(const T &probe) const
  {
    return (set.find(probe) != set.end());
  }

  inline iterator begin() { return set.begin(); }
  inline iterator end() {   return set.end(); }

  inline void erase(const iterator& itr) {
    return set.erase(itr);
  }

  inline void erase(const T& key) {
    set.erase(key);
  }

  inline void find(const T& key) {
    return set.find(key);
  }

  void insert(const T &probe) {
    set.insert(probe);
  }

  inline void clear() { set.clear(); }

  inline size_t size() { return set.size(); }

  inline size_t count() { return set.count(); }

  inline bool empty() { return set.empty(); }
};

template <class T>
struct WorkList : public BaseWorkList<T, false> {
  static inline boost::shared_ptr<WorkList<T> >
  make() {
    WorkList<T> *tmp = new WorkList<T>();
    return boost::shared_ptr<WorkList<T> >(tmp);
  }
};

template <class T>
struct DoneList : public BaseWorkList<T, true> {
  static inline boost::shared_ptr<DoneList<T> >
  make() {
    DoneList<T> *tmp = new DoneList<T>();
    return boost::shared_ptr<DoneList<T> >(tmp);
  }
};

#endif /* WORKLIST_HXX */

