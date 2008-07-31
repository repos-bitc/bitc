#ifndef LIBSHERPA_CVECTOR_HXX
#define LIBSHERPA_CVECTOR_HXX

/**************************************************************************
 *
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, The EROS
 *   Group, LLC. 
 * Copyright (C) 2004, 2005, 2006, Johns Hopkins University.
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
#include <libsherpa/GCPtr.hxx>

namespace sherpa {

  template<class T>
  struct CVector {
    enum { increment = 20 };
    size_t len;
    size_t maxLen;
    T *content;

    CVector(size_t sz = 0)
    {
      len = sz;

      if (sz == 0)
	sz = 20;

      content = new T[sz];
      maxLen = sz;
    }

    static inline GCPtr<CVector<T> >
    make(size_t sz = 0) {
      CVector<T> *tmp = new CVector<T>(sz);
      return GCPtr<CVector<T> >(tmp);
    }

    CVector(const CVector<T>& vec)
    {
      len = vec.len;
      maxLen = vec.len;
      content = new T[len];
      for (size_t i = 0; i < len; i++)
	content[i] = vec.content[i];
    }

    ~CVector()
    {
      delete [] content;
    }

    void erase()
    {
      delete [] content;
      len = 0;
      if (maxLen > 20)
	maxLen = 20;
      content = new T[maxLen];
    }

    void insert(size_t ndx, const T& v)
    {
      if (len == maxLen) {
	maxLen += increment;
	T* newContent = new T[maxLen];
	for (size_t i = 0; i < len; i++)
	  newContent[i] = content[i];
	delete [] content;
	content = newContent;
      }

      len++;

      for (size_t i = len-1; i > ndx; i--)
	content[i] = content[i-1];
      content[ndx] = v;
    }

    void insert(size_t ndx, const CVector<T>& vec)
    {
      if (maxLen < len + vec.len) {
	while (maxLen < len + vec.len)
	  maxLen += increment;
	T* newContent = new T[maxLen];
	for (size_t i = 0; i < len; i++)
	  newContent[i] = content[i];
	delete [] content;
	content = newContent;
      }

      len += vec.len;

      for (size_t i = len-1; i > ndx+vec.len; i--)
	content[i] = content[i-vec.len];
      for (size_t i = 0; i < vec.len; i++)
	content[ndx+i] = vec[i];
    }

    void append(const T& v)
    {
      if (len == maxLen) {
	maxLen += increment;
	T* newContent = new T[maxLen];
	for (size_t i = 0; i < len; i++)
	  newContent[i] = content[i];
	delete [] content;
	content = newContent;
      }

      content[len++] = v;
    }

    void append(const CVector<T>& vec)
    {
      for (size_t i = 0; i < vec.size(); i++)
	append(vec[i]);
    }

    CVector<T>& operator=(const CVector<T>& vec)
    {
      delete [] content;
      len = vec.len;
      maxLen = vec.len;
      content = new T[len];
      for (size_t i = 0; i < len; i++)
	content[i] = vec.content[i];
      return *this;
    }

    CVector<T> operator+(const T& elem) const
    {
      CVector<T> newVec(*this);
      newVec.append(elem);
      return newVec;
    }

    CVector<T>& operator+=(const T& elem)
    {
      this->append(elem);
      return *this;
    }

    CVector<T>& operator+=(const CVector<T>& vec)
    {
      this->append(vec);
      return *this;
    }

    CVector<T> operator+(const CVector<T>& vec) const
    {
      CVector<T> newVec(*this);
      newVec.append(vec);
      return newVec;
    }

    void remove(size_t ndx)
    {
      len--;
      for (size_t i = ndx; i < len; i++)
	content[i] = content[i+1];
      content[len] = T();
    }

    T& operator[](size_t ndx)
    {
      return content[ndx];
    }

    const T& operator[](size_t ndx) const
    {
      return content[ndx];
    }

    T& elem(size_t ndx)
    {
      return content[ndx];
    }

    const T& elem(size_t ndx) const
    {
      return content[ndx];
    }

    size_t size() const
    {
      return len;
    }

    bool contains(const T& probe) const
    {
      for (size_t i = 0; i < len; i++)
	if (content[i] == probe) 
	  return true;

      return false;
    }

    void qsort(int (*cmpfn)(const T* mem1, const T* mem2))
    {
      ::qsort(content, len, sizeof(T), 
	      (int (*)(const void *, const void *))cmpfn);
    }

  };

} /* namespace sherpa */
#endif /* LIBSHERPA_CVECTOR_HXX */
