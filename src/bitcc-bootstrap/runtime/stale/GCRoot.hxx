#ifndef GCPTR_HXX
#define GCPTR_HXX

/**************************************************************************
 *
 * Copyright (C) 2005, Johns Hopkins University.
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

class void_GCRoot {
  friend void majorGC(void);
  static void_GCRoot TrueRoot;

protected:
  inline void InitLink()
  {
    next = TrueRoot.next;
    prev = &TrueRoot;
    TrueRoot.next->prev = this;
    TrueRoot.next = this;
    prev->next = this;
  }

  void_GCRoot *next;
  void_GCRoot *prev;
  void *ptr;

  void_GCRoot()
  {
    next = prev = this;

    ptr = 0;
  }

  void_GCRoot(const void_GCRoot& that)
  {
    InitLink();

    ptr = that.ptr;
  }

  void_GCRoot& operator =(const void_GCRoot& that)
  {
    ptr = that.ptr;
    return *this;
  }
};

template<typename T>
struct GCRoot : public void_GCRoot {
  struct BoolConversionSupport {
    int dummy;
  };

  GCRoot()
  {
    InitLink();
  }

  /// Sleazy support for if(GCRoot).
  inline operator int BoolConversionSupport::* () const
  {
    return ptr ? &BoolConversionSupport::dummy : 0;
  }

  inline T* operator -> () const
  {
    return (T *) ptr;
  }

  inline T& operator * () const
  {
    return *(T *)ptr;
  }

  inline bool operator==(const GCRoot<T>& other)
  {
    return ptr == other.ptr;
  }

  inline bool operator!=(const GCRoot<T>& other)
  {
    return ptr != other.ptr;
  }
};
#endif /* GCPTR_HXX */
