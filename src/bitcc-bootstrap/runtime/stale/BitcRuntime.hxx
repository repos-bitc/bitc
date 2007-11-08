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

#include <stdint.h>
#include <string.h>
#include "GC.hxx"

#define GCSCALAR(ty) \
  inline void gcMark(ty& ob) {}

// Use the standard names from stdint.h to instantiate the scalar GC
// functions that simply do nothing. We really do not want to call
// these at all, but they allow us to blindly recurse in the BitcPair,
// BitcVector, and BitcArray templates below without worrying about the
// instantiation type.

GCSCALAR(int8_t);
GCSCALAR(int16_t);
GCSCALAR(int32_t);
GCSCALAR(int64_t);

GCSCALAR(uint8_t);
GCSCALAR(uint16_t);
GCSCALAR(uint32_t);
GCSCALAR(uint64_t);

GCSCALAR(bool);
GCSCALAR(float);
GCSCALAR(double);
GCSCALAR(long double);

// Set up names that the compiler can use without fear of collision:
typedef uint32_t    BitcChar_t;

typedef int8_t      BitcInt8_t;
typedef int16_t     BitcInt16_t;
typedef int32_t     BitcInt32_t;
typedef int64_t     BitcInt64_t;

typedef uint8_t     BitcUns8_t;
typedef uint16_t    BitcUns16_t;
typedef uint32_t    BitcUns32_t;
typedef uint64_t    BitcUns64_t;

// This is machine dependent.
typedef uint32_t    BitcWord_t;

typedef float       BitcFloat_t;
typedef double      BitcDouble_t;

// FIX: Following is WRONG
typedef long double BitcQuad_t;

// We handle exceptions with a common base class. 
// Sleazy, but effective.
struct BitcException {
  BitcWord_t *tag;

  BitcException(BitcWord_t *_tag)
  {
    tag = _tag;
  }
};

// Counter-intuitive though this is, it is NOT a good idea for the
// primitive Bitc structure type representations to have constructors.
// The problem is that we need to be able to create them inside
// unions, and C++ does not permit structures having constructors to
// appear inside unions.  Note that the runtime is going to allocate
// zero'd memory in any case, and the type checker has 
//
// Note that the ref /ptr/ field may be null even when the
// corresponding application reference type is not nullable. This is
// necessary to avoid false GC capture.

// WARNING: The REF class should ONLY be used inside objects in the
// GC'd heap, NOT on the C stack!
template<typename T>
struct BitcRef {
  T* ptr;

  struct BoolConversionSupport {
    int dummy;
  };

  /// Sleazy support for if(BitcRef<T>).
  inline operator int BoolConversionSupport::* () const
  {
    return ptr ? &BoolConversionSupport::dummy : 0;
  }

  inline T& operator*() { return *ptr; };
  inline T* operator->() { return ptr; };

#if 0
  inline BitcRef() { ptr = 0; }
  inline BitcRef(const BitcRef& that) { ptr = that.ptr; }

  inline BitcRef& operator=(const BitcRef& that) { ptr = that.ptr; return *this; }
#endif

  inline BitcRef& operator=(T* that) { ptr = that; return *this; }

  inline bool operator==(const BitcRef& other)
  {
    return ptr == other.ptr;
  }

  inline bool operator!=(const BitcRef& other)
  {
    return ptr != other.ptr;
  }
};

template<typename T>
inline void gcMark(BitcRef<T>& ob) 
{ 
  if (ob.ptr) {
    mark(ob.ptr); gcMark(*ob.ptr); 
  }
}

// The /body/ must accept a BitcRef<Tc> as its first argument.
template<typename Tc, typename Tp>
struct BitcProc {
  BitcRef<Tc> closure;
  Tp          body;
};

template<typename Tc, typename Tp>
inline void gcMark(BitcProc<Tc, Tp>& ob) { 
  gcMark(ob.closure); 
}


template<typename T1, typename T2>
struct BitcPair {
  T1 fst;
  T2 snd;

  BitcPair(const T1& _fst, const T1& _snd) { fst = _fst; snd = _snd; }
};

// It would surely be nice if this inlined aggressively enough. Regrettably,
// I fear that it won't do so in practice.
template<typename T1, typename T2>
inline void gcMark(BitcPair<T1,T2>& ob) { gcMark(ob.fst); gcMark(ob.snd); }

template<typename T>
struct BitcVector {
  BitcWord_t length;
  T elements[0];
};

// Likewise...
template<typename T>
inline void gcMark(BitcVector<T> *& ob) { 
  // mark(ob->elements);

  for (BitcWord_t i = 0; i < ob->length; i++)
    gcMark(ob->elements[i]);
}

template<typename T, const BitcWord_t len>
struct BitcArray {
  T elements[len];
};

// Likewise...
template<typename T, const BitcWord_t len>
inline void gcMark(BitcArray<T,len>& ob) { 
  for (BitcWord_t i = 0; i < len; i++)
    gcMark(ob.elements[i]);
}

// Note that s[] should not really be a BitcChar_t here -- the intent
// is to do unicode encoding.
struct BitcString_t {
  BitcWord_t length;
  BitcChar_t s[0];			// unicode UTF-8!
};

inline void gcMark(BitcString_t *& ob) { 
  // mark(s);
}
