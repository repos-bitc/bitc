#ifndef LIBSHERPA_GCPTR_HXX
#define LIBSHERPA_GCPTR_HXX

/*
 * Copyright (c) 2006, The EROS Group, LLC. All rights reserved.
 * Copyright (c) 2004, The EROS Group, LLC and Johns Hopkins
 * University. All rights reserved.
 * 
 * This software was developed to support the EROS secure operating
 * system project (http://www.eros-os.org). The latest version of
 * the OpenCM software can be found at http://www.opencm.org.
 * 
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials
 *    provided with the distribution.
 * 
 * 3. Neither the name of the The EROS Group, LLC nor the name of
 *    Johns Hopkins University, nor the names of its contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <typeinfo>
#include <assert.h>
#include <stdint.h>

namespace sherpa {
  /// The original implementation of Countable just used an integer, but
  /// this created problems for constant pointers. The issue is that the
  /// refcount field of a const Countable cannot be incremented and
  /// decremented (because it is const). I introduced the GCRefCounter
  /// class to take advantage of a failure in the C++ type system: const
  /// objects can legally contain pointers to non-const objects. This
  /// allows us to say "const Serializable *" and still be able to
  /// increment/decrement the reference count by indirecting to the
  /// mutable GCRefCounter object that sits in the heap.
  struct GCRefCounter {
    unsigned refCount;
    GCRefCounter()
    {
      refCount = 0;
    }
  };

  /// @brief Common base class used by the reference counting pointer
  /// implementation (GCPtr).
  ///
  /// It may seem surprising that the IncrementRefCount() and
  /// DecrementRefCount() methods are implemented here. This is because
  /// of a bug in g++ wherein it is impossible for Counter to declare:
  ///
  /// @verbatim
  ///   template<class T> friend class GCPtr;
  /// @endverbatim
  class Countable {
    GCRefCounter *theCounter;

  public:
    /// @brief Constructor for a countable object.
    ///
    /// This is mildly tricky. The problem is that for statically
    /// allocated objects we need to arrange for the reference count to
    /// be incremented by 1 extra to guarantee that the expiration of
    /// any GcPtr that points to it does not cause an attempt to
    /// deallocate a statically declared object. The following test
    /// assumes that the stack grows downwards.
  
    Countable()
      : theCounter(new GCRefCounter)
    {
    }

    Countable(const Countable&)
      : theCounter(new GCRefCounter)
    {
    }
    virtual ~Countable();

    inline Countable& operator=(const Countable&)
    {
      /* Do NOT change the target refcount! */
      return *this;
    }

    static void TraceDangles(bool on);

#ifdef DOXYGEN
  private:
#endif
    /// @deprecated This method would be private but for what appears to
    /// be a g++ bug.
    inline void IncrementRefCount(void) const
    {
      if (this)
	theCounter->refCount++;
    }
    /// @deprecated This method would be private but for what appears to
    /// be a g++ bug.
    void DecrementRefCount(void) const;

    bool ValidRefCount(void) const {
      return theCounter->refCount ? true : false;
    }
#ifdef DOXYGEN
  public:
#endif
  };

  // Forward declaration:
  template<class T> class NoGCPtr;

  /// @brief Reference counting pointer implementation. See also Countable.
  ///
  /// GCPtr is a reference-counting pointer implementation. It can only
  /// be used to point to objects derived from Countable. 
  template<class T> 
  class GCPtr {
  public:
    struct BoolConversionSupport {
      int dummy;
    };

  private:
    T *pObject;
  public:
    inline GCPtr()
    {
      pObject = 0;
    }

    inline GCPtr(const GCPtr& other)
    {
      pObject = other.pObject;
      pObject->IncrementRefCount();
    }

    /// @brief Copy constructor for GCPtrs to derived classes.
    template<typename T2>
    inline GCPtr(const GCPtr<T2> & other)
    {
      pObject = other ? &*other : 0;
      pObject->IncrementRefCount();
    }

    // For use in construction from raw pointers:
    inline GCPtr(T* obPtr)
    {
      pObject = obPtr;
      pObject->IncrementRefCount();
    }
    inline ~GCPtr()
    {
      pObject->DecrementRefCount();
    }

    /// @brief Default assignment operator.
    ///
    /// Curiously, this doesn't conflict with the templated version
    /// below, which I do not understand.
    inline GCPtr& operator=(const GCPtr& p)
    {
      T* pOtherObject = p ? &*p : 0;

      /* Careful: order of increment/decrement matters! */
      pOtherObject->IncrementRefCount();
      pObject->DecrementRefCount();
      pObject = pOtherObject;

      return *this;
    }

    /// @brief Pointer coersion assignment operator.
    ///
    /// This one gets triggered when you do an assignment of a derived
    /// class GCPtr to a base class GCPtr. Curiously, it doesn't seem to
    /// conflict with the default assignment operator above.
    template<typename T2>
    inline GCPtr& operator=(const GCPtr<T2>& p)
    {
      T2* pOtherObject = p ? &*p : 0;

      /* Careful: order of increment/decrement matters! */
      pOtherObject->IncrementRefCount();
      pObject->DecrementRefCount();
      pObject = pOtherObject;

      return *this;
    }

    /// @brief Assignment from suitable raw pointer.
    template<typename T2>
    inline GCPtr& operator=(T2* p)
    {
      /* Careful: order of increment/decrement matters! */
      ((Countable *)p)->IncrementRefCount();
      pObject->DecrementRefCount();
      pObject = p;

      return *this;
    }

    template<typename T2>
    inline GCPtr<T2> upcast()
    {
      T2* pUpcastedPtr = dynamic_cast<T2 *>(pObject);
      if (pUpcastedPtr == 0)
	throw std::bad_cast();

      return GCPtr<T2>(pUpcastedPtr);
    }

    inline T* operator -> () const
    {
      assert(pObject->ValidRefCount());
      return pObject;
    }

    inline T& operator * () const
    {
      assert(pObject->ValidRefCount());
      return *pObject;
    }

    // This doesn't really belong in GCPtr, but it is necessary
    // if we want to implement std::set over them...
    inline bool operator<(const GCPtr& other) const
    {
      return ((uintptr_t)pObject) < ((uintptr_t)other.pObject);
    }

    inline bool operator==(const GCPtr& other) const
    {
      return pObject == other.pObject;
    }

    inline bool operator!=(const GCPtr& other) const
    {
      return pObject != other.pObject;
    }

    /// Sleazy support for if(GCPtr).
    inline operator int BoolConversionSupport::* () const
    {
      return pObject ? &BoolConversionSupport::dummy : 0;
    }

    unsigned long value()
    {
      return (unsigned long) pObject;
    }
  };

  /// @brief Non-counting pointer implementation compatible with
  /// GCPtr. See also Countable.
  ///
  /// This variant should be used for upward-pointing pointers that
  /// should not be counted.
  template<class T> 
  class NoGCPtr {
  public:
    struct BoolConversionSupport {
      int dummy;
    };

  private:
    T *pObject;
  public:
    inline NoGCPtr()
    {
      pObject = 0;
    }

    inline NoGCPtr(const NoGCPtr& other)
    {
      pObject = other ? &*other : 0;
    }

    inline NoGCPtr(const GCPtr<T>& other)
    {
      pObject = other ? &*other : 0;
    }

    /// @brief Copy constructor for NoGCPtrs to derived classes.
    template<typename T2>
    inline NoGCPtr(const NoGCPtr<T2> & other)
    {
      pObject = other ? &*other : 0;
    }

    template<typename T2>
    inline NoGCPtr(const GCPtr<T2> & other)
    {
      pObject = other ? &*other : 0;
    }

    // For use in construction from raw pointers:
    inline NoGCPtr(T* obPtr)
    {
      pObject = obPtr;
      pObject->IncrementRefCount();
    }
    inline ~NoGCPtr()
    {
    }

    /// @brief Default assignment operator.
    ///
    /// Curiously, this doesn't conflict with the templated version
    /// below, which I do not understand.
    inline NoGCPtr& operator=(const NoGCPtr& p)
    {
      T* pOtherObject = (p ? &*p : 0);

      /* Careful: order of increment/decrement matters! */
      pObject = pOtherObject;

      return *this;
    }

    inline NoGCPtr& operator=(const GCPtr<T>& p)
    {
      T* pOtherObject = (p ? &*p : 0);

      /* Careful: order of increment/decrement matters! */
      pObject = pOtherObject;

      return *this;
    }

    /// @brief Pointer coersion assignment operator.
    ///
    /// This one gets triggered when you do an assignment of a derived
    /// class NoGCPtr to a base class NoGCPtr. Curiously, it doesn't seem to
    /// conflict with the default assignment operator above.
    template<typename T2>
    inline NoGCPtr& operator=(const NoGCPtr<T2>& p)
    {
      T2* pOtherObject = p ? &*p : 0;

      /* Careful: order of increment/decrement matters! */
      pObject = pOtherObject;

      return *this;
    }

    template<typename T2>
    inline NoGCPtr& operator=(const GCPtr<T2>& p)
    {
      T2* pOtherObject = p ? &*p : 0;

      /* Careful: order of increment/decrement matters! */
      pObject = pOtherObject;

      return *this;
    }

    /// @brief Assignment from suitable raw pointer.
    template<typename T2>
    inline NoGCPtr& operator=(T2* p)
    {
      /* Careful: order of increment/decrement matters! */
      pObject = p;

      return *this;
    }

    template<typename T2>
    inline NoGCPtr<T2> upcast()
    {
      T2* pUpcastedPtr = dynamic_cast<T2 *>(pObject);
      if (pUpcastedPtr == 0)
	throw std::bad_cast();

      return NoGCPtr<T2>(pUpcastedPtr);
    }

    inline T* operator -> () const
    {
      assert(pObject->ValidRefCount());
      return pObject;
    }

    inline T& operator * () const
    {
      assert(pObject->ValidRefCount());
      return *pObject;
    }

    inline bool operator==(const NoGCPtr& other)
    {
      return pObject == other.pObject;
    }

    inline bool operator==(const GCPtr<T>& other)
    {
      return pObject == other.pObject;
    }

    inline bool operator!=(const NoGCPtr& other)
    {
      return pObject != other.pObject;
    }

    inline bool operator!=(const GCPtr<T>& other)
    {
      return pObject != other.pObject;
    }

    /// Sleazy support for if(NoGCPtr).
    inline operator int BoolConversionSupport::* () const
    {
      return pObject ? &BoolConversionSupport::dummy : 0;
    }

    /// @brief Conversion to GCPtr
    ///
    /// This should be enough to cover most comparisons, assignments,
    /// and initializers.
    inline operator GCPtr<T> () const
    {
      return GCPtr<T>(pObject);
    }

    unsigned long value()
    {
      return (unsigned long) pObject;
    }
  };
} /* namespace sherpa */
// Local Variables:
// mode:c++
// End:

#endif /* LIBSHERPA_GCPTR_HXX */
