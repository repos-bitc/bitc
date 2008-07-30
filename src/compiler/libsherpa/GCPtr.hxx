#ifndef LIBSHERPA_GCPTR_HXX
#define LIBSHERPA_GCPTR_HXX

/*
 * Copyright (c) 2008, The EROS Group, LLC. All rights reserved.
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
#include <stddef.h>

#define GCPTR_SUPPORT_RAW

/** @file This is a non-invasive variant of the original sherpa GCPtr
 * that has some debugging features. */
namespace sherpa {
  struct dynamic_cast_tag {};
  struct shared_from_this_tag {};

  struct GCRefCounter {
    const void * pObject;
    size_t nCounted;
    size_t nWeak;

    GCRefCounter(const void *ptr) {
      pObject = ptr;
      nCounted = 0;
      nWeak = 0;
    }

    GCRefCounter(const void *ptr, size_t _nCounted, size_t _nWeak) {
      pObject = ptr;
      nCounted = _nCounted;
      nWeak = _nWeak;
    }

    void incCount() {
      assert(pObject || this == &NullPtrCounter);
      nCounted++;
    }
    void incWeakCount() {
      nWeak++;
    }

    bool decCount() {
      bool needsDelete = false;

      assert(nCounted);

      nCounted--;

      if (nCounted == 0) {
	needsDelete = true;
	pObject = 0;
      }

      assert(nCounted || (pObject == 0));

      // If there are still any references at all, refcount object
      // must remain valid:
      if (nCounted + nWeak)
	return false;

      delete this;
      return needsDelete;
    }

    void decWeakCount() {
      nWeak--;

      assert(nCounted || (pObject == 0));

      // If there are still any references at all, refcount object
      // must remain valid:
      if (nCounted + nWeak)
	return;

      delete this;
    }

    static GCRefCounter NullPtrCounter;
  };

  /// @brief Common base class for counted and weak pointers. 
  ///
  /// This exists primarily so that we can keep GetRefCounter()
  /// private to the implementation.
  class PtrBase {
  protected:
    struct BoolConversionSupport {
      int dummy;
    };

#ifndef GCPTR_SUPPORT_RAW
    static inline GCRefCounter *GetRefCounter(const void *ob) {
      if (ob == NULL)
	return &NullPtrCounter;
      return new GCRefCounter(ob);
    }
    static inline void DeregisterObject(const void *ob) {
    }
#else
    static GCRefCounter *GetRefCounter(const void *ob);
    static void DeregisterObject(const void *ob);
#endif
  };

  // Forward declaration:
  template<class T> class NoGCPtr;

  template<class T>
  struct GCPtr : public PtrBase {
    T *pObject;
    GCRefCounter *rc;

    // As if assigned from NULL
    GCPtr() {
      pObject = 0;
      rc = &GCRefCounter::NullPtrCounter;
      assert(rc);
      rc->incCount();
    }

    inline GCPtr(const GCPtr& other)
    {
      pObject = other.pObject;
      rc = other.rc;
      assert(rc);
      rc->incCount();
    }

    // This one works for downcast, but not for upcast. Technically it
    // would subsume the one above, but the X(const X&) constructor has
    // special meaning in C++.
    template<typename T2>
    inline GCPtr(const GCPtr<T2>& other)
    {
      pObject = other.pObject;
      rc = other.rc;
      assert(rc);
      rc->incCount();
    }

    /// @brief Copy constructor using dynamic_cast.
    template<typename T2>
    inline GCPtr(const GCPtr<T2> & other, dynamic_cast_tag) {
      pObject = dynamic_cast<T2 *>(other.pObject);
      // If dynamic_cast fails it will return NULL, in which case this
      // acts like assignment from a NULL GCPtr
      rc = pObject ? other.rc : &GCRefCounter::NullPtrCounter;
      assert(rc);
      rc->incCount();
    }

    // For use in construction from raw pointers:
    inline GCPtr(T* obPtr)
    {
      pObject = obPtr;
      rc = GetRefCounter(pObject);

      assert(rc);
      rc->incCount();
    }

    inline ~GCPtr()
    {
      assert(rc);
      if (rc->decCount()) {
	DeregisterObject(pObject);
	delete pObject;
      }
    }

    /// @brief Default assignment operator.
    inline GCPtr& operator=(const GCPtr& p) {
      assert(p.rc);
      assert(rc);
      p.rc->incCount();
      rc->decCount();

      pObject = p.pObject;
      rc = p.rc;

      return *this;
    }

    /// @brief Default assignment operator.
    template<typename T2>
    inline GCPtr& operator=(const GCPtr& p)
    {
      assert(p.rc);
      assert(rc);
      p.rc->incCount();
      rc->decCount();

      pObject = p.pObject;
      rc = p.rc;

      return *this;
    }

    /// @brief Assignment from raw pointer.
    template<typename T2>
    inline GCPtr& operator=(T2 *ptr)
    {
      pObject = ptr;

      rc = GetRefCounter(pObject);
      assert(rc);

      rc->incCount();

      return *this;
    }

#if 0
    inline T* operator -> ()
    {
      assert(rc->nCounted);
      return pObject;
    }

    inline T& operator * ()
    {
      assert(rc->nCounted);
      return *pObject;
    }
#endif

    inline T* operator -> () const
    {
      assert(rc->nCounted);
      return pObject;
    }

    inline T& operator * () const
    {
      assert(rc->nCounted);
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
  };

  template<class T>
  struct NoGCPtr : public PtrBase {
    T *pObject;
    GCRefCounter *rc;

    // As if assigned from NULL
    NoGCPtr() {
      pObject = 0;
      rc = &GCRefCounter::NullPtrCounter;
      assert(rc);
      rc->incWeakCount();
    }

    inline NoGCPtr(const NoGCPtr& other)
    {
      pObject = other.pObject;
      rc = other.rc;
      assert(rc);
      rc->incWeakCount();
    }

    inline NoGCPtr(const GCPtr<T>& other)
    {
      pObject = other.pObject;
      rc = other.rc;
      assert(rc);
      rc->incWeakCount();
    }

    // This one works for downcast, but not for upcast. Technically it
    // would subsume the one above, but the X(const X&) constructor has
    // special meaning in C++.
    template<typename T2>
    inline NoGCPtr(const NoGCPtr& other)
    {
      pObject = other.pObject;
      rc = other.rc;
      assert(rc);
      rc->incWeakCount();
    }

    // This one works for downcast, but not for upcast. Technically it
    // would subsume the one above, but the X(const X&) constructor has
    // special meaning in C++.
    template<typename T2>
    inline NoGCPtr(const GCPtr<T2>& other)
    {
      pObject = other.pObject;
      rc = other.rc;
      assert(rc);
      rc->incWeakCount();
    }

    /// @brief Copy constructor using dynamic_cast.
    template<typename T2>
    inline NoGCPtr(const NoGCPtr<T2> & other, dynamic_cast_tag) {
      pObject = dynamic_cast<T2 *>(other.pObject);
      // If dynamic_cast fails it will return NULL, in which case this
      // acts like assignment from a NULL NoGCPtr
      rc = pObject ? other.rc : &GCRefCounter::NullPtrCounter;
      assert(rc);
      rc->incWeakCount();
    }

    /// @brief Copy constructor using dynamic_cast.
    inline NoGCPtr(T *ptr, shared_from_this_tag) {
      pObject = ptr;

      rc = GetRefCounter(pObject);
      assert(rc);
      rc->incWeakCount();
    }

    inline ~NoGCPtr()
    {
      rc->decWeakCount();
    }

    /// @brief Default assignment operator.
    inline NoGCPtr& operator=(const NoGCPtr& p) {
      assert(p.rc);
      assert(rc);
      p.rc->incWeakCount();
      rc->decWeakCount();

      pObject = p.pObject;
      rc = p.rc;

      return *this;
    }

    /// @brief Default assignment operator.
    template<typename T2>
    inline NoGCPtr& operator=(const NoGCPtr& p)
    {
      assert(p.rc);
      assert(rc);

      p.rc->incWeakCount();
      rc->decWeakCount();

      pObject = p.pObject;
      rc = p.rc;

      return *this;
    }

    /// @brief Default assignment operator.
    template<typename T2>
    inline NoGCPtr& operator=(const GCPtr<T2>& p)
    {
      assert(p.rc);
      assert(rc);

      p.rc->incWeakCount();
      rc->decWeakCount();

      pObject = p.pObject;
      rc = p.rc;

      return *this;
    }

    inline T* operator -> () const
    {
      assert(pObject->nCounted());
      return pObject;
    }

    inline T& operator * () const
    {
      assert(pObject->nCounted);
      return *pObject;
    }

    // This doesn't really belong in NoGCPtr, but it is necessary
    // if we want to implement std::set over them...
    inline bool operator<(const NoGCPtr& other) const
    {
      return ((uintptr_t)pObject) < ((uintptr_t)other.pObject);
    }

    inline bool operator==(const NoGCPtr& other) const
    {
      return pObject == other.pObject;
    }

    inline bool operator!=(const NoGCPtr& other) const
    {
      return pObject != other.pObject;
    }

    /// Sleazy support for if(NoGCPtr).
    inline operator int BoolConversionSupport::* () const
    {
      return pObject ? &BoolConversionSupport::dummy : 0;
    }
  };

  template <typename T>
  class enable_shared_from_this {
    NoGCPtr<T> gc_this;

  public:
    NoGCPtr<T> shared_from_this() {
      return gc_this;
    }

    enable_shared_from_this() 
      :gc_this((T *)this, shared_from_this_tag()) {
    }
  };
} /* namespace sherpa */
// Local Variables:
// mode:c++
// End:

#endif /* LIBSHERPA_GCPTR_HXX */
