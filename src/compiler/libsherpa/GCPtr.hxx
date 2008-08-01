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
#include <libsherpa/UExcept.hxx>

#define GCPTR_SUPPORT_RAW
//#define GCPTR_DEBUG
#define GCPTR_EXPLICIT

/** @file This is a non-invasive variant of the original sherpa GCPtr
 * that has some debugging features. */
namespace sherpa {
  template <class T>
  inline void checked_delete(T *x) {
    typedef char type_must_be_complete[sizeof(T)?1:-1];
    (void) sizeof(type_must_be_complete);
    delete x;
  }

  struct GC_Null_tag {};
  extern GC_Null_tag GC_NULL;

  struct dynamic_cast_tag {};
  struct shared_from_this_tag {};

  struct GCRefCounter {
#ifndef GCPTR_SUPPORT_RAW
    static inline GCRefCounter *Find(const void *ob) {
      if (ob == NULL)
	return &NullPtrCounter;
      return 0;
    }
    inline void Register() {
    }
    inline void Deregister() {
    }
#else
    static GCRefCounter *Find(const void *ob);
    void Register();
    void Deregister();
#endif

    const void *pObject;
    size_t nCounted;
    size_t nWeak;

    GCRefCounter(const void *ob) {
      pObject = ob;
      nCounted = 0;
      nWeak = 0;

#ifndef GCPTR_DEBUG
      Register();
#endif
    }

    GCRefCounter(const void *ob, size_t _nCounted, size_t _nWeak) {
      pObject = ob;
      nCounted = _nCounted;
      nWeak = _nWeak;

      Register();
    }

    virtual ~GCRefCounter() {
    }

    void incCount() {
      assert(pObject || this == &NullPtrCounter);
      nCounted++;
    }

    void incWeakCount() {
      nWeak++;
    }

    // This really ought to be abstract, but I need it to be concrete
    // so that NullPtrCounter can be constructed
    virtual void destroy() {}

    virtual size_t obSize() { return 0; }

    void decCount() {
      assert(nCounted);

      // Order of operation matters here, because the target object
      // may have a weak pointer to itself, and we do not want to
      // trigger recursive self-deletion of the GCRefCounter.

      if (nCounted == 1) {
	Deregister();
	destroy();
      }

      nCounted--;

      assert(nCounted || (pObject == 0));

      // If there are still any references at all, refcount object
      // must remain valid:
      if (nCounted + nWeak)
	return;

      assert(this != &NullPtrCounter);

      delete this;
    }

    void decWeakCount() {
      nWeak--;

      assert(nCounted || (pObject == 0));

      // If there are still any references at all, refcount object
      // must remain valid:
      if (nCounted + nWeak)
	return;

      assert(this != &NullPtrCounter);

      delete this;
    }

    static GCRefCounter NullPtrCounter;
  };

  template<class T>
  struct T_GCRefCounter : public GCRefCounter {
    T_GCRefCounter(T *pObject) 
      : GCRefCounter(pObject)
    {
#ifdef GCPTR_DEBUG
      Register();
#endif
    }

    size_t obSize() {
      return sizeof(T);
    }

    void destroy() {
      // Order of operations is important here. If we do not zero
      // pObject before calling checked_delete(), then an object
      // derived from enable_shared_from_this will assert when
      // decWeakCount() is called.
      T *tmp = (T *)pObject;
      pObject = 0;
      checked_delete (tmp);
    }
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
  };

  // Forward declaration:
  template<class T> class NoGCPtr;

  /// @brief Reference counting, non-invasive pointer implementation.
  ///
  /// There is a pernicious problem in the assignment operators. It
  /// has bitten me multiple times, so I want to document it
  /// here. Because operator= accepts its argument by reference, it
  /// is possible to see an assignment of the form x=x.y where y is a
  /// member of x. In this case, the reference passed to the
  /// assignment operator is an internal pointer to a field of x. If x
  /// has no other outstanding references at the time of this
  /// assignment, it will be destroyed by the assignment. If the
  /// necessary fields of x.y have not been copied to a temporary
  /// first, the results are not what you wanted.
  ///
  /// To make matters worse, this problem is masked by most versions
  /// of malloc(), because the stale data will still be out there
  /// (because no reallocation has yet occurred). In my case, it was
  /// revealed by the Boehm collector, because that collector eagerly
  /// zeroes storage on reclamation. The current GCPtr implementation
  /// now makes the requisite temporary copies.
  ///
  /// Finally, note that the GCPtr which was x.y will be getting
  /// destroyed when its parent gets destroyed, so you need to bump
  /// the reference count before taking the temporary.
  ///
  /// These issues do not apply when assigning to weak pointers,
  /// because a weak pointer cannot (by definition) be the last of its
  /// kind, and assignment therefore cannot trigger deletion of the
  /// target. The bug here is not triggered by direct assignment
  /// provided the reference count increment/decrement order is
  /// handled correctly.
  template<class T>
  class GCPtr : public PtrBase {
    template<class T2> friend class GCPtr;
    template<class T2> friend class NoGCPtr;

    T *pObject;
    GCRefCounter *rc;

    GCRefCounter *GetRefCounter(T *ob) {
      GCRefCounter *rc = GCRefCounter::Find(ob);
      if (!rc)
	rc = new T_GCRefCounter<T>(ob);

      return rc;
    }

  public:
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

    GCPtr(GC_Null_tag) {
      pObject = 0;
      rc = &GCRefCounter::NullPtrCounter;
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

    // This one works for downcast, but not for upcast. Technically it
    // would subsume the one above, but the X(const X&) constructor has
    // special meaning in C++.
    template<typename T2>
#ifdef GCPTR_EXPLICIT
    explicit 
#endif
    inline GCPtr(const NoGCPtr<T2>& wp)
    {
      if (wp.valid()) {
	assert(wp.rc);

	rc = wp.rc;
	pObject = wp.pObject;

	assert(rc);
	rc->incCount();
      }
      else {
	THROW(excpt::BadValue, "Expired weak pointer");

	// Weak pointer is deceased. Result is NULL pointer.
	pObject = 0;
	rc = &GCRefCounter::NullPtrCounter;
	assert(rc);
	rc->incCount();
      }
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
#ifdef GCPTR_EXPLICIT
    explicit
#endif
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
      rc->decCount();
    }

    /// @brief Default assignment operator.
    inline GCPtr& operator=(const GCPtr& p) {
      assert(p.rc);
      assert(rc);
      
      p.rc->incCount();

      GCRefCounter *tmpRC = p.rc;
      T *tmpObject = p.pObject;

      rc->decCount();

      pObject = tmpObject;
      rc = tmpRC;

      return *this;
    }

    /// @brief General assignment operator (including subclass to
    /// superclass conversion)
    template<typename T2>
    inline GCPtr& operator=(const GCPtr<T2>& p)
    {
      assert(p.rc);
      assert(rc);
      p.rc->incCount();

      GCRefCounter *tmpRC = p.rc;
      T *tmpObject = p.pObject;

      rc->decCount();

      pObject = tmpObject;
      rc = tmpRC;

      return *this;
    }

    /// @brief Assignment from weak pointer (including subclass to
    /// superclass conversion)
    template<typename T2>
    inline GCPtr& operator=(const NoGCPtr<T2>& wp)
    {
      if (wp.valid()) {
	assert(wp.rc);
	assert(rc);
	wp.rc->incCount();

	GCRefCounter *tmpRC = wp.rc;
	T *tmpObject = wp.pObject;

	rc->decCount();

	pObject = tmpObject;
	rc = tmpRC;
      }
      else {
	// Weak pointer is deceased. Result is NULL pointer.
	pObject = 0;
	rc = &GCRefCounter::NullPtrCounter;
	assert(rc);
	rc->incCount();
      }

      return *this;
    }

#ifndef GCPTR_EXPLICIT
    /// @brief Assignment from raw pointer.
    inline GCPtr& operator=(T *ptr)
    {
      pObject = ptr;

      rc = GetRefCounter(pObject);
      assert(rc);

      rc->incCount();

      return *this;
    }
#endif

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
  class NoGCPtr : public PtrBase {
    template<class T2> friend class GCPtr;

    T *pObject;
    GCRefCounter *rc;

    GCRefCounter *GetRefCounter(T *ob) {
      GCRefCounter *rc = GCRefCounter::Find(ob);
      if (!rc)
	rc = new T_GCRefCounter<T>(ob);

      return rc;
    }

  public:
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

    //inline NoGCPtr(const GCPtr<T>& other)
    //{
    //pObject = other.pObject;
    // rc = other.rc;
    //assert(rc);
    //    rc->incWeakCount();
    // }

    // This one works for downcast, but not for upcast. Technically it
    // would subsume the one above, but the X(const X&) constructor has
    // special meaning in C++.
    template<typename T2>
    inline NoGCPtr(const NoGCPtr<T2>& other)
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

    bool valid() const {
      assert(rc);
      return (rc->nCounted != 0);
    }
#if 0
    // Being of uncertain contents, weak pointers are neither
    // orderable nor comparable.
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
#endif
  };

  template <typename T>
  class enable_shared_from_this {
    NoGCPtr<T> gc_this;

  public:
    GCPtr<T> shared_from_this() {
      GCPtr<T> p(gc_this);
      return p;
    }

    GCPtr<const T> shared_from_this() const {
      GCPtr<const T> p(gc_this);
      return p;
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
