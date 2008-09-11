#ifndef LIBSHERPA_ENUMSET_HXX
#define LIBSHERPA_ENUMSET_HXX

/**************************************************************************
 *
 * Copyright (C) 2008, The EROS Group, LLC. 
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

#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_enum.hpp>

namespace sherpa {

  /// @brief Zero-overhead wrapper class for type-preserving bit
  /// sets.
  ///
  /// This is a simply horrible hack. It started as a workaround for
  /// the fact that enums do not provide type-preserving bit sets,
  /// because (a|b) yields <tt>int</tt> rather than the underlying
  /// enumeration type. What we do here is to define EnumSet over an
  /// underlying arithmetic type. If you instantiate it over an enum,
  /// it will only permit initializers over that enum, which will have
  /// the desired effect that the enumerands can serve as bit labels
  /// for purposes of documentation.
  ///
  /// That isn't hard to fix, except that you can run out of
  /// enumeration values -- it is surprising how many bit sets turn out
  /// to require 33 bits on a 32-bit machine.
  ///
  /// DO NOT attempt to instantiate one of these over an underlying
  /// integral type. It will produce an error in the template expander
  /// because of the non-member overloading of the binary operators on
  /// the underlying types!  I need to do some template magic to make
  /// that overloading more selective, and I haven't had time yet.

  template <class T>
  class EnumSet {

    // Ensure that this class will only instantiate over enumeration
    // types.
    //
    // If this is being instantiated over an enumeration type, the
    // following enable_if_c does the right thing by selecting the
    // underlying unsigned type.
    //
    // If this is being instantiated over a non-enumeral type, then we
    // need a construct that will select the repr type
    // conditionally. There are template metaprogramming techniques
    // for that, but I haven't had a chance to brush up on them. In
    // non-enumeration cases, it is imperative to disable the
    // top-level overloads on const T& at botom, which I am already
    // doing. See comments there.
    typename boost::enable_if_c<boost::is_enum<T>::value, unsigned long>::type repr;

    struct BoolConversionSupport {
      int dummy;
    };

  public:
    EnumSet()
      :repr((T)0)
    {
    }

    EnumSet(const EnumSet<T>& that)
    {
      repr = that.repr;
    }

    // Constructor from underlying repr type
    EnumSet(const T& that)
    {
      repr = that;
    }

    // Comparison operators
    bool
    operator==(const EnumSet<T>& that) const
    {
      return (repr == that.repr);
    }

    bool
    operator!=(const EnumSet<T>& that) const
    {
      return (repr != that.repr);
    }

    // Assignment operators
    EnumSet<T>&
    operator=(const EnumSet<T>& that)
    {
      repr = that.repr;
      return *this;
    }

    EnumSet<T>&
    operator |=(const EnumSet<T>& that)
    {
      repr |= that.repr;
      return *this;
    }

    EnumSet<T>&
    operator &=(const EnumSet<T>& that)
    {
      repr &= that.repr;
      return *this;
    }

    EnumSet<T>&
    operator ^=(const EnumSet<T>& that)
    {
      repr ^= that.repr;
      return *this;
    }



    // Set operators over EnumSet:
    EnumSet
    operator |(const EnumSet<T>& that) const
    {
      EnumSet b(*this);
      b |= that;
      return b;
    }

    EnumSet
    operator &(const EnumSet<T>& that) const
    {
      EnumSet b(*this);
      b &= that;
      return b;
    }

    EnumSet
    operator ^(const EnumSet<T>& that) const
    {
      EnumSet b(*this);
      b ^= that;
      return b;
    }

    // Set negation:
    EnumSet
    operator ~() const
    {
      EnumSet b(*this);
      b.repr = ~b.repr;
      return b;
    }

#if 0
    // Set sum:
    EnumSet
    operator +(const EnumSet<T>& that) const
    {
      EnumSet b(b.repr | that.repr);
      return b;
    }

    // Set difference:
    EnumSet
    operator -(const EnumSet<T>& that) const
    {
      EnumSet b(b.repr & ~that.repr);
      return b;
    }
#endif

#if 0
    // Set sum:
    EnumSet<T>&
    operator +=(const EnumSet<T>& that)
    {
      *this = (repr | that.repr);
      return *this;
    }

    // Set difference:
    EnumSet<T>&
    operator -=(const EnumSet<T>& that)
    {
      *this = (repr & ~that.repr);
      return *this;
    }
#endif

    // Sleazy support for if(EnumSet): 
    inline operator int BoolConversionSupport::*() const
    {
      return (repr == 0) ? 0 : &BoolConversionSupport::dummy;
    }
  };


  // Operators when RHS is a EnumSet:
  template <class T>
  inline EnumSet<T> operator | (const T& lhs, const EnumSet<T>& rhs)
  {
    return (rhs | EnumSet<T>(lhs));
  }

  template <class T>
  inline EnumSet<T> operator & (const T& lhs, const EnumSet<T>& rhs)
  {
    return (rhs & EnumSet<T>(lhs));
  }

  template <class T>
  inline EnumSet<T> operator ^ (const T& lhs, const EnumSet<T>& rhs)
  {
    return (rhs ^ EnumSet<T>(lhs));
  }

  // Combining operators when both sides are of underlying enumeral
  // type.
  //
  // These MUST NOT be expanded if the underlying type is some form of
  // integer type, because that will lead to a recursive expansion
  // error (or at least, it will when I fix the use of enable_if_c
  // properly above).
  template <class T>
  inline
  typename boost::enable_if_c<boost::is_enum<T>::value, EnumSet<T> >::type
  operator | (const T& lhs, const T& rhs)
  {
    return (EnumSet<T>(lhs) | EnumSet<T>(rhs)); 
  }

  template <class T>
  inline
  typename boost::enable_if_c<boost::is_enum<T>::value, EnumSet<T> >::type
  operator & (const T& lhs, const T& rhs)
  {
    return (EnumSet<T>(lhs) & EnumSet<T>(rhs)); 
  }

  template <class T>
  inline
  typename boost::enable_if_c<boost::is_enum<T>::value, EnumSet<T> >::type
  operator ^ (const T& lhs, const T& rhs)
  {
    return (EnumSet<T>(lhs) ^ EnumSet<T>(rhs)); 
  }

  // Unary bitwise negate on underlying type:
  template <class T>
  inline
  typename boost::enable_if_c<boost::is_enum<T>::value, EnumSet<T> >::type
  operator ~(const T& lhs)
  {
    return ~EnumSet<T>(lhs);
  }

} /* namespace sherpa */

#endif /* LIBSHERPA_ENUMSET_HXX */
