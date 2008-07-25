#ifndef LIBSHERPA_BIGNUM_HXX
#define LIBSHERPA_BIGNUM_HXX

/**************************************************************************
 *
 * Copyright (C) 2007, The EROS Group, LLC. 
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
#include <stdlib.h>
#include <vector>
#include <iostream>

namespace sherpa {

  class BigNum {
    friend struct nvec;

    bool negative;
    size_t nDigits;

    union {
      uint32_t oneDigit;	// when it fits
      uint32_t *digits;		// dynamically allocated
    };
    
    /// @brief Return values are always normalized so that if nDigits
    /// is 1 the value resides in the oneDigit field.
    //    void normalize();

    uint64_t getDigit(size_t i) const
    {
      if (i >= nDigits)
	return 0;
      return (nDigits == 1) ? oneDigit :  digits[i];
    }

    uint32_t& theDigit(size_t i)
    {
      if (nDigits == 1)
	return oneDigit;
      return digits[i];
    }

    // Used internally.
    inline BigNum(size_t nDigits, uint32_t *digits, bool negative = false);

    BigNum rshift_digits(size_t nDigits) const;
    BigNum lshift_digits(size_t nDigits) const;
  public:
    ~BigNum();
    BigNum()
    {
      negative = false;
      nDigits = 1;
      oneDigit = 0;
    }

    inline BigNum(uint32_t u, bool neg)
    {
      negative = neg;
      nDigits = 1;
      oneDigit = u;
    }

    BigNum(uint64_t u, bool neg);

    // Ideally, we would just have a pair of constructors above with
    // an OPTIONAL boolean negation. Unfortunately, there are
    // compilers that will not resolve the overload correctly given
    //
    //   BigNum bn = (size_t) i;
    //
    // The following constructors exist so that this case will have an
    // exact match:
    
    inline BigNum(uint32_t u) {
      negative = false;
      nDigits = 1;
      oneDigit = u;
    }
    BigNum(uint64_t u);

    inline BigNum(int32_t i)
    {
      negative = (i < 0);
      nDigits = 1;
      oneDigit = (i < 0) ? -i : i;
    }

    BigNum(const std::string& s, size_t radix = 0);

    BigNum(const BigNum&);
    
    BigNum operator+(const BigNum&) const;
    BigNum operator-(const BigNum&) const;
    BigNum operator*(const BigNum&) const;
    BigNum operator/(const BigNum&) const;
    BigNum operator%(const BigNum&) const;

    inline BigNum& operator+=(const BigNum& other)
    {
      BigNum tmp = *this + other;
      *this = tmp;
      return *this;
    }
    inline BigNum& operator-=(const BigNum& other)
    {
      BigNum tmp = *this - other;
      *this = tmp;
      return *this;
    }
    inline BigNum& operator*=(const BigNum& other)
    {
      BigNum tmp = *this * other;
      *this = tmp;
      return *this;
    }
    inline BigNum& operator/=(const BigNum& other)
    {
      BigNum tmp = *this / other;
      *this = tmp;
      return *this;
    }
    inline BigNum& operator%=(const BigNum& other)
    {
      BigNum tmp = *this % other;
      *this = tmp;
      return *this;
    }

    BigNum& operator=(const BigNum&);

    BigNum abs() const;
    BigNum neg() const;

    inline BigNum operator-() const	// unary minus
    {
      return this->neg();
    }

    int cmp(const BigNum& other) const;

    inline bool operator<(const BigNum& other) const
    {
      return cmp(other) < 0;
    }

    inline bool operator<=(const BigNum& other) const
    {
      return cmp(other) <= 0;
    }
    inline bool operator>(const BigNum& other) const
    {
      return cmp(other) > 0;
    }

    inline bool operator>=(const BigNum& other) const
    {
      return cmp(other) >= 0;
    }

    inline bool operator==(const BigNum& other) const
    {
      return cmp(other) == 0;
    }

    inline bool operator!=(const BigNum& other) const
    {
      return cmp(other) != 0;
    }

    BigNum operator<<(size_t n);
    BigNum operator>>(size_t n);

    inline BigNum& operator<<=(size_t n)
    {
      *this = *this << n;
      return *this;
    }
    inline BigNum& operator>>=(size_t n)
    {
      *this = *this >> n;
      return *this;
    }

    std::string asString(size_t radix = 10) const;

    void toStream(std::ostream& strm, size_t radix = 10) const;
    void fromStream(std::istream& strm, size_t radix = 10);

    inline uint32_t as_uint32() const
    {
      return getDigit(0);
    }

    inline uint64_t as_uint64() const
    {
      return (getDigit(1) << 32) | getDigit(0);
    }
  };
} /* namespace sherpa */

inline
std::ostream& operator<<(std::ostream& strm, const sherpa::BigNum& bn)
{
  if (strm.flags() & strm.hex)
    bn.toStream(strm, 16);
  else if (strm.flags() & strm.oct)
    bn.toStream(strm, 8);
  else
    bn.toStream(strm, 10);

  return strm;
}

inline
std::istream& operator>>(std::istream& strm, sherpa::BigNum& bn)
{
  if (strm.flags() & strm.hex)
    bn.fromStream(strm, 16);
  else if (strm.flags() & strm.oct)
    bn.fromStream(strm, 8);
  else
    bn.fromStream(strm, 10);

  return strm;
}

#endif /* LIBSHERPA_BIGNUM_HXX */
