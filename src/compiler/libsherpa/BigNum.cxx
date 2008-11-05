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

#include <assert.h>
#include <string>
#include <string.h>  /* For memset() */
#include <libsherpa/BigNum.hxx>
#include <libsherpa/UExcept.hxx>

// Note that this implementation relies on calloc() for the sake of
// using realloc, but we only do this on heap-allocated arrays of
// scalars so no consideration of destructor logic need apply.

const uint64_t UINT32_T_MAX = 4294967295ull;

/// @brief memset, but initializing from top to bottom.
///
/// In large heap-allocated vectors a segv can occur if a reference
/// occurs too far below bottom of stack. The easiest way to prevent
/// this is to touch the bytes in the proper order.
static void *
rmemset(void *s, int c, size_t n)
{
  unsigned char *cp = (unsigned char *) s;
  for (size_t i = n; i > 0; i--)
    cp[i-1] = c;

  return s;
}

static inline uint32_t max(uint32_t a, uint32_t b)
{
  return (a > b) ? a : b;
}

static inline uint32_t min(uint32_t a, uint32_t b)
{
  return (a < b) ? a : b;
}

namespace sherpa {
  // These operate on "raw" vectors and sizes, which are assumed to *be*
  // vectors.

  struct nvec {
    size_t len;
    uint32_t *vec;

    inline nvec()
    {
      len = 0;
      vec = 0;
    }

    inline nvec(const sherpa::BigNum& bn)
    {
      if (bn.nDigits == 1) {
	len = 1;
	vec = (uint32_t *) &bn.oneDigit;
      }
      else {
	len = bn.nDigits;
	vec = (uint32_t *) bn.digits;
      }
    }

    inline nvec(size_t l, uint32_t *v)
    {
      len = l, vec = v;
    }

    inline uint64_t getDigit(size_t i) const
    {
      return (i >= len) ? 0 : vec[i];
    }
  } ;

  static uint32_t u_zero = 0;
  static const nvec nv_zero(1, &u_zero);
  static uint32_t u_one = 1;
  static const nvec nv_one(1, &u_one);

  /// @brief compare two digit vectors for relative magnitude. Return
  /// -1, 0, 1 according to whether v1 is less than, equal to, or
  /// greater than v2.
  static int
  vu_cmp(const nvec n1, const nvec n2)
  {
    // Recall that the arguments may be denormalized:
    size_t len = max(n1.len, n2.len);

    for (size_t i = 0; i < len; i++) {
      size_t pos = len - i - 1;

      if (n1.getDigit(pos) < n2.getDigit(pos))
	return -1;
      if (n1.getDigit(pos) > n2.getDigit(pos))
	return 1;
    }

    return 0;
  }

  /// @brief n2 = n1 / (radix^nShift)
  ///
  /// Requirement: n2 > 0
  static void
  vu_rshift_digits(const nvec n1, size_t nShift, nvec n2)
  {
    if (nShift > n1.len) {
      memset(n2.vec, 0, n2.len * sizeof(uint32_t));
      return;
    }

    for (size_t i = 0; i < min(n1.len - nShift, n2.len); i++)
      n2.vec[i] = n1.vec[i + nShift];
    for (size_t i = n1.len - nShift; i <n2.len; i++)
      n2.vec[i] = 0;

    return;
  }

  /// @brief n2 = n1 * (radix^nShift)
  ///
  /// Requirement: n2 > 0
  static void
  vu_lshift_digits(const nvec n1, size_t nShift, nvec n2)
  {
    for (size_t i = 0; i < min(n1.len, n2.len - nShift); i++)
      n2.vec[i+nShift] = n1.vec[i];

    for (size_t i = 0; i < nShift; i++)
      n2.vec[i] = 0;

    return;
  }

  /// @brief n2 = n1 >> nShift
  ///
  /// Requirement: n2 > 0
  static void
  vu_rshift_bits(const nvec n1, size_t nShift, nvec n2)
  {
    vu_rshift_digits(n1, nShift / 32, n2);
    nShift %= 32;

    for (size_t i = 0; i < n2.len; i++) {
      uint32_t u = n2.getDigit(i) >> nShift;
      u |= (n2.getDigit(i+1) << (32 - nShift));

      n2.vec[i] = u;
    }

    return;
  }

  /// @brief n2 = n1 << nShift
  ///
  /// Requirement: n2 > 0
  static void
  vu_lshift_bits(const nvec n1, size_t nShift, nvec n2)
  {
    vu_lshift_digits(n1, nShift / 32, n2);
    nShift %= 32;

    for (size_t i = 0; i < min(n1.len, n2.len - nShift); i++) {
      size_t pos = n2.len - i - 1;

      uint32_t u = n2.getDigit(pos) << nShift;
      if (pos > 0)
	u |= n2.getDigit(pos-1) >> (32 - nShift);
      n2.vec[pos] = u;
    }

    return;
  }

  static inline unsigned
  vu_get_bit(const nvec n1, size_t bit)
  {
    return (n1.vec[bit/32] & (1u << (bit % 32))) ? 1 : 0;
  }

  static inline void
  vu_set_bit(nvec n1, size_t bit)
  {
    n1.vec[bit/32] |= (1u << (bit % 32));
  }

  static inline void
  vu_clear_bit(nvec n1, size_t bit)
  {
    n1.vec[bit/32] &= ~(1u << (bit % 32));
  }

  static inline void
  vu_copy(const nvec n1, nvec n2)
  {
    vu_lshift_digits(n1, 0, n2);
  }

  BigNum BigNum::lshift_digits(size_t n) const
  {
    if ((nDigits == 1) && (oneDigit == 0))
      return *this;

    if (n == 0)
      return *this;

    size_t nNewDigits = nDigits + n;
    uint32_t *newDigits = (uint32_t *) alloca(nNewDigits * sizeof(uint32_t));
    rmemset(newDigits, 0, nNewDigits * sizeof(uint32_t));

    for (size_t i = 0; i < nNewDigits; i++)
      newDigits[i+n] = getDigit(i);

    return BigNum(nNewDigits, newDigits, negative);
  }


  /// @brief Subtract: n3 = n1 - n2.
  ///
  /// Update to n3 occurs in-place.
  ///
  /// Requirement: n2 <= n1.
  /// Requirement: n3 is at least as big as n1.
  /// Requirement: n3 does not alias n2
  static void
  vu_sub(const nvec n1, const nvec n2, nvec n3)
  {
    uint64_t borrow = 0llu;

    assert(n1.len <= n3.len);

    for (size_t i = n1.len; i < n3.len; i++)
      n3.vec[i] = 0;

    for (size_t i = 0; i < n1.len; i++) {
      // Note that in the following carry does not get truncated but
      // oDigits[i] DOES.
      uint64_t n1digit = n1.getDigit(i);
      uint64_t n2digit = n2.getDigit(i) + borrow;

      borrow = 0;
      if (n1digit < n2digit) {
	n1digit += (UINT32_T_MAX + 1);
	borrow = 1;
      }

      assert(i < n3.len);
      n3.vec[i] = n1digit - n2digit;
    }

    assert(borrow == 0);
  }

  /// @brief Add: n3 = n1 + n2.
  ///
  /// Update to n3 occurs in-place.
  ///
  /// Requirement: n3.len = max(n1.len, n2.len)+1
  ///
  static void
  vu_add(const nvec n1, const nvec n2, nvec n3)
  {
    uint64_t carry = 0;

    for (size_t i = 0; i < n3.len; i++) {
      carry = n1.getDigit(i) + n2.getDigit(i) + carry;
      if (i < n3.len)
	n3.vec[i] = carry;	// truncates
      carry >>= 32;
    }

    assert(carry == 0);
  }

  /// @brief Multiply: n3 = n1 * n2.
  ///
  /// Update to n3 occurs in-place.
  ///
  /// Requirement: n3.len = n1.len + n2.len
  ///
  static void
  vu_mul(const nvec n1, const nvec n2, nvec n3)
  {
    uint64_t carry = 0llu;

    nvec step;
    step.len = n1.len + n2.len;
    step.vec = (uint32_t *) alloca(step.len * sizeof(uint32_t));
    rmemset(step.vec, 0, step.len * sizeof(uint32_t));

    nvec result;
    result.len = n1.len + n2.len;
    result.vec = (uint32_t *) alloca(result.len * sizeof(uint32_t));
    rmemset(result.vec, 0, result.len * sizeof(uint32_t));

    for (size_t i = 0; i < n1.len; i++) {
      memset(step.vec, 0, step.len * sizeof(uint32_t));
      carry = 0llu;

      for (size_t j = 0; j < step.len; j++) {
	carry = n1.getDigit(i) * n2.getDigit(j) + carry;
	step.vec[i+j] = carry;	// truncating
	carry >>= 32;
      }

      vu_add(result, step, result);
    }

    assert(carry == 0);
    vu_copy(result, n3);
  }

  /// @brief Divide q = n1 / n2, r = (n1 - (n2 * q)).
  static void
  vu_divqr(const nvec dividend, const nvec divisor,
	   nvec quotient, nvec remainder)
  {
    int cmp = vu_cmp(dividend, divisor);
    if (cmp < 0) {
      vu_copy(nv_zero, quotient);
      vu_copy(dividend, remainder);
      return;
    }
    else if (cmp == 0) {
      vu_copy(nv_one, quotient);
      vu_copy(nv_zero, remainder);
      return;
    }

    // Need to do things the hard way...

    nvec q;
    q.len = dividend.len;  // conservative
    q.vec = (uint32_t *) alloca(q.len * sizeof(uint32_t));
    rmemset(q.vec, 0, q.len * sizeof(uint32_t));

    // Internal working copies of quotient and remainder:
    nvec tmp;
    tmp.len = q.len + divisor.len;	// because of vu_mul requirement
    tmp.vec = (uint32_t *) alloca(tmp.len * sizeof(uint32_t));

    size_t bits = q.len * 32;

    for (size_t i = 0; i < bits; i++) {
      size_t bitndx = bits - i - 1;

      vu_set_bit(q, bitndx);

      vu_mul(divisor, q, tmp);
      if (vu_cmp(tmp, dividend) > 0)
	vu_clear_bit(q, bitndx);
    }

    vu_copy(q, quotient);
    vu_mul(divisor, q, tmp);
    vu_sub(dividend, tmp, remainder);
  }

  BigNum::~BigNum()
  {
    if (nDigits > 1)
      delete [] digits;
  }

  BigNum::BigNum(const BigNum& other)
  {
    negative = other.negative;
    nDigits = other.nDigits;
    if (nDigits > 1) {
      digits = new uint32_t[nDigits];
      for (size_t i = 0; i < nDigits; i++)
	digits[i] = other.digits[i];
    }
    else
      oneDigit = other.oneDigit;
  }

  BigNum& BigNum::operator=(const BigNum& other)
  {
    if (nDigits > 1)
      delete [] digits;

    negative = other.negative;
    nDigits = other.nDigits;
    if (nDigits > 1) {
      digits = new uint32_t[nDigits];
      for (size_t i = 0; i < nDigits; i++)
	digits[i] = other.digits[i];
    }
    else
      oneDigit = other.oneDigit;

    return *this;
  }

  BigNum::BigNum(uint64_t u, bool neg)
  {
    negative = neg;
    if (u > UINT32_T_MAX) {
      nDigits = 2;
      digits = new uint32_t[nDigits];
      digits[0] = u;		// truncating
      digits[1] = (u >> 32);
    }
    else {
      nDigits = 1;
      oneDigit = u;
    }
  }

  BigNum::BigNum(uint64_t u)
  {
    negative = false;
    if (u > UINT32_T_MAX) {
      nDigits = 2;
      digits = new uint32_t[nDigits];
      digits[0] = u;		// truncating
      digits[1] = (u >> 32);
    }
    else {
      nDigits = 1;
      oneDigit = u;
    }
  }

  BigNum::BigNum(int64_t i)
  {
    if (i < 0) {
      negative = true;
      i = -i;
    }
    else
      negative = false;

    // Rest proceeds as for uint64_t:
    uint64_t u = i;
    if (u > UINT32_T_MAX) {
      nDigits = 2;
      digits = new uint32_t[nDigits];
      digits[0] = u;		// truncating
      digits[1] = (u >> 32);
    }
    else {
      nDigits = 1;
      oneDigit = u;
    }
  }

  // This should be called *only* in return position, because it
  // potentially modifies an argument!!
  inline
  BigNum::BigNum(size_t _nDigits, uint32_t* _digits, bool _negative)
  {
    negative = _negative;
    nDigits = _nDigits;

    while (nDigits > 1 && (_digits[nDigits - 1] == 0))
      nDigits--;

    if (nDigits == 1) {
      oneDigit = _digits[0];

      if (oneDigit == 0)
	negative = false;
    }
    else {
      digits = new uint32_t[nDigits];
      for (size_t i = 0; i < nDigits; i++)
	digits[i] = _digits[i];

      return;
    }
  }

  BigNum::BigNum(const std::string& s, uint32_t radix)
  {
    // Defer setting is_negative, because the iterative multiplication
    // by radix can get the wrong answer if there is an even number of
    // digits.

    bool is_negative = false;

    // Initialize to zero so that we are well-formed for the loop below.
    negative = false;
    nDigits = 1;
    oneDigit = 0;

    size_t i = 0;

    if (s[0] == '-') {
      is_negative = true;
      i++;
    }
    else if (s[0] == '+') {
      i++;
    }

    if (radix == 0) {
      if (s[i] == '0' && ((s[i+1] == 'x') || (s[i+1] == 'X'))) {
	radix = 16;
	i += 2;
      }
      else if (s[i] == 0) {
	radix = 8;
	i += 1;
      }
      else
	radix = 10;
    }

    for (; i < s.size(); i++) {
      char c = s[i];
      uint32_t thisDigit = 0;

      if (c >= '0' && c <= '9')
	thisDigit = c - '0';
      else if (c >= 'a' && c <= 'f')
	thisDigit = 10 + (c - 'a');
      else if (c >= 'A' && c <= 'F')
	thisDigit = 10 + (c - 'A');

      *this = *this * radix + thisDigit;
    }

    if (is_negative)
      negative = is_negative;
  }

  BigNum BigNum::operator+(const BigNum& other) const
  {
    // Check if this is really addition:
    if (negative == other.negative) {
      const nvec nvother(other);
      const nvec nvthis(*this);

      nvec n3;
      n3.len = max(nvother.len, nvthis.len) + 1;
      n3.vec = (uint32_t *) alloca(n3.len * sizeof(uint32_t));

      vu_add(nvthis, nvother, n3);

      return BigNum(n3.len, n3.vec, negative);
    }

    if (negative)		// !other.negative
      return (other - this->abs());

    // !negative, other.negative
    return (*this - other.abs());
  }

  BigNum BigNum::operator-(const BigNum& other) const
  {
    const nvec nvother(other);
    const nvec nvthis(*this);

    // Check for addition in disguise:
    if (negative != other.negative) {
      size_t osize = max(nDigits, other.nDigits) + 1;
      uint32_t *oDigits = (uint32_t *) alloca(osize * sizeof(uint32_t));

      nvec n3(osize, oDigits);
      vu_add(nvthis, nvother, n3);

      return BigNum(n3.len, n3.vec, negative);
    }

    // Both negative or both positive: result sign follows magnitude:
    nvec n3;
    n3.len = max(nvother.len, nvthis.len);
    n3.vec = (uint32_t *) alloca(n3.len * sizeof(uint32_t));

    if (vu_cmp(nvthis, nvother) > 0) {
      vu_sub(nvthis, nvother, n3);
      return BigNum(n3.len, n3.vec, negative);
    }
    else {
      vu_sub(nvother, nvthis, n3);
      return BigNum(n3.len, n3.vec, !negative);
    }

  }

  BigNum BigNum::operator*(const BigNum& other) const
  {
    bool result_neg = (negative != other.negative);

    const nvec nvother(other);
    const nvec nvthis(*this);

    nvec n3;
    n3.len = nvother.len + nvthis.len;
    n3.vec = (uint32_t *) alloca(n3.len * sizeof(uint32_t));

    // u_mul is faster if argument is the shorter one.
    vu_mul(nvother, nvthis, n3);

    return BigNum(n3.len, n3.vec, result_neg);
  }

  BigNum BigNum::operator/(const BigNum& other) const
  {
    if (other == 0)
      THROW(excpt::BadValue, "Divide by zero");

    bool result_neg = (negative != other.negative);

    const nvec nvthis(*this);
    const nvec nvother(other);

    // Following are longer than we need, but required to satisfy
    // vu_sub input field length requirements.
    nvec r;			// will be discarded
    r.len = nvthis.len;
    r.vec = (uint32_t *) alloca(r.len * sizeof(uint32_t));

    nvec q;
    q.len = nvthis.len;
    q.vec = (uint32_t *) alloca(q.len * sizeof(uint32_t));

    vu_divqr(nvthis, nvother, q, r);

    return BigNum(q.len, q.vec, result_neg);
  }

  BigNum BigNum::operator%(const BigNum& other) const
  {
    const nvec nvthis(*this);
    const nvec nvother(other);

    // Following are longer than we need, but required to satisfy
    // vu_sub input field length requirements.
    nvec r;
    r.len = nvthis.len;
    r.vec = (uint32_t *) alloca(r.len * sizeof(uint32_t));

    nvec q;			// will be discarded
    q.len = nvthis.len;
    q.vec = (uint32_t *) alloca(q.len * sizeof(uint32_t));

    vu_divqr(nvthis, nvother, q, r);

    // remainder sign follows sign of *this:
    return BigNum(r.len, r.vec, negative);
  }

  int BigNum::cmp(const BigNum& other) const
  {
    if (negative == other.negative) {
      const nvec n1(*this);
      const nvec n2(other);
      int result= vu_cmp(n1, n2);
      return negative ? -result : result;
    }
    else if (negative)
      return -1;
    else
      return 1;
  }

  BigNum BigNum::abs() const
  {
    if (negative) {
      BigNum v = *this;
      v.negative = false;
      return v;
    }
    else
      return *this;
  }

  BigNum BigNum::neg() const
  {
    if (nDigits == 0 && oneDigit == 0)
      return *this;

    BigNum v = *this;
    v.negative = !negative;
    return v;
  }

  BigNum BigNum::operator<<(size_t n)
  {
    nvec nv;
    nv.len = nDigits + ((n+31)/32); // upper bound
    nv.vec = (uint32_t *) alloca(nv.len * sizeof(uint32_t));

    const nvec me(*this);
    vu_lshift_bits(me, n, nv);

    return BigNum(nv.len, nv.vec, negative);
  }

  BigNum BigNum::operator>>(size_t n)
  {
    nvec nv;
    nv.len = nDigits - (n/32);	// upper bound
    nv.vec = (uint32_t *) alloca(nv.len * sizeof(uint32_t));

    const nvec me(*this);
    vu_rshift_bits(me, n, nv);

    return BigNum(nv.len, nv.vec, negative);
  }

  std::string BigNum::asString(uint32_t radix) const
  {
    static const char *hexdigits = "0123456789ABCDEF";
    std::string s;

    if (negative)
      s.append(1, '-');

    if (radix == 16) {
      for (size_t i = 0; i < nDigits; i++) {
	size_t pos = nDigits - i - 1;
	uint32_t u = getDigit(pos);

	for (size_t hd = 0; hd < sizeof(u)*2; hd++) {
	  uint32_t nibble = sizeof(u)*2 - hd - 1;

	  uint32_t digit = (u >> (nibble * 4)) & 0xFu;
	
	  if (digit == 0) {
	    if (s.size())
	      s.append(1, hexdigits[digit]);
	  }
	  else
	    s.append(1, hexdigits[digit]);
	}
      }

      return s;
    }
    else {
      BigNum me = *this;

#if 0
      BigNum lastPlace(radix);
      BigNum place(radix);

      while (place < *this) {
	lastPlace = place;
	place = lastPlace * radix;
      }

      // Place is now too big. Back off one:
      place = lastPlace;

      while (place != BigNum(0)) {
	BigNum thisDigit = me / place;

	if (thisDigit == 0) {
	  if (s.size())
	    s.append(1, hexdigits[thisDigit.oneDigit]);
	}
	else			// non-zero digit
	  s.append(1, hexdigits[thisDigit.oneDigit]);

	me = me - (thisDigit * place);
	place = place / radix;
      }

      if (s.size() == 0)
	s.append(1, '0');
#else
      std::string result;

      while (me != 0) {
	BigNum thisDigit = me % radix;

	result.append(1, hexdigits[thisDigit.getDigit(0)]);
	
	me /= radix;
      }

      if (result.size() == 0)
	result.append(1, '0');

      for (size_t i = 0; i < result.size(); i++)
	s.append(1, result[result.size() - i - 1]);

#endif
    }

    return s;
  }

  void BigNum::toStream(std::ostream& strm, uint32_t radix) const
  {
    strm << asString(radix);
  }

  void BigNum::fromStream(std::istream& strm, uint32_t radix)
  {
    *this = 0;

    for(;;) {
      char c;
      strm >> c;

      if ('0' <= c && c <= '9' && radix > (size_t)(c - '0')) {
	*this *= radix;
	*this += (c - '0');
      }
      else if (radix == 16 && 'a' <= c && c <= 'f') {
	  *this *= radix;
	  *this += (c - 'a' + 10);
      }
      else if (radix == 16 && 'A' <= c && c <= 'F') {
	  *this *= radix;
	  *this += (c - 'A' + 10);
      }
      else {
	strm.putback(c);
	return;
      }
    }
  }
}
