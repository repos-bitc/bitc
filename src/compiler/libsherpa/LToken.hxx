#ifndef LIBSHERPA_LTOKEN_HXX
#define LIBSHERPA_LTOKEN_HXX

/**************************************************************************
 *
 * Copyright (C) 2008, The EROS Group, LLC. 
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

#include <libsherpa/LexLoc.hxx>

/// @brief A lexically indivisible unit of text with an associated
/// location.
///
/// The LToken class provides a degree of support for multiphase
/// lexical processing where the input may be rewritten by several
/// passes. In particular, it allows the intermediate passes to
/// correctly track input locations for any portion of the current
/// string that originated from a file, and also to correctly
/// attribute locations when input strings must be rewritten into
/// intermediate forms whose length may not agree with the length of
/// the original text. Provision is made for an LToken that explicitly
/// identifies its origin location as "unspecified".
///
/// For example, the CapIDL input processing
/// for documentation comments goes through several phases during
/// which pieces of the input may be replaced internally by
/// alternative strings, and also phases in which new text may be
/// introduced whose length does not match the original input text. In
/// order to support correct error reporting, we track these cases by
/// describing the input as a string whose elements are LTokens.

namespace sherpa {

  struct LToken {
    LexLoc loc;
    std::string str;

    char operator[](size_t pos) const
    {
      return str[pos];
    }

    LToken()
      :loc(), str()
    {
    }

    LToken(const LexLoc& loc, const std::string& s)
    {
      this->loc = loc;
      this->str = s;
    }

    LToken(const LToken& that)
    {
      this->loc = that.loc;
      this->str = that.str;
    }

    LToken(const std::string& that)
    {
      this->loc = LexLoc();
      this->str = that;
    }

    LToken& operator=(const LToken& that)
    {
      this->loc = that.loc;
      this->str = that.str;
      return *this;
    }
  };

} /* namespace sherpa */

#endif /* LIBSHERPA_LTOKEN_HXX */
