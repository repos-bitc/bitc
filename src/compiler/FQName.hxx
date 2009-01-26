#ifndef FQNAME_HXX
#define FQNAME_HXX

/**************************************************************************
 *
 * Copyright (C) 2008, Johns Hopkins University.
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
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>

#define FQ_NOIF ""

struct FQName {
  std::string iface;
  std::string ident;

  /// @brief Separator to use when displaying a fully qualified name.
  ///
  /// This must <em>not</em> be a legal identifier character.
  static const char sep;
  /// @brief Separator to use when making a local binding of a name
  /// that is being imported.
  ///
  /// This should be the same character that is used in any
  /// environment-like combining form, which is usually '.'
  static const char LocalBindingSep;

  bool operator <(const FQName&) const;
  bool operator >(const FQName&) const;
  bool operator ==(const FQName&) const;
  FQName& operator =(const FQName&);

  bool operator !=(const FQName& _fqname) const
  {
    return !(*this == _fqname);
  }

  bool isInitialized()
  {
    return (ident.size() != 0);
  }

  FQName()
  {
  }

  /// @brief Copy constructor
  FQName(const FQName& that)
  {
    iface = that.iface;
    ident = that.ident;
  }

  /// @brief Initialize from separate strings
  FQName(const std::string& _iface, const std::string& _ident)
  {
    iface = _iface;
    ident = _ident;
  }

  /// @brief Initialize from a single string, where the interface and
  /// ident components are separated by FQName::sep.
  FQName(const std::string& nm);

  std::string asString() const;
};

inline
std::ostream& operator<<(std::ostream& strm, const FQName& fqn)
{
  strm << fqn.asString();
  return strm;
}

#endif /* FQNAME_HXX */
