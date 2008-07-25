#ifndef LIBSHERPA_UTIL_HXX
#define LIBSHERPA_UTIL_HXX

/**************************************************************************
 *
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, The EROS
 *   Group, LLC. 
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

namespace sherpa {
  std::string strdowncase(const std::string&);

  std::string unsigned_str(uint32_t);
  std::string unsigned64_str(uint64_t);

  std::string sgetenv(const char *);

  inline std::string sgetenv(const std::string& s)
  {
    return sgetenv(s.c_str());
  }

  uint64_t atollu(const char *);
  uint64_t atollu(const std::string&);

  typedef uint32_t hash32_t;

  extern const char * strnchr(const char *s, size_t len, char c);
  inline const unsigned char * 
  ustrnchr(const unsigned char *s, size_t len, unsigned char c) {
    return (unsigned char *) strnchr((const char *) s, len, (char) c);
  }

#ifdef NEED_CONVERT
  void* xbsearch(const void *key, const void *base, size_t nmemb,
		 size_t size, int (*compar)(const void *, const void *));

#endif /* NEED_CONVERT */

} /* namespace sherpa */

#endif /* LIBSHERPA_UTIL_HXX */
