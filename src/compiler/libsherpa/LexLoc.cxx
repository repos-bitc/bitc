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

#include <assert.h>
#include <dirent.h>

#include <string>
#include <sstream>

#include <stdlib.h>  // for strtoul()

#include <libsherpa/LexLoc.hxx>

namespace sherpa {
  std::string
  LexLoc::asString() const
  {
    if (line) {
      std::ostringstream msg;
      msg << origin << ":"
	  << line << ":" << offset;
      return msg.str();
    }
    else
      return "<internal>";
  }

  // Implementation of this lives in LTokString.cxx, along with all of
  // the other decoding logic.
  void
  LexLoc::updateWith(const std::string& s)
  {
    size_t i = 0;

    for (i = 0; i < s.size(); i++) {
      switch(s[i]) {
      case '\n':
	{
	  line++;
	  offset = 0;
	  break;
	}
      case '\r':
	{
	  line++;
	  offset = 0;
	  // Check for CR LF:
	  if ((i + 1) < s.size() && s[i+1] == '\n')
	    i++;
	  break;
	}
      default:
	{
	  offset++;
	}
      }
    }
  }

} /* namespace sherpa */
