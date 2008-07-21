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

#include "LexLoc.hxx"

namespace sherpa {
  LexLoc LexLoc::Unspecified("<internal>", 0,0);

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

  std::string 
  LexLoc::encode() const
  {
    std::ostringstream msg;

    msg << '#' << '{'
	<< origin.size()
	<< ' ';

    /* If the origin string contains a '}' character, double it. This
       allows us to write a regular expression that can skip location 
       updates without difficulty. */
    for (size_t i = 0; i < origin.size(); i++) {
      if (origin[i] == '}' || origin[i] == '#')
	msg << '#';
      msg << origin[i];
    }

    msg << ' ' << line << ' ' << offset << '}';

    return msg.str();
  }

  LexLoc
  LexLoc::decode(const std::string& s)
  {
    assert(s[0] == '#' && s[1] == '{');

    size_t i = 2;

    /* Pick off the filename size */
    std::string num_s;
    while (isdigit(s[i])) {
      num_s += s[i];
      i++;
    }

    /* Pick off the filename, decoding the '#' characters. */
    assert (s[i] == ' ');
    i++;
    size_t len = strtoul(num_s.c_str(), 0, 0);
    std::string fileName;
  
    for (size_t p = 0; p < len; p++) {
      if (s[i] == '#')
	i++;
      fileName += s[i];
      i++;
    }

    assert (s[i] == ' ');
    i++;

    /* Pick off the line number */
    num_s = "";
    while (isdigit(s[i])) {
      num_s += s[i];
      i++;
    }

    unsigned line = strtoul(num_s.c_str(), 0, 0);

    assert (s[i] == ' ');
    i++;

    /* Pick off the offset */
    num_s = "";
    while (isdigit(s[i])) {
      num_s += s[i];
      i++;
    }

    unsigned offset = strtoul(num_s.c_str(), 0, 0);

    LexLoc loc(fileName, line, offset);

    assert (s[i] == '}');
    i++;

    return loc;
  }

  size_t
  LexLoc::skip(const std::string& s, size_t start)
  {
    assert(s[start] == '#' && s[start+1] == '{');

    /* At the moment, these two tests are prevented by the above assert, 
       but at some point I may want to generalize the code. */
    if (s[start] != '#')
      return 0;

    if (s[start+1] != '{')
      return 0;

    for (size_t i = start+2; i < s.size(); i++) {
      if (s[i] == '}')
	return i+1;

      if (s[i] == '#')
	i++;
    }

    return s.size();
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
      case '#':
	{
	  if (s[i+1] == '{') {
	    i = skip(s, i) - 1;
	    continue;
	  }

	  /* Otherwise, this '#' character quotes the next character */
	  i++;
	  /* fall through */
	}
      default:
	{
	  offset++;
	}
      }
    }
  }

} /* namespace sherpa */
