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

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include <libsherpa/utf8.hxx>

#include "LitValue.hxx"

uint32_t
LitValue::DecodeStringCharacter(const char *s, const char **next)
{
  unsigned long codePoint;

  if (*s != '\\') {
    const char *snext;
    codePoint = sherpa::utf8_decode(s, &snext); // &OK
    s = snext;
  }
  else {
    // Initial character was '\\', so this is either \<char> or it is
    // encoded somehow.  Next thing is *probably* a left curly
    // brace, but might be a normal char.
    s++;
    switch(*s++) {
    case '{':
      {
	// Okay, could be either a natural number or one of the special
	// character expansions.
	if (isdigit(*s)) {
	  char *snext;
	  unsigned long radix = strtoul(s, &snext, 10); //&OK

	  // Okay, this is pretty disgusting. If there actually WAS a
	  // radix prefix, then *snext is now 'r'. If not, then either
	  // /radix/ holds the actual numerical code point or something
	  // went horribly wrong.

	  if (*snext == 'r') {
	    s = snext+1;
	    codePoint = strtoul(s, &snext, radix); //&OK
	  }
	  else {
	    codePoint = radix;
	    s = snext;
	  }
	}
	else if (s[0] == 'U' && s[1] == '+') {
	  char *snext;
	  s += 2;
	  codePoint = strtoul(s, &snext, 16); //&OK
	  s = snext;
	}
	else
	  assert(false);

	assert (*s == '}');

	s++;

	break;
      }
    case 'n':
      {
	codePoint = '\n';	// newline
	break;
      }
    case 'r':
      {
	codePoint = '\r';	// return
	break;
      }
    case 't':
      {
	codePoint = '\t';	// tab
	break;
      }
    case 'b':
      {
	codePoint = '\010';	// backspace
	break;
      }
    case 's':
      {
	codePoint = ' ';	// space
	break;
      }
    case 'f':
      {
	codePoint = '\f';	// formfeed
	break;
      }
    case '"':
      {
	codePoint = '"';	// double quote
	break;
      }
    case '\\':
      {
	codePoint = '\\';	// backslash
	break;
      }
    default:
      {
	assert(false);
	break;
      }
    }
  }

  if (next) *next = s;
  return codePoint;
}

// FIX: the current implementation assumes that the input stream
// consists of ASCII characters, which is most definitely a bug.
uint32_t
LitValue::DecodeRawCharacter(const char *s, const char **next)
{
  unsigned long codePoint;

  if (*s != '\\') {
    codePoint = *s++;
  }
  else {
    // Initial character was '\\', so this is either \<char> or it is
    // encoded somehow.  Next thing is *probably* a left curly
    // brace, but might be a normal char.
    s++;
    if (strcmp(s, "space") == 0) {
      codePoint = ' ';
      s+= 5;
    }
    else if (strcmp(s, "tab") == 0) {
      codePoint = '\t';
      s += 3;
    }
    else if (strcmp(s, "linefeed") == 0) {
      codePoint = '\n';
      s += 8;
    }
    else if (strcmp(s, "return") == 0) {
      codePoint = '\r';
      s += 6;
    }
    else if (strcmp(s, "lbrace") == 0) {
      codePoint = '{';
      s += 6;
    }
    else if (strcmp(s, "rbrace") == 0) {
      codePoint = '}';
      s += 6;
    }
    else
      codePoint = *s++;
  }

  if (next) *next = s;
  return codePoint;
}

uint32_t
LitValue::DecodeCharacter(const std::string& s)
{
  const char *str = s.c_str();

  assert(*str == '#');

  return DecodeRawCharacter(str + 1, 0);
}
