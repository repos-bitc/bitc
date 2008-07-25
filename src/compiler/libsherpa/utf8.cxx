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

#include <stdint.h>
#include "utf8.hxx"

namespace sherpa {
// Decode a UTF-8 character, returning the start of next character via
// snext.  Note that this decoder ignores the recent ISO-10646
// resolution, and works for private code points.
//
// Note that this assumes that the input character is canonically
// encoded (i.e. not encoded using perverse length), but does not
// check. The natural implementation of a lexer implementing ungetChar
// is to use utf8_encode on the character being returned to determine
// how far it should back up the input pointer, but if the character
// is perversely encoded this will not work. The fix is for the code
// below to range check the input and generate a ExBadValue exception,
// and then provide a raw_utf8_decode for people who need to fix
// content having bad encodings.
uint32_t
utf8_decode(const char *s, const char **snext)
{
  uint32_t ucs4 = ~0u;
  const uint8_t *sb = (uint8_t *)s;

  if (*sb <= 127) {
    ucs4 = *sb++;
  }
  else if (*sb <= 223) {
    ucs4 = (*sb++ - 192)*64;
    ucs4 += (*sb++ - 128);
  }
  else if (*sb <= 239) {
    ucs4 = (*sb++ - 192)*4096;
    ucs4 += (*sb++ - 128)*64;
    ucs4 += (*sb++ - 128);
  }
  else if (*sb <= 247) {
    ucs4 = (*sb++ - 192)*262144;
    ucs4 += (*sb++ - 128)*4096;
    ucs4 += (*sb++ - 128)*64;
    ucs4 += (*sb++ - 128);
  }
  else if (*sb <= 251) {
    ucs4 = (*sb++ - 192)*16777216;
    ucs4 += (*sb++ - 128)*262144;
    ucs4 += (*sb++ - 128)*4096;
    ucs4 += (*sb++ - 128)*64;
    ucs4 += (*sb++ - 128);
  }
  else if (*sb <= 253) {
    ucs4 = (*sb++ - 192)*1073741824;
    ucs4 += (*sb++ - 128)*16777216;
    ucs4 += (*sb++ - 128)*262144;
    ucs4 += (*sb++ - 128)*4096;
    ucs4 += (*sb++ - 128)*64;
    ucs4 += (*sb++ - 128);
  }

  if (snext) *snext = (char *)sb;
  return ucs4;
}

unsigned
utf8_encode(uint32_t ucs4, char utf[7])
{
  char *utf8 = utf;

  if (ucs4 <= 0x7f) {
    *utf8++ = ucs4;
  }
  else if (ucs4 <= 0x7ff) {
    *utf8++ = 192u + (ucs4 / 64);
    *utf8++ = 128u + (ucs4 % 64);
  }
  else if (ucs4 <= 0xffff) {
    *utf8++ = 224u + (ucs4 / 4096);
    *utf8++ = 128u + ((ucs4 / 64) % 64);
    *utf8++ = 128u + (ucs4 % 64);
  }
  else if (ucs4 <= 0x1fffff) {
    *utf8++ = 240 + (ucs4 / 262144);
    *utf8++ = 128u + ((ucs4 / 4096) % 64);
    *utf8++ = 128u + ((ucs4 / 64) % 64);
    *utf8++ = 128u + (ucs4 % 64);
  }
  else if (ucs4 <= 0x3ffffff) {
    *utf8++ = 248u + (ucs4 / 16777216);
    *utf8++ = 128u + ((ucs4 / 262144) % 64);
    *utf8++ = 128u + ((ucs4 / 4096) % 64);
    *utf8++ = 128u + ((ucs4 / 64) % 64);
    *utf8++ = 128u + (ucs4 % 64);
  }
  else if (ucs4 <= 0x7fffffff) {
    *utf8++ = 252u + (ucs4 / 1073741824);
    *utf8++ = 128u + ((ucs4 / 16777216) % 64);
    *utf8++ = 128u + ((ucs4 / 262144) % 64);
    *utf8++ = 128u + ((ucs4 / 4096) % 64);
    *utf8++ = 128u + ((ucs4 / 64) % 64);
    *utf8++ = 128u + (ucs4 % 64);
  }

  *utf8 = 0;

  return utf8 - utf;
}

} /* namespace sherpa */
