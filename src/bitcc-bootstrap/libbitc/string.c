/*
 * Copyright (C) 2006, The EROS Group, LLC.
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
 */

#include <unistd.h>
#include <fcntl.h>
#include "BUILD/bitc-runtime.h"

static bitc_char_t
utf8_decode(const char *s, const char **snext)
{
  uint32_t ucs4=0;
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


// FIX: Swaroop carried this code over from Libsherpa
static unsigned
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


bitc_word_t
DEFUN(bitc_string_length, bitc_string_t *str)
{
  bitc_word_t len = 0;
  const char *s = str->s;
  while (*s) {
    utf8_decode(s, &s);
    len++;
  }

  return len;
}
DEFCLOSURE(bitc_string_length);

bitc_char_t
DEFUN(bitc_string_nth, bitc_string_t *str, bitc_word_t ndx)
{
  bitc_word_t len = ndx;
  const char *s = str->s;

  while(*s && len) {
    utf8_decode(s, &s);
    len--;
  }

  if (*s == 0)
    bitc_throw(&val_ExIndexBoundsError);

  return utf8_decode(s, 0);
}
DEFCLOSURE(bitc_string_nth);

/* This is a very bad implementation of vector->string, and it is
   probably even wrong, deppending on how correct the utf8_encode is.
   This should be considered a placeholder for real code */
bitc_string_t *
DEFUN(bitc_vector_string, arg_0_bitc_vector_string vec)
{
  size_t len = vec->len;
  bitc_string_t *tmp = (bitc_string_t *) 
    GC_ALLOC_ATOMIC(sizeof(bitc_string_t));

  char *max = (char *) GC_ALLOC_ATOMIC(sizeof(char) * len * 7);
  size_t totLen = 0;
  for(size_t i=0; i < len; i++) {
    char utf8[7];
    size_t thisLen = utf8_encode(vec->elem[i], utf8);    
    for(size_t j=0; j < thisLen; j++) {
      max[totLen] = utf8[thisLen]; 
      totLen++;
    }
  }

  char* exact = (char *) GC_ALLOC_ATOMIC(sizeof(char) * totLen);  
  for(size_t i=0; i < totLen; i++)
    exact[i] = max[i];

  tmp->length = totLen;
  tmp->s = exact;
  return tmp;
}
DEFCLOSURE(bitc_vector_string);

#if 0
bitc_unit_t
DEFUN(bitc_string_map, bitc_string_t *str, CL1* fn)
{
  bitc_word_t len = ndx;
  const char *s = str->s;

  while(*s && len) {
    utf8_decode(s, &s);
    len--;
  }

  if (*s == 0)
    bitc_throw(&val_ExIndexBoundsError);

  return utf8_decode(s, 0);
}
DEFCLOSURE(bitc_string_nth);
#endif
