/*
 * Copyright (C) 2006, The EROS Group, LLC.
 *
 * This file is part of the EROS Operating System runtime library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330 Boston, MA 02111-1307, USA.
 */

#include <unistd.h>
#include <fcntl.h>

#include "BUILD/bitc-runtime.h"

struct ty_bitc_stdioStream {
  FILE *f;
} ;

// Okay -- this is nasty. We would LIKE to write these declarations
// as:
//
//   static stdiostream our_stdin = { stdin };
//   static stdiostream our_stdout = { stdout };
//   static stdiostream our_stderr = { stderr };
//
// but unfortunately stdin, stdout, stderr are not constants that can
// be reused in this way. We can either resolve this with an init
// section procedure to hand-initialize them or with an accessor
// function. Shap prefers the init section approach, but that is ELF
// dependent and not everyone uses ELF (which is silly, but what can
// you do).

static ty_bitc_stdioStream our_stdin = { 0 };
static ty_bitc_stdioStream our_stdout = { 0 };
static ty_bitc_stdioStream our_stderr = { 0 };
 
static inline void
fix_stdio_stream(ty_bitc_stdioStream *ios)
{
  if (ios->f == 0) {
    if (ios == &our_stdin)
      ios->f = stdin;
    else if (ios == &our_stdout)
      ios->f = stdout;
    else if (ios == &our_stderr)
      ios->f = stderr;
  }
}

ty_bitc_stdioStream *bitc_stdio_stdin  = &our_stdin;
ty_bitc_stdioStream *bitc_stdio_stdout = &our_stdout;
ty_bitc_stdioStream *bitc_stdio_stderr = &our_stderr;

ty_bitc_stdioStream *
DEFUN(bitc_stdio_open, bitc_string_t *nm, bitc_string_t *mode)
{
  ty_bitc_stdioStream *ios = bitc_malloc_atomic(sizeof(ty_bitc_stdioStream));
  ios->f = fopen(nm->s, mode->s);
  if (ios->f == NULL)
    bitc_throw(&val_ExNoPermission);

  return ios;
}
DEFCLOSURE(bitc_stdio_open);

bitc_unit_t
DEFUN(bitc_stdio_close, ty_bitc_stdioStream *ios)
{
  fix_stdio_stream(ios);

  if (ios->f > 0) {
    fclose(ios->f);
    ios->f = NULL;
  }

  return BITC_UNIT;
}
DEFCLOSURE(bitc_stdio_close);
  
// The encoding and decoding procedures below are taken from
//
//  http://www1.tip.nl/~t876506/utf8tbl.html#algo
//

// A BitC char is a 32-bit UNICODE UCS-4 code point. The BitC external
// representation is UTF-8, so we need to transcode it here. 
// Note: This is deficient, because it is not doing proper unicode
// code point decoding.
bitc_char_t
DEFUN(bitc_stdio_read_char, ty_bitc_stdioStream *ios)
{
  bitc_uns8_t encoded[6];
  bitc_char_t ucs4;
  ssize_t result;

  fix_stdio_stream(ios);

  // Read the first char:
  result = fread(&encoded[0], 1, 1, ios->f);
  if (result != 1)
    bitc_throw(&val_ExAtEOF);

  if (encoded[0] <= 127) {
    ucs4 = encoded[0];
  }
  else if (encoded[0] <= 223) {
    result = fread(&encoded[1], 1, 1, ios->f);
    if (result != 1)
      bitc_throw(&val_ExAtEOF);
    ucs4 = 
      (encoded[0] - 192)*64 
      + (encoded[1]-128);
  }
  else if (encoded[0] <= 239) {
    result = fread(&encoded[1], 2, 1, ios->f);
    if (result != 1)
      bitc_throw(&val_ExAtEOF);
    ucs4 = 
      (encoded[0] - 224)*4096 
      + (encoded[1]-128)*64
      + (encoded[2]-128);
  }
  else if (encoded[0] <= 247) {
    result = fread(&encoded[1], 3, 1, ios->f);
    if (result != 1)
      bitc_throw(&val_ExAtEOF);
    ucs4 = 
      (encoded[0] - 240)*262144
      + (encoded[1]-128)*4096
      + (encoded[2]-128)*64
      + (encoded[3]-128);
  }
  else if (encoded[0] <= 251) {
    result = fread(&encoded[1], 4, 1, ios->f);
    if (result != 1)
      bitc_throw(&val_ExAtEOF);
    ucs4 = 
      (encoded[0] - 248)*16777216 
      + (encoded[1]-128)*262144
      + (encoded[2]-128)*4096
      + (encoded[3]-128)*64
      + (encoded[4]-128);
  }
  else if (encoded[0] <= 253) {
    result = fread(&encoded[1], 5, 1, ios->f);
    if (result != 1)
      bitc_throw(&val_ExAtEOF);
    ucs4 = 
      (encoded[0] - 252)*1073741824
      + (encoded[1]-128)*16777216 
      + (encoded[2]-128)*262144
      + (encoded[3]-128)*4096
      + (encoded[4]-128)*64
      + (encoded[5]-128);
  }
  else
    bitc_throw(&val_ExNotUTF8);

  return ucs4;
}
DEFCLOSURE(bitc_stdio_read_char);

// A BitC char is a 32-bit UNICODE UCS-4 code point. The BitC external
// representation is UTF-8, so we need to transcode it here. 
bitc_unit_t
DEFUN(bitc_stdio_write_char, ty_bitc_stdioStream *ios, bitc_char_t ucs4)
{
  ssize_t result;
  fix_stdio_stream(ios);

  bitc_uns8_t encoded[6];
  bitc_uns8_t *utf8 = encoded;

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

  result = fwrite(encoded, utf8-encoded, 1, ios->f);

  if (result == 0)
    bitc_throw(&val_ExAtEOF);
  else if (result < 0)
    bitc_throw(&val_ExNoPermission);

  return BITC_UNIT;
}
DEFCLOSURE(bitc_stdio_write_char);

// Note: This is deficient, because it is not doing proper unicode
// code point deco ding.
bitc_uns8_t
DEFUN(bitc_stdio_read_byte, ty_bitc_stdioStream *ios)
{
  unsigned char c;

  fix_stdio_stream(ios);

  if ( fread(&c, 1, 1, ios->f) < 0 )
    bitc_throw(&val_ExNoPermission);

  return c;
}
DEFCLOSURE(bitc_stdio_read_byte);

// Note: This is deficient, because it is not doing proper unicode
// code point encoding.
bitc_unit_t
DEFUN(bitc_stdio_write_byte, ty_bitc_stdioStream *ios, bitc_uns8_t c)
{
  fix_stdio_stream(ios);

  if ( fwrite(&c, 1, 1, ios->f) < 0 )
    bitc_throw(&val_ExNoPermission);

  return BITC_UNIT;
}
DEFCLOSURE(bitc_stdio_write_byte);

bitc_bool_t
DEFUN(bitc_stdio_eofp, ty_bitc_stdioStream *ios)
{
  fix_stdio_stream(ios);

  return (feof(ios->f) ? true : false);
}
DEFCLOSURE(bitc_stdio_eofp);
