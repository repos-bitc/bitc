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

#include <stdarg.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include <string>
#include <iostream>
#include <sstream>

#include <libsherpa/format.hxx>

namespace sherpa {
  std::string vformat(const char *fmt, va_list ap)
  {
    std::ostringstream oss;

#define PUT_DIGIT(d)			   \
    do {				   \
    *bufp = '0'+(d);			   \
    if (d >= 10) *bufp = (cnv + ((d)-10)); \
    bufp++;				   \
  } while(false)

    while (*fmt) {
      if (*fmt != '%') {
	oss << *fmt++;
	continue;
      }

      /* *fmt is '%', so we are looking at an argument specification. */
      fmt++;

      {
	char buf[40];		/* long enough for anything */
	char *bufp = buf;
	char *sptr = 0;
	size_t slen = 0;

	bool needSign = false;	/* true if we need space for a sign */
	bool isNegative = false;
	bool ladjust = false;
	char posSign = ' ';
	unsigned width = 0;
	unsigned prec = 0;
	unsigned base = 10;
	char len = ' ';
	char padc = ' ';
	char cnv = ' ';

	/* Parse the flag characters */
	for(;;) {
	  switch (*fmt) {
	  case ' ': /* Initial sign/space? */
	    {
	      needSign = true;
	      fmt++;
	      continue;
	    }

	  case '+': /* Initial +/- mandatory */
	    {
	      needSign = true;
	      posSign = '+';
	      fmt++;
	      continue;
	    }

	  case '-': /* Left adjust */
	    {
	      ladjust = true;
	      fmt++;
	      continue;
	    }

	  case '0': /* Leading zero? */
	    {
	      padc = '0';
	      fmt++;
	      continue;
	    }

	    /* Following are ignored: */
	  case '#': /* Alternate output format */
	  case '\'': /* SUSv2 Group by thousands according to locale */
	  case 'I': /* GLIBC use locale-specific digits */
	    fmt++;
	    continue;

	  default:
	    break;
	  }

	  break;			/* proceed to field width */
	}

	/* Field width */
	while (isdigit(*fmt)) {
	  width *= 10;
	  width += (*fmt - '0');
	  fmt++;
	}

	/* Precision */
	if (*fmt == '.') {
	  fmt++;
	  while (isdigit(*fmt)) {
	    prec *= 10;
	    prec += (*fmt - '0');
	    fmt++;
	  }
	}

	/* Length modifier */
	switch (len = *fmt) {
	case 'h':			/* short, char */
	  {
	    fmt++;

	    if (*fmt == 'h') {
	      fmt++;
	      len = 'c';
	    }
	    break;
	  }
	case 'l':			/* long, long long */
	  {
	    fmt++;
	    if (*fmt == 'l') {
	      fmt++;
	      len = 'q';
	    }
	    break;
	  }
#if 0
	case 'L':			/* long double */
	  {
	    fmt++;
	    len = sizeof(long double);
	    break;
	  }
#endif
	  // 'q' legacy BSD 4.4 not supported
	case 'j':			/* intmax_t */
	  {
	    fmt++;
	    len = sizeof(intmax_t);
	    break;
	  }
	case 'z':			/* size_t */
	  {
	    fmt++;
	    len = sizeof(size_t);
	    break;
	  }
	case 't':			/* ptrdiff_t */
	  {
	    fmt++;
	    len = sizeof(ptrdiff_t);
	    break;
	  }
	}

	/* Conversion specifier */
	switch (cnv = *fmt++) {
	  /* FIRST ALL THE SIGNED INTEGER CASES */
	case 'o':			/* octal */
	  {
	    base = 8;
	    /* FALL THROUGH */
	  }
	case 'd':			/* signed int */
	case 'i':
	  {
	    long long ll;

	    switch (len) {
	    case 'q':
	      ll = va_arg(ap, long long);
	      break;
	    case 'j':
	      ll = va_arg(ap, intmax_t);
	      break;
	    case 'z':
	      ll = va_arg(ap, size_t);
	      break;
	    case 't':
	      ll = va_arg(ap, ptrdiff_t);
	      break;
	    default:
	      ll = va_arg(ap, long); /* handles char, short too */
	      break;
	    }

	    if (ll < 0ll) {
	      isNegative = true;
	      needSign = true;
	      ll = -ll;
	    }

	    if (ll == 0ll)
	      PUT_DIGIT(0);

	    while (ll) {
	      int digit = ll % base;
	      ll = ll / base;
	      PUT_DIGIT(digit);
	    }
	    break;
	  }
	case 'p':			/* pointer */
	  {
	    cnv = 'x';
	    if (len == ' ')
	      len = 'p';

	    /* FALL THROUGH */
	  }
	case 'x':			/* unsigned hexadecimal, lowercase */
	case 'X':			/* unsigned hexadecimal, uppercase */
	  {
	    base = 16;
	    cnv = cnv - ('x' - 'a');
	    /* FALL THROUGH */
	  }
	case 'u':			/* unsigned int */
	  {
	    unsigned long long ull;

	    switch (len) {
	    case 'q':
	      ull = va_arg(ap, unsigned long long);
	      break;
	    case 'j':
	      ull = va_arg(ap, intmax_t);
	      break;
	    case 'z':
	      ull = va_arg(ap, size_t);
	      break;
	    case 't':
	      ull = va_arg(ap, ptrdiff_t);
	      break;
	    case 'p':
	      ull = (uintptr_t) va_arg(ap, void *);
	      break;
	    default:
	      ull = va_arg(ap, unsigned long); /* handles char, short too */
	      break;
	    }

	    if (ull == 0llu)
	      PUT_DIGIT(0);

	    while (ull) {
	      unsigned int digit = ull % base;
	      ull = ull / base;
	      PUT_DIGIT(digit);
	    }
	    break;
	  }
	case 'c':			/* character, long character */
	  // SUSv2 'C' not supported
	  {
	    char c = va_arg(ap, int);
	    *bufp++ = c;
	    break;
	  }
	case 's':			/* string, long string */
	  // SUSv2 'S' not supported
	  {
	    sptr = va_arg(ap, char *);
	    slen = strlen(sptr);
	    break;
	  }
	  // 'n' not supported
	case '%':
	  {
	    // FIX: What is supposed to be done about field widths?
	    oss << '%';
	    continue;
	  }
#if 0
	  /* Floating point formats not supported */
	case 'e':			/* double */
	case 'E':			/* double */
	case 'f':			/* double */
	case 'F':			/* double */
	case 'g':			/* double */
	case 'G':			/* double */
	case 'a':			/* double */
	case 'A':			/* double */
	  {
	    break;
	  }
#endif
	}

	if (bufp != buf)
	  slen = bufp - buf;

	// FIX: I am not sure if the sign should be considered part of
	// the field for field width purposes. I think that it is, but
	// this complicates the padding algorithm in the right-adjust
	// case:
	//
	//     Sign in field     Sign not in field
	//     xxxxxxxxxx         xxxxxxxxxxxx
	//     -        3        -           3
	//     -000000003        -000000000003
	//
	// My resolution here is to reduce the field width by one if a
	// sign is required, and then treat the sign as appearing
	// outside the field.

	if (needSign && width)
	  width--;

	if (needSign) {
	  oss << (isNegative ? '-' : posSign);
	}

	// If we are right adjusting, insert needed pad characters: */
	// FIX: Doesn't zero padding *require* left adjust logic?
	if (ladjust == false) {
	  while (slen < width) {
	    oss << padc;
	    slen++;
	  }
	}

	if (bufp != buf) {
	  do {
	    --bufp;
	    oss << *bufp;
	  } while (bufp != buf);
	}
	if (sptr)
	  while(*sptr)
	    oss << *sptr++;

	/* If we are left adjusting, pad things out to end of field */
	if (ladjust) {
	  while (slen < width) {
	    oss << padc;
	    slen++;
	  }
	}
      }
    }

    return oss.str();
  }

  std::string format(const char *fmt, ...)
  {
    va_list ap;

    va_start(ap, fmt);

    return vformat(fmt, ap);

    /* va_end(ap); */
  }

} /* namespace sherpa */
