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
#include <stdarg.h>
#include <assert.h>

#include <string>

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

#include "util.hxx"

namespace sherpa {
  std::string
  strdowncase(const std::string& s)
  {
    std::string new_s = s;

    for (size_t i = 0; i < new_s.size(); i++) {
      if (isalpha(new_s[i]))
	new_s.replace(i, 1, 1, tolower(new_s[i]));
    }

    return new_s;
  }

  /* Before this used sprintf. I grepped around, and found that this function
     was only called _ONCE_, in Browse.c

     Initially, I changed Browse.c to use xunsigned64_str, and removed
     xunsigned_str all together. Then I realized people (Shap+JV) might get mad
     that this function wasn't around any more, blah blah blah, so I put it back.

     In theory this is less efficient than sprintf, but all in all it doesn't
     really matter. It's not called (at all), and using xunsigned*_str implies
     doing some form of I/O (always very expensive).

     JL (9/12/02)
  */
  std::string unsigned_str(uint32_t n)
  {
    return unsigned64_str(n);
  }

  /* This helper routine is here because the logging logic should not
   * rely on the availability of mallocable memory. It should not be
   * called by anyone else! */
#define BUFSZ 20 /* 19 for digits + 1 for null */
 
  static char *
  unsigned64_to_buffer(uint64_t ul, char *buf)
  {
    char *p;

    buf[BUFSZ-1] = 0;
    p = &buf[BUFSZ-1];

    if (ul == 0)
      *(--p) = '0';

    while(ul) {
      *(--p) = '0' + (ul % 10);
      ul = ul / 10;
    }

    return p;
  }

  /* bc says that 2^64-1==18446744073709551615, which looks to me like
     19 characters. */
  std::string
  unsigned64_str(uint64_t ul)
  {
    char buf[BUFSZ];
    char *p = unsigned64_to_buffer(ul, buf);

    return p;
  }

  uint64_t
  atollu(const char *s)
  {
    uint64_t ull = 0;

    for ( ; *s; s++) {
      ull *= 10;
      ull += (*s - '0');
    }

    return ull;
  }

  uint64_t
  atollu(const std::string& s)
  {
    uint64_t ull = 0;

    for (size_t i = 0; i < s.size(); i++) {
      ull *= 10;
      ull += (s[i] - '0');
    }

    return ull;
  }

#if 0
  FILE *xprintf_outfile = 0;

  /* The only reason this exists is because I don't want to have to
   * bother being careful about printing null strings. Some
   * implementations of printf() behave gracelessly about that.
   */

#define xputchar(c)  putc(c, xprintf_outfile)

  /* This is derived from the EROS kprintf code */
  size_t
  xprintf(const char *fmt, ...)
  {
    unsigned long len;
    unsigned long width = 0;
    bool sign;
    bool rightAdjust;
    char fillchar;
    char buf[20];
    char *p, *pend;
    size_t output_count = 0;
  
    va_list ap;

    if (xprintf_outfile == 0)
      xprintf_outfile = stdout;

    va_start(ap, fmt);
    
    for( ; *fmt; fmt++) {
      if (*fmt != '%') {
	xputchar(*fmt);
	output_count ++;
	continue;
      }

      /* largest thing we might convert fits in 20 digits (unsigned long
       * long as decimal */
    
      pend = &buf[20];
      p = pend;

      fmt++;			/* now looking at specification */

      /* check for left adjust.. */
      rightAdjust = true;
      if (*fmt == '-') {
	rightAdjust = false;
	fmt++;
      }
      
      fillchar = ' ';
    
      /* we just saw a format character.  See if what follows
       * is a width specifier:
       */
      width = 0;

      if (*fmt == '0')
	fillchar = '0';

      while (*fmt && *fmt >= '0' && *fmt <= '9') {
	width *= 10;
	width += (*fmt - '0');
	fmt++;
      }
    
      assert (*fmt);		/* check bogus fmt */

      sign = false;
    
      switch (*fmt) {
      case '%':
	{
	  xputchar(*fmt);
	  output_count ++;
	  break;
	}
      case 's':
	{
	  p = pend = va_arg(ap, char *);
      
	  if (pend == 0)
	    p = pend = "<null>";

	  while (*pend)
	    pend++;
	  
	  break;
	}
      case 'c':
	{
	  long c;
	  c = va_arg(ap, long);
	  *(--p) = (char) c;
	  break;
	}	    
      case 'd':
	{
	  long l;
	  unsigned long ul;

	  l = va_arg(ap, long);
	      
	  if (l == 0) {
	    *(--p) = '0';
	  }
	  else {
	    if (l < 0)
	      sign = '-';

	    ul = (l < 0) ? (unsigned) -l : (unsigned) l;

	    if (l == LONG_MIN)
	      ul = ((unsigned long) LONG_MAX) + 1ul;

	    while(ul) {
	      *(--p) = '0' + (ul % 10);
	      ul = ul / 10;
	    }
	  }
	  break;
	}
      case 'u':
	{
	  unsigned long ul;

	  ul = va_arg(ap, unsigned long);
	      
	  if (ul == 0) {
	    *(--p) = '0';
	  }
	  else {
	    while(ul) {
	      *(--p) = '0' + (ul % 10);
	      ul = ul / 10;
	    }
	  }
	  break;
	}
      case 'x':
	{
	  unsigned long ul;
	  static char *hex_digits = "0123456789abcdef";

	  ul = va_arg(ap, unsigned long);
	      
	  if (ul == 0) {
	    *(--p) = '0';
	  }
	  else {
	    while(ul) {
	      *(--p) = hex_digits[ul % 16];
	      ul = ul / 16;
	    }
	  }
	  break;
	}
      default:
	{
	  assert(false);
	}
      }
  
      len = pend - p;
      if (sign)
	len++;

      /* do padding with initial spaces for right justification: */
      if (width && rightAdjust && len < width) {
	while (len < width) {
	  xputchar(fillchar);
	  output_count ++;
	  width--;
	}
      }

      if (sign)
	xputchar(sign);

      /* output the text */
      while (p != pend) {
	xputchar(*p++);
	output_count ++;
      }
    
      /* do padding with trailing spaces for left justification: */
      if (width && rightAdjust == false && len < width) {
	while (len < width) {
	  xputchar(fillchar);
	  output_count ++;
	  width--;
	}
      }
    }
    
    va_end(ap);

    return output_count;
  }
#endif

  /* This is _exactly_ like bsearch, except that if base == NULL, simply return
     NULL. Normally this should work anyway, but it seems Solaris has 'issues'
     with this -- thus, this wrapper function.
  */
  void* 
  xbsearch(const void *key, const void *base, size_t nmemb,
	   size_t size, int (*compar)(const void *, const void *))
  {
    /* FIX: Should we check for base==NULL and nmemb > 0? */
    if(base == NULL)
      return NULL;
    return bsearch(key, base, nmemb, size, compar);
  }

  std::string
  sgetenv(const char *var)
  {
    std::string s;

    const char *result = getenv(var);

    if (result)
      s = result;

    return s;
  }

} /* namespace sherpa */
