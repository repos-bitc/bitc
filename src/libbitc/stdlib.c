#include <unistd.h>

#include "BUILD/bitc-runtime.h"

/// @brief Return an integer from the leading characters of a string.
///
/// atoi() converts a string consisting of an optional leading "-"
/// followed by ASCII digits into a 32-bit integer. Conversion halts
/// at the first non-digit character in the input string.

/* (proclaim atoi: (fn (string) int32) external bitc_stdlib_atoi) */
bitc_int32_t
bitc_stdlib_atoi(bitc_string_t *str)
{
  bitc_int32_t sign = 1;
  bitc_int32_t val = 0;
  bitc_int32_t base = 10;
  size_t i = 0;

  if(str->s[i] == '-') {
    i++;
    sign = -1;
  }

  /* Deal with optional leading radix: */
  if (str->s[i] == 0) {
    i++;

    switch (str->s[i]) {
    case 'x':
    case 'X':
      {
	base = 16;
	i++;
	break;
      }
    case 'b':
    case 'B':
      {
	// BitC library extension to handle base 2 using 0b:
	base = 2;
	i++;
	break;
      }
    default:
      // In absence of qualifier it must be base 8 (octal):
      base = 8;
      break;
    }
  }

  for ( ; i < str->length; i++) {
    // Note: We are dealing with a UTF8-encoded string.  All legal
    // ASCII digits, including hexadecimal digits, fall within the
    // leading 128 code points.  Any code point whose encoding
    // requires more than 8 bits will have a prefix marker that is not
    // a digit, and we will abandon the loop in that case. In light of
    // which, it is okay here to use an 8-bit C character to hold the
    // code point, and it is okay NOT to use the utf_decode() code
    // point extraction functions.
    unsigned char c = str->s[i];
    bitc_int32_t digit = (c - '0'); // presumptively

    if (c < '0')
      break;

    if ('0' <= c && c <= '9') {
      digit = (c - '0');
      if (digit >= base)
	break;			// out of range
    }
    else if (base == 16) {
      if ('a' <= c && c <= 'f')
	digit = (c - 'a' + 10);
      else if ('A' <= c && c <= 'F')
	digit = (c - 'A' + 10);
      else 
	break;			// out of range
    }
    else
      break;			// out of range

    val = val * base + digit;
  }

  return (val * sign);
}

#if defined(__i386__)
/// @brief Read the IA32 time stamp counter.
///
/// @bug This exists to provide support for Swaroop's benchmarking. It
/// should be moved to an IA32 architecture-specific subdirectory.

/* (proclaim getTimeStamp: (fn () uint64) external bitc_stdlib_rdtsc) */
bitc_uns64_t
bitc_stdlib_getTimeStamp()
{
  unsigned long temp1=0;
  unsigned long temp2=0;  
  unsigned long long ts=0;
  __asm__ volatile("rdtsc\t\n"
	       "movl %%eax, %0\t\n"
	       "movl %%edx, %1\t\n"
	       : "=m" (temp1), "=m" (temp2)
	       : 
	       : "%eax", "%edx");
  
  ts = (((unsigned long long)temp2) << 32) + temp1;
  return ts;
}
#else
bitc_uns64_t
bitc_stdlib_getTimeStamp()
{
  return 0;
}
#endif
