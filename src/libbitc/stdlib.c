#include <unistd.h>

#include "BUILD/bitc-runtime.h"

/// @brief Return an integer from the leading characters of a string.
///
/// @bug Why isn't this implemented as BitC code?

/* (proclaim atoi: (fn (string) int32) external bitc_stdlib_atoi) */
bitc_int32_t
bitc_stdlib_atoi(bitc_string_t *str)
{
  bitc_int32_t val = 0;
  bitc_int32_t ndx = 1;
  bitc_int32_t base = 10;
  int i;
  int limit;

  if(str->s[0] == '-')
    limit = 1;
  else
    limit = 0;

  for(i = str->length-1; i >= limit; i--) {
    val += (str->s[i] - '0') * ndx;
    ndx *= base;
    //printf("i = %d, ndx = %d, Val = %d\n", i, ndx, val);
  }

  if(limit == 1)
    val = -val;
  
  return val;
}

/// @brief Read the IA32 time stamp counter.
///
/// @bug This exists to provide support for Swaroop's benchmarking. It
/// should be moved to an IA32 architecture-specific subdirectory.

/* (proclaim getTimeStamp: (fn () uint64) external bitc_stdlib_rdtsc) */
bitc_uns64_t
bitc_stdlib_rdtsc()
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
