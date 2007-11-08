#include <unistd.h>

#include <BUILD/bitc-runtime.h>

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

/* (proclaim getTimeStamp: (fn () uint64) external bitc_stdlib_rdtsc) */
bitc_uns64_t
bitc_stdlib_rdtsc()
{
  unsigned long temp1=0;
  unsigned long temp2=0;  
  unsigned long long ts=0;
  asm volatile("rdtsc\t\n"
	       "movl %%eax, %0\t\n"
	       "movl %%edx, %1\t\n"
	       : "=m" (temp1), "=m" (temp2)
	       : 
	       : "%eax", "%edx");
  
  ts = (((unsigned long long)temp2) << 32) + temp1;
  return ts;
}
