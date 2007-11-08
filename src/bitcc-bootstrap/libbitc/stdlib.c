#include <unistd.h>

#include <BUILD/bitc-runtime.h>

/* NAME MANGLINGS */
#define atoi32 _16bitc_DTstdlib_atoi

bitc_int32_t
atoi32(bitc_string_t *str)
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
