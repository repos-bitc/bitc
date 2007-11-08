#include <gc/gc.h>
#include <inttypes.h>
#include <BUILD/bitc-runtime.h>

void *
bitc_make_closure(void *closureP, void *procP)
{
  uint32_t closureW = (uint32_t) closureP;
  uint32_t procW = (uint32_t) procP;

  char *clo = GC_MALLOC(17);

  clo[0] = 0xff;		/* PUSH (%esp) */
  clo[1] = 0x34;
  clo[2] = 0x24;

  clo[3] = 0x90;		/* NOP */

  clo[4] = 0xc7;		/* MOVL imm32,0x4(%esp) */
  clo[5] = 0x44;
  clo[6] = 0x24;
  clo[7] = 0x04;

  clo[8] = closureW & 0xffu;
  clo[9] = (closureW >> 8) & 0xffu;
  clo[10] = (closureW >> 16) & 0xffu;
  clo[11] = (closureW >> 24) & 0xffu;

  /* Pentium jump is an offset relative to the NEXT instruction: */
  uint32_t wnext = (uint32_t) &clo[17];
  procW -= wnext;

  clo[12] = 0xe9;		/* jmp imm32 */

  clo[13] = procW & 0xffu;
  clo[14] = (procW >> 8) & 0xffu;
  clo[15] = (procW >> 16) & 0xffu;
  clo[16] = (procW >> 24) & 0xffu;

  return clo;
}
