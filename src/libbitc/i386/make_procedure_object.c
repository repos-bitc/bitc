/**************************************************************************
 *
 * Copyright (C) 2008, The EROS Group, LLC
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

#include <gc/gc.h>
#include <inttypes.h>
#include "../BUILD/bitc-runtime.h"

void *
bitc_emit_procedure_object(void *stubP, void *envP)
{
  uint32_t envW = (uint32_t) envP;
  uint32_t stubW = (uint32_t) stubP;

  bitc_Procedure* proc = GC_ALLOC(sizeof(bitc_Procedure));

  proc->code[0] = 0x90;		/* padding */
  proc->code[1] = 0x90;
  proc->code[2] = 0x90;

  /* movl $envP,%eax */
  proc->code[3] = 0xb8u;	/* MOV imm,%eax */

  /* [4..7] will be filled in via env.ptr */

  /* jmp rel32 */
  stubW -= (uint32_t)&proc->code[13];

  proc->code[8] = 0xe9u;
  proc->code[9] = stubW;
  proc->code[10] = (stubW >> 8);
  proc->code[11] = (stubW >> 16);
  proc->code[12] = (stubW >> 24);

  proc->env.ptr = envP;

  return proc;

  /* On this architecture, no explicit iCache flush is required as
   * long as a branch appears. Return suffices for a branch, which is why
   * this must not be an inline procedure. */
}

