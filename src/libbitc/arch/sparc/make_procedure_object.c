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
#include "BUILD/bitc-runtime.h"

/// @addtogroup ProcObjects
///
/// <b>SPARC</b> The SPARC implementation uses a position-independent
/// variant of the Procedure-Object-Relative method, storing the
/// closure record pointer into <code>@%g1</code>, which is defined as
/// call-clobbered on this architecture. No other register is touched
/// by this sequence.
///
/// The code sequence is slightly tricky. It takes advantage of the
/// fact that the jump and link instruction (<code>JMPL</code>) can
/// write the address of the <code>JMPL</code> instruction into a
/// designated register (in this case <code>@%g1</code>, and uses JMPL
/// to collect the address at which the procedure object is
/// executing. It then exploits the delay slot of the
/// <code>JMPL</code> to implement the actual load of the closure
/// pointer.
///
/// On its face this sequence should not be efficient, since it
/// involves <em>both</em> a PC store and a D-space load. In practice
/// it is the canonical sequence used for PC-relative load on this
/// architecture, and is therefore a specifically optimized case in
/// SPARC hardware implementations.
///
/// Thanks to Jonathan Adams for assistance in defining and debugging
/// this sequence.

void *
bitc_emit_procedure_object(void *stubP, void *envP)
{
  uint32_t envW = (uint32_t) envP;
  uint32_t stubW = (uint32_t) stubP;

  bitc_Procedure* proc = GC_ALLOC(sizeof(bitc_Procedure));

  /* SETHI %hi(stubW),%g1 */
  proc->code[0] = 0x03000000u | (stubW >> 10);
  /* JMPL %g1 + %lo(stubW), %g1 */
  proc->code[1] = 0x83c06000 | (stubW & 0x3ffu);
  /* LD   [%g1 + 0x8], %g1 */
  proc->code[2] = 0xc2006008;

  proc->env.ptr = envP;

  return proc;

  /* On this architecture, no explicit iCache flush is required as
   * long as a branch appears. Return suffices for a branch, which is why
   * this must not be an inline procedure. */
}

