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
/// <b>PowerPC</b> The PowerPC implementation uses a position-independent
/// variant of the Procedure-Object-Relative method, storing the
/// closure record pointer into <code>@r0</code>, which is defined as
/// call-clobbered on this architecture. r2, which is defined as available
/// on Mach-O systems, is also clobbered by this sequence.
///
/// The code sequence is slightly tricky. It takes advantage of the
/// fact that the branch and link instruction (<code>BL</code>)
/// writes the address of the instruction following the <code>BL</code>
/// instruction into the link register, and uses BL
/// to collect the address at which the procedure object is
/// executing. It then exploits the delay slot of the
/// <code>BL</code> to implement the actual load of the closure
/// pointer.
///
/// Thanks to Paul Snively for assistance in defining and debugging
/// this sequence.

void *
bitc_emit_procedure_object(void *stubP, void *envP)
{
  bitc_Procedure* proc = GC_ALLOC(sizeof(bitc_Procedure));

  /* mflr r0 */
  proc->code[0] = 0x7c0802a6u;
  /* bl .skip */
  proc->code[1] = 0x4800000du;
  /* mflr r2 */
  proc->code[4] = 0x7c4802a6u;
  /* mtlr r0 */
  proc->code[5] = 0x7c0803a6u;
  /* lwz r0, 0(r2) */
  proc->code[6] = 0x80020000u;
  /* mtctr r0 */
  proc->code[7] = 0x7c0903a6u;
  /* lwz r2,4(r2) */
  proc->code[8] = 0x80420004u;
  /* bctr */
  proc->code[9] = 0x4e800420u;

  proc->code[2] = stubP;
  proc->code[3] = envP;

  return proc;

  /* On this architecture, no explicit iCache flush is required as
   * long as a branch appears. Return suffices for a branch, which is why
   * this must not be an inline procedure. */
}
