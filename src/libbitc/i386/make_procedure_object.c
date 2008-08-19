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

void *currentClosureEnvPtr;

void *
bitc_emit_procedure_object(void *stubP, void *envP)
{
  /* mov CurrentClosurePointer <- $ProperClosureValue
     is realized on IA32 as:
     push $ProperClosureValue
     pop CurrentClosurePointer     */
  
  /* Not sure if C99 allows structure within structure definition */
  
  struct __ia32_nops {
    unsigned char op[3];
  }__attribute__((packed));
  struct __ia32_push {
    unsigned char op; 
    void *imm;
  }__attribute__((packed));
  struct __ia32_pop {
    unsigned char op;
    unsigned char modrm;
    void *addr;
  }__attribute__((packed));
  struct __ia32_jmp {
    unsigned char op;
    void *addr;
  }__attribute__((packed));
  
  typedef struct __cl_code __cl_code;  
  struct __cl_code {
    struct __ia32_nops nops; /* 3 bytes */
    struct __ia32_push push; /* 5 bytes */
    struct __ia32_pop  pop;  /* 6 bytes */
    struct __ia32_jmp  jmp;  /* 5 bytes */
    /* No Padding necessary */
  }__attribute__((packed));
  
  typedef void* __cl_ptr;
  
  typedef struct closure closure;  
  struct closure {
    __cl_code code;
    __cl_ptr  env;
  }__attribute__((packed));;
  
  struct closure* cl = GC_ALLOC(sizeof(closure));
  
  cl->code.nops.op[0] = 0x90;
  cl->code.nops.op[1] = 0x90;
  cl->code.nops.op[2] = 0x90;

  /* push $env :: 68 imm32*/
  cl->code.push.op = 0x68u;
  cl->code.push.imm = envP;

  /* pop $currentClosurePtr :: 8F /0 r/m32 */
  cl->code.pop.op = 0x8Fu;
  cl->code.pop.modrm = 0x05u; /* 00 000 101 */
  cl->code.pop.addr = &currentClosureEnvPtr;

  /* jmp $transP :: E9 rel32 */
  cl->code.jmp.op = 0xE9u;
  cl->code.jmp.addr = (void *)((unsigned long)stubP -
			       ((unsigned long)&(cl->code.jmp.addr) + 4));  
  cl->env = envP;
  return cl;
}
