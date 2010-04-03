#ifndef DEBUG_HXX
#define DEBUG_HXX

/**************************************************************************
 *
 * Copyright (C) 2008, Johns Hopkins University.
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

#include <assert.h>

#define DEBUG_ON  if (1)
#define DEBUG_OFF if (0)
#define DEBUG_VERBOSE true

#define dbg_all           ~0u

#define dbg_BEG_SIMP    0x00000001u
#define dbg_METH_DECL   0x00000002u
#define dbg_ILH         0x00000004u        // Inst-Lambda-Hoist
#define dbg_INST        0x00000008u        // Debug Polyinstantiation
#define dbg_INST_ENV    0x00000010u // Debug Polyinstantiator's env-handling
#define dbg_REPR_SIMP   0x00000020u
#define dbg_UNION_INF   0x00000040u
#define dbg_DEF_INF     0x00000080u
#define dbg_TI_TOP      0x00000100u   // Inference Top-Loop
#define dbg_TI_UNITWISE 0x00000200u   // Print Inferred interface/module info
#define dbg_TI_AST      0x00000400u   // Inference AST-wise output
#define dbg_CLCONV      0x00000800u
#define dbg_GEN_TL      0x00001000u   // Treat all generalization as local
#define dbg_GEN         0x00002000u   // Debug Type Generalization
#define dbg_INS         0x00004000u   // Debug Type Instantiation
#define dbg_ID_INS      0x00008000u   // Debug Instantiation at at_ident
#define dbg_SOL         0x00010000u   // Debug Constraint Solver
#define dbg_SPSOL       0x00020000u   // Debug Solving Special constaints
#define dbg_PCST        0x00040000u   // Debug Solving Polymorhic * constaints
#define dbg_TCSOL       0x00080000u   // Debug Solving Type-class constaints
#define dbg_UNIFY       0x00100000u   // Debug Unification
#define dbg_UNF_RES     0x00200000u  // Debug Unification by showing Results
#define dbg_TRAIL       0x00400000u   // Debug Type Linking
#define dbg_TS_NORM     0x00800000u   // Debug TypeScheme Normalization
#define dbg_TYPE_ACC    0x01000000u
#define dbg_DEF_DECL    0x02000000u   // Debug Definition-Declaration consistency checking

#define dbg_flags         (0u)

#define DEBUG_CND(x) ((dbg_flags) & (dbg_ ## x))
#define DEBUG(x) if (DEBUG_CND(x))

#endif /* DEBUG_HXX */
