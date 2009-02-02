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

#define BEG_SIMP_DEBUG    DEBUG_OFF
#define METH_DECL_DEBUG   DEBUG_OFF
#define ILH_DEBUG         DEBUG_OFF   // Inst-Lambda-Hoist
#define INST_DEBUG        DEBUG_ON   // Debug Polyinstantiation
#define INST_ENV_DEBUG    DEBUG_ON   // Debug Polyinstantiator's env-handling
#define REPR_SIMP_DEBUG   DEBUG_OFF
#define UNION_INF_DEBUG   DEBUG_OFF
#define DEF_INF_DEBUG     DEBUG_OFF
#define TI_TOP_DEBUG      DEBUG_OFF   // Inference Top-Loop
#define TI_UNITWISE       DEBUG_OFF   // Print Inferred interface/module info
#define TI_AST_DEBUG      DEBUG_OFF   // Inference AST-wise output
#define CLCONV_DEBUG      DEBUG_OFF
#define GEN_DEBUG_TL      DEBUG_OFF   // Treat all generalization as local
#define GEN_DEBUG         DEBUG_OFF   // Debug Type Generalization
#define INS_DEBUG         DEBUG_OFF   // Debug Type Instantiation
#define ID_INS_DEBUG      DEBUG_OFF   // Debug Instantiation at at_ident
#define SOL_DEBUG         DEBUG_OFF   // Debug Constraint Solver
#define SPSOL_DEBUG       DEBUG_OFF   // Debug Solving Special constaints
#define PCST_DEBUG        DEBUG_OFF   // Debug Solving Polymorhic * constaints
#define TCSOL_DEBUG       DEBUG_OFF   // Debug Solving Type-class constaints
#define UNIFY_DEBUG       DEBUG_OFF   // Debug Unification
#define UNF_RES_DEBUG     DEBUG_OFF  // Debug Unification by showing Results
#define TRAIL_DEBUG       DEBUG_OFF   // Debug Type Linking
#define TS_NORM_DEBUG     DEBUG_OFF   // Debug TypeScheme Normalization
#define TYPE_ACC_DEBUG    DEBUG_OFF
#define DEF_DECL_DEBUG    DEBUG_OFF   // Debug Definition-Declaration consistency checking

#endif /* DEBUG_HXX */
