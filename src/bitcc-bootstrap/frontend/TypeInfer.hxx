#ifndef TYPEINFER_HXX
#define TYPEINFER_HXX

/**************************************************************************
 *
 * Copyright (C) 2006, Johns Hopkins University.
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

/* The following flags are Type-Inference / Unification Flags.
   The ones defined in Type.hxx are internal flags for
   communication/processing within Type-methods annd inference
   routines. */

#define UNIFY_STRICT        0x00001u // Overrides ecerything else.
#define UNIFY_STRICT_TVAR   0x00002u // No-alpha-renaming.
#define UNIFY_TRY           0x00004u // Trial mode.
#define DEF_DECL_NO_MATCH   0x00008u
#define TYP_NO_PRELUDE      0x00010u
#define NO_MORE_TC          0x00020u // No more type classes 
                                     // beyond polyinstantiation
#define UN_IGN_RIGIDITY     0x00040u
#define ALL_INSTS_OK        0x00080u // All instances are OK.

#define POST_REFIZE         0x00200u // We have passedrefization pass  
        // of Closure conversion. The (temporary?) restriction that
        // letrecs must define only define lambdas must be prepared to
        // take closures or refs to functions / closures.

#define INF_REINIT          0x00400u 

#endif /* TYPEINFER_HXX */
