#ifndef TYPEINFER_HXX
#define TYPEINFER_HXX

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

/// @brief Flags used by Type-inference engine.
///
/// They are internal flags for communication/processing within
/// the different Type inference procedures.
/// These flags are different from the Unifier's flags.
enum TI_FlagValues {
  TI_NO_FLAGS     = 0x00u,
  TI_TYP_EXP      = 0x01u,
  TI_TYP_APP      = 0x02u,
  TI_TCC_SUB      = 0x04u,
  TI_TYP_DEFN     = 0x08u, 

  /// @brief Passed downward from at_apply handler when the thing in
  /// the applicative position is a select node.
  TI_METHOD_OK    = 0x10u
};
typedef sherpa::EnumSet<TI_FlagValues> TI_Flags;


/* The following flags are Unification Flags. */

enum UnifyFlagValues {
  UFLG_NO_FLAGS            = 0x0,

  UFLG_UNIFY_STRICT        = 0x00001u, // Overrides everything else.
  UFLG_UNIFY_STRICT_TVAR   = 0x00002u, // No-alpha-renaming.
  UFLG_UNIFY_TRY           = 0x00004u, // Trial mode.
  UFLG_DEF_DECL_NO_MATCH   = 0x00008u,
  UFLG_TYP_NO_PRELUDE      = 0x00010u,
  // No more type classes beyond polyinstantiation
  UFLG_NO_MORE_TC          = 0x00020u,
  UFLG_UN_IGN_RIGIDITY     = 0x00040u,
  UFLG_ALL_INSTS_OK        = 0x00080u, // All instances are OK.

  // We have passed refization pass of Closure conversion. The
  // (temporary?) restriction that letrecs must define only define
  // lambdas must be prepared to take closures or refs to functions /
  // closures.
  UFLG_POST_REFIZE         = 0x00100u,

  UFLG_INF_REINIT          = 0x00200u,
  // Unifying the Var() part of an mbFull type. Here, the
  // propagateMutability() check must not be performed.
  UFLG_UN_MBFULL_VAR       = 0x00400u
};

typedef sherpa::EnumSet<UnifyFlagValues> UnifyFlags;

#endif /* TYPEINFER_HXX */
