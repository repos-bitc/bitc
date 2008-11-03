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
/// They are used for communication/processing within
/// the different Type inference procedures, and for driving the
/// inference engine under certain special conditions.

enum TI_FlagValues {
  TI_NO_FLAGS            = 0x000u,
  /// The following flags are used for internal communication within
  /// the type inference algorithm.

  /// Inference engine is now within a type AST 
  /// (ex: (fn int -> bool))
  TI_TYP_EXP             = 0x001u,
  /// Inference engine is within a type AST that is a type application 
  /// (ex: (pair int bool)
  TI_TYP_APP             = 0x002u,
  /// Inference engine is processing a subsumed type class
  /// (ex: Since Ord is a sub-class of Eq, the presence of Ord
  /// constraint subsumes the presence of another Eq Constraint)
  TI_TCC_SUB             = 0x004u,
  /// Processing a Type definition
  /// (ex: defstruct and defunion)
  TI_TYP_DEFN            = 0x008u,

  /// The following flags are used to drive the inference engine under
  /// specific modes/conditions.
  
  /// No longer need to check Declaration/Definition matching
  TI_DEF_DECL_NO_MATCH   = 0x010u,

  /// Don't import prelude
  TI_NO_PRELUDE          = 0x020u,

  /// No more type classes: The inference system will not add default
  /// constraints (ex: IntLit/FloatLit constraints on 
  /// Integer/Float literals). This mode is used beyond the 
  /// polyinstantiation pass.
  TI_NO_MORE_TC          = 0x040u,

  /// No longer need to check instance permissibility, since this
  /// check is expensive, and only needs to be performed once.
  TI_ALL_INSTS_OK        = 0x100u,
};
typedef sherpa::EnumSet<TI_FlagValues> TI_Flags;

/* The following flags are Unification Flags. */
enum UnifyFlagValues {
  UFLG_NO_FLAGS            = 0x00,
  UFLG_UNIFY_STRICT        = 0x01u, // Overrides everything else.
  UFLG_UNIFY_STRICT_TVAR   = 0x02u, // No-alpha-renaming.
  UFLG_UN_IGN_RIGIDITY     = 0x04u,
};

typedef sherpa::EnumSet<UnifyFlagValues> UnifyFlags;


#endif /* TYPEINFER_HXX */
