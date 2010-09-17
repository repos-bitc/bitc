#ifndef SYMTAB_HXX
#define SYMTAB_HXX

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

/* Definitions of flags used in Symbol Resolution */
enum ResolverFlagValues {
  RSLV_NO_FLAGS                 = 0,
  
  /// Don't check validity of identType in at_ident case. This is used
  /// when the resolver does not have enough contextual information to
  /// expect the correct identType (ex: for at_select)
  RSLV_NO_CHK_USE_TYPE          = 0x0001u,

  /// Use of an incomplete identifier in this context is okay (ex:
  /// within a lambda abstraction
  RSLV_INCOMPLETE_OK            = 0x0002u,

  /// No longer need to perform incompleteness checking
  /// The closure-conversion pass introduces some carefully crafted
  /// code using internal constructors that appear incomplete the
  /// following restrictions. Since there cannot be any incompleteness
  /// errors at that point, incompleteness checking is disabled.
  RSLV_INCOMPLETE_NO_CHK        = 0x0004u,
  
  /// Use of new type variables is okay in this context. This is true
  /// in the context of expressions, where annotation with new
  /// variables is permitted. But, within structure/union definitions,
  /// onlt variables bound in the parameter list should be used.
  RSLV_NEW_TV_OK                = 0x0008u,

  /// We are processing an interface unit of compilation
  RSLV_IS_INTERFACE             = 0x0010u,

  /// A name binding is public
  RSLV_BIND_PUBLIC              = 0x0020u,

  /// Only public bindings must be used for name resolution.
  RSLV_USE_ONLY_PUBLIC          = 0x0040u,

  /// Don't warn about unused proclaimations in source module;
  /// disabled in the unified UOC past polyinstantiation.
  RSLV_NO_RESOLVE_DECL          = 0x0080u,

  /// Currently processing a defunion (used to check that tagtype
  /// declarations onty occur on defunions.
  RSLV_WITHIN_DEFUNION          = 0x0100u,

  /// Dont' import prelude.
  RSLV_SYM_NO_PRELUDE           = 0x0200u,

  /// Used to mark legal contexts (only on the left of a select) where
  /// a identifier that represents a deconstructed (matched) value in
  /// a switch statement can be used.
  RSLV_SWITCHED_ID_OK           = 0x0400u,
  
  /// Processing a catch block
  RSLV_WITHIN_CATCH             = 0x0800u,

  /// Processing a Catch block with multiple exceptions
  RSLV_WITHIN_CATCH_MC          = 0x1000u,
};

typedef sherpa::EnumSet<ResolverFlagValues> ResolverFlags;

#endif /* SYMTAB_HXX */
