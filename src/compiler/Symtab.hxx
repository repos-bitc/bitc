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

  RSLV_NO_CHK_USE_TYPE          = 0x000001u,
  RSLV_INCOMPLETE_OK            = 0x000002u,
  RSLV_NEW_TV_OK                = 0x000004u,
  RSLV_IS_ESCAPING              = 0x000008u,
  RSLV_IS_MODULE                = 0x000010u,
  RSLV_IS_INTERFACE             = 0x000020u,
  RSLV_BIND_PUBLIC              = 0x000040u,
  RSLV_USE_ONLY_PUBLIC          = 0x000080u,

  RSLV_RESOLVE_APPLY_MODE       = 0x000200u,
  RSLV_RES_APP_PAT_MODE         = 0x000400u,
  RSLV_INCOMPLETE_NO_CHK        = 0x000800u,
  RSLV_NO_RESOLVE_DECL          = 0x001000u,
  RSLV_WITHIN_DEFUNION          = 0x002000u,
  RSLV_SYM_NO_PRELUDE           = 0x004000u,
  RSLV_SWITCHED_ID_OK           = 0x008000u,
  RSLV_WITHIN_CATCH             = 0x010000u,
  // Catch block with multiple exceptions
  RSLV_WITHIN_CATCH_MC     = 0x020000u,
  RSLV_SYM_REINIT               = 0x040000u,
  // Resolution *during* (and post) polyinstantiation ignore
  // tvar-scoping.
  RSLV_SYM_POST_POLY            = 0x080000u,
  RSLV_RESOLVE_INREF_MODE       = 0x100000u
};

typedef sherpa::EnumSet<ResolverFlagValues> ResolverFlags;

#endif /* SYMTAB_HXX */
