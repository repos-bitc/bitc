#ifndef SYMTAB_HXX
#define SYMTAB_HXX

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

#include <libsherpa/UExcept.hxx>
#include <libsherpa/avl.hxx>
#include "AST.hxx"
#include "Environment.hxx"


/* Definitions of flags used in Symbol Resolution */
#define NO_CHK_USE_TYPE     0x000001u
#define INCOMPLETE_OK       0x000002u
#define NEW_TV_OK           0x000004u
#define IS_ESCAPING         0x000008u
#define IS_MODULE           0x000010u
#define IS_INTERFACE        0x000020u
#define BIND_PUBLIC         0x000040u
#define USE_ONLY_PUBLIC     0x000080u

#define RESOLVE_APPLY_MODE  0x000200u
#define RES_APP_PAT_MODE    0x000400u
#define INCOMPLETE_OK_PROC  0x000800u
#define NO_RESOLVE_DECL     0x001000u
#define WITHIN_DEFUNION     0x002000u
#define SYM_NO_PRELUDE      0x004000u
#define SWITCHED_ID_OK      0x008000u
#define WITHIN_CATCH        0x010000u
#define WITHIN_CATCH_MC     0x020000u // Catch block with 
                                     // multiple exceptions
#define SYM_REINIT          0x040000u
#define SYM_POST_POLY       0x080000u // Resolution *during* 
                                     // (and post) polyinstantiation
                                     // ignore tvar-scoping.
#define RESOLVE_INREF_MODE  0x100000u

#endif /* SYMTAB_HXX */
