#ifndef SPECIAL_HXX
#define SPECIAL_HXX

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

#include <string>
#include <libsherpa/LToken.hxx>
#include <libsherpa/CVector.hxx>

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "UocInfo.hxx"

/* Structure that isused by the specializer */
struct spStruct {  
  GCPtr<AST> ast; 
  GCPtr<Type> typ;
  bool lifted;

  spStruct(GCPtr<AST> _ast, GCPtr<Type> _typ)
  {
    typ = _typ;
    ast = _ast;
    lifted = false;
  }
};
 
/* Structure to hold the names of certain Special ASTs that are
   mostly part of the prelude. They are needed by the
   type inference engine. They are stored here because will
   otherwise become unidentifiable after the name-mangling phase. */

/* I am not superimpressed with this structure, Once a better method
   is obtained, THIS SRUCTURE IS VERY LIKELY TO GO AWAY */

#define SP_NAME_INTEGER          0x02u
#define SP_NAME_FP               0x04u
#define SP_NAME_IOB              0x08u
#define SP_NAME_LT               0x10u
#define SP_NAME_REF_TYPES        0x20u
#define SP_NAME_COPY_COMPAT      0x40u

struct SpecialNames {
  std::string sp_integral;
  std::string sp_fp;
  std::string sp_iob;
  std::string sp_lt;
  std::string sp_ref_types;
  std::string sp_copy_compat;

  SpecialNames();
  std::string getSpName(unsigned name);
  void fixUpSpNames(GCPtr<UocInfo> prelude);
  static SpecialNames spNames;
};

#endif /* SPECIAL_HXX */