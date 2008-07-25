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

#include <assert.h>

#include <string>
#include "Special.hxx"

using namespace sherpa;

SpecialNames SpecialNames::spNames;

SpecialNames::SpecialNames()
{
  sp_integral = "IntLit";
  sp_fp = "FloatLit";
  sp_iob = "IndexBoundsError";
  sp_lt = "__index_lt";
  sp_ref_types = "ref-types";
  sp_copy_compat = "copy-compat";
}

std::string
SpecialNames::getSpName(unsigned name) {
  switch(name) {
    
  case SP_NAME_INTEGER:
    return sp_integral;
    
  case SP_NAME_FP:
      return sp_fp;
      
  case SP_NAME_IOB:
    return sp_iob;

  case SP_NAME_LT:
    return sp_lt;

  case SP_NAME_REF_TYPES:
    return sp_ref_types;

  case SP_NAME_COPY_COMPAT:
    return sp_copy_compat;
  
  default:
    assert(false);
    return "unknown";
  }    
}

// In front-end passes, the passed UOC will be the prelude. In the
// backend we will be called with the UOC of the bigAST
void 
SpecialNames::fixUpSpNames(GCPtr<UocInfo> theUoc)
{
  sp_integral = theUoc->env->getBinding(sp_integral)->s;
  sp_fp = theUoc->env->getBinding(sp_fp)->s;
  sp_iob = theUoc->env->getBinding(sp_iob)->s;
  sp_lt = theUoc->env->getBinding(sp_lt)->s;
  sp_ref_types = theUoc->env->getBinding(sp_ref_types)->s;
  sp_copy_compat = theUoc->env->getBinding(sp_copy_compat)->s;
}
