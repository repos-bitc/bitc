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

#include <stdint.h>
#include <dirent.h>

#include <iostream>
#include <string>
#include "backend.hxx"
#include "UocInfo.hxx"

/** @brief Targets known to the compiler. */
BackEnd BackEnd::backends[] = {
  // The first one is the default.
  { "exe", pn_npass, op_ssaTrans, 0, 0, EmitExe, BK_LINKING },
  { "c", pn_npass, op_ssaTrans, 0, 0, EmitC, 0 },
  { "h", pn_npass, op_ssaTrans, 0, 0, EmitHeader, BK_HDR_MODE },
  { "bito", pn_npass, op_none, 0, EmitBitO, 0, BK_UOC_MODE },
  { "xmldump", pn_parse, op_none, XMLdump, 0, 0, 0 },
  { "xmlpp", pn_parse, op_none, XMLpp, 0, 0, 0 },
  { "xmltypes", pn_typeCheck, op_none, XMLtypesPP, 0, 0, 0 }
};

size_t BackEnd::nBackEnd = sizeof(BackEnd::backends) / sizeof(BackEnd);

