/**************************************************************************
 *
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, The EROS
 *   Group, LLC. 
 * Copyright (C) 2004, 2005, 2006, Johns Hopkins University.
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
#include <string>
#include "UExcept.hxx"

namespace sherpa {

  UExceptionType::UExceptionType(const std::string& s)
    : name(s)
  {
  }

  UExceptionType::~UExceptionType()
  {
  }

  UExceptionBase::~UExceptionBase()
  {
  }

  UExceptionBase::UExceptionBase(const UExceptionType *et,
				 const char *file, int line, 
				 const std::string& msg)
  {
    this->et = et;
    this->msg = msg;
    this->file = file;
    this->line = line;
  }
  UExceptionBase::UExceptionBase(const UExceptionBase& cme)
  {
    this->et = cme.et;
    this->msg = cme.msg;
    this->file = cme.file;
    this->line = cme.line;
  }

#define DEF_UEXCEPT(nm) UExceptionType nm(#nm)

  namespace excpt {
    DEF_UEXCEPT(Assert);
    DEF_UEXCEPT(OutOfMemory);
    DEF_UEXCEPT(IntegrityFail);
    DEF_UEXCEPT(Malformed);
    DEF_UEXCEPT(BadValue);
    DEF_UEXCEPT(NullArg);
    DEF_UEXCEPT(NoAccess);
    DEF_UEXCEPT(NoObject);
    DEF_UEXCEPT(ObjectExists);
    DEF_UEXCEPT(Truncated);
    DEF_UEXCEPT(Overrun);
    DEF_UEXCEPT(Subprocess);
    DEF_UEXCEPT(LockFail);
    DEF_UEXCEPT(LostLock);
    DEF_UEXCEPT(UniverseDied);
    DEF_UEXCEPT(NoConnect);
    DEF_UEXCEPT(NoAuth);
    DEF_UEXCEPT(Environ);
    DEF_UEXCEPT(VersionError);
    DEF_UEXCEPT(ConnLost);
    DEF_UEXCEPT(IoError);
    DEF_UEXCEPT(BadOpcode);
    DEF_UEXCEPT(PrngError);
    DEF_UEXCEPT(BoundsError);
    DEF_UEXCEPT(Unspecified);
  };

} /* namespace sherpa */
