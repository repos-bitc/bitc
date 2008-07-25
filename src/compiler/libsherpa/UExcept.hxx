#ifndef LIBSHERPA_UEXCEPT_H
#define LIBSHERPA_UEXCEPT_H

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

#include "format.hxx"

namespace sherpa {

  /// @brief Class used to declare an exception instance.
  ///
  /// All UException objects should be globally declared, and should
  /// probably be declared as constants. The <b>address</b> of the
  /// UException object is used as a template parameter so that catch
  /// blocks can be created for different exception types.
  struct UExceptionType {
    const std::string name;
    UExceptionType(const std::string& s);
    ~UExceptionType();
  };

  class UExceptionBase {
  public:
    const UExceptionType *et;
    std::string msg;
    const char *file;
    uint32_t line;

    UExceptionBase(const UExceptionType *et, const char *file, int line, 
		   const std::string& msg);
    UExceptionBase(const UExceptionBase& cme);
    virtual ~UExceptionBase();
  };


  /// @brief Exception type template.
  ///
  /// The UException template exists purely to allow us to differentiate 
  /// exception type codes.
  template<UExceptionType *eType>
  class UException : public UExceptionBase {
  public:
    UException(const char *file, int line, const std::string& msg)
      : UExceptionBase(eType, file, line, msg)
    {
    }
    UException(const UException& cme)
      : UExceptionBase(cme)
    {
    }
    ~UException()
    {
    }
  };

  // Commonly used exception types
  namespace excpt {
    // Assertion failure
    extern UExceptionType Assert;
    // Out of memory
    extern UExceptionType OutOfMemory;
    // Object integrity has failed 
    extern UExceptionType IntegrityFail;
    // Object violates its schema 
    extern UExceptionType Malformed;
    // Inappropriate value for operation
    extern UExceptionType BadValue;
    // Non-null argument required 
    extern UExceptionType NullArg;
    // Access violation
    extern UExceptionType NoAccess;
    // Object not found
    extern UExceptionType NoObject;
    // No object expected, but object exists
    extern UExceptionType ObjectExists;
    // I/O Operation was truncated
    extern UExceptionType Truncated;
    // I/O proceeded past end of object 
    extern UExceptionType Overrun;
    // Some error occurred while running a subprocess.
    // FIX: This is too general. It needs at least an 
    // error code and perhaps something better than that.
    extern UExceptionType Subprocess;
    // Could not acquire lock 
    extern UExceptionType LockFail;
    // Lock race failed 
    extern UExceptionType LostLock;
    // Heat death of universe has occurred (a cryptographic hash
    // collision has occurred
    extern UExceptionType UniverseDied;
    // Network connect/session setup failed 
    extern UExceptionType NoConnect;
    // Unable to authenticate 
    extern UExceptionType NoAuth;
    // Missing something from environment 
    extern UExceptionType Environ;
    // Unknown object version 
    extern UExceptionType VersionError;
    // Network connection lost 
    extern UExceptionType ConnLost;
    // Unspecified error during I/O
    extern UExceptionType IoError;
    // Unknown protocol operation 
    extern UExceptionType BadOpcode;
    // Some problem with the pseudo-random number generator 
    extern UExceptionType PrngError;
    // Array bounds violation
    extern UExceptionType BoundsError;

    // Use when you can't decide. It is *always* a mistake to use this.
    extern UExceptionType Unspecified;
  } /* namespace excpt */

} /* namespace sherpa */

#define THROW(ex,s) throw ::sherpa::UException<&ex>(__FILE__, __LINE__, s)

// Local Variables:
// mode:c++
// End:

#endif /* LIBSHERPA_UEXCEPT_H */
