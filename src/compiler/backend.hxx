#ifndef BACKEND_HXX
#define BACKEND_HXX

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

#include <iostream>
#include "UocInfo.hxx"
#include "AST.hxx"

/** @file Interface for defining back-ends.
 *
 * This is actually misnamed, since what it describes is an emission
 * target.
 */

typedef bool (*BackEndFn) (std::ostream& out, std::ostream& err,
			   boost::shared_ptr<UocInfo> uoc);

typedef bool (*MidEndFn) (std::ostream& out, std::ostream& err);


/* @brief Emit output in XML pretty-printed form.
 *
 * This target has not been maintained in a long time, and should
 * probably be dropped.
 */
bool XMLpp(std::ostream& out, std::ostream& err, boost::shared_ptr<UocInfo> uoc);
/* Dump output in not-so-pretty XML form.
 *
 * This target has not been maintained in a long time, and should
 * probably be dropped.
 */
bool XMLdump(std::ostream& out, std::ostream& err, boost::shared_ptr<UocInfo> uoc);

/* @brief Emit output in XML pretty-printed form, including types */
bool XMLtypesPP(std::ostream& out, std::ostream& err, boost::shared_ptr<UocInfo> uoc);

/* @brief Emit a C header file containing the externalizable
 * declarations from every interface that was referenced by the input
 * units of compilation.
 */
bool EmitHeader(std::ostream& out, std::ostream& err, 
		boost::shared_ptr<UocInfo> uoc);

/* @brief Emit a C source file that is the whole-program compilation
 * result for all input source files.
 *
 * The C file will include only those procedures that are reachable
 * from the entry points.
 */
bool EmitC(std::ostream& out, std::ostream& err, boost::shared_ptr<UocInfo> uoc);

/* @brief Emit a C source file as in EmitC, and compile the result to
 * an execuitable.
 */
bool EmitExe(std::ostream& out, std::ostream& err, boost::shared_ptr<UocInfo> uoc);

/* @brief Gather all of ths source units of compilation mentioned on
 * the command line into a quasi-archive file.
 */
bool EmitBitO(std::ostream& out, std::ostream& err);

struct BackEnd {
  /** @brief Name of this target */
  std::string name;
  /** @brief Name of last front-end pass that is required before
   * proceeding to the mid-end */
  Pass needPass;
  /** @brief Name of last mid-end pass that must be run before
   * proceeding to the back end.
   */
  OnePass oPass;
  /** @brief For targets that enit on a per-UoC basis, procedure to
   * run on each input unit of compilation. */
  BackEndFn fn;
  /** @brief For targets that require the grand UoC to be built,
   * procedure to run just before building the grand UoC. */
  MidEndFn  midfn;
  /** @brief For targets that emit from the Grand UoC, procedure to
   * call on the Grand UoC after it has been built. */
  BackEndFn plfn;

  /** @brief Flags directing the front end. See below. */
  unsigned long flags;

  /** @brief Total number of targets known to the compiler. */
  static size_t nBackEnd;
  /** @brief Vector of targets known to the compiler (first is
   * default). */
  static BackEnd backends[];
};

/** @brief Set in BackEnd.flags if this target only produces header
 * files.
 *
 * @bug This may be reduntant, since it may be implied by which
 * backend pass functions are set. Shap needs to check. */
#define BK_HDR_MODE 0x001u
/** @brief Set if we will not be running any passes that require the
 * mid-end or back-end to be run.
 *
 * @bug This may be reduntant, since it may be implied by which
 * backend pass functions are set. Shap needs to check. */
#define BK_UOC_MODE 0x002u
/** @brief Set if the target will be attempting to produce a binary,
 * and therfore requires archive libraries mentioned on the command
 * line to actually resolve to something.
 */
#define BK_LINKING  0x004u

#endif /* BACKEND_HXX */
