#ifndef UOCINFO_HXX
#define UOCINFO_HXX

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

#include <set>
#include <string>
#include <ostream>

#include <boost/filesystem/path.hpp>
#include <libsherpa/LexLoc.hxx>

#include "Environment.hxx"
#include "AST.hxx"
#include "TypeScheme.hxx"
#include "Symtab.hxx"
#include "TypeInfer.hxx"
#include "WorkList.hxx"

enum UocFlagValues {
  UOC_NO_FLAGS = 0,

  /// @brief Indicates that this UOC is the prelude UOC.
  ///
  /// This does not appear to be consulted anywhere.
  UOC_IS_PRELUDE   = 0x1u,

  /// @brief Indicates that definitions in this UOC may be changed.
  ///
  /// This is (in theory) used in the instantiator.
  ///
  /// @bug This flag is not set anywhere. What is it for?
  UOC_IS_MUTABLE   = 0x2u,

  /// @brief Indicates that this UOC has been visited by the instantiator.
  UOC_SEEN_BY_INST = 0x4u,
};

typedef sherpa::EnumSet<UocFlagValues> UocFlags;

// Passes consist of transforms on the AST. The parse pass is included in 
// the list for debugging annotation purposes, but is handled as a special
// case.

enum Pass {
#define PASS(nm, short, descrip) pn_##nm,
#include "pass.def"
};

enum OnePass {
#define ONEPASS(nm, short, descrip) op_##nm,
#include "onepass.def"
};

class UocInfo;

struct PassInfo {
  const char *name;
  bool (UocInfo::* fn)(std::ostream& errStream, 
                       bool init, unsigned long flags);
  const char *descrip;
  bool printAfter;                // for debugging
  bool typesAfter;                // for debugging
  bool stopAfter;                // for debugging
};

struct OnePassInfo {
  const char *name;
  bool (UocInfo::* fn)(std::ostream& errStream, 
                       bool init, unsigned long flags);
  const char *descrip;
  bool printAfter;                // for debugging
  bool typesAfter;                // for debugging
  bool stopAfter;                // for debugging
};


class UocInfo;
typedef std::map<std::string, boost::shared_ptr<UocInfo> > UocMap;

// Unit Of Compilation Info. One of these is constructed for each
// unit of compilation (source or interface). In the source case, the
// /name/ field will be the file name. In the interface case, the
// /name/ will be the "ifname" reported in the import statement.
class UocInfo : public boost::enable_shared_from_this<UocInfo> {
  static boost::filesystem::path resolveInterfacePath(std::string ifName);

public:
  std::string uocName;                // either ifName or simulated
  std::string origin;                // typically the file name
  UocFlags flags;
  
  Pass lastCompletedPass;

  bool fromCommandLine;
  boost::shared_ptr<AST> uocAst;
  boost::shared_ptr<ASTEnvironment > env;
  boost::shared_ptr<TSEnvironment > gamma;
  boost::shared_ptr<InstEnvironment > instEnv;

  inline bool isSourceUoc() {
    return (uocAst->astType == at_module);
  }

  inline bool isInterfaceUoc() {
    return (uocAst->astType == at_interface);
  }

  
  UocInfo(const std::string& _uocName, const std::string& _origin, 
          boost::shared_ptr<AST> _uocAst);
  UocInfo(boost::shared_ptr<UocInfo> uoc);

  static inline boost::shared_ptr<UocInfo>
  make(const std::string& _uocName, const std::string& _origin, 
          boost::shared_ptr<AST> _uocAst) {
    UocInfo *tmp = new UocInfo(_uocName, _origin, _uocAst);
    return boost::shared_ptr<UocInfo>(tmp);
  }

  static inline boost::shared_ptr<UocInfo>
  make(boost::shared_ptr<UocInfo> uoc) {
    UocInfo *tmp = new UocInfo(uoc);
    return boost::shared_ptr<UocInfo>(tmp);
  }

  /* Create a fresh, empty UOC that is set up to become the unified
   * output UoC module, initializing all environments by hand.
   */
  static boost::shared_ptr<UocInfo> CreateUnifiedUoC();

  static std::vector<boost::filesystem::path> searchPath;

  // FIX -- All of these should be locals

  // The presence of a UocInfo record in the ifList indicates that
  // parsing of an interface has at least started. If the /ast/ pointer
  // is non-null, the parse has been completed.
  static UocMap ifList;
  static UocMap srcList;
  //  static boost::shared_ptr<UocInfo> linkedUoc;  // grand Uoc after linkage

  static boost::shared_ptr<UocInfo> 
  findInterface(const std::string& ifName);

  static boost::shared_ptr<UocInfo> 
  importInterface(std::ostream&, const sherpa::LexLoc& loc, 
                  const std::string& ifName);

  // This is only used within the parser, and perhaps should not be
  // part of UocInfo.
  static std::string 
  UocNameFromSrcName(const std::string& srcFileName, unsigned ndx);

  // Parse a file, admitting source and/or interface units of
  // compilation into the ifList or the srcList as a side effect.
  static bool 
  CompileFromSexprFile(const boost::filesystem::path& src, bool fromCmdLine);
  static bool 
  CompileFromBlockFile(const boost::filesystem::path& src, bool fromCmdLine);
  static bool 
  CompileFromTransitionFile(const boost::filesystem::path& src, bool fromCmdLine);
  static bool 
  CompileFromFile(const boost::filesystem::path& src, bool fromCmdLine);

  // Individual passes:
#define PASS(nm,short,descrip)                                  \
  bool fe_##nm(std::ostream& errStream,                          \
               bool init=true, unsigned long flags=0);
#include "pass.def"

#define ONEPASS(nm,short,descrip)                                \
  bool be_##nm(std::ostream& errStream,                                \
               bool init=true, unsigned long flags=0);
#include "onepass.def"  

  static PassInfo passInfo[];  
  static OnePassInfo onePassInfo[];

  static bool mainIsDefined;
  void Compile();
  void DoBackend();

  void PrettyPrint(std::ostream& out, bool decorate=false);
  void ShowTypes(std::ostream& out);
 

  //////////////////////////////////////////////////////
  //
  // FOLLOWING ARE IN inter-pass.cxx
  //
  //////////////////////////////////////////////////////
  void findDefForms(boost::shared_ptr<AST> start, 
                    boost::shared_ptr<AST> local=boost::GC_NULL, 
                    boost::shared_ptr<AST> top=boost::GC_NULL);
  static void findAllDefForms();
  
  // Add all candidate Entry-points to the entry-point vectror
  static void addAllCandidateEPs();
  
  /* Build/Remove a newDefScope wrapper that can be used to
     temporarily add definitions into them and later commit/abort */
  void wrapEnvs();
  void unwrapEnvs();
  
// Post inference Flags
#define PI_SYM_FLAGS (RSLV_NO_RESOLVE_DECL)
#define PI_TYP_FLAGS (TI_ALL_INSTS_OK)

// RandTflags used by onePass definitions
#define OP_SYM_FLAGS (PI_SYM_FLAGS | RSLV_SYM_NO_PRELUDE)
#define OP_TYP_FLAGS (PI_TYP_FLAGS | TI_NO_PRELUDE)

// RandT flags used by passes past polyinstantiation. 
#define POLY_SYM_FLAGS (OP_SYM_FLAGS)
#define POLY_TYP_FLAGS (OP_TYP_FLAGS | TI_NO_MORE_TC |                \
                        TI_DEF_DECL_NO_MATCH | TI_USING_FQNS)
  
// RandT flags used by passes Refization pass of Closure-conversion.
#define REF_SYM_FLAGS (POLY_SYM_FLAGS | RSLV_INCOMPLETE_NO_CHK)
#define REF_TYP_FLAGS (POLY_TYP_FLAGS)

// RandT flags used by passes past Closure-conversion. 
#define CL_SYM_FLAGS (REF_SYM_FLAGS)
#define CL_TYP_FLAGS (REF_TYP_FLAGS)


  bool DoResolve(std::ostream& errStream, bool init, 
               ResolverFlags rflags);

  bool Resolve(std::ostream& errStream, bool init, 
               ResolverFlags rflags, std::string pre);

  bool 
  DoTypeCheck(std::ostream& errStream, bool init, 
              bool &rewrite, TI_Flags ti_flags);
  
  bool 
  TypeCheck(std::ostream& errStream, bool init, 
            bool& rewrite, TI_Flags ti_flags, std::string pre);
  
  bool RandT(std::ostream& errStream,
             bool init=false, 
             ResolverFlags rflags= RSLV_NO_FLAGS,
             TI_Flags ti_flags=TI_NO_FLAGS,
             std::string pre = "Internal Compiler error :");

  bool RandTexpr(std::ostream& errStream,
                 boost::shared_ptr<AST> ast,
                 bool &rewrite,
                 ResolverFlags rflags= RSLV_NO_FLAGS,
                 TI_Flags ti_flags=TI_NO_FLAGS,
                 std::string pre = "Internal Compiler error :",
                 bool keepResults = true,
                 boost::shared_ptr<EnvSet> altEnvSet=boost::GC_NULL);

  //////////////////////////////////////////////////////
  //
  // New instantiation logic:
  //
  //////////////////////////////////////////////////////
  static boost::shared_ptr<AST> lookupByFqn(const FQName& fqn, 
                          boost::shared_ptr<UocInfo> &targetUoc);
  

private:
  void addTopLevelForm(boost::shared_ptr<AST> ast); // Add a new Top-level form

  bool instantiateFQN(std::ostream &errStream, 
                      const FQName& fqn);

  // The main AST specializer/ instantiator
  boost::shared_ptr<AST> doInstantiate(std::ostream &errStream, 
                     boost::shared_ptr<AST> defAST, 
                     boost::shared_ptr<Type> typ,
                     bool &errFree,
                     WorkList<std::string>& worklist);

  boost::shared_ptr<AST> recInstantiate(std::ostream &errStream, 
                      boost::shared_ptr<AST> ast,
                      bool &errFree,
                      WorkList<std::string>& worklist); // Recursively walk the
                     // specialized AST, and specialize the body.

public:
  /// @brief Instantiate a single fully qualified name.
  bool instantiate(std::ostream &errStream, 
                   const FQName& fqn);
  
  /// @brief Instantiate a set of entry point names.
  bool instantiateBatch(std::ostream &errStream, 
                        std::set<FQName>& epName);

};

#endif /* UOCINFO_HXX */
