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

#include <libsherpa/Path.hxx>
#include <libsherpa/LexLoc.hxx>
#include <libsherpa/CVector.hxx>

#include "Environment.hxx"
#include "AST.hxx"
#include "TypeScheme.hxx"
#include "Symtab.hxx"
#include "TypeInfer.hxx"
#include "WorkList.hxx"

#define UOC_IS_PRELUDE    0x001u
#define UOC_IS_BUILTIN    0x002u
#define UOC_IS_MUTABLE    0x004u // New definitions can be added to
  // this UOC after the frontend has seen it once, ex. in the case of
  // an interactive environment.
#define UOC_SEEN_BY_INST  0x008u // UOC already seen by Instantiator.
 

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
  bool printAfter;		// for debugging
  bool typesAfter;		// for debugging
  bool stopAfter;		// for debugging
};

struct OnePassInfo {
  const char *name;
  bool (UocInfo::* fn)(std::ostream& errStream, 
		       bool init, unsigned long flags);
  const char *descrip;
  bool printAfter;		// for debugging
  bool typesAfter;		// for debugging
  bool stopAfter;		// for debugging
};


// Unit Of Compilation Info. One of these is constructed for each
// unit of compilation (source or interface). In the source case, the
// /name/ field will be the file name. In the interface case, the
// /name/ will be the "ifname" reported in the import statement.
class UocInfo : public sherpa::Countable {
  static sherpa::GCPtr<sherpa::Path> resolveInterfacePath(std::string ifName);

public:
  std::string uocName;		// either ifName or simulated
  std::string origin;		// typically the file name
  unsigned long flags;
  
  Pass lastCompletedPass;

  bool fromCommandLine;
  sherpa::GCPtr<AST> uocAst;
  sherpa::GCPtr<ASTEnvironment > env;
  sherpa::GCPtr<TSEnvironment > gamma;

  inline bool isSourceUoc() {
    return (uocAst->astType == at_module);
  }

  inline bool isInterfaceUoc() {
    return (uocAst->astType == at_interface);
  }

  // This is the Instance environment. 
  // Currently indexed by FQN onto a list of Instances 
  // currently visible. The key must be FQN and not the 
  // because this environment must be cross referenced 
  // across modules and recognizable by the common
  // Type-class name.

  // TODO TODO TODO:
  // Using FQN (in its string representation) is actually 
  // very stupid. The AST pointer of the defining occurance
  // of the typeclass is what I should really use. However,
  // due to the shortsightedness of initial implementation,
  // the key in the environment structure is fixed to
  // std::string. This must someday be changed to use 2 
  // template parameters for the key and value. I dont want
  // to jump and make this change unless I know that this
  // implementation of instances works. 
   
  sherpa::GCPtr<InstEnvironment > instEnv;
  
  UocInfo(const std::string& _uocName, const std::string& _origin, 
	  sherpa::GCPtr<AST> _uocAst);
  UocInfo(sherpa::GCPtr<UocInfo> uoc);

  /* Create a fresh, empty UOC that is set up to become the unified
   * output UoC module, initializing all environments by hand.
   */
  static sherpa::GCPtr<UocInfo> CreateUnifiedUoC();

  static sherpa::GCPtr<sherpa::CVector<sherpa::GCPtr<sherpa::Path> > > searchPath;

  // FIX -- All of these should be locals

  // The presence of a UocInfo record in the ifList indicates that
  // parsing of an interface has at least started. If the /ast/ pointer
  // is non-null, the parse has been completed.
  static sherpa::GCPtr<sherpa::CVector<sherpa::GCPtr<UocInfo> > > ifList;
  static sherpa::GCPtr<sherpa::CVector<sherpa::GCPtr<UocInfo> > > srcList;
  //  static sherpa::GCPtr<UocInfo> linkedUoc;  // grand Uoc after linkage

  static sherpa::GCPtr<UocInfo> 
  findInterface(const std::string& ifName);

  static sherpa::GCPtr<UocInfo> 
  importInterface(std::ostream&, const sherpa::LexLoc& loc, 
		  const std::string& ifName);

  // This is only used within the parser, and perhaps should not be
  // part of UocInfo.
  static std::string 
  UocNameFromSrcName(const std::string& srcFileName, unsigned ndx);

  // Parse a file, admitting source and/or interface units of
  // compilation into the ifList or the srcList as a side effect.
  static bool 
  CompileFromFile(const std::string& srcFileName, bool fromCmdLine);

  // Individual passes:
#define PASS(nm,short,descrip)				  \
  bool fe_##nm(std::ostream& errStream,			  \
	       bool init=true, unsigned long flags=0);
#include "pass.def"

#define ONEPASS(nm,short,descrip)				\
  bool be_##nm(std::ostream& errStream,				\
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
  void findDefForms(sherpa::GCPtr<AST> start, sherpa::GCPtr<AST> local=NULL, sherpa::GCPtr<AST> top=NULL);
  static void findAllDefForms();
  
  // Add all candidate Entry-points to the entry-point vectror
  static void addAllCandidateEPs();
  
  /* Build/Remove a newDefScope wrapper that can be used to
     temporarily add definitions into them and later commit/abort */
  void wrapEnvs();
  void unwrapEnvs();
  
// Post inference Flags
#define PI_SYM_FLAGS (NO_RESOLVE_DECL)
#define PI_TYP_FLAGS (ALL_INSTS_OK)

// RandTflags used by onePass definitions
#define OP_SYM_FLAGS (PI_SYM_FLAGS | SYM_NO_PRELUDE)
#define OP_TYP_FLAGS (PI_TYP_FLAGS | TYP_NO_PRELUDE)

// RandT flags used by passes past polyinstantiation. 
#define POLY_SYM_FLAGS (OP_SYM_FLAGS | SYM_POST_POLY)
#define POLY_TYP_FLAGS (OP_TYP_FLAGS | NO_MORE_TC | DEF_DECL_NO_MATCH)

// RandT flags used by passes Refization pass of Closure-conversion.
#define REF_SYM_FLAGS (POLY_SYM_FLAGS)
#define REF_TYP_FLAGS (POLY_TYP_FLAGS | POST_REFIZE)

// RandT flags used by passes past Closure-conversion. 
#define CL_SYM_FLAGS (REF_SYM_FLAGS)
#define CL_TYP_FLAGS (REF_TYP_FLAGS)


  bool Resolve(std::ostream& errStream, bool init, 
	       unsigned long rflags, std::string pre);

  bool 
  TypeCheck(std::ostream& errStream, bool init, 
	    unsigned long tflags, std::string pre);

  bool RandT(std::ostream& errStream,
	     bool init=false, 
	     unsigned long rflags=0,
	     unsigned long tflags=0,
	     std::string pre = "Internal Compiler error :");

  bool RandTexpr(std::ostream& errStream,
		 sherpa::GCPtr<AST> ast,
		 unsigned long rflags=0,
		 unsigned long tflags=0,
		 std::string pre = "Internal Compiler error :",
		 bool keepResults = true,
		 sherpa::GCPtr<envSet> altEnvSet=NULL);

  //////////////////////////////////////////////////////
  //
  // New instantiation logic:
  //
  //////////////////////////////////////////////////////
  static sherpa::GCPtr<AST> lookupByFqn(const std::string& s, 
			  sherpa::GCPtr<UocInfo> &targetUoc);
  

private:
  void addTopLevelForm(sherpa::GCPtr<AST> ast); // Add a new Top-level form

  bool instantiateFQN(std::ostream &errStream, 
			 const std::string& fqn);

  // The main AST specializer/ instantiator
  sherpa::GCPtr<AST> doInstantiate(std::ostream &errStream, 
		     sherpa::GCPtr<AST> defAST, 
		     sherpa::GCPtr<Type> typ,
		     bool &errFree,
		     sherpa::GCPtr<WorkList<std::string> > worklist);

  sherpa::GCPtr<AST> recInstantiate(std::ostream &errStream, 
		      sherpa::GCPtr<AST> ast,
		      bool &errFree,
		      sherpa::GCPtr<WorkList<std::string> > worklist); // Recursively walk the
                     // specialized AST, and specialize the body.

public:
  bool instantiate(std::ostream &errStream, 
		   const std::string& fqn);
  
  bool instantiateBatch(std::ostream &errStream, 
			std::set<std::string>& fqns);

};

#endif /* UOCINFO_HXX */
