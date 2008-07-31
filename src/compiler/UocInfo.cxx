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

#include <assert.h>
#include <stdint.h>
#include <dirent.h>
#include <string>
#include <iostream>
#include <sstream>

#include "Options.hxx"
#include "Version.hxx"
#include "UocInfo.hxx"
#include "Symtab.hxx"
#include "Type.hxx"
#include "backend.hxx"
#include "inter-pass.hxx"
#include "SexprLexer.hxx"

#include <boost/filesystem/operations.hpp>
#include <libsherpa/util.hxx>

using namespace std;
using namespace boost;
using namespace sherpa;

vector<filesystem::path> UocInfo::searchPath;
UocMap UocInfo::ifList;
UocMap UocInfo::srcList;

bool UocInfo::mainIsDefined = false;

PassInfo UocInfo::passInfo[] = {
#define PASS(nm, short, descrip) { short, &UocInfo::fe_##nm, descrip, false },
#include "pass.def"
};

OnePassInfo UocInfo::onePassInfo[] = {
#define ONEPASS(nm, short, descrip) { short, &UocInfo::be_##nm, descrip, false },
#include "onepass.def"
};


std::string 
UocInfo::UocNameFromSrcName(const std::string& srcFileName, unsigned ndx)
{
  // Use the filename with the extension (if any) chopped off.
  size_t lastDot = srcFileName.rfind(".");
  return srcFileName.substr(0, lastDot) + "#" + unsigned_str(ndx);
}

UocInfo::UocInfo(const std::string& _uocName, const std::string& _origin,
		 GCPtr<AST> _uocAst)
{
  lastCompletedPass = pn_none;
  uocAst = _uocAst;
  env = sherpa::GC_NULL;
  gamma = sherpa::GC_NULL;

  uocName = _uocName;
  origin = _origin;
}

// Why does this exist? Are we actually using it anywhere?
// I'm concerned because it isn't doing deep copy, I don't understand
// whether it *needs* to do deep copy (or not), and there is no
// comment here answering my question.
UocInfo::UocInfo(GCPtr<UocInfo> uoc)
{
  uocName = uoc->uocName;
  origin = uoc->origin;
  lastCompletedPass = uoc->lastCompletedPass;
  uocAst = uoc->uocAst;
  env = uoc->env;
  gamma = uoc->gamma;
  instEnv = uoc->instEnv;
}

GCPtr<UocInfo>
UocInfo::CreateUnifiedUoC()
{
  LexLoc loc;
  GCPtr<AST> ast = AST::make(at_module, loc);

  std::string uocName = "*emit*";
  GCPtr<UocInfo> uoc = UocInfo::make(uocName, "*internal*", ast);

  uoc->env = ASTEnvironment::make(uocName);
  uoc->gamma = TSEnvironment::make(uocName);
  uoc->instEnv = InstEnvironment::make(uocName);

  uoc->env = uoc->env->newDefScope();
  uoc->gamma = uoc->gamma->newDefScope();
  uoc->instEnv = uoc->instEnv->newDefScope();  

  return uoc;
}


filesystem::path
UocInfo::resolveInterfacePath(std::string ifName)
{
  std::string tmp = ifName;
  for (size_t i = 0; i < tmp.size(); i++) {
    if (tmp[i] == '.')
      tmp[i] = '/';
  }
  
  std::string leafName = tmp + ".bitc";

  for (size_t i = 0; i < UocInfo::searchPath.size(); i++) {
    filesystem::path testPath = UocInfo::searchPath[i] / leafName;
    
    if (filesystem::exists(testPath)) {
      if (!filesystem::is_regular(testPath)) {
	std::cerr << "bitcc: source path \""
		  << testPath << "\" for interface \""
		  << ifName << "\" is not "
		  << "a regular file." << std::endl;
	exit(1);
      }
      return testPath;
    }
  }

  std::cerr << "bitcc: error: no source path found for interface \""
	    << ifName << "\"." << std::endl;
  exit(0);
}


void 
UocInfo::addTopLevelForm(GCPtr<AST> def)
{
  assert(uocAst->astType == at_module);
  uocAst->children.push_back(def);  
}

bool
  UocInfo::CompileFromFile(const filesystem::path& src, bool fromCmdLine)
{
  // Use binary mode so that newline conversion and character set
  // conversion is not done by the stdio library.
  std::ifstream fin(src.string().c_str(), std::ios_base::binary);

  if (!fin.is_open()) {
    std::cerr << "Couldn't open input file \""
	      << src
	      << "\"" << std::endl;
    return false;
  }

  SexprLexer lexer(std::cerr, fin, src.string(), fromCmdLine);

  // This is no longer necessary, because the parser now handles it
  // for all interfaces whose name starts with "bitc.xxx"
  //
  // if (this->flags & UOC_IS_PRELUDE)
  //   lexer.isRuntimeUoc = true;

  lexer.setDebug(Options::showLex);

  extern int bitcparse(SexprLexer *lexer);
  bitcparse(&lexer);  
  // On exit, ast is a pointer to the AST tree root.
  
  fin.close();

  if (lexer.num_errors != 0u)
    return false;

  return true;
}

GCPtr<UocInfo> 
UocInfo::findInterface(const std::string& ifName)
{
  UocMap::iterator itr = UocInfo::ifList.find(ifName);
  if (itr == UocInfo::ifList.end())
    return sherpa::GC_NULL;

  return itr->second;
}

GCPtr<UocInfo> 
UocInfo::importInterface(std::ostream& errStream,
			 const LexLoc& loc, const std::string& ifName)
{
  // First check to see if we already have it. Yes, this
  // IS a gratuitously stupid way to do it.
  GCPtr<UocInfo> puoci = findInterface(ifName);

  if (puoci && puoci->uocAst)
    return puoci;

  if (puoci) {
    errStream
      << loc.asString() << ": "
      << "Fatal error: recursive dependency on interface "
      << "\"" << ifName << "\"\n"
      << "This needs a better diagnostic so you can decipher"
      << " what happened.\n";
    exit(1);
  }

  filesystem::path path = resolveInterfacePath(ifName);
  if (path.empty()) {
    errStream
      << loc.asString() << ": "
      << "Import failed for interface \"" << ifName << "\".\n"
      << "Interface unit of compilation not found on search path.\n";
    exit(1);
  }

  CompileFromFile(path, false);

  // If we survived the compile, the interface is now in the ifList
  // and can be found.
  return findInterface(ifName);
}

bool
UocInfo::fe_none(std::ostream& errStream, bool init, 
		 unsigned long flags)
{
  return true;
}

bool
UocInfo::fe_npass(std::ostream& errStream, bool init,
		  unsigned long flags)
{
  return true;
}


bool
UocInfo::be_none(std::ostream& errStream, bool init, 
		 unsigned long flags)
{
  return true;
}

bool
UocInfo::be_npass(std::ostream& errStream, bool init,
		  unsigned long flags)
{
  return true;
}


bool
UocInfo::fe_parse(std::ostream& errStream, bool init,
		  unsigned long flags)
{
  // The parse pass is now vestigial. The only reason that it still
  // exists is to preserve the ability to get a post-parse dump of the
  // parse tree in various output languages.

  return true;
}

void
UocInfo::Compile()
{
  for (size_t i = pn_none+1; (Pass)i <= Options::backEnd->needPass; i++) {
    if (Options::showPasses)
      std::cerr << uocName << " PASS " << passInfo[i].name << std::endl;
    
    //std::cout << "Now performing "
    //	      << passInfo[i].descrip
    //	      << " on " << path->asString()
    //	      << std::endl;
    
    bool showTypes = false;
    
    if(Options::showTypesUocs.find(uocName) != Options::showTypesUocs.end())
      showTypes = true;
    
    if (! (this->*passInfo[i].fn)(std::cerr, true, 0) ) {
      std::cerr << "Exiting due to errors during "
		<< passInfo[i].descrip
		<< std::endl;
      exit(1);
    }

    if(!uocAst->isValid()) {
      std::cerr << "PANIC: Invalid AST built for file \""	
		<< origin
		<< "\"."
		<< "[Pass: "
		<< passInfo[i].descrip
		<< "] "
		<< "Please report this problem.\n"; 
      std::cerr << uocAst->asString() << std::endl;
      exit(1);
    }

    if (passInfo[i].printAfter) {
      std::cerr << "==== DUMPING "
		<< uocName
		<< " AFTER " << passInfo[i].name 
		<< " ====" << std::endl;
      this->PrettyPrint(std::cerr, Options::ppDecorate);
    }
    if (passInfo[i].typesAfter || 
	(showTypes && passInfo[i].name == "typecheck")) {
      std::cerr << "==== TYPES "
		<< uocName
		<< " AFTER " << passInfo[i].name 
		<< " ====" << std::endl;
      this->ShowTypes(std::cerr);
      std::cerr <<std::endl << std::endl;
    }

    if (passInfo[i].stopAfter) {
      std::cerr << "Stopping (on request) after "
		<< passInfo[i].name
		<< std::endl;
      return;
    }

    lastCompletedPass = (Pass) i;
  }
}

void
UocInfo::DoBackend() 
{
  for(size_t i = op_none+1; (OnePass)i <= Options::backEnd->oPass ; i++) {
    
    if (Options::showPasses)
      std::cerr << "PASS " << onePassInfo[i].name << std::endl;

    if(! (this->*onePassInfo[i].fn)(std::cerr, true, 0)) {
      std::cerr << "Exiting due to errors during "
		<< onePassInfo[i].descrip
		<< std::endl;
      exit(1);
    }

    if(!uocAst->isValid()) {
      std::cerr << "PANIC: Invalid AST built for file \""
		<< origin
		<< "\"."
		<< "Please report this problem.\n"; 
      std::cerr << uocAst->asString() << std::endl;
      exit(1);
    }

    if (onePassInfo[i].printAfter) {
      std::cerr << "==== DUMPING bigAST AFTER"
		<< onePassInfo[i].name 
		<< " ====" << std::endl;
      this->PrettyPrint(std::cerr, Options::ppDecorate);
    }

    if (onePassInfo[i].typesAfter) {
      std::cerr << "==== TYPES bigAST AFTER"
		<< onePassInfo[i].name 
		<< " ====" << std::endl;
      this->ShowTypes(std::cerr);
      std::cerr <<std::endl << std::endl;
    }

    if (onePassInfo[i].stopAfter) {
      std::cerr << "Stopping (on request) after "
		<< onePassInfo[i].name
		<< std::endl;
      exit(1);
    }
  }  
}
