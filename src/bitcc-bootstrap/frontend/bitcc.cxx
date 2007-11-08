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

#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <errno.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>


#include <fstream>
#include <getopt.h>
#include <langinfo.h>

#include <libsherpa/Path.hxx>
#include <iostream>

#include "Version.hxx"
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "backend.hxx"
#include "INOstream.hxx"
#include "Instantiate.hxx"

using namespace sherpa;
using namespace std;

#define BITC_COMPILER_MODE        0x1u
#define BITC_INTERPRETER_MODE     0x2u
#define BITC_CURRENT_MODE BITC_COMPILER_MODE

bool Options::showParse = false;
bool Options::showTypes = false;
bool Options::xmlTypes = false;
bool Options::showLex = false;
bool Options::useStdInc = true;
bool Options::useStdLib = true;
bool Options::advisory = false;
bool Options::rawTvars = false;
bool Options::showMaybes = false;
bool Options::FQtypes = false;
bool Options::showAllTccs = false;
bool Options::showPasses = false;
bool Options::topMutOnly = false; 
bool Options::dumpAfterMidEnd = false;
bool Options::dumpTypesAfterMidEnd = false;
GCPtr<CVector<std::string> > Options::showTypesUocs;
GCPtr<CVector<std::string> > Options::xmlTypesUocs;
bool Options::ppFQNS = false;
bool Options::ppDecorate = false;
GCPtr<CVector<std::string> > Options::entryPts;
BackEnd *Options::backEnd = 0;
std::string Options::outputFileName;
GCPtr<CVector<GCPtr<Path> > > Options::libPath;
bool Options::Wall = false;
bool Options::nogc = false;

#define LOPT_SHOWLEX      257   /* Show tokens */
#define LOPT_SHOWPARSE    258   /* Show parse */
#define LOPT_DUMPAFTER    259   /* PP after this pass */
#define LOPT_SHOWPASSNMS  260   /* Show all pass names */
#define LOPT_NOSTDINC     261   /* Do not append std search paths */
#define LOPT_NOSTDLIB     262   /* Do not append std lib paths */
#define LOPT_ADVISORY     263   /* Show advisory information */
#define LOPT_RAW_TVARS    264   /* Show tvars as is */
#define LOPT_FQ_TYPES     265   /* Show fully qualified types */
#define LOPT_SA_TCC       266   /* Show all Type class constraints */
#define LOPT_SHOWPASSES   267   /* Show passes as they are run */
#define LOPT_PPFQNS       268   /* Show FQNs when pretty printing */
#define LOPT_DUMPTYPES    269   /* Show types after this pass */
#define LOPT_STOPAFTER    270   /* Stop after this pass */
#define LOPT_PPDECORATE   271   /* Decorate pretty printing with types */
#define LOPT_SHOW_MAYBES  272   /* Show all maybe wrapper types, hints */
#define LOPT_SHOW_TYPES   273   /* Dump types a particular uoc only */
#define LOPT_TOP_MUT      274   /* Top-level mutability compatibility only */
#define LOPT_XML_TYPES    275   /* Dump XML types */
#define LOPT_NOGC         276   /* NO GC mode */

struct option longopts[] = {
  /*  name,           has-arg, flag, val           */
  { "decorate",             0,  0, LOPT_PPDECORATE },
  { "dumpafter",            1,  0, LOPT_DUMPAFTER },
  { "dumptypes",            1,  0, LOPT_DUMPTYPES },
  { "free-advice",          0,  0, LOPT_ADVISORY },
  { "full-qual-types",      0,  0, LOPT_FQ_TYPES },
  { "help",                 0,  0, 'h' },
  { "topmutonly",           0,  0, LOPT_TOP_MUT },
  { "nostdinc",             0,  0, LOPT_NOSTDINC },
  { "nostdlib",             0,  0, LOPT_NOSTDLIB },
  { "ppfqns",               0,  0, LOPT_PPFQNS },
  { "raw-tvars",            0,  0, LOPT_RAW_TVARS },
  { "show-maybes",          0,  0, LOPT_SHOW_MAYBES },
  { "show-all-tccs",        0,  0, LOPT_SA_TCC },
  { "showlex",              0,  0, LOPT_SHOWLEX },
  { "showparse",            0,  0, LOPT_SHOWPARSE },
  { "showpasses",           0,  0, LOPT_SHOWPASSES },
  { "showpassnames",        0,  0, LOPT_SHOWPASSNMS },
  { "showtypes",            1,  0, LOPT_SHOW_TYPES },
  { "xmltypes",             1,  0, LOPT_XML_TYPES },
  { "stopafter",            1,  0, LOPT_STOPAFTER },
  { "no-gc",                0,  0, LOPT_NOGC },
  { "version",              0,  0, 'V' },
#if 0
  /* Options that have short-form equivalents: */
  { "debug",                0,  0, 'd' },
  { "dispatchers",          0,  0, 's' },
  { "entry",                1,  0, 'e' },
  { "execdir",              1,  0, 'X' },
  { "header",               1,  0, 'h' },
  { "include",              1,  0, 'I' },
  { "index"  ,              1,  0, 'n' },
  { "language",             1,  0, 'l' },
  { "outdir",               1,  0, 'D' },
  { "output",               1,  0, 'o' },
  { "verbose",              0,  0, 'v' },
#endif
  {0,                       0,  0, 0}
};

void
help()
{
  std::cerr 
    << "Usage:" << endl
    << "  bitcc [-o outfile] [-l language] [-I include] files"
    << endl
    << "  bitcc -V|--version" << endl
    << "  bitcc -h|--help" << endl
    << "Debugging options:" << endl
    << "  --showlex --showparse --showpasses" << endl
    << "  --showpassnames --stopafter --version" << endl
    << "  --decorate --dumpafter 'pass' --dumptypes 'pass'" << endl
    << "  --showtypes 'uoc' " << endl
    //<< "  --xmltypes 'uoc' " << endl
    << "  --ppfqns --full-qual-types" << endl
    << "  --raw-tvars --show-maybes --show-all-tccs " << endl
    << "Languages: xmlpp, xmldump, xmltypes, bitcpp, showtypes, c, h, obj" << endl;
}
 
void
fatal()
{
  cerr << "Confused due to previous errors, bailing."
       << endl;
  exit(1);
}


void 
handle_sigsegv(int param)
{
  cerr << "Internal Compiler error: SIGSEGV. "
       << "Please report this problem along with sample source."
       << endl;
  exit(1);
}

int
main(int argc, char *argv[]) 
{
  int c;
  extern int optind;
  int opterr = 0;
  bool userAddedEntryPts = false;

  // Allocate memory for some static members
  Options::showTypesUocs = new CVector<std::string>;
  Options::xmlTypesUocs = new CVector<std::string>;
  Options::entryPts = new CVector<std::string>;
  Options::libPath = new CVector<GCPtr<Path> >;
  UocInfo::searchPath = new CVector<GCPtr<Path> >;
  UocInfo::ifList = new CVector<GCPtr<UocInfo> >;
  UocInfo::srcList = new CVector<GCPtr<UocInfo> >;

  signal(SIGSEGV, handle_sigsegv);

#if 0
  // Shap thinks this is no longer necessary now that he gave up and
  // pulled in libicu.

  // Make sure that we are running in a UNICODE locale:
  setlocale(LC_ALL, "");
  if (strcmp("UTF-8", nl_langinfo(CODESET)) != 0) {
    std::cerr
      << "We appear to be running in the non-unicode locale "
      << nl_langinfo(CODESET)
      << ".\n"
      << "The BitC compiler will only operate correctly "
      << "in a unicode locale.\n";
    exit(1);
  }
#endif

  while ((c = getopt_long(argc, argv, 
			  "e:o:l:VhI:L:",
			  longopts, 0
		     )) != -1) {
    switch(c) {
    case 'V':
      cerr << "Bitc Version: " << BITC_VERSION << endl;
      exit(0);
      break;

    case LOPT_NOSTDINC:
      Options::useStdInc = false;
      break;

    case LOPT_NOSTDLIB:
      Options::useStdLib = false;
      break;

    case LOPT_SHOWPARSE:
      Options::showParse = true;
      break;

    case LOPT_SHOWLEX:
      Options::showLex = true;
      break;

    case LOPT_SHOWPASSES:
      Options::showPasses = true;
      break;

    case LOPT_PPFQNS:
      Options::ppFQNS = true;
      break;

    case LOPT_PPDECORATE:
      Options::ppDecorate = true;
      break;

    case LOPT_NOGC:
      Options::nogc = true;
      break;

    case LOPT_TOP_MUT:
      Options::topMutOnly = true;
      break;

    case LOPT_SHOWPASSNMS:
      {
	std::cerr.width(15);
	std::cerr << left 
		  << "PASS"
		  << "PURPOSE" << std::endl << std::endl;

	for (size_t i = (size_t)pn_none+1; i < (size_t) pn_npass; i++) {
	  std::cerr.width(15);
	  std::cerr << left 
		    << UocInfo::passInfo[i].name
		    << UocInfo::passInfo[i].descrip << std::endl;
	}

	for (size_t i = (size_t)op_none+1; i < (size_t) op_npass; i++) {
	  std::cerr.width(15);
	  std::cerr << left 
		    << UocInfo::onePassInfo[i].name
		    << UocInfo::onePassInfo[i].descrip << std::endl;
	}

	// FIX: What about the onepass passes? 
	exit(0);
      }

    case LOPT_DUMPAFTER:
      {
	for (size_t i = (size_t)pn_none+1; i < (size_t) pn_npass; i++) {
	  if (strcmp(UocInfo::passInfo[i].name, optarg) == 0 ||
	      strcmp("ALL", optarg) == 0)
	    UocInfo::passInfo[i].printAfter = true;
	}

	for (size_t i = (size_t)op_none+1; i < (size_t) op_npass; i++) {
	  if (strcmp(UocInfo::onePassInfo[i].name, optarg) == 0 ||
	      strcmp("ALL", optarg) == 0)
	    UocInfo::onePassInfo[i].printAfter = true;
	}
	
	if (strcmp("midend", optarg) == 0 ||
	    strcmp("ALL", optarg) == 0) 
	  Options::dumpAfterMidEnd = true;
 
	break;
      }

    case LOPT_DUMPTYPES:
      {
	for (size_t i = (size_t)pn_none+1; i < (size_t) pn_npass; i++) {
	  if (strcmp(UocInfo::passInfo[i].name, optarg) == 0 ||
	      strcmp("ALL", optarg) == 0)
	    UocInfo::passInfo[i].typesAfter = true;
	}

	for (size_t i = (size_t)op_none+1; i < (size_t) op_npass; i++) {
	  if (strcmp(UocInfo::onePassInfo[i].name, optarg) == 0 ||
	      strcmp("ALL", optarg) == 0)
	    UocInfo::onePassInfo[i].typesAfter = true;
	}
	
	if (strcmp("midend", optarg) == 0 ||
	    strcmp("ALL", optarg) == 0) 
	  Options::dumpTypesAfterMidEnd = true;

	break;
      }

    case LOPT_STOPAFTER:
      {
	for (size_t i = (size_t)pn_none+1; i < (size_t) pn_npass; i++) {
	  if (strcmp(UocInfo::passInfo[i].name, optarg) == 0 ||
	      strcmp("ALL", optarg) == 0)
	    UocInfo::passInfo[i].stopAfter = true;
	}

	for (size_t i = (size_t)op_none+1; i < (size_t) op_npass; i++) {
	  if (strcmp(UocInfo::onePassInfo[i].name, optarg) == 0 ||
	      strcmp("ALL", optarg) == 0)
	    UocInfo::onePassInfo[i].stopAfter = true;
	}

	break;
      }

    case LOPT_ADVISORY:
      {
	Options::advisory = true;
      }      

    case LOPT_RAW_TVARS:
      {
	Options::rawTvars = true;
	break;
      }

    case LOPT_SHOW_MAYBES:
      {
	Options::showMaybes = true;
	break;
      }
      
    case LOPT_FQ_TYPES:
      {
	Options::FQtypes = true;
	break;
      }

    case LOPT_SA_TCC:
      {
	Options::showAllTccs = true;
	break;
      }

    case LOPT_SHOW_TYPES:
      {
	if(!Options::showTypesUocs->contains(optarg))
	  Options::showTypesUocs->append(optarg);
	break;
      }

    case LOPT_XML_TYPES:
      {
	if(!Options::xmlTypesUocs->contains(optarg))
	  Options::xmlTypesUocs->append(optarg);
	break;
      }

    case 'l':
      {
	if (Options::backEnd) {
	  std::cerr << "Can only specify one output language.\n";
	  exit(1);
	}
	for (size_t i = 0; i < BackEnd::nBackEnd; i++) {
	  if (BackEnd::backends[i].name == optarg) {
	    Options::backEnd = &BackEnd::backends[i]; //&OK
	  }
	}
	if (!Options::backEnd) {
	  std::cerr << "Unknown target language.\n";
	  exit(1);
	}
	break;
      }

    case 'e':
      {	
	userAddedEntryPts = true;
	Options::entryPts->append(optarg);
	break;
      }

    case 'h': 
      {
	help();
	exit(0);
      }

    case 'o':
      Options::outputFileName = optarg;
      break;

    case 'I':
      UocInfo::searchPath->append(new Path(optarg));
      break;

    case 'L':
      Options::libPath->append(new Path(optarg));
      break;

    default:
      opterr++;
      break;
    }
  }
  
  const char *root_dir = getenv("COYOTOS_ROOT");
  const char *xenv_dir = getenv("COYOTOS_XENV");

  if (root_dir) {
    stringstream incpath;
    stringstream libpath;
    incpath << root_dir << "/host/include";
    libpath << root_dir << "/host/lib";

    if (Options::useStdInc)
      UocInfo::searchPath->append(new Path(incpath.str()));
    if (Options::useStdLib)
      Options::libPath->append(new Path(libpath.str()));
  }

  if (xenv_dir) {
    stringstream incpath;
    stringstream libpath;
    incpath << xenv_dir << "/host/include";
    libpath << xenv_dir << "/host/lib";

    if (Options::useStdInc)
      UocInfo::searchPath->append(new Path(incpath.str()));
    if (Options::useStdLib)
      Options::libPath->append(new Path(libpath.str()));
  }

  argc -= optind;
  argv += optind;
  
  if (argc == 0)
    opterr++;

  if (opterr) {
    std::cerr << "Usage: Try bitcc --help" << std::endl;
    exit(0);
  }

  if(Options::backEnd == 0) {
    Options::backEnd = &BackEnd::backends[0];
  }

  if (Options::outputFileName.size() == 0)
    Options::outputFileName = "bitc.out";
  
  /************************************************************/
  /*                        The frontend                      */
  /************************************************************/
  // Process the Prelude:
  sherpa::LexLoc loc = LexLoc();
  UocInfo::importInterface(std::cerr, loc, "bitc.prelude");
  
  // Compile everything
  for(int i = 0; i < argc; i++)
    (void) UocInfo::compileSource(argv[i]);

  /* Per-file backend output after processing frontend, if any */
  bool doFinal = true;
  /* Output for interfaces */
  for(size_t i = 0; i < UocInfo::ifList->size(); i++) {
    GCPtr<UocInfo> puoci = UocInfo::ifList->elem(i);
    
    if (puoci->lastCompletedPass >= Options::backEnd->needPass) {
      if (Options::backEnd->fn)
	Options::backEnd->fn(std::cout, std::cerr, puoci);
    }
    else
      doFinal = false;
  } 

  /* Output for Source modules */
  for(size_t i = 0; i < UocInfo::srcList->size(); i++) {
    GCPtr<UocInfo> puoci = UocInfo::srcList->elem(i);

    if (puoci->lastCompletedPass >= Options::backEnd->needPass){
      if (Options::backEnd->fn)
	Options::backEnd->fn(std::cout, std::cerr, puoci);
    }
    else
      doFinal = false;
  } 

  if (!doFinal)
    exit(1);


  /************************************************************/
  /*   The mid zone, build the polyinstantiate + big-ast      */
  /************************************************************/
  GCPtr<UocInfo> unifiedUOC = new UocInfo("*emit*", true);
  bool midPassOK = true;
  
#if (BITC_CURRENT_MODE == BITC_COMPILER_MODE)
  /* Build the list of things to be instantiated */

  /* First add things that are absolutely necessary, but may be
     unreached at the stage of polyinstantiation. ex: SSA pass inserts
     them */
  Options::entryPts->append("bitc.prelude.__index_lt");
  Options::entryPts->append("bitc.prelude.IndexBoundsError");  

  /* Then add what the user wants to start with (default is
     bitc.main.main unless we are compiling in header mode) */
  if(!userAddedEntryPts &&
     ((Options::backEnd->flags & BK_HDR_MODE) == 0))
    Options::entryPts->append("bitc.main.main");

  /* TEMPORARY: Then add all other top level forms that might cause
     side-effects. */ 
  UocInfo::addAllCandidateEPs(); 
#endif

  // Initialize
  initUnifiedUoc(unifiedUOC);
    
  // Update all of the defForm pointers so that we can find things:
  UocInfo::markAllDefForms();
    
  // Build the master back-end AST. This is done in a way that can be
  // extended incrementally.
  // The batch version is similar to the unbatched instantiator as
  // it calls the real instantiator one entry point at a time over
  // the list of entry points it recieves. The only difference is
  // that the megaENVs is not updated after each entry point if we
  // use batch mode. In an interpreter, if we are instantiating only
  // one entry point, one might as well use instantiate instead of
  // instantiateBatch call.
  midPassOK = unifiedUOC->instantiateBatch(std::cerr, Options::entryPts);
    
  if (Options::dumpAfterMidEnd) {
    std::cerr << "==== DUMPING *unified UOC*"
	      << " AFTER mid-end"
	      << " ====" << std::endl;
    unifiedUOC->PrettyPrint(std::cerr, Options::ppDecorate);
  }
  if (Options::dumpTypesAfterMidEnd) {
    std::cerr << "==== TYPES for *unified UOC*"
	      << " AFTER mid-end"
	      << " ====" << std::endl;
    unifiedUOC->ShowTypes(std::cerr);
    std::cerr <<std::endl << std::endl;
  }
  
  if(!midPassOK) {
    std::cerr << "Exiting due to errors during Instantiation."
	      << std::endl;
    exit(1);    
  }
  
  /************************************************************/
  /*                        The backend                       */
  /************************************************************/

  // Finally perform one-pass backend Functions.
  unifiedUOC->DoBackend();
  
  // If there is any post-gather output, it should be done now.
  if(Options::backEnd->plfn) {
    bool done = Options::backEnd->plfn(std::cout, std::cerr, 
				       unifiedUOC);
    if(!done)
      exit(1);
  }
  
  exit(0);
}

