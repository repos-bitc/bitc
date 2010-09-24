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

/// @file
///
/// @brief Command line driver for the static, whole-program BitC
/// compiler.
///
/// There are three input file extensions:
///
///   .bitc  - bitc interface files
///   .bits  - bitc source files
///   .bito  - bitc "object" files.
///
/// This compiler basically operates in three modes:
///
/// <ol>
/// <li>
///     source->object mode, in which X.bits is checked and re-emitted
///     as X.bito, which is a legal BitC file. We pretend that such
///     files are object files for command line handling purposes.
///
///     This mode can be identified by the presence of the -c option on
///     the command line. If -c is specified, no output will be emitted.
/// </li>
/// <li>
///     As a header file synthesizer, in which an interface file is
///     re-emitted as a C header file, allowing portions of the
///     low-level runtime to be implemented in C.
///
///     This usage can be identified by the presence of the -h option
///     on the command line, but has no effect if -c is also specified.
///
///     Note that -h is a convenience shorthand for --lang h.
/// </li>
/// <li>
///     As a linker, in which some number of .bits and .bito files are
///     combined to form an executable. This is actually the
///     whole-program compiler mode.
///
///     This mode can be identified by the @em absence of either the -c
///     or the -h options on the command line.
/// </li>
/// </ol>

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
#include <iostream>

#include <getopt.h>
#include <langinfo.h>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include "BoostCompat.hxx"
#include <libsherpa/util.hxx>

#include "Version.hxx"
#include "UocInfo.hxx"
#include "Options.hxx"
#include "AST.hxx"
#include "backend.hxx"
#include "Instantiate.hxx"
#include "TvPrinter.hxx"
#include "MixFix.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;

#define BITC_COMPILER_MODE        0x1u
#define BITC_INTERPRETER_MODE     0x2u
#define BITC_CURRENT_MODE BITC_COMPILER_MODE

#define LOPT_SHOWLEX      257   /* Show tokens */
#define LOPT_SHOWPARSE    258   /* Show parse */
#define LOPT_DUMPAFTER    259   /* PP after this pass */
#define LOPT_SHOWPASSNMS  260   /* Show all pass names */
#define LOPT_NOSTDINC     261   /* Do not append std search paths */
#define LOPT_NOSTDLIB     262   /* Do not append std lib paths */
#define LOPT_RAW_TVARS    263   /* Show tvars as is */
#define LOPT_FQ_TYPES     264   /* Show fully quantified types */
#define LOPT_SA_TCC       265   /* Show all Type class constraints */
#define LOPT_SHOWPASSES   266   /* Show passes as they are run */
#define LOPT_PPFQNS       267   /* Show FQNs when pretty printing */
#define LOPT_DUMPTYPES    268   /* Show types after this pass */
#define LOPT_STOPAFTER    269   /* Stop after this pass */
#define LOPT_PPDECORATE   270   /* Decorate pretty printing with types */
#define LOPT_SHOW_TYPES   271   /* Dump types a particular uoc only */
#define LOPT_XML_TYPES    272   /* Dump XML types */
#define LOPT_NOGC         273   /* NO GC mode */
#define LOPT_NOPRELUDE    274   /* Don't process prelude */
#define LOPT_HEURISTIC    275   /* Use Heuristic Inference */
#define LOPT_HELP         276   /* Display usage information. */
#define LOPT_EMIT         277   /* Specify the desired output language. */
#define LOPT_SYSTEM       278   /* Specify the desired output
                                   language. */
#define LOPT_NOALLOC      279   /* Statically reject heap-allocating
                                   operations and constructs */
#define LOPT_MIXDEBUG     280   /* Debug level for the mixfix parser */

struct option longopts[] = {
  /*  name,           has-arg, flag, val           */
  // Decorate pretty printing with types */
  { "decorate",             0,  0, LOPT_PPDECORATE },
  // Dump the AST after the named pass
  { "dumpafter",            1,  0, LOPT_DUMPAFTER },
  // Dump symbol types after the named pass
  { "dumptypes",            1,  0, LOPT_DUMPTYPES },
  // Specify the desired output language.
  { "emit",                 1,  0, LOPT_EMIT },
  // Show fully quantified types in output
  { "full-qual-types",      0,  0, LOPT_FQ_TYPES },
  // Print command line help and exit
  { "help",                 0,  0, LOPT_HELP },
  // Use heuristic inference instead of complete inference
  { "heuristic-inf",        0,  0, LOPT_HEURISTIC },
  // Set the debug level for the mixfix parser:
  { "mixfix-debug",         1,  0, LOPT_MIXDEBUG },
  // Disallow any construct that would cause heap allocation at runtime.
  { "no-alloc",             0,  0, LOPT_NOALLOC },
  // Do not link the garbage collector.
  { "no-gc",                0,  0, LOPT_NOGC },
  // Do not load the standard BitC prelude.
  { "no-prelude",           0,  0, LOPT_NOPRELUDE },
  // Do not add standard include locations to the include search path.
  { "nostdinc",             0,  0, LOPT_NOSTDINC },
  // Do not add standard library locations to the library/module search path.
  { "nostdlib",             0,  0, LOPT_NOSTDLIB },
  // Show FQNs when pretty printing
  { "ppfqns",               0,  0, LOPT_PPFQNS },
  // Show tvars without pretty-printing: useful for Debugging
  { "raw-tvars",            0,  0, LOPT_RAW_TVARS },
  // Show all Type class constraints, including subsumed ones
  { "show-all-tccs",        0,  0, LOPT_SA_TCC },
  // Print tokens as they are accepted. Primarily useful for debugging
  // parse errors.
  { "showlex",              0,  0, LOPT_SHOWLEX },
  // Print parse reductions as they occur.
  { "showparse",            0,  0, LOPT_SHOWPARSE },
  // Print the names of all passes as they are executed
  { "showpasses",           0,  0, LOPT_SHOWPASSES },
  // Print the names of all passes
  { "showpassnames",        0,  0, LOPT_SHOWPASSNMS },
  // Show types as they appear after the named path
  { "showtypes",            1,  0, LOPT_SHOW_TYPES },
  // Stop processing after the named pass
  { "stopafter",            1,  0, LOPT_STOPAFTER },
  // Change the standard search prefix directory, similar to gcc --system
  { "system",               1,  0, LOPT_SYSTEM },
  // Run verbosely. Not clear that this is actually being used anyware.
  { "verbose",              0,  0, 'v' },
  // Print compiler version and exit
  { "version",              0,  0, 'V' },
  // Dump symbol types in XML form after the named pass. This should
  // probably be obsoleted.
  { "xmltypes",             1,  0, LOPT_XML_TYPES },
#if 0
  /* Options that have short-form equivalents: */
  { "debug",                0,  0, 'd' },

  { "dispatchers",          0,  0, 's' },
  { "entry",                1,  0, 'e' },
  { "execdir",              1,  0, 'X' },
  { "header",               1,  0, 'h' },
  { "include",              1,  0, 'I' },
  { "index"  ,              1,  0, 'n' },
  { "outdir",               1,  0, 'D' },
  { "output",               1,  0, 'o' },
  { "verbose",              0,  0, 'v' },
#endif
  {0,                       0,  0, 0}
};

/// @brief Print usage information and exit.
void
help()
{
  std::cerr
    << "Common Usage:" << endl
    //    << "  bitcc [-I include] -c file1.bits ...\n"
    << "  bitcc [-I include] [-o outfile.bito] -c file.bits\n"
    // << "  bitcc [-I include] -h file.bitc ...\n"
    << "  bitcc [-I include] [-o outfile.h] -h file.bits\n"
    << "  bitcc [file1.bito|library.a] ... [-o exefile]\n"
    << "  bitcc -V|--version\n"
    << "  bitcc --help\n"
    << "\n"
    << "Debugging options:\n"
    << "  --showlex --showparse --showpasses\n"
    << "  --stopafter 'pass'\n"
    << "  --decorate --dumpafter 'pass' --dumptypes 'pass'\n"
    << "  --showtypes 'uoc' \n"
    << "  --emit 'Language'\n" 
    //<< "  --xmltypes 'uoc' \n"
    << "  --ppfqns --full-qual-types\n"
    << "  --raw-tvars --show-maybes --show-all-tccs \n"
    << "\n"
    << "Languages: xmlpp, xmldump, xmltypes, bitcpp, showtypes, bito,\n"
    << "           c, h, obj\n"
    << flush;
}

/// @brief Cease processing after fatal error
void
fatal()
{
  cerr << "Confused due to previous errors, bailing."
       << endl;
  exit(1);
}


/// @brief Record whether we have seen any BitC input file yet.
///
/// This determines whether a command line link argument should be
/// presented to the linker before or after the object file comprising
/// the BitC portion of the program.
static bool SawFirstBitcInput = false;

/// @brief Add an argument to be passed to the linker either before or
/// after the generated BitC object file.
void
AddLinkArgumentForGCC(const std::string& s)
{
  if (SawFirstBitcInput)
    Options::LinkPostOptionsGCC.push_back(s);
  else {
    Options::LinkPreOptionsGCC.push_back(s);
  }
}

/// @brief Add an argument to be passed to the compiler before or
/// after the generated BitC C file.
void
AddCompileArgumentForGCC(const std::string& s)
{
  if (SawFirstBitcInput) {
    cerr << "Compiler options must appear before the first BitC input file."
         << endl;
    exit(1);
  }

  Options::CompilePreOptionsGCC.push_back(s);
}

/// @brief Find the backend corresponding to the specified target type.
BackEnd *
FindBackEnd(const char *nm)
{
  for (size_t i = 0; i < BackEnd::nBackEnd; i++) {
    if (BackEnd::backends[i].name == nm)
      return &BackEnd::backends[i]; //&OK
  }

  return NULL;
}

/// @brief Catch certain memory errors that plagued us for a while in
/// GCPtr.
///
/// These were subsequently resolved, but reporting borkage is not a
/// bad thing.
void
handle_sigsegv(int param)
{
  cerr << "Internal Compiler error: SIGSEGV. "
       << "Please report this problem along with sample source."
       << endl;
  exit(1);
}

/// @brief Resolve a library name to a path.
///
/// This is called when we see <code>-lname</code> or <code>-l
/// name</code> on the command line. The @em name will be passed as an
/// argument here. What we need to do here is
/// check the currently known library paths for a resolution. If we
/// find a file matching "libname.bita" on the search path, we add it
/// to the list of inputs.
///
/// In some cases, -lmumble will indicate simultaneously a need to
/// add an input file named ..../libmumble.bita and also an archive
/// library named .../libmumble.a. This arises in libbitc, for
/// example, where some of the library is implemented in C.
///
/// Unfortunately, this means that the @em absence of
/// .../libmumble.bita does not reliably indicate an error.
filesystem::path
ResolveLibPath(std::string name)
{
  string fullNm = "lib" + name + ".bita";

  for (size_t i = 0; i < Options::libDirs.size(); i++) {
    filesystem::path testPath = Options::libDirs[i] / fullNm;
    if (filesystem::exists(testPath)) {
      if (!filesystem::is_regular_file(testPath)) {
        std::cerr << "bitcc: error: \"-l" << name
                  << "\" resolves to \""
                  << testPath.string()
                  << "\", which is not a regular file."
                  << endl;
        exit(1);
      }

      return testPath;
    }
  }

  return filesystem::path();
}

int
main(int argc, char *argv[])
{
  int c;
  //  extern int optind;
  int opterr = 0;

  signal(SIGSEGV, handle_sigsegv);

  /// Note the "-" at the start of the getopt_long option string. In
  /// order to generate behavior that is compatible with other
  /// compilers (and linkers), we need to process library archives in
  /// strict left-to-right form, without regard to whether they appear
  /// in .../libfoo.a or -lbar format. That is: -lbar must be treated
  /// as if macro-expanded
  ///
  /// But it is possible to see things like:
  ///
  ///    bitc a.bito b.bito libfoo.a -L path -lbar -o out
  ///
  /// This  means that there is left-context sensitivity to
  /// consider, because the inputs are:
  ///
  ///    a.bito b.bito libfoo.a ResolveLibPath("bar")
  ///
  /// and the behavior of ResolveLibPath relies on having processed
  /// the -L options appearing to its left and NOT any -L options
  /// appearing to its right. To make matters even more fun, we have
  /// to selectively accumulate some of these options to be passed
  /// along to gcc, and given the interspersal we need to do so in an
  /// order-preserving way.

  while ((c = getopt_long(argc, argv,
                          "-e:o:O::l:VvcghI:L:",
                          longopts, 0
                     )) != -1) {
    switch(c) {
    case 1:
      {
        std::string sfx = filesystem::extension(optarg);

        if (
            // Interface files. Probably should not appear on the
            // command line, but this may be a case of obsolete
            // usage. Allow it for now.
            (sfx == ".bitc")
            // BitC source file.
            || (sfx == ".bits")
            // BitC "object" file.
            || (sfx == ".bito")
            // BitC archive file.
            || (sfx == ".bita")) {

          SawFirstBitcInput = true;
          Options::inputs.push_back(optarg);
        }
        else
          // Else it is something to be passed through to GCC:
          AddLinkArgumentForGCC(optarg);

        break;
      }

    case 'V':
      cerr << "Bitc Version: " << BITC_VERSION << endl;
      exit(0);
      break;

    case LOPT_NOSTDINC:
      Options::useStdInc = false;
      AddCompileArgumentForGCC("--nostdinc");
      AddLinkArgumentForGCC("--nostdinc");
      break;

    case LOPT_NOSTDLIB:
      Options::useStdLib = false;
      AddLinkArgumentForGCC("--nostdlib");
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
      Options::noGC = true;
      break;

    case LOPT_NOALLOC:
      Options::noAlloc = true;
      break;

    case LOPT_NOPRELUDE:
      Options::noPrelude = true;
      break;

    case LOPT_HEURISTIC:
      Options::heuristicInference=true;
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

        std::cerr << left
                  << "Polyinst"
                  << "Template-like instantiation of polymorphic definitions" << std::endl;

        for (size_t i = (size_t)op_none+1; i < (size_t) op_npass; i++) {
          std::cerr.width(15);
          std::cerr << left
                    << UocInfo::onePassInfo[i].name
                    << UocInfo::onePassInfo[i].descrip << std::endl;
        }

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

    case LOPT_RAW_TVARS:
      {
        Options::rawTvars = true;
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
        Options::showTypesUocs.insert(optarg);
        break;
      }

    case LOPT_XML_TYPES:
      {
        Options::xmlTypesUocs.insert(optarg);
        break;
      }

    case 'v':
      Options::verbose++;
      if (Options::verbose > 1) {
        AddCompileArgumentForGCC("-v");
        AddLinkArgumentForGCC("-v");
      }
      break;

    case 'c':
      {
        if (Options::backEnd) {
          std::cerr << "Can only specify one output language.\n";
          exit(1);
        }

        // Issue: if we are passed a .c file, shouldn't we pass that
        // along to GCC in this case? Problem: what if there aren't
        // any inputs exclusively for GCC?
        //
        // AddArgumentForGCC("-c");

        Options::backEnd = FindBackEnd("bito");
        break;
      }

    case 'h':
      {
        if (Options::backEnd) {
          std::cerr << "Can only specify one output language.\n";
          exit(1);
        }

        Options::backEnd = FindBackEnd("h");
        break;
      }

    case LOPT_EMIT:
      {
        if (Options::backEnd) {
          std::cerr << "Can only specify one output language.\n";
          exit(1);
        }

        Options::backEnd = FindBackEnd(optarg);
        if (!Options::backEnd) {
          std::cerr << "Unknown target language.\n";
          exit(1);
        }

        break;
      }

    case LOPT_MIXDEBUG:
      {
        if (Options::mixfixDebug > 0) {
          // already set
          std::cerr << "Warning: overriding previous setting of mixfix-debug.\n";
        }

        // Validation:
        for (const char *arg = optarg; *arg; arg++) {
          if(!isdigit(*arg)) {
            std::cerr << "Argument to --mixfix-debug must be a number.\n";
            exit(1);
          }
        }

        Options::mixfixDebug = strtoul(optarg, 0, 0);

        break;
      }

    case 'e':
      {        
        Options::entryPts.insert(FQName(optarg));
        break;
      }

    case LOPT_HELP:
      {
        help();
        exit(0);
      }

    case 'o':
      Options::outputFileName = optarg;

      AddLinkArgumentForGCC("-o");
      AddLinkArgumentForGCC(optarg);

      break;

    case 'I':
      AddCompileArgumentForGCC("-I");
      AddCompileArgumentForGCC(optarg);
      AddLinkArgumentForGCC("-I");
      AddLinkArgumentForGCC(optarg);
      UocInfo::searchPath.push_back(optarg);
      break;

    case LOPT_SYSTEM:
      Options::SystemDirs.push_back(optarg);
      break;

    case 'l':
      {
        AddLinkArgumentForGCC("-l");
        AddLinkArgumentForGCC(optarg);

        filesystem::path path = ResolveLibPath(optarg);
        if (!path.empty())
          Options::inputs.push_back(path.string());
        break;
      }

    case 'L':
      AddLinkArgumentForGCC("-L");
      AddLinkArgumentForGCC(optarg);

      Options::libDirs.push_back(optarg);
      break;

    case 'g':
      {
        AddCompileArgumentForGCC("-g");
        AddLinkArgumentForGCC("-g");
        break;
      }

    case 'O':                        // a.k.a. -O2
      {
        std::string optlevel = "-O";
        if (optarg)
          optlevel += optarg;

        AddCompileArgumentForGCC(optlevel);
        AddLinkArgumentForGCC(optlevel);
      }

      break;

    default:
      opterr++;
      break;
    }
  }

  mixfix_init();

  // Select default backend if none chosen otherwise.
  if (Options::backEnd == 0)
    Options::backEnd = &BackEnd::backends[0];

  for (size_t i = 0; i < Options::SystemDirs.size(); i++) {
    filesystem::path incPath = Options::SystemDirs[i] / "include";

    UocInfo::searchPath.push_back(incPath);
    Options::CompilePreOptionsGCC.push_back("-I");
    Options::CompilePreOptionsGCC.push_back(incPath.string());

    /// @bug: What about lib64?
    std::string lib_leaf_name = filesystem::path(AUTOCONF_LIBDIR).leaf();
    filesystem::path libPath = Options::SystemDirs[i] / lib_leaf_name;

    Options::libDirs.push_back(libPath);
    Options::LinkPostOptionsGCC.push_back("-L");
    Options::LinkPostOptionsGCC.push_back(libPath.string());
  }

  // Since BitC may hot have been compiled for install into a standard
  // location, add AUTOCONF_LIBDIR to libDirs and also to the link
  // options:

  if (Options::useStdLib) {
    Options::libDirs.push_back(filesystem::path(AUTOCONF_LIBDIR));
    Options::LinkPostOptionsGCC.push_back("-L");
    Options::LinkPostOptionsGCC.push_back(AUTOCONF_LIBDIR);
  }

  if (Options::useStdLib) {
    filesystem::path path = ResolveLibPath("bitc");
    if (path.empty() && (Options::backEnd->flags & BK_LINKING)) {
      cerr << "Cannot find bitc standard library, which is needed"
           << endl;
      exit(1);
    }
    if (!path.empty())
      Options::inputs.push_back(path.string());
  }

  /* From this point on, argc and argv should no longer be consulted. */

  if (Options::inputs.empty())
    opterr++;

  if (opterr) {
    std::cerr << "Usage: Try bitcc --help" << std::endl;
    exit(0);
  }

  if (Options::outputFileName.size() == 0)
    Options::outputFileName = "bitc.out";

  /************************************************************/
  /*                UOC Parse and Validate                    */
  /************************************************************/
  // Process the Prelude:

  /* Per-file backend output after processing frontend, if any */
  bool doFinal = true;

  // FIX: TEMPORARY
  if (!Options::noPrelude) {
    sherpa::LexLoc loc = LexLoc();
    if (!UocInfo::importInterface(std::cerr, loc, "bitc.prelude"))
      doFinal = false;
  }

  // Compile everything
  for (size_t i = 0; i < Options::inputs.size(); i++)
    if (!UocInfo::CompileFromFile(Options::inputs[i], true))
      doFinal = false;

  /* Backend-defined processing for each interface module */
  for (UocMap::iterator itr = UocInfo::ifList.begin();
      itr != UocInfo::ifList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;

    if (puoci->lastCompletedPass >= Options::backEnd->needPass) {
      if (Options::backEnd->fn)
        Options::backEnd->fn(std::cout, std::cerr, puoci);
    }
    else
      doFinal = false;
  }

  /* Backend-defined processing for each source module */
  for (UocMap::iterator itr = UocInfo::srcList.begin();
      itr != UocInfo::srcList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;

    if (puoci->lastCompletedPass >= Options::backEnd->needPass){
      if (Options::backEnd->fn)
        Options::backEnd->fn(std::cout, std::cerr, puoci);
    }
    else
      doFinal = false;
  }

  /* We have completed all of the per-UOC passes. Assuming that we did
   * so successfully, run any required mid-end function:
   */
  if (doFinal) {
    if (Options::backEnd->midfn) {
      Options::backEnd->midfn(std::cout, std::cerr);
    }
  }

  if (!doFinal)
    exit(1);

  if (Options::backEnd->flags & BK_UOC_MODE)
    exit(0);

  /************************************************************/
  /*   The mid zone, build the polyinstantiate + big-ast      */
  /************************************************************/
#if (BITC_CURRENT_MODE == BITC_COMPILER_MODE)
  /* Build the list of things to be instantiated */

  /// If we are not compiling in header mode, we need to emit code for
  /// the primary entry point. In the absence of some other
  /// specification by the user, that entry point is
  /// bitc.main.main. All else will follow from that as
  /// polyinstantiation proceeds.
  ///
  /// Header mode is a bit different. In that mode we are converting
  /// interfaces into header files, and there is no particular entry
  /// point to emit.
  if (Options::entryPts.empty() &&
     ((Options::backEnd->flags & BK_HDR_MODE) == 0))
    Options::entryPts.insert(FQName("bitc.main","main"));

  /** Add all other top level forms that might cause
   * side-effects. These are the top-level initializers. This is a
   * temporary expedient. We need to re-examine the rules for legal
   * top-level initialization.
   */
  UocInfo::addAllCandidateEPs();
#endif

  // Update all of the defForm pointers so that we can find things:
  UocInfo::findAllDefForms();

  /* Create a new unit of compilation that will become the grand,
     unified UoC */
  shared_ptr<UocInfo> unifiedUOC = UocInfo::CreateUnifiedUoC();

  // Build the master back-end AST. This is done in a way that can be
  // extended incrementally.
  // The batch version is similar to the unbatched instantiator as
  // it calls the real instantiator one entry point at a time over
  // the list of entry points it recieves. The only difference is
  // that the megaENVs is not updated after each entry point if we
  // use batch mode. In an interpreter, if we are instantiating only
  // one entry point, one might as well use instantiate instead of
  // instantiateBatch call.
  bool midPassOK = unifiedUOC->instantiateBatch(std::cerr, Options::entryPts);

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

  if (!midPassOK) {
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
  if (Options::backEnd->plfn) {
    bool done = Options::backEnd->plfn(std::cout, std::cerr,
                                       unifiedUOC);
    if (!done)
      exit(1);
  }

  exit(0);
}

