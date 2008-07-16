##########################################################################
#
#  Copyright (C) 2006, Johns Hopkins University.
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or
#  without modification, are permitted provided that the following
#  conditions are met:
#
#    - Redistributions of source code must contain the above 
#      copyright notice, this list of conditions, and the following
#      disclaimer. 
#
#    - Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions, and the following
#      disclaimer in the documentation and/or other materials 
#      provided with the distribution.
#
#    - Neither the names of the copyright holders nor the names of any
#      of any contributors may be used to endorse or promote products
#      derived from this software without specific prior written
#      permission. 
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
##########################################################################

default: package
BITC_SRC=../..

# This needs to change when we fix the install targets
CXXFLAGS+=-g
#CXXFLAGS+=-Wuninitialized -O
#CXXFLAGS+=-finline-limit=8000 --param inline-unit-growth=400 --param max-inline-insns-single=16000 --param large-function-growth=4000
GC_INC=
GC_LIBDIR=
GC_LIB=-lgccpp -lgc

INC=-I. -I.. -I../.. $(GC_INC)
DEF+= $(LEXDEF)

include $(BITC_SRC)/build/make/makerules.mk

CLEANLIST=y.tab.c y.tab.h y.output
#CLEANLIST+=SexprLexer.c
#CLEANLIST=./AST.cxx ./AST.hxx

LIBS= -L../../libsherpa/BUILD $(GC_LIBDIR) $(LIBSHERPA) -lgmp -licuuc
LIBS+=$(GC_LIB)

OBJECTS=\
	$(BUILDDIR)/SexprLexer.o \
	$(BUILDDIR)/LitValue.o \
	$(BUILDDIR)/AST.o \
	$(BUILDDIR)/ASTimpl.o \
	$(BUILDDIR)/Environment.o \
	$(BUILDDIR)/die.o \
	$(BUILDDIR)/bitcc.o \
	$(BUILDDIR)/Instantiate.o \
	$(BUILDDIR)/BitcParser.o \
	$(BUILDDIR)/DefreprXform.o \
	$(BUILDDIR)/DefreprCheck.o \
	$(BUILDDIR)/BeginSimp.o \
	$(BUILDDIR)/Simplify.o \
	$(BUILDDIR)/FQName.o \
	$(BUILDDIR)/ASTaux.o \
	$(BUILDDIR)/XML-pp.o \
	$(BUILDDIR)/BitC-pp.o \
	$(BUILDDIR)/Symtab.o \
	$(BUILDDIR)/Unify.o\
	$(BUILDDIR)/UocInfo.o \
	$(BUILDDIR)/Trail.o \
	$(BUILDDIR)/Type.o \
	$(BUILDDIR)/TopInit.o \
	$(BUILDDIR)/TypeInfer.o \
	$(BUILDDIR)/TypeInferUtil.o \
	$(BUILDDIR)/TypePoly.o \
	$(BUILDDIR)/TypeScheme.o \
	$(BUILDDIR)/Typeclass.o \
	$(BUILDDIR)/TypeValRes.o \
	$(BUILDDIR)/Type-pp.o \
	$(BUILDDIR)/Type-debug-pp.o \
	$(BUILDDIR)/Type-mangle.o \
	$(BUILDDIR)/Type-to-ast.o \
	$(BUILDDIR)/Type-xml.o \
	$(BUILDDIR)/Type-size.o \
	$(BUILDDIR)/TypeMut.o \
	$(BUILDDIR)/Constraints.o \
	$(BUILDDIR)/Solver.o \
	$(BUILDDIR)/LocChk.o \
	$(BUILDDIR)/NoAllocCheck.o \
	$(BUILDDIR)/Special.o\
	$(BUILDDIR)/TvPrinter.o \
	$(BUILDDIR)/backend.o \
	$(BUILDDIR)/inter-pass.o \
	$(BUILDDIR)/Clconv.o\
	$(BUILDDIR)/Tail.o \
	$(BUILDDIR)/InstLamHoist.o \
	$(BUILDDIR)/SSA.o \
	$(BUILDDIR)/bito.o \
	$(BUILDDIR)/gen-c.o

TARGETS=$(BUILDDIR)/bitcc

all: $(TARGETS)

run: all

install: all
	$(INSTALL) -d $(BITC_ROOT)/host
	$(INSTALL) -d $(BITC_ROOT)/host/bin
	$(INSTALL) -m 0755 $(TARGETS) $(BITC_ROOT)/host/bin

#This is a placeholder.

$(BUILDDIR)/bitcc: AST.hxx $(OBJECTS)
	$(GPLUS) $(GPLUSFLAGS) $(OBJECTS) $(LIBS) -o $@

$(BUILDDIR)/%.o: %.cxx
	$(CXX_DEP)
	$(CXX_BUILD)

$(BUILDDIR)/BitcParser.o: $(BUILDDIR)/BitcParser.cxx
	$(CXX_DEP)
	$(CXX_BUILD) 

$(BUILDDIR)/BitcParser.cxx: $(MAKE_BUILDDIR)
$(BUILDDIR)/BitcParser.cxx $(BUILDDIR)/BitcParser.hxx: BitcParser.y $(MAKE_BUILDDIR) 
	bison -t -p bitc -v -d -o $(BUILDDIR)/BitcParser.cxx -y BitcParser.y

AST.cxx: AST.ast AST.hxx
	$(ASTMAKER) -s AST.ast

AST.hxx: AST.ast
	$(ASTMAKER) -h AST.ast

# Following is a dummy dependency. It ensures that the header file
# containing token numbers gets generated before we try to compile
# SexprLexer.cxx
SexprLexer.cxx: $(BUILDDIR)/BitcParser.cxx
#$(BUILDDIR)/SexprLexer.cxx: $(BUILDDIR)/BitcParser.cxx

#$(BUILDDIR)/SexprLexer.o: $(BUILDDIR)/SexprLexer.cxx
#	$(CXX_DEP)
#	$(CXX_BUILD) 	

#$(BUILDDIR)/SexprLexer.cxx: SexprLexer.l $(MAKE_BUILDDIR)
#	flex -d -o$@ SexprLexer.l


-include $(BUILDDIR)/.*.m

