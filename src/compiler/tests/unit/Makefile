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
BITC_SRC=../../..
CLEANLIST=*.c BUILD/*.o core.*

include $(BITC_SRC)/build/make/makerules.mk
GC_INC=
GC_LIBDIR=
GC_LIB=-lgc

INC+=-I$(BITC_XENV)/host/include $(GC_INC)
SOURCE= $(wildcard *.bitc)
OBJECTS=$(SOURCE:%.bitc=BUILD/%.o)
BITCC=../../frontend/BUILD/bitcc
LIBBITC=../../libbitc/BUILD/libbitc.a
RUN=a.out
GCCWARN+=-Wno-unused
LIBS=$(GC_LIBDIR) $(LIBBITC) $(GC_LIB)

CFLAGS+=-g -I$(PWD)/../../runtime
c-source: $(SOURCE:%.bitc=%.c)
objects: $(OBJECTS)
all: link

link: $(SOURCE:%.bitc=BUILD/%)

test: $(SOURCE:%.bitc=BUILD/%.test)

comp:
	make run > comp.stdout 2> comp.stderr
	diff comp.stdout comp-out
	diff comp.stderr comp-err
	rm -f comp.stdout comp.stderr

EXPECT_STATUS=0
EXPECT_OUTPUT=/dev/null
USE_INPUT=/dev/null
ARGS=""
TESTOPTS=-v

$(BUILDDIR)/%.test: $(BUILDDIR)/%
	@../testit.sh ${TESTOPTS} -s $(EXPECT_STATUS) -o $(EXPECT_OUTPUT) -i $(USE_INPUT) "$< $(ARGS)"

$(BUILDDIR)/clconv1.test: EXPECT_STATUS=1
$(BUILDDIR)/closure.test: EXPECT_STATUS=3
$(BUILDDIR)/AppLoc.test: EXPECT_STATUS=3
$(BUILDDIR)/newException.test: EXPECT_STATUS=1
$(BUILDDIR)/newException.test: EXPECT_OUTPUT=newException.stdout
$(BUILDDIR)/VecBoundsError.test: EXPECT_STATUS=1
$(BUILDDIR)/VecBoundsError.test: EXPECT_OUTPUT=VecBoundsError.stdout
$(BUILDDIR)/UncaughtException.test: EXPECT_STATUS=1
$(BUILDDIR)/UncaughtException.test: EXPECT_OUTPUT=UncaughtException.stdout
$(BUILDDIR)/wchar.test: EXPECT_OUTPUT=wchar.stdout
$(BUILDDIR)/odd_even_letrec.test: EXPECT_STATUS=139
$(BUILDDIR)/Echo_Args.test: EXPECT_OUTPUT=Echo_Args.stdout
$(BUILDDIR)/echo-char.test: USE_INPUT=echo-char.stdin
$(BUILDDIR)/echo-char.test: EXPECT_OUTPUT=echo-char.stdout
$(BUILDDIR)/hello.test: EXPECT_OUTPUT=hello.stdout
$(BUILDDIR)/Fact-Rec.test: EXPECT_STATUS=120
$(BUILDDIR)/Fact-Loop.test: EXPECT_STATUS=120
$(BUILDDIR)/Fact-Mut.test: EXPECT_STATUS=120
$(BUILDDIR)/intout.test: EXPECT_OUTPUT=intout.stdout
$(BUILDDIR)/io-time.test: ARGS=file.input
$(BUILDDIR)/io-time.test: EXPECT_OUTPUT=io-time.stdout
$(BUILDDIR)/sha1-file.test: ARGS="file.input 128028"
$(BUILDDIR)/sha1-file.test: EXPECT_OUTPUT=sha1-file.stdout
$(BUILDDIR)/sha1-test.test: EXPECT_OUTPUT=sha1-test.stdout 

$(BUILDDIR)/%: $(BUILDDIR)/%.o $(LIBBITC)
	$(NATIVE_GCC) $(CFLAGS) -o $@ $< $(LIBS)

.PRECIOUS: $(SOURCE:%.bitc=%.c)

sha1-file.c: sha1-file.bitc  $(BITCC)
	$(BITCC) $(BITCOPT) -I. -I ../../runtime -o $@ $< ../../lib/crypt/sha1.bitc

sha1-test.c: sha1-test.bitc  $(BITCC)
	$(BITCC) $(BITCOPT) -I. -I ../../runtime -o $@ $< ../../lib/crypt/sha1.bitc

%.c: %.bitc  $(BITCC)
	$(BITCC) $(BITCOPT) -I. -I ../../runtime -o $@ $<

-include $(BUILDDIR)/.*.m
