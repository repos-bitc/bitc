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

include $(BITC_SRC)/build/make/makerules.mk

INC+=-I$(BITC_XENV)/host/include
SOURCE= $(wildcard src*.bitc)
OBJECTS=$(SOURCE:%.bitc=BUILD/%.o)
BITCC=../../frontend/BUILD/bitcc
RUN=a.out
GCCWARN+=-Wno-unused
ILANG=c
GCLIB=$(BITC_XENV)/host/lib/libgc.a

CFLAGS+=-g -I$(PWD)/../../runtime
c-source: $(SOURCE:%.bitc=%.$(ILANG))
all: $(OBJECTS)

link: $(OBJECTS:%.o=%.out)

run: $(OBJECTS:%.o=%.out)
	for i in $(OBJECTS:%.o=%.out); do echo $$i; $$i; echo""; done

%.out: %.o
	$(NATIVE_GCC) $(CFLAGS) -o $@ $< $(GCLIB)

.PRECIOUS: $(SOURCE:%.bitc=%.$(ILANG))

check.c: $(SOURCE) $(BITCC)
	$(BITCC) -I. -l c -o $@ $(SOURCE)

clean:
	-rm -f *.c BUILD/*.o core.*

-include $(BUILDDIR)/.*.m
