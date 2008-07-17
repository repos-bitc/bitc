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

CLEANLIST=*.cxx cgen
OBJECTS=BUILD/cgen.o BUILD/GC.o
SOURCE= toc.bitc
ALLFILES=$(wildcard *.bitc)
OLANG=c
BITCC=$(BITC_SRC)/compiler/frontend/BUILD/bitcc
BITCINC=-I $(BITC_SRC)/runtime -I.
BITCLIB=-L $(BITC_SRC)/libbitc
RUN=a.out
CFLAGS+=-g -I$(PWD)/$(BITC_SRC)/runtime -I/coyotos/host/include
LIBGC=-lgc 
LIBBITC=$(BITC_SRC)/libbitc/BUILD/libbitc.a

include $(BITC_SRC)/build/make/makerules.mk

# The new inference algorithm is in promitive stage, trying some test cases.
new:
#	$(BITCC) -I $(BITC_SRC)/runtime --no-prelude --full-qual-types --showtypes fnxn fnxn.bitc
	$(BITCC) -I $(BITC_SRC)/runtime -o fnxn.c --full-qual-types --dumptypes typecheck fnxn.bitc

all:
	$(BITCC) $(BITCINC) $(BITCLIB) -l $(OLANG) $(BITCOPT) $(SOURCE)

obj: $(SOURCE:.bitc=.c)
	$(NATIVE_GCC) $(CFLAGS) -o $(SOURCE:.bitc=.out) $(SOURCE:.bitc=.c) $(LIBGC) $(LIBBITC)

%.$(OLANG): %.bitc  $(BITCC)
	$(BITCC) $(BITCINC) $(BITCLIB) -l $(OLANG) $(BITCOPT) -o $@ $<

#GC.o: $(BITC_SRC)/runtime/GC.cxx

cgen: $(OBJECTS)
	$(GPLUS) $(GPLUSFLAGS) $(OBJECTS) $(LIBS) -o $@

run: all $(RUN)
	./$(RUN)

#%.c: %.bitc  $(BITCC)
#	$(BITCC) $(BITCINC) $(BITCLIB) -l c -o $@ $< 

#clean:
#	-rm -f *.c *.o

-include $(BUILDDIR)/.*.m
