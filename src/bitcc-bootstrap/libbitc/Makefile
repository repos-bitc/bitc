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
CFLAGS+=-g
GC_INC=

INC+=-I../runtime -I. -I$(BITC_XENV)/host/include $(GC_INC) 
INC+=-I../include

include $(BITC_SRC)/build/make/makerules.mk

BITC_SOURCES=\
	$(wildcard ../runtime/bitc/*.bitc) \
	$(wildcard ../runtime/bitc/*/*.bitc)

C_SOURCES=\
	$(wildcard *.c) \
	mkclosure.c

VPATH=.:$(BITC_TARGET)

OBJECTS=$(C_SOURCES:%.c=$(BUILDDIR)/%.o)

$(OBJECTS): $(BUILDDIR)/bitc-runtime.h

$(BUILDDIR)/bitc-runtime.h: $(BUILDDIR) $(BITC_SOURCES)
	../frontend/BUILD/bitcc $(INC) -l h -o $@ bitc-runtime.bitc

all: BUILD/libbitc.a

install: all
	$(INSTALL) -d $(BITC_ROOT)/host
	$(INSTALL) -d $(BITC_ROOT)/host/lib
	$(INSTALL) -m 0644 BUILD/libbitc.a $(BITC_ROOT)/host/lib

BUILD/libbitc.a: $(OBJECTS)
	ar -crv $@ $(OBJECTS)

-include $(BUILDDIR)/.*.m

