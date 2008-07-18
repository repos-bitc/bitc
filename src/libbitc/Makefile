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
BITC_SRC=..

# This needs to change when we fix the install targets
CFLAGS+=-O4 #-g
GC_INC=

INC+=-I$(BITC_SRC)/runtime -I. -I$(BITC_XENV)/host/include $(GC_INC) 
#INC+=-I../include

BITCC=$(BITC_SRC)/compiler/BUILD/bitcc

include $(BITC_SRC)/build/make/makerules.mk

BITC_SOURCES=\
	$(wildcard *.bits) \
	$(wildcard */*.bits)

BITC_IFS=\
	$(wildcard *.bitc) \
	$(wildcard */*.bitc)

# mkclosure.c is sourced from the appropriate target architecture directory
# by virtue of the VPATH setting below. Contrary to appearences, it is not
# matched by the wildcard.
NOGC_SOURCES=\
	$(wildcard nogc-*.c)
C_SOURCES=\
	$(filter-out $(NOGC_SOURCES),$(wildcard *.c)) \
	mkclosure.c


C_HDR=\
	$(wildcard */*.h)

VPATH=.:$(BITC_TARGET)

RUNTIME_OBJECTS=$(C_SOURCES:%.c=$(BUILDDIR)/%.o)
NOGC_OBJECTS=$(NOGC_SOURCES:%.c=$(BUILDDIR)/%.o)
OBJECTS=$(RUNTIME_OBJECTS) $(NOGC_OBJECTS)

$(OBJECTS): $(BUILDDIR)/bitc-runtime.h

$(BUILDDIR)/bitc-runtime.h: $(BITC_SOURCES) | $(BUILDDIR)
	$(BITCC) $(INC) -h -o $@ bitc-runtime.bitc

all: BUILD/libbitc.a BUILD/libbitc.bita BUILD/libbitc-no-gc.a

install: all
	$(INSTALL) -d $(BITC_ROOT)/host
	$(INSTALL) -d $(BITC_ROOT)/host/lib
	$(INSTALL) -d $(BITC_ROOT)/host/include
	$(INSTALL) -m 0644 BUILD/libbitc.a $(BITC_ROOT)/host/lib
	$(INSTALL) -m 0644 BUILD/libbitc-no-gc.a $(BITC_ROOT)/host/lib
	$(INSTALL) -m 0644 BUILD/libbitc.bita $(BITC_ROOT)/host/lib
	for if in $(BITC_IFS); do \
	  $(INSTALL) -d $(BITC_ROOT)/host/include/`dirname $$if`; \
	  $(INSTALL) -m 0644 $$if $(BITC_ROOT)/host/include/`dirname $$if`; \
	done
	for hdr in $(C_HDR); do \
	  $(INSTALL) -d $(BITC_ROOT)/host/include/`dirname $$hdr`; \
	  $(INSTALL) -m 0644 $$hdr $(BITC_ROOT)/host/include/`dirname $$hdr`; \
	done

BUILD/libbitc.a: $(RUNTIME_OBJECTS)
	ar -crv $@ $(RUNTIME_OBJECTS)

BUILD/libbitc.bita: $(BITC_SOURCES)
	$(BITCC) $(INC) -c -o $@ $(BITC_SOURCES)

BUILD/libbitc-no-gc.a: $(NOGC_OBJECTS)
	ar -crv $@ $(NOGC_OBJECTS)

-include $(BUILDDIR)/.*.m

