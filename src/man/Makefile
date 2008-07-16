##########################################################################
#
#  Copyright (C) 2008, Johns Hopkins University.
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

include $(BITC_SRC)/build/make/makerules.mk

MANSRC=$(wildcard *.man)
MAN1SRC=$(wildcard *.1.man)
#MANROOT=$(word 1 $(wildcard $(BITC_SRC)/../host/man /usr/share/man /usr/man))
MANROOT=$(BITC_SRC)/../host/man

TARGETS=$(MANSRC:%.man=$(BUILDDIR)/%.gz)

install: all
	$(INSTALL) -d $(MANROOT)/man1
	for page in $(MAN1SRC:%.man=%); do \
	  $(INSTALL) -m 0644 $(BUILDDIR)/$${page}.gz $(MANROOT)/man1; \
	done

all:	$(TARGETS)

$(BUILDDIR)/%.gz: %.man | $(BUILDDIR)
	gzip -c $< > $@

-include $(BUILDDIR)/.*.m

