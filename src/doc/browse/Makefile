#
# Copyright (C) 2010, Jonathan S. Shapiro.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or
# without modification, are permitted provided that the following
# conditions are met:
#
#   - Redistributions of source code must contain the above 
#     copyright notice, this list of conditions, and the following
#     disclaimer. 
#
#   - Redistributions in binary form must reproduce the above
#     copyright notice, this list of conditions, and the following
#     disclaimer in the documentation and/or other materials 
#     provided with the distribution.
#
#   - Neither the names of the copyright holders nor the names of any
#     of any contributors may be used to endorse or promote products
#     derived from this software without specific prior written
#     permission. 
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

default: package
BITC_SRC=../..
OSDOC_NAVCONTEXT="Doxygen"
OSDOC_XHTML_SRC=$(patsubst %.xhtml,%,$(wildcard *.xhtml))
OSDOC_OHTML_SRC=$(patsubst %.ohtml,%,$(wildcard *.ohtml))
HTML_TOPNAV=projects.html

DOCSTYLES= #docstyles.css

include $(BITC_SRC)/build/make/makerules.mk

all: doxygen-stuff

install: all

.PHONY: doxygen-stuff
doxygen-stuff:
	(cd $(BITC_SRC)/compiler;make doxygen)
	(cd $(BITC_SRC)/libbitc;make doxygen)
	rsync --delete -avz $(BITC_SRC)/compiler/doxygen/html/ $(BITC_SRC)/doc/browse/compiler
	rsync --delete -avz $(BITC_SRC)/libbitc/doxygen/html/ $(BITC_SRC)/doc/browse/libbitc

clean.doxygen-stuff:
	rm -rf compiler libbitc
