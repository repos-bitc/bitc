#
# Copyright (C) 2004, Jonathan S. Shapiro.
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
BITC_SRC=../../..

.SECONDARY:

#OSDOC_XML_SRC=$(patsubst %.xml,%,$(wildcard *.xml)) \
#              $(patsubst %.txml,%,$(wildcard *.txml)) 

TEXINPUTS=":../latex_classes::"
export TEXINPUTS

XSL_BTYPE=../txml_proc/btypes-osdoc.xsl
FIX_LTX='../txml_proc/fixup.pl'

MY_XML=$(patsubst %.xml,%,$(wildcard *.xml))
MY_TYPE_XML=$(patsubst %.txml,%,$(wildcard *.txml))

OSDOC_XML_SRC=$(MY_XML) $(MY_TYPE_XML)
OSDOC_SBOX_SRC=$(wildcard *.sbox)

CLEANLIST=bitc-grammar.c
CLEANLIST+=$(MY_TYPE_XML:%=%.xml)

include $(BITC_SRC)/build/make/makerules.mk
OSDOC_TARGETS+=%.ps

all:

install: all


$(MY_TYPE_XML:%=%.xml): %.xml : %.txml $(XSL_BTYPE) include/%/$(wildcard *.xmli)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

$(MY_TYPE_XML:%=%.ltx): %.ltx : %.xml.ltx
	$(FIX_LTX) $< |\
	awk '$$1 ~ /\\begin{alltt}/ {if(pre != ""){print pre;} pre = $$0;}\
	     $$1 !~ /\\begin{alltt}/ {print pre; pre = $$0;}\
	     END {print $$0;}' - |\
	sed '/./,/^$$/!d' - > $@
