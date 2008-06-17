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

MY_XML=changes closures PLOS2006-shap PLOS2006 polyinst 
MY_XML+=relnotes-0.10 relnotes-0.9 spec
MY_TYPE_XML=c-formal e-formal h-formal mut-formal 
MY_TYPE_XML+=complete core mutinfer sys-infer 

OSDOC_XML_SRC=$(MY_XML) $(MY_TYPE_XML)

CLEANLIST=bitc-grammar.c mutinfer.xml sys-infer.xml 
CLEANLIST+=c-formal.xml h-formal.xml e-formal.xml mut-formal.xml
CLEANLIST+=core.xml complete.xml
XSL_BTYPE=btypes-osdoc.xsl
FIX_LTX='./fixup.pl'

#Do not add these .xmli files into clean list
SYSINFER_INCLUDE=include/sys-infer/$(wildcard *.xmli)
COMPLETE_INCLUDE=include/complete/$(wildcard *.xmli)
CORE_INCLUDE=include/core/$(wildcard *.xmli)
H_FORMAL_INCLUDE=include/h-formal/$(wildcard *.xmli)
C_FORMAL_INCLUDE=include/c-formal/$(wildcard *.xmli)
E_FORMAL_INCLUDE=include/e-formal/$(wildcard *.xmli)
MUT_FORMAL_INCLUDE=include/mut-formal/$(wildcard *.xmli)

include $(BITC_SRC)/build/make/makerules.mk
OSDOC_TARGETS+=%.ps

all:

install: all

#%.xmli: %.btype $(XSL_BTYPE)
#	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

mutinfer.xml: mutinfer.txml $(XSL_BTYPE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

h-formal.xml: h-formal.txml $(XSL_BTYPE) $(H_FORMAL_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

c-formal.xml: c-formal.txml $(XSL_BTYPE) $(C_FORMAL_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

e-formal.xml: e-formal.txml $(XSL_BTYPE) $(E_FORMAL_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

mut-formal.xml: mut-formal.txml $(XSL_BTYPE) $(MUT_FORMAL_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

sys-infer.xml: sys-infer.txml $(XSL_BTYPE) $(SYSINFER_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

core.xml: core.txml $(XSL_BTYPE) $(CORE_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

complete.xml: complete.txml $(XSL_BTYPE) $(COMPLETE_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

%.ps: %.ltx
	$(FIX_LTX) $< |\
	awk '$$1 ~ /\\begin{alltt}/ {if(pre != ""){print pre;} pre = $$0;}\
	     $$1 !~ /\\begin{alltt}/ {print pre; pre = $$0;}\
	     END {print $$0;}' - |\
	sed '/./,/^$$/!d' - > $*.fix.ltx
	$(OSDOC_RUNLATEX) $*.fix.ltx
	dvips -t letter -o $@ $*.fix.dvi
