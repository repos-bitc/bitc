#
# Copyright (C) 2004, Jonathan S. Shapiro.
#
# This file is part of the Coyotos Operating System.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2,
# or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#

default: package
BITC_SRC=../../..

.SECONDARY:

#OSDOC_XML_SRC=$(patsubst %.xml,%,$(wildcard *.xml)) \
#              $(patsubst %.txml,%,$(wildcard *.txml)) 

MY_XML=changes closures PLOS2006-shap PLOS2006 polyinst 
MY_XML+=relnotes-0.10 relnotes-0.9 spec
MY_TYPE_XML=mutinfer formal sys-infer mut-formal #core

OSDOC_XML_SRC=$(MY_XML) $(MY_TYPE_XML)

CLEANLIST=bitc-grammar.c mutinfer.xml old_sys-infer.xml sys-infer.xml 
CLEANLIST+=formal.xml mut-formal.xml
XSL_BTYPE=btypes-osdoc.xsl
FIX_LTX='./fixup.pl'

#Do not add these .xmli files into clean list
SYSINFER_INCLUDE=include/sys-infer/$(wildcard *.xmli)
CORE_INCLUDE=include/core/$(wildcard *.xmli)
FORMAL_INCLUDE=include/formal/$(wildcard *.xmli)
MUT_FORMAL_INCLUDE=include/mut-formal/$(wildcard *.xmli)

include $(BITC_SRC)/build/make/makerules.mk
OSDOC_TARGETS+=%.ps

all:

install: all

#%.xmli: %.btype $(XSL_BTYPE)
#	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

mutinfer.xml: mutinfer.txml $(XSL_BTYPE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

formal.xml: formal.txml $(XSL_BTYPE) $(FORMAL_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

mut-formal.xml: mut-formal.txml $(XSL_BTYPE) $(MUT_FORMAL_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

sys-infer.xml: sys-infer.txml $(XSL_BTYPE) $(SYSINFER_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

core.xml: core.txml $(XSL_BTYPE) $(CORE_INCLUDE)
	$(XSLTPROC) -o $@ $(XSL_BTYPE) $<

%.ps: %.ltx
	$(FIX_LTX) $< |\
	awk '$$1 ~ /\\begin{alltt}/ {if(pre != ""){print pre;} pre = $$0;}\
	     $$1 !~ /\\begin{alltt}/ {print pre; pre = $$0;}\
	     END {print $$0;}' - |\
	sed '/./,/^$$/!d' - > $*.fix.ltx
	$(OSDOC_RUNLATEX) $*.fix.ltx
	dvips -t letter -o $@ $*.fix.dvi
