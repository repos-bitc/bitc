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
BITC_SRC=../..

CLEANLIST=dir.dev.gxml
DIRS=bitc osverify-2004
OSDOC_XML_SRC=index
# Only build the HTML targets in this directory...
OSDOC_TARGETS=%.html

include $(BITC_SRC)/build/make/makerules.mk

all: index.html
install: all

dirfrag:
	$(XSLTPROC) \
		--stringparam url $(patsubst %.xml,%.html,$(XMLURL)) \
		--stringparam pdfurl $(patsubst %.xml,%.pdf,$(XMLURL)) \
		$(OSDOC_DIRENT) $(XMLURL) >> tmp.xml

xmlroots:
	for i in $(DIRS); do \
	  make -C $$d xmlsources; \
	done

dir.dev.gxml:
	echo "<fakeroot>" > tmp.xml
	for i in $(foreach dir,$(DIRS),$(wildcard $(dir)/*.xml)); do \
		$(MAKE) XMLURL="$$i" dirfrag; \
	done
	echo "</fakeroot>" >> tmp.xml
	mv tmp.xml dir.dev.gxml

#index.html: STYLESHEET="$(WEBROOT)/dirstyles.css"
index.html: index.xml dir.dev.gxml $(XSL_HTML)
