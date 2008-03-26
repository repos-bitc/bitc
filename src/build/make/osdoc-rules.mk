#
# Copyright (C) 2005, Jonathan S. Shapiro.
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

# Include this file into your makefile to pick up the document building rules.
# You need to define the variable DOCTOOLS first.

OSDOCTOOLS=/usr/share/osdoc

# If we are in the doc/web tree, we want a relative path 
# to the web root, otherwise we want an absolute path:#
WEBROOT=$(BITC_SRC)/doc
ifeq "$(PACKAGE)" "doc"
ifeq "$(BITC_SRC)" ".."
WEBROOT=.
else
WEBROOT=$(subst /../doc/,,$(BITC_SRC)/doc/)
endif
endif

ifeq "$(HTML_TOPNAV)" ""
HTML_TOPNAV=docs.html
endif

LATEX_XSLTPROC_OPTS+=
ifeq "$(PACKAGE)" "doc"
OSDOC_HTML_OPTS+=\
	--param navbars 1 \
	--stringparam sidenav $(WEBROOT)/sidenav.html \
	--stringparam sidenavdir $(WEBROOT) \
	--stringparam topnav $(WEBROOT)/topnav/$(HTML_TOPNAV) \
	--stringparam topnavdir $(WEBROOT)/topnav
OSDOC_XHTML_OPTS+=\
	--param navbars 1 \
	--stringparam sidenav $(WEBROOT)/sidenav.html \
	--stringparam sidenavdir $(WEBROOT) \
	--stringparam topnav $(WEBROOT)/topnav/$(HTML_TOPNAV) \
	--stringparam topnavdir $(WEBROOT)/topnav
OSDOC_OHTML_OPTS+=\
	--param navbars 1 \
	--stringparam sidenav $(WEBROOT)/sidenav.html \
	--stringparam sidenavdir $(WEBROOT) \
	--stringparam topnav $(WEBROOT)/topnav/$(HTML_TOPNAV) \
	--stringparam topnavdir $(WEBROOT)/topnav
endif
OSDOC_HTML_CHUNK_OPTS+= --param enable.chunking 1

ifndef STYLESHEET
# Definite path to default style sheet would be
#    $(BITC_SRC)/doc/web/styles.css
# We assume here that if a style sheet is going to be used, it will
# be used from within the doc subtree, in which case we need the 
# relative path from the build directory to
#    $(BITC_SRC)/doc/web.
# We construct this using substitution:

STYLESHEET=$(WEBROOT)/styles.css
endif

webroot:
	@echo "bitcsrc: $(BITC_SRC)"
	@echo "webroot: $(WEBROOT)"
	@echo "pwd:     $(PWD)"
	@echo "web:     $(BITC_SRC)/doc"
	@echo "pkg:     $(PACKAGE)"
	@echo "STYLESHEET=$(WEBROOT)/styles.css"

include $(OSDOCTOOLS)/osdoc.mk

OSDOC_RULES_LOADED=1
