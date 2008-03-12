#
# Copyright (C) 2005, Jonathan S. Shapiro.
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
