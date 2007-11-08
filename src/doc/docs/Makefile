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
