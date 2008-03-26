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
BITC_SRC=..
CLEANLIST=index.html
OSDOC_XHTML_SRC=$(patsubst %.xhtml,%,$(wildcard *.xhtml))
OSDOC_OHTML_SRC=$(patsubst %.ohtml,%,$(wildcard *.ohtml))
HTML_TOPNAV=home.html

DIRS=topnav docs download projects
DOCSTYLES= #docstyles.css

include $(BITC_SRC)/build/make/makerules.mk

all: index.html

install: all

#docstyles.css: ../doctools/XSLT/styles.css
#	cp ../doctools/XSLT/styles.css docstyles.css

index.html: bitc.html
	ln -s bitc.html index.html
