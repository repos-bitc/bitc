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

ifneq "$(wildcard doxygen.html.cfg)" ""
doxygen-html:
	doxygen doxygen.html.cfg

doxygen: doxygen-html
endif

ifneq "$(wildcard doxygen.ltx.cfg)" ""
doxygen-ltx:
	doxygen doxygen.ltx.cfg

doxygen: doxygen-ltx
endif

clean: doxygen-clean

# Use of XMLSOURCE here isn't as scary as it looks, since XMLSOURCE already
# has the ".xml" removed.

doxygen-clean:
	-rm -rf doxygen

DOXYGENRULES_LOADED=1
