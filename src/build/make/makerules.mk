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

ifndef MAKEVARS_LOADED
include $(BITC_SRC)/build/make/makevars.mk
endif

ifndef BUILDRULES_LOADED
include $(BITC_SRC)/build/make/build-rules.mk
endif

ifdef OSDOC_XML_SRC
ifndef OSDOC_RULES_LOADED
include $(BITC_SRC)/build/make/osdoc-rules.mk
endif
endif

ifdef OSDOC_XHTML_SRC
ifndef OSDOC_RULES_LOADED
include $(BITC_SRC)/build/make/osdoc-rules.mk
endif
endif

ifndef COMMONRULES_LOADED
include $(BITC_SRC)/build/make/common-rules.mk
endif

ifneq "$(wildcard doxygen.*)" ""
include $(BITC_SRC)/build/make/doxygen-rules.mk
endif

MAKERULES_LOADED=1
