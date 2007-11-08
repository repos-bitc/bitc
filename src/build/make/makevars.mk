#
# Copyright (C) 2007, The EROS Group, LLC
# All rights reserved.
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

# Cancel the old suffix list, because the order matters.  We want assembly 
# source to be recognized in preference to ordinary source code, so the
# .S, .s cases must come ahead of the rest.
.SUFFIXES:
.SUFFIXES: .S .s .cxx .c .y .l .o .dvi .ltx .gif .fig .xml .html .pdf .xhtml

# Some shells do not export this variable. It is used in the package
# generation rule and in the discovery of BITC_ROOT.
PWD=$(shell pwd)

#
# Set up default values for these variables so that a build in an improperly
# configured environment has a fighting chance:
#
ifdef BITC_ROOT

BITC_SRCDIR=$(word 1, $(strip $(subst /, ,$(subst $(BITC_ROOT)/,,$(PWD)))))
BITC_FINDDIR=old

else

ifneq "" "$(findstring /bitc/pkgsrc,$(PWD))"
BITC_SRCDIR=pkgsrc
else
BITC_SRCDIR=src
endif

BITC_ROOT=$(firstword $(subst /bitc/$(BITC_SRCDIR), ,$(PWD)))/bitc

endif

ifndef BITC_TARGET
BITC_TARGET=i386
endif

export BITC_ROOT
export BITC_TARGET

INSTALL=$(BITC_SRC)/build/bin/bitc-install
REPLACE=$(BITC_SRC)/build/bin/move-if-change
MKIMAGEDEP=$(BITC_SRC)/build/bin/mkimagedep

#
# First, set up variables for building native tools:
#
GAWK=gawk
PYTHON=python
XML_LINT=xmllint
XSLTPROC=xsltproc --xinclude

NATIVE_STRIP=strip
NATIVE_OBJDUMP=objdump
NATIVE_SIZE=size
NATIVE_AR=ar
NATIVE_LD=ld
NATIVE_REAL_LD=ld
NATIVE_RANLIB=ranlib

NATIVE_GCC=gcc --std=gnu99
ifndef GCCWARN
MOPSWARN=-Wall -Winline -Wno-format -Wno-char-subscripts
GCCWARN=$(MOPSWARN)
endif

NATIVE_GPLUS=g++
ifndef GPLUSWARN
GPLUSWARN=-Wall -Winline -Wno-format
endif

#
# This is where the target environment makefile gets a chance to override
# things:
#
ifndef BITC_HOSTENV
BITC_HOSTENV=env-standard
endif

include $(BITC_SRC)/build/make/$(BITC_HOSTENV).mk

# search for ppmtogif in all the obvious places:
ifndef NETPBMDIR
ifneq "" "$(findstring /usr/bin/ppmtogif,$(wildcard /usr/bin/*))"
NETPBMDIR=/usr/bin
endif
endif

ifndef NETPBMDIR
ifneq "" "$(findstring /usr/bin/X11/ppmtogif,$(wildcard /usr/bin/X11/*))"
NETPBMDIR=/usr/bin/X11
endif
endif

ifndef NETPBMDIR
ifneq "" "$(findstring /usr/local/netpbm/ppmtogif,$(wildcard /usr/local/netpbm/*))"
NETPBMDIR=/usr/local/netpbm
endif
endif

ifndef BITC_FD
BITC_FD=$(HOST_FD)
endif

# Record the location of our DEPGEN script:
BITC_RUNLATEX=$(BITC_SRC)/build/make/runlatex.sh
BITC_PS2PDF=$(BITC_SRC)/build/make/ps2pdf.sh
BITC_XCACHE=$(BITC_SRC)/build/make/xcache.sh

# search for ccache in the obvious places:
ifndef BITC_CCACHE
ifneq "" "$(findstring /usr/bin/ccache,$(wildcard /usr/bin/ccache*))"
BITC_CCACHE=/usr/bin/ccache
endif
endif

ifndef BITC_CCACHE
ifneq "" "$(findstring /usr/local/bin/ccache,$(wildcard /usr/local/bin/ccache*))"
BITC_CCACHE=/usr/local/bin/ccache
endif
endif

GCC=$(BITC_CCACHE) $(NATIVE_GCC)
GPLUS=$(BITC_CCACHE) $(NATIVE_GPLUS)
LD=$(NATIVE_LD)
AR=$(NATIVE_AR)
OBJDUMP=$(NATIVE_OBJDUMP)
SIZE=$(NATIVE_SIZE)
STRIP=$(NATIVE_STRIP)
RANLIB=$(NATIVE_RANLIB)
GPLUS_OPTIM=$(NATIVE_GPLUS_OPTIM)
GCC_OPTIM=$(NATIVE_GCC_OPTIM)

ASTMAKER=astmaker

LIBSHERPA=-lsherpa

## This is a holdover from the EROS tree that we may not want:
## DOMLIB= $(BITC_ROOT)/lib/libdomain.a
## DOMLIB += $(BITC_ROOT)/lib/libidlstub.a
## DOMLIB += $(BITC_ROOT)/lib/libdomgcc.a
## DOMLIB += $(BITC_ROOT)/lib/libc.a

#FIX: Need to define DOMCRT0 and DOMCRTN and DOMLINKOPT
# ifeq "$(BITC_HOSTENV)" "linux-xenv-gcc3"
# #DOMCRT0=$(BITC_ROOT)/lib/gcc-lib/$(BITC_TARGET)-unknown-eros/3.3/crt1.o
# #DOMCRTN=$(BITC_ROOT)/lib/gcc-lib/$(BITC_TARGET)-unknown-eros/3.3/crtn.o
# DOMLINKOPT=-N -static -Ttext 0x0 -L$(BITC_ROOT)/usr/lib
# DOMLINK=$(GCC)
# else
# DOMCRT0=$(BITC_ROOT)/lib/crt0.o
# DOMCRTN=$(BITC_ROOT)/lib/crtn.o
# DOMLINKOPT=-N -Ttext 0x0 -nostdlib -static -e _start -L$(BITC_ROOT)/usr/lib -L$(BITC_ROOT)/usr/lib/$(BITC_TARGET)
# DOMLINK=$(BITC_LD)
# endif

DOMLIB += $(DOMCRTN)


# Really ugly GNU Makeism to extract the name of the current package by
# stripping $BITC_ROOT/ out of $PWD (yields: src/PKG/xxxxxx), turning /
# characters into spaces (yields: "src PKG xxx xxx xxx"), and  
# then selecting the appropriate word. Notice that the first substring is 
# empty, so the appropriate word is the second one.
PACKAGE=$(word 1, $(strip $(subst /, ,$(subst $(BITC_ROOT)/src/,,$(PWD)))))

PKG_ROOT=$(BITC_ROOT)/pkg/${PACKAGE}
PKG_SRC=$(BITC_ROOT)/$(BITC_SRCDIR)/${PACKAGE}

# Until proven otherwise...
IMGMAP=imgmap

# Until proven otherwise...
BOOTSTRAP=boot

# Until proven otherwise...

ifeq "$(BUILDDIR)" ""
ifeq "$(PACKAGE)" "doc"
BUILDDIR=.
RBUILDDIR=.
endif
ifeq "$(PACKAGE)" "legal"
BUILDDIR=.
RBUILDDIR=.
endif
ifeq "$(PACKAGE)" "build"
BUILDDIR=.
RBUILDDIR=.
endif
ifeq "$(BUILDDIR)" ""
BUILDDIR=BUILD
RBUILDDIR=..
endif
endif

showme:
	@echo "BITC_ROOT: " $(BITC_ROOT)
	@echo "BITC_SRCDIR: " $(BITC_SRCDIR)
	@echo "PKG_ROOT:" $(PKG_ROOT)
	@echo "PKG_SRC:" $(PKG_SRC)
	@echo "BUILDDIR:" $(BUILDDIR)

MAKEVARS_LOADED=1
