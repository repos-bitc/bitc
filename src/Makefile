#
# Copyright (C) 2001, The EROS Group, LLC.
#
# This file is part of the EROS Operating System.
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


default: world

BITC_SRC=.

# DIRS are ordered:
# build must be made before others. base and doc can be in either order.
# Nothing depends on legal.
REQUIRED_DIRS=bitcc-bootstrap
OPTIONAL_DIRS=doc
DIRS=$(REQUIRED_DIRS) $(OPTIONAL_DIRS)
CLEANDIRS=build $(DIRS)

UPDATE_DIRS=build $(REQUIRED_DIRS)
OPT_UPDATE_DIRS=$(OPTIONAL_DIRS)
ALL_UPDATE_DIRS=$(UPDATE_DIRS) $(OPT_UPDATE_DIRS)

TARDIRS=$(DIRS)

include $(BITC_SRC)/build/make/pkgrules.mk

distclean: targdir-clean clean

targdir-clean:
	-rm -rf $(BITC_ROOT)/include $(BITC_ROOT)/lib
	-rm -rf $(BITC_ROOT)/doc $(BITC_ROOT)/doc
	-rm -rf $(BITC_ROOT)/host $(BITC_ROOT)/usr
	-rm -rf $(BITC_ROOT)/idl

packages:
	$(MAKE) install

build-pkg:
	$(MAKE) -C build PACKAGE=build $(MAKERULES) package

# The following do not use the generic recurse target because we do not
# want them to recurse deeply -- only to one level. Also, we need to 
# re-set the PACKAGE variable for each of them.
#interfaces: build-pkg
#interfaces: RECURSE_TARGET=interfaces
#interfaces: recurse
#interfaces: idl-headers

# The install target recurses deeply in any case.
install: build-pkg

tar:	clean
	-rm -f eros.tar eros.tar.gz eros.tgz
	tar cvf eros.tar Makefile $(TARDIRS)
	gzip eros.tar
	mv eros.tar.gz eros.tgz

# Targets specific to OpenCM:
HOSTNAME=$(shell hostname)
ifeq "$(HOSTNAME)" "office.eros-os.com"
CMR=cm
else
CMR=cmr
endif

status:
	@for d in `echo $(ALL_UPDATE_DIRS)`; do\
	    if [ -d $$d ]; then \
	        echo "Status of $$d:"; \
		cm -C $$d status; \
	    fi; \
	done

update:
	@for d in `echo $(ALL_UPDATE_DIRS)`; do\
	    if [ -d $$d ]; then \
	        echo "Updating $$d"; \
		(cd $$d; $(CMR) update); \
	    fi; \
	done

destructive-sync:
	rsync -avz rsync://www.bitc-lang.org/src.bitc/src/Makefile ./Makefile
	for d in `echo $(UPDATE_DIRS) build`; do\
	   rsync -avz --delete rsync://www.bitc-lang.org/src.bitc/src/$$d/ $$d/; \
	done
	for d in `echo $(OPT_UPDATE_DIRS)`; do\
	    if [ -d $$d ]; then \
	        rsync -avz --delete rsync://www.bitc-lang.org/src.bitc/src/$$d/ $$d/; \
	    fi; \
	done

sync:
	rsync -avz rsync://www.bitc-lang.org/src.bitc/src/Makefile ./Makefile
	for d in `echo $(UPDATE_DIRS) build`; do\
	    rsync -avz rsync://www.bitc-lang.org/src.bitc/src/$$d/ $$d/; \
	done
	for d in `echo $(OPT_UPDATE_DIRS)`; do\
	    if [ -d $$d ]; then \
	        rsync -avz rsync://www.bitc-lang.org/src.bitc/src/$$d/ $$d/; \
	    fi; \
	done
