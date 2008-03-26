#
# Copyright (C) 2001, The EROS Group, LLC.
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
