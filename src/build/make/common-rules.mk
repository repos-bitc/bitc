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


unexport DIRS
unexport ETAGDIRS
unexport GENERATED
unexport CLEANLIST

ifndef MAKEVARS_LOADED
include $(BITC_SRC)/build/make/makevars.mk
endif

ifndef CLEANDIRS
CLEANDIRS=$(DIRS)
endif

ifeq "$(BUILDDIR)" ""
$(error BUILDDIR is not set!)
endif
ifneq "$(BUILDDIR)" "."
CLEAN_BUILDDIR=$(BUILDDIR)
endif

.PHONY: tags
.PHONY: world package

######################################################################
#
# TOP LEVEL BUILD RULES FOR REMAKING CURRENT PACKAGE OR ENTIRE WORLD
#
######################################################################
world:
	$(MAKE) -C $(BITC_SRC) $(MAKERULES) packages

ifneq "$(PACKAGE)" ""
package:
	$(MAKE) -C $(BITC_ROOT)/src/$(PACKAGE) $(MAKERULES) install

endif

pristine:
	$(MAKE) -C $(BITC_ROOT)/src -k clean
	$(MAKE) -C $(BITC_ROOT)/src targdir-clean
	$(MAKE) -C $(BITC_ROOT)/src world

######################################################################
#
# GLOBAL AND RECURSIVE TARGETS FOR USE WITHIN A PACKAGE.
#
######################################################################

export RECURSE_TARGET

.PHONY: recurse
recurse:
	@for i in `echo $(DIRS)`; do \
		if [ -d "$$i" ]; then\
			$(MAKE) -C $$i $(MAKERULES) $(RECURSE_TARGET) recurse; \
			if [ $$? -ne 0 ]; then\
				echo "*** RECURSIVE BUILD STOPS ***";\
				exit 1;\
			fi; \
		fi; \
	done

.PHONY: subdirs
subdirs: RECURSE_TARGET=subdirs
subdirs: recurse

.PHONY: install
install: RECURSE_TARGET=install
install: recurse

.PHONY: interfaces
interfaces: RECURSE_TARGET=interfaces
interfaces: recurse

.PHONY: doxygen
doxygen: $(GENERATED)
doxygen: RECURSE_TARGET=doxygen
doxygen: recurse

### install: recursive-install
### recursive-install:
### ifneq "$(DIRS)" ""
### 	@for i in $(DIRS); do \
### 		if [ -d "$$i" ]; then\
### 			$(MAKE) -C $$i $(MAKERULES) install; \
### 			if [ $$? -ne 0 ]; then\
### 				echo "*** RECURSIVE BUILD STOPS ***";\
### 				exit 1;\
### 			fi; \
### 		fi; \
### 	done
### endif

.PHONY: clean

clean: nodepend

# Clean does not use the generic recurse, because it needs to recurse over
# different directories
clean: generic-clean
clean:
	@for i in `echo $(CLEANDIRS)`; do \
		if [ -d "$$i" ]; then\
			$(MAKE) -C $$i $(MAKERULES) clean; \
			if [ $$? -ne 0 ]; then\
				echo "*** RECURSIVE CEAN STOPS ***";\
				exit 1;\
			fi; \
		fi; \
	done

.PHONY: generic-clean
generic-clean:
	-rm -f *.o core *~ new.Makefile  ".#"*
	-rm -f .*.m sysgen.map $(TARGETS) TAGS
	-rm -f *.dvi *.blg *.aux *.log *.toc $(CLEANLIST)
ifneq "$(CLEAN_BUILDDIR)" ""
	-rm -rf $(CLEAN_BUILDDIR)
endif

package-clean:
	$(MAKE) -C $(BITC_ROOT)/src/$(PACKAGE) $(MAKERULES) clean

## CLEANING: The following piece of idiocy works around the fact that
## the autodependency files may refer to stuff that has been cleaned,
## and that this can therefore perversely cause the clean target to
## fail.

.PHONY: nodepend 
nodepend:
	-find . -name '.*.m' -exec rm {} \;
	-find . -name 'DEPEND' -exec rm {} \;

# This is a debugging target..
.PHONY: walk
walk: RECURSE_TARGET=walk
walk: recurse

here:
	@echo $(PWD)

COMMONRULES_LOADED=1
