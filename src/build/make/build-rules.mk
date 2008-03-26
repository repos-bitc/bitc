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

ifndef MAKEVARS_LOADED
include $(BITC_SRC)/build/make/makevars.mk
endif

# The following variables depend on things set in the makefile:
GCCFLAGS=$(CFLAGS) $(OPTIM) $(GCC_OPTIM) $(INC) $(DEF)
GPLUSFLAGS=-fdefault-inline $(CXXFLAGS) $(OPTIM) $(GPLUS_OPTIM) $(INC) $(DEF)
ASMFLAGS=$(CFLAGS) $(INC) $(DEF)

#DEP_SEDHACK=sed 's,^[^:]*:[:]*,'$@' '$(BUILDDIR)/'&,g'
#DEP_SEDHACK=sed 's,^[^:]*:[:]*,'$(BUILDDIR)/'&,g'
DEP_SEDHACK=cat -

C_DEP=$(GCC) $(GCCFLAGS) $(GCCWARN) $(BITC_WARN_ERROR) -M -MT $@ -MF $(BUILDDIR)/.$(patsubst %.o,%.m,$(notdir $@)) $<
CXX_DEP=@$(GPLUS) $(GPLUSFLAGS) $(GPLUSWARN) $(BITC_WARN_ERROR) -M -MT $@ -MF $(BUILDDIR)/.$(patsubst %.o,%.m,$(notdir $@)) $<
ASM_DEP=@$(GCC) $(ASMFLAGS) -M -MT $@ -MF $(BUILDDIR)/.$(patsubst %.o,%.m,$(notdir $@)) $<
#MOPS_DEP=$(GCC) $(GCCFLAGS) $(MOPSWARN) $(BITC_WARN_ERROR) -S $< -o $(BUILDDIR)/.$(patsubst %.o,%.m,$(notdir $@))

C_BUILD=$(GCC) $(GCCFLAGS) $(GCCWARN) $(BITC_WARN_ERROR) -c $< -o $@
CXX_BUILD=$(GPLUS) $(GPLUSFLAGS) $(GPLUSWARN) $(BITC_WARN_ERROR) -c $< -o $@
ASM_BUILD=$(GCC) $(ASMFLAGS) -c $< -o $@
MOPS_BUILD=$(GCC) -B$(MOPS)/rc/ $(GCCFLAGS) $(MOPSWARN) $(BITC_WARN_ERROR) -S $< -o $@

$(BUILDDIR):
	if [ ! -d $(BUILDDIR) ]; then \
		mkdir -p $(BUILDDIR); \
	fi
######################################################################
#
# Only want to rebuild the build directory if it doesn't already exist.
# Cannot list $(BUILDDIR) as a dependency directly, because the directory
# timestamp changes every time we compile, which forces rebuilds
# unnecessarily.
#
######################################################################

ifeq "$(wildcard $(BUILDDIR))" ""
DEPEND: $(BUILDDIR)
MAKE_BUILDDIR=$(BUILDDIR)
endif

#
# Object construction rules
#

$(BUILDDIR)/%.o: %.S $(MAKE_BUILDDIR)
	$(ASM_DEP)
	$(ASM_BUILD) 

$(BUILDDIR)/%.o: %.c $(MAKE_BUILDDIR)
	$(C_DEP)
	$(C_BUILD) 

$(BUILDDIR)/%.cfg: %.c $(MAKE_BUILDDIR)
	$(MOPS_DEP)
	$(MOPS_BUILD) 

$(BUILDDIR)/%.o: %.cxx $(MAKE_BUILDDIR)
	$(CXX_DEP)
	$(CXX_BUILD) 


$(BUILDDIR)/%-constituents.h: $(MAKE_BUILDDIR)
$(BUILDDIR)/%-constituents.h: %.imgmap
	@grep 'CONSTIT(' $< | \
		grep -v '#define' | \
		sed 's/[ ]*=.*$$//' | \
		sed 's/^[^,]*, */#define /' | \
		sed 's/)[^)]*$$//' | \
		sed 's/,/ /' > $@
	echo "Making $@"

BUILDRULES_LOADED=1
