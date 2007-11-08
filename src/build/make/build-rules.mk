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
