## File:      packages/flora/pkgs/Makefile
## Author(s): kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1999
## 
## XSB is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## XSB is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with XSB; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id$
## 
##

XSBDIR=../../..

ALL = 	

EMU_INCLUDES = $(XSBDIR)/emu/builtin.h \
	$(XSBDIR)/emu/flag_defs_xsb.h \
	$(XSBDIR)/emu/io_defs_xsb.h $(XSBDIR)/emu/syscall_xsb.h \
	$(XSBDIR)/emu/system_defs_xsb.h $(XSBDIR)/emu/socket_xsb.h 
PROLOG_INCLUDES = $(XSBDIR)/prolog_includes/char_defs.h \
	$(XSBDIR)/prolog_includes/standard.h \
	$(XSBDIR)/packages/flora/includes/flora_prefix.h


Option = [optimize]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
XSB=$(XSBDIR)/bin/xsb
Prolog = $(XSB) -i -s -m 2000 -c 2000 -e 'segfault_handler(warn).'
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.flora_pkg_warn.tmp'
OWRITE = 1
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/flora/pkg:"


# suffix rules
.SUFFIXES: .P .flr

# We should have touch *.flr here when there will be packages
all: init recompile

recompile: $(ALL) finish
	@touch cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)

.flr.P:
	echo 'import flcompile/1 from flrutils. flcompile($*).' >> cmd...

init:
	/bin/rm -f $(SVFILE)
	@echo "import bootstrap_flora/0 from flora." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "file_reopen($(SVFILE),$(OWRITE),$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...
	@echo "[flora],bootstrap_flora." >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"---------------------------------------------------\n",_).' >> cmd...


.DONE:
	echo Done
