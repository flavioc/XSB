## File:      packages/flora2/lib/Makefile
## Author(s): kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 2001
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

ALL = 	flrprettyprint.P flrio.P flrstorage.P flrsystem.P

EMU_INCLUDES = $(XSBDIR)/emu/builtin.h \
	$(XSBDIR)/emu/flag_defs_xsb.h \
	$(XSBDIR)/emu/io_defs_xsb.h \
	$(XSBDIR)/emu/system_defs_xsb.h $(XSBDIR)/emu/socket_defs_xsb.h 
PROLOG_INCLUDES = $(XSBDIR)/prolog_includes/char_defs.h \
	$(XSBDIR)/prolog_includes/standard.h


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
SVFILE = '$(XSBDIR)/build/.flora_lib_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/flora2/lib:"


# suffix rules
.SUFFIXES: .P .flr

all: clean init recompile

recompile: $(ALL) finish
	@touch cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)

.flr.P:
	echo 'flora_compile_system_module($*).' >> cmd...

init:
	rm -f $(SVFILE)
	@echo "import bootstrap_flora/0 from flora2." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "import flora_compile_system_module/1 from flrutils." >> cmd...
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...


.DONE:
	echo Done

clean:
	/bin/rm -f *.P *.O *.fdb *~
