## File:      packages/flora/Makefile
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

XSBDIR=../..

ALL = 	flrutils.O flrtokens.O flrparser.O flrcompiler.O flrshell.O

EMU_INCLUDES = $(XSBDIR)/emu/builtin.h $(XSBDIR)/emu/celltags.h \
	$(XSBDIR)/emu/flag_defs.h \
	$(XSBDIR)/emu/io_defs.h $(XSBDIR)/emu/msyscall.h $(XSBDIR)/emu/sig.h \
	$(XSBDIR)/emu/system.h $(XSBDIR)/emu/xsbsocket.h 
PROLOG_INCLUDES = $(XSBDIR)/prolog_includes/char_defs.h \
	$(XSBDIR)/prolog_includes/standard.h


Option = [optimize]


#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
XSB=$(XSBDIR)/bin/xsb
Prolog = $(XSB) -s -m 2000 -c 2000 -e 'set_dcg_style(standard),segfault_handler(warn).'
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.flora_top_warn.tmp'
OWRITE = 1
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/flora:"


# suffix rules
.SUFFIXES: .P .O .H

all: init $(ALL) finish
	@$(XSBDIR)/build/touch.sh cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)
	cd syslib; make
	cd lib; make
	cd pkgs; make


%.O:   %.P  $(PROLOG_INCLUDES) $(EMU_INCLUDES)
	echo 'mc($*, $(Option)).' >> cmd...
# The following is done because of brain damage in some make utilities
# (notably Sun's), which don't take %.P %.H in the same dependency list
%.P:   %.H
	$(XSBDIR)/build/touch.sh $@

# These targets are obsolete. They are here just in case some 
# obscure make utility doesn't understand patterns
.H.P:
	$(XSBDIR)/build/touch.sh $@
.P.O:
	echo 'mc($*, $(Option)).' >> cmd...

init:
	/bin/rm -f $(SVFILE)
	@echo "file_reopen($(SVFILE),$(OWRITE),$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"---------------------------------------------------\n",_).' >> cmd...


.DONE:
	echo Done
