## File:      lib/Makefile
## Author(s): Kostis Sagonas, Jiyang Xu
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
## Copyright (C) ECRC, Germany, 1990
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


ALL = 	aggregs.O array.O array1.O directry.O foreign.O \
	intern.O interprolog.O obsolete.O odbc_call.O ordsets.O ora_call.O \
	packaging.O scrptutl.O setof.O socket.O wrapping.O

EMU_INCLUDES = ../emu/builtin.h ../emu/celltags.h ../emu/flag_defs.h \
	../emu/io_defs.h ../emu/msyscall.h ../emu/sig.h \
	../emu/system.h ../emu/xsbsocket.h 
PROLOG_INCLUDES = ../prolog_includes/char_defs.h ../prolog_includes/standard.h

Option = [sysmod,optimize,quit_on_error]

#-----------------------------------------------------------------------#
#
XSB=../bin/xsb
Prolog = $(XSB) -e "segfault_handler(warn)."
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '/tmp/xsb_lib_warn'
OWRITE = 1
STDWARN = 3
HDRMSG = "While compiling XSB/lib:"


# suffix rules
.SUFFIXES: .P .O .H

all: init $(ALL) finish
	@touch cmd...
	$(Prolog) < cmd...
	/bin/rm -f cmd...
	@cat $(SVFILE)

%.O:   %.P $(PROLOG_INCLUDES) $(EMU_INCLUDES)
	echo 'mc($*, $(Option)).' >> cmd...
# The following is done because of brain damage in some make utilities
# (notably Sun's), which don't take %.P %.H in the same dependency list
%.P:   %.H
	touch $@


# These targets are obsolete. They are here just in case some 
# obscure make utility doesn't understand patterns
.H.P:
	touch $@
.P.O:
	echo 'mc($*, $(Option)).' >> cmd...

init:
	/bin/rm -f $(SVFILE)
	@echo 'import file_reopen/4, fmt_write/3 from file_io.' >> cmd...
	@echo "file_reopen($(SVFILE),$(OWRITE),$(STDWARN),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg("-------------- Warnings --------------------")).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"%s\n",arg("--------------------------------------------")).' >> cmd...


.DONE:
	echo Done
