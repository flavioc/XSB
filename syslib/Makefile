## File:      syslib/Makefile
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

ALL =	assert.O atts.O basics.O consult.O curr_sym.O dbclause.O \
	dcg.O debugger.O domain.O eval.O file_io.O \
	file_op.O hilogsym.O loader.O machine.O \
	num_vars.O setof.O shell.O standard.O std_xsb.O \
	string.O subsumes.O sysinitrc.O tables.O term_exp.O \
	unix.O x_interp.O xsb_hook.O xsb_read.O xsb_writ.O xcallxsb.O

EMU_INCLUDES = ../emu/builtin.h ../emu/celltags.h ../emu/flag_defs.h \
	../emu/io_defs_xsb.h ../emu/syscall_xsb.h ../emu/sig_xsb.h \
	../emu/system_defs_xsb.h ../emu/socket_defs_xsb.h 
PROLOG_INCLUDES = ../prolog_includes/char_defs.h ../prolog_includes/standard.h


Option = [sysmod,optimize,quit_on_error]

#-------------------------------------------------------------------#
#
XSB=../bin/xsb
Prolog = $(XSB) -e "segfault_handler(warn)."
#
#-------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '../build/.xsb_syslib_warn.tmp'
OWRITE = 1
STDWARN = 3
HDRMSG = "While compiling XSB/syslib:"

OBJFILESAVED = objfiles.saved


# suffix rules
.SUFFIXES: .P .H .O

all: init $(ALL) finish
	@../build/touch.sh cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)

%.O:   %.P  $(PROLOG_INCLUDES) $(EMU_INCLUDES)
	@cp -f $*.O $(OBJFILESAVED) || echo ""
	echo 'mc($*, $(Option)).' >> cmd...
# The following is done because of brain damage in some make utilities
# (notably Sun's), which don't take %.P %.H in the same dependency list
%.P:   %.H
	../build/touch.sh $@

# These targets are obsolete. They are here just in case some 
# obscure make utility doesn't understand patterns
.H.P:
	../build/touch.sh $@
.P.O:
	@cp -f $*.O $(OBJFILESAVED) || echo ""
	echo 'mc($*, $(Option)).' >> cmd...

init:
	rm -f $(SVFILE)
	@test -d $(OBJFILESAVED) || mkdir $(OBJFILESAVED)
	@echo 'import file_reopen/4, fmt_write/3, file_flush/2 from file_io.' >> cmd...
	@echo "file_reopen($(SVFILE),$(OWRITE),$(STDWARN),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo 'file_flush($(STDWARN),_).' >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"%s\n",arg("--------------------------------------------")).' >> cmd...

restore:
	mv -f $(OBJFILESAVED)/*.O .


.DONE:
	echo Done
