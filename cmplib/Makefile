## File:      cmplib/Makefile
## Author(s): Kostis Sagonas, Jiyang Xu, Terrance Swift
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


ALL =	asm.O asm_inst.O asm_opt.O asmpass2.O auxlry.O builtin.O \
	compile.O config.O cp_opt.O cutcheck.O flatten.O inprog.O \
	listutil.O makemode.O modeinf.O modes.O parse.O peephole.O \
	pre_cond.O preproc.O printres.O prof_sbp.O sanity.O \
	singlton.O spec.O suppltab.O symtab.O tabdef.O ti_mod.O \
	tp_comp.O tp_cond.O tp_eval.O tp_goal.O tp_index.O tp_var.O \
	tpinline.O tprog.O useinfer.O varproc.O writeasm.O

EMU_INCLUDES = ../emu/builtin.h ../emu/celltags.h ../emu/flag_defs.h \
	../emu/io_defs.h ../emu/msyscall.h ../emu/sig.h \
	../emu/system.h ../emu/xsbsocket.h 
PROLOG_INCLUDES = ../prolog_includes/char_defs.h ../prolog_includes/standard.h

Option = [sysmod,optimize,spec_repr]

#-------------------------------------------------------------------#
#
XSB=../bin/xsb
Prolog = $(XSB)
#
#-------------------------------------------------------------------#


# suffix rules
.SUFFIXES: .P .O .H

all: $(ALL)
	@touch cmd...
	$(Prolog) < cmd...
	/bin/rm cmd...

%.O:   %.P  $(PROLOG_INCLUDES) $(EMU_INCLUDES)
	echo 'mc($*, $(Option)).' >> cmd...
%.O:   %.H
	echo 'mc($*, $(Option)).' >> cmd...

# These targets are obsolete
.H.O:
	echo 'mc($*, $(Option)).' >> cmd...
.P.O:
	echo 'mc($*, $(Option)).' >> cmd...


.DONE:
	echo Done
