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


CMP =	asm.O asm_inst.O asm_opt.O asmpass2.O auxlry.O builtin.O \
	compile.O config.O cp_opt.O cutcheck.O flatten.O inprog.O \
	listutil.O makemode.O modeinf.O modes.O parse.O peephole.O \
	pre_cond.O preproc.O printres.O prof_sbp.O sanity.O \
	singlton.O spec.O suppltab.O symtab.O tabdef.O ti_mod.O \
	tp_comp.O tp_cond.O tp_eval.O tp_goal.O tp_index.O tp_var.O \
	tpinline.O tprog.O useinfer.O varproc.O writeasm.O

Option = [sysmod,optimize,spec_repr]

#-------------------------------------------------------------------#
### Please select ONE Prolog below to run this makefile.
### At developement stage, an older version of XSB or PSB-Prolog is
### suggested.
###
### Note: if you select self-make, be careful to save all the
###       current *.O files, so that when the new files have
###       errors, the old *.O can be re-installed.
#
# Self-make: Version 1.6.0
#
XSB=../bin/xsb
Prolog = $(XSB) -i -s -m 1200 -c 2000
#
#-------------------------------------------------------------------#

# continue to the next when one error occurs

.IGNORE:

# suffix rules
.SUFFIXES: .P .O .H

.H.P:
	echo 'mc($*, $(Option)).' >> cmd...

.P.O:
	echo 'mc($*, $(Option)).' >> cmd...

# Unfortunately, this needs to be commented on the NeXT.
# %.O:	%.H

all: $(CMP)
	@touch cmd...
	$(Prolog) < cmd...
	/bin/rm cmd...

.DONE:
	echo Done
