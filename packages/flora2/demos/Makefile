## File:      flora2/demos/Makefile
## Author(s): Michael Kifer
#
## Contact:   flora-users@lists.sourceforge.net
## 
## Copyright (C) The Research Foundation of SUNY, 2001
## 
## FLORA-2 is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## FLORA-2 is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with FLORA-2; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id$
## 
##

OBJEXT=.xwam

FLORADIR = ..

ALL = 	aggregate.P benchmark.P default.P family_obj.P \
	family_rel.P flogic_basics.P metavar.P mix.P module1.P mod1.P \
	mono_inherit.P rel_ops.P tree_traversal.P \
	btupdates.P pretty.P

Option = [optimize]


#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
PROLOG = none
PROLOG_COMMAND = $(PROLOG) -i -s -m 2000 -c 2000 -e 'segfault_handler(warn).'
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_demos_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling .../flora2/demos:"


# suffix rules
.SUFFIXES: .P .flr

all: init recompile

recompile: $(ALL) finish
	./CompileChangedFiles.sh $(PROLOG_COMMAND)
	@cat $(SVFILE)

.flr.P:
	echo 'import (flCompile)/1 from flora2. flCompile($*).' >> cmd...

init:
	rm -f $(SVFILE)
	echo Initializing...
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...
	@echo "['../flora2devel']." >> cmd...hdr
	@echo "import bootstrap_flora/0 from flora2." >> cmd...hdr
	@echo "bootstrap_flora." >> cmd...hdr
	@echo "file_reopen($(SVFILE),a,$(STDWARN),_)." >> cmd...hdr
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...hdr

finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


.DONE:
	echo Done

clean:
	/bin/rm -f *.P *.P_gpp *${OBJEXT} *.O *.bak *.fdb *.fld *~ .*~ .#*
