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
PROLOGEXT=.P

FLORADIR = ..

ALL = 	aggregate${PROLOGEXT} benchmark${PROLOGEXT} \
	clpdemo${PROLOGEXT} default${PROLOGEXT} family_obj${PROLOGEXT} \
	family_rel${PROLOGEXT} flogic_basics${PROLOGEXT}\
	metavar${PROLOGEXT} mix${PROLOGEXT} module1${PROLOGEXT}\
	mod1${PROLOGEXT} \
	mono_inherit${PROLOGEXT} rel_ops${PROLOGEXT}\
	tree_traversal${PROLOGEXT} diamond${PROLOGEXT}\
	btupdates${PROLOGEXT} pretty${PROLOGEXT}

Option = [optimize]


#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
PROLOG = none
PROLOG_COMMAND = $(PROLOG) -i -s -m 40000 -c 4000 -e 'segfault_handler(warn).'
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_demos_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling .../flora2/demos:"


# suffix rules
.SUFFIXES: ${PROLOGEXT} .flr

all: init recompile

recompile: $(ALL) finish
	./CompileChangedFiles.sh $(PROLOG_COMMAND)
	@cat $(SVFILE)

.flr${PROLOGEXT}:
	echo 'import (flCompile)/1 from flora2. flCompile($*).' >> cmd...

init:
	rm -f $(SVFILE)
	@echo Initializing...
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...
	@echo "asserta(library_directory('..'))." >> cmd...hdr
	@echo "['../flora2']." >> cmd...hdr
	@echo "import bootstrap_flora/0 from flora2." >> cmd...hdr
	@echo "bootstrap_flora." >> cmd...hdr
	@echo "file_reopen($(SVFILE),a,$(STDWARN),_)." >> cmd...hdr
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...hdr

finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


.DONE:
	echo Done

clean:
	/bin/rm -f *${PROLOGEXT} *${PROLOGEXT}_gpp *${OBJEXT} *.O *.bak *.fdb *.fld *~ .*~ .#*
