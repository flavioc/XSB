## File:      flora2/lib/Makefile
## Author(s): kifer
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

FLORADIR=..

ALL = 	flrprettyprint${PROLOGEXT} flrio${PROLOGEXT} \
	flrstorage${PROLOGEXT} flrsystem${PROLOGEXT}

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
SVFILE = '$(FLORADIR)/.flora_lib_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling .../flora2/lib:"


# suffix rules
.SUFFIXES: ${PROLOGEXT} .flr

all: clean init recompile

recompile: $(ALL) finish
	@../touch.sh cmd...
	$(PROLOG_COMMAND) < cmd...
	@rm cmd...
	@cat $(SVFILE)

.flr${PROLOGEXT}:
	echo 'flora_compile_system_module($*).' >> cmd...

init:
	rm -f $(SVFILE)
	@echo "asserta(library_directory('..'))." >> cmd...
	@echo "['../flora2']." >> cmd...
	@echo "import bootstrap_flora/0 from flora2." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "import flora_compile_system_module/1 from flrutils." >> cmd...
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


.DONE:
	echo Done

clean:
	/bin/rm -f *${PROLOGEXT} *$(OBJEXT) *.O *.fdb *~ .*~ *.fld *.bak .#*
