## File:      flora2/closure/Makefile
##
## Author(s): Michael Kifer
##            Guizhen Yang
##
## Contact:   flora-users@lists.sourceforge.net
## 
## Copyright (C) The Research Foundation of SUNY, 1999-2001
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


FLORADIR = ..

ALLOBJS = flrcommon.flh \
          flrtrailer.flh \
	  flreqltrailer.flh \
          flrscalareql.flh

PROLOG = none

PROLOG_COMMAND = $(PROLOG) -s -m 2000 -c 2000 -e 'segfault_handler(warn).'

FLORA_PRED_DEF = includes/flrpred.fli

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_closure_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling .../flora2/closure:"

# suffix rules
.SUFFIXES: .flh .fli

all: clean init $(ALLOBJS) finish
	@../touch.sh cmd...
	$(PROLOG_COMMAND) < cmd...
	@rm cmd...
	@cat $(SVFILE)


%.flh: %.fli $(FLORA_PRED_DEF)
	@echo 'flWrapAround($*).' >> cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "['../flora2devel']." >> cmd...
	@echo "import bootstrap_flora/0 from flora2." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "import flWrapAround/1 from flrwraparound." >> cmd...
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...


finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


clean:
	/bin/rm -f *~ .*~ *.flh *.bak .#*
	/bin/rm -f includes/*~


.DONE:
	@echo Done
