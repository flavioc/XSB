## File:      flora2/syslib/Makefile
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

OBJEXT=.xwam

FLORADIR = ..

ALLOBJS = flrdebugger${OBJEXT} dynamic_data.dat static_data.dat

FLORA_INCLUDES_DIR = ../flrincludes
DYN_INCLUDES_DIR = ../includes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh
FLORA_ERRORS = $(FLORA_INCLUDES_DIR)/flora_errors.flh
FLRHEADER = $(DYN_INCLUDES_DIR)/flrheader.flh

.SUFFIXES:  .in .dat .P .H ${OBJEXT}


OPTION = [optimize]

PROLOG = none

PROLOG_COMMAND = $(PROLOG) -s -m 2000 -c 2000 -e 'segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_debug_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling .../flora2/debugger:"


all: init $(ALLOBJS) finish
	@../touch.sh cmd...
	$(PROLOG_COMMAND) < cmd...
	/bin/rm -f cmd...
	@cat $(SVFILE)

static_data.dat: static_data.in $(FLORA_TERMS)
	cp -f static_data.in static_data.dat

%.dat: %.in $(FLORA_TERMS)
	@echo 'flWrapAround($*).' >> cmd...


flrdebugger${OBJEXT}: flrdebugger.P $(FLORA_TERMS)
	@echo 'mc(flrdebugger,$(OPTION)).' >> cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "['../flora2devel']." >> cmd...
	@echo "import bootstrap_flora/0 from flora2." >> cmd...
	@echo "bootstrap_flora." >> cmd...
	@echo "[flrwraparound]." >> cmd...
	@echo "file_reopen($(SVFILE),a,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...


finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


clean:
	/bin/rm -f *~ .*~ *$(OBJEXT) *.O *.dat *.bak .#*


.DONE:
	@echo Done
