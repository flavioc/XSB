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
PROLOGEXT = .P
PROLOGHEADEREXT=.H

PROLOG = none
FLORADIR = ..

ALLOBJS = flranswer$(OBJEXT) \
	  flrcontrol$(OBJEXT) \
	  flrclause$(OBJEXT) \
	  flrdisplay$(OBJEXT) \
	  flrload$(OBJEXT) \
	  flraggavg$(OBJEXT) \
	  flraggcolbag$(OBJEXT) \
	  flraggcolset$(OBJEXT) \
	  flraggcount$(OBJEXT) \
	  flraggmax$(OBJEXT) \
	  flraggmin$(OBJEXT) \
	  flraggsum$(OBJEXT) \
	  flrstorageutils$(OBJEXT) \
	  flrstoragebase$(OBJEXT) \
	  flrdbop$(OBJEXT) \
	  flrbtdbop$(OBJEXT) \
	  flrshdirect$(OBJEXT) \
	  flrdynmod$(OBJEXT) \
	  flrdynrule$(OBJEXT) \
	  flrsemantics$(OBJEXT) \
	  flrnegation$(OBJEXT) \
	  flrtables$(OBJEXT) \
	  flrimportedcalls$(OBJEXT) \
	  flrerrhandler$(OBJEXT)  \
	  flrdecode$(OBJEXT)  \
	  flrcanon$(OBJEXT)  \
	  flrmetaops$(OBJEXT)  \
	  flrtrim$(OBJEXT) \
	  flrexport$(OBJEXT) \
	  flrimport$(OBJEXT)

FLORA_INCLUDES_DIR = ../flrincludes
DYN_INCLUDES_DIR = ../includes

FLORA_TERMS = $(FLORA_INCLUDES_DIR)/flora_terms.flh
FLORA_ERRORS = $(FLORA_INCLUDES_DIR)/flora_errors.flh
FLRHEADER = $(DYN_INCLUDES_DIR)/flrheader.flh


OPTION = [optimize]

PROLOG_COMMAND = $(PROLOG) -s -m 2000 -c 2000 -e 'segfault_handler(warn).'

# Make sure warnings are saved in a file.
SVFILE = '$(FLORADIR)/.flora_syslib_warn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling .../flora2/syslib:"

.SUFFIXES: $(PROLOGEXT) $(PROLOGHEADEREXT) $(OBJEXT)

all: init $(ALLOBJS) finish
	./CompileChangedFiles.sh $(PROLOG_COMMAND)
	@cat $(SVFILE)


%$(OBJEXT): %$(PROLOGEXT) $(FLORA_TERMS)
	@echo 'mc($*,$(OPTION)).' >> cmd...

%$(PROLOGEXT) : %$(PROLOGHEADEREXT)
	../touch.sh $@

flrcontrol$(OBJEXT) : flrcontrol$(PROLOGEXT) $(FLORA_TERMS) $(XSB_STANDARD)
	@echo 'mc(flrcontrol,$(OPTION)).' >> cmd...

flrdisplay$(OBJEXT) : flrdisplay$(PROLOGEXT) $(FLORA_TERMS) $(XSB_STANDARD)
	@echo 'mc(flrdisplay,$(OPTION)).' >> cmd...

flrimport$(OBJEXT): flrimport$(PROLOGEXT) $(FLORA_TERMS) $(FLRHEADER)
	@echo 'mc(flrimport,$(OPTION)).' >> cmd...


init:
	/bin/rm -f $(SVFILE)
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN),_)." >> cmd...
	@echo "asserta(library_directory('..'))." >> cmd...hdr
	@echo "['../flora2']." >> cmd...hdr
	@echo "import bootstrap_flora/0 from flora2." >> cmd...hdr
	@echo "bootstrap_flora." >> cmd...hdr
	@echo "file_reopen($(SVFILE),a,$(STDWARN),_)." >> cmd...hdr
	@echo "file_clone($(STDWARN),$(STDERR),_)." >> cmd...hdr


finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...


clean:
	/bin/rm -f core *~ .*~ *$(OBJEXT) *.O *.bak .#*


.DONE:
	@echo Done
