## File:      clpqr/Makefile
## Author(s): Baoqiu Cui
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

OBJEXT=.xwam

XSBDIR=../..

ALL = 	arith${OBJEXT} arith_r${OBJEXT} bv${OBJEXT} class${OBJEXT} \
	clpr${OBJEXT} ineq${OBJEXT} itf3${OBJEXT} \
	fourmotz${OBJEXT} nf${OBJEXT} ordering${OBJEXT} \
	project${OBJEXT} redund${OBJEXT} store${OBJEXT} \
	dump${OBJEXT}

# Option = [optimize, spec_off]

Option = [spec_off]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.9 or higher
#
XSB=$(XSBDIR)/bin/xsb
Prolog = $(XSB)
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.clpqr.tmp'
OWRITE = 1
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/clpqr:"


# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P ${OBJEXT} .H

all: init $(ALL) finish
	@touch cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)

.P${OBJEXT}:
	echo 'mc($*, $(Option)).' >> cmd...

init:
	/bin/rm -f $(SVFILE)
	@echo "file_reopen($(SVFILE),$(OWRITE),$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...

.PHONY: clean

clean:
	/bin/rm -f *${OBJEXT} *.O *~ .*~ *.bak

.DONE:
	echo Done

