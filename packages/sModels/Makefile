## File:      packages/SModels/Makefile
## Author(s): luis (adapted from packages/flora/Makefile)
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1998
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

ALL = smodels.O smodelsops.O

SMODELSOBJ = api.o atomrule.o dcl.o denant.o improve.o list.o main.o \
	program.o queue.o read.o smodels.o stable.o stack.o timer.o tree.o

SMODELSHEADER= api.h atomrule.h dcl.h defines.h list.h print.h program.h \
	queue.h read.h smodels.hh stable.h stack.h timer.h tree.h

SMODELS = ~luis/Programs/smodels-2.23/

Option = [optimize]

XSB=../../bin/xsb
Prolog = $(XSB) 

.IGNORE:

.SUFFIXES: .P .O .H .so

.H.O:
	echo 'compile($*, $(Option)).' >> cmd...

.P.O:
	echo 'mc($*,$(Option)).' >> cmd...

all: $(ALL)
	@touch cmd...
	/bin/ln -s $(SMODELS)/*.o .
	/bin/ln -s $(SMODELS)/*.h .
	$(Prolog) < cmd...
	/bin/rm cmd...

clean: 
	/bin/rm *.o *.so *.O *.h

.DONE:
	echo Done
