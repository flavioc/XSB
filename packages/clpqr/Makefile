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

ALL = arith.O arith_r.O assoc.O bv.O class.O clpr.O ineq.O itf3.O\
fourmotz.O nf.O ordering.O project.O redund.O store.O ugraphs.O\
dump.O

# Option = [optimize, spec_off]

Option = [spec_off]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.9 or higher
#
XSB=../bin/xsb
Prolog = $(XSB)
#
#-----------------------------------------------------------------------#

# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P .O .H

.H.P:
	echo 'mc($*, $(Option)).' >> cmd...

.P.O:
	echo 'mc($*, $(Option)).' >> cmd...

all: $(ALL)
	@touch cmd...
	$(Prolog) < cmd...
	/bin/rm cmd...

# Unfortunately, this needs to be commented on the NeXT.
# %.O:	%.H

.DONE:
	echo Done
