# Make file for Microsoft NMAKE

OBJEXT = .xwam

ALLOBJS =

XSB=..\..\..\config\x86-pc-windows\bin\xsb.exe

OPTIONS=[optimize]

.SUFFIXES: .P .flr

ALL: $(ALLOBJS)


.flr.P:
	$(XSB) -e "bootstrap_flora. import (flCompile)/1 from flora2. flCompile(%|fF)."


CLEAN:
	-@del *~
	-@del *$(OBJEXT)
	-@del *.P
	-@del *.fdb
	-@del *.fld
	-@del *.bak
	-@del .#*
