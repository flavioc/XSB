# Make file for Microsoft NMAKE

ALLOBJS =

XSB=..\..\..\config\x86-pc-windows\bin\xsb.exe

OPTIONS=[optimize]

.SUFFIXES: .P .flr

ALL: $(ALLOBJS)


.flr.P:
	$(XSB) -e "bootstrap_flora. import (flCompile)/1 from flora2. flCompile(%|fF)."


CLEAN:
	-@del *~
	-@del *.O
	-@del *.P
	-@del *.fdb
