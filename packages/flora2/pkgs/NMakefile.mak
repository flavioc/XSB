# Make file for Microsoft NMAKE

!include ..\.prolog_path

OBJEXT = .xwam
PROLOGEXT = .P

ALLOBJS =

OPTIONS=[optimize]

.SUFFIXES: $(PROLOGEXT) .flr

ALL: $(ALLOBJS)


.flr$(PROLOGEXT):
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. import (flCompile)/1 from flora2. flCompile(%|fF)."


CLEAN:
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *$(PROLOGEXT)
	-@erase *.fdb
	-@erase *.fld
	-@erase *.bak
	-@erase .#*
