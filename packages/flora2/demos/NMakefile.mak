# Make file for Microsoft NMAKE

!include ..\.prolog_path

OBJEXT = .xwam
PROLOGEXT = .P

ALLOBJS = aggregate$(PROLOGEXT) benchmark$(PROLOGEXT) \
	  default$(PROLOGEXT) family_obj$(PROLOGEXT) \
	  family_rel$(PROLOGEXT) flogic_basics$(PROLOGEXT) \
	  metavar$(PROLOGEXT) mix$(PROLOGEXT) module1$(PROLOGEXT) \
	  mod1$(PROLOGEXT) \
	  mono_inherit$(PROLOGEXT) rel_ops$(PROLOGEXT) \
	  tree_traversal$(PROLOGEXT)

OPTIONS = [optimize]

.SUFFIXES: $(PROLOGEXT) .flr

ALL: $(ALLOBJS)


.flr$(PROLOGEXT):
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. import (flCompile)/1 from flora2. flCompile(%|fF). halt."


CLEAN:
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *$(PROLOGEXT)
	-@erase *$(PROLOGEXT)_gpp
	-@erase *.fdb
	-@erase *.fld
	-@erase *.bak
	-@erase .#*

