# Make file for Microsoft NMAKE

ALLOBJS = flrcommon.flh \
          flrtrailer.flh \
	  flreqltrailer.flh \
          flrscalareql.flh

!include ..\.prolog_path

.SUFFIXES: .flh .fli

ALL: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *.flh
	-@erase *.bak
	-@erase .#*

.fli.flh:
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrutils]. import flCompileInclude/1 from flrutils. flCompileInclude(%|fF). halt."

