# Make file for Microsoft NMAKE


ALLOBJS = flrpatch.flh flrpreddef.flh flrindex_P.flh

!include ..\.prolog_path

.SUFFIXES: .fli .flh

ALL: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase .#*
	-@erase *.flh
	-@erase *.bak

flrpreddef.flh:
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrwraparound]. flWrapAround(flrpreddef). halt."

flrindex_P.flh:
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrwraparound]. flWrapAround(flrindex_P). halt."


.fli.flh:
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrutils]. import flCompileInclude/1 from flrutils. flCompileInclude(%|fF). halt."

