# Make file for Microsoft NMAKE


ALLOBJS = flrpatch.flh flrpreddef.flh flrindex_P.flh \
	  flrdyna.flh flrdynz.flh \
	  flrexportcheck.flh \
	  flrtable.flh flrhilogtable.flh \
	  flrrefreshtable.flh

!IF EXISTS (..\.prolog_path_wind) 
!INCLUDE ..\.prolog_path_wind
!ENDIF

.SUFFIXES: .fli .flh

ALL: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase .#*
	-@erase *.flh
	-@erase *.bak

flrpreddef.flh:
	$(PROLOG) -e "asserta(library_directory('..')). ['..\flora2']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrwraparound]. flWrapAround(flrpreddef). halt."

flrindex_P.flh:
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrwraparound]. flWrapAround(flrindex_P). halt."


.fli.flh:
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrwraparound]. import flWrapAround/1 from flrwraparound. flWrapAround(%|fF). halt." 
