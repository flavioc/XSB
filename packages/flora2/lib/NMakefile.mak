# Make file for Microsoft NMAKE

OBJEXT = .xwam
PROLOGEXT = .P

ALLOBJS = flrprettyprint$(PROLOGEXT) flrio$(PROLOGEXT) \
	  flrstorage$(PROLOGEXT) flrsystem$(PROLOGEXT)

!IF EXISTS (..\.prolog_path_wind) 
!INCLUDE ..\.prolog_path_wind
!ENDIF

OPTIONS = [optimize]

.SUFFIXES: $(PROLOGEXT) .flr

ALL: $(ALLOBJS)


.flr$(PROLOGEXT):
	$(PROLOG) -e "asserta(library_directory('..')). ['..\flora2']. import bootstrap_flora/0 from flora2. bootstrap_flora. import flora_compile_system_module/1 from flrutils. flora_compile_system_module(%|fF). halt."


CLEAN:
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *$(PROLOGEXT)
	-@erase *.fdb
	-@erase *.fld
	-@erase *.bak
	-@erase .#*
