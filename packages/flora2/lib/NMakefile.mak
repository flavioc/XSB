# Make file for Microsoft NMAKE

OBJEXT = .xwam
PROLOGEXT = .P

ALLOBJS = flrprettyprint$(PROLOGEXT) flrio$(PROLOGEXT) \
	  flrstorage$(PROLOGEXT) flrsystem$(PROLOGEXT)

!include ..\.prolog_path_wind

OPTIONS = [optimize]

.SUFFIXES: $(PROLOGEXT) .flr

ALL: $(ALLOBJS)


.flr$(PROLOGEXT):
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. import flora_compile_system_module/1 from flrutils. flora_compile_system_module(%|fF). halt."


CLEAN:
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *$(PROLOGEXT)
	-@erase *.fdb
	-@erase *.fld
	-@erase *.bak
	-@erase .#*
