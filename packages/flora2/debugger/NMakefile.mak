# Make file for Microsoft NMAKE

!IF EXISTS (..\.prolog_path_wind) 
!INCLUDE ..\.prolog_path_wind
!ENDIF

OBJEXT = .O
PROLOGEXT = .P

ALLOBJS=  flrdebugger$(OBJEXT) dynamic_data.dat static_data.dat

OPTIONS=[optimize]

.SUFFIXES:  .in .dat $(PROLOGEXT) .H $(OBJEXT)

ALL:: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase .#*
	-@erase *.dat
	-@erase *.bak

$(PROLOGEXT)$(OBJEXT):
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."

static_data.dat: static_data.in
	copy static_data.in static_data.dat

dynamic_data.dat: dynamic_data.in
	$(PROLOG) -e "asserta(library_directory('..')). ['..\flora2']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrwraparound]. import flrWrapAround/1 from flrwraparound. flWrapAround(%|fF). halt.


