# Make file for Microsoft NMAKE

!IF EXISTS (..\.prolog_path_wind) 
!INCLUDE ..\.prolog_path_wind
!ENDIF

OBJEXT = .O
PROLOGEXT = .P

OPTIONS = [optimize]

.SUFFIXES: $(PROLOGEXT) .O

ALL:: 
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora. [flrutils]. import flsetuptrailer/1, flsetuppatch/0 from flrutils. flsetuptrailer(none). flsetuptrailer(basic). flsetuptrailer(flogic). flsetuppatch. halt."

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *$(PROLOGEXT)
	-@erase *.flh
	-@erase *.bak
	-@erase .#*




