# Make file for Microsoft NMAKE

!IF EXISTS (..\.prolog_path_wind) 
!INCLUDE ..\.prolog_path_wind
!ENDIF

OBJEXT = .xwam
PROLOGEXT = .P

ALLOBJS =  flranswer$(OBJEXT) \
	   flrcontrol$(OBJEXT) \
	   flrclause$(OBJEXT) \
	   flrdisplay$(OBJEXT) \
	   flrload$(OBJEXT) \
	   flraggavg$(OBJEXT) \
	   flraggcolbag$(OBJEXT) \
	   flraggcolset$(OBJEXT) \
	   flraggcount$(OBJEXT) \
	   flraggmax$(OBJEXT) \
	   flraggmin$(OBJEXT) \
	   flraggsum$(OBJEXT) \
	   flrstorageutils$(OBJEXT) \
	   flrstoragebase$(OBJEXT) \
	   flrdbop$(OBJEXT) \
	   flrbtdbop$(OBJEXT) \
	   flrshdirect$(OBJEXT) \
	   flrdynmod$(OBJEXT) \
	   flrdynrule$(OBJEXT) \
	   flrsemantics$(OBJEXT) \
	   flrnegation$(OBJEXT) \
	   flrtables$(OBJEXT) \
	   flrimportedcalls$(OBJEXT) \
	   flrerrhandler$(OBJEXT) \
	   flrdecode$(OBJEXT)  \
	   flrcanon$(OBJEXT)  \
	   flrmetaops$(OBJEXT)  \
	   flrtrim$(OBJEXT) \
	   flrexport$(OBJEXT) \
	   flrimport$(OBJEXT)

OPTIONS = [optimize]

.SUFFIXES: $(PROLOGEXT) $(OBJEXT)

ALL:: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *.bak
	-@erase .#*


$(PROLOGEXT)$(OBJEXT):
	$(PROLOG) -e "asserta(library_directory('..')). ['..\flora2']. import bootstrap_flora/0 from flora2. bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."


