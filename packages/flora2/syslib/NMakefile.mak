# Make file for Microsoft NMAKE

!include ..\.prolog_path

OBJEXT = .xwam
PROLOGEXT = .P

ALLOBJS =  flranswer$(OBJEXT) \
	   flrcontrol$(OBJEXT) \
	   flrdisplay$(OBJEXT) \
	   flrload$(OBJEXT) \
	   flraggavg$(OBJEXT) \
	   flraggcolbag$(OBJEXT) \
	   flraggcolset$(OBJEXT) \
	   flraggcount$(OBJEXT) \
	   flraggmax$(OBJEXT) \
	   flraggmin$(OBJEXT) \
	   flraggsum$(OBJEXT) \
	   flrstorage$(OBJEXT) \
	   flrdbop$(OBJEXT) \
	   flrbtdbop$(OBJEXT) \
	   flrshdirect$(OBJEXT) \
	   flrdynmod$(OBJEXT) \
	   flrequality$(OBJEXT) \
	   flrimport$(OBJEXT) \
	   flrexpunge$(OBJEXT)

OPTIONS = [optimize]

.SUFFIXES: $(PROLOGEXT) $(OBJEXT)

ALL:: $(ALLOBJS)

CLEAN :
	-@erase *~
	-@erase *$(OBJEXT)
	-@erase *.bak
	-@erase .#*


$(PROLOGEXT)$(OBJEXT):
	$(PROLOG) -e "['..\flora2devel']. import bootstrap_flora/0 from flora2. bootstrap_flora,mc(%|fF,$(OPTIONS)). halt."


