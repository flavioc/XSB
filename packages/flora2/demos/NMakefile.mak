# Make file for Microsoft NMAKE

OBJEXT = .O

ALLOBJS = aggregate.P benchmark.P default.P family_obj.P \
	  family_rel.P flogic_basics.P metavar.P mix.P module1.P mod1.P \
	  mono_inherit.P rel_ops.P tree_traversal.P

XSB = ..\..\..\config\x86-pc-windows\bin\xsb.exe

OPTIONS = [optimize]

.SUFFIXES: .P .flr

ALL: $(ALLOBJS)


.flr.P:
	$(XSB) -e "bootstrap_flora. import (flCompile)/1 from flora2. flCompile(%|fF). halt."


CLEAN:
	-@del *~
	-@del *$(OBJEXT)
	-@del *.P
	-@del *.P_gpp
	-@del *.fdb
	-@del *.fld
	-@del *.bak
	-@del .#*

