#
#	File		: XSB/examples/Makefile
#	Authors		: Kostis Sagonas, Jiyang Xu
#	Last updated	: April 5, 1993
#

ALL =	fib.O farmer.O ham.O houses.O qsort.O\
	queens.O mandel.O map.O nrev.O lips_mega.O\
	first.O ptq.O ptq_utils.O ptqlex.O parser.O ptq_examples.O\
	shortest_path.O wfs.O

Option = []
Prolog = ../emu/xsb -i -m 600 -c 400

# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P .O .H
.P.O:
	echo 'mc($*, $(Option)).' >> cmd...

all: $(ALL)
	echo 'statistics.' >> cmd...
	$(Prolog) < cmd...
	/bin/rm cmd...
# Unfortunately, this needs to be commented on the NeXT
# %.O:	%.H

.DONE:
	echo Done
	

