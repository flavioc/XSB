# File: XSB/docs/userman/Makefile
#
# $Id$

.PHONY: all manual1 manual2

all: manual1 manual2

manual1:
	latex manual1	
	bibtex manual1
	latex manual1
	latex manual1
	dvips manual1.dvi -o manual1.ps
	dvipdf manual1.dvi

manual2:
	latex manual2	
	bibtex manual2
	latex manual2
	latex manual2
	dvips manual2.dvi -o manual2.ps
	dvipdf manual2.dvi
