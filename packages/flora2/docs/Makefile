
all:  flora2.dvi flora2.ps flora2.pdf

flora2.dvi: flora2.tex
	@echo
	@echo Compiling...
	latex flora2
	@echo
	@echo Making bibliography...
	bibtex flora2
	@echo
	@echo Making index...
	makeindex flora2
	@echo
	@echo Recompiling...
	latex flora2
	latex flora2

flora2.ps: flora2.dvi
	@echo
	@echo Making postscript...
	dvips flora2.dvi -o flora2.ps

flora2.pdf: flora2.dvi
	@echo
	@echo Making pdf...
	pdflatex flora2

clean:
	/bin/rm -f  *.bak *~ .*~ .#*
	/bin/rm -f  *.aux *.bbl *.blg *.dvi *.idx *.ilg *.ind *.log \
			*.out *.pdf *.ps *.toc
