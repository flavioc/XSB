#!/bin/csh -f

if ( ($#argv > 2) || ($#argv < 1) ) then
  echo "Usage:	`basename $0` Volume# [pdf|html]"
  exit(1)
endif

set manual = manual{$1}

if ( ($#argv == 1) || ($2 == pdf) ) then
  latex ${manual}	
  bibtex ${manual}
  latex ${manual}	# now cites are correct, but references aint
  makeindex ${manual}	# assuming no indexes in the bibliography
  latex ${manual}	# now index entries to all pages except index itself
  if ($1 == 1) then 
    makeindex manual1.prdx -o manual1.pnd
  endif
  latex ${manual}	# now index entries to all pages except index itself
			#   is ok...
  latex ${manual}	# make that correct too
  dvips -t letter -o ${manual}.ps ${manual}.dvi
  ps2pdf ${manual}.ps
  \rm -f ${manual}.ps
endif

if ( ($#argv == 1) || ($2 == html) ) then
  latex2html -local_icons -scalable_fonts -show_section_numbers ${manual}
endif

exit(0)
