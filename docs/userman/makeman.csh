#!/bin/csh -f

if ( ($#argv > 2) || ($#argv < 1) ) then
  echo "Usage:	`basename $0` Volume# [ps|html]"
  exit(1)
endif

set manual = manual{$1}

if ( ($#argv == 1) || ($2 == ps) ) then
  latex ${manual}	
  bibtex ${manual}
  latex ${manual}	# now cites are correct, but references aint
  makeindex ${manual}	# assuming no indexes in the bibliography
  latex ${manual}	# now index entries to all pages except index itself
			#   is ok...
  latex ${manual}	# make that correct too
  dvips -t letter -o ${manual}.ps ${manual}.dvi
endif

if ( ($#argv == 1) || ($2 == html) ) then
  latex2html ${manual}
endif

exit(0)
