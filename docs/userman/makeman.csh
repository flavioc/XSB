#!/bin/csh -f

echo **** Making XSB manual for local consumption.
echo **** To release to the web server, please follow usual release procedure.

latex manual1	
bibtex manual1
latex manual1	# now, \cites are correct, but references aint, but thats ok.
MAKE_INDEX/makeindex manual  # assuming no indexes in the bibliography
latex manual1	# now, index entries to all pages xcept index itself is ok.
latex manual1	# make that correct too.

# Now that we have made .dvi file,

dvips manual1.dvi -o manual1.ps

# we're ready for the html part

echo **** Making HTML version; don't worry, it'll be done by tomorrow...

latex2html manual

echo **** latex2html done, Making images transparent
(cd manual; ../cleargif.csh)
 
echo **** Copying icons...
cp -r icons manual
echo ****                 done.

