#!/bin/csh

#translates XSB manual to html, tar's it and copies it over to the
#www server
#assumes that:
#	1. there is an icons subdirectory, and the file
#	 (these are the standard icons, e.g. forward, back)
#	2. .latex2html-init in the user's home dir.
#	3. the .rhosts file in the www server includes the machine
#	   the translation is being done
#instructions:
#	1. cd to the manual directory
#	2. run the script: htmlmanual.csh

echo "XSB Manual"
echo "latex2html: This might take a LONG time..."

latex2html manual.tex

echo "latex2html: done"

echo "Making images transparent..."
(cd manual; ../cleargif.csh)
echo "...done"

echo "Copying icons..."
cp -r icons manual
echo "...done"

echo "Creating tar-compressed file..."
tar chvf manual.tar manual
if -r manual.tar.Z then
	\rm manual.tar.Z
endif
compress manual.tar
echo "...done"

echo "Copying over to www..."
rcp manual.tar.Z www:public_html/man.tar.Z
if -r manual.ps then
	gzip manual.ps
	rcp manual.ps.gz www:public_html/manual.ps.gz
endif

rcp -r manual www:public_html/manual

echo "done."
