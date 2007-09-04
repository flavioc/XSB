#!/bin/sh

## ====================================================================
## This script builds a MacOS X Installer package using MacPorts
##
## The installer is built from the latest CVS sources; you may modify 
## the cvs command below to use instead a tag or a date.
##
## The script must be run from a MacOS X computer with the MacPorts
## installed. Run the script by typing "./build_package.sh" and enter
## your admin password when asked. The package will be copied to the
## to the same directory containing the script. Use the MacOS X Finder
## "Create archive of..." command to zip the installer package before
## uploading.
##
## ====================================================================

dir=`PWD`

rm -rf xsb
rm -rf xsb-3.1-*.pkg 
rm -f xsb-3.1.tar.gz

cvs -z3 -d :pserver:anonymous@xsb.cvs.sourceforge.net:/cvsroot/xsb export -Dnow -d xsb XSB

cd xsb
chmod a+x admin/cleandist.sh
admin/cleandist.sh

cd ..
tar -czf xsb-3.1.tar.gz xsb

md5="`md5 -q xsb-3.1.tar.gz`"
sudo mkdir -p /opt/local/var/macports/distfiles/xsb
sudo cp -f xsb-3.1.tar.gz /opt/local/var/macports/distfiles/xsb/xsb-3.1.tar.gz
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 3.1/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo port -k -d destroot
sudo port -d pkg
cp -R work/xsb-3.1.pkg $dir/xsb-3.1-`uname -p`.pkg
sudo port clean
