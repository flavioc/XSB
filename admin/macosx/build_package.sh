#!/bin/sh

## ====================================================================
## This script builds a MacOS X Installer packages using MacPorts
##
## The installer is built from the latest CVS sources; you may modify 
## the cvs command below to use instead a tag or a date.
##
## The script must be run from a MacOS X computer with the MacPorts
## installed. Run the script by typing "./build_packages.sh" and enter
## your admin password when asked. The packages will be copied to the
## to the same directory containing the script. Use the MacOS X Finder
## "Create archive of..." command to zip each installer package before
## uploading.
##
## ====================================================================

dir=`PWD`

echo "Removing old files..."
rm -rf xsb
rm -rf xsb-3.1-*.pkg*
rm -f xsb-3.1.tar.gz

echo "Retrieving current XSB CVS version..."
cvs -z3 -d :pserver:anonymous@xsb.cvs.sourceforge.net:/cvsroot/xsb export -Dnow -d xsb XSB

echo "Cleaning up exported XSB CVS version..."
cd xsb
chmod a+x admin/cleandist.sh
admin/cleandist.sh

echo "Creating XSB sources archive..."
cd ..
tar -czf xsb-3.1.tar.gz xsb

echo "Updating MacPorts XSB portfile..."
md5="`md5 -q xsb-3.1.tar.gz`"
sudo mkdir -p /opt/local/var/macports/distfiles/xsb
sudo cp -f xsb-3.1.tar.gz /opt/local/var/macports/distfiles/xsb/xsb-3.1.tar.gz
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 3.1/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile

echo "Creating XSB single-threaded, 32 bits version installer..."
sudo port -k -d destroot +bits32st
sudo port -d pkg
cp -R work/xsb-3.1.pkg $dir/xsb-3.1-`uname -p`.pkg
sudo port clean

echo "Creating XSB multi-threaded, 32 bits version installer..."
sudo port -k -d destroot +bits32mt
sudo port -d pkg +mt
cp -R work/xsb-3.1.pkg $dir/xsb-3.1-mt-`uname -p`.pkg
sudo port clean

echo "Creating XSB single-threaded, 64 bits version installer..."
sudo port -k -d destroot +bits64st
sudo port -d pkg +bits64
cp -R work/xsb-3.1.pkg $dir/xsb-3.1-64bits-`uname -p`.pkg
sudo port clean

echo "Creating XSB multi-threaded, 64 bits version installer..."
sudo port -k -d destroot +bits64mt
sudo port -d pkg +mt +bits64mt
cp -R work/xsb-3.1.pkg $dir/xsb-3.1-mt-64bits-`uname -p`.pkg
sudo port clean
