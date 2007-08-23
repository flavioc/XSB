#!/bin/sh

## ================================================================
## This script build a MacOS X Installer package using MacPorts
## ================================================================

dir=`PWD`

cvs -z3 -d :pserver:anonymous@xsb.cvs.sourceforge.net:/cvsroot/xsb export -Dnow -d xsb XSB

cd xsb
find . -type f -print0 | xargs -0 chmod 644
find . -type d -print0 | xargs -0 chmod 755
chmod a+x build/configure
cd ..
tar -czf xsb-3.1.tar.gz xsb

md5="`md5 -q xsb-3.1.tar.gz`"
sudo mkdir -p /opt/local/var/macports/distfiles/xsb
sudo cp -f xsb-3.1.tar.gz /opt/local/var/macports/distfiles/xsb/xsb-3.1.tar.gz
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 3.1/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo port -d destroot
sudo port -d pkg
cp -R work/xsb-3.1.pkg $dir
sudo port clean
