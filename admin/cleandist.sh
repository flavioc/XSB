#!/bin/sh

## ====================================================================
## This script cleans a CVS checkout or export in preparation for
## buiding installer packages
##
## The script must be invoked from within the checkout/export directory
## ====================================================================

find . -name .svn -print0 | xargs -0 rm -rf
find . -name CVS -print0 | xargs -0 rm -rf
find . -name .cvsignore -print0 | xargs -0 rm -f
find . -name '.#*' -print0 | xargs -0 rm -f
find . -name .DS_Store -print0 | xargs -0 rm -f
find . -name '.gdb*' -print0 | xargs -0 rm -f

find . -type f -print0 | xargs -0 chmod 644
find . -type d -print0 | xargs -0 chmod 755

find . -type f -name "*.sh" -print0 | xargs -0 chmod a+x

cd build
chmod a+x configure config.guess config.sub topMakefile.in
cd ../admin/macosx
chmod a+x postflight
