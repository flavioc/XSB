#! /bin/sh

## File:      copysubdirs.sh -- Copy all subdirectories of $1 to $2 recursively
## Author(s): kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1998
## 
## XSB is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## XSB is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with XSB; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id$
## 
##

    # Paths $1 and $2 are assumed to be relative to the current directory!

if test ! -d "$1" ; then
  echo "\`$1': not a directory"
  exit
fi
if test ! -d "$2" ; then
  echo "\`$2': not a directory"
  exit
fi

cur_dir=`pwd`

cd $1
files=`ls`

cd $cur_dir
umask 022

for f in $files ; do
  if test -d "$1/$f" && test ! "$f" = "CVS" ; then
    echo "Copying $1/$f to $2"
    cp -rpf $1/$f  $2
  fi
done

# This also makes plain files executable. Any easy way to add x to dirs only? 
chmod -R a+rx,u+w $2
