#! /bin/bash
if [ "$1" ] 
then
for i in $1/*.h; do ln -s $i `basename $i .h`.h; done
for i in $1/*.o; do ln -s $i `basename $i .o`.o; done
else
    echo "Usage: "
    echo "makelinks.sh smodels/path/"
    echo
fi