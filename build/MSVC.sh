#! /bin/sh

cat <<EOF  > .makedepend.tmp
# DO NOT DELETE THIS LINE -- make  depend  depends  on it.

EOF

cd ../emu

# Make sure we have the right config.h before runing makedepend
configdir=../config/x86-pc-windows
cp -f $configdir/config.h configs
cp -f $configdir/debug.h  debugs

# -w1000 tells makedepend to create long line, 1 dependency per source file: 
# we don't know how MSVC's NMAKE handles multiple dependencies per source file
# -Y tells not to bother with system include dirs:
# they aren't important as dependencies
# Grep is here so that the warnings about system include files 
# won't be displayed. 
makedepend -w1000 -f ../build/.makedepend.tmp -o.obj -p@@@ -Y -- -- *c 2>&1 \
	| grep -v "cannot find include" | grep -v "not in"

cat ../build/.makedepend.tmp \
        | sed  -f ../build/MSVC.sed > ../build/MSVC.dep

