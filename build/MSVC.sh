#! /bin/sh

cat <<EOF  > .makedepend.tmp
# DO NOT DELETE THIS LINE -- make  depend  depends  on it.

EOF

cd ../emu

configdir=../config/x86-pc-windows

# -w2000 tells makedepend to create long line, 1 dependency per source file:
# we don't know how MSVC's NMAKE handles multiple dependencies per source file
# -Y tells not to bother with system include dirs:
# they aren't important as dependencies
# Grep is here so that the warnings about system include files
# won't be displayed.
makedepend -w2000 -f ../build/.makedepend.tmp -o.obj -p@@@ -Y -- -I$configdir -- *c 2>&1 \
	| grep -v "cannot find include" | grep -v "not in"

# Convert Unix Makefile dependencies to NMAKE format, add ^M at the end
cat ../build/.makedepend.tmp | sed  -f ../build/MSVC.sed | unix2dos --unix2dos > ../build/MSVC.dep


