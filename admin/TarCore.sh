#!/bin/sh 

    # Create an XSB tarball; to be untarr'ed in the XSB installation directory

    # Run this in ./admin/ directory!

files="./XSB/LICENSE ./XSB/INSTALL ./XSB/INSTALL_PROBLEMS ./XSB/INSTALL_WINDOWS \
	./XSB/README ./XSB/FAQ ./XSB/Makefile \
	./XSB/admin \
        ./XSB/build/ac* ./XSB/build/*.in ./XSB/build/config.guess \
        ./XSB/build/config.sub ./XSB/build/*sh ./XSB/build/*.msg \
        ./XSB/build/configure ./XSB/build/README \
        ./XSB/emu ./XSB/syslib ./XSB/cmplib ./XSB/lib \
        ./XSB/etc \
        ./XSB/packages "

    cd ../..

    tar cvf XSB/XSB.tar $files

    gzip -f XSB/XSB.tar
    chmod 644 XSB/XSB.tar.gz
