#!/bin/sh 

    # Create an XSB tarball sans testsuite; untar in the XSB install directory

    # RUN this in ./admin/ directory!

files="./LICENSE ./INSTALL ./INSTALL_PROBLEMS ./INSTALL_WINDOWS \
	./README ./FAQ ./Makefile \
	./admin \
      ./build/ac* ./build/*.in ./build/config.guess ./build/config.sub \
      ./build/*sh ./build/*.msg ./build/configure ./build/README \
      ./emu/README     ./emu/*.[chi]     ./emu/*.pc    ./emu/*.mk \
      ./emu/configs ./emu/debugs ./emu/orastuff \
      ./syslib/README  ./syslib/Makefile ./syslib/*.[DHPO] \
      ./cmplib/README  ./cmplib/Makefile ./cmplib/*.[DHPO] \
      ./lib/README     ./lib/Makefile    ./lib/*.[DHPO] \
      ./etc \
      ./packages \
      ./docs \
      ./examples "

    cd ..

    tar cvf XSB.tar $files

    gzip -f XSB.tar
