#!/bin/sh 

    # Create a FLORA-2 tarball 

    # RUN this in ./admin/ directory!

files="./flora2/LICENSE ./flora2/INSTALLATION \
        ./flora2/Makefile ./flora2/NMakefile.mak \
    	./flora2/makeflora ./flora2/makeflora.bat \
        ./flora2/*.sh ./flora2/*.P ./flora2/*.H ./flora2/*.flh\
    	./flora2/closure/Makefile ./flora2/closure/NMakefile.mak \
    	./flora2/closure/*.fli ./flora2/closure/includes/*.fli \
    	./flora2/debugger/Makefile ./flora2/debugger/NMakefile.mak \
    	./flora2/debugger/*.in ./flora2/debugger/*.P \
    	./flora2/demos/*.flr ./flora2/demos/*.sh \
    	./flora2/demos/Makefile ./flora2/demos/NMakefile.mak \
    	./flora2/docs/flora2.pdf \
    	./flora2/docs/technical.txt \
    	./flora2/emacs/flora.el \
    	./flora2/flrincludes/*.flh \
    	./flora2/genincludes/Makefile ./flora2/genincludes/NMakefile.mak \
    	./flora2/genincludes/*.fli \
    	./flora2/headerinc/*.flh \
    	./flora2/includes/*.flh \
    	./flora2/lib/*.flr ./flora2/lib/Makefile ./flora2/lib/NMakefile.mak \
    	./flora2/libinc/*.flh \
    	./flora2/p2h/prolog2hilog.* ./flora2/p2h/*.P \
        ./flora2/p2h/Makefile ./flora2/p2h/NMakefile.mak \
    	./flora2/pkgs/Makefile ./flora2/pkgs/NMakefile.mak \
    	./flora2/syslib/*.P ./flora2/syslib/*.H ./flora2/syslib/*.sh \
    	./flora2/syslib/Makefile ./flora2/syslib/NMakefile.mak \
    	./flora2/syslibinc/*.flh  \
        ./flora2/trailer/Makefile ./flora2/trailer/NMakefile.mak \
        ./flora2/trailer/README"

    curdir=`pwd`
    curdir=`basename $curdir`
    if test ! $curdir = admin; then
	echo "+++ This script must be run out of flora2/admin/ directory"
	exit 1
    fi

    cd ../..

    tar cvf flora2/flora2.tar $files

    gzip -f flora2/flora2.tar
