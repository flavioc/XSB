# Generated automatically from Makefile.in by configure.
#! /bin/sh

JC=javac
JM=java
JCLASSPATH= -classpath .:junit.jar:tests:tempCompiled
XSB_DIR=xsb
SUBDIR=tempCompiled

all:
	rm -r tempCompiled;\
	mkdir tempCompiled;\
	$(JC) -d tempCompiled $(JCLASSPATH) tests/com/declarativa/interprolog/*.java tests/com/xsb/interprolog/*.java com/declarativa/interprolog/*.java com/declarativa/interprolog/util/*.java com/declarativa/interprolog/gui/*.java com/declarativa/interprolog/examples/*.java com/xsb/interprolog/*.java;\
	./compile_Ps $(XSB_DIR);\
	cp com/declarativa/interprolog/*.xwam tempCompiled/com/declarativa/interprolog/;\
	cp com/declarativa/interprolog/gui/*.xwam tempCompiled/com/declarativa/interprolog/gui;\
#Added - to create folder images inside gui
	mkdir tempCompiled/com/declarativa/interprolog/gui/images;\
	cp com/declarativa/interprolog/gui/images/* tempCompiled/com/declarativa/interprolog/gui/images;\
	rm interprolog.jar;\
	cd tempCompiled;\
	jar cf ../interprolog.jar *;\
	cd ..;\
	rm -r tempCompiled

clean: 
	rm -r tempCompiled
