#! /bin/sh

PROLOG_COMMAND=$1

./touch.sh cmd...

split -l 7 cmd... cmd..._

for f in cmd..._*; do
     cat cmd...hdr $f | "$PROLOG_COMMAND"
done

rm cmd... cmd...hdr cmd..._*
