#!/bin/sh

## File:      test.sh

## This file is called in ../testall.sh.
## $1 is expected to be the XMC executable
## $2 is expected to be the command options passed to XMC

echo "-------------------------------------------------------"
echo "--- Running iproto/test.sh                          ---"
echo "-------------------------------------------------------"

XMC=$1
opts=$2

# gentest.sh "$XMC $opts" FILE-TO-TEST COMMAND

../gentest.sh "$XMC $opts" test "test(1,bug)." bugtest_out
../gentest.sh "$XMC $opts" test "test(1,fix)." fixtest_out
/bin/mv -f test_new test_old
cat bugtest_out fixtest_out > test_new
/bin/rm -f bugtest_out fixtest_out
