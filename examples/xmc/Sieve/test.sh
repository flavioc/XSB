#!/bin/sh

## File:      test.sh

## This file is called in ../testall.sh.
## $1 is expected to be the XMC executable
## $2 is expected to be the command options passed to XMC

echo "-------------------------------------------------------"
echo "--- Running Sieve/test.sh                           ---"
echo "-------------------------------------------------------"

XMC=$1
opts=$2

# gentest.sh "$XMC $opts" FILE-TO-TEST COMMAND

../gentest.sh "$XMC $opts" test "test(3, 4, 17, ae_finish)." test_out
/bin/mv -f test_new test_old
/bin/mv -f test_out test_new

