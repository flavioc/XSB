#!/bin/sh

## File:      test.sh

## This file is called in ../testall.sh.
## $1 is expected to be the XMC executable
## $2 is expected to be the command options passed to XMC

echo "-------------------------------------------------------"
echo "--- Running Leader/test.sh                          ---"
echo "-------------------------------------------------------"

XMC=$1
opts=$2

# gentest.sh "$XMC $opts" FILE-TO-TEST COMMAND

../gentest.sh "$XMC $opts" test "test(2,ae_leader)." ae_out
../gentest.sh "$XMC $opts" test "test(2,one_leader)." one_out
/bin/mv -f test_new test_old
cat ae_out one_out > test_new
/bin/rm -rf ae_out one_out
