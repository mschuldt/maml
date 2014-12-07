#!/usr/bin/env bash

# this script starts a VM, and then runs the python program
# This prevents you from having start the vm in another window
# before running the python program

#Usage:
#   ./run.sh filename.py

pid=$(pgrep avm)
#TODO: if only one 'avm' process is running,
#      use that one instead of creating a new one
if [ $? -eq 0 ] ;
then
    pgrep avm
    echo "terminate exiting 'avm' processes first"
    exit
fi

out=_test_out

rm -f $out

#the vm needs to start before python sends it the interupt,
#this works because python startup is so slow
#
python3 maml.py $1 & #> /dev/null &
./avm

