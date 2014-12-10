#!/usr/bin/env bash

pid=$(pgrep avm)
# TODO: if only one 'avm' process is running,
#       use that one instead of creating a new one
if [ $? -eq 0 ] ;
then
    pgrep avm
    echo "terminate exiting 'avm' processes first"
    exit
fi


good=0
bad=0
bad_err=0
failed=""

if [ $1 ] ; then
    files=$1
else
    cd tests/
    files=$(ls *.py) #because ls tests/*py includes the directory
    cd ..
fi
echo $files

for f in $files ;
do
    echo -n "testing ${f}..."    

    file=tests/$f
    expect=${file}.out 
    test_out=${file}.test_out

    rm -f $test_out

    # TODO: direct stderrr to vm_out as well
    ./avm > $test_out &
    pid=$!

    python3 maml.py $file

    wait

    diff $expect $test_out > /dev/null
    ret=$?
    if [ $ret != 0 ] ; then
        echo "fail"
        bad=$((bad + 1))
        failed="$f $failed"
        cp _bc.txt ${expect}.bc
    else
        echo "ok"
        good=$((good + 1))
        rm -f $test_out
        rm -f ${expect}.bc
    fi
done


if [ $bad -ne 0 ] ; then
    echo "---------------------------------"
    for file in $failed
    do
        echo FAILED: $file 
    done
fi


echo $bad tests failed
echo $good tests passed


