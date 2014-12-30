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

################################################################################
#run true tests

if [ $1 ] ; then
    files=$1
else
    cd tests/
    files=$(ls *.maml) #because ls tests/*py includes the directory
    cd ..
fi

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

    python3 maml.py -d $file

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

################################################################################
#run false tests

if [ $1 ] ; then
    files=$1
else
    cd false_tests/
    files=$(ls *.maml)  # because ls false_tests/*py includes the directory
    cd ..
fi

for f in $files ;
do
    echo -n "testing ${f}..."

    file=false_tests/$f
    expect=${file}.err
    error_out=${file}.err_out

    rm -f $error_out

    python3 maml.py -d $file 2> ${error_out}
    tail -n 1 $error_out > ${error_out}.tmp
    mv ${error_out}.tmp $error_out

    wait

    diff $expect $error_out > /dev/null
    ret=$?
    if [ $ret != 0 ] ; then
        echo "fail"
        bad=$((bad + 1))
        failed="$f $failed"
        cp _bc.txt ${expect}.bc
    else
        echo "ok"
        good=$((good + 1))
        rm -f $error_out
        rm -f ${expect}.bc
    fi
done

################################################################################
# report


if [ $bad -ne 0 ] ; then
    echo "---------------------------------"
    for file in $failed
    do
        echo FAILED: $file
    done
fi

echo $bad tests failed
echo $good tests passed
