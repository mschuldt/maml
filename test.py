# ___HOW TO RUN THIS EXAMPLE___
# (to compile, run 'make')
# In first terminal start the vm:
#    ./avm
#
# In second terminal, get the pid of the avm process:
#    ps -C avm
#
# Now interactively evaluate this file
#    python3 -i maml.py test.py
#
# The arduino object needs to know the id of the avm process,
# so at the python prompt type:
#   arduino.set_vm_pid(<PID from step 2>)
#
# The compiled codeblocks can now be sent to the avm process.
# at the python prompt type (one at a time so you can see how it behaves):
#    arduino.send(test_block)
#    arduino.send(another_block)
#    arduino.send(another_block)
#    ....
#
# After sending test_block and another_block the vm should be printing:
#   ...
#   4
#   164
#   14
#   ...
# 
# Each time a codeblock is sent, it will be appended to the end of the
# blockchain. This behavior is temporary (hopefully), a codeblock should
# replace another codeblock with the same name.

print("loading test.py")

arduino = Arduino(True)

@block
def test_block():
    1 + 3   #   for testing, the vm is currently modified so
    88 + 77 #   that a+b is equivalent to print(a+b);sleep(1)

@block
def another_block():
    5 + 9

# alternatively we could send the blocks here and
# non-interactively call "./maml.py test.py"
# (but not yet, the PID issue has yet to be resolved)
#
#arduino.send(test_block)
#arduino.send(another_block)

print("done loading test.py")
