arduino = Arduino(desktop=True)

#each block will will be executed in the blockchain many times before 
#the next one is sent so each of them delays while the next block
#is sent to the vm

@block
def b1():
    print_i(11)
    delay(1)
@block    
def b2():
    print_i(22)
    delay(1)
@block    
def b3():
    print_i(33)
    delay(1)
@block    
def b4():
    print_i(44)
    die(0)


arduino.send(b1)
arduino.send(b2)
arduino.send(b3)
arduino.send(b4)
