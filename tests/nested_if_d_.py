arduino = Arduino(desktop=True)

#each block will will be executed in the blockchain many times before 
#the next one is sent so each of them delays while the next block
#is sent to the vm

@block
def test():
    if 1:
        if 2:
            print_i(1)
        if 0:
            print_i(2)
        else:
            if 2:
                if 0:
                    print_i(3)
                elif 2:
                    print_i(4)
            else:
                print_i(4)

            if 2:
                if 0:
                    print_i(3)
                elif 2:
                    print_i(4)
            else:
                print_i(4)
    else:
        print_i(5)

    die()

arduino.send(test)

