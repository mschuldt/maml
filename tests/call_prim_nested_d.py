arduino = Arduino(desktop=True)

@block
def func():
    print_i(test_add(test_add(1,2), test_add(3,4)))
    print_i(test_add(test_add(test_add(test_add(1,2), test_add(3,4)),2),
                     test_add(test_add(test_add(1,2), test_add(3,4)),4)))
    print_i(test_add(test_add(test_add(test_add(test_add(test_add(test_add(test_add(1,2), test_add(3,4)),2),
                                                         test_add(test_add(test_add(1,2), test_add(3,4)),4)),2), test_add(3,4)),2),
                     test_add(test_add(test_add(1,2), test_add(3,4)),4)))
    die(0)
    
arduino.send(func)