arduino = Arduino(desktop=True)

@block
def test():
    print_i(5)
    print_i(5 + 6)
    print_i(1 + 2 + 3)
    test_add2(1, 2)
    test_add3(1+1, 2, 3)
    test_add4(1+1, 2, 3, 4)
    test_add5(1+1, 2, 3, 4, 5)
    test_add6(1+1, 2, 3, 4, 5, 6)
    die(0)
    
arduino.send(test)
