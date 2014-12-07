arduino = Arduino(desktop=True)

@block
def test():
    test_print_args2(test_print(1),test_print(2))
    test_print_args3(test_print(1),test_print(2),test_print(3))
    test_print_args4(test_print(1),test_print(2),test_print(3),test_print(4))
    test_print_args5(test_print(1),test_print(2),test_print(3),test_print(4),test_print(5))
    test_print_args6(test_print(1),test_print(2),test_print(3),test_print(4),test_print(5),test_print(6))
    die(0)
    
arduino.send(test)
