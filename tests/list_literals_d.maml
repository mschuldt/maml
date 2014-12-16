arduino = Arduino(desktop=True)

@block
def test():
    x = [1,2,3,4]
    y = x
    while y:
        print_i(car(y))
        y = cdr(y)
    print_i(car(x))

    x = [test_print(11), test_print(33), test_print(33)]
    while x:
        print_i(car(x))
        x = cdr(x)
    die(1)

arduino.send(test)
