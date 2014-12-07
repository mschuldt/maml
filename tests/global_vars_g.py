arduino = Arduino(desktop=True)

@block
def test():
    a = 1
    b = 2
    c = 3
    print_i(a)
    print_i(b)
    print_i(c)
    print_i(a+b+c)
    die(0)

arduino.send(test)
