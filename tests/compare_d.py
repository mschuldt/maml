arduino = Arduino(desktop=True)

@block
def test():
    x = 0
    print_i(x < 3)
    print_i(2 < x)
    print_i(1 < 2)
    print_i(2 < 1)
    die()

arduino.send(test)