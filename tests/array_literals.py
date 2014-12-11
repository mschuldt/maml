arduino = Arduino(desktop=True)

@block
def test():
    x = (1,2,3,4,5,6,7,8)
    print_a(x)
    die(1)

arduino.send(test)
