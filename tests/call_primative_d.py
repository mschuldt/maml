arduino = Arduino(desktop=True)

@block
def test():
    print_i(5)
    print_i(5 + 6)
    print_i(1 + 2 + 3)
    quit(0)
    
arduino.send(test)
