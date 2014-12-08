arduino = Arduino(desktop=True)

@block
def test():
    x = "test string"
    print_s(x)
    die(1)
    
arduino.send(test)
