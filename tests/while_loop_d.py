arduino = Arduino(desktop=True)

@block
def test():
    x = 0
    while x < 10:
        x = x + 1
        print_i(x)

    n = 0
    while n < 30:
        x = 0
        while x < 3:
            print_i(n+x)
            x = x + 1
        n = n + 10
    die()
    
arduino.send(test)
