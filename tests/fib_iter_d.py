arduino = Arduino(desktop=True)

@block
def fib():
    x = 1
    while x < 20:
        n = x
        a = 0
        b = 1
        while n:
            tmp = a + b
            a = b
            b = tmp
            n = n - 1
        print_i(a)
        x = x + 1
    die(1)    
arduino.send(fib)
