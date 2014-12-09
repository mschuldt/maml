arduino = Arduino(desktop=True)

@block
def fib():
    x = cons(10, None);
    x = cons(9, x)
    x = cons(6, cons(7, cons(8, x)))
    y = x
    while y:
        print_i(car(y))
        y = cdr(y)
    print_i(car(x))
    die(1)

arduino.send(fib)