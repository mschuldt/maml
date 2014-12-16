arduino = Arduino(desktop=True)

@block
def test():
    x = 1
    print_i(x) # 1
    x *= 5
    print_i(x) # 5
    x /= -1
    print_i(x) # -5
    x -= -6
    print_i(x) # 1
    y = -1 - -3
    print_i(y) # 2
    print_i(-1 - -3 >= 18) # 0
    print_i(-1 - -3 <= 18) # 1
    die()

arduino.send(test)
