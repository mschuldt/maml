arduino = Arduino(desktop=True)

@block
def test():
    x = -1
    print_i(x) # -1
    x *= 5
    print_i(x) # -5
    x *= -1
    print_i(x) # 5
    x *= -6
    print_i(x) # -30
    die()

arduino.send(test)
