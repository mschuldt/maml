arduino = Arduino(desktop=True)

@block
def test():
    a = b = c = d = e = 12
    f = g = d
    f /= 6
    h = f
    print_i(a) # 12
    print_i(b) # 12
    print_i(c) # 12
    print_i(d) # 12
    print_i(e) # 12
    print_i(f) # 2
    print_i(g) # 12
    print_i(h) # 2
    die()

arduino.send(test)
