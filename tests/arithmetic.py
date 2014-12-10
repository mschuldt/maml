arduino = Arduino(desktop=True)

@block
def test():
    # True assignments
    print_i(1+1 == 2) # 1
    print_i(1+1 != 0) # 1
    print_i(1+1 <= 2) # 1
    print_i(1+1 <= 3) # 1
    print_i(1+1 < 3)  # 1
    print_i(1+1 >= 2) # 1
    print_i(1+1 > 1)  # 1
    # False assignments
    print_i(1+1 != 2) # 0
    print_i(1+1 == 0) # 0
    print_i(1+1 >= 3) # 0
    print_i(1+1 > 3)  # 0
    print_i(1+1 <= 1) # 0
    print_i(1+1 < 1)  # 0
    print_i("s" == "s")
    # Arithmetic equality tests
    print_i(1 - 1) # 0
    print_i(2 - 1) # 1
    print_i(1 - 2) # -1
    print_i(4 * 6) # 24
    print_i(6 * 4) # 24
    print_i(4 * 0) # 0
    print_i(0 * 4) # 0
    print_i(6 / 3) # 2
    print_i(6 / 4) # 1
    print_i(0 / 4) # 0

    print_i(-1 + 2) # 1
    die()

arduino.send(test)
