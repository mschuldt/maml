arduino = Arduino(desktop=True)

@block
def test():
    if 1: print_i(111)
    else: print_i(222)
        
    if 0: print_i(111)
    else: print_i(222)

    false = 0
    true = 1
    
    if true: print_i(true) 
    else: print_i(false)
    
    if false: print_i(true)
    else: print_i(false)
    
    if true:
        print_i(3)
    else:
        print_i(5)

    if true:
        print_i(300)
        
    if false:
        print_i(300)
        
    if false:
        print_i(500)
    elif true:
        print_i(100)

    if false:
        pass
    elif false:
        pass
    elif false:
        pass
    elif true:
        print_i(888)
    else:
        print_i(11111)
        
    if 1+1+1:
        print_i(91)
        
    if test_add(1, 2):
        print_i(92)

    if test_print(0):
        print_i(93)

    if test_print(11):
        print_i(94)

    if test_print(test_print(12)+1):
        print_i(95)
        
    die()

arduino.send(test)
