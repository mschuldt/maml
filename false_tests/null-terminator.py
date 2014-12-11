arduino = Arduino(desktop=True)

@block
def test():
    "x\0"
    die()

arduino.send(test)
