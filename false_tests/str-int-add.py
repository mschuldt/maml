arduino = Arduino(desktop=True)

@block
def test():
    "x" + 1
    die()

arduino.send(test)
