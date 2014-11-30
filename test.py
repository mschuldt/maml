
print("loading test.py")

arduino = Arduino(True)

@block
def test_block():
    1 + 3
    88 + 77

@block
def another_block():
    5 + 9

#arduino.send(test_block)
#arduino.send(another_block)

print("done loading test.py")
