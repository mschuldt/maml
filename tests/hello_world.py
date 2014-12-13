arduino = Arduino(desktop=True)
@block
def hello_world():
    print_s("Hello, world!")
    die(1)
arduino.send(hello_world)
