
class Maml_serial:
    "Automatically find and maintain a connection to an Arduino over serial"
    def __init__(self, speed=9600, port=None):
        pass

    def send_bytecode(self, code):
        "send a byte over serial"
        #TODO: how to handle disconnection and other errors?
        if type(code) is int:
            self.send_byte(code)
        elif type(code) is str:
            self.send_str(code)
        else:
            print("ERROR (send_bytecode): invalid bytecode type") #TODO
    def send_byte(byte):
        pass
    def send_str(string):
        pass
