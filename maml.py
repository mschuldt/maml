#!/usr/bin/env python3

from maml_compile import compile
from operator import add
from sys import argv


#maps block names to bytecode arrays and
#function names to FunctionByteCode objects
_compiled_code = {}

#TODO: compile ASTs to bytecode here and insert into `compiled_code' here

_blocks = {} #maps block names to A_block objects
_functions = {} #maps function names to A_function objects

_arduino = None #this references the global arduino object, there can only be one

# Functions decorated with @block will have its body compiled
# to bytecode and placed in the '_compiled_code' dictionary.
# when the function is evaluated, An 'A_Block' object that
# represents this block on the Arduino will be created and
# placed in '_blocks' 
#
# Functions decorated with @function will be compiled
# and FunctionByteCode object that references its bytecode
# will be placed in the '_compiled_code' dictionary.
# When the function is evaluated, An 'A_function' object that
# represents this function on the Arduino will be created
# and placed in '_functions'
# 
# when a function or codeblock is re-evaluated it checks
# if the corresponding compiled code in _blocks or
# _functions is has changed, if it has, the updated bytecode
# is sent to the Arduino

class FunctionByteCode():
    def __init__(self, bytecode, args, stacksize, nlocals):
        self.bytecode = bytecode
        self.args = args
        self.stacksize = stacksize
        self.nlocals = nlocals
    
class A_block:
    def __init__(self, code):
        self.bytecode = code.bytecode
        #these attributes are set when this block is sent to the Arduino
        self.block_index = None
        self.in_arduino = False

    def update_code(self, code):
        if self.bytecode != code:
            self.bytecode = code
            if self.in_arduino:
                _arduino.update(self)
    def __hash__(self):
        return hash([self.block_index, self.in_arduino] + self.bytecode)
        
class A_function:
    def __init__(self, bytecode_obj):
        pass
    
    
def block(fn):
    block_name = fn.__name__
    code = _compiled_code.get(block_name)
    if not code:
        print("Error: could not retrieve compiled block code")
    #update (or add) this block in the global list of blocks
    codeblock = _blocks.get(block_name)
    if codeblock:
        codeblock.update_code(code)
    else:
        _blocks[block_name] = codeblock = CodeBlock(code)

    _arduino.send(codeblock)
    
    #by returning the codeblock, we bind it to the value of `block_name'
    return codeblock

def function(fn):
    fn_name = fn.__name__
    code = _compiled_code.get(fn_name)
    if not code:
        print("Error: could not retrieve compiled function code")
    #update (or add) this block in the global list of blocks
    codeblock = _blocks.get(fn_name)
    if codeblock:
        codeblock.update_code(code)
    else:
        _functions[fn_name] = codeblock = Function(code)

    #automatically send function?
    # doing so is inconsistent - codeblocks are not automatically sent
    _arduino.send(codeblock)
    
    #by returning the codeblock, we bind it to the value of `block_name'
    return codeblock        
    
class Arduino:
    def __init__(self):
        global _arduino
        if _arduino:
            print("WARNING: multiple Arduino boards are not supported")
            return self
        _arduino = self

        #initialize serial
        self.serial_hook = []
        self.serial = Maml_serial()
        pass
    def send(self,code):
        "compile and send CODE to the arduino"
        pass
    def inject(self, code):
        "evaluate CODE (once) immediately. Does not insert into main loop"
        pass
    def set(self, var, value):
        "compile and send the code 'VAR=VALUE' to the arduino"
        pass
    def get(self, var):
        "returns the value of variable VAR from the Arduino environment"
        pass
    #TODO overload lookup
    def add_serial_hook(self, hook):
        """add function HOOK to the serial communication hook list
        when the arduino send data back to the computer, the data
        will be passed to each function in the hook list. they will
        be called in order that they where added.
        HOOK must accept a single parameter, a string """
        pass
    def status(self):
        "returns the current status of the Arduino (running, disconnected, etc) "
        #what statuses can it have?
        pass
    def _send_to_board(self, code):
        "compile CODE and send to board"
        for c in compile(code):
            #TODO: check for errors
            self.serial.send_byte(c)
        
    
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
        
if __name__ == '__main__':
    if len(argv) != 2:
        print('Usage:')
        print('  ./maml.py <filename>.py')
        exit(1)
    filename = argv[1]
    try:
        f = open(filename, 'r')
    except IOError:
        print("Error: where is '{}'?".format(filename))
        exit(1)
    print(compile(f.read()))
    exit(0)
