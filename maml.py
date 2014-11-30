#!/usr/bin/env python3

from maml_compile import compile_ast
from maml_ast import make_ast
from operator import add
from functools import reduce
from sys import argv
from maml_serial import Maml_serial

#maps block names to bytecode arrays and
#function names to FunctionByteCode objects
_compiled_code = {}

_blocks = {} #maps block names to A_block objects
_functions = {} #maps function names to A_function objects

_arduino = None #this references the global arduino object, there can only be one

# Note:
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
        self.bytecode = code
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

#NOTE: the variables '_block_decorator' and '_function_decorator'
#      must match the names of the decorator functions.

_block_decorator = 'block'
def block(fn):
    block_name = fn.__name__
    code = _compiled_code.get(block_name)
    if not code:
        print("Error: could not retrieve compiled block code")
        print("_compiled_code = ", _compiled_code)
    #update (or add) this block in the global list of blocks
    codeblock = _blocks.get(block_name)
    if codeblock:
        codeblock.update_code(code)
    else:
        _blocks[block_name] = codeblock = A_block(code)

    _arduino.send(codeblock)

    #by returning the codeblock, we bind it to the value of `block_name'
    return codeblock

_function_decorator = 'function'
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
    def __init__(self, desktop_p):
        global _arduino
        if _arduino:
            print("WARNING: multiple Arduino boards are not supported")
            return self
        _arduino = self

        #initialize serial
        self.serial_hook = []
        self.serial = Maml_serial()
        self.desktop = desktop_p
        self.serial.desktop = desktop_p

    def send(self, code):
        "compile and send CODE to the arduino"
        #NOTE: when sending functions is implemented the protocol
        #      for sending blocks will have to change
        if type(code) is str:
            pass #TODO: compile and send
        elif type(code) is A_block:
            self.serial.send_codeblock(code)
        elif type(code) is A_function:
            self.serial.send_function(code)
        else:
            print("ERROR: attempt to send invalid code object")
            exit(1)


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

def update_compiled_code(code):
    "update the compiled blocks and functions in _compiled_code"
    print("update_compiled_code({})'".format(filename))

    for ast in make_ast(code):
        if ast['type'] == 'function':
            decorators = ast['decorator_list']
            if len(decorators) == 1:
                name = decorators[0]['id']
                if name == _block_decorator:
                    print("compiling block '{}'".format(ast['name']))
                    _compiled_code[ast['name']] = compile_ast(ast['body'])
                elif name == _function_decorator:
                    pass #TODO


if __name__ == '__main__':
    err = False
    if len(argv) != 2:
        err = True
    filename = argv[1]
    sp = filename.split(".")
    if len(sp) < 2 or sp[-1] != 'py':
        err = True
    if err:
        print('Usage:')
        print('  ./maml.py <filename>.py')
        exit(1)

    try:
        f = open(filename, 'r')
    except IOError:
        print("Error: where is '{}'?".format(filename))
        exit(1)

    code = f.read()
    f.close()
    modulename = reduce(add, sp[:-1])

    #first update the compiled code pieces in _compiled_code,
    #then evaluate the file
    update_compiled_code(code)
    print("::_compiled_code = ", _compiled_code)
    
    # importing as a module does not work because variables like
    # '_compiled_code' are reset
    #__import__(modulename)
    exec(code)

    exit(0)
    
else:
    print("Error: invalid usage")
    print("  Instead of doing 'import maml' in NAME.py")
    print("  run it with:")
    print("    ./maml.py NAME.py")
    #TODO: is there anyway that a module can get the filename
    #      of the file that is importing it?
    #      Then we can handle this case without error
    #      and have a more intuitive interface.
    exit(1)
