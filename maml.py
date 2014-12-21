#!/usr/bin/env python3

from maml_compile import compile_ast
from maml_ast import make_ast
from operator import add
from functools import reduce
from sys import argv
from maml_serial import Maml_serial
from maml_env import env
from maml_opcodes import *

#TODO: this value is also set in maml_compile.py
#      we need a global config file for all of these
allow_type_reassign = True  # enable re-declaring variable type


# maps block names to bytecode arrays and
# function names to FunctionByteCode objects
_compiled_code = {}

_blocks = {}  # maps block names to A_block objects
_functions = {}  # maps function names to A_function objects

# This references the global arduino object, there can only be one
_arduino = None

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
    def __init__(self, code, block_type):
        self.bytecode = code
        # these attributes are set when this block is sent to the Arduino
        self.block_index = None
        self.in_arduino = False
        self.block_type = block_type

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

# NOTE: the variables '_block_decorator' and '_function_decorator'
#       must match the names of the decorator functions.

class once: pass
once = once()
class chain: pass
chain = chain()

_block_decorator = 'block'
_block_decorator_types = ['once', 'chain']
def block(block_type):
    if block_type is not once and block_type is not chain:
        print("Error: block decorator type is invalid: {}".format(block_type))
        return #TODO

    def decorator(fn):
        block_name = fn.__name__
        raw = _compiled_code.get(block_name)
        block_end = OP_BLOCK_NEXT if block_type is chain else OP_BLOCK_SUICIDE
        code = [SOP_START_CODEBLOCK] + raw + [block_end, SOP_END]

        if not code:
            print("Error: could not retrieve compiled block code")
            print("_compiled_code = ", _compiled_code)

        # update (or add) this block in the global list of blocks
        codeblock = _blocks.get(block_name)
        if codeblock:
            codeblock.update_code(code)
        else:
            _blocks[block_name] = codeblock = A_block(code, block_type)

        _arduino.add_block(codeblock)

        # _arduino.send(codeblock)

        # By returning the codeblock, we bind it to the value of `block_name'
        return codeblock
    return decorator


# _block_decorator = 'block'
# def block(fn):
#     block_name = fn.__name__
#     code = _compiled_code.get(block_name)
#     if not code:
#         print("Error: could not retrieve compiled block code")
#         print("_compiled_code = ", _compiled_code)
#     # update (or add) this block in the global list of blocks
#     codeblock = _blocks.get(block_name)
#     if codeblock:
#         codeblock.update_code(code)
#     else:
#         _blocks[block_name] = codeblock = A_block(code)

#     # _arduino.send(codeblock)

#     # By returning the codeblock, we bind it to the value of `block_name'
#     return codeblock

_function_decorator = 'function'
def function(fn):
    fn_name = fn.__name__
    code = _compiled_code.get(fn_name)
    if not code:
        print("Error: could not retrieve compiled function code")
    # update (or add) this block in the global list of blocks
    codeblock = _blocks.get(fn_name)
    if codeblock:
        codeblock.update_code(code)
    else:
        _functions[fn_name] = codeblock = Function(code)

    # _arduino.send(codeblock)

    # by returning the codeblock, we bind it to the value of `block_name'
    return codeblock



class Arduino:
    def __new__(cls, *args, **kwargs):
        global _arduino
        #we only support one Arduino and have one global Arduino object
        #to represent it
        if not _arduino:
            print("making new arduino object")
            _arduino = object.__new__(cls, *args, **kwargs)
            _arduino.serial = Maml_serial()
            _arduino.env = env(None, allow_type_reassign)
        return _arduino

    def __init__(self, desktop=False):
        print ("here")
        # initialize serial
        self.serial_hook = []
        self.blocks = []
        self.desktop = desktop
        self.serial.desktop = desktop
        self.paused = False

    def set_vm_pid(self, pid):
        """set the pid of the vm processes (only used when self.desktop == True)
        Hopefully this manual step is temporary"""
        self.serial.vm_pid = pid

    def send(self, code):
        "compile and send CODE to the arduino"
        # NOTE: when sending functions is implemented the protocol
        #       for sending blocks will have to change
        if type(code) is str:
            pass  # TODO: compile and send
        elif type(code) is A_block:
            self.serial.send_codeblock(code)
        elif type(code) is A_function:
            self.serial.send_function(code)
        else:
            print("ERROR: attempt to send invalid code object")
            exit(1)

    def inject(self, code):
        """
        Evaluate CODE (once) immediately. Does not insert into main loop
        """

        pass

    def set(self, var, value):
        """
        Compile and send the code 'VAR=VALUE' to the arduino
        """
        globalp, index = self.env.get_store_index(var)
        assert globalp, "how is this not in the global env?"

        typ = type(value)
        if typ is str:
            sop_code = SOP_STR
        elif typ is int:
            sop_code = SOP_INT
        else:
            print("Error: Arduino.set currently only accepts str and int values")
            return
        self.serial.send_code([sop_code, value, SOP_INT, index, SOP_SET])

    def get(self, var):
        """
        Returns the value of variable VAR from the Arduino environment
        """
        if var in self.env.names:
            globalp, index = self.env.get_store_index(var)
            was_paused = self.paused
            if not was_paused:
                self.pause()
            self.serial.update()
            self.serial.send_code([SOP_INT, index, SOP_GET])
            ret = self.serial.update()
            if not was_paused:
                self.resume()
            if len(ret) > 0:
                return int(ret[0].strip())
            return None
        else:
            print("Error: variable '{}' does not exist in the global env"
                  .format(var))

    def dump(self):
        """
        dump values from the VMs environment
        """
        was_paused = self.paused
        if not was_paused:
            self.pause()
        was_verbose = self.serial.verbose
        self.serial.verbose = False

        print("\n_________VM DUMP___________")
        print("\n___Variables:___")
        if self.env.names:
            m = max(map(len, self.env.names)) + 2
            for v in self.env.names:
                print("{}:{}{}".format(v, " "*(m-len(v)), self.get(v)))
        else:
            print("None")

        self.serial.update()
        self.serial.send_code([SOP_DUMP_STACK])
        print("\n___Stack:___");
        for x in self.serial.update():
            x = str(x.strip())[1:]
            ind, val = x.split(" ")
            print ("{}: {}".format(ind, val))

        if not was_paused:
            self.resume()
        self.serial.verbose = was_verbose

    def pause(self):
        """
        Pause the VM.
        """
        self.serial.send_code([SOP_PAUSE])
        self.paused = True

    def resume(self):
        """
        resume the VM if paused.
        """
        if self.paused:
            self.serial.send_code([SOP_RESUME])
            self.paused = False
        else:
            print("Error: Cannot resume a non-paused VM")

    def add_block(self, block):
        self.blocks.append(block)

    # TODO overload lookup
    def add_serial_hook(self, hook):
        """
        Add function HOOK to the serial communication hook list
        when the arduino send data back to the computer, the data
        will be passed to each function in the hook list. they will
        be called in order that they where added.
        HOOK must accept a single parameter, a string
        """

        pass

    def status(self):
        """
        Returns the current status of the Arduino (running, disconnected, etc)
        """

        # what statuses can it have?
        # => disconnected, running, paused
        pass

    def _send_to_board(self, code):
        """
        Compile CODE and send to board
        """

        for c in compile(code):
            # TODO: check for errors
            self.serial.send_byte(c)

_arduino = Arduino()

def update_compiled_code(code):
    """
    Update the compiled blocks and functions in _compiled_code
    """

    # print("update_compiled_code({})'".format(filename))
    for ast in make_ast(code):
        if ast['type'] == 'function':
            decorators = ast['decorator_list']
            if len(decorators) == 1:
                decorator = decorators[0]
                if 'func' not in decorator:
                    continue
                name = decorator['func']['id']
                if name == _block_decorator:
                    args = decorator['args']
                    if len(args) != 1:
                        continue
                    if args[0]['id'] in _block_decorator_types:
                        _compiled_code[ast['name']] = compile_ast(ast['body'], desktop_p, _arduino.env)
                elif name == _function_decorator:
                    pass  # TODO


if __name__ == '__main__':
    err = False
    if len(argv) != 3:
        err = True
    else:
        target = argv[1]
        if target != "-a" and target != "-d":
            err = True
        else:
            desktop_p = (target == '-d')
        filename = argv[2]
        sp = filename.split(".")
        if len(sp) < 2 or sp[-1] != 'maml':
            err = True
    if err:
        print('Usage:')
        print('  ./maml.py -[a|d] <filename>.maml')
        exit(1)

    try:
        f = open(filename, 'r')
    except IOError:
        print("Error: where is '{}'?".format(filename))
        exit(1)


    code = f.read()
    f.close()
    modulename = reduce(add, sp[:-1])

    # first update the compiled code pieces in _compiled_code,
    # then evaluate the file
    update_compiled_code(code)

    # importing as a module does not work because variables like
    # '_compiled_code' are reset
    # __import__(modulename)
    exec(code)

    # if we exit here, it creates an error if we use the interactive (-i) flag
    # exit(0)

else:
    print("Error: invalid usage")
    print("  Instead of doing 'import maml' in NAME.py")
    print("  run it with:")
    print("    ./maml.py NAME.py")
    # TODO: is there anyway that a module can get the filename
    #       of the file that is importing it?
    #       Then we can handle this case without error
    #       and have a more intuitive interface.
    exit(1)
