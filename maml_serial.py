
#NOTE: If this are changed, their value in avm.c must be also changed.
NUM_TERMINATOR = 'x'
BYTECODE_IN_FILE = '_bc.txt'

from maml_opcodes import *

class Maml_serial:
    "Automatically find and maintain a connection to an Arduino over serial"
    def __init__(self, speed=9600, port=None):
        self.speed = speed
        self.port = port
        self.desktop = False


    def send_codeblock(self, block):
        "send BLOCK to vm on arduino or desktop"
        #TODO: how to handle disconnection and other errors?
        bc = block.bytecode
        length = len(bc)
        exp = expand_bytecode(bc);
        exp = list(str(length+1)) + [NUM_TERMINATOR, chr(SOP_START_CODEBLOCK)] +exp
        self._send(exp + [chr(SOP_END), chr(SOP_END)])#end block and end file

    def send_function(self, fn):
        #TODO
        self._send(expanded)

    def _send(self, bytecode):
        "send fully expanded BYTECODE"
        if self.desktop:
            self._write_to_file(bytecode)
            #TODO: signal vm with SIGIO
        else:
            pass #TODO: send to arduino over serial

    def _write_to_file(self, bytecode):
        "write BYTECODE to file"
        f = open(BYTECODE_IN_FILE, 'w')
        for c in bytecode:
            f.write(str(c)+'\n');
        print("wrote file '{}'".format(BYTECODE_IN_FILE))
        f.close()

def expand_bytecode(bc):
    "expands bc into an array of bytes"
    long_code = []
    i=0;
    length = len(bc)
    while i < length:
        c = bc[i]
        long_code.append(chr(c))
        if c == SOP_INT:
            i += 1
            long_code.extend(list(str(bc[i])) + [NUM_TERMINATOR])
        elif c == SOP_STR:
            i += 1
            long_code.extend(list(bc[i]) + [0])
        i += 1
    return long_code
