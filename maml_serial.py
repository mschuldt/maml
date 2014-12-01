
VM_PROCESS_NAME = 'avm'
#NOTE: If this are changed, their value in avm.c must be also changed.
NUM_TERMINATOR = 'x'
BYTECODE_IN_FILE = '_bc.txt'


from maml_opcodes import *
from signal import SIGIO as VM_SIGNAL
from os import kill
import subprocess
from time import sleep

class Maml_serial:
    "Automatically find and maintain a connection to an Arduino over serial"
    def __init__(self, speed=9600, port=None):
        self.speed = speed
        self.port = port
        self.desktop = False
        self.vm_pid = None

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
            if not self.vm_pid:
                self.vm_pid = find_vm_pid();
                if not self.vm_pid:
                    return False #find_vm_pid prints the error message
            #TODO: check that vm is still alive
            while True:
                f = open("{}.lock".format(self.vm_pid), 'r')
                if f.read(1) == '0':
                    break;
                print("VM is locked")
                #TODO: after some number of iterations, report failure
                f.close()
                sleep(0.2)

            self._write_to_file(bytecode)
            print("sending vm interrupt...")
            kill(self.vm_pid, VM_SIGNAL)
            return True
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

def find_vm_pid():
    p = subprocess.Popen(['ps -C '+ VM_PROCESS_NAME], stdout=subprocess.PIPE, shell=True)
    out, err = p.communicate()
    vm_pid = None
    found = 0

    for line in out.splitlines():
        pieces = str(line).split()
        if len(pieces) < 3: continue
        pid, *rest, name = pieces
        if VM_PROCESS_NAME == name.strip(" '"):
            found += 1
            vm_pid = pid

    if found == 1:
        return int(vm_pid[2:])

    if found > 1:
        print("Error: found multiple '{}' PIDs, terminate all but one vm")
    else:
        print("Error: failed to find vm pid")
    return None
