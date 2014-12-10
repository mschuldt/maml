
VM_PROCESS_NAME = 'avm'
# NOTE: If this are changed, their value in avm.c must be also changed.
NUM_TERMINATOR = 'x'
BYTECODE_IN_FILE = '_bc.txt'

from maml_opcodes import *
if debug:
    print("Error serializing: in maml_opcodes.py, debug must be False")
    exit(1)
from signal import SIGIO as VM_SIGNAL
from os import kill
import subprocess
from time import sleep
import platform
from glob import glob
import serial


class Maml_serial:
    """
    Automatically find and maintain a connection to an Arduino over serial
    """

    def __init__(self, speed=9600, port=None):
        self.speed = speed
        self.port = port
        self.desktop = False
        self.vm_pid = None

    def send_codeblock(self, block):
        """
        Send BLOCK to vm on arduino or desktop
        """

        # TODO: how to handle disconnection and other errors?
        bc = block.bytecode
        length = len(bc)
        exp = expand_bytecode(bc)
        exp += list(str(length+1)) + [NUM_TERMINATOR, chr(SOP_START_CODEBLOCK)]
        # end block and end file
        self._send(exp + [chr(SOP_END), chr(SOP_END)])

    def send_function(self, fn):
        # TODO
        self._send(expanded)

    def _send(self, bytecode):
        """
        Send fully expanded BYTECODE
        """

        if self.desktop:
            if not self.vm_pid:
                self.vm_pid = find_vm_pid()
                if not self.vm_pid:
                    return False  # find_vm_pid prints the error message
            # TODO: check that vm is still alive
            while True:
                f = open("{}.lock".format(self.vm_pid), 'r')
                if f.read(1) == '0':
                    break
                print("VM is locked")
                # TODO: after some number of iterations, report failure
                f.close()
                sleep(0.2)

            self._write_to_file(bytecode)
            print("sending vm interrupt...")
            kill(self.vm_pid, VM_SIGNAL)
            return True
        else:
            pass  # TODO: send to arduino over serial

    def _write_to_file(self, bytecode):
        "write BYTECODE to file"
        f = open(BYTECODE_IN_FILE, 'w')
        for c in bytecode:
            f.write(str(c)+'\n')
        print("wrote file '{}'".format(BYTECODE_IN_FILE))
        f.close()

    def find_arduino_port():
        def ping(s):
            s.write(bytes(chr(SOP_PING), 'UTF-8'))
            n = s.read()
            if n:
                return int(n) == SOP_ALIVE

        for port in list_serial_ports():
            print("trying port: {}...".format(port), end="")
            try:
                s = serial.Serial(port, 9600)
                s.timeout = 0.1
                if ping(s):
                    print("yes")
                    return port
                print("no")
            except serial.SerialException:
                print("no")
            return True


def expand_bytecode(bc):
    "expands bc into an array of bytes"
    long_code = []
    i = 0
    length = len(bc)

    def expand_int(n):
        # TODO: better protocol for sending numbers, this is pretty dumb
        return list(str(n)) + [NUM_TERMINATOR]

    while i < length:
        c = bc[i]
        long_code.append(chr(c))
        if c == SOP_INT:
            i += 1
            long_code.extend(expand_int(bc[i]))
        elif c == SOP_STR:
            i += 1
            s = bc[i]
            long_code.extend(expand_int(len(s)) + list(s) + [chr(0)])
        elif c == SOP_ARRAY or c == SOP_INT_ARRAY:
            i += 1
            s = bc[i]
            # Types: 0 -> int
            #        1 -> void*
            long_code.extend(expand_int(len(s)) + expand_int(c == SOP_ARRAY) +
                             s)

        i += 1
    return long_code


def find_vm_pid():
    p = subprocess.Popen(['ps -C ' + VM_PROCESS_NAME], stdout=subprocess.PIPE,
                         shell=True)
    out, err = p.communicate()
    vm_pid = None
    found = 0

    for line in out.splitlines():
        pieces = str(line).strip("'b").split()
        if len(pieces) < 3:
            continue
        pid, *rest, name = pieces
        if VM_PROCESS_NAME == name.strip(" '"):
            found += 1
            vm_pid = pid

    if found == 1:
        return int(vm_pid)

    if found > 1:
        print("Error: found multiple '{}' PIDs, terminate all but one vm"
              .format(VM_PROCESS_NAME))
    else:
        print("Error: failed to find vm pid")
    return None


def list_serial_ports():
    # Adapted from http://stackoverflow.com/questions/11303850/what-is-the-cross-platform-method-of-enumerating-serial-ports-in-python-includi
    system_name = platform.system()
    assert system_name != "Windows", "gross OS error"
    if system_name == "Windows":
        # Scan for available ports.
        available = []
        for i in range(256):
            try:
                s = serial.Serial(i)
                available.append(i)
                s.close()
            except serial.SerialException:
                pass
        return available
    elif system_name == "Darwin":  # Mac
        return glob.glob('/dev/tty*') + glob.glob('/dev/cu*')
    elif system_name == "Linux":
        # + glob('/dev/ttyS*')
        return glob('/dev/ttyUSB*') + glob('/dev/ttyACM*')
    else:
        print("Error: unknown system: " + system_name)
        exit(1)
