#!/usr/bin/env python3

# To re-generate the C opcode file, run:
#    ./maml_opcodes.py

C_OPCODE_FILE = "_opcodes.h"
C_ENTRYTABLE_FILE = "_entrytable.h"
C_PRIM_CALL_MAX_ARGS = 6 #maximum number of arguments to a primitive function

# if True, use strings instead of numbers for opcode values
debug = False
# example:
#  when False: [22, 2, 9, 0]
#  when True:  ['SOP_INT', 2, 'OP_GLOBAL_STORE', 0]

###############################################################################

opcodes = []
vm_cases = []
i = 0

def OP(name, vm_case=True):
    # if VM_CASE is False, NAME will not have an entry in _entrytable.c
    global i
    i = i + 1
    opcodes.append((name, i))
    if vm_case:
        vm_cases.append((name, i))
    if debug:
        return name
    return i


# opcodes are numbered from 1
# all opcodes must have a corresponding (lower-case) label in avm.c
# codes that do have have a vm case  must pass False as a second arg to OP
# (some opcodes that are not implemented yet are also marked False)

#OP_NAME = OP("OP_NAME")
#OP_ASSIGN = OP("OP_ASSIGN")
#OP_PRIM_CALL = OP("OP_PRIM_CALL")
OP_CALL = OP("OP_CALL")
#OP_FUNCTION = OP("OP_FUNCTION")
OP_RETURN = OP("OP_RETURN")
OP_POP = OP("OP_POP")
OP_GLOBAL_LOAD = OP("OP_GLOBAL_LOAD")
OP_GLOBAL_STORE = OP("OP_GLOBAL_STORE")
OP_LOCAL_LOAD = OP("OP_LOCAL_LOAD")
OP_LOCAL_STORE = OP("OP_LOCAL_STORE")
OP_IF = OP("OP_IF")
OP_JUMP = OP("OP_JUMP")
OP_LIST = OP("OP_LIST")
OP_ARRAY = OP("OP_ARRAY")
OP_LIST_LOAD = OP("OP_LIST_LOAD", False)
OP_LIST_STORE = OP("OP_LIST_STORE", False)
OP_CONST = OP("OP_CONST")

OP_NEXT_BLOCK = OP("OP_NEXT_BLOCK")#?
OP_BLOCK_SUICIDE = OP("OP_BLOCK_SUICIDE")


# bin ops
OP_ADD = OP("OP_ADD")
OP_MULT = OP("OP_MULT")
OP_SUB = OP("OP_SUB")
OP_DIV = OP("OP_DIV")
OP_FDIV = OP("OP_FDIV", False)
OP_EXPT = OP("OP_EXPT", False)
OP_L_XOR = OP("OP_L_XOR", False)
OP_L_OR = OP("OP_L_OR", False)
OP_L_AND = OP("OP_L_AND", False)
OP_MOD = OP("OP_MOD", False)

# comparison ops
OP_GT = OP("OP_GT")
OP_LT = OP("OP_LT")
OP_EQ = OP("OP_EQ")
OP_NOT_EQ = OP("OP_NOT_EQ")
OP_LT_EQ = OP("OP_LT_EQ")
OP_GT_EQ = OP("OP_GT_EQ")
OP_IN = OP("OP_IN", False)
OP_NOT_IN = OP("OP_NOT_IN", False)
OP_IS = OP("OP_IS", False)

# codes for serial protocol
SOP_PING = OP("SOP_PING", False)
SOP_ALIVE = OP("SOP_ALIVE", False)
SOP_INT = OP("SOP_INT", False)
SOP_FLOAT = OP("SOP_FLOAT", False)
SOP_STR = OP("SOP_STR", False)
SOP_ARRAY = OP("SOP_ARRAY", False)  # void*
SOP_INT_ARRAY = OP("SOP_INT_ARRAY", False)
SOP_START_CODEBLOCK = OP("SOP_START_CODEBLOCK", False)
SOP_START_FUNCTION = OP("SOP_START_FUNCTION", False)
SOP_PRIM_CALL = OP("SOP_PRIM_CALL", False)
SOP_LABEL = OP("SOP_LABEL", False)
SOP_NULL = OP("SOP_NULL", False)
SOP_END = OP("SOP_END", False)
SOP_PAUSE = OP("SOP_PAUSE", False)
SOP_RESUME = OP("SOP_RESUME", False)
SOP_DUMP_STACK = OP("SOP_DUMP_STACK", False)
SOP_SET = OP("SOP_SET", False)
SOP_GET = OP("SOP_GET", False)

def print_opcodes():
    max_len = max(map(lambda x: len(x[0]), opcodes))
    for name, num in opcodes:
        print(name, " "*(max_len - len(name)) + str(num) + "  " + chr(num))


if __name__ == '__main__':
    from sys import argv
    if len(argv) == 2 and argv[1] == '-print':
        print_opcodes();
        exit(0)
    assert not debug, "disable debug mode when not debugging"

    print("re-generating C opcode file...", end="")
    f = open(C_OPCODE_FILE, "w")
    guard = C_OPCODE_FILE.upper().replace('.', '_')
    f.write("/***** This file is auto-generated, do not modify. *****/\n\n")
    f.write("#ifndef {}\n#define {}\n\n".format(guard, guard))
    for name, num in opcodes:
        f.write("#define {} {}\n".format(name, num))
    f.write("\n#endif\n")
    f.close()
    print("done")

    print("re-generating C entrytable file...", end="")
    f = open(C_ENTRYTABLE_FILE, "w")
    guard = C_ENTRYTABLE_FILE.upper().replace('.', '_')
    f.write("/***** This file is auto-generated, do not modify. *****/\n\n")
    f.write("#ifndef {}\n#define {}\n\n".format(guard, guard))
    f.write("entry_table = (void**)malloc(sizeof(void*)*{});\n"
            #.format(len(vm_cases))) #can't do this because we skip some cases
            .format(max(map(lambda x: x[1], vm_cases))+1))
    f.write("prim_call_entry_table = (void**)malloc(sizeof(void*)*{});\n"
            .format(C_PRIM_CALL_MAX_ARGS+1))
    for name, num in vm_cases:
        f.write("entry_table[{}] = &&{};\n".format(num, name.lower()))
    for i in range(C_PRIM_CALL_MAX_ARGS+1):
        f.write("prim_call_entry_table[{}] = &&call_prim_{};\n".format(i,i))
    f.write("\n#endif\n")
    f.close()
    print("done")

    exit(0)
else:
    del i
    del OP
