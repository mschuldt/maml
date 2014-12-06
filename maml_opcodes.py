#!/usr/bin/env python3

# To re-generate the C opcode file, run:
#    ./maml_opcodes.py

C_OPCODE_FILE = "_opcodes.h"
################################################################################

opcodes = []
i = 0
def OP(name):
    global i
    i = i + 1
    opcodes.append((name, i))
    return i

#opcodes are numbered from 1

OP_NAME             = OP("OP_NAME")
OP_ASSIGN           = OP("OP_ASSIGN")
OP_CALL             = OP("OP_CALL")
OP_FUNCTION         = OP("OP_FUNCTION")
OP_RETURN           = OP("OP_RETURN")
#bin ops
OP_ADD              = OP("OP_ADD")
OP_MULT             = OP("OP_MULT")
OP_SUB              = OP("OP_SUB")
OP_DIV              = OP("OP_DIV")
OP_FDIV             = OP("OP_FDIV")
OP_EXPT             = OP("OP_EXPT")
OP_L_XOR            = OP("OP_L_XOR")
OP_L_OR             = OP("OP_L_OR")
OP_L_AND            = OP("OP_L_AND")
OP_MOD              = OP("OP_MOD")

#codes for serial protocol
SOP_INT             = OP("SOP_INT")
SOP_FLOAT           = OP("SOP_FLOAT")
SOP_STR             = OP("SOP_STR")
SOP_START_CODEBLOCK = OP("SOP_START_CODEBLOCK")
SOP_START_FUNC      = OP("SOP_START_FUNCTION")
SOP_END             = OP("SOP_END")

OP_PRINT_INT        = OP("OP_PRINT_INT")


OP("OP_LOAD_CONST") #this opcode exists only in avm.c


if __name__ == '__main__':
    print("re-generating C opcode file...", end="")
    f = open(C_OPCODE_FILE, "w")
    guard = C_OPCODE_FILE.upper().replace('.','_')
    f.write("/***** This file is auto-generated, do not modify. *****/\n\n")
    f.write("#ifndef {}\n#define {}\n\n".format(guard, guard))
    for name, num in opcodes:
        f.write("#define {} {}\n".format(name, num))
    ### special cases

    ###
    f.write("\n#endif\n")
    f.close()
    print("done")
else:
    del i
    del OP
    del opcodes
