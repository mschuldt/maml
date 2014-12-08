#!/usr/bin/env python3

# To re-generate the C opcode file, run:
#    ./maml_opcodes.py

C_OPCODE_FILE = "_opcodes.h"

#if True, use strings instead of numbers for opcode values
debug = False
#example:
#  when False: [22, 2, 9, 0]
#  when True:  ['SOP_INT', 2, 'OP_GLOBAL_STORE', 0]

################################################################################

opcodes = []
i = 0
def OP(name):
    global i
    i = i + 1
    opcodes.append((name, i))
    if debug:
        return name
    return i


#opcodes are numbered from 1

OP_NAME             = OP("OP_NAME")
OP_ASSIGN           = OP("OP_ASSIGN")
OP_PRIM_CALL        = OP("OP_PRIM_CALL")
OP_USER_CALL        = OP("OP_USER_CALL")
OP_FUNCTION         = OP("OP_FUNCTION")
OP_RETURN           = OP("OP_RETURN")
OP_POP              = OP("OP_POP")
OP_GLOBAL_LOAD      = OP("OP_GLOBAL_LOAD")
OP_GLOBAL_STORE     = OP("OP_GLOBAL_STORE")
OP_LOCAL_LOAD       = OP("OP_LOCAL_LOAD")
OP_LOCAL_STORE      = OP("OP_LOCAL_STORE")
OP_IF               = OP("OP_IF")
OP_JUMP             = OP("OP_JUMP")
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
#comparison ops
OP_GT               = OP("OP_GT")
OP_LT               = OP("OP_LT")
OP_EQ               = OP("OP_EQ")
OP_NOT_EQ           = OP("OP_NOT_EQ")
OP_LT_EQ            = OP("OP_LT_EQ")
OP_GT_EQ            = OP("OP_GT_EQ")
OP_IN               = OP("OP_IN")
OP_NOT_IN           = OP("OP_NOT_IN")
OP_IS               = OP("OP_IS")

#codes for serial protocol
SOP_INT             = OP("SOP_INT")
SOP_FLOAT           = OP("SOP_FLOAT")
SOP_STR             = OP("SOP_STR")
SOP_START_CODEBLOCK = OP("SOP_START_CODEBLOCK")
SOP_START_FUNC      = OP("SOP_START_FUNCTION")
SOP_PRIM_CALL       = OP("SOP_PRIM_CALL")
SOP_LABEL           = OP("SOP_LABEL")
SOP_NULL            = OP("SOP_NULL")
SOP_END             = OP("SOP_END")


OP("OP_LOAD_CONST") #this opcode exists only in avm.c

if __name__ == '__main__':
    assert not debug, "disable debug mode when not debugging"
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

