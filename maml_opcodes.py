def i():
    n = 0
    def ii():
        nonlocal n
        n = n + 1
        return n
    return ii
i = i()

#opcodes are numbered from 1

OP_NAME       = i()
OP_ASSIGN     = i()
OP_CALL       = i()
#OP_EXPR       = i()
OP_FUNCTION   = i()
OP_RETURN     = i()
#OP_BINOP      = i()
#bin ops
OP_ADD        = i()
OP_MULT       = i()
OP_SUB        = i()
OP_DIV        = i()
OP_FDIV       = i()
OP_EXPT       = i()
OP_L_XOR      = i()
OP_L_OR       = i()
OP_L_AND      = i()
OP_MOD        = i()

#opcodes for serial protocol
SOP_INT             = i()
SOP_FLOAT           = i()
SOP_STR             = i()
SOP_START_CODEBLOCK = i()
SOP_START_FUNCTION  = i()
SOP_END             = i()

OP_PRINT_INT = i() #temporary

#OP_LOAD_CONST         NOTE: this opcode is created in avm.c after bytecodes
#                             are transferred
del i
