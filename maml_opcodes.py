def i():
    n = 0
    def ii():
        nonlocal n
        n = n + 1
        return n
    return ii
i = i()

#opcodes are numbered from 1
OP_STR        = i()
OP_NUM        = i()
OP_NAME       = i()
OP_ASSIGN     = i()
OP_CALL       = i()
OP_EXPR       = i()
OP_FUNCTION   = i()
OP_RETURN     = i()
OP_PRINT      = i() #should be a function instead
OP_BINOP      = i()
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

OP_NEXT_BLOCK = i()

del i
