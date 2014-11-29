def i():
    n = 0
    def ii():
        nonlocal n
        n = n + 1
        return n
    return ii
i = i()

#opcodes are numbered from 1
OP_STR      = i()
OP_NUM      = i()
OP_NAME     = i()
OP_ASSIGN   = i()
OP_CALL     = i()
OP_EXPR     = i()
OP_FUNCTION = i()
OP_BINOP    = i()
OP_RETURN   = i()
OP_PRINT    = i() #should be a function instead

OP_NEXT_BLOCK = i()

del i
