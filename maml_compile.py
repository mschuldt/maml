#!/usr/bin/env python3

#TODO: need to calculate the max stack size used by a block of code
#     and have avm.c adjust the stacksize as needed ast start of
#     block execution.

verbose = True
compile_decorator = 'arduino'

from maml_ast import make_ast
from maml_opcodes import *
from maml_env import env
from functools import reduce
from operator import add
from sys import argv
from _prim import primitives

# for the AST node with type X:
#   * node['type'] == 'X'
#   * The code generation function is defined as
#        @node('X')
#        def _(ast, btc, env, top):
#           ...
#   * The ast node checking function is defined as
#        @check('X')
#        def _(ast):
#           ...
#   * The corresponding opcode is OP_X
#   * maml_ast.py defines a function X (capitalized)
#     that returns the AST node in dictionary format.
#     Functions in maml_ast.py that are not capitalized
#     generate AST nodes whose bytecode is generated
#     by another nodes generation function
#     (ex: 'function' node consumes the 'arguments' node,
#          so there is no @node('arguments') function)

_bytecode_switch_table = {}
_ast_check_switch_table = {}

def node(name):
    def decorator(fn):
        _bytecode_switch_table[name] = fn
    return decorator

def check(name):
    def decorator(fn):
        _ast_check_switch_table[name] = fn
    return decorator


################################################################################
# bytecode generation

# @node function parameters:
#   AST is the ast node
#   BTC is the bytecode array
#   ENV keeps track of variable index mappings and types
#   TOP is this a top level node?

################################################################################
# str

@node('str')
def _(ast, btc, env, top):
    btc.extend([SOP_STR, ast['s']])

################################################################################
# int
@node('int')
def _(ast, btc, env, top):
    btc.extend([SOP_INT, ast['n']])

################################################################################
# float
@node('float')
def _(ast, btc, env, top):
    not_implemented_error(ast)

################################################################################
# list
@node('list')
def _(ast, btc, env, top):
    elts = ast['elts']
    for e in elts:
        gen_bytecode(e, btc, env, False)
    print("compiling 'list', len(elts) = {}".format(len(elts)))
    btc.extend([OP_LIST, SOP_INT, len(elts)])
    #TODO: if len = 0 ==> NULL

################################################################################
# str
@node('tuple')
def _(ast, btc, env, top):
    pass

################################################################################
# str
#TODO: 'name' and 'assign' still need to be tested with local names
@node('name')
def _(ast, btc, env, top):
    name = ast['id']
    if name == 'None' or name == 'False':
        btc.append(SOP_NULL)
    else:
        globalp, index = env.get_load_index(name)
        op = OP_GLOBAL_LOAD if globalp else OP_LOCAL_LOAD
        btc.extend([op, SOP_INT, index])

################################################################################
# nameconstant
@node('nameconstant')
def _(ast, btc, env, top):
    if ast['value']:
        btc.extend([SOP_INT, 1])
    else:
        btc.append(SOP_NULL)

################################################################################
# assign

@node('assign')
def _(ast, btc, env, top):
    #TODO: check that target is declared

    target = ast['targets'][0] #no support for unpacking
    #value = gen_bytecode(ast['value'])
    gen_bytecode(ast['value'], btc, env, False)
    globalp, index = env.get_store_index(target['id'])
    op = OP_GLOBAL_STORE if globalp else OP_LOCAL_STORE
    btc.extend([op, SOP_INT, index])

@check('assign')
def _(ast):
    assert_type(ast, "assign")
    targets = ast['targets']
    if len(targets) > 1:
        #example: 'a,b=x'
        syntax_error(ast, "unpacking is not supported")
        return False
    if targets[0]['type'] == 'starred':
        #example: '*a=x'
        syntax_error(ast, "starred assignment is not supported")
        return False
    return True

################################################################################
# expr
#TODO: eliminate this type in maml_ast.py ?
@node('expr')
def _(ast, btc, env, top):
    #if not check_expr(ast): return
    gen_bytecode(ast['value'], btc, env, top)

################################################################################
# binop

@node('binop')
def _(ast, btc, env, top):
    gen_bytecode(ast['left'], btc, env, False);
    gen_bytecode(ast['right'], btc, env, False);
    btc.append(bin_ops[ast['op']])

@check('binop')
def _(ast):
    assert_type(ast, "binop")
    if ast['op'] in bin_ops:
        return True
    #check 'left' and 'right' properties
    return False

################################################################################
# call

@node('call')
def _(ast, btc, env, top):
    """bytecode format:
    arg1 arg2 ... argn OP_PRIM_CALL function_pointer"
serial format:
    SOP_PRIM  num_arguments func_index
where:
func_index = index of function_pointer in the array 'primitives'"""
    nargs = len(ast['args'])
    for arg in ast['args']:
        gen_bytecode(arg, btc, env, False)
    index = primitives.get(ast['func']['id'], None)
    if index is not None: # calling a primative
        #we have to use SOP_INT here so that the bytecode expansion
        #can expand the numbers
        btc.extend([SOP_PRIM_CALL, SOP_INT, nargs, SOP_INT, index])
        if top:
            btc.append(OP_POP);
    else: #calling a user defined function
        print("Error -- not implemented: calling non-primitives ('{}')"
              .format(ast['func']['id']));
        exit(1)

@check('call')
def _(ast):
    if ast['keywords']:
        syntax_error(ast, "keyword args are not supported")
    if ast['starargs']:
        syntax_error(ast, "starargs are not supported")
    if ast['kwargs']:
        syntax_error(ast, "kwargs args are not supported")

################################################################################
# if

@node('if')
def _(ast, btc, env, top):
    false_l = env.make_label() #marks beginning of false code
    done_l = env.make_label() #marks end of false code
    gen_bytecode(ast['test'], btc, env, False)
    btc.extend([OP_IF, OP_JUMP, SOP_INT, false_l])
    for node in ast['body']:
        gen_bytecode(node, btc, env, top);
    btc.extend([OP_JUMP, SOP_INT, done_l, SOP_LABEL, SOP_INT, false_l])
    for node in ast['else']:
        gen_bytecode(node, btc, env, top);
    btc.extend([SOP_LABEL, SOP_INT, done_l])

################################################################################
# while

@node('while')
def _(ast, btc, env, top):
    # [start] <test> OP_IF <jump end> <body> <jump start> [end]
    start_l = env.make_label()
    end_l = env.make_label()
    btc.extend([SOP_LABEL, SOP_INT, start_l])
    gen_bytecode(ast['test'], btc, env, False)
    btc.extend([OP_IF, OP_JUMP, SOP_INT, end_l])
    for node in ast['body']:
        gen_bytecode(node, btc, env, top);
    btc.extend([OP_JUMP, SOP_INT, start_l, SOP_LABEL, SOP_INT, end_l])

@check('while')
def _(ast):
    if ast['orelse']:
        syntax_error(ast, "while loop else thing is not supported")

################################################################################
# compare
@node('compare')
def _(ast, btc, env, top):
    #this implementation of chained comparisons does not work
    #so...they are 'not supported' by @check('compare')
    gen_bytecode(ast['left'], btc, env, False);
    for comp, op in zip(ast['comparators'], ast['ops']):
        gen_bytecode(comp, btc, env, False);
        btc.append(comparison_ops[op])

@check('compare')
def _(ast):
    if len(ast['ops']) > 1: #ex: x < 1 < 1
        syntax_error(ast, "chained comparison is (currently) not supported")

################################################################################
# str
@node('function')
def _(ast, btc, env, top):
    not_implemented_error(ast)

################################################################################
# 'function'
@check('function')
def _(ast):
    """verifies syntatic correctness of function node"""
    assert_type(ast, "function")
    #TODO:
    #check decorators
    #check args
    return True

################################################################################
### return

@node('return')
def _(ast, btc, env, top):
    not_implemented_error(ast)

################################################################################
# pass
@node('pass')
def _(ast, btc, env, top):
    pass

################################################################################

bin_ops = {"+": OP_ADD,
           "*": OP_MULT,
           "-": OP_SUB,
           "/": OP_DIV,
           "//": OP_FDIV,
           "**": OP_EXPT,
           "^": OP_L_XOR,
           "|": OP_L_OR,
           "&": OP_L_AND,
           "%": OP_MOD}

comparison_ops = {'>': OP_GT,
                  '<': OP_LT,
                  '==': OP_EQ,
                  '!=': OP_NOT_EQ,
                  '<=': OP_LT_EQ,
                  '>=': OP_LT_EQ,
                  'in': OP_IN,
                  'not-in': OP_NOT_IN,
                  'is': OP_IS}

def gen_bytecode(ast, btc=None, env=None, top=True):
    global _error
    if btc is None: btc = []
    if env is None: env = make_new_env()
    check_fn = _ast_check_switch_table.get(ast['type'])
    if check_fn:
        check_fn(ast) #the check function is responsible for
                      #error message and exiting
    # else:
    #     print("Warning: node '{}' has no checking function".format(ast['type']))

    fn = _bytecode_switch_table.get(ast['type'])
    if fn:
        fn(ast, btc, env, top)
        return btc
    else:
        _error = True
        print("Error -- gen_bytecode(): unknown AST node type: '{}'"
              .format(ast['type']));

def make_new_env():
    return env()

#TODO: should exit immediately on error, check functions should not return anyting

################################################################################
# error reporting functions

def assert_type(ast, typ):
        pass #TODO

def syntax_error(ast, message):
    global _error
    _error = True
    print("SYNTAX ERROR[{}:{}]: {}"
          .format(ast['lineno'], ast['col_offset'], message))

def not_implemented_error(ast):
    global _error
    _error = True
    print("ERROR[{}:{}]: node type '{}' is not implemented"
          .format(ast['lineno'], ast['col_offset'], ast['type']))


################################################################################

def compile_str(code : str) -> list:
    global _error
    ast = make_ast(code)
    #print(ast)
    _error = False
    env = make_new_env()
    bytecode = []
    for a in ast:
        gen_bytecode(a, bytecode, env)
    if _error:
        exit(1)
    #TODO: CHECK TYPES
    #COMPILE
    return bytecode

#TODO: instead of handling errors like gcc, just terminate after the first one
def compile_ast(ast : list) -> list:
    global _error
    _error = False
    env = make_new_env()
    bytecode = []
    for a in ast:
        gen_bytecode(a, bytecode, env)
    if _error:
        exit(1)
    #TODO: CHECK TYPES
    #COMPILE
    return bytecode


if __name__ == '__main__':
    if len(argv) != 2:
        print('Usage:')
        print('  ./maml-compile.py <filename>.py')
        exit(1)
    filename = argv[1]
    try:
        f = open(filename, 'r')
    except IOError:
        print('Error: where is "{}"?'.format(filename))
        exit(1)

    _block_decorator = 'block'
    _function_decorator = 'function'
    _blocks = {}
    _functions = {}
    def compile_blocks(code):
        "update the compiled blocks and functions in _compiled_code"
        #print("update_compiled_code({})'".format(filename))

        for ast in make_ast(code):
            if ast['type'] == 'function':
                decorators = ast['decorator_list']
                if len(decorators) == 1:
                    name = decorators[0]['id']
                    if name == _block_decorator:
                        #print("compiling block '{}'".format(ast['name']))
                        _blocks[ast['name']] = compile_ast(ast['body'])
                    elif name == _function_decorator:
                        pass #TODO

    #print(compile_str(f.read()))
    compile_blocks(f.read())
    for k in _blocks:
        print("block: '{}'".format(k))
        print("   ", _blocks[k])
    #TODO: compile/print functions

    exit(0)
