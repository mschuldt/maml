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

# Because python has no equivalent to a switch statement
# we use a dictionary to achieve constant time lookups
# and use the following convention to keep ourselves sane:
#
# for the AST node X:
#   * node['type'] == 'X'
#   * the function _gen_X(ast,btc,env) returns bytecode for X,
#     these functions call `gen_bytecode' recursively
#   * the dictionary `_bytecode_switch_table' has entries
#     in the format: 'X' : _gen_X
#   * the function check_X(ast) verifies syntax
#   * the corresponding opcode is OP_X
#   * maml_ast.py defines a function X (capitalized)
#     that returns the AST node in dictionary format.
#     Functions in maml_ast.py that are not capitalized
#     generate AST nodes whose bytecode is generated
#     by another nodes generation function
#     (ex: '_gen_function' consumes the 'arguments' ast node,
#          so there is no _gen_arguments function)

# _gen_X function parameters:
#   AST is the ast node
#   BTC is the bytecode array
#   ENV keeps track of variable index mappings and types

def _gen_str(ast, btc, env):
    if not check_str(ast): return
    btc.extend([SOP_STR, ast['s']])

def _gen_int(ast, btc, env):
    if not check_int(ast): return
    btc.extend([SOP_INT, ast['n']])

def _gen_float(ast, btc, env):
    not_implemented_error(ast)

def _gen_name(ast, btc, env):
    if not check_name(ast): return
    btc.extend([OP_NAME, env.name_index(ast['id'])])

def _gen_assign(ast, btc, env):
    if not check_assign(ast): return
    #TODO: check that target is declared

    target = ast['targets'][0] #no support for unpacking
    #value = gen_bytecode(ast['value'])
    gen_bytecode(ast['value'], btc, env)
    btc.extend([OP_ASSIGN, env.name_get_create(target)])

def _gen_expr(ast, btc, env):
    #if not check_expr(ast): return
    gen_bytecode(ast['value'], btc, env)


def _gen_binop(ast, btc, env):
    if not check_binop(ast): return
    gen_bytecode(ast['left'], btc, env);
    gen_bytecode(ast['right'], btc, env);
    btc.append(bin_ops[ast['op']])

def _gen_call(ast, btc, env):
    not_implemented_error(ast)
def _gen_function(ast, btc, env):
    not_implemented_error(ast)
def _gen_return(ast, btc, env):
    not_implemented_error(ast)

_bytecode_switch_table = {'str': _gen_str,
                          'int': _gen_int,
                          'float': _gen_float,
                          'name' : _gen_name,
                          'assign': _gen_assign,
                          'call' : _gen_call,
                          'expr': _gen_expr,
                          'function': _gen_function,
                          'binop': _gen_binop,
                          'expr': _gen_expr,
                          'return': _gen_return}

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

def gen_bytecode(ast, btc=None, env=None):
    global _error
    if btc is None: btc = []
    if env is None: env = make_new_env()
    fn = _bytecode_switch_table.get(ast['type'])
    if fn:
        fn(ast, btc, env)
        return btc
    else:
        _error = True
        print("Error -- gen_bytecode(): unknown AST node type: '{}'"
              .format(ast['type']));

def make_new_env():
    return env()

################################################################################
# ast checking functions
def check_function(ast):
    """verifies syntatic correctness of function node"""
    assert_type(ast, "function")
    #TODO:
    #check decorators
    #check args
    return True

def check_str(ast):
    assert_type(ast, "str")
    #TODO:
    return True

def check_int(ast):
    assert_type(ast, "int")
    return type(ast['n']) is int

def check_float(ast):
    assert_type(ast, "float")
    return type(ast['n']) is float

def check_name(ast):
    assert_type(ast, "name")
    return True

def check_assign(ast):
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

def check_binop(ast):
    assert_type(ast, "binop")
    if ast['op'] in bin_ops:
        return True
    #check 'left' and 'right' properties
    return False

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
    bytecode = reduce(add, map(gen_bytecode, ast));
    if _error:
        exit(1)
    #TODO: CHECK TYPES
    #COMPILE
    return bytecode

#TODO: instead of handling errors like gcc, just terminate after the first one
def compile_ast(ast : list) -> list:
    global _error
    _error = False
    bytecode = reduce(add, map(gen_bytecode, ast));
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
