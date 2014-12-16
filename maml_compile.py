#!/usr/bin/env python3

# TODO: Need to calculate the max stack size used by a block of code
#       and have avm.c adjust the stacksize as needed ast start of
#       block execution.


type_checking = False        # enable static type checking
auto_var_types = True       # auto detect variable type
                            # (x = 1 becomes equivalent to x <- int; x = 1)
allow_type_reassign = True  # enable re-declaring variable type
verbose = True
compile_decorator = 'arduino'

from maml_ast import make_ast
from maml_opcodes import *
from maml_env import env
from maml_functions import function_compiler_functions
from functools import reduce
from operator import add
from sys import argv
from _prim import primitives
from maml_syntaxError import *
from maml_typeError import *
from maml_notimpError import *

# for the AST node with type X:
#   * node['type'] == 'X'
#   * The code generation function is defined as
#        @code_gen('X')
#        def _(ast, btc, env, top):
#           ...
#   * The ast node checking function is defined as
#        @ast_check('X')
#        def _(ast):
#           ...
#   * the type annotation and checking function is defined as
#       @type_check('X')
#       def _(ast, env):
#           ...
#   * The corresponding opcode is OP_X
#   * maml_ast.py defines a function X (capitalized)
#     that returns the AST node in dictionary format.
#     Functions in maml_ast.py that are not capitalized
#     generate AST nodes whose bytecode is generated
#     by another nodes generation function
#     (ex: 'function' node consumes the 'arguments' node,
#          so there is no @code_gen('arguments') function)

_bytecode_switch_table = {}
_ast_check_switch_table = {}
_ast_type_check_switch_table = {}


def code_gen(name):
    def decorator(fn):
        _bytecode_switch_table[name] = fn
    return decorator


def ast_check(name):
    def decorator(fn):
        _ast_check_switch_table[name] = fn
    return decorator


def type_check(name):
    def decorator(fn):
        _ast_type_check_switch_table[name] = fn
    return decorator

###############################################################################
# bytecode generation, ast checking, and type annotation/checking

# function parameters:
#   AST is the ast node
#   BTC is the bytecode array
#   ENV keeps track of variable index mappings and types
#   TOP is this a top level node?

###############################################################################
# str


@code_gen('str')
def _(ast, btc, env, top):
    btc.extend([SOP_STR, ast['s'], OP_CONST])

@ast_check('str')
def _(ast):
    if ast['s'].find('\0') != -1:
        raise MamlSyntaxError("Null terminators not allowed in strings.")
        exit(1)


@type_check('str')
def _(ast, env):
    ast['s_type'] = 'str'

###############################################################################
# int


@code_gen('int')
def _(ast, btc, env, top):
    btc.extend([SOP_INT, ast['n'], OP_CONST])


@type_check('int')
def _(ast, env):
    ast['s_type'] = 'int'

###############################################################################
# float


@code_gen('float')
def _(ast, btc, env, top):
    raise MamlNotImplementedError("Node type '{}' is not implemented".format(ast['type']))

###############################################################################
# list


@code_gen('list')
def _(ast, btc, env, top):
    elts = ast['elts']
    for e in elts:
        gen_bytecode(e, btc, env, False)
    print("compiling 'list', len(elts) = {}".format(len(elts)))
    btc.extend([SOP_INT, len(elts), OP_LIST])
    # TODO: if len = 0 ==> NULL


@type_check('list')
def _(ast, env):
    prev_type = None
    for e in ast['elts']:
        check_types(e, env)
        if prev_type:
            if prev_type != e['s_type']:
                raise MamlTypeError("List has multiple types. {} and {}"
                                    .format(prev_type, e['s_type']))
        prev_type = e['s_type']
    ast['s_type'] = '[' + prev_type + ']'

###############################################################################
# subscript

@code_gen('subscript')
def _(ast, btc, env, top):
    theArray = ast['value']
    theIndex = ast['slice']['value']
    gen_bytecode(theArray, btc, env, False)
    gen_bytecode(theIndex, btc, env, False)
    if ast['ctx'] == 'load':
        btc.append(OP_LIST_LOAD)
    else:
        btc.append(OP_LIST_STORE)


@type_check('subscript')
def _(ast, env):
    theArray = ast['value']['id']
    check_types(theArray, env)
    ast['s_type'] = theArray['s_type']


###############################################################################
# tuple


@code_gen('tuple')
def _(ast, btc, env, top):
    elts = ast['elts']
    for e in elts:
        gen_bytecode(e, btc, env, False)
    print("compiling 'tuple', len(elts) = {}".format(len(elts)))
    btc.extend([SOP_INT, len(elts), OP_ARRAY])


@type_check('tuple')
def _(ast, env):
    prev_type = None
    for e in ast['elts']:
        check_types(e, env)
        if prev_type:
            if prev_type != e['s_type']:
                raise MamlTypeError("Tuple has multiple types. {} and {}"
                                    .format(prev_type, e['s_type']))
        prev_type = e['s_type']
    ast['s_type'] = '(' + prev_type + ')'

###############################################################################
# declaration


@code_gen('declaration')
def _(ast, btc, env, top):
    pass


@type_check('declaration')
def _(ast, env):
    # Declarations must be top level so we don't have to set the s_type of this
    # node
    env.declare_type(ast['name'], ast['s_type'])

###############################################################################
# name


# TODO: 'name' and 'assign' still need to be tested with local names
@code_gen('name')
def _(ast, btc, env, top):
    name = ast['id']
    if name == 'None' or name == 'False':
        btc.append(SOP_NULL)
    else:
        globalp, index = env.get_load_index(name)
        op = OP_GLOBAL_LOAD if globalp else OP_LOCAL_LOAD
        btc.extend([SOP_INT, index, op])


@type_check('name')
def _(ast, env):
    ast['s_type'] = env.get_type(ast['id'])

###############################################################################
# nameconstant


@code_gen('nameconstant')
def _(ast, btc, env, top):
    if ast['value']:
        btc.extend([SOP_INT, 1, OP_INT])
    else:
        btc.append(SOP_NULL)

###############################################################################
# assign


@code_gen('assign')
def _(ast, btc, env, top):
    # TODO: Check that target is declared
    # target = ast['targets'][0] #no support for unpacking
    # value = gen_bytecode(ast['value'])
    for target in ast['targets']:
        # TODO: Value should only be evaluated once
        gen_bytecode(ast['value'], btc, env, False)
        globalp, index = env.get_store_index(target['id'])
        op = OP_GLOBAL_STORE if globalp else OP_LOCAL_STORE
        btc.extend([SOP_INT, index, op])


@ast_check('assign')
def _(ast):
    targets = ast['targets']
    # if len(targets) > 1:
    #    #example: 'a,b=x'
    #    raise MamlSyntaxError("unpacking is not supported")
    #    return False
    if targets[0]['type'] == 'starred':
        # example: '*a=x'
        raise MamlSyntaxError("starred assignment is not supported")
        return False
    if ast['targets'][0]['type'] == 'tuple':
        raise MamlSyntaxError("tuple assignment is not supported")
    return True


@type_check('assign')
def _(ast, env):
    # TODO: This currently only works for assignment to variables
    check_types(ast['value'], env)
    val_type = ast['value']['s_type']

    for target in ast['targets']:
        if auto_var_types:
            target['s_type'] = val_type
        else:
            check_types(target, env)
        env.declare_type(target['id'], target['s_type'])

        if target['s_type'] != val_type:
            raise MamlTypeError("incompatible assignment. var '{}' has type '{}'" +
                                ", got '{}'".format(ast['targets'][0]['id'],
                                                    ast['targets'][0]['s_type'],
                                                    ast['value']['s_type']))
    ast['s_type'] = 'None'


###############################################################################
# expr


# TODO: Eliminate this type in maml_ast.py ?
@code_gen('expr')
def _(ast, btc, env, top):
    # if not check_expr(ast): return
    gen_bytecode(ast['value'], btc, env, top)


@type_check('expr')
def _(ast, env):
    check_types(ast['value'], env)
    ast['s_type'] = ast['value']['s_type']

###############################################################################
# binop


@code_gen('binop')
def _(ast, btc, env, top):
    gen_bytecode(ast['left'], btc, env, False)
    gen_bytecode(ast['right'], btc, env, False)
    btc.append(bin_ops[ast['op']])


@type_check('binop')
def _(ast, env):
    check_types(ast['left'], env)
    check_types(ast['right'], env)
    t_l = ast['left']['s_type']
    t_r = ast['right']['s_type']
    op = ast['op']
    if t_l not in valid_bin_op_types[op]:
        raise MamlTypeError("Invalid type for left operand: '{}'. Expected '{}'"
                            .format(t_l, reduce(lambda a, b: a + " or " + b,
                                                valid_bin_op_types[op])))

    if t_r not in valid_bin_op_types[op]:
        raise MamlTypeError("Invalid type for right operand: '{}'. Expected '{}'"
                            .format(t_r, reduce(lambda a, b: a + "' or '" + b,
                                                valid_bin_op_types[op])))
    if t_l != t_r:
        raise MamlTypeError("Type for {} do not match, '{}' and '{}'"
                            .format(op, t_l, t_r))
    ast['s_type'] = t_l


@ast_check('binop')
def _(ast):
    if ast['op'] in bin_ops:
        return True
    # Check 'left' and 'right' properties
    return False

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

valid_bin_op_types = {"+": ['int', 'float', 'str'],
                      "*": ['int', 'float'],
                      "-": ['int', 'float'],
                      "/": ['int', 'float'],
                      "//": ['int', 'float'],
                      "**": ['int', 'float'],
                      "^": ['int', 'float'],
                      "|": ['int', 'float'],
                      "&": ['int', 'float'],
                      "%": ['int', 'float']}

###############################################################################
# call


@code_gen('call')
def _(ast, btc, env, top):
    """
    bytecode format:
    arg1 arg2 ... argn OP_PRIM_CALL function_pointer
    serial format:
    SOP_PRIM  num_arguments func_index
    where:
    func_index = index of function_pointer in the array 'primitives'
    """

    nargs = len(ast['args'])
    for arg in ast['args']:
        gen_bytecode(arg, btc, env, False)
    name = ast['func']['id']
    index = primitives.get(name, None)
    transform_fn = function_compiler_functions.get(name)
    if index is not None:  # Calling a primative
        # We have to use SOP_INT here so that the bytecode expansion
        # can expand the numbers
        if transform_fn:
            raise MamlSyntaxError("primitive function '{}' has tranform function"
                  .format(name))
        btc.extend([SOP_INT, index, SOP_INT, nargs, SOP_PRIM_CALL])
        if top:
            btc.append(OP_POP)
    elif transform_fn:
        transform_fn(ast, btc, env, top)
    else:  # Calling a user defined function
        raise MamlNotImplementedError("Not implemented: calling non-primitives ('{}')"
              .format(ast['func']['id']))


@type_check('call')
def _(ast, env):
    # TODO:
    ast['s_type'] = "TODO"


@ast_check('call')
def _(ast):
    if ast['keywords']:
        raise MamlSyntaxError("keyword args are not supported")
    if ast['starargs']:
        raise MamlSyntaxError("starargs are not supported")
    if ast['kwargs']:
        raise MamlSyntaxError("kwargs args are not supported")


@type_check('call')
def _(ast, env):
    ast['s_type'] = "TODO:call"
    return
    functionArgs = env.funcTypes[ast['func']['id']]
    argNum = 0
    for elem in ast['args']:
        check_types(elem, env)
        if elem['s_type'] != functionArgs.argTypes[argNum]:
            raise MamlTypeError("argument type {} does not match received "
                                + "argument type {}"
                                .format(functionArgs.argTypes[argNum], elem['s_type']))
        argNum += 1
    ast['s_type'] = functionArgs.returnType

###############################################################################
# if


@code_gen('if')
def _(ast, btc, env, top):
    false_l = env.make_label()  # Marks beginning of false code
    done_l = env.make_label()  # Marks end of false code
    gen_bytecode(ast['test'], btc, env, False)
    btc.extend([OP_IF, SOP_INT, false_l, OP_JUMP])
    for node in ast['body']:
        gen_bytecode(node, btc, env, top)
    btc.extend([SOP_INT, done_l, OP_JUMP, SOP_INT, false_l, SOP_LABEL])
    for node in ast['else']:
        gen_bytecode(node, btc, env, top)
    btc.extend([SOP_INT, done_l, SOP_LABEL])


@type_check('if')
def _(ast, env):
    check_types(ast['test'], env)
    for node in ast['body']:
        check_types(node, env)
    for node in ast['else']:
        check_types(node, env)
    ast['s_type'] = 'None'

###############################################################################

# if-exp
@code_gen('if-exp')
def _(ast, btc, env, top):
    raise MamlNotImplementedError("Node type '{}' is not implemented".format(ast['type']))

@type_check('if-exp')
def _(ast, env):
    pass

###############################################################################
# while


@code_gen('while')
def _(ast, btc, env, top):
    # [start] <test> OP_IF <jump end> <body> <jump start> [end]
    start_l = env.make_label()
    end_l = env.make_label()
    env.start_while(start_l, end_l)
    btc.extend([SOP_INT, start_l, SOP_LABEL])
    gen_bytecode(ast['test'], btc, env, False)
    btc.extend([OP_IF, SOP_INT, end_l, OP_JUMP])
    for node in ast['body']:
        gen_bytecode(node, btc, env, top)
    btc.extend([SOP_INT, start_l, OP_JUMP, SOP_INT, end_l, SOP_LABEL])
    env.end_while()


@ast_check('while')
def _(ast):
    if ast['orelse']:
        raise MamlSyntaxError("while loop else thing is not supported")


@type_check('while')
def _(ast, env):
    for node in ast['body']:
        check_types(node, env)
    ast['s_type'] = 'None'

###############################################################################
#continue

@code_gen('continue')
def _ast(ast, btc, env, top):
    btc.extend([SOP_INT, env.get_while_start_label(), OP_JUMP])

@type_check('continue')
def _ast(ast, env):
    pass



###############################################################################
#break

@code_gen('break')
def _ast(ast, btc, env, top):
    btc.extend([SOP_INT, env.get_while_end_label(), OP_JUMP])

@type_check('break')
def _ast(ast, env):
    pass


###############################################################################
# compare


@code_gen('compare')
def _(ast, btc, env, top):
    # This implementation of chained comparisons does not work
    # so...they are 'not supported' by @ast_check('compare')
    gen_bytecode(ast['left'], btc, env, False)
    for comp, op in zip(ast['comparators'], ast['ops']):
        gen_bytecode(comp, btc, env, False)
        btc.append(comparison_ops[op])

@type_check('compare')
def _(ast, env):
    check_types(ast['left'], env)
    check_types(ast['comparators'][0], env)
    #we can compare any types together
    #TODO: check types of comparisons like '>'
    ast['s_type'] = 'int'


@ast_check('compare')
def _(ast):
    if len(ast['ops']) > 1:  # ex: x < 1 < 1
        raise MamlSyntaxError("chained comparison is (currently) not supported")

comparison_ops = {'>': OP_GT,
                  '<': OP_LT,
                  '==': OP_EQ,
                  '!=': OP_NOT_EQ,
                  '<=': OP_LT_EQ,
                  '>=': OP_GT_EQ,
                  'in': OP_IN,
                  'not-in': OP_NOT_IN,
                  'is': OP_IS}

###############################################################################
# function


@code_gen('function')
def _(ast, btc, env, top):
    argTypes = {}
    argNum = 0
    for elem in ast['args']:
        argTypes[argNum] = elem['argType']
        argNum += 1
    env.createFuncTypes(ast['name'], argTypes, ast['returns']['id'])
    raise MamlNotImplementedError("Node type '{}' is not implemented".format(ast['type']))


@ast_check('function')
def _(ast):
    """verifies syntatic correctness of function node"""
    # TODO:
    # check decorators
    # check args
    return True

##############################################################################
# return


@code_gen('return')
def _(ast, btc, env, top):
    raise MamlNotImplementedError("Node type '{}' is not implemented".format(ast['type']))

###############################################################################
# pass


@code_gen('pass')
def _(ast, btc, env, top):
    pass


@type_check('pass')
def _(ast, env):
    pass

###############################################################################


def gen_bytecode(ast, btc=None, env=None, top=True):
    if btc is None:
        btc = []
    if env is None:
        env = make_new_env()
    check_fn = _ast_check_switch_table.get(ast['type'])
    if check_fn:
        # The check function is responsible for
        # error message and exiting
        check_fn(ast)
    # else:
    #     print("Warning: node '{}' has no checking function"
    #           .format(ast['type']))

    fn = _bytecode_switch_table.get(ast['type'])
    if fn:
        fn(ast, btc, env, top)
        return btc
    else:
        raise MamlNotImplementedError("gen_bytecode(): unknown AST node type: '{}'"
              .format(ast['type']))


def make_new_env():
    return env(None, allow_type_reassign)

# TODO: Should exit immediately on error, check functions should not return
# anyting

###############################################################################
# type analysis


def check_types(ast, env):
    """
    Checks for type correctness and annotates AST nodes with their type
    """

    fn = _ast_type_check_switch_table.get(ast['type'])
    if fn:
        fn(ast, env)
    else:
        raise MamlNotImplementedError("ast node '{}' has no @type_check type analysis function"
              .format(ast['type']))


###############################################################################


def compile_str(code: str) -> list:
    ast = make_ast(code)
    env = make_new_env()
    bytecode = []
    for a in ast:
        if type_checking:
            check_types(a, env)
            if not a.get('s_type'):
                raise MamlTypeError("node '{}' was not annotated with static type"
                                    .format(a['type']))
        gen_bytecode(a, bytecode, env)
    # TODO: CHECK TYPES
    # COMPILE
    return bytecode


# TODO: instead of handling errors like gcc, just terminate after the first one
def compile_ast(ast: list) -> list:
    env = make_new_env()
    bytecode = []
    for a in ast:
        if type_checking:
            check_types(a, env)
            if not a.get('s_type'):
                raise MamlTypeError("node '{}' was not annotated with static type"
                                    .format(a['type']))
        gen_bytecode(a, bytecode, env)
    # TODO: CHECK TYPES
    # COMPILE
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
        """
        Update the compiled blocks and functions in _compiled_code
        """
        # print("update_compiled_code({})'".format(filename))

        for ast in make_ast(code):
            if ast['type'] == 'function':
                decorators = ast['decorator_list']
                if len(decorators) == 1:
                    name = decorators[0]['id']
                    if name == _block_decorator:
                        # print("compiling block '{}'".format(ast['name']))
                        _blocks[ast['name']] = compile_ast(ast['body'])
                    elif name == _function_decorator:
                        pass  # TODO

    # print(compile_str(f.read()))
    compile_blocks(f.read())
    for k in _blocks:
        print("block: '{}'".format(k))
        print("   ", _blocks[k])
    # TODO: compile/print functions

    exit(0)
