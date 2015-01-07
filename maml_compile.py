#!/usr/bin/env python3

# TODO: Need to calculate the max stack size used by a block of code
#       and have avm.c adjust the stacksize as needed ast start of
#       block execution.


type_checking = True        # enable static type checking
auto_var_types = True       # auto detect variable type
                            # (x = 1 becomes equivalent to x <- int; x = 1)
allow_type_reassign = True  # enable re-declaring variable type
verbose = True
compile_decorator = 'arduino'

from maml_ast import make_ast
from maml_opcodes import *
from maml_functions import function_compiler_functions
from maml_functions import function_compiler_arg_types
from functools import reduce
from operator import add
from sys import argv
from maml_syntaxError import *
from maml_typeError import *
from maml_notimpError import *
from maml_env import env, ftype
from maml_config import config

primitives = config['primitives']
primitive_dispatch = config['primitive_dispatch']

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

@ast_check('int')
def _(ast):
    pass #TODO: check size

###############################################################################
# float

@code_gen('float')
def _(ast, btc, env, top):
    raise MamlNotImplementedError("Node type '{}' is not implemented".format(ast['type']))

@ast_check('float')
def _(ast):
    pass#TODO: check size

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
    ast['s_type'] = ('[' + prev_type + ']') if prev_type else 'None'

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


@ast_check('subscript')
def _(ast):
    pass

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
        #NOTE: if this changes, similar logic in 'gen_call_code' in
        #maml_functions.py should also be modified
        globalp, index = env.get_load_index(name, ast)
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
        globalp, index = env.get_store_index(target['id'], ast)
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
    #print("'call' code gen for '{}'".format(name))
    arg_types = []
    for arg in ast['args']:
        arg_types.append(arg['s_type'])
    #print("arg_types = ", arg_types)
    #print("primative dispatch = ", primitive_dispatch)
    prim = primitive_dispatch.get((name, tuple(arg_types)))
    transform_fn = function_compiler_functions.get(name)
    if prim is not None:  # Calling a primative
        # We have to use SOP_INT here so that the bytecode expansion
        # can expand the numbers
        index = prim.index
        if transform_fn:
            raise MamlSyntaxError("conflicting types: function '{}' has tranform function and primitive definition".format(name))
        btc.extend([SOP_INT, index, SOP_INT, nargs, SOP_PRIM_CALL])
        if top:
            btc.append(OP_POP)
    elif transform_fn:
        transform_fn(ast, btc, env, top)
    else:  # Calling a user defined function
        gen_bytecode(ast['func'], btc, env, False)#False because we want the result
        btc.append(OP_CALL)
        #raise MamlNotImplementedError("Not implemented: calling non-primitives ('{}')"
        #                             .format(ast['func']['id']))
        if top:
            btc.append(OP_POP)

def type_cmp(a, b):
    if type(a) is list:
        return (b in a) or b == 'any'
    return a == b or a == 'any' or b == 'any'

@type_check('call')
def _(ast, env):
    name = ast['func']['id']
    #check argument types
    arg_types = []
    for a in ast['args']:
        check_types(a, env)
        arg_types.append(a['s_type'])

    #check if we are calling a primitive
    func_type = primitive_dispatch.get((name, tuple(arg_types)))
    if not func_type:
        #check if we are calling a transform function
        if name in function_compiler_arg_types:
            func_type = function_compiler_arg_types[name]
    if not func_type:
        #check if we are calling a user defined function:
        func_type = env.get_type(name, True)

    #TODO: check if the types of the args are subtypes of the func parameters
    #      (currently the arg types must be the same)

    if not func_type:
        raise MamlTypeError("invalid function: {}({})".format(name, ", ".join(arg_types)))

    #TODO: check that we are calling a function
    #      (currently assuming that we are calling a name)

    ast['s_type'] = func_type.ret

@ast_check('call')
def _(ast):
    #TODO: check that ast['func'] is a function with len(ast['args']) args
    if ast['keywords']:
        raise MamlSyntaxError("keyword args are not supported")
    if ast['starargs']:
        raise MamlSyntaxError("starargs are not supported")
    if ast['kwargs']:
        raise MamlSyntaxError("kwargs args are not supported")


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
    compile_function_node(ast, btc, env, top)

def compile_function_node(ast, btc, env, top):
    # body: ast['body']
    if not top:
        raise MamlSyntaxError("function definition not at top level")
    fn_name = ast['name']
    #print("compiling function '{}'".format(fn_name))
    _, index = env.get_store_index(fn_name, ast)
    new_env = make_new_env(env)
    args = ast['args']['args']
    arg_indexes = []
    for arg in args:
        arg_indexes.append(new_env.get_store_index(arg['arg'])[1])
    n_args = len(args)
    assert not (set(range(n_args)) - set(arg_indexes)), "args must be in the first n indexes"
    body = []
    for a in ast['body']:
        gen_bytecode(a, body, new_env, True)
    #TODO: len(body) is not the actual length of the function body
    n_locals = len(new_env.names)
    btc.extend([SOP_INT, n_args, SOP_INT, n_locals, SOP_INT, len(body), SOP_START_FUNCTION] + body + [SOP_INT, index, SOP_END])


function_return_type = None

@type_check('function')
def _(ast, env):
    #TODO: support recursive function calls
    global function_return_type
    #get arg annotations
    arg_types = []
    new_env = make_new_env(env)
    for arg in ast['args']['args']:
        name = arg['arg']
        if arg['argType']:
            typ = arg['argType']['id']##TODO: fix for other types
        else:
            raise MamlTypeError("undeclared parameter type in '{}'".format(ast['name']))
        new_env.declare_type(name, typ)
        arg_types.append(typ)

    #declare this functions type in its own environment so that
    #it can be called recursively. At this point we don't know
    #the return value, so recursively called functions must
    #have a return value type annotation.
    if ast['returns']:
        new_env.declare_type(ast['name'], ftype(arg_types, ast['returns']['id']))

    #check body and return type
    function_return_type = None
    for a in ast['body']:
        check_types(a, new_env)

    assert function_return_type, "cannot find function return type"
    #TODO: "ast['returns']['id']" only works for names
    if ast['returns'] and ast['returns']['id'] != function_return_type:
        raise MamlTypeError("annotated type does not match return type")

    print("____declaring_type:", ast['name'])
    env.declare_type(ast['name'], ftype(arg_types, function_return_type))

    ast['s_type'] = 'None'
    function_return_type = None
    return


    argTypes = {}
    argNum = 0
    for elem in ast['args']['args']:
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
    #raise MamlNotImplementedError("Node type '{}' is not implemented".format(ast['type']))
    gen_bytecode(ast['value'], btc, env, False)
    btc.append(OP_RETURN)

@type_check('return')
def _(ast, env):
    global function_return_type
    check_types(ast['value'], env)
    s_type = ast['value']['s_type']
    if function_return_type and function_return_type != s_type:
        raise MamlTypeError("returning multiple types from function, found: '{}' previously: '{}'".format(s_type, function_return_type))

    ast['s_type'] = function_return_type = s_type

@ast_check('return')
def _(ast):
    pass #TODO

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


def make_new_env(parent=None):
    return env(parent, allow_type_reassign)

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
last_fields = ['s_type', 'lineno', 'col_offset']
indent = "    "
def print_ast(ast, level=0):
    if type(ast) is list:
        print(indent * level, "[")
        level += 1
        for a in ast:
            print_ast(a, level)
            print(indent * level, ',')
        level -= 1
        print(indent * level, "]")
        return
    def print_fields(fields, exclude):
        for field in fields:
            if field not in exclude:
                print(indent * level, field + ":", end="")
                v = ast.get(field)
                if type(v) not in [dict, list]:
                    print(" ", v)
                    continue

                print("")
                print_ast(v, level+1)
    print_fields(ast.keys(), last_fields)
    print_fields(last_fields, [])

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
def compile_ast(ast, env=None):
    env = env or make_new_env()
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

def compile_function(ast, env=None):
    env = env or make_new_env()
    bytecode = []
    if type_checking:
        check_types(ast, env)
    compile_function_node(ast, bytecode, env, True)
    return bytecode

if __name__ == '__main__':
    #TODO: this needs to be updated to read the -[a|d] arg and
    #      call compile_ast with desktop_p
    l = len(argv)
    if l < 2 or l > 3 or (l == 3 and argv[1] != "--print-ast"):
        print('Usage:')
        print('  ./maml-compile.py [--print-ast] <filename>.py')
        exit(1)
    if argv[1] == "--print-ast":
        show_ast = True
        filename = argv[2]
    else:
        show_ast = False
        filename = argv[1]
    try:
        f = open(filename, 'r')
    except IOError:
        print('Error: where is "{}"?'.format(filename))
        exit(1)

    _block_decorator = 'block'
    _function_decorator = 'function'
    _block_decorator_types = ['once', 'chain']

    _blocks = {}
    _funcs = {}
    _block_ast = {}
    _func_ast = {}
    _env = make_new_env()
    def compile_blocks(code):
        """
        Update the compiled blocks and functions in _compiled_code
        """
        # print("update_compiled_code({})'".format(filename))

        for ast in make_ast(code):
            if ast['type'] == 'function':
                decorators = ast['decorator_list']
                if len(decorators) == 1:
                    decorator = decorators[0]
                    if 'func' in decorator:
                        name = decorator['func']['id']
                        if name == _block_decorator:
                            args = decorator['args']
                            if len(args) != 1:
                                continue
                            if args[0]['id'] in _block_decorator_types:
                                a = ast['body']
                                _block_ast[ast['name']] = a
                                _blocks[ast['name']] = compile_ast(a,  _env)
                    elif 'id' in decorator:
                        if decorator['id'] == _function_decorator:
                            _func_ast[ast['name']] = ast
                            _funcs[ast['name']] = compile_function(ast,  _env)

    # print(compile_str(f.read()))
    compile_blocks(f.read())
    for k in _blocks:
        print("block: '{}'".format(k))
        print("   ", _blocks[k])
    for f in _funcs:
        print("function: '{}'".format(f))
        print("   ", _funcs[f])

    if show_ast:
        for k in _block_ast:
            print("block AST: '{}'".format(k))
            print("   ", print_ast(_block_ast[k]))
        for f in _func_ast:
            print("function AST: '{}'".format(f))
            print("   ", print_ast(_func_ast[f]))


    exit(0)
