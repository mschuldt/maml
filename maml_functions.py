# compilation and transformations for specific function calls

from maml_syntaxError import *
from maml_typeError import *
from maml_notimpError import *

function_compiler_functions = {}


def compile(name, args=None, ret='none'):
    def decorator(fn):
        function_compiler_arg_types[name] = ftype(args, ret)
        function_compiler_functions[name] = fn
    return decorator

def gen_call_code(name, ast, btc, env, top):
    """
    generate a call to function NAME
    """
    index = primitives.get(name, None)
    if index:
        btc.extend([SOP_INT, index, SOP_INT, nargs, SOP_PRIM_CALL])
        if top:
            btc.append(OP_POP)
    else:
        raise MamlNotImplementedError("Not implemented: calling non-primitives ('{}')"
                                      .format(ast['func']['id']))

@compile('print', [['int', 'float', 'str', '[int]', '(int)']], 'none')

def _(ast, btc, env, top):
    s_type = ast['args'][0]['s_type']
    if s_type == 'int':
        gen_call_code('print_i', 1, ast, btc, env, top)
    elif s_type == 'float':
        gen_call_code('print_i', 1, ast, btc, env, top)
    elif s_type == 'str':
        gen_call_code('print_s', 1, ast, btc, env, top)
    elif s_type == '[int]':
        gen_call_code('print_l', 1, ast, btc, env, top)
    elif s_type == '(int)':
        gen_call_code('print_a', 1, ast, btc, env, top)
    else:
        raise MamlTypeError("function print: invalid type: '{}'".format(s_type))


@compile('cast')

def _(ast, nargs, btc, env, top):
    pass #TODO
