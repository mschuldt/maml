# compilation and transformations for specific function calls

from maml_syntaxError import *
from maml_typeError import *
from maml_notimpError import *

function_compiler_functions = {}


def compile(name):
    def decorator(fn):
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


@compile('print')
def _(ast, nargs, btc, env, top):
    s_type = ast['s_type']
    if s_type == 'int':
        gen_call_code('print_i', ast, btc, env, top)
    elif s_type == 'float':
        gen_call_code('print_i', ast, btc, env, top)
    elif s_type == 'str':
        gen_call_code('print_s', ast, btc, env, top)
    elif s_type == '[int]':
        gen_call_code('print_l', ast, btc, env, top)
    elif s_type == '(int)':
        gen_call_code('print_a', ast, btc, env, top)
    else:
        raise MamlTypeError("function print: invalid type: '{}'".format(s_type))


@compile('cast')

def _(ast, nargs, btc, env, top):
    pass #TODO
