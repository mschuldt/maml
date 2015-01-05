# compilation and transformations for specific function calls

from maml_syntaxError import *
from maml_typeError import *
from maml_notimpError import *
from maml_env import ftype
from _prim import desktop_primitives, arduino_primitives
from maml_opcodes import *

function_compiler_functions = {}
function_compiler_arg_types = {}

desktop_p = True #TODO: fix

primitives = (desktop_primitives if desktop_p else arduino_primitives)

def compile(name, args=None, ret='none'):
    def decorator(fn):
        function_compiler_arg_types[name] = ftype(args, ret)
        function_compiler_functions[name] = fn
    return decorator

def gen_call_code(name, nargs, ast, btc, env, top):
    """
    generate a call to function NAME
    """
    index = primitives.get(name, None)
    if index:
        btc.extend([SOP_INT, index.index, SOP_INT, nargs, SOP_PRIM_CALL])
        if top:
            btc.append(OP_POP)
    else:
        #TODO: test this
        globalp, index = env.get_load_index(name, ast)
        op = OP_GLOBAL_LOAD if globalp else OP_LOCAL_LOAD
        btc.extend([SOP_INT, index, op])
        btc.append(OP_CALL)

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
