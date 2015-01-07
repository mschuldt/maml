# compilation and transformations for specific function calls

from maml_syntaxError import *
from maml_typeError import *
from maml_notimpError import *
from maml_env import ftype

from maml_opcodes import *
from maml_config import config
from functools import reduce
from operator import mul

function_compiler_functions = {}
function_compiler_arg_types = {}

primitives = config['primitives']
primitive_dispatch = config['primitive_dispatch']
possible_arg_types = config['possible_arg_types']

def define_dispatch(name :str, arg_types :(str,), primitive :str):
    """
    dispatch: NAME(ARG_TYPES) ==> PRIMITIVE(...)
    """
    if primitive in primitives:
        arg_types = [[]]
        for a in primitives[primitive].args:
            if type(a) is set:
                new_args = []
                for old_args in arg_types:
                    for x in a:
                        l = list(old_args)
                        l.append(x)
                        new_args.append(l)
                arg_types = new_args
            elif type(a) is str:
                for args in arg_types:
                    args.append(a)
            else:
                print("Error -- define_dispatch: invalid arg type"); exit(1)
        prim = primitives[primitive]
        for arg_type in arg_types:
            primitive_dispatch[(name, tuple(arg_type))] = prim
    else:
        print("Error: cannot define a dispatch on a function that does not exist")
        exit(1)

#initialize dispatches to that primitive function names dispatch to themselves
def initialize_primitive_dispatch():
    for name in primitives:
        define_dispatch(name, primitives[name].args, name)

initialize_primitive_dispatch()
#print("PRIMITIVE_DISPATCH:\n", primitive_dispatch)

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

################################################################################
# print

print_function = {'int': 'print_i',
                  'float': 'print_i',
                  'str': 'print_s',
                  '[int]': 'print_l', #TODO: fix list and array types
                  '(int)': 'print_a'}

@compile('print', [['int', 'float', 'str', '[int]', '(int)']], 'none')
def _(ast, btc, env, top):
    s_type = ast['args'][0]['s_type']
    func = print_function.get(s_type)
    if func:
        gen_call_code(func, 1, ast, btc, env, top)
    else:
        raise MamlTypeError("function print: invalid type: '{}'".format(s_type))


@compile('cast')
def _(ast, nargs, btc, env, top):
    pass #TODO
