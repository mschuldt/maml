# compilation and transformations for specific function calls

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
    else:
        raise MamlNotImplementedError("Not implemented: calling non-primitives ('{}')"
                                      .format(ast['func']['id']))


@compile('print')
def _(ast, nargs, btc, env, top):
    if ast['s_type'] == 'int':
        gen_call_code('print_i', 1, ast, btc, env, top)
    elif ast['s_type'] == 'float':
        gen_call_code('print_i', 1, ast, btc, env, top)
    elif ast['s_type'] == 'str':
        gen_call_code('print_s', 1, ast, btc, env, top)
    elif ast['s_type'] == 'list':
        gen_call_code('print_l', 1, ast, btc, env, top)
    elif ast['s_type'] == 'tuple':
        gen_call_code('print_a', 1, ast, btc, env, top)
    else:
        pass  # other


@compile('cast')

def _(ast, nargs, btc, env, top):
    pass #TODO
