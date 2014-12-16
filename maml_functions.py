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
        print_i(ast['n'])
    elif ast['s_type'] == 'str':
        print_s(ast['s']) # generate call to print_s
    elif ast['s_type'] == 'list':
        print_l(ast['elts'])
    elif ast['s_type'] == 'tuple':
        print_a(ast['elts'])
    else:
        pass  # other


@compile('cast')

def _(ast, nargs, btc, env, top):
    pass #TODO
