# compilation and transformations for specific function calls

function_compiler_functions = {}


def compile(name):
    def decorator(fn):
        function_compiler_functions[name] = fn
    return decorator


@compile('print')
def _(ast, btc, env, top):
    if ast['s_type'] == 'int':
        print_i(ast['n'])  # generate call to print_i
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

def _(ast, btc, env, top):
    pass #TODO
