# compilation and transformations for specific function calls

function_compiler_functions = {}

def compile(name):
    def decorator(fn):
        function_compiler_functions[name] = fn
    return decorator


@compile('print')
def _(ast, btc, env, top):
    if ast['s_type'] == 'int':
        pass #generate call to print_i    
    elif ast['s_type'] == 'str':
        pass #generate call to print_s
    else:
        pass #other

@compile('cast')
def _(ast, btc, env, top):
    pass #TODO

    

        
        
