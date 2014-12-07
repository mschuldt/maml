#!/usr/bin/env python3

import ast
from sys import argv

def Module(body):
    return body
    
def FunctionDef(name, args, body, decorator_list, returns, lineno=None, col_offset=None):
    return {'type': 'function',
            'name' : name,
            'args' : args,
            'body' : body,
            'decorator_list' : decorator_list,
            'returns' : returns,
            'lineno': lineno,
            'col_offset': col_offset}

def arguments(args=None, vararg=None, varargannotation=None, kwonlyargs=None,
              kwarg=None, kwargannotation=None,defaults=None, kw_defaults=None):
    return {'type': 'arguments',
            'args' :args,
            'vararg' :vararg,
            'varargannotation' :varargannotation,
            'kwonlyargs' :kwonlyargs,
            'kwarg' :kwarg,
            'kwargannotation' :kwargannotation,
            'defaults' :defaults,
            'kw_defaults' :kw_defaults}

def Assign(targets, value, lineno=None, col_offset=None):
    return {'type' : 'assign',
            'targets': targets,
            'value': value,
            'lineno': lineno,
            'col_offset': col_offset}
    
def Num(n, lineno=None, col_offset=None):
    return {'type': ('int' if type(n) is int else 'float'),
            'n' : n,
            'lineno': lineno,
            'col_offset': col_offset}

def Str(s, lineno=None, col_offset=None):
    return {'type' : 'str',
            's' : s,
            'lineno': lineno,
            'col_offset': col_offset}
    
def Name(id, ctx, lineno=None, col_offset=None):
    return {'type' : 'name',
            'id' : id,
            'ctx' : ctx,
            'lineno': lineno,
            'col_offset': col_offset}


def Call(func, args, keywords, starargs, kwargs, lineno=None, col_offset=None):
    return {'type': 'call',
            'lineno': lineno,
            'func':func,
            'args':args,
            'keywords':keywords,
            'starargs':starargs,
            'kwargs':kwargs,
            'col_offset': col_offset}

def keyword(arg, value):
    return {'type': keyword,
            'arg': arg,
            'value':value}

def Expr(value, lineno=None, col_offset=None):
    return {'type' : 'expr',
            'value': value,
            'lineno': lineno,
            'col_offset': col_offset}


    
def Return(value, lineno='nil', col_offset='nil'):
    return {'type' : 'return',
            'value': value,
            'lineno': lineno,
            'col_offset': col_offset}
    
def Load():     return 'load'
def Store():    return 'store'

def is_type_declaration(ast):
    "return type if AST is a type declaration node, else False"
    pass

valid_types = ['int', 'str', 'float', 'func']

def get_type(ast):
    "returns the type of ast or None if it does not describe one"
    if ast['type'] == 'list':
        is_list = True if len(ast['elts']) == 1 else False
        ast = ast['elts'][0]
    if ast['type'] == name:
        t = ast['id']
        if t not in valid_types: return None
    return "[{}]".format(t) if is_list else t
    
def Compare (left, ops, comparators, lineno=None, col_offset=None):
    if len(ops) == 1 and ops[0] == '<':
        #special case for type declaration
        if left['type'] == 'name':
            c = comparators[0] 
            if c['type'] == 'unaryOp' and c['op'] == 'usub':
                t = get_type(c['operand'])
                if t:
                    return {'type': 'declaration',
                            'name': left['id'],
                            'type_':t,
                            'lineno': lineno,
                            'col_offset': col_offset}
    return {'type': 'compare',
            'left': left,
            'comparators' : comparators,
            'lineno': lineno,
            'col_offset': col_offset}

def BinOp(left, op, right, lineno=None, col_offset=None):
    return {'type': 'binop',
            'left': left,
            'op': op,
            'right': right,
            'lineno': lineno,
            'col_offset': col_offset}
    
def Add():      return "+"
def Mult():     return "*"
def Sub():      return "-"
def Div():      return "/"
def FloorDiv(): return "//"
def Pow():      return "**"
def BitXor():   return "^"
def BitOr():    return "|"
def BitAnd():   return "&"
def Mod():      return "%"

def UnaryOp(op, operand, lineno=None, col_offset=None):
    return {'type': 'unaryOp',
            'op': op,
            'operand': operand,
            'lineno': lineno,
            'col_offset': col_offset}

def Not(): return "not"
def USub(): return "usub"
    
def Starred(value, ctx, lineno, col_offset):
    return {'type' : 'starred',
            'value': value,
            'ctx': ctx,
            'lineno': lineno,
            'col_offset': col_offset}

def Tuple(elts, ctx, lineno=None, col_offset=None):
    return {'type' : 'tuple',
            'elts': elts,
            'ctx' : ctx,
            'lineno': lineno,
            'col_offset': col_offset}

def Attribute(value, attr, ctx, lineno=None, col_offset=None):
    return {'type' : 'attribute',
            'value': value,
            'attr': attr,
            'ctx': ctx,
            'lineno': lineno,
            'col_offset': col_offset}


def Import(names, lineno=None, col_offset=None):
    return {'type': 'import',
            'names': names,
            'lineno': lineno,
            'col_offset': col_offset}

def ImportFrom(module, names, level, lineno=None, col_offset=None):
    return {'type': 'importfrom',
            'module': module,
            'names': names,
            'level': level,
            'lineno': lineno,
            'col_offset': col_offset}
    
def alias(name, asname):
    return {'type': 'alias',
            'name': name,
            'asname': asname}

def If(test, body, orelse, lineno=None, col_offset=None):
    return {'type': 'if',
            'test': test,
            'body': body,
            'else': orelse,
            'lineno': lineno,
            'col_offset': col_offset}

def make_ast(code):
    return eval(ast.dump(ast.parse(code),include_attributes=True))

if __name__ == "__main__":
    if len(argv) != 2:
        print('Usage:')
        print('  ./maml_ast.py <filename>.py')
        exit(1)
    filename = argv[1]
    try:
        f = open(filename, 'r')
    except IOError:
        print('Error: where is "{}"?'.format(filename))
        exit(1)
    print(make_ast(f.read()))
