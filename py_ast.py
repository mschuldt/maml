#!/usr/bin/env python3

from sys import argv
from ast import dump, parse

#helper program to dump the ast of a given python program
if __name__ == "__main__":
    if len(argv) != 2:
        print('Usage:')
        print('  ./py_ast.py <filename>.py')
        exit(1)
    filename = argv[1]
    try:
        f = open(filename, 'r')
    except IOError:
        print('Error: where is "{}"?'.format(filename))
        exit(1)
    print(dump(parse(f.read()),include_attributes=True))
else:
    print("what are you doing?")
    exit(1)
    
