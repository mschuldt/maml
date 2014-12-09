
avm: avm.c _opcodes.h _prim.c _prim.py
	g++ -g -fpermissive avm.c -o avm
#	g -std=c99 -g  avm.c -o avm

all: avm ino

_opcodes.h: maml_opcodes.py
	./maml_opcodes.py

_prim.c _prim.py: primitives.c process_primitives.el
	./process_primitives.el


avm.ino: avm.c _opcodes.h _prim.c _prim.py build_ino_file.el
	./build_ino_file.el

ino: avm.ino

.PHONY: clean test

test:
	./run_tests.sh
clean::
	rm -f avm _prim.c _prim.py _opcodes.h avm.ino


