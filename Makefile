all: avm

_opcodes.h: maml_opcodes.py
	./maml_opcodes.py

_prim.c _prim.py: primitives.c process_primitives.el
	./process_primitives.el

avm: avm.c _opcodes.h _prim.c _prim.py
	gcc -std=c99 -g avm.c -o avm

avm.ino: avm.c _opcodes.h _prim.c _prim.py
	./build_ino_file.el

ino: avm.ino

.PHONY: clean

clean:
	rm -f avm _prim.c _prim.py _opcodes.h avm.ino


