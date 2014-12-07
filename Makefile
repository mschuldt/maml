all: avm

_opcodes.h: maml_opcodes.py
	./maml_opcodes.py

_prim.c _prim.py: primitives.c process_primitives.el
	./process_primitives.el

avm: avm.c _opcodes.h _prim.c _prim.py
	gcc -std=c99 -g avm.c -o avm

.PHONY: clean

clean:
	rm avm _prim.c _prim.py _opcodes.h


