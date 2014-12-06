opcodes: maml_opcodes.py
	python3 maml_opcodes.py

avm: avm.c opcodes
	gcc -std=c99 -g avm.c -o avm
clean:
	rm avm



