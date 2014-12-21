avm: avm.c _opcodes.h _prim.c _prim.py
	g++ -g -w -fpermissive avm.c -o avm
#	g -std=c99 -g  avm.c -o avm

all: avm ino

_opcodes.h: maml_opcodes.py
	./maml_opcodes.py

_prim.c _prim.py: primitives.c arduino_only_primitives.c desktop_only_primitives.c process_primitives.el
	./process_primitives.el


avm.ino: avm.c _opcodes.h _prim.c _prim.py build_ino_file.el maml_HardwareSerial.cpp
	./build_ino_file.el

ino: avm.ino

.PHONY: clean test ☃

test: avm
	./run_tests.sh 
	./run_false_tests.sh

clean::
	rm -f avm _prim.c _prim.py _opcodes.h _entrytable.h avm.ino *.lock

☃:
	echo "snowman!"
snowman:
	echo ☃
