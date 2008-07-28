all: shoothead.so

ok: calc.py shoothead.so x86.py
	./calc-test

shoothead.so: buildshoot.py shoothead.c
	python buildshoot.py build
	cp build/lib.*/shoothead.so .

clean:
	rm -rf build
	rm -f *~ *.pyc shoothead.so tmp-output
