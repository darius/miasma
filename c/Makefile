CFLAGS = -g

all: calc test

ok: calc
	./calc-test

calc: calc.o 

test: test.o

calc.o: calc.c asm.h
test.o: test.c asm.h


clean:
	rm -f *~ *.o calc test tmp-output

distclean: clean
	rm -f asm.h
