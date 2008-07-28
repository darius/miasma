EXES = trivial.o hello.o multi.o combo.o

all: $(EXES)

test: all
	./compare-each trivial hello multi combo

smalltest: trivial.o hello.o multi.o
	./compare-each trivial hello multi

clean: 
	rm -f $(EXES) *~ stack_depth_profile


trivial.o: trivial.s
	./gas2binary trivial

hello.o: hello.s
	./gas2binary hello

multi.o: multi.s
	./gas2binary multi

combo.o: combo.s
	./gas2binary combo
