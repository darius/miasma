dist: clean
	cd .. && tar cfz miasma.tar.gz miasma

clean:
	(cd src; make clean)
	(cd c; make clean)
	(cd python; make clean)
	rm -f *.o calc test *~ *.bak expanded/*.scm support/*~
