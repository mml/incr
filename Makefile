.PHONY: test

test: test-program
	./test-program

scheme_entry.s: compiler.ss
	racket --script $< > $@

test-program: driver.o scheme_entry.o
	$(CC) -o $@ $^
