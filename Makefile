WORKDIR = /dev/shm/incr
TARGET = $(WORKDIR)/x-test-program
OBJECT = $(WORKDIR)/x-test-program.o
ASSEMBLY = $(WORKDIR)/x-test-program.s
COMPILE_SCHEME = compiler.ss test-driver.ss annotate-free-variables.ss

.PHONY: test debug dump annotest raco cat

test: raco
	racket -f tests.ss

annotest: raco
	racket -f test-anno.ss

raco:
	raco make -v $(COMPILE_SCHEME)

debug: $(TARGET)
	gdb -q $<

dump: $(OBJECT)
	objdump -d $<

cat: $(ASSEMBLY)
	less -j.5 -J $<
