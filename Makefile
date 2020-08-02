TARGET = x-test-program
OBJECT = x-test-program.o

.PHONY: test debug dump annotest

test:
	racket --script tests.ss

annotest:
	racket --script test-anno.ss

debug: $(TARGET)
	gdb -q $<

dump: $(OBJECT)
	objdump -d $<
