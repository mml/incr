TARGET = x-test-program
OBJECT = x-test-program.o

.PHONY: test debug dump

test:
	racket --script tests.ss

debug: $(TARGET)
	gdb -q $<

dump: $(OBJECT)
	objdump -d $<
