WORKDIR = /dev/shm/incr
TARGET = $(WORKDIR)/x-test-program
OBJECT = $(WORKDIR)/x-test-program.o
ASSEMBLY = $(WORKDIR)/x-test-program.s
COMPILE_SCHEME = compiler.ss test-driver.ss

.PHONY: test unit debug dump raco cat edit clean

test: raco $(WORKDIR)
	racket -f tests.ss

unit:
	raco test $(COMPILE_SCHEME) lang/*.ss pass/*.ss

$(WORKDIR):
	mkdir -p $@

raco:
	raco make -v $(COMPILE_SCHEME)

debug: $(TARGET)
	gdb -q $<

dump: $(OBJECT)
	objdump -d $<

cat: $(ASSEMBLY)
	less -j.5 -J $<

edit: $(ASSEMBLY)
	$(EDITOR) $<

clean:
	rm -rf $(WORKDIR) compiled lang/compiled pass/compiled
