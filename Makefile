SHMDIR = /dev/shm/incr
WORKDIR = $(SHMDIR)/out
RACKET_COMPILEDIR = $(SHMDIR)/compiled
TARGET = $(WORKDIR)/x-test-program
OBJECT = $(WORKDIR)/x-test-program.o
ASSEMBLY = $(WORKDIR)/x-test-program.s
COMPILE_SCHEME = compiler.ss test-driver.ss
SCHEME_DIRS = . lang pass
SCHEME_COMPILE_DIRS = $(foreach dir,$(SCHEME_DIRS),$(dir)/compiled)

.PHONY: test unit debug dump raco cat edit clean

test: raco $(WORKDIR)
	racket -f tests.ss

unit:
	raco test --table --fresh-user --deps -x -j 20 $(COMPILE_SCHEME) lang/*.ss pass/*.ss

raco: $(SCHEME_COMPILE_DIRS)
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
	rm -rf $(SHMDIR) compiled lang/compiled pass/compiled

$(SHMDIR) $(RACKET_COMPILEDIR) $(WORKDIR):
	mkdir -p $@

$(RACKET_COMPILEDIR)/%:
	mkdir -p $@

compiled: $(RACKET_COMPILEDIR)/root
	ln -s $< $@

%/compiled: $(RACKET_COMPILEDIR)/%
	ln -s $< $@

