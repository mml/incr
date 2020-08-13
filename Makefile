MAKEFLAGS += -j
SHMDIR = /dev/shm/incr
WORKDIR = $(SHMDIR)/out
RACKET_COMPILEDIR = $(SHMDIR)/compiled
TARGET = $(WORKDIR)/x-test-program
OBJECT = $(WORKDIR)/x-test-program.o
ASSEMBLY = $(WORKDIR)/x-test-program.s
COMPILE_SCHEME = compiler.ss test-driver.ss
SCHEME_DIRS = lang pass .
scheme_files = $(wildcard $(1)/*.ss)
zo_file = $(dir $(1))compiled/$(patsubst %.ss,%_ss.zo,$(notdir $(1)))
zo_files = $(foreach file,$(call scheme_files,$(1)),$(call zo_file,$(file)))
ZO_FILES = $(foreach dir,lang pass,$(call zo_files,$(dir))) $(call zo_file,compiler.ss) $(call zo_file,test-driver.ss) $(call zo_file,tests.ss)
SCHEME_COMPILE_DIRS = $(foreach dir,$(SCHEME_DIRS),$(dir)/compiled)

.PHONY: test unit debug dump raco cat edit clean realclean zo

test: $(ZO_FILES) | $(WORKDIR)
	racket -f tests.ss

unit: $(call zo_files,lang) $(call zo_files,pass)
	raco test --table --fresh-user --deps -x -j 20 $(COMPILE_SCHEME) lang/*.ss pass/*.ss

debug: $(TARGET)
	gdb -q $<

dump: $(OBJECT)
	objdump -d $<

cat: $(ASSEMBLY)
	less -j.5 -J $<

edit: $(ASSEMBLY)
	$(EDITOR) $<


### Deprecated targets
raco: | $(SCHEME_COMPILE_DIRS)
	raco make -v $(COMPILE_SCHEME)


### A nasty mess showing how to do byte compilation.
zo: $(ZO_FILES)

$(call zo_files,.) : $(call zo_file,%.ss): %.ss | compiled
	raco make -v --no-deps $<

$(call zo_files,lang) : $(call zo_file,lang/%.ss): lang/%.ss | lang/compiled
	raco make -v --no-deps $<

$(call zo_files,pass) : $(call zo_file,pass/%.ss): pass/%.ss | pass/compiled
	raco make -v --no-deps $<


### clean rules
clean:
	rm -f $(ZO_FILES) $(WORKDIR)/*

realclean:
	rm -rf $(SHMDIR) compiled lang/compiled pass/compiled


### directories
$(SHMDIR) $(RACKET_COMPILEDIR) $(WORKDIR):
	mkdir -p $@

.SECONDARY:

$(RACKET_COMPILEDIR)/%:
	mkdir -p $@

compiled: | $(RACKET_COMPILEDIR)/root
	ln -s $< $@

%/compiled: | $(RACKET_COMPILEDIR)/%
	ln -s $< $@

