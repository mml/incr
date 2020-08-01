MACH=arm32le

.DEFAULT: debug

SRCS = $(wildcard *.c)
RUN_SRCS = sizes.c align.c

OBJ_TARGETS = $(patsubst %.c,$(MACH)/%.s,$(SRCS)) $(patsubst %.c,$(MACH)/%.dump,$(SRCS))
RUN_TARGETS = $(patsubst %.c,$(MACH)/%,$(RUN_SRCS))

TARGETS = $(OBJ_TARGETS) $(RUN_TARGETS)

.PHONY: all run clean test
all: $(TARGETS)

run: $(RUN_TARGETS)
	@for target in $(RUN_TARGETS); do \
		echo ./$$target; \
		./$$target; \
	done

clean:
	rm -f $(TARGETS)

test: scheme_entry.out
	./$<

$(MACH)/%.s: %.c
	gcc -march=armv8-a+simd -mno-sched-prolog -O3 --omit-frame-pointer -S -o $@ $<

$(MACH)/%.o: $(MACH)/%.s
	as -o $@ $<

$(MACH)/%.dump: $(MACH)/%.o
	objdump -D $< > $@

$(MACH)/%: $(MACH)/%.o
	gcc -o $@ $<

scheme_entry.out: arm32le/scheme_entry.s ../driver.c
	gcc -march=armv8-a+simd -mno-sched-prolog -O3 --omit-frame-pointer -o $@ $^
