MACH=arm32le

.DEFAULT: debug

SRCS = $(wildcard *.c)
TARGETS = $(patsubst %.c,$(MACH)/%.s,$(SRCS)) $(patsubst %.c,$(MACH)/%.dump,$(SRCS))

.PHONY: all clean test
all: $(TARGETS)

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

scheme_entry.out: arm32le/scheme_entry.s ../driver.c
	gcc -march=armv8-a+simd -mno-sched-prolog -O3 --omit-frame-pointer -o $@ $^
