MACH=arm32le

.DEFAULT: debug

SRCS = $(wildcard *.c)
TARGETS = $(patsubst %.c,$(MACH)/%.s,$(SRCS))

.PHONY: all clean test
all: $(TARGETS)

clean:
	rm -f $(TARGETS)

test: scheme_entry.out
	./$<

$(MACH)/%.s: %.c
	gcc -march=armv8-a+simd -mno-sched-prolog -O3 --omit-frame-pointer -S -o $@ $<

scheme_entry.out: arm32le/scheme_entry.s ../driver.c
	gcc -march=armv8-a+simd -mno-sched-prolog -O3 --omit-frame-pointer -o $@ $^
