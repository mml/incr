MACH=arm32le

.DEFAULT: debug

SRCS = $(wildcard *.c)
TARGETS = $(patsubst %.c,$(MACH)/%.s,$(SRCS))

.PHONY: all
all: $(TARGETS)

$(MACH)/%.s: %.c
	gcc -O3 --omit-frame-pointer -S -o $@ $<
