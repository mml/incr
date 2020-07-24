#include <stdio.h>
#include <err.h>

#define FIXNUM_MASK 3
#define FIXNUM_TAG 0
#define FIXNUM_SHIFT 2

extern int scheme_entry();

int main(int argc, char **argv) {
	int val = scheme_entry();

	if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
		printf("%d", val >> FIXNUM_SHIFT);
	} else  {
		err(1, "Unknown value 0x%04x\n", val);
	}

#ifndef NO_NEWLINE
	printf("\n");
#endif
	return 0;
}
