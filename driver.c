#include <stdio.h>
#include <err.h>

#define FIXNUM_MASK 3
#define FIXNUM_TAG 0
#define FIXNUM_SHIFT 2

#define BOOLEAN_MASK 0b10111111
#define BOOLEAN_TAG 0b00101111
#define FALSE_VALUE 0b00101111
#define TRUE_VALUE 0b01101111

#define CHAR_MASK 0b11111111
#define CHAR_TAG 0b00001111
#define CHAR_SHIFT 8

#define NULL_VALUE 0b00111111

extern int scheme_entry();

int main(int argc, char **argv) {
	int val = scheme_entry();

  if (val == NULL_VALUE) {
    printf("()");
	} else if (val == FALSE_VALUE) {
		printf("#f");
	} else if (val == TRUE_VALUE) {
		printf("#t");
	} else if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
		printf("%d", val >> FIXNUM_SHIFT);
  } else if ((val & CHAR_MASK) == CHAR_TAG) {
    char c = val >> CHAR_SHIFT;
    printf("#\\%c", c);
	} else {
		err(1, "Unknown value 0x%04x\n", val);
	}

#ifndef NO_NEWLINE
	printf("\n");
#endif
	return 0;
}
