#include <stdio.h>
extern int scheme_entry();

int main(int argc, char **argv) {
	printf("%d", scheme_entry());
#ifndef NO_NEWLINE
	printf("\n");
#endif
	return 0;
}
