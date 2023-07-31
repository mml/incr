#include <stdio.h>

union word {
	unsigned long long int i;
	unsigned char c[8];
};

int main(void) {
	union word w;
	w.i = 0x0102030405060708ull;
	if (w.c[0] == 8)
		printf("Little-endian.\n");
	if (w.c[0] == 1)
		printf("Big-endian.\n");
	return 0;
}
