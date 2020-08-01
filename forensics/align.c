#include <stdio.h>
#include <stdint.h>
#include <unistd.h>

void print_address(uint16_t x) {
  printf("0x%04x ", x);
  fflush(stdout);

  uint16_t bit = 1 << 15;
  char c;
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      if (bit & x) {
        c = '1';
      } else {
        c = '0';
      }
      write(1, &c, 1);
      bit >>= 1;
    }
    write(1, " ", 1);
  }

  printf("%5u\n", x);
}

int main(int argc, char **argv) {
  uint16_t base = 0xff10;
  print_address(base);
  print_address(10);
  print_address((uint16_t)-8);

  for (int i = 0; i < 8; i++) {
    printf("\n");
    print_address(i);
    print_address(i+10);
    print_address((i+10) & ((uint16_t)-8));
  }
}
