#include <stdio.h>
#include <arm_fp16.h>

#define PRINT_SIZEOF(t) printf("sizeof(" #t ") = %3d\n", sizeof(t))

int main(int argc, char **argv) {
  PRINT_SIZEOF(short);
  PRINT_SIZEOF(int);
  PRINT_SIZEOF(long);
  PRINT_SIZEOF(long long);

  // PRINT_SIZEOF(__fp16);
  PRINT_SIZEOF(float);
  PRINT_SIZEOF(double);
}
