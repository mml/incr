#include <stdio.h>

long thingie;
long other;

int main(int argc, char **argv) {
  other = argc;
  thingie = other + argc;
  printf("%l\n", thingie);
}
