#include <stdio.h>

extern long someone_elses_thingie;

long thingie;
long other;

static long my_thingie;

void foo(int);

int main(int argc, char **argv) {
  other = argc;
  thingie = other + argc;
  printf("%l\n", thingie);
  foo(argc);
  printf("%l\n", my_thingie);
}

void foo(int argc) {
  my_thingie = argc * argc;
}
