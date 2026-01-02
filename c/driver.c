#include <unistd.h>
#include <stdio.h>
#include <err.h>
#include <sys/mman.h>
#include "machine.h"

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

#define VOID_VALUE 0b00011111

#define NULL_VALUE 0b00111111

#define PAIR_TAG        0b001
#define VECTOR_TAG      0b010
#define STRING_TAG      0b011
#define SYMBOL_TAG      0b100
#define CLOSURE_TAG     0b110
#define PTR_MASK 0b111

#ifndef PTR_T
#define ADDRESS_MASK 0xfffffff8
typedef int ptr_t;
#endif

extern int scheme_entry();
void print_ptr(ptr_t);
void print_cdr(ptr_t);

static char* allocate_protected_space(int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED) {
    err(1, "Can't allocate memory");
  }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) {
    err(1, "Can't protect memory");
  }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) {
    err(1, "Can't protect memory");
  }
  return (p + page);
}

static void deallocate_protected_space(char* p, int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) {
    err(1, "Can't unmap memory");
  }
}

void print_vector(ptr_t *addr) {
  ptr_t size = addr[0];
  printf("#(");
  for (ptr_t i = 0; i < size; i++) {
    if (i != 0 && i != size) {
      printf(" ");
    }
    print_ptr((ptr_t)addr[i+1]);
  }
  printf(")");
}

void print_string(ptr_t *addr) {
  ptr_t size = addr[0];
  char *bytes = (char *) &(addr[1]);
  printf("\"");
  for (ptr_t i = 0; i < size; i++) {
    printf("%c", bytes[i]);
  }
  printf("\"");
}

void print_pair(ptr_t *addr) {
  ptr_t car = addr[0];
  ptr_t cdr = addr[1];

  printf("(");
  print_ptr(car);
  print_cdr(cdr);
  printf(")");
}

void print_symbol(ptr_t *addr) {
  ptr_t str = addr[0];
  printf( "\'");
  print_string((ptr_t *)(str & ADDRESS_MASK));
}

void print_cdr(ptr_t cdr) {
  if (cdr == NULL_VALUE) {
    return;
  } else if (PAIR_TAG == (cdr & PTR_MASK)) {
    ptr_t *addr = (ptr_t *)(cdr & ADDRESS_MASK);
    ptr_t cadr = addr[0];
    ptr_t cddr = addr[1];
    printf(" ");
    print_ptr(cadr);
    print_cdr(cddr);
  } else {
    printf(" . ");
    print_ptr(cdr);
  }
}

void print_ptr(ptr_t val) {
  if (val == NULL_VALUE) {
    printf("()");
  } else if (val == FALSE_VALUE) {
    printf("#f");
  } else if (val == TRUE_VALUE) {
    printf("#t");
  } else if (val == VOID_VALUE) {
    printf("#<void>");
  } else if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
    printf("%d", val >> FIXNUM_SHIFT);
  } else if ((val & CHAR_MASK) == CHAR_TAG) {
    char c = val >> CHAR_SHIFT;
    printf("#\\%c", c);
  } else if ((val & PTR_MASK) == (VECTOR_TAG)) {
    print_vector((ptr_t *)(val & ADDRESS_MASK));
  } else if ((val & PTR_MASK) == PAIR_TAG) {
    print_pair((ptr_t *)(val & ADDRESS_MASK));
  } else if ((val & PTR_MASK) == STRING_TAG) {
    print_string((ptr_t *)(val & ADDRESS_MASK));
  } else if ((val & PTR_MASK) == SYMBOL_TAG) {
    print_symbol((ptr_t *)(val & ADDRESS_MASK));
  } else {
    errx(1, "Unknown value 0x%04x\n", val);
  }
}

int main(int argc, char **argv) {
  int heap_size = (64 * 4096); /* 64K values */
  char *heap_base = allocate_protected_space(heap_size);
  char *heap_top = heap_base + heap_size;

  ptr_t val = scheme_entry(heap_base);
  print_ptr(val);

#ifndef NO_NEWLINE
  printf("\n");
#endif
  deallocate_protected_space(heap_base, heap_size);
  return 0;
}
