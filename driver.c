#include <unistd.h>
#include <stdio.h>
#include <err.h>
#include <sys/mman.h>

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

#define VECTOR_TAG 0b010
#define ADDRESS_MASK 0xfffffff8

#define PAIR_TAG 0b001

extern int scheme_entry();
void print_ptr(int);
void print_cdr(int);

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

void print_vector(int *addr) {
  int size = addr[0];
  printf("#(");
  for (int i = 0; i < size; i++) {
    if (i != 0 && i != size) {
      printf(" ");
    }
    print_ptr((int)addr[i+1]);
  }
  printf(")");
}

void print_pair(int *addr) {
  int car = addr[0];
  int cdr = addr[1];

  printf("(");
  print_ptr(car);
  print_cdr(cdr);
  printf(")");
}

void print_cdr(int cdr) {
  if (cdr == NULL_VALUE) {
    return;
  } else if (cdr & PAIR_TAG) {
    int *addr = (int *)(cdr & ADDRESS_MASK);
    int cadr = addr[0];
    int cddr = addr[1];
    printf(" ");
    print_ptr(cadr);
    print_cdr(cddr);
  } else {
    printf(" . ");
    print_ptr(cdr);
  }
}

void print_ptr(int val) {
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
  } else if (val & VECTOR_TAG) {
    print_vector((int *)(val & ADDRESS_MASK));
  } else if (val & PAIR_TAG) {
    print_pair((int *)(val & ADDRESS_MASK));
	} else {
		errx(1, "Unknown value 0x%04x\n", val);
	}
}

int main(int argc, char **argv) {
  int heap_size = (16 * 4096); /* 16K values */
  char *heap_base = allocate_protected_space(heap_size);
  char *heap_top = heap_base + heap_size;

	int val = scheme_entry(heap_base);
  print_ptr(val);

#ifndef NO_NEWLINE
	printf("\n");
#endif
  deallocate_protected_space(heap_base, heap_size);
	return 0;
}
