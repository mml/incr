# Preamble Design Specification

This document specifies a two-phase approach to adding a standard library preamble to the compiler.

## Overview

The preamble is a collection of Scheme procedures defined in a `.ss` source file that gets prepended to every user program before compilation. This allows implementing library functions in Scheme itself rather than as compiler primitives, making them easier to write, test, and iterate on.

**Compilation Model**: The preamble source is prepended to user code and compiled together each time (no pre-compilation).

**Shadowing**: User definitions override preamble definitions via standard lexical scoping. User can redefine any preamble function.

**Architecture**: Single `preamble.ss` file shared across ARM32 and RISC-V architectures.

**Optimization**: Defer inlining and optimization decisions until benchmarking demonstrates need.

---

## Phase 1: Non-Variadic Library (Implement Immediately)

These procedures can be implemented using current compiler features and primitives. They don't require variadic procedure support.

### List Predicates and Accessors

**Length and Indexing:**
```scheme
(define (length lst)
  ; Returns the length of a list by recursively counting elements
  )

(define (list-ref lst n)
  ; Returns the nth element (0-indexed) of a list
  ; Assumes n is in bounds
  )

(define (list-tail lst n)
  ; Returns the tail of lst after dropping n elements
  )

(define (nth lst n)
  ; Alias for list-ref (some Scheme dialects use this name)
  )
```

### List Predicates

**Membership and Search:**
```scheme
(define (member x lst)
  ; Returns the sublist starting with x if found; #f otherwise
  ; Uses equal? semantics
  )

(define (memq x lst)
  ; Like member but uses eq? (pointer equality)
  )

(define (memv x lst)
  ; Like member but uses eqv? (value equality for immediates)
  )

(define (assoc key alist)
  ; Association list lookup: returns (key . value) pair if found; #f otherwise
  ; Uses equal? for key comparison
  )

(define (assq key alist)
  ; Like assoc but uses eq? for key comparison
  )

(define (assv key alist)
  ; Like assoc but uses eqv? for key comparison
  )
```

### Higher-Order Functions

These depend on the language's support for first-class procedures and closures (already implemented).

```scheme
(define (map f lst)
  ; Applies f to each element; returns list of results
  ; (map (lambda (x) (* x 2)) '(1 2 3)) => '(2 4 6)
  )

(define (filter pred lst)
  ; Returns list of elements satisfying predicate
  ; (filter odd? '(1 2 3 4 5)) => '(1 3 5)
  )

(define (foldr f init lst)
  ; Right fold: f is (element accumulator) -> result
  ; (foldr + 0 '(1 2 3 4)) => 10
  )

(define (foldl f init lst)
  ; Left fold: same semantics, different evaluation order
  )

(define (reverse lst)
  ; Reverses a list
  ; Can be implemented as: (foldl (lambda (x acc) (cons x acc)) '() lst)
  )

(define (for-each f lst)
  ; Applies f to each element for side effects; returns void
  )
```

### Composite Accessors

These are trivial combinations of car/cdr:

```scheme
(define (caar lst) (car (car lst)))
(define (cdar lst) (cdr (car lst)))
(define (cadar lst) (car (cdr (car lst))))
(define (caddr lst) (car (cdr (cdr lst))))  ; Already have this as primitive
(define (caddar lst) (car (cdr (cdr (car lst)))))
; ... and other combinations as needed
```

### Type Predicates

These can be implemented by checking type tags:

```scheme
(define (pair? x)
  ; Returns #t if x is a pair
  )

(define (symbol? x)
  ; Returns #t if x is a symbol
  )

(define (number? x)
  ; Returns #t if x is a number (fixnum)
  )

(define (integer? x)
  ; Returns #t if x is an integer (all fixnums are integers)
  )

(define (boolean? x)
  ; Returns #t if x is a boolean
  )

(define (char? x)
  ; Returns #t if x is a character
  )

(define (string? x)
  ; Returns #t if x is a string
  )

(define (procedure? x)
  ; Returns #t if x is a procedure (closure)
  )

(define (vector? x)
  ; Already have as primitive, but included for completeness
  )
```

---

## Phase 2: Variadic Procedures (Requires Language Feature)

These procedures require variadic procedure support (`define (f . args)` syntax), which needs compiler implementation.

### Essential Variadic Operations

**List Construction:**
```scheme
(define (list . elements)
  ; Creates a list from arguments
  ; (list 1 2 3) => '(1 2 3)
  ; Also works as (list) => '()
  )

(define (append . lists)
  ; Concatenates multiple lists
  ; (append '(1 2) '(3 4) '(5)) => '(1 2 3 4 5)
  ; (append) => '()
  )
```

**Extended Arithmetic:**
```scheme
(define (+ . nums)
  ; Adds multiple numbers
  ; (+) => 0
  ; (+ 1) => 1
  ; (+ 1 2 3 4) => 10
  ; Overloads the binary primitive
  )

(define (* . nums)
  ; Multiplies multiple numbers
  ; (*) => 1
  ; (* 2) => 2
  ; (* 2 3 4) => 24
  )

(define (- . nums)
  ; Subtraction: (- x) => -x; (- x y z ...) => x - y - z - ...
  )
```

**Comparison:**
```scheme
(define (= . nums)
  ; Checks if all arguments are equal
  ; (= 3 3 3) => #t
  ; (= 3 3 4) => #f
  )

(define (< . nums)
  ; Checks if arguments are in strictly increasing order
  ; (< 1 2 3) => #t
  ; (< 1 3 2) => #f
  )

(define (> . nums)
  ; Checks if arguments are in strictly decreasing order
  )

(define (<= . nums)
  ; Checks if arguments are in non-decreasing order
  )

(define (>= . nums)
  ; Checks if arguments are in non-increasing order
  )
```

---

## Implementation Approach

### Phase 1 Implementation Steps

1. Create `preamble.ss` with all Phase 1 procedures implemented in Scheme
2. Modify the compiler's pipeline to prepend preamble.ss source before parsing user code
3. Write unit tests for each preamble function in `t/preamble.ss`
4. Verify all existing tests still pass (no shadowing issues)

### Phase 2 Implementation Steps (Deferred)

1. Implement variadic procedure support in compiler:
   - Update parser to recognize `(define (f . args) ...)` syntax
   - Update code generation to handle rest parameters
   - Implement argument collection into a list at runtime

2. Add Phase 2 procedures to preamble.ss

3. Test variadic procedures with preamble functions

---

## Edge Cases and Decisions

### Recursive Definitions

The preamble can contain recursive functions (e.g., `length` calling itself). The compiler's existing closure and tail-call support makes this safe.

### Mutual Recursion

Preamble functions can reference each other. Since the entire preamble is prepended as one unit before compilation, forward references work naturally.

### Primitive Shadowing

If a preamble function has the same name as a built-in primitive (e.g., defining a new `append` or `map`), the preamble version shadows the primitive during that compilation. This is correct behavior per lexical scoping.

### Performance Implications

- **Phase 1**: Most overhead comes from recursive list operations. Tail recursion is optimized.
- **Phase 2**: Variadic procedures will involve runtime list construction, adding some overhead
- Inlining can be added later if profiling shows bottlenecks

### Testing Strategy

Create `t/preamble.ss` that tests preamble functions independently. Example:

```scheme
(test-cases "Preamble functions"
  (test-case (length '(1 2 3)) "3")
  (test-case (length '()) "0")
  (test-case (list-ref '(a b c) 1) "b")
  (test-case (map (lambda (x) (* x 2)) '(1 2 3)) "(2 4 6)")
  ; ... etc
)
```

---

## Success Criteria

**Phase 1 is complete when:**
- All non-variadic procedures are implemented in preamble.ss
- Unit tests pass for all Phase 1 functions
- Existing test suite still passes (no regressions)
- Documentation is updated to list available preamble functions

**Phase 2 is complete when:**
- Variadic procedure syntax is implemented in compiler
- Variadic procedures in preamble work correctly
- Extended arithmetic and comparison work as expected
- Performance is acceptable (or inlining is implemented to improve it)

---

## Notes

- The preamble should NOT include type definitions or macros (not yet supported)
- Preamble procedures should not rely on features in MISSING.md's higher-priority sections (most are there because they're not yet supported)
- As new compiler features are added, the preamble can grow to include more procedures
- Eventually, the preamble could include domain-specific libraries (e.g., for graphics, networking), but this is future work
