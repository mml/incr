# Compiler Infrastructure TODOs

## Missing Language Features

### Variadic Procedures
Currently, the compiler does not support user-defined variadic procedures (procedures that accept a variable number of arguments).

**Current state:**
- Variadic primitives exist (`string`, `and`, `or`) but are hard-coded in the compiler
- They're registered in `s/terminals.ss` as `variable-arity-primitives`
- No syntax for defining variadic procedures in user code

**Desired:**
```scheme
(define (foo . args)  ; rest parameter syntax
  ...)

(define (bar x y . rest)  ; fixed + rest parameters
  ...)
```

**Blockers:**
- Parser doesn't recognize rest parameter syntax (`.` in lambda/define)
- No calling convention for passing variable arguments
- No runtime representation for argument lists

### Preamble/Standard Library
The compiler has no preamble - no standard Scheme code that gets automatically compiled and linked with user programs.

**Current state:**
- All functionality must be either:
  - Built-in primitives (hard-coded in compiler)
  - Defined by user in their program

**Desired:**
- A `preamble.ss` or `stdlib.ss` that defines standard procedures
- Examples of what could be moved to preamble:
  - `(vector ...)` - variadic vector constructor
  - `(list ...)` - variadic list constructor
  - `(caar x)`, `(cadr x)`, etc. - car/cdr combinations
  - `(map f lst)` - higher-order functions
  - `(length lst)` - list utilities

**Blockers:**
- No mechanism to compile and link preamble code
- Need variadic procedures (see above) for many stdlib functions
- Need to decide: separate compilation or inline into each program?

**Workarounds:**
- Procedures like `vector` are transformed in `parse-and-rename.ss` into primitives
- This works but is less flexible than having them as library code

## Impact

Without these features, useful procedures must be:
1. Manually defined by users in every program, OR
2. Hard-coded as compiler transformations, OR
3. Hard-coded as primitives with code generation

Example: `(vector a b c)` could be defined as:
```scheme
(define (vector . args)
  (let ([v (make-vector (length args) #f)])
    (let loop ([lst args] [i 0])
      (if (null? lst)
          v
          (begin
            (vector-set! v i (car lst))
            (loop (cdr lst) (+ i 1)))))))
```

But this requires:
- Variadic procedures (the `. args` syntax)
- A preamble to hold this definition
