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
A basic preamble system exists but is limited to non-variadic procedures.

**Current state (as of 2026-01-09):**
- Basic preamble system in `lib/preamble.ss`
- Preamble definitions are prepended to user code before compilation
- Currently contains: `append`
- See `preamble.md` for design documentation

**What works:**
- Non-variadic procedures can be defined in preamble
- Examples: `append`, `length`, `map`, `filter`, `reverse`, etc.
- Users can call these without defining them

**What's still missing:**
- Variadic procedures cannot be defined in preamble yet
- Examples that need variadic support:
  - `(vector ...)` - variadic vector constructor
  - `(list ...)` - variadic list constructor
  - `(+ a b c ...)` - multi-arg arithmetic
  - `(append lst1 lst2 ...)` - multi-list append

**Blockers for full stdlib:**
- Need variadic procedures (see above) for many stdlib functions
- User-defined variadic procedures still not supported

**Current workarounds:**
- Some procedures like `vector` are transformed in `parse-and-rename.ss`
- Binary `append` is now in preamble
- Variadic operations still require primitives or transformations

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
