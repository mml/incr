# Define Implementation Design Specification

This document specifies how to implement `(define)` in the compiler.

## Overview

The `define` special form binds names to values. There are two distinct
contexts:

- **Internal definitions**: Inside other forms (let, lambda, begin, etc.).
  Transformed to `letrec*` for mutual recursion support.
- **Top-level definitions**: At the program level. Collected and wrapped
  in a single `letrec*` that binds all definitions and evaluates the
  final expression.

**Key design decisions**:
- Definitions must come before body expressions (strict order).
- Internal definitions capture their containing environment (closure
  semantics via `letrec*`).
- Function definitions `(define (f x y) body)` are syntactic sugar for
  `(define f (lambda (x y) body))`.
- Evaluation is immediate (standard Scheme semantics).
- Mutual recursion works naturally via `letrec*` scope.
- Allow shadowing: second definition of same name shadows first.

---

## Syntax Rules

### Variable Definition

```scheme
(define name value-expression)
```

**Semantics**: Binds `name` to the result of evaluating
`value-expression`. The variable is available in all following
expressions in the same scope.

**Immediate evaluation**: `value-expression` is evaluated when the
define is processed, not lazily.

### Function Definition (Syntactic Sugar)

```scheme
(define (name param1 param2 ...) body ...)
```

**Desugaring**: This is syntax sugar for:

```scheme
(define name (lambda (param1 param2 ...) body ...))
```

**Recursion**: The function can call itself; the name is in scope
within the lambda body due to `letrec*` semantics.

**Multiple body expressions**: `(define (f x) e1 e2 e3)` desugars to
`(define f (lambda (x) (begin e1 e2 e3)))`.

### Malformed Definitions

Invalid forms that should be rejected (with any crappy error message):
- `(define)` - no name or value
- `(define x)` - no value
- `(define x y z)` - extra arguments
- `(define (f . args) body)` - variadic functions not yet supported
- `(define (f x x) body)` - duplicate parameter names (optional check)

---

## Internal Definitions

### Transformation Pipeline

Internal definitions are those appearing inside a containing form like
`let`, `lambda`, or `begin`.

**Two-phase transformation**:

**Phase 1: Desugar function syntax**

Transform `(define (f x) body)` to `(define f (lambda (x) body))`.

```scheme
; Before
(let ()
  (define (add2 x) (add1 (add1 x)))
  (define (add1 x) (+ 1 x))
  (add2 5))

; After phase 1
(let ()
  (define add2 (lambda (x) (add1 (add1 x))))
  (define add1 (lambda (x) (+ 1 x)))
  (add2 5))
```

**Phase 2: Extract definitions and wrap in letrec***

Collect all leading definitions before the first body expression, then
wrap them in a `letrec*`:

```scheme
; After phase 2
(let ()
  (letrec* ([add2 (lambda (x) (add1 (add1 x)))]
            [add1 (lambda (x) (+ 1 x))])
    (add2 5)))
```

The `letrec*` form naturally allows mutual recursion because all
bindings are in the same scope.

### Requirements from letrec*

For internal definitions to work, the compiler must:
1. Already support `letrec*` (check if this is implemented)
2. Support extracting trailing body expression(s) after definitions

If `letrec*` is not implemented, the compiler must implement it. Key
feature: `letrec*` evaluates bindings in order, allowing each binding
to reference previous bindings. Example:

```scheme
(letrec* ([a 1]
          [b (+ a 1)])  ; can reference 'a'
  b)
; => 2
```

### Edge Cases

**No body expression**:
```scheme
(let ()
  (define x 5))
```
After transformation, the body is implicitly `void` or the value of the
last definition. Behavior: undefined (error or returns unspecified
value). Compiler can choose either.

**Mixed definitions and expressions** (strictly disallowed):
```scheme
(let ()
  (define x 5)
  (+ x 1)
  (define y 10)  ; ERROR: definition after body expression
  y)
```
The compiler should reject this during parsing/transformation.

**Shadowing of surrounding bindings**:
```scheme
(let ([x 10])
  (letrec* ([x 20])  ; shadows outer 'x'
    x))
; => 20
```
This is standard scoping; no special handling needed.

**Mutual recursion**:
```scheme
(letrec* ([even? (lambda (n) (or (zero? n) (odd? (- n 1))))]
          [odd?  (lambda (n) (and (positive? n) (even? (- n 1))))])
  (even? 4))
; => #t
```
The `letrec*` binding order matters: earlier bindings are available to
later ones, and all bindings can reference all other bindings (they're
all in scope during their own evaluation, via closure).

---

## Top-Level Definitions

### Compilation Model

The current compiler accepts a single expression. To support top-level
definitions, the entire program is wrapped in a `letrec*`.

### Transformation Pipeline

**Phase 1: Desugar function syntax** (same as internal definitions)

Transform `(define (f x) body)` to `(define f (lambda (x) body))`.

**Phase 2: Wrap in letrec***

Collect ALL definitions in the program. Find the final body expression
(last non-definition). Wrap everything:

```scheme
; Original program
(define x 9)
(define (make-adder n) (lambda (m) (+ n m)))
(define add-x (make-adder x))
(add-x 12)

; After wrapping
(letrec* ([x 9]
          [make-adder (lambda (n) (lambda (m) (+ n m)))]
          [add-x (make-adder x)])
  (add-x 12))
```

**Key detail**: The last expression must be a body expression (not a
definition). If the program contains only definitions and no final
expression, this is a compile error (or the implicit body is `void`).

### Interaction with Parser

The parser needs to be extended to recognize that a program can start
with `(define ...)` forms. Currently, the parser expects a single
expression.

**Approach**:
1. Parser recognizes top-level `define` forms
2. Parser collects all leading defines
3. Parser consumes a final expression (the body)
4. Compiler transforms this to the `letrec*` form above before further
   processing

Alternatively, preprocessing could happen before parsing:
- Scan the raw token stream
- Extract all `define` forms
- Wrap remainder in `(letrec* (...) (begin ...))` syntax
- Pass transformed program to existing parser

### Requirements from letrec*

Same as internal definitions: `letrec*` must be implemented.

### Edge Cases

**Only definitions, no body**:
```scheme
(define x 5)
(define y 10)
```
This should either error ("no final expression") or implicitly return
`void`. Compiler choice.

**Definitions in the middle** (strictly disallowed):
```scheme
(define x 5)
(+ x 1)
(define y 10)  ; ERROR
y
```
Parser should reject this: definitions must all come first.

**Duplicate definitions** (allowed, shadowing):
```scheme
(define x 10)
(define x 20)
x
; => 20
```
Second binding shadows first. This is handled naturally by `letrec*`
with the same variable name appearing twice.

---

## Implementation Approach

### Parser Changes Required

The parser (currently in `s/parse-and-rename.ss` or similar) needs to:

1. Recognize `(define name expr)` as a special form (not a procedure
   call)
2. Recognize `(define (name params...) body...)` as a special form
3. At the **top-level only**: collect all `define` forms, extract
   trailing body expression(s), and build the `letrec*` wrapper
4. At the **internal level**: when inside another form that already
   handles definitions (like `let`), process defines as described below

### Compiler Pass Changes

**Pass: parse-and-rename**

This pass should:
1. Desugar `(define (f x) body)` to `(define f (lambda (x) body))`
   - Add helper function: `(desugar-define expr env)`
   - If `expr` is `(define (name . params) . body)`, transform to
     `(define name (lambda params body-as-begin))`
2. Keep `(define name value)` as-is (a special form in the IR)

Output IR may look like:
```scheme
(define name value-expr)    ; remains as Define record in IR
```

**Pass: simplify-binding-forms or new pass**

This pass should:
1. When visiting a `let`, `lambda`, `begin`, etc. that contains
   definitions:
   - Collect all leading `(define name value)` forms
   - Extract the remaining body expression(s)
   - Transform to `(letrec* ((name value) ...) body)`
2. At top-level (when there are no outer forms):
   - Collect all top-level defines
   - Wrap with `letrec*`
   - Pass the `letrec*` to code generation

Or, a dedicated pass "extract-defines" that:
- Traverses the IR
- Finds all context-local define forms
- Transforms them to letrec*
- Removes define forms from the IR (only letrec* remains)

### Key Data Structures

In the IR, represent `define` as a record:

```scheme
(define-record-type define
  (fields name value))
```

Or integrate into the existing expression types.

### Testing Strategy

Create `t/define.ss` with test cases:

**Internal definitions** (must work with `letrec*`):
```scheme
(let ()
  (define (add2 x) (add1 (add1 x)))
  (define (add1 x) (+ 1 x))
  (add2 5))
; => 7

(let ([x 10])
  (define y (+ x 5))
  y)
; => 15

; Mutual recursion
(letrec* ([even? (lambda (n) (or (zero? n) (odd? (- n 1))))]
          [odd?  (lambda (n) (and (positive? n) (even? (- n 1))))])
  (even? 4))
; => #t
```

**Top-level definitions**:
```scheme
(define x 9)
(define (make-adder n) (lambda (m) (+ n m)))
(define add-x (make-adder x))
(add-x 12)
; => 21

(define (fib n)
  (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(fib 6)
; => 8
```

**Edge cases**:
```scheme
; Function recursion
(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))
(fact 5)
; => 120

; Shadowing
(define x 10)
(define x 20)
x
; => 20

; Multiple body expressions in define
(define (f x) (display x) (+ x 1))
(f 5)
; outputs: 5, returns 6
```

### Debugging Notes

When testing, remember:
- If `letrec*` is not yet implemented, these tests will fail with
  "undefined form: letrec*"
- The transformation happens during parsing/compilation, so errors may
  reference `letrec*` even if the source code has `define`
- Variable renaming in `parse-and-rename` must handle define forms
  correctly (rename the bound variable, not the name being defined)

---

## Success Criteria

**Define support is complete when:**

1. Both `(define name value)` and `(define (name params) body)` syntax
   are accepted by the parser
2. Function syntax sugar is correctly desugared to lambdas
3. Internal definitions (in let/lambda/begin) are correctly extracted
   and wrapped in letrec*
4. Top-level definitions are collected and wrapped in a single letrec*
5. Mutual recursion works correctly (even?, odd? example)
6. Definitions may shadow surrounding bindings or previous definitions
7. All test cases in `t/define.ss` pass on both ARM32 and RISC-V
8. Existing tests continue to pass (no regressions)

---

## Notes and Open Questions

- **Dependency**: `letrec*` must be implemented first. Verify this is
  available before beginning define implementation.
- **Variadic parameters**: `(define (f . args) body)` requires variadic
  procedure support (not yet in compiler). Initial implementation should
  reject this with an error.
- **Duplicate parameters**: `(define (f x x) body)` - should this error?
  (Optional strictness check)
- **Define order in errors**: When a define references an undefined
  variable, the error message should point to the define location, not
  the letrec* wrapper.
- **Top-level body requirement**: Must the program have a final
  expression? Current decision: yes (error if only definitions). But
  could also implicitly return `void`.
