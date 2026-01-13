# Quasiquote/Unquote Implementation Specification

## Overview

Implement quasiquote (backtick), unquote (comma), and unquote-splicing
(comma-at) for the incremental Scheme compiler. This is a **BLOCKER**
for self-compilation - all 18 compiler source files use quasiquote
extensively for code generation.

**Status**: Identified as critical gap #2 in gaps.md

**Scope**: Phase 1 focuses on **list quasiquote only** - single-level
quasiquotes without nesting. Vectors and strings are deferred to
later phases.

## Goals

1. Enable the compiler to compile its own source code that uses
   quasiquote
2. Support standard quasiquote, unquote, and unquote-splicing semantics
3. Integrate cleanly with existing compiler passes
4. Provide correct list construction semantics matching R4RS/R5RS

## Non-Goals (Deferred to Later Phases)

- Nested quasiquotes (`` `(a `(b ,c)) ``)
- Vector quasiquotes (`` `#(a ,b) ``)
- String quasiquotes (if such a thing exists)
- Sophisticated error messages (let standard error handling apply)

## Prerequisites

### Preamble/Standard Library

Quasiquote expansion requires a **preamble system** with internal
helper functions that cannot be shadowed by user code. This must be
implemented first.

**Minimal preamble requirements:**

```scheme
;; Internal append - cannot be shadowed by user (define (lambda ...) ...)
;; Signature: (%append list1 list2 ...) → list
;; Semantics: Concatenate lists left to right
;; Implementation: Standard recursive append
(define (%append x y)
  (if (null? x)
      y
      (cons (car x) (%append (cdr x) y))))

;; Optional helper for building lists from multiple elements
;; Signature: (%list elem ...) → list
;; Semantics: Construct list from variadic arguments
;; Note: May not be needed if we can expand to cons chains directly
(define (%list . args) args)
```

**Why internal %append?**

User code like `(let ([append foo]) `(a ,@b))` would break if
quasiquote expansion used regular `append`. The % prefix denotes
internal compiler infrastructure that exists in a separate namespace.

**Preamble integration:**

- Preamble functions are defined once in a standard library file
- Linked before user program code
- Available to all compiled programs
- See separate preamble spec for full design

## Language Semantics

### Reader Transformation

Racket's reader automatically converts backtick syntax to expanded
forms before the compiler sees the code:

```scheme
;; User writes:        Reader produces:
`(a ,b ,@c)       →   (quasiquote (a (unquote b) (unquote-splicing c)))
`42               →   (quasiquote 42)
`()               →   (quasiquote ())
`(a . ,b)         →   (quasiquote (a . (unquote b)))
```

The compiler processes these expanded forms, not the raw backtick
syntax.

### Expansion Semantics

#### Simple Cases

```scheme
`()               → '()
`42               → '42
`foo              → 'foo
`(a b c)          → '(a b c)
```

#### Unquote (,)

```scheme
`(a ,x b)         → (cons 'a (cons x (cons 'b '())))
`(,x)             → (cons x '())
`(a ,x ,y)        → (cons 'a (cons x (cons y '())))
```

#### Unquote-Splicing (,@)

```scheme
`(a ,@x b)        → (cons 'a (%append x (cons 'b '())))
`(,@x)            → x  ; or (%append x '()) for safety
`(a ,@x ,@y b)    → (cons 'a (%append x y (cons 'b '())))
```

**Runtime requirement**: The value spliced via ,@ must be a list at
runtime. If not, %append will error naturally when trying to traverse
it.

#### Improper Lists (Dotted Tails)

```scheme
`(a . ,x)         → (cons 'a x)
`(a ,b . ,c)      → (cons 'a (cons b c))
`(a ,@x . ,y)     → (cons 'a (%append x y))
```

#### Nested Structure

```scheme
`(a (b ,c) d)     → (cons 'a (cons (cons 'b (cons c '())) (cons 'd '())))
`((,a) (,b))      → (cons (cons a '()) (cons (cons b '()) '()))
```

The Quasiquote function recursively descends into nested lists,
processing unquotes at any depth within the **same** quasiquote level.

#### Multiple Splicing Optimization

When multiple unquote-splicing forms appear consecutively, optimize
by combining append operations:

```scheme
`(a ,@x ,@y b)    → (cons 'a (%append x y (cons 'b '())))
                     ; NOT: (cons 'a (%append x (%append y (cons 'b '()))))
```

This reduces allocation and improves runtime performance.

### Illegal Forms

**Unquote outside quasiquote:**

```scheme
,x               → Error: "unquote not inside quasiquote"
,@x              → Error: "unquote-splicing not inside quasiquote"
```

Detection happens in parse-and-rename when processing expressions.

## Architecture

### Two-Stage Processing

Quasiquote handling occurs in two stages, mirroring how `quote` is
currently handled:

#### Stage 1: Parse-and-Rename (Preserve Structure)

In `s/parse-and-rename.ss`, add a `Quasiquote` function that:

1. Recursively traverses the quasiquote structure
2. **Preserves** most forms unchanged
3. **Processes** unquoted expressions through `Expr`
4. **Preserves** the `(quasiquote ...)` wrapper

```scheme
;; Input:  (quasiquote (a (unquote x) b))
;; Where x is bound to x.1 in env
;; Output: (quasiquote (a (unquote x.1) b))
```

This ensures unquoted expressions get variable renaming, type
checking, and other parse-and-rename transformations, while the
quasiquoted structure itself remains intact for later expansion.

#### Stage 2: Remove-Complex-Constants (Expand to Cons Chains)

In `s/remove-complex-constants.ss`, expand `(quasiquote ...)` forms
to explicit `cons` and `%append` calls:

```scheme
;; Input:  (quasiquote (a (unquote x.1) b))
;; Output: (primcall cons 'a (primcall cons x.1 (primcall cons 'b '())))
```

This happens in the same pass that currently expands `(quote ...)` to
runtime construction for complex constants.

### Pass-Through Behavior

All intermediate passes between parse-and-rename and
remove-complex-constants must **pass through** `(quasiquote ...)`
forms unchanged, similar to how they currently handle `(quote ...)`.

**Passes that need awareness:**

- simplify-conditionals.ss - add case for quasiquote (pass through)
- simplify-binding-forms.ss - add case for quasiquote (pass through)
- remove-memv.ss - add case for quasiquote (pass through)
- make-begin-explicit.ss - add case for quasiquote (pass through)
- uncover-settable.ss - add case for quasiquote (pass through)
- remove-set.ss - add case for quasiquote (pass through)
- uncover-free.ss - add case for quasiquote (pass through)
- collect-code.ss - add case for quasiquote (pass through)
- identify-tail-calls.ss - add case for quasiquote (pass through)

Each pass adds a match clause:

```scheme
[`(quasiquote ,datum) expr]  ; pass through unchanged
```

## Implementation Details

### Stage 1: Parse-and-Rename

Add to `s/parse-and-rename.ss`:

```scheme
;; Quasiquote processing - preserves structure, processes unquotes
(define (Quasiquote datum env)
  (match datum
    ;; Atoms - preserve as-is
    [(? immediate? c) datum]
    [(? symbol? s) datum]

    ;; Unquote - process expression through Expr
    [`(unquote ,expr)
     `(unquote ,(Expr expr env))]

    ;; Unquote-splicing - process expression through Expr
    [`(unquote-splicing ,expr)
     `(unquote-splicing ,(Expr expr env))]

    ;; Proper list - recurse into elements
    [`(,elem* ___)
     (map (lambda (e) (Quasiquote e env)) elem*)]

    ;; Improper list - recurse into both parts
    [`(,car-part . ,cdr-part)
     (cons (Quasiquote car-part env)
           (Quasiquote cdr-part env))]

    ;; Fallback
    [else datum]))

;; Add to Expr function:
(define Expr
  (lambda (expr env)
    (match expr
      ...
      [`(quasiquote ,datum)
       `(quasiquote ,(Quasiquote datum env))]

      ;; Bare unquote/unquote-splicing outside quasiquote
      [`(unquote ,_)
       (error 'parse-and-rename "unquote not inside quasiquote")]
      [`(unquote-splicing ,_)
       (error 'parse-and-rename "unquote-splicing not inside quasiquote")]
      ...)))
```

### Stage 2: Remove-Complex-Constants

Modify `s/remove-complex-constants.ss`:

```scheme
;; Expand quasiquote to cons/%append chains
(define (qq->code datum)
  (match datum
    ;; Empty list
    ['() ''()]

    ;; Atom (immediate or symbol)
    [(? immediate? c) `',c]
    [(? symbol? s) `',s]

    ;; Unquote - return expression directly
    [`(unquote ,expr) expr]

    ;; Unquote-splicing in illegal position (not in list context)
    [`(unquote-splicing ,_)
     (error 'qq->code "unquote-splicing in invalid context")]

    ;; List - process elements, handling splicing
    [`(,elem* ___)
     (qq-list elem*)]

    ;; Improper list (dotted tail)
    [`(,car-part . ,cdr-part)
     (match cdr-part
       [`(unquote ,tail-expr)
        `(primcall cons ,(qq->code car-part) ,tail-expr)]
       [`(unquote-splicing ,tail-expr)
        `(primcall cons ,(qq->code car-part) ,tail-expr)]
       [else
        `(primcall cons ,(qq->code car-part) ,(qq->code cdr-part))])]))

;; Process list elements, handling unquote-splicing
(define (qq-list elems)
  (define (build elems)
    (match elems
      ;; Base case
      ['() ''()]

      ;; Unquote-splicing - append the list
      [`((unquote-splicing ,expr) . ,rest)
       (let ([rest-code (build rest)])
         (if (equal? rest-code ''())
             expr  ; Optimize: ,@x at end
             `(funcall %append ,expr ,rest-code)))]

      ;; Multiple consecutive unquote-splicing - optimize
      [`((unquote-splicing ,e1) (unquote-splicing ,e2) . ,rest)
       (let ([rest-code (build rest)])
         `(funcall %append ,e1 ,e2 ,rest-code))]

      ;; Regular element - cons it
      [`(,elem . ,rest)
       `(primcall cons ,(qq->code elem) ,(build rest))]))

  (build elems))

;; Add to Expr function in remove-complex-constants:
(define (Expr expr)
  (match expr
    ...
    [`(quasiquote ,datum)
     (let ([t (tmp)])
       (values t `((,t ,(qq->code datum)))))]
    ...))
```

### Optimization Notes

**Right-to-left fold**: Build the result from the tail backwards
using cons, which is the standard Scheme expansion pattern.

**Multiple splicing**: When consecutive unquote-splicing forms appear,
collect them and pass to %append together:

```scheme
`(a ,@x ,@y ,@z b) → (cons 'a (%append x y z (cons 'b '())))
```

**Single splice at end**: Optimize `(a ,@x)` to `(cons 'a x)` instead
of `(cons 'a (%append x '()))` when possible.

## Testing Strategy

### Unit Tests

Add to `s/parse-and-rename.ss` module+ test:

```scheme
(module+ test
  (require rackunit)

  ;; Basic quasiquote
  (check-equal? (Expr '(quasiquote (a b c)) primitives)
                '(quasiquote (a b c)))

  ;; Quasiquote with unquote - variable gets renamed
  (check-match (Expr '(quasiquote (a (unquote x)))
                     (cons '(x . x.1) primitives))
               '(quasiquote (a (unquote x.1))))

  ;; Unquote outside quasiquote - should error
  (check-exn exn:fail?
             (lambda () (Expr '(unquote x) primitives)))

  ;; Unquote-splicing outside quasiquote - should error
  (check-exn exn:fail?
             (lambda () (Expr '(unquote-splicing x) primitives))))
```

Add to `s/remove-complex-constants.ss` module+ test:

```scheme
(module+ test
  (require rackunit)

  ;; Empty quasiquote
  (check-equal? (remove-complex-constants '(quasiquote ()))
                ''())

  ;; Atom quasiquote
  (check-equal? (remove-complex-constants '(quasiquote 42))
                ''42)

  ;; Simple unquote
  (check-match (remove-complex-constants '(quasiquote (a (unquote x))))
               `(let ([,t (primcall cons 'a (primcall cons x '()))])
                  ,t))

  ;; Unquote-splicing
  (check-match (remove-complex-constants
                 '(quasiquote (a (unquote-splicing x) b)))
               `(let ([,t (primcall cons 'a
                            (funcall %append x (primcall cons 'b '())))])
                  ,t))

  ;; Multiple splicing
  (check-match (remove-complex-constants
                 '(quasiquote (a (unquote-splicing x)
                                 (unquote-splicing y) b)))
               `(let ([,t (primcall cons 'a
                            (funcall %append x y (primcall cons 'b '())))])
                  ,t))

  ;; Improper list
  (check-match (remove-complex-constants
                 '(quasiquote (a . (unquote x))))
               `(let ([,t (primcall cons 'a x)])
                  ,t)))
```

### Integration Tests

Add to `t/expansion.ss` or create new `t/quasiquote.ss`:

```scheme
(test-cases "quasiquote basics"
  (test-case `() "()")
  (test-case `42 "42")
  (test-case `(a b c) "(a b c)")

  (test-case
    (define x 10)
    `(a ,x b)
    "(a 10 b)")

  (test-case
    (define x 5)
    (define y 10)
    `(,x ,y)
    "(5 10)")

  (test-case
    (define xs '(1 2 3))
    `(a ,@xs b)
    "(a 1 2 3 b)")

  (test-case
    (define xs '(1 2))
    (define ys '(3 4))
    `(a ,@xs ,@ys b)
    "(a 1 2 3 4 b)")

  (test-case
    `(a (b c) d)
    "(a (b c) d)")

  (test-case
    (define x 10)
    `(a (b ,x) c)
    "(a (b 10) c)")

  ;; Improper lists
  (test-case
    (define x 10)
    `(a . ,x)
    "(a . 10)")

  (test-case
    (define x 5)
    (define y 10)
    `(a ,x . ,y)
    "(a 5 . 10)"))
```

## Success Criteria

1. **Parser integration**: Quasiquote forms processed correctly in
   parse-and-rename, variables renamed within unquoted expressions

2. **Expansion correctness**: Quasiquotes expand to cons/%append
   chains that produce correct runtime values

3. **All tests passing**: Unit tests in both passes + integration
   tests all pass on both ARM32 and RISC-V

4. **Preamble integration**: %append available from preamble, works
   correctly at runtime

5. **Self-compilation readiness**: The compiler source files that use
   quasiquote (all 18 files) can be processed through parse-and-rename
   and remove-complex-constants without errors

6. **No regressions**: Existing tests continue to pass

## Implementation Phases

### Phase 1: Preamble Foundation (Prerequisite)

Implement preamble/standard library system with %append. See separate
preamble spec.

**Estimated effort**: Medium (1-2 weeks)

### Phase 2: Parse-and-Rename Integration

Add Quasiquote function and quasiquote case to Expr. Implement
unquote error detection.

**Estimated effort**: Small (2-3 days)
**Validation**: Unit tests in parse-and-rename.ss pass

### Phase 3: Remove-Complex-Constants Expansion

Implement qq->code expansion logic. Handle all cases: atoms, lists,
improper lists, splicing.

**Estimated effort**: Medium (3-5 days)
**Validation**: Unit tests in remove-complex-constants.ss pass

### Phase 4: Pass-Through Updates

Update all intermediate passes to pass quasiquote through unchanged.

**Estimated effort**: Small (1-2 days)
**Validation**: Compiler pipeline doesn't error on quasiquote forms

### Phase 5: Integration Testing

Add comprehensive integration tests. Test on both architectures.

**Estimated effort**: Small (2-3 days)
**Validation**: All integration tests pass

### Phase 6: Self-Compilation Testing

Attempt to compile compiler source files through the new passes.

**Estimated effort**: Small (1-2 days, plus debugging)
**Validation**: No errors processing parse-and-rename.ss and other
compiler files

**Total estimated effort**: 2-3 weeks (after preamble is complete)

## Future Enhancements (Phase 2+)

### Vector Quasiquote

Support `` `#(a ,b ,@c) ``:

```scheme
`#(a ,x)          → (let ([v (make-vector 2 void)])
                      (vector-set! v 0 'a)
                      (vector-set! v 1 x)
                      v)
```

**Complexity**: Medium - needs vector construction patterns

### Nested Quasiquote

Support `` `(a `(b ,c)) ``:

Requires depth tracking and level-aware unquote processing. Each
backtick increases depth, each comma decreases depth. Unquotes only
fire when depth reaches 0.

**Complexity**: High - significant algorithm complexity

### Quasiquote Optimization

Detect compile-time constant quasiquotes and pre-compute them:

```scheme
`(a b c)          → '(a b c)  ; no runtime construction needed
`(a ,x b)         → (cons 'a (cons x (cons 'b '())))  ; still dynamic
```

**Complexity**: Low-Medium - pattern analysis

## Design Rationale

### Why Two-Stage Processing?

Following the existing architecture for `quote` processing:
- Early passes do variable renaming and type checking
- Quasiquoted expressions need variable renaming inside unquotes
- Late expansion keeps early passes simple

Alternative considered: Expand quasiquote immediately in
parse-and-rename. Rejected because unquoted expressions wouldn't get
proper variable renaming.

### Why %append Instead of Primitive?

Using a preamble function instead of compiler primitive:
- **Flexibility**: Can optimize %append implementation independently
- **Simplicity**: Code generators don't need append logic
- **Standard**: Most Scheme implementations use library append
- **Reuse**: %append useful for user code beyond quasiquote

Tradeoff: Requires preamble system (which is needed anyway for
self-compilation per gaps.md).

### Why Right-to-Left Fold?

Building from tail backwards is the standard Scheme expansion:
- Matches cons list construction semantics
- Enables tail optimization in some contexts
- Traditional implementation approach

Alternative considered: Left-to-right build with reverse. Rejected
as less idiomatic.

### Why Defer Vectors and Nesting?

**Vectors**: The compiler source doesn't use vector quasiquotes.
Implementing lists first unblocks self-compilation.

**Nesting**: The compiler source doesn't use nested quasiquotes.
Single-level is sufficient and much simpler (no depth tracking).

Both can be added incrementally after core functionality works.

## Open Questions

None - all design decisions finalized during specification process.

## References

- R4RS Scheme Specification - Section 4.2.6 Quasiquotation
- gaps.md - Self-compilation gap analysis
- CLAUDE.md - Compiler architecture and pass pipeline
- preamble-spec.md - Preamble/standard library design (to be written)

## Appendix: Quasiquote Usage in Compiler Source

The compiler uses quasiquote extensively in code generation. Examples
from `s/parse-and-rename.ss`:

```scheme
`(primcall cons ,(Expr hd env) ,(List tl* env))
`(case ,(Expr expr env) ,@(map (lambda (clause) (Clause clause env)) clause*))
`(letrec* ,(map list ux* e*) ,@(lambda-body body* env))
```

These patterns rely on:
- Basic unquote (`,`)
- Unquote-splicing (`,@`)
- Nested list structure
- No vector quasiquote
- No nested quasiquote (no `` `...`...` ``)

The specification covers all patterns actually used in the compiler
source.
