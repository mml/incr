# Incremental Scheme Compiler Documentation

This is an incremental Scheme-to-native compiler targeting ARM32
and RISC-V 64-bit architectures. The compiler is written in Racket
and generates assembly code that links with a small C runtime.

## Table of Contents

- [Project Structure](#project-structure)
  - [Directory Layout](#directory-layout)
  - [Work Directives](#work-directives)
  - [Build Workflow](#build-workflow)
- [Compiler Architecture](#compiler-architecture)
  - [Pass Pipeline](#pass-pipeline)
  - [Internal Defines Implementation](#internal-defines-implementation)
  - [Compiler Pass Transformations](#compiler-pass-transformations)
- [Data Representation](#data-representation)
  - [Immediate Values](#immediate-values-unboxed)
  - [Heap Objects](#heap-objects-boxed)
- [Test Infrastructure](#test-infrastructure)
- [Extending the Compiler](#extending-the-compiler)
  - [Adding New Primitives](#adding-new-primitives-to-code-generators)
  - [Adding New Transformations](#adding-new-transformations)
  - [Architecture Quick Reference](#architecture-quick-reference)
  - [Code Generation Patterns](#code-generation-patterns)
- [Debugging](#debugging-test-failures)
- [Design Decisions](#design-decisions)
- [Common Gotchas and Limitations](#common-gotchas-and-limitations)
- [RISC-V Code Generation](#risc-v-code-generation)

## Project Structure

### Directory Layout

```
incr/
├── TODO.md              # Infrastructure gaps (variadic procedures, preamble)
├── s/                    # Compiler source (Racket/Scheme)
│   ├── *.ss             # Compiler passes
│   ├── arm32le.def      # ARM32 code generator
│   ├── rv64le.def       # RISC-V code generator
│   └── compile-shared.ss # Shared constants and utilities
├── c/                    # C runtime
│   ├── driver.c         # Print functions, memory allocation
│   ├── Mf-base         # Shared C makefile
│   └── Mf-arm32le      # ARM32-specific C makefile
├── t/                    # Test suite
│   ├── *.ss            # Test files
│   └── TODO.md         # Test status documentation
├── arm32le/             # ARM32 workarea (created by ./configure)
│   ├── s/              # Symlinks to s/ + machine.ss
│   ├── c/              # Compiled C runtime
│   └── t/              # Symlinks to t/ + test outputs
├── rv64le/              # RISC-V workarea (created by ./configure)
└── configure            # Script to set up workareas
```

### Work Directives

- Always check for balanced parentheses after creating or
  modifying files
- The codebase prefers square brackets in certain contexts:
  - Binding forms:
    `(let ([x 5] [y 2]) ...)`
  - Conditionals:
    `(cond [(even? x) #t] [else 999])`
  - Case statements:
    `(case foo [(a b c) 'letter] [(1 2 3) 'number])`
  - case-lambda, match patterns
  - Emulate this style when adding new code

### Documentation References

**URL Aliases:** For future reference, use these short names:
- `objects.html` → https://www.scheme.com/tspl4/objects.html

### Build Workflow

**Initial setup:**
```bash
./configure --machine=arm32le -x    # or rv64le
```
Creates architecture workarea with `machine.ss` defining architecture.

**Build and test:**
- Run all build commands from the project root
- `make -C arm32le/s unit` - run unit tests (only need on one
  arch)
- `make -C ${machine}/t test` - run integration tests
- After changes in `s/`, run unit tests before integration
  tests
- `./test-parallel` - run integration tests in parallel on both
  architectures (see **Parallel Testing** below)
- ARM32le runs faster than RISC-V
- For specific test:
  `make -C arm32le/t catchall`
- After a few hours idle: `make realclean` in both architectures
  to ensure fresh compilation

**Parallel Testing:**

The `test-parallel` script runs integration tests on ARM32 and
RISC-V in parallel with 10-minute timeouts. This is the primary
way to validate changes across both architectures simultaneously.

Usage:
```bash
# Run all integration tests on both architectures in parallel
./test-parallel

# Run specific tests on both architectures in parallel
./test-parallel binprims numprims ratnum
./test-parallel fxop
```

When to use:
- **After compiler passes changes:** Run `./test-parallel` to
  ensure full test suite passes on both architectures
- **After code generator changes:** Run `./test-parallel` to
  validate architecture-specific implementations
- **During iterative development:** Use `make -C arm32le/t
  specific-test` to test one architecture quickly, then
  `./test-parallel specific-test` to validate both architectures
- **Performance verification:** Compares ARM32 vs RISC-V
  execution time (ARM32 typically faster due to lower overhead)

**Important:** The Makefile dependency `zo: src` ensures
symlinks are created before compiling. Without this,
`make` fails on fresh workareas.

**Testing preamble.ss for syntax errors:** Unit tests
(`make -C arm32le/s unit`) do not load preamble.ss. To test
preamble changes for syntax errors, run an integration test that
loads it. Use a simple test file like:
`make -C arm32le/t literals` - tests basic literal expressions
and will fail fast if preamble.ss has syntax errors.

## Compiler Architecture

### Pass Pipeline

The compiler transforms Scheme code through these passes in order:

1. **normalize-program** - Normalize expression lists to single expression
   - Single: `(e)` → `e` (unwrap)
   - Multiple: `(e1 e2 ...)` → `(let () e1 e2 ...)` (wrap)
2. **parse-and-rename** - Parse syntax and rename variables to unique names
3. **simplify-conditionals** - Normalize if/cond/case to simpler forms
4. **simplify-binding-forms** - Convert let*/letrec/letrec* to let + set!
5. **remove-complex-constants** - Transform quoted data into runtime construction:
   - `'a` → `(string->symbol (string #\a))`
   - `"hi"` → `(string #\h #\i)`
   - `'(a . b)` → `(cons ... ...)`
6. **remove-memv** - Transform memv into primitive operations
7. **make-begin-explicit** - Convert implicit sequences to explicit begin
8. **uncover-settable** - Identify variables that need heap allocation
9. **remove-set!** - Transform set! into heap-allocated boxes
10. **uncover-free** - Find free variables for closure conversion
11. **collect-code** - Separate top-level functions from main code
12. **identify-tail-calls** - Mark tail positions for optimization
13. **Code generation** - `arm32le.def` or `rv64le.def` emits assembly

**Key debugging insight:** Error messages reference transformed
code, not original source. A test with `'a` will show
`string->symbol` errors because the pass pipeline transforms
it before code generation.

### Internal Defines Implementation

Internal `define` forms in lambda/let/let*/letrec/letrec*
bodies are transformed to `letrec*` by `parse-and-rename`:

**Implementation in `s/parse-and-rename.ss`:**

1. **`desugar-define-forms`** - Converts function syntax to
   lambda:
   ```scheme
   (define (f x) body) → (define f (lambda (x) body))
   ```

2. **`lambda-body`** - Processes body expressions in binding
   forms:
   ```scheme
   (define (lambda-body expr* env)
     (let ([desugared (desugar-define-forms expr*)])
       (match desugared
         [`((define ,k* ,x*) __1 ,body* ___)
           (list (Letrec* k* x* body* env))]
         [`(,body* __1)
           (Expr* body* env)])))
   ```

3. **Architectural rationale:** Variable renaming requires body
   expressions be lexically nested within the scope that
   introduces bindings. Using `letrec*` ensures:
   - Unique names created when entering the letrec* scope
   - Body processed in extended environment
   - Mutual recursion works (all names in scope for all
     definitions)

**Example transformation:**
```scheme
;; Input
(lambda (x)
  (define (helper y) (* 2 y))
  (helper (+ x 1)))

;; After parse-and-rename
(lambda (x.1)
  (letrec* ([helper.2 (lambda (y.3) (* 2 y.3))])
    (helper.2 (+ x.1 1))))
```

### Compiler Pass Transformations

#### Special Forms in `parse-and-rename.ss`

| Source Code | Transformation |
|-------------|----------------|
| `(list a b c)` | `(cons a (cons b (cons c '())))` |
| `(vector a b c)` | `(let ([v (make-vector 3 void)])` |
| | `(vector-set! v 0 a)` |
| | `(vector-set! v 1 b)` |
| | `(vector-set! v 2 c) v)` |

Implemented via dedicated helper functions (`List`, `Vector`)
that recursively build transformed IR.

#### Constants in `remove-complex-constants.ss`

| Source Code | After Transformation |
|-------------|---------------------|
| `'a` | `(string->symbol (string #\a))` |
| `'foo` | `(string->symbol (string #\f #\o #\o))` |
| `"hi"` | `(string #\h #\i)` |
| `'(a . b)` | `(cons (string->symbol ...)` |
| | `(string->symbol ...))` |
| `'()` | Unchanged (immediate null value) |

**When to use transformations vs primitives:**
- Transformations: Express operation using existing primitives
  (works automatically on all architectures)
- Primitives: Need new runtime behavior or performance is
  critical

## Data Representation

The runtime uses a tagged pointer scheme to distinguish types.

### Immediate Values (unboxed)
Fit in a machine word, no heap allocation needed:

| Type | Tag (low bits) | Example | Representation |
|------|----------------|---------|----------------|
| Fixnum | `00` | `5` | `0b10100` (value << 2) |
| Boolean | `0b00101111` / `0b01101111` | `#f` / `#t` | Special bit patterns |
| Character | `0b00001111` | `#\a` | `0b011000010001111` (char << 8, tag) |
| Void | `0b00011111` | `(void)` | Fixed value |
| Null | `0b00111111` | `'()` | Fixed value |

### Heap Objects (boxed)
Allocated on heap, pointer tagged with low 3 bits:

| Type | Tag | Layout |
|------|-----|--------|
| Pair | `#b001` | `[car\|cdr]` (2 words) |
| Vector | `#b010` | `[size\|elem0\|elem1\|...]` (size+1 words) |
| String | `#b011` | `[size\|byte0\|byte1\|...]` (size in bytes, 8-byte aligned) |
| Symbol | `#b111` | `[string-ptr]` (1 word, points to string) |
| Ratnum | `#b101` | `[numerator\|denominator]` (2 words) |
| Closure | `#b110` | `[code-ptr\|free0\|free1\|...]` |

**Memory alignment:** All heap allocations are 8-byte aligned.
The low 3 bits of aligned pointers are always 0, enabling use
for type tags.

**Tag value constraints:** Pointer tag values must be chosen
carefully to avoid conflicts with immediate value checks:
- Fixnum check uses low 2 bits (`val & 0b11 == 0b00`)
- This check happens BEFORE pointer tag checks in `print_ptr`
- Therefore, pointer tags with low 2 bits = `00` (like `#b100`)
  would be misinterpreted as fixnums
- Available safe pointer tags: `#b001`, `#b010`, `#b011`,
  `#b101`, `#b110`, `#b111`
- Tag `#b100` is UNSAFE and must not be used for pointers

**Heap pointer:** Maintained in r8 (ARM32) or s11 (RISC-V),
points to next free byte.

**String layout detail:**
- Header: 1 word (4 bytes ARM32, 8 bytes RISC-V) containing
  byte count
- Data: N bytes of UTF-8 character data (1 byte per ASCII
  character)
- Padding: To reach next 8-byte boundary
- Example: "hi" =
  `[2][h][i][5 bytes padding]` (8 bytes total)

## Test Infrastructure

### Writing Tests

Tests use the `test-case` macro which accepts **variable
arguments**:

```scheme
(test-case expr1 expr2 ... expected-output)
```

All arguments except the last are expressions to execute;
the last is the expected output string.

**Examples:**

```scheme
;; Single expression
(test-case (+ 1 2) "3")

;; Multiple expressions (wrapped in let by normalize-program)
(test-case
  (define x 5)
  (+ x 10)
  "15")

;; Function definition and call
(test-case
  (define (double x) (* 2 x))
  (double 21)
  "42")
```

### Test Organization

Tests are organized in test files under `t/`:
- Each test file is a Racket module providing `runtests`
- `test-cases` groups related tests with a description
- Add new test files to `tests` variable in `t/Mf-base`

**Running tests:**
- `make -C arm32le/t testname` - run specific test
- `make -C arm32le/t test` - run all tests for that
  architecture
- `./test-parallel` - run all tests on both architectures in
  parallel

## Extending the Compiler

### Adding New Primitives to Code Generators

Code generators (`s/arm32le.def` and `s/rv64le.def`) follow a
consistent pattern.

**Step 1: Add to primcall dispatcher**

In `emit-primitive-call`, add primitive to appropriate
category:

```scheme
[(cons make-vector string string->symbol)
 (emit-allocation-primcall op expr si env)]
```

Categories:
- **Allocation primcalls**: Allocate heap objects (cons,
  make-vector, string, string->symbol)
- **Side-effect primcalls**: Mutate objects (vector-set!,
  set-car!)
- **Inline primcalls**: Direct code in case statement (add1,
  sub1, +, -, <, etc.)

**Step 2: Implement the primitive**

For allocation primcalls, add case to
`emit-allocation-primcall`:

```scheme
[(string->symbol)
 (emit-expr (primcall-operand1 expr) si env)
 (emit "  str r0, [~a]" heap-register)
 (emit "  add r0,~a,#~a" heap-register
       (constant symbol-tag))
 (emit "  add ~a,~a,#8" heap-register
       heap-register)]
```

**Step 3: Implement for both architectures**

**CRITICAL**: Primitives must be implemented in both
`arm32le.def` AND `rv64le.def`. Always test on both
architectures.

### Adding New Transformations

To add a compiler transformation in `parse-and-rename.ss`:

1. Add case in `Expr` function:
   ```scheme
   [`(myop ,expr* ___)
     (Myop expr* env)]
   ```

2. Add helper function:
   ```scheme
   (define (Myop expr* env)
     ; Transform expr* into primcalls using existing
     ; primitives
     ...)
   ```

### Compiler Pass Design Patterns

Successful passes in the compiler follow consistent patterns for clarity
and maintainability:

**Naming conventions:**
- Top-level entry point: typically `Expr` (e.g., `(define (Expr expr env) ...)`)
- Helper transformers: named after the AST node they process:
  - `Expr` - transforms expressions
  - `Expr*` - transforms expression lists
  - `Code` - transforms code blocks
  - `Clause*` - transforms clause lists
  - Use capitalized names for transformers (e.g., `List`, `Vector`)

**Single-pass transformation with multiple values:**

Passes should transform the entire tree in one pass, returning both the
transformed expression and accumulated information using Scheme's
multiple-value construct:

```scheme
(define (pass-entry expr)
  (let-values ([(transformed env-or-info) (Expr expr (initial-env))])
    transformed))

(define (Expr expr env)
  (match expr
    [`(quote ,c) (values expr (set))]
    [`(let ([,x* ,e*] ___) ,body)
     (let-values ([(e* free*) (Expr* e*)])
       (let-values ([(body free) (Expr body)])
         (values `(let ,(map list x* e*) ,body)
                 (set-union* (cons free free*)))))]))

(define (Expr* expr*)
  (let loop ([expr* expr*] [rexpr* '()] [rinfo* '()])
    (if (null? expr*)
      (values (reverse rexpr*) (reverse rinfo*))
      (let-values ([(rexpr rinfo) (Expr (car expr*))])
        (loop (cdr expr*) (cons rexpr rexpr*) (cons rinfo rinfo*))))))
```

**Benefits of this pattern:**
- Single pass through the tree (efficient)
- No mutable state with `set!` (functional, easier to reason about)
- Environment/information passed as parameters (clear data flow)
- Information accumulated on return (like `uncover-free.ss` returns free
  variable sets, `parse-and-rename.ss` returns transformed expressions)

**Anti-pattern to avoid:**
Multiple passes with mutable state:
```scheme
; BAD: Two passes + mutable state
(define (bad-pass expr)
  (let ([collected '()])
    (define (collect-walk expr)
      (set! collected (cons datum collected))  ; Mutation!
      ...)
    (collect-walk expr)  ; First pass
    (replace-walk expr)  ; Second pass
    ...))
```

**Real examples:**
- `uncover-free.ss` - Single pass returning transformed expr + free variable
  sets
- `parse-and-rename.ss` - Single pass with environment threading
- `collect-code.ss` - Single pass with multiple-value returns for (expr,
  labels)

### Idiomatic Racket Patterns for Compiler Passes

These patterns make passes more concise and idiomatic:

**1. The `=>` idiom for association list lookups**

Instead of storing the result and checking it:
```scheme
; AVOID: Double lookup or unnecessary let binding
(let ([entry (env-lookup e datum)])
  (if entry
    (use (cdr entry))
    (add-new)))
```

Use the `=>` idiom to pass the truthy result directly:
```scheme
; PREFER: Single lookup, direct use
(cond
  [(env-lookup e datum)
   => (lambda (entry)
        (use (cdr entry)))]
  [else (add-new)])
```

The `=>` passes the truthy result (the assoc pair) to the lambda, avoiding a second lookup.

**2. Extract common lookup-or-add patterns**

When you frequently need "lookup existing or create new", wrap it in a helper:
```scheme
(define (lookup-or-add env key)
  (cond
    [(env-lookup env key)
     => (lambda (entry) (values env (cdr entry)))]
    [else
     (env-add env key)]))
```

This centralizes the pattern and makes call sites clearer.

**3. Unconditional `begin` with splicing for optional lists**

Instead of conditionally wrapping with `begin`:
```scheme
; AVOID: Conditional begin wrapper
(if (null? init-code)
  body
  `(begin ,@init-code ,body))
```

Use unconditional `begin` - splicing handles the empty case:
```scheme
; PREFER: Let splicing handle it
`(begin ,@init-code ,body)
```

When `init-code` is empty, `(begin ,@'() ,body)` splices to `(begin ,body)`, which is semantically equivalent to `body` in most contexts.

**4. Pattern guards over body conditionals**

Move predicate checks into match patterns when possible:
```scheme
; AVOID: Conditional in match body
[`(quote ,datum)
 (if (complex-datum? datum)
   (intern-it datum)
   (values expr e))]

; PREFER: Pattern guard
[`(quote ,(? complex-datum? datum))
 (intern-it datum)]
```

Pattern guards fail the match early if the predicate fails, making control flow clearer.

**5. Combine patterns with `or` when semantically equivalent**

If multiple syntactic forms require identical transformations because they represent the same *semantic concept*, combine them:
```scheme
; Multiple forms that all mean "intern this constant"
[(or `(quote ,(? complex-datum? ex))
     (? string? ex))
 (intern-constant ex)]
```

This is a judgment call based on semantic equivalence. Both quoted complex data and raw strings are "constants to intern" - same meaning, different syntax. Don't combine patterns just because they happen to use the same code if they represent different concepts.

**6. Naming conventions for expressions and environments**

Follow these conventions consistently:
```scheme
; Expressions: e, e1, e2, e* for lists
(define (Expr e env)
  (match e
    [`(if ,e1 ,e2 ,e3) ...]
    [`(let ([,x* ,e*] ...) ,body) ...]))

; Environments: always env, env1, env2
(define (transform expr env)
  (let-values ([(result env1) (process expr env)])
    ...))

; Avoid: e for environments - that's reserved for expressions
```

**7. Use `let*-values` for nested `let-values`**

Instead of nesting `let-values`:
```scheme
; AVOID: Nested let-values
(let-values ([(new-test env1) (Expr test env)])
  (let-values ([(new-conseq env2) (Expr conseq env1)])
    (let-values ([(new-altern env3) (Expr altern env2)])
      (values `(if ,new-test ,new-conseq ,new-altern) env3))))
```

Use `let*-values`:
```scheme
; PREFER: let*-values for sequential bindings
(let*-values ([(new-test env1) (Expr test env)]
              [(new-conseq env2) (Expr conseq env1)]
              [(new-altern env3) (Expr altern env2)])
  (values `(if ,new-test ,new-conseq ,new-altern) env3))
```

**8. Avoid parameter shadowing of struct constructors**

When you have a struct and parameters of that type, use descriptive struct names:
```scheme
; AVOID: Parameter shadows constructor
(struct env (map counter) #:transparent)
(define (process env) ; 'env' parameter shadows 'env' constructor!
  (env new-map new-counter)) ; Error: not a procedure

; PREFER: Descriptive struct name
(struct constant-env (map counter) #:transparent)
(define (process env) ; No shadowing
  (constant-env new-map new-counter)) ; Works correctly
```

**9. Shadow variable names instead of using numbered suffixes**

When transforming a value, reuse the same name rather than adding suffixes. You rarely need both the old and new values:
```scheme
; AVOID: Numbered suffixes that clutter the code
(let*-values ([(new-test env1) (Expr test env)]
              [(new-conseq env2) (Expr conseq env1)]
              [(new-altern env3) (Expr altern env2)])
  (values `(if ,new-test ,new-conseq ,new-altern) env3))

; PREFER: Shadow with the same name - cleaner and clearer
(let*-values ([(test env) (Expr test env)]
              [(conseq env) (Expr conseq env)]
              [(altern env) (Expr altern env)])
  (values `(if ,test ,conseq ,altern) env))
```

The shadowing makes it clear you're replacing the old value with its transformed version. Only use different names (like `test-old`, `test-new`) when you genuinely need to refer to both values at the same time, which is rare in functional code.

### Adding New Special Forms

When adding a new special form (like `quasiquote`, `let`, etc.) that
the compiler recognizes, multiple passes must be updated.

**Pass-through pattern**: If a pass doesn't process a form, it must
**explicitly pass it through** unchanged. Otherwise the form will
cause a match error.

**Example: Adding `quasiquote` support**

Each pass needs a case for the new form:

```scheme
;; In simplify-conditionals.ss, simplify-binding-forms.ss, etc.:
(define (Expr expr)
  (match expr
    ...
    [`(quasiquote ,datum) expr]  ; Pass through unchanged
    ...))
```

**Passes that typically need updates:**
- parse-and-rename.ss (process the form)
- simplify-conditionals.ss (pass through)
- simplify-binding-forms.ss (pass through)
- remove-complex-constants.ss (often expands the form)
- remove-memv.ss (pass through)
- make-begin-explicit.ss (pass through)
- uncover-settable.ss (pass through)
- remove-set.ss (pass through)
- uncover-free.ss (pass through)
- collect-code.ss (pass through)
- identify-tail-calls.ss (pass through)

**Two-stage processing pattern** (used by `quote` and `quasiquote`):

Some forms are **preserved early, expanded late**:

1. **Early passes** (parse-and-rename through collect-code): Preserve
   form structure, but process subexpressions as needed
2. **Late passes** (remove-complex-constants): Expand form to runtime
   construction (cons, append, etc.)

This allows early passes to do variable renaming and analysis while
deferring complex expansion to later.

**Example:**
```scheme
;; parse-and-rename.ss - processes unquoted expressions
[`(quasiquote ,datum) `(quasiquote ,(Quasiquote datum env))]

;; Middle passes - pass through
[`(quasiquote ,datum) expr]

;; remove-complex-constants.ss - expands to cons/append
[`(quasiquote ,datum) (expand-to-cons-chains datum)]
```

### Reader Transformations

The Racket reader automatically transforms certain syntax before the
compiler sees it:

| User writes | Reader produces |
|-------------|-----------------|
| `` `(a ,b) `` | `(quasiquote (a (unquote b)))` |
| `'(a b)` | `(quote (a b))` |
| `#(a b)` | `#(a b)` (vector literal) |

**Implication**: Compiler passes work with the expanded forms, not
the original syntax. When adding features, check what the reader
produces first:

```scheme
;; In Racket REPL:
> (read (open-input-string "`(a ,b)"))
'(quasiquote (a (unquote b)))
```

### Internal Identifier Conventions

**Prefix conventions for unshadowable identifiers:**

When the compiler needs to call runtime functions that user code
shouldn't be able to shadow, use **% prefix**:

```scheme
;; Good: Internal helper, can't be shadowed
(funcall %append x y)

;; Bad: User could redefine append
(funcall append x y)
```

**Examples of internal identifiers:**
- `%append` - Used by quasiquote expansion for splicing
- `%cons`, `%car`, `%cdr` - If runtime type checking is needed
- Future: `%map`, `%filter`, etc. for standard library internals

These functions are defined in the preamble/standard library in a
protected namespace.

### Architecture Quick Reference

| Aspect | ARM32 (32-bit) | RISC-V (64-bit) |
|--------|----------------|-----------------|
| **Heap register** | r8 | s11 |
| **Result register** | r0 | a0 |
| **Word size** | 4 bytes | 8 bytes |
| **Load** | `ldr r0, [r1, #offset]` | `ld a0, offset(a1)` |
| **Store** | `str r0, [r1, #offset]` | `sd a0, offset(a1)` |
| **Add immediate** | `add r0, r1, #imm` | `addi a0, a1, imm` |
| **Bitwise OR** | `orr r0, r1, #imm` | `ori a0, a1, imm` |
| **Tag values** | pair=#b001, vector=#b010, string=#b011 | symbol=#b111, ratnum=#b101, closure=#b110 |

### Code Generation Patterns

**Heap allocation:**
1. Evaluate operands (result goes to r0/a0)
2. Store in heap at current heap pointer
3. Tag the pointer with appropriate tag bits
4. Advance heap pointer by aligned size (usually 8 bytes)

**Using the stack (`si` parameter):**
- Temporary storage when evaluating multiple operands
- `si` is the stack index (negative offset from sp)
- Save: `str r0, [sp, #si]` (ARM) or `sd a0, si(sp)`
  (RISC-V)
- Load: `ldr r0, [sp, #si]` (ARM) or `ld a0, si(sp)`
  (RISC-V)

**Byte-access vs word-access:**

Strings store bytes; vectors store words.

| Operation | Vector | String |
|-----------|--------|--------|
| **Header** | 1 word of size | 1 word of byte count |
| **Element access** | Load word: `ldr`/`ld` | Load byte: |
| | | `ldrb`/`lbu` |
| **Index calc** | Multiply by wordsize | Direct (no multiply) |
| **Example** | `vector-ref v 2` → | `string-ref s 2` → |
| | offset `(2+1)*8` | offset `2+8` |

**String-ref pattern (byte-level):**
1. Clear tag to get raw pointer
2. Convert fixnum index to integer (right shift by 2)
3. Add wordsize to skip header
4. Load byte using `ldrb`/`lbu`
5. Convert byte to character: shift left 8 bits, OR with
   char-tag

**Key insight:** Use `with-saved-registers` to preserve one
operand across multiple operations.

**Testing:**
- No unit tests for `.def` files - only integration tests in
  `t/`
- Run `make unit` in `{arch}/s/` to test compiler passes
- Test on BOTH arm32le and rv64le

**Label generation:**

Assembly labels must follow assembler syntax:
- **Valid:** Letters, numerals, underscores, periods, dollar
  signs
- **Invalid:** Hyphens, question marks, other special
  characters
- **Cannot start with:** Dollar sign ($)

```scheme
(let ([done (unique-label "list_done")])
  (emit "beq a0,a1,~a" done)
  ; ... code ...
  (emit-label done))
```

Use `unique-label` with descriptive names (underscores, no
hyphens) to avoid label collisions.

## Debugging Test Failures

Error messages refer to compiler internals, not original test
code. For example, `(eq? 'a 'a)` might fail with:

```
compile-program: Unsupported primcall in
"'(string->symbol ...)"
```

The error references `string->symbol` because passes transform
`'a` before code generation.

**To debug:** grep for the error message under `s/` to find
the failing pass:

```bash
grep -r "Unsupported primcall" s/
```

This shows which pass is failing and what primitive/feature
needs implementation.

**Syntax checking for .def files:**

When editing architecture-specific code generators
(`arm32le.def`, `rv64le.def`), quickly check syntax before
running tests:

```bash
racket -e '(require "s/arm32le.def")'
```

This detects bracket/parenthesis mismatches and other read
errors without compilation. Exit code 0 means syntax is valid.

**Tracing Compiler Passes:**

When a compiler pass isn't transforming expressions as expected,
enable tracing to see what the pass is actually doing:

1. **Add tracing to the pass:** Insert `(require racket/trace)` at
   the top, then add a trace directive to the main entry point:
   ```scheme
   (trace-define (remove-complex-constants expr) ; changed from (define...
     (match expr ...))
   ```

2. **Run the failing test:** Execute the test that fails:
   ```bash
   make -C arm32le/t test-name
   ```

3. **Examine output files:** Check both `.out` and `.err` files:
   - `.out` - Contains the traced output (before-and-after values)
   - `.err` - Contains error messages and stack traces

4. **Analyze the transformation:** Look at what the pass returned:
   - Does it match expected output?
   - Are subexpressions being walked correctly?
   - Are all forms (quotes, let, primcall, etc.) handled?

**Example:** When `remove-complex-constants` wasn't collecting
constants from code definitions in labels, tracing showed that
the main function only walked the `body`, not the `labels`. The
solution was to add a case to also walk code definitions.

**Remote debugging with GDB (ARM32):**

When tests segfault or produce incorrect output, use GDB with
qemu for debugging:

1. **Two-terminal interactive workflow (preferred):**
   ```bash
   # Terminal 1: Start qemu with gdb server on port 9500
   cd arm32le/t
   make xdebug  # or manually: qemu-arm-static -L /usr/arm-linux-gnueabihf -g 9500 run-testname/out/x-test-program

   # Terminal 2: Connect gdb
   make gdb     # or manually: gdb-multiarch --quiet --tui -x ../../debug/setup.gdb --eval-command='target remote localhost:9500' run-testname/out/x-test-program
   ```

2. **Non-interactive batch mode (for automation):**
   ```bash
   cd arm32le/t

   # Start qemu in background
   qemu-arm-static -L /usr/arm-linux-gnueabihf -g 9500 run-testname/out/x-test-program &
   QEMU_PID=$!
   sleep 1

   # Run gdb batch commands
   gdb-multiarch --batch \
     --eval-command='target remote localhost:9500' \
     --eval-command='continue' \
     --eval-command='backtrace' \
     --eval-command='info registers' \
     run-testname/out/x-test-program

   # Clean up
   kill $QEMU_PID
   ```

3. **Check for lingering qemu processes:**
   ```bash
   ps aux | grep qemu-arm-static
   pkill -f "qemu-arm-static.*9500"
   ```

**Key debugging commands in GDB:**
- `continue` or `c` - Run until crash/breakpoint
- `backtrace` or `bt` - Show call stack
- `info registers` - Show all register values
- `pp $r0` - Pretty-print Scheme value in register r0 (custom
  command from setup.gdb)
- `x/10xw $r8` - Examine 10 words at heap pointer

**GDB debug script synchronization:**

The `debug/setup.gdb` file provides GDB helper functions for
inspecting Scheme values during debugging. It must stay in
sync with `c/driver.c`:

**Synchronization checklist:**
- All value tag constants (FIXNUM, BOOLEAN, CHAR, NULL, VOID,
  PAIR, VECTOR, STRING, SYMBOL, RATNUM, CLOSURE) defined in
  both files
- The dispatch order in `print_ptr` (C) matches the
  if-else nesting in GDB's `print_ptr` definition
- All type-specific print functions exist in both files
  (`print_vector`, `print_string`, `print_symbol`,
  `print_ratnum`)
- Bit-shift operations use identical constants for extracting
  values (e.g., `>> FIXNUM_SHIFT` for fixnum extraction)

**Common drift points:**
- When adding new data types to runtime (new tag values),
  both files need updates
- GDB uses deeply nested if-else while C uses flat
  if-else-if chains - ensure dispatch order is equivalent
- GDB helper functions must exactly mirror C function logic

**Example:** When ratnums were added, `c/driver.c` was
updated with RATNUM_TAG, `print_ratnum()`, and dispatch
logic, but `setup.gdb` was initially missed. Always update
both when adding new types.

## Design Decisions

### No Symbol Interning

Symbols are NOT interned. Each `(string->symbol "a")`
creates a fresh symbol:
- `(eq? 'a 'a)` → `#f` (two different symbol objects)
- `(let ([x 'a]) (eq? x x))` → `#t` (same object)

Non-standard but simplifies the compiler (no symbol table
needed).

### No Code Generator Unit Tests

The `.def` files lack unit tests - only integration tests
exist in `t/`. This is a known gap (documented in
`t/TODO.md`).

### Language Infrastructure

**Preamble system (basic):** A simple preamble system exists
in `lib/preamble.ss`. Definitions are prepended to user code
before compilation, making functions like `append` available
to all programs. See `preamble.md` for design details.

**Still missing (see `/TODO.md`):**
- **No variadic procedures** - Cannot define user functions
  with variable arguments (e.g., `(define (list . args) ...)`)
- **Limited standard library** - Only non-variadic procedures
  can be defined in the preamble

These limitations mean variadic procedures like `list`, `+`
(multi-arg), etc. must be either:
- Hard-coded as primitives with code generation
- Transformed in compiler (e.g., `vector` →
  `make-vector` + `vector-set!`)
- Unavailable to users

## Common Gotchas and Limitations

### Missing Features

1. **Quoted vectors**: `'#(a b c)` → Error (not in
   `remove-complex-constants`)
   - But `(vector a b c)` works (compiler transformation)

2. **String constructor from chars**: `(string #\h #\i)` →
   Not implemented
   - The `string` primcall only handles compile-time literals

3. **Primitives not first-class**: `(eq? car car)` → Not
   implemented

4. **No rationals, floats, bignums**: `9/2`, `3.4`, large
   integers → Not implemented

### Test Result Interpretation

Some tests expect "unspecified" results but implementation
returns concrete values:
- `(eq? #\a #\a)` → Returns `#t`, spec says "unspecified"
- `(eq? "abc" "abc")` → Returns `#f`, spec says
  "unspecified"

These tests are commented with `; implementation-defined`
notes.

### Makefile Dependencies

- `zo: src` in `s/Mf-base` is critical - ensures symlinks
  exist before compilation
- `unit: zo` ensures passes are compiled before running unit
  tests
- Without these, fresh clones fail with "cannot open input
  file" errors

## RISC-V Code Generation

**See `rv64le.md` for architecture-specific gotchas.**

When implementing primitives or porting ARM32 code to RISC-V,
consult `rv64le.md` for:

- **Comment syntax differences** (`@` vs `#`)
- **Wordsize-aware heap advancement** (critical bug risk)
- **Tag clearing patterns** (bitwise operations)
- **Register allocation** (callee-saved vs temporaries)
- **Load immediate behavior** (pseudo-instruction expansion)
- **Branch and addressing patterns** (RISC-V syntax)

Key lesson: RISC-V wordsize=8 (not 4 like ARM32). Allocations
storing multiple fields must advance heap by `(* 2 (wordsize))`,
not hardcoded bytes. Silent data corruption can result from heap
pointer bugs.

Also reference CLAUDE.md [Architecture Quick Reference](#architecture-quick-reference)
for register and instruction patterns across both architectures.
