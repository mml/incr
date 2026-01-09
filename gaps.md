# Self-Compilation Gaps Analysis

This document identifies features required by the compiler source code
(in `s/`) that are not currently supported by the compiler's code
generation, preventing the compiler from compiling itself.

## Executive Summary

**Total Source Lines**: ~6,300 lines across 18 `.ss` files
**Language**: Racket (`#lang racket`)
**Current Status**: Cannot self-compile due to 15 critical feature gaps

The compiler is written in Racket and uses extensive Racket-specific
features. To achieve self-compilation, Matt must either:
1. Implement missing features in the compiler's code generation
2. Rewrite the compiler to use only supported features
3. Implement a minimal Racket subset sufficient for the compiler

---

## CRITICAL GAPS - Must Have for Self-Compilation

### 1. Pattern Matching (match)

**Status**: NOT IMPLEMENTED
**Usage**: Pervasive - used in 15 out of 18 compiler passes
**Impact**: BLOCKER - Entire compiler pipeline depends on this

**What the compiler uses:**
```scheme
(match expr
  [`(quote ,_) expr]
  [(? immediate? c) `',c]
  [`(list ,expr* ___) (List expr* env)]
  [`(lambda (,x* ___) ,body* __1) ...])
```

**What the compiler supports:**
- Only `case` statement (pattern matching on atoms, not structures)
- No structural destructuring
- No pattern guards with `?`
- No ellipsis patterns (`___`, `__1`)

**Workaround complexity**: HIGH - Would require rewriting ~4,000
lines of pattern-matching code to use manual car/cdr destructuring

---

### 2. Quasiquote with Unquote and Unquote-Splicing

**Status**: NOT IMPLEMENTED
**Usage**: Universal - every compiler pass uses this for code generation
**Impact**: BLOCKER - All IR construction depends on this

**What the compiler uses:**
```scheme
`(lambda (,x) ,@(Expr* body* env))
`(letrec* ,(map list ux* e*) ,@body*)
```

**What the compiler supports:**
- Only `quote` for atoms and lists
- No backtick/unquote (`` ` `` , `,` , `,@`)
- Complex constants are transformed to runtime construction

**Workaround complexity**: VERY HIGH - Would require replacing all
template-based code generation with explicit cons/list construction

---

### 3. Macros (define-syntax with syntax-case)

**Status**: NOT IMPLEMENTED
**Usage**: 4 critical files
**Impact**: HIGH - Essential abstractions rely on this

**What the compiler uses:**
- `s/compile-shared.ss`: `define-constant`, `constant` macros for
  tag constants
- `s/arm32le.def`, `s/rv64le.def`: `with-saved-registers`, `emit`
  macros
- `s/test-driver.ss`: `test-cases`, `test-case` variadic macros

**What the compiler supports:**
- No macro system at all
- No compile-time code expansion

**Workaround complexity**: HIGH - Would need to inline all macro
expansions or implement runtime equivalents

---

### 4. Module System (require/provide)

**Status**: NOT IMPLEMENTED
**Usage**: Every file
**Impact**: BLOCKER - No way to organize multi-file programs

**What the compiler uses:**
```scheme
(require racket/match)
(provide compile-program)
```

**What the compiler supports:**
- Only single-file programs
- No namespace management
- No separate compilation

**Workaround complexity**: MEDIUM - Could concatenate all files, but
would need to resolve circular dependencies and remove module
declarations

---

### 5. Multiple Value Returns (let-values)

**Status**: NOT IMPLEMENTED
**Usage**: 4 critical compiler passes
**Impact**: HIGH - Used for tracking free/settable variables

**What the compiler uses:**
```scheme
(let-values ([(expr frees) (Expr e1 '())])
  ...)
```
Used in: `uncover-free.ss`, `uncover-settable.ss`,
`collect-code.ss`, `remove-complex-constants.ss`

**What the compiler supports:**
- Only single return values
- No `values` primitive
- No `let-values` binding form

**Workaround complexity**: MEDIUM - Could rewrite to return
pairs/lists and manually destructure

---

### 6. Variadic Functions (rest parameters)

**Status**: NOT IMPLEMENTED (only hardcoded primitives support this)
**Usage**: 3 essential functions
**Impact**: HIGH - Assembly emission depends on this

**What the compiler uses:**
```scheme
(define (emit . args) ...)         ; s/rv64le.def:36
(define (emit/ comment . args) ...) ; s/arm32le.def:110
(define (syms-unique? . args) ...) ; s/parse-and-rename.ss:27
```

**What the compiler supports:**
- Fixed-arity user functions only
- Variadic primitives: `string`, `and`, `or` (hardcoded)

**Workaround complexity**: MEDIUM - Could convert to explicit list
parameters: `(define (emit args) ...)`

---

### 7. Function Arity Overloading (case-lambda)

**Status**: NOT IMPLEMENTED
**Usage**: 3 files
**Impact**: MEDIUM - Used for convenience functions

**What the compiler uses:**
```scheme
(define get-stack-index
  (case-lambda
    [(si) si]
    [(si offset) (- si offset)]))
```

Used in: `compile-shared.ss`, `arm32le.def`, `rv64le.def`

**What the compiler supports:**
- Only single-arity lambda

**Workaround complexity**: LOW - Can split into separate functions
with different names

---

### 8. Set Operations

**Status**: NOT IMPLEMENTED
**Usage**: 2 critical compiler passes
**Impact**: HIGH - Free variable analysis depends on this

**What the compiler uses:**
- `set`, `set-union`, `set-subtract`, `set-intersect`,
  `set->list`, `set-empty?`
- Used extensively in `uncover-free.ss` and `uncover-settable.ss`

**What the compiler supports:**
- Lists only
- No set data structure

**Workaround complexity**: MEDIUM - Could implement sets as sorted
lists with custom operations, but would need to implement the entire
set library

---

### 9. String Manipulation Functions

**Status**: PARTIALLY IMPLEMENTED
**Usage**: Universal - used for symbol generation and code emission
**Impact**: HIGH - Cannot construct symbols or format output

**What the compiler uses:**
- `string-append` - Used in `compile-shared.ss`, `generators.ss`
- `string->list`, `list->string` - Used in `generators.ss`
- `format` - Used for assembly code generation and error messages
- `symbol->string`, `number->string` - Used for identifier generation

**What the compiler supports:**
- `string-ref` - Read single character (read-only)
- String literals

**Workaround complexity**: HIGH - Would need to implement all string
operations from scratch

---

### 10. Symbol Generation (gensym)

**Status**: NOT IMPLEMENTED
**Usage**: 2 files
**Impact**: HIGH - Variable renaming depends on unique symbols

**What the compiler uses:**
```scheme
(gensym 'vec)    ; parse-and-rename.ss:177
(gensym '_)      ; parse-and-rename.ss:184
```

**What the compiler supports:**
- Manual unique identifier generation using counters

**Workaround complexity**: LOW - Already has `unique-variable` in
`generators.ss` - could be adapted

---

### 11. Format Strings

**Status**: NOT IMPLEMENTED
**Usage**: Universal - all code generators use this
**Impact**: HIGH - Assembly emission depends on this

**What the compiler uses:**
```scheme
(format "~a~n" (pretty-format expr*))
(format "  add r0,~a,#~a" heap-register offset)
```

**What the compiler supports:**
- No format function
- No string interpolation

**Workaround complexity**: HIGH - Would need full printf-style
formatting or manual string construction

---

### 12. Higher-Order Functions

**Status**: PARTIALLY IMPLEMENTED
**Usage**: Universal
**Impact**: HIGH - All list transformations use these

**What the compiler uses:**
- `map` - Pervasive (75+ uses in parse-and-rename.ss alone)
- `apply` - Used with `set-union*` in uncover passes
- `filter` - List filtering
- `for-each` - Iteration with side effects

**What the compiler supports:**
- None of these as primitives
- Lambda is supported, but no built-in map/apply

**Workaround complexity**: MEDIUM - Could implement in library, but
needs variadic support for full `apply` implementation

---

### 13. List Utilities

**Status**: NOT IMPLEMENTED
**Usage**: Universal
**Impact**: HIGH - Core data structure operations

**What the compiler uses:**
- `append` - List concatenation
- `reverse` - List reversal
- `length` - List length
- `list-ref` - Access nth element
- `memq`, `assq` - Membership and association lookup

**What the compiler supports:**
- Only: `car`, `cdr`, `cadr`, `cddr`, `caddr`, `cons`, `null?`,
  `list` (as transformation)

**Workaround complexity**: MEDIUM - Could implement in user code, but
common enough that they should be primitives

---

### 14. I/O and Port Operations

**Status**: NOT IMPLEMENTED
**Usage**: Test driver and compilation infrastructure
**Impact**: HIGH - Cannot write assembly output or read input

**What the compiler uses:**
- `open-output-file`, `close-output-port` - File writing
- `fprintf`, `newline` - Formatted output
- `with-output-to-string` - String output capture
- `compile-port` parameter - Current output port
- `current-output-port` - Standard output

**What the compiler supports:**
- None

**Workaround complexity**: VERY HIGH - Would need full I/O system
with ports, parameters, and exception handling

---

### 15. Parameterization System

**Status**: NOT IMPLEMENTED
**Usage**: 2 files (compile-shared.ss, test-driver.ss)
**Impact**: MEDIUM - Used for configuration

**What the compiler uses:**
```scheme
(make-parameter default-value validation-fn)
(parameterize ([param value]) ...)
```

**What the compiler supports:**
- Nothing similar

**Workaround complexity**: LOW-MEDIUM - Could use global variables
with `set!` or pass as explicit parameters

---

## MINOR GAPS - Nice to Have

### 16. Unit Testing Infrastructure

**Status**: NOT IMPLEMENTED (but not needed for self-compilation)
**Usage**: All compiler passes have `(module+ test ...)` blocks
**Impact**: LOW - Tests can run externally

**What the compiler uses:**
- `rackunit` with `check-equal?`, `check-not-equal?`, `check-exn`
- `module+ test` blocks for inline tests

**Workaround**: Run tests using existing Racket installation;
self-compiled compiler doesn't need to run its own tests

---

### 17. Debugging and Tracing

**Status**: NOT IMPLEMENTED
**Usage**: Optional debugging support
**Impact**: NEGLIGIBLE - Can be removed

**What the compiler uses:**
- `racket/trace` module
- Optional tracing for development

**Workaround**: Remove tracing or use manual printf debugging

---

## INFRASTRUCTURE GAPS

### 18. Lazy Module Loading

**Status**: NOT IMPLEMENTED
**Usage**: 1 file (compile-shared.ss)
**Impact**: LOW - Can work around

**What the compiler uses:**
```scheme
(lazy-require
  ["machine.ss" (wordsize unique-label emit-label emit-function-header)])
```

**Workaround**: Reorder modules to avoid circular dependencies, or
inline the machine-specific definitions

---

## EXISTING LIMITATIONS (Already Documented)

The following are documented in MISSING.md and TODO.md:

1. **No variadic user functions** - Parser doesn't recognize `. args`
   syntax
2. **Limited standard library** - Basic preamble system exists
   (`lib/preamble.ss`) but only supports non-variadic procedures
3. **No symbol interning** - `(eq? 'a 'a)` → `#f` (by design)
4. **No floating-point numbers** - Only fixnums and ratnums
5. **No bignums** - Large integers not supported
6. **Incomplete ratnum arithmetic** - `(+ 1/2 1/3)` not implemented
7. **No eval/load** - Cannot evaluate S-expressions at runtime
8. **No call/cc** - No first-class continuations
9. **Primitives not first-class** - Cannot pass `car` as a value

---

## SUMMARY TABLE

| Feature | Status | Impact | Files Using | Workaround |
|---------|--------|--------|-------------|------------|
| Pattern matching (match) | ✗ | BLOCKER | 15 | Rewrite 4000+ lines |
| Quasiquote/unquote | ✗ | BLOCKER | 18 | Rewrite all codegen |
| Module system | ✗ | BLOCKER | 18 | Concatenate files |
| Multiple values | ✗ | HIGH | 4 | Return pairs |
| Macros (define-syntax) | ✗ | HIGH | 4 | Inline expansions |
| Variadic functions | ✗ | HIGH | 3 | Explicit list params |
| Set operations | ✗ | HIGH | 2 | Implement as library |
| String operations | Partial | HIGH | 18 | Implement missing ops |
| Format strings | ✗ | HIGH | 18 | Implement printf |
| Higher-order (map/apply) | ✗ | HIGH | 18 | Implement as library |
| List utilities | ✗ | HIGH | 18 | Implement as library |
| I/O operations | ✗ | HIGH | 2 | Implement file I/O |
| Gensym | ✗ | HIGH | 2 | Adapt unique-variable |
| Parameters | ✗ | MEDIUM | 2 | Use globals |
| case-lambda | ✗ | MEDIUM | 3 | Separate functions |
| Lazy require | ✗ | LOW | 1 | Reorder modules |
| Testing (rackunit) | ✗ | LOW | 18 | External testing |
| Tracing | ✗ | NEGLIGIBLE | 13 | Remove or printf |

**Total Gaps**: 18 features
**Blockers**: 3 (pattern matching, quasiquote, modules)
**High Priority**: 10
**Medium Priority**: 2
**Low Priority**: 3

---

## RECOMMENDATION

**Path to self-compilation requires one of:**

**Option A: Minimal Racket Subset (Recommended)**
- Implement the 13 critical/high-priority features
- This creates a "Scheme+" that can compile the compiler
- Estimated effort: Large (3-6 months of focused work)

**Option B: Compiler Rewrite**
- Rewrite compiler using only currently-supported features
- Replace pattern matching with manual destructuring
- Replace quasiquote with explicit cons
- Merge all modules into single file
- Estimated effort: Very Large (6-12 months)

**Option C: Staged Bootstrap**
1. Implement minimal features (quasiquote, modules, basic strings)
2. Rewrite small portions to use only those features
3. Incrementally self-compile more of the compiler
4. Gradually add features until full self-compilation
- Estimated effort: Large but incremental (4-8 months)

**Option A (Minimal Racket Subset) is most practical** because it
preserves the elegant architecture of the existing compiler while
building a more complete language implementation.
