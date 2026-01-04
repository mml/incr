# Project Overview

This is an incremental Scheme-to-native compiler targeting ARM32 and
RISC-V 64-bit architectures. The compiler is written in Racket and
generates assembly code that links with a small C runtime.

## Directory Structure

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

## Work Directives

- always check for balanced parentheses after you create or modify a file
- the code base prefers square brackets in certain places, including:
  - binding forms like `(let ([x 5] [y 2]) ...)`
  - cond like `(cond [(even? x) #t] [else 999])`
  - case like `(case foo [(a b c) 'letter] [(1 2 3) 'number])`
  - case-lambda
  - match
  - there may be others. emulate this style where you see it

## Build Workflow

**Initial setup:**
```bash
./configure --machine=arm32le -x    # or rv64le
```
Creates architecture workarea with `machine.ss` defining architecture.

**Build and test:**
- run all build commands from the project root
- make -C arm32le/s unit: run unit tests (only need to run on one arch)
- make -C ${machine}/s test: run integration tests (arm32le or rv64le)
- after changes which include `s/`, run the unit tests before trying the integration tests
- Claude can run the integration tests in parallel.  use `test-parallel`
- arm32le runs faster than rv64le
- when iterating on a specific test (e.g. catchall.ss), Claude can run it
  with make -C arm32le/t catchall
- if it's been a few hours, run make realclean before running make unit
  to make sure racket compilation is still good.  and for good measure,
  do this in both architectures.

**Important:** The Makefile dependency `zo: src` ensures symlinks are created before compiling. Without this, `make` fails on fresh workareas.

## Compiler Pass Pipeline

The compiler transforms Scheme code through several passes before code generation:

1. **parse-and-rename** - Parse syntax and rename variables to avoid conflicts
2. **remove-complex-constants** - Transform quoted data into primcalls:
   - `'a` → `(string->symbol (string #\a))`
   - `"hi"` → `(string #\h #\i)`
   - `'(a . b)` → `(cons (string->symbol ...) (string->symbol ...))`
3. **simplify-binding-forms** - Convert let/let*/letrec to simpler forms
4. **simplify-conditionals** - Normalize if/cond expressions
5. **remove-set** - Transform set! into heap-allocated boxes
6. **uncover-settable** - Identify which variables need boxes
7. **uncover-free** - Find free variables for closure conversion
8. **identify-tail-calls** - Mark tail positions
9. **collect-code** - Separate top-level functions
10. **Code generation** - `arm32le.def` or `rv64le.def` emits assembly

**Key insight:** When debugging, remember that the code being compiled
is NOT the original source - it's been transformed by earlier passes. A
test with `'a` will show `string->symbol` in error messages.

## Data Representation

The runtime uses a tagged pointer scheme to distinguish types:

### Immediate Values (unboxed)
These fit in a machine word and don't require heap allocation:

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
| Symbol | `#b100` | `[string-ptr]` (1 word, points to string) |
| Closure | `#b110` | `[code-ptr\|free0\|free1\|...]` |

**Memory alignment:** All heap allocations are 8-byte aligned. The low 3
bits of aligned pointers are always 0, which allows using them for type
tags.

**Heap pointer:** Maintained in r8 (ARM32) or s11 (RISC-V), points to
next free byte.

**String layout detail:**
- Header: 1 word (4 bytes ARM32, 8 bytes RISC-V) containing byte count
- Data: N bytes of UTF-8 character data (1 byte per ASCII character)
- Padding: To reach next 8-byte boundary
- Example: string "hi" occupies 8 bytes total: `[2][h][i][5 bytes padding]`

## Design Decisions

### No Symbol Interning
Symbols are NOT interned. Each `(string->symbol "a")` creates a fresh symbol object:
- `(eq? 'a 'a)` → `#f` (two different symbol objects)
- `(let ([x 'a]) (eq? x x))` → `#t` (same object) ;'

This is non-standard but simplifies the compiler (no symbol table needed).

### No Code Generator Unit Tests
The `.def` files lack unit tests - only integration tests exist in `t/`. This is a known gap (documented in `t/TODO.md`).

### Missing Language Infrastructure
See `/TODO.md` for major missing features:
- **No variadic procedures** - Cannot define user functions with variable arguments (e.g., `(define (foo . args) ...)`)
- **No preamble/standard library** - No way to define standard Scheme procedures once and link them into all programs

These limitations mean that functions like `vector`, `list`, etc. must be either:
- Hard-coded as primitives with code generation
- Transformed in the compiler (e.g., `vector` → `make-vector` + `vector-set!` calls)
- Manually defined by users in every program

# Debugging Test Failures

Error messages from test failures typically refer to compiler internals, not the test case that failed. For example, a test like `(eq? 'a 'a)` might fail with:

```
compile-program: Unsupported primcall in "'(string->symbol ...)"
```

The error references `string->symbol`, which doesn't appear in the test. This happens because compiler passes transform the code before code generation. Quoted symbols like `'a` become `(string->symbol (string #\a))`.

**To debug:** grep for the error message text under `s/` to find where in the compiler the error originates:

```
grep -r "Unsupported primcall" s/
```

This will show Claude which pass is failing and what primitive or feature needs to be implemented.

# Adding New Primitives to Code Generators

The code generators (`s/arm32le.def` and `s/rv64le.def`) follow a consistent pattern. To add a new primitive:

## 1. Add to the Primcall Dispatcher

In `emit-primitive-call`, add your primitive to the appropriate case:

```scheme
[(cons make-vector string string->symbol)  ; <- add here
 (emit-allocation-primcall op expr si env)]
```

Categories:
- **Allocation primcalls**: `emit-allocation-primcall` - allocate heap objects (cons, make-vector, string, string->symbol)
- **Side-effect primcalls**: `emit-side-effect-primcall` - mutate objects (vector-set!)
- **Inline primcalls**: Direct code in the case statement (add1, sub1, arithmetic, comparisons)

## 2. Implement the Primitive

For allocation primcalls, add a case to `emit-allocation-primcall`:

```scheme
[(string->symbol)
 (emit-expr (primcall-operand1 expr) si env)    ; evaluate argument
 (emit "  str r0, [~a]" heap-register)           ; store in heap
 (emit "  add r0,~a,#~a" heap-register (constant symbol-tag))  ; tag result
 (emit "  add ~a,~a,#8" heap-register heap-register)]  ; advance heap
```

## 3. Implement for Both Architectures

**CRITICAL**: Primitives must be implemented in both `arm32le.def` AND `rv64le.def`. Always test on both architectures.

## Architecture Quick Reference

| Aspect | ARM32 (32-bit) | RISC-V (64-bit) |
|--------|----------------|-----------------|
| **Heap register** | r8 | s11 |
| **Result register** | r0 | a0 |
| **Word size** | 4 bytes | 8 bytes |
| **Load** | `ldr r0, [r1, #offset]` | `ld a0, offset(a1)` |
| **Store** | `str r0, [r1, #offset]` | `sd a0, offset(a1)` |
| **Add immediate** | `add r0, r1, #imm` | `addi a0, a1, imm` |
| **Bitwise OR** | `orr r0, r1, #imm` | `ori a0, a1, imm` |
| **Tag values** | pair=#b001, vector=#b010, string=#b011, symbol=#b100, closure=#b110 |

## Common Patterns

**Heap allocation:**
1. Evaluate operands (result goes to r0/a0)
2. Store in heap at current heap pointer
3. Tag the pointer with appropriate tag bits
4. Advance heap pointer by aligned size (usually 8 bytes)

**Using the stack (`si` parameter):**
- Temporary storage when evaluating multiple operands
- `si` is the stack index (negative offset from sp)
- Save: `str r0, [sp, #si]` (ARM) or `sd a0, si(sp)` (RISC-V)
- Load: `ldr r0, [sp, #si]` (ARM) or `ld a0, si(sp)` (RISC-V)

**Byte-access vs word-access operations:**

Strings store data as individual bytes, unlike vectors which store word-sized elements. This creates key differences:

| Operation | Vector | String |
|-----------|--------|--------|
| **Header** | 1 word of size info | 1 word of byte count |
| **Element access** | Load word: `ldr`/`ld` | Load byte: `ldrb`/`lbu` |
| **Index calculation** | Multiply by wordsize | No multiplication needed |
| **Example** | `vector-ref v 2` → load at offset `(2+1)*8` | `string-ref s 2` → load at offset `2+8` |

**String-ref pattern (byte-level access):**

1. Clear the tag to get raw pointer
2. Convert fixnum index to integer (right shift by fixnum-shift=2)
3. Add wordsize constant to skip header (not multiply by wordsize)
4. Load byte using `ldrb` (ARM32) or `lbu` (RISC-V)
5. Convert byte to character by shifting and tagging:
   - Shift left by char-shift (8 bits) to position the ASCII value
   - OR with char-tag (`0b00001111`)

**ARM32 example:**
```scheme
(with-saved-registers [si ("r4")]
  (emit-expr string-operand si env)           ; string in r0
  (emit "BIC r4,r0,#~a" (constant string-tag)) ; clear tag
  (emit-expr index-operand si env)            ; index in r0 (fixnum)
  (emit "LSR r1,r0,#~a" (constant fixnum-shift)) ; index→int
  (emit "add r1,r1,#~a" (constant wordsize))  ; skip header
  (emit "ldrb r0, [r4,r1]")                   ; load byte
  (emit "LSL r0,r0,#~a" (constant char-shift)) ; position for char
  (emit "orr r0,r0,#~a" (constant char-tag))) ; apply tag
```

**RISC-V example:**
```scheme
(with-saved-registers [si ("s4")]
  (emit-expr string-operand si env)           ; string in a0
  (emit "li t0,~a" (bitwise-not (constant string-tag)))
  (emit "and s4,a0,t0")                       ; clear tag
  (emit-expr index-operand si env)            ; index in a0 (fixnum)
  (emit "srli a1,a0,~a" (constant fixnum-shift)) ; index→int
  (emit "addi a1,a1,~a" (constant wordsize)) ; skip header
  (emit "add s4,s4,a1")
  (emit "lbu a0, (s4)")                       ; load byte unsigned
  (emit "slli a0,a0,~a" (constant char-shift)) ; position for char
  (emit "ori a0,a0,~a" (constant char-tag))) ; apply tag
```

**Key insight:** Use `with-saved-registers` to preserve one operand across multiple operations.

**Testing:**
- No unit tests for `.def` files - only integration tests in `t/`
- Run `make unit` in `{arch}/s/` to test compiler passes
- Run `make test t=file.ss` in `{arch}/t/` to test code generation
- Test on BOTH arm32le and rv64le

# Common Gotchas and Limitations

## Missing Features

1. **Quoted vectors not in `datum->code`**:
   - `'#(a b c)` → Error: "no matching clause for '#(a)'"
   - The `remove-complex-constants` pass doesn't handle vector literals yet
   - Note: `(vector a b c)` works (implemented as compiler transformation)

2. **No `string` constructor from chars**:
   - `(string #\h #\i)` → Not implemented
   - The `string` primcall only handles compile-time string literals

3. **Primitives not first-class**:
   - `(eq? car car)` → Not implemented
   - Primitives like `car`, `cdr` cannot be used as values

4. **No rationals, floats, bignums**:
   - `9/2`, `3.4`, large integers → Not implemented

## Test Result Interpretation

Some tests expect "unspecified" results but the implementation returns concrete values:
- `(eq? #\a #\a)` → Returns `#t`, spec says "unspecified"
- `(eq? "abc" "abc")` → Returns `#f`, spec says "unspecified"

These tests are commented out with `; implementation-defined` notes.

## Compiler Pass Transformations

The compiler transforms some high-level constructs into lower-level primitives:

### In `parse-and-rename.ss` (Special Forms)

| Source Code | Transformation | Pass |
|-------------|----------------|------|
| `(list a b c)` | `(cons a (cons b (cons c '())))` | parse-and-rename |
| `(vector a b c)` | `(let ([v (make-vector 3 void)]) (vector-set! v 0 a) (vector-set! v 1 b) (vector-set! v 2 c) v)` | parse-and-rename |

These are handled as **special forms** with dedicated helper functions (`List`, `Vector`) that recursively build the transformed IR.

### In `remove-complex-constants.ss` (Constants)

| Source Code | After Transformation |
|-------------|---------------------|
| `'a` | `(string->symbol (string #\a))` |
| `'foo` | `(string->symbol (string #\f #\o #\o))` |
| `"hi"` | `(string #\h #\i)` |
| `'(a . b)` | `(cons (string->symbol ...) (string->symbol ...))` |
| `'()` | Unchanged (immediate null value) |

**Key insight:** If Claudes sees an error about a primitive not being implemented, grep for where it's used in the compiler passes - it might be generated by a transformation, not written by the user.

### Adding New Transformations

To add a new operation as a compiler transformation (following the `list`/`vector` pattern):

1. Add a case in `parse-and-rename.ss` Expr function (around line 261):
   ```scheme
   [`(myop ,expr* ___)
     (Myop expr* env)]
   ```

2. Add a helper function (around line 182):
   ```scheme
   (define (Myop expr* env)
     ; Transform expr* into primcalls using existing primitives
     ...)
   ```

**When to use transformations vs primitives:**
- Use transformations when you can express the operation using existing primitives
- Use primitives when you need new runtime behavior or performance is critical
- Transformations work automatically on all architectures (no code generation needed)

## Makefile Dependencies

- The `zo: src` dependency in `s/Mf-base` is critical - ensures symlinks exist before compilation
- The `unit: zo` dependency ensures passes are compiled before running unit tests
- Without these, fresh clones fail with "cannot open input file" errors

## Label Generation and Naming Conventions

When generating assembly code in `.def` files, labels must follow assembler syntax rules:

**Valid characters:** Letters, numerals, underscores, periods, and dollar signs
**Invalid characters:** Hyphens, question marks, other special characters
**Cannot start with:** Dollar sign ($)

**Pattern for generating unique labels:**
```scheme
(let ([done (unique-label "list_done")])
  ; ... code ...
  (emit "beq a0,a1,~a" done)
  ; ... more code ...
  (emit-label done))
```

Use `unique-label` with descriptive names (underscores, no hyphens) to
avoid label collisions when multiple instances of the same operation
appear in a program.
