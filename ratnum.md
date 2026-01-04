# Rational Number (Ratnum) Implementation Specification

## Overview

This document specifies the implementation of exact rational numbers (ratnums)
for the incremental Scheme compiler. The primary goal is to **unblock the `/`
division operator** to support exact division that returns rational results
when needed.

## Design Principles

1. **Simplicity over optimization** - Start with straightforward implementation
2. **Eager normalization** - Always maintain canonical form (lowest terms)
3. **Type safety** - Clear type dispatch and predicates
4. **TDD approach** - Write tests first, then implement

## Memory Representation

### Tagged Pointer Layout

**Type:** Heap-allocated (boxed) object
**Tag:** `#b101` (5)
**Size:** 2 words (8 bytes on ARM32, 16 bytes on RISC-V)

```
Memory layout:
┌─────────────┬─────────────┐
│  numerator  │ denominator │
│  (fixnum)   │  (fixnum)   │
└─────────────┴─────────────┘
       ↑
       └─ Tagged pointer (address | #b101)
```

### Canonical Form Invariants

1. **Sign location**: Sign always in numerator, denominator always positive
   - Valid: `3/4`, `-3/4`, `0/1`
   - Invalid: `3/-4`, `-3/-4`

2. **Normalization**: Always reduced to lowest terms via GCD
   - `6/4` is stored as `3/2`
   - `4/2` is stored as `2/1` (but see division optimization below)

3. **Zero representation**: `0/1` (denominator never zero in valid ratnums)

## Type System Integration

### Predicates

| Predicate | Fixnum | Ratnum | Future Float |
|-----------|--------|--------|--------------|
| `rational?` | `#t` | `#t` | `#f` |
| `number?` | `#t` | `#t` | `#t` |
| `exact?` | `#t` | `#t` | `#f` |
| `integer?` | `#t` | `#f` | depends |

**Note:** `integer?` is representation-based, not value-based:
- `(integer? 4/2)` → `#f` even though mathematically 2
- To check if ratnum represents integer: `(= (denominator x) 1)`

### Accessor Functions

**Signature:**
```scheme
(numerator q)   → fixnum
(denominator q) → fixnum (always positive)
```

**Behavior:**
- **On ratnums**: Extract components
  - `(numerator 3/4)` → `3`
  - `(denominator 3/4)` → `4`

- **On fixnums**: Treat as rational with denominator 1
  - `(numerator 5)` → `5`
  - `(denominator 5)` → `1`

- **On other types**: Error (undefined behavior for now)

## Arithmetic Operations

### Division Operator `/`

**Type Dispatch Cases:**

#### Case 1: `fixnum / fixnum`

Returns fixnum if exact, ratnum if inexact.

```scheme
(/ 4 2)  → 2      ; fixnum (exact division)
(/ 3 2)  → 3/2    ; ratnum (inexact division)
(/ 6 4)  → 3/2    ; ratnum, normalized
(/ 0 5)  → 0      ; fixnum (zero)
```

**Algorithm:**
1. Compute `quotient = a / b` (integer division)
2. Compute `remainder = a % b` (modulo)
3. If `remainder == 0`: return `quotient` as fixnum
4. Else: construct ratnum with `make-ratnum(a, b)` (normalizes automatically)

#### Case 2: `ratnum / fixnum`

Always returns ratnum.

```scheme
(/ 3/4 2)  → 3/8
(/ 2/3 4)  → 1/6
```

**Formula:** `(a/b) / c = a / (b*c)`

**Algorithm:**
1. Extract `a = numerator(ratnum)`, `b = denominator(ratnum)`
2. Compute `new_den = b * c` (check overflow)
3. Return `make-ratnum(a, new_den)` (normalizes)

#### Case 3: `fixnum / ratnum`

Always returns ratnum.

```scheme
(/ 2 3/4)  → 8/3
(/ 6 2/3)  → 9/1  → 9  ; normalized to fixnum
```

**Formula:** `a / (c/d) = (a*d) / c`

**Algorithm:**
1. Extract `c = numerator(ratnum)`, `d = denominator(ratnum)`
2. Compute `new_num = a * d` (check overflow)
3. Return `make-ratnum(new_num, c)` (normalizes)

#### Case 4: `ratnum / ratnum`

Always returns ratnum.

```scheme
(/ 2/3 4/5)  → 10/12  → 5/6  ; normalized
(/ 3/4 3/4)  → 1/1    → 1    ; normalized to fixnum
```

**Formula:** `(a/b) / (c/d) = (a*d) / (b*c)`

**Algorithm:**
1. Extract `a, b` from first ratnum, `c, d` from second
2. Compute `new_num = a * d`, `new_den = b * c` (check overflow)
3. Return `make-ratnum(new_num, new_den)` (normalizes)

### Other Arithmetic (Deferred to Phase 2)

**Addition:** `(a/b) + (c/d) = (ad + bc) / bd`
**Subtraction:** `(a/b) - (c/d) = (ad - bc) / bd`
**Multiplication:** `(a/b) * (c/d) = (ac) / (bd)`, then normalize

**Comparison:** `(a/b) < (c/d)` iff `(a*d) < (b*c)` (cross-multiply)

These are not required for Phase 1 but included in spec for completeness.

## Normalization and GCD

### GCD Algorithm

**Implementation:** Euclidean algorithm, inline assembly

```
gcd(a, b):
  while b ≠ 0:
    t = b
    b = a mod b
    a = t
  return |a|  ; absolute value
```

**Assembly Emission:**
- Generate once as callable label `__gcd:` in epilogue/helper section
- Called from `make-ratnum` and arithmetic operations
- Takes two fixnums (as tagged or untagged per calling convention)
- Returns GCD as fixnum

**Inputs:** Two fixnums (may be negative)
**Output:** Positive fixnum (GCD is always positive)

### Normalization Procedure

**Function:** `make-ratnum(numerator, denominator)`

**Algorithm:**
1. **Check denominator:** If `denominator == 0`, undefined behavior (TODO: add
   error check)
2. **Normalize sign:**
   - If `denominator < 0`: negate both `numerator` and `denominator`
   - Ensures denominator is always positive
3. **Compute GCD:**
   - `g = gcd(abs(numerator), denominator)`
4. **Reduce:**
   - `num = numerator / g`
   - `den = denominator / g`
5. **Optimize:** If `den == 1`, return `num` as fixnum (not ratnum)
6. **Allocate:** Store `[num | den]` in heap, tag with `#b101`, advance heap
   pointer

**Edge Cases:**
- `make-ratnum(0, n)` → `0` (fixnum, not `0/1` ratnum)
- `make-ratnum(n, 1)` → `n` (fixnum, not `n/1` ratnum)
- `make-ratnum(-6, 4)` → `-3/2` (normalized, sign in numerator)
- `make-ratnum(6, -4)` → `-3/2` (sign moved to numerator)
- `make-ratnum(-6, -4)` → `3/2` (both signs cancel)

## Printing

### Output Format

**Standard Scheme notation:** `numerator/denominator`

**Examples:**
- `3/4` (positive)
- `-3/4` (negative numerator)
- `-2/7` (negative)

### Implementation

**File:** `c/driver.c`
**Function:** `print_value(val)`

**Pseudo-code:**
```c
if ((val & 0x7) == 0b101) {  // ratnum tag
    int64_t *ptr = (int64_t *)(val & ~0x7);  // clear tag
    int64_t num = ptr[0] >> 2;  // untag fixnum
    int64_t den = ptr[1] >> 2;  // untag fixnum
    printf("%lld/%lld", num, den);
}
```

**Note:** Numbers are stored as tagged fixnums in memory (shifted left 2 bits),
so must be untagged before printing.

## Error Handling and Limitations

### Division by Zero

**Current behavior:** Undefined (hardware exception or garbage result)
**TODO:** Add runtime check in division and `make-ratnum`:
```c
if (denominator == 0) {
    fprintf(stderr, "Error: division by zero\n");
    exit(1);
}
```

**Rationale:** Skip for Phase 1 to simplify implementation. Add safety later.

### Overflow

**Policy:** Error immediately if numerator or denominator exceed fixnum range

**Fixnum range:**
- ARM32: 30-bit signed integers (-2^29 to 2^29 - 1)
- RISC-V: 62-bit signed integers (-2^61 to 2^61 - 1)

**Overflow scenarios:**
```scheme
(/ 1 (expt 2 30))           ; Error: denominator overflow
(* 536870911/2 536870911/2) ; Error: intermediate overflow
```

**Detection:**
- Check overflow on multiply operations in arithmetic
- If overflow: print error and abort
- No automatic promotion to bignum or inexact (not implemented)

**Future:** When bignums are implemented, promote instead of erroring.

### Future Numeric Tower

When inexact arithmetic (floats) is added:

**Conversions:**
- `exact->inexact`: Convert ratnum to floating-point
  - `(exact->inexact 3/4)` → `0.75`
- `inexact->exact`: Convert float to ratnum (if possible)
  - `(inexact->exact 0.75)` → `3/4`

**Mixed arithmetic:**
- `ratnum op float` → `float` (inexact contagion)
- Keep exact arithmetic exact as long as possible

## Implementation Plan

### Phase 1: Minimal Division Support

**Goal:** Unblock `/` operator with basic ratnum support

**Components:**

1. **Constants** (`s/compile-shared.ss`)
   - Add `(define ratnum-tag #b101)`
   - Export for use in code generators

2. **C Runtime** (`c/driver.c`)
   - Implement `print_value` case for ratnum tag
   - Format: `printf("%lld/%lld", num, den)`

3. **Code Generators** (`s/arm32le.def`, `s/rv64le.def`)
   - **GCD helper:** Emit `__gcd:` label with Euclidean algorithm loop
   - **make-ratnum primitive:**
     - Sign normalization
     - GCD call
     - Reduction
     - Heap allocation (2 words)
     - Optimization: return fixnum if denominator = 1
   - **Division `/` primitive:**
     - Type dispatch on both operands (fixnum vs ratnum tag check)
     - Four cases as specified above
     - Call `make-ratnum` for ratnum results
   - **Predicates:** `rational?`, `number?`, `exact?`, `integer?`
   - **Accessors:** `numerator`, `denominator`

4. **Tests** (`t/ratnum.ss`)
   - Write test cases first (TDD)
   - Add `ratnum` to `tests` list in `t/Mf-base`

5. **Build Order** (Top-Down / TDD)
   - Step 1: Write test file with expected outputs
   - Step 2: Add ratnum-tag constant
   - Step 3: Implement print support (to see test output)
   - Step 4: Implement GCD helper
   - Step 5: Implement make-ratnum
   - Step 6: Implement division cases
   - Step 7: Implement predicates and accessors
   - Step 8: Run tests, fix bugs, iterate

### Phase 2: Full Arithmetic (Future)

- Implement `+`, `-`, `*` for ratnums
- Implement comparison: `=`, `<`, `>`, `<=`, `>=`
- Extend test coverage

## Test Coverage

### Test File Structure

**File:** `t/ratnum.ss`

**Test groups:**

1. **Division returning ratnums**
   ```scheme
   (test-case (/ 3 2) "3/2")
   (test-case (/ 1 3) "1/3")
   (test-case (/ -5 2) "-5/2")
   ```

2. **Division returning fixnums** (exact division optimization)
   ```scheme
   (test-case (/ 4 2) "2")
   (test-case (/ 6 3) "2")
   (test-case (/ -8 4) "-2")
   ```

3. **GCD normalization verification**
   ```scheme
   (test-case (numerator (/ 6 4)) "3")      ; 6/4 → 3/2
   (test-case (denominator (/ 6 4)) "2")
   (test-case (/ 10 15) "2/3")              ; gcd(10,15)=5
   ```

4. **Sign normalization**
   ```scheme
   (test-case (/ 3 -2) "-3/2")              ; sign moved to numerator
   (test-case (/ -3 -2) "3/2")              ; both signs cancel
   ```

5. **Zero cases**
   ```scheme
   (test-case (/ 0 5) "0")                  ; zero numerator → fixnum 0
   (test-case (numerator 0) "0")
   (test-case (denominator 0) "1")
   ```

6. **Predicates**
   ```scheme
   (test-case (rational? (/ 3 2)) "#t")
   (test-case (rational? 5) "#t")           ; fixnums are rational
   (test-case (number? (/ 1 3)) "#t")
   (test-case (exact? (/ 2 3)) "#t")
   (test-case (integer? (/ 3 2)) "#f")      ; representation-based
   (test-case (integer? 5) "#t")
   ```

7. **Accessors on fixnums**
   ```scheme
   (test-case (numerator 5) "5")
   (test-case (denominator 5) "1")
   ```

8. **Mixed-type division** (when ratnums exist)
   ```scheme
   (test-case (/ (/ 3 4) 2) "3/8")          ; ratnum / fixnum
   (test-case (/ 2 (/ 3 4)) "8/3")          ; fixnum / ratnum
   (test-case (/ (/ 2 3) (/ 4 5)) "5/6")    ; ratnum / ratnum
   ```

### Edge Cases to Cover

- **Negative numbers:** Numerator negative, denominator always positive
- **Zero numerator:** Returns fixnum 0, not ratnum 0/1
- **Already normalized:** Division that doesn't need reduction
- **Large GCD:** Test GCD algorithm with coprime vs high common factors
- **Denominator = 1 optimization:** Should return fixnum, not ratnum

### Performance Considerations

**GCD overhead:**
- Eager normalization means GCD on every ratnum construction
- Euclidean algorithm is O(log(min(a,b)))
- For small numbers (common case), very fast
- For large numbers, may be bottleneck

**Mitigation (future optimization):**
- Profile real usage to identify hot paths
- Consider lazy normalization for arithmetic chains
- Document performance characteristics for users

## Architecture-Specific Details

### ARM32 vs RISC-V

**Semantics:** Identical across both architectures
**Differences:** Only assembly instruction syntax

**GCD Implementation:**

**ARM32:**
```asm
__gcd:
    ; Input: r0 = a, r1 = b (fixnums, may be negative)
    ; Output: r0 = gcd(a, b) (positive fixnum)
    ; Make absolute values
    cmp r0, #0
    rsblt r0, r0, #0     ; if negative, negate
    cmp r1, #0
    rsblt r1, r1, #0     ; if negative, negate
    ; Euclidean algorithm
.gcd_loop:
    cmp r1, #0
    beq .gcd_done
    ; t = a % b
    ; (use division instructions or shift-based approach)
    ; swap: a = b, b = t
    ; loop
    b .gcd_loop
.gcd_done:
    bx lr                ; return r0
```

**RISC-V:**
```asm
__gcd:
    # Input: a0 = a, a1 = b (fixnums, may be negative)
    # Output: a0 = gcd(a, b) (positive fixnum)
    # Make absolute values (similar logic)
    # Euclidean algorithm loop
    # return
```

**Division:**
- ARM32: `sdiv` instruction (if available), else libgcc call
- RISC-V: `div` and `rem` instructions

**Heap Allocation:**
- ARM32: Heap pointer in `r8`, 8-byte aligned allocation
- RISC-V: Heap pointer in `s11`, 8-byte aligned allocation

## Files to Modify

| File | Changes |
|------|---------|
| `s/compile-shared.ss` | Add `ratnum-tag` constant |
| `c/driver.c` | Implement print case for ratnum tag |
| `s/arm32le.def` | Implement GCD, make-ratnum, division, predicates, accessors |
| `s/rv64le.def` | Same as ARM32, different instruction syntax |
| `t/ratnum.ss` | New test file with comprehensive test cases |
| `t/Mf-base` | Add `ratnum` to `tests` list (line 3) |

## Design Decisions Summary

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| **Representation** | Heap-allocated, tag #b101 | Consistent with other boxed types, simple |
| **Normalization** | Eager (on construction) | Canonical form, simpler equality/comparison |
| **GCD** | Euclidean algorithm, inline asm | Callable helper, emitted in epilogue |
| **Sign** | Always in numerator | Mathematical convention, simpler comparison |
| **Division result** | Fixnum if exact, ratnum if not | Efficient, avoids unnecessary allocations |
| **Overflow** | Error immediately | Simple, defer bignum support |
| **Division by zero** | Undefined (TODO: error) | Pragmatic for Phase 1, fix later |
| **Type dispatch** | Runtime checks in codegen | Fast, explicit control flow |
| **Testing** | TDD - tests first | Ensures correctness from the start |
| **Architecture** | Identical semantics | Portable, only asm syntax differs |

## Notes and TODOs

- **Division by zero:** Currently undefined behavior (no check). Should add
  runtime check eventually.
- **Overflow detection:** Need to check multiply operations in arithmetic.
  Document that large rationals will error.
- **Performance:** GCD on every construction may be slow. Profile and optimize
  if needed.
- **Future float integration:** Document interaction with inexact arithmetic
  when floats are added.
- **Bignum support:** When implemented, change overflow policy from error to
  promote.
- **Comparison operators:** Not in Phase 1 but needed for full testing.
  Consider implementing `=` at minimum for assertions.
