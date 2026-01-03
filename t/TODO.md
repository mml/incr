# Commented-Out Tests

## Needs Implementation

### Internal defines (expansion.ss:138-184, 3 tests)
- [ ] Support `define` inside `let`/`let*`/`letrec` bodies
- Tests: basic define, shadowing, mutually recursive defines

### eq? tests from tspl4 (catchall.ss:288-376)

**Implementation-defined (commented):**
- Tests that return specific values but spec says "unspecified"
- Examples: `(eq? #\a #\a)`, `(eq? "abc" "abc")`, `(eq? (lambda (x) x) (lambda (y) y))`

**Needs symbol interning**
- Two `eq?` tests.

**Needs string-ref:**
- `(let ([x (string-ref "hi" 0)]) (eq? x x))`

**Needs make-bytevector:**
- 2 tests with make-bytevector

**Needs rationals/floats (~6 tests):**
- Tests with 9/2, 3.4, 1/3, etc.

**Needs bignums:**
- `(let ([x (* 12345678987654321 2)]) (eq? x x))`

**Needs primitives as values:**
- `(eq? car car)` - requires car/cdr to be first-class

## Intentionally Disabled

### Bug markers (keep commented)
- `expansion.ss:203` - letrec forward ref should fail at runtime but doesn't

### Stress tests (keep commented)
- `sweep-test.ss` - iterates 2^29 numbers, too slow for regular testing
- `expansion.ss:206-209` - large vector (10000 elements), adds 1-2s per test run

## Infrastructure Gaps

### Code generator unit tests
- The `.def` files (arm32le.def, rv64le.def) lack unit tests
- Only integration testing via t/ directory
- Consider adding unit tests for individual primcall implementations
