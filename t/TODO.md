# Commented-Out Tests

## Needs Implementation

### Internal defines (expansion.ss:138-184, 3 tests)
- [ ] Support `define` inside `let`/`let*`/`letrec` bodies
- Tests: basic define, shadowing, mutually recursive defines

### eq? tests from tspl4 (catchall.ss:291-373)

**Ready to enable (~25 tests):**
- eq? on booleans, symbols, chars, null, pairs, vectors, strings, closures
- These use only currently-implemented features

**Needs string-ref:**
- `(let ([x (string-ref "hi" 0)]) (eq? x x))`

**Needs string->symbol:**
- `(eq? 'a (string->symbol "a"))`

**Needs string constructor:**
- `(let ([x (string #\h #\i)]) (eq? x x))`
- `(eq? (string #\h #\i) (string #\h #\i))`

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
