# 2026 Compiler Progress

## Major Features Implemented

### Ratnum (Rational Numbers) Support
- Ratnum heap objects with proper tagging for exact arithmetic
- Division operator `/` with automatic GCD reduction to canonical form
- Ratnum literals: `2/3`, `-5/7`, etc.
- Numeric equality `=` with fixnum/ratnum dispatch
- All comparison operators: `<`, `>`, `<=`, `>=` with cross-product algorithm
- Type predicates: `rational?`, `number?`, `exact?`, `integer?`
- Accessors: `numerator`, `denominator`
- **Still missing**: Arithmetic operations (+, -, *, /) with ratnum operands

### Numeric Primitives (9 operators)
- Binary comparisons: `<=`, `>=`
- Binary operations: `min`, `max`
- Unary operations: `abs`
- Arithmetic predicates: `odd?`, `even?`, `positive?`, `negative?`
- Implementations optimized for both ARM32 and RISC-V

### Type Predicates
- `list?` and `vector?` type checking
- Full numeric type hierarchy: `rational?`, `number?`, `exact?`, `integer?`

### Vector Literal Support
- Vector literals `'#(a b c)` now compile
- Compiler transformation for `(vector a b c)` constructor

## Testing Infrastructure

**Unit Tests**: ~22 passing in compiler passes (`s/`)
**Integration Tests**: ~570 passing across 14 test files (`t/`)

Newly enabled test suites:
- Vector literal support tests
- Type predicate tests (`list?`, `vector?`)
- Large vector test (10,000 elements)
- Ratnum equality and comparison tests (86+ cases)
- Numeric primitive tests (67+ cases)

## Infrastructure Improvements

- Synchronized `debug/setup.gdb` with `c/driver.c` runtime
- Added ratnum support to GDB debugging helpers
- Fixed symbol pointer tag masking
- Documented RISC-V-specific code generation gotchas
- Documented architecture patterns and code generation recipes

## Next Steps

**High Priority**: Ratnum arithmetic (+, -, *, /)
**Medium Priority**: Remaining type predicates (pair?, symbol?, string?, boolean?, char?, procedure?)
**Infrastructure Gap**: Variadic procedures and standard library preamble
