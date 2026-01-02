# Missing R4RS Features

This document tracks features from R4RS (Revised^4 Report on the Algorithmic Language Scheme) that could be added to the compiler.

## Currently Implemented ✓

- **Data types**: Fixnums, booleans, characters, pairs, vectors, strings (as constants)
- **Functions**: Lambda, closures, tail call optimization
- **Bindings**: let, let*, letrec, letrec*, internal definitions
- **Control flow**: if, cond (with =>), case, begin
- **Mutation**: set!, vector-set!
- **Arithmetic**: +, -, *, =, <, >
- **Bitwise operations**: bitwise-arithmetic-shift, bitwise-arithmetic-shift-left, bitwise-arithmetic-shift-right
- **List operations**: car, cdr, cadr, cddr, caddr, cons, null?
- **Type predicates**: zero?, not, null?
- **Character conversion**: char->integer, integer->char

## High-Priority Additions

### 1. Complete Numeric Operations

**Missing primitives**:
```scheme
/                    ; division
quotient             ; integer division
remainder            ; remainder
modulo               ; modulo
<=, >=              ; comparison operators
min, max            ; minimum/maximum
abs                 ; absolute value
odd?, even?         ; parity predicates
positive?, negative? ; sign predicates
```

**Rationale**: Programs constantly need division and comparison. Easy to implement with existing fixnum infrastructure.

**Implementation complexity**: Low (1-2 days)

### 2. Essential List Operations

**Missing primitives**:
```scheme
length              ; list length
list-ref            ; nth element access
list-tail           ; drop n elements
append              ; concatenate lists
reverse             ; reverse a list
memq, memv, member  ; membership tests
assq, assv, assoc   ; association list lookup
```

**Rationale**: Can't write practical list-processing code without these. `append` and `reverse` are foundational.

**Implementation complexity**: Low-Medium (2-3 days)

### 3. Type Predicates

**Missing primitives**:
```scheme
pair?               ; pair type check
symbol?             ; symbol type check
string?             ; string type check
number?, integer?   ; numeric type checks
boolean?            ; boolean type check
char?               ; character type check
procedure?          ; procedure type check
vector?             ; vector type check
```

**Rationale**: Enable runtime type checking and polymorphic code. Trivial to add given existing tagging scheme.

**Implementation complexity**: Very Low (< 1 day)

### 4. Mutation Operations

**Missing primitives**:
```scheme
set-car!            ; mutate car of pair
set-cdr!            ; mutate cdr of pair
string-set!         ; mutate string character
```

**Rationale**: Already have vector-set! and set!. Completes the mutation story.

**Implementation complexity**: Low (1 day)

### 5. String Operations

**Missing primitives**:
```scheme
string-length       ; length of string
string-ref          ; access character at index
string-set!         ; mutate character at index
string-append       ; concatenate strings
substring           ; extract substring
string=?            ; string equality
string<?, string>?  ; string comparison
string<=?, string>=?
```

**Rationale**: Strings exist but are read-only constants. These operations unlock string manipulation.

**Implementation complexity**: Medium (2-3 days, requires runtime support)

### 6. apply

**Missing primitive**:
```scheme
(apply + '(1 2 3))  ; => 6
(apply cons '(1 (2 3)))  ; => (1 2 3)
```

**Rationale**: Essential for higher-order programming. Enables writing variadic function wrappers.

**Implementation complexity**: Medium-High (3-5 days, requires calling convention changes)

## Medium Priority

### 7. Basic I/O

**Missing primitives**:
```scheme
display             ; output human-readable form
newline             ; output newline
write               ; output machine-readable form
read-char           ; read single character
write-char          ; write single character
peek-char           ; peek at next character
eof-object?         ; test for EOF
```

**Rationale**: Requires C runtime extensions but unlocks interactive programs. Start with just display/newline.

**Implementation complexity**: Medium (requires C runtime integration)

### 8. quasiquote (Backquote)

**Missing syntax**:
```scheme
`(a ,b ,@c)         ; quasiquote with unquote/unquote-splicing
```

**Rationale**: Makes template-based list construction much cleaner. Essential for macro systems.

**Implementation complexity**: Medium (parser/macro expansion changes)

### 9. call-with-current-continuation (call/cc)

**Missing primitive**:
```scheme
(call/cc (lambda (k) ...))  ; first-class continuations
```

**Rationale**: The "killer feature" of Scheme. Enables generators, exceptions, backtracking, coroutines. Excellent learning opportunity for advanced compiler techniques.

**Implementation complexity**: High (5+ days, requires CPS transformation or stack manipulation)

## Lower Priority

### 10. Character Predicates

```scheme
char=?, char<?, char>?, char<=?, char>=?
char-alphabetic?, char-numeric?, char-whitespace?
char-upper-case?, char-lower-case?
char-upcase, char-downcase
```

**Implementation complexity**: Low-Medium

### 11. File I/O

```scheme
open-input-file, open-output-file
close-input-port, close-output-port
read, write
```

**Implementation complexity**: Medium-High (requires port abstraction)

### 12. Metaprogramming

```scheme
eval                ; evaluate s-expression
load                ; load and execute file
```

**Rationale**: Requires embedding the compiler or interpreter.

**Implementation complexity**: High

### 13. Extended Numeric Tower

```scheme
; Rational numbers
numerator, denominator, rationalize

; Floating-point
exact->inexact, inexact->exact
floor, ceiling, truncate, round
sin, cos, tan, exp, log, sqrt
```

**Rationale**: Significant complexity; would require implementing new numeric representations.

**Implementation complexity**: Very High

## Recommended Implementation Order

**Phase 1** (Foundation - 4-7 days):
1. Type predicates (#3)
2. Complete numeric operations (#1)
3. Essential list operations (#2)
4. Mutation operations (#4)

**Phase 2** (Practical Programming - 5-8 days):
5. String operations (#5)
6. Basic I/O (#7)
7. apply (#6)

**Phase 3** (Advanced Features - 5+ days):
8. quasiquote (#8)
9. Character predicates (#10)
10. call/cc (#9)

**Phase 4** (Complete R4RS - variable):
11. File I/O (#11)
12. eval/load (#12)
13. Extended numeric tower (#13)

## Notes

- The compiler already has sophisticated closure conversion, tail call optimization, and proper mutation handling via boxing
- The tagging scheme (2-bit tags for immediates, 3-bit tags for heap objects) is well-designed for type predicates
- Many of these features can be implemented incrementally without disrupting existing functionality
- Focus on features that unlock practical programming before advanced metaprogramming features
