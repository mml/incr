# RISC-V 64-bit Code Generation Gotchas

When porting ARM32 code to RISC-V or implementing new primitives
for rv64le.def, watch for these common pitfalls.

## Comment Syntax

**Critical difference**: ARM32 and RISC-V use different comment
characters in assembly.

```scheme
; ARM32 uses @
(emit "  @ numeric equality {{{")

; RISC-V uses #
(emit "# numeric equality {{{")
```

Forgetting this causes assembler errors: "junk at end of line,
first unrecognized character is `@'". Easy to miss when copying
ARM32 patterns verbatim.

## Wordsize-Aware Heap Advancement (Critical Bug Risk)

ARM32 and RISC-V have different wordsizes:
- ARM32: 4 bytes per word
- RISC-V: 8 bytes per word

Heap allocations that store multiple fields must advance the
heap pointer by `(* 2 (wordsize))`, not hardcoded bytes.

**Example - make-ratnum**:

Stores numerator (1 word) and denominator (1 word) = 2 words
total.

**Wrong (causes silent data corruption)**:
```scheme
(emit "  addi ~a,~a,8" heap-register heap-register)  ; only 1 word!
```

**Correct**:
```scheme
(emit "  addi ~a,~a,~a" heap-register heap-register
      (* 2 (constant wordsize)))  ; 2 words
```

Without this fix, consecutive allocations overlap:
- Allocation 1: heap[0] = num, heap[8] = denom
- Allocation 2: heap[8] = num (overwrites allocation 1's denom!),
  heap[16] = denom

Tests with single allocations pass; tests with multiple
allocations fail mysteriously. Always verify heap advancement
matches allocation size.

## Tag Clearing Pattern

ARM32 uses BIC (bit clear) instruction. RISC-V must use
load-immediate + AND.

**ARM32 pattern** (don't use on RISC-V):
```scheme
(emit "  BIC r4, r3, #~a" (constant ratnum-tag))
```

**RISC-V idiomatic pattern**:
```scheme
(emit "li s6, ~a" (bitwise-not (constant ptr-mask)))  ; s6 = ~ptr-mask
(emit "and s4, a3, s6")                                ; s4 = a3 & s6
```

where `ptr-mask` is the constant `#b111` (defined in
compile-shared.ss).

Why not `xor`? While XOR technically works:
```scheme
(emit "xor s4, a3, s6")  ; Works but not idiomatic
```

The AND pattern is:
- Clearer intent (clear specific bits)
- More efficient (no extra register for inverted mask)
- Consistent with RISC-V conventions

## Register Allocation

**Callee-saved registers** (safe to use, must preserve):
- `s0-s11` (but some reserved: `s10`=closure-register,
  `s11`=heap-register)
- Use `s4-s9` for temporaries in with-saved-registers

**Caller-saved temporaries** (don't need to preserve):
- `t0-t6`

**Argument/return**:
- `a0-a7` (argument registers, caller-saved)

The `with-saved-registers` macro automatically saves/restores
your temporary registers.

## Load Immediate (`li`) Behavior

`li` is a pseudo-instruction that expands based on the value:

```scheme
(emit "li s6, 5")              ; expands to single movw or addi
(emit "li s6, -8")             ; expands to lui + addi
(emit "li s6, ~a" (bitwise-not #b111))  ; expands to lui + addi
```

Large immediates (outside 12-bit signed range) expand to
multiple instructions. This is transparent but explains apparent
"bloat" in generated code. The assembler handles it.

## Comparison and Branch Patterns

RISC-V branches perform comparison implicitly (no separate CMP
instruction).

**Branch instructions**:
- `beq a2, a3, label` - branch if a2 == a3
- `bne a2, a3, label` - branch if a2 != a3
- `blt a2, a3, label` - branch if a2 < a3 (signed)

**Order matters**: `beq a2, a3` and `beq a3, a2` are equivalent,
but when reading code, order reflects what's being tested.

## Offset Addressing

Load/store instructions use different addressing syntax than ARM32.

```scheme
; Load from base + offset
(emit "ld a3, 0(s4)")              ; load from s4 + 0
(emit "ld a3, ~a(s4)" offset-var)  ; load from s4 + offset-var

; Store to base + offset
(emit "sd a0, (~a)" heap-register)  ; store to heap-register + 0
(emit "sd a0, ~a(~a)" (wordsize) heap-register)
```

Offsets go in the immediate field, not as separate operands.

## When to Consult This Document

- Porting primitives from `arm32le.def` to `rv64le.def`
- Implementing new allocation primitives (vectors, strings, ratnums)
- Adding comparison/equality operations
- Debugging silent data corruption or type errors
- Understanding register usage patterns in existing code

Cross-reference with CLAUDE.md for general compiler architecture
and passes.
