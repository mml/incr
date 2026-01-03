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

This will show you which pass is failing and what primitive or feature needs to be implemented.

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

**Testing:**
- No unit tests for `.def` files - only integration tests in `t/`
- Run `make unit` in `{arch}/s/` to test compiler passes
- Run `make test t=file.ss` in `{arch}/t/` to test code generation
- Test on BOTH arm32le and rv64le
