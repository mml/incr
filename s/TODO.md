# Compiler Unit Tests TODO

This file lists commented-out unit tests in the compiler passes. These tests are disabled but may be useful for catching regressions or understanding expected behavior.

## parse-and-rename.ss

- **Line 108-110**: `Case` with else clause - expects desugaring to let + begin + quoted value
- **Line 111-129**: `Case` with numeric patterns and else - expects complex desugaring with lambda and set!
- **Line 163-166**: `Cond` with two unary clauses - tests cond handling without test expressions
- **Line 169-171**: `Cond` with arrow syntax - tests cond with `=>` receiver syntax

## uncover-free.ss

- **Line 67-74**: Nested lambda expression with closure over free variables - partial test case in comment

## terminals.ss

- **Line 19-21**: Type predicates `list?`, `vector?`, `bytevector?` - marked as not implemented

## test-driver.ss

- **Line 41**: Debug printf of assembler command - commented debug output
