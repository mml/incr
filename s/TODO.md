# Compiler Unit Tests TODO

This file lists commented-out unit tests in the compiler passes. These tests are disabled but may be useful for catching regressions or understanding expected behavior.

## parse-and-rename.ss

- **Line 163-166**: `Cond` with two unary clauses - tests cond handling without test expressions

## uncover-free.ss

- **Line 67-74**: Nested lambda expression with closure over free variables - partial test case in comment

## test-driver.ss

- **Line 41**: Debug printf of assembler command - commented debug output
