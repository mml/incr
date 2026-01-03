# Compiler Unit Tests TODO

This file lists commented-out unit tests in the compiler passes. These tests are disabled but may be useful for catching regressions or understanding expected behavior.

## parse-and-rename.ss

- **Lines 283-292**: Complex constant pooling tests - expects `(datum constN ...)` forms
  - Requires constant pooling implementation for quoted pairs, lists, strings, and symbols
  - Currently, each quote of a complex constant produces new runtime construction code
  - Tests pending implementation of compile-time constant pooling

## test-driver.ss

- **Line 41**: Debug printf of assembler command - commented debug output
