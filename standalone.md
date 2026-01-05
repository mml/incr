# Standalone Scheme Tool (`ins`) - Comprehensive Specification

## Overview

Build `ins` (Incremental Scheme), a standalone command-line tool
for compiling and running Scheme programs without the makefile
test infrastructure. The tool will be architecture-specific
(one binary per workarea: `arm32le/ins`, `rv64le/ins`).

## Goals

1. **Quick experimentation**: `ins -e "(+ 1 2)"` compiles, runs,
   prints result
2. **File compilation**: `ins program.scm` creates executable
3. **Compiler inspection**: `ins -S program.scm` shows generated
   assembly
4. **Shared infrastructure**: Extract common code from
   test-driver.ss to avoid duplication

## User Interface

### Command-Line Syntax

```bash
ins [options] <input>
ins [options] -e <expression>
ins [options] -
```

### Input Modes

| Mode | Example | Behavior |
|------|---------|----------|
| **Expression** | `ins -e "(+ 1 2)"` | Compile expr, run,
auto-print result |
| **File** | `ins program.scm` | Compile file, create
a.out, run, print result |
| **Stdin** | `echo "(+ 1 2)" \| ins -` | Read from stdin until
EOF, compile, run |

### Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--eval <expr>` | `-e` | Evaluate expression (auto-print
result) |
| `--compile-only` | `-S` | Generate assembly only (stops
after .s file) |
| `--no-link` | `-c` | Generate object file only (stops
after .o file) |
| `--output <file>` | `-o` | Specify output executable name
(default: a.out) |
| `--keep-temps` | `-k` | Preserve intermediate .s/.o files
after execution |
| `--verbose` | `-v` | Show commands executed (as, gcc,
etc.) |
| `--passes` | | Show all compiler pass inputs/outputs
(debug feature) |

**Future extension**: `--passes=parse,closure` to show only
specific passes (deferred to later phase).

### File Naming Conventions

**Expression mode** (`-e`):
- Assembly: `a.s` (or kept if `--keep-temps`)
- Object: `a.o` (or kept if `--keep-temps`)
- Executable: `a.out` (or as specified by `-o`)

**File mode** (`program.scm`):
- Assembly: `program.s` (or kept if `--keep-temps`)
- Object: `program.o` (or kept if `--keep-temps`)
- Executable: `a.out` (or as specified by `-o`)

**Stdin mode** (`-`):
- Same as expression mode: `a.s`, `a.o`, `a.out`

**Temp file cleanup**:
- By default: `.s` and `.o` files deleted after successful
  compilation/linking
- With `--keep-temps`: All intermediate files preserved
- With `-S`: Only `.s` file created (no assembly/linking)
- With `-c`: Only `.s` and `.o` created (no linking)

## Behavior Details

### Auto-Print in Expression Mode

When using `-e`, the tool automatically prints the result of the
expression using the existing C runtime `print_ptr` function:

```bash
$ ins -e "(+ 1 2)"
3

$ ins -e "(cons 1 2)"
(1 . 2)
```

**Multi-expression handling**: Print only the last value:

```bash
$ ins -e "(define x 5) (+ x 1)"
6
```

The intermediate `(define x 5)` returns void (not printed), only
the final expression result is printed.

### Default Execution Behavior

| Command | Compile | Link | Run | Output |
|---------|---------|------|-----|--------|
| `ins -e "(+ 1 2)"` | ✓ | ✓ | ✓ | Prints `3` |
| `ins file.scm` | ✓ | ✓ | ✓ | Prints result |
| `ins -S file.scm` | ✓ | ✗ | ✗ | Creates `file.s` |
| `ins -c file.scm` | ✓ | ✓ | ✗ | Creates `file.o` |
| `ins -o prog file.scm` | ✓ | ✓ | ✓ | Creates `prog`,
prints result |
| `ins -k file.scm` | ✓ | ✓ | ✓ | Keeps `file.s`,
`file.o`, `a.out` |

**Default is interpreter-like**: Compile, link, run, show result.
Use flags to stop at intermediate stages.

### No Arguments

```bash
$ ins
Usage: ins [options] <input>
...
```

Shows help message with usage examples.

### Exit Codes

- **0**: Success (compilation and execution succeeded)
- **Non-zero**: Pass through exit code from compiled program or
  compilation error

If the compiled program exits with code 42, `ins` exits with 42.
If compilation fails, `ins` exits with non-zero (typically 1).

### Error Handling

- **Missing driver.o**: Error message: "C runtime not built. Run:
  make -C arm32le/c"
- **Compilation errors**: Raw Racket exception traces (includes
  compiler pass information)
- **Assembly/linking errors**: Show gcc/as error output directly

### Verbose Mode

With `-v`, show executed commands:

```bash
$ ins -v -e "(+ 1 2)"
[ins] Compiling expression...
[ins] Assembling: arm-linux-gnueabihf-as -g -o a.o a.s
[ins] Linking: arm-linux-gnueabihf-gcc -DNO_NEWLINE -static -g -o
a.out /abs/path/to/arm32le/c/driver.o a.o
[ins] Running: ./a.out
3
[ins] Cleaning up: rm a.s a.o
```

## Architecture

### Directory Structure

```
incr/
├── s/
│   ├── compile-driver.ss     # NEW: Shared compilation functions
│   ├── ins.ss                # NEW: Standalone tool main script
│   ├── config.ss.in          # MODIFIED: Add driver-object-path
│   ├── test-driver.ss        # MODIFIED: Use compile-driver.ss
│   └── ...
├── arm32le/
│   ├── s/
│   │   ├── config.ss         # GENERATED: arm-linux-gnueabihf-*
paths + driver.o path
│   │   └── ...
│   ├── c/
│   │   └── driver.o          # Precompiled C runtime
│   └── ins                   # Executable script (Racket or raco
exe)
└── rv64le/
    ├── s/
    │   ├── config.ss         # GENERATED: riscv64-linux-gnu-*
    paths + driver.o path
    │   └── ...
    └── ins                   # Executable script
```

### Toolchain Path Discovery

**Problem**: ins needs to know the cross-compiler paths
(arm-linux-gnueabihf-gcc, riscv64-linux-gnu-gcc) and driver.o
location.

**Solution**: Extend existing `config.ss` generation in configure
script:

1. **Modify template**: `s/config.ss.in`
   ```scheme
   #lang racket
   (provide c-compiler-path)
   (provide assembler-path)
   (provide driver-object-path)

   (define c-compiler-path "${CC}")
   (define assembler-path "${AS}")
   (define driver-object-path "${DRIVER_O}")
   ```

2. **Update configure script** (around line 100):
   ```bash
   perl -p -e 's/\${CC}/'$CC'/g;' \
           -e 's/\${AS}/'$AS'/g;' \
           -e 's|\${DRIVER_O}|'$(pwd)'/'$w'/c/driver.o|g;' \
     < s/config.ss.in > $w/s/config.ss
   ```

3. **ins.ss requires**: `(require "config.ss")` (already
   generated per-workarea)

This reuses existing infrastructure:
- configure already generates config.ss per workarea
- No new files needed
- Absolute path to driver.o embedded during configure

### Shared Code Module: `compile-driver.ss`

Extract from test-driver.ss into `s/compile-driver.ss`:

**Exported functions**:
```scheme
(provide compile-to-assembly)   ; expr* → writes .s file
(provide assemble)               ; .s → .o
(provide link)                   ; .o + driver.o → executable
(provide cleanup-files)          ; delete temp files
```

Note: `run-and-capture` is NOT shared because:
- test-driver.ss needs to capture output for assertion comparison
- ins.ss just runs subprocess with stdout to terminal (no capture)
- Different requirements mean no code sharing needed

**Function signatures**:

```scheme
;; Compile Scheme expressions to assembly file
(define (compile-to-assembly expr* output-path)
  ; Calls compile-program, writes to output-path
  ; Uses existing pass pipeline
  ...)

;; Assemble .s to .o
(define (assemble as-path s-path o-path verbose?)
  ; Invokes: as-path -g -o o-path s-path
  ; If verbose?, prints command
  ; Errors on failure
  ...)

;; Link .o + driver.o to executable
(define (link cc-path driver-o-path o-path exe-path verbose?)
  ; Invokes: cc-path -DNO_NEWLINE -static -g -o exe-path
driver-o-path o-path
  ; If verbose?, prints command
  ; Errors on failure
  ...)

;; Delete intermediate files
(define (cleanup-files paths)
  ; Delete files in paths list if they exist
  ...)
```

**Modified test-driver.ss**:
- Requires `compile-driver.ss`
- Simplifies `run-compile`, `assemble`, `build`, `execute` to call
  shared functions
- Preserves test-specific logic (test-case macros, output
  comparison)

### Main Tool: `ins.ss`

**Script structure**:

```scheme
#lang racket

(require "compile-driver.ss")
(require "config.ss")
(require "compiler.ss")
(require racket/cmdline)

;; Command-line argument parsing
(define eval-expr #f)
(define input-file #f)
(define use-stdin #f)
(define output-exe "a.out")
(define compile-only #f)    ; -S flag
(define no-link #f)         ; -c flag
(define keep-temps #f)      ; -k flag
(define verbose #f)         ; -v flag
(define show-passes #f)     ; --passes flag

;; Parse command-line arguments using racket/cmdline
(command-line
 #:program "ins"
 #:once-each
 [("-e" "--eval") expr "Evaluate expression"
  (set! eval-expr expr)]
 [("-o" "--output") file "Output executable name"
  (set! output-exe file)]
 [("-S" "--compile-only") "Generate assembly only"
  (set! compile-only #t)]
 [("-c" "--no-link") "Generate object file only"
  (set! no-link #t)]
 [("-k" "--keep-temps") "Keep intermediate files"
  (set! keep-temps #t)]
 [("-v" "--verbose") "Show executed commands"
  (set! verbose #t)]
 [("--passes") "Show compiler pass output"
  (set! show-passes #t)]
 #:args (input-args)
 (cond
   [(null? input-args) (show-usage-and-exit)]
   [(equal? (car input-args) "-") (set! use-stdin #t)]
   [else (set! input-file (car input-args))]))

;; Main compilation pipeline
(define (main)
  (let* ([exprs (get-input-expressions)]
         [base-name (compute-base-name)]
         [s-file (string-append base-name ".s")]
         [o-file (string-append base-name ".o")]
         [exe-file (if (member output-exe '("a.out"))
                       output-exe
                       (if compile-only s-file
                           (if no-link o-file output-exe)))])

    ;; Compilation
    (when verbose (printf "[ins] Compiling...~n"))
    (when show-passes (trace-all-passes!))
    (compile-to-assembly exprs s-file)

    ;; Assembly (unless -S)
    (unless compile-only
      (assemble assembler-path s-file o-file verbose))

    ;; Linking (unless -S or -c)
    (unless (or compile-only no-link)
      (link c-compiler-path driver-object-path o-file exe-file
verbose))

    ;; Execution (unless -S, -c, or custom -o)
    (when (and (not compile-only)
               (not no-link)
               (should-auto-run?))
      (run-executable exe-file))

    ;; Cleanup (unless -k or intermediate flags)
    (unless (or keep-temps compile-only no-link)
      (cleanup-files (list s-file o-file)))))

(main)
```

**Helper functions**:

```scheme
;; Determine if we should auto-run the compiled program
(define (should-auto-run?)
  ; Run if:
  ; - Not stopped at intermediate stage (-S/-c)
  ; - No custom output specified (or output is a.out)
  (and (not compile-only)
       (not no-link)
       (equal? output-exe "a.out")))

;; Get input expressions based on mode
(define (get-input-expressions)
  (cond
    [eval-expr (read (open-input-string eval-expr))]
    [use-stdin (port->string (current-input-port))]
    [input-file (read-file input-file)]))

;; Compute base name for intermediate files
(define (compute-base-name)
  (cond
    [eval-expr "a"]
    [use-stdin "a"]
    [input-file (path-replace-extension input-file "")]))

;; Read all expressions from file
(define (read-file path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([exprs '()])
        (let ([expr (read port)])
          (if (eof-object? expr)
              (reverse exprs)
              (loop (cons expr exprs))))))))

;; Run executable without capturing output
(define (run-executable exe-path)
  ; Runs exe-path with stdout/stderr going to terminal
  ; Pass through exit code
  (unless (system exe-path)
    (exit 1)))
```

### Dual Build Modes

**During development** (interpreted):
```bash
#!/usr/bin/env racket
#lang racket
; ins.ss contents
```

Make executable: `chmod +x arm32le/ins`

**For production** (compiled):
```bash
raco exe -o arm32le/ins s/ins.ss
```

Makefile target in `s/Mf-base`:
```makefile
ins: ins.ss compile-driver.ss config.ss
    raco exe -o ../ins ins.ss
```

## Implementation Plan

### Phase 1: Shared Module Extraction

**Files to create**:
- `s/compile-driver.ss` - Extract functions from test-driver.ss

**Files to modify**:
- `s/test-driver.ss` - Require compile-driver.ss, use shared
  functions

**Functions to extract**:
1. `compile-to-assembly` (based on `run-compile`)
2. `assemble` (extract from `assemble`)
3. `link` (extract from `build`, generalize)
4. `cleanup-files` (new helper)

Note: `run-and-capture` is NOT extracted - test-driver keeps it
for output comparison, ins doesn't need it.

**Key changes**:
- Generalize path handling (remove parameter dependencies)
- Add verbose flag support
- Explicit error handling

### Phase 2: Toolchain Configuration

**Files to modify**:
- `s/config.ss.in` - Add `driver-object-path` provider
- `configure` - Add DRIVER_O substitution when generating
  config.ss

**configure script changes** (around line 100-102):
```bash
# Change from:
perl -p -e 's/\${CC}/'$CC'/g;' \
        -e 's/\${AS}/'$AS'/g;' \
  < s/config.ss.in > $w/s/config.ss

# To:
perl -p -e 's/\${CC}/'$CC'/g;' \
        -e 's/\${AS}/'$AS'/g;' \
        -e 's|\${DRIVER_O}|'$(pwd)'/'$w'/c/driver.o|g;' \
  < s/config.ss.in > $w/s/config.ss
```

This reuses existing config.ss generation rather than creating
new files.

### Phase 3: Standalone Tool (Core)

**Files to create**:
- `s/ins.ss` - Main standalone tool script

**Core features**:
1. Command-line argument parsing (racket/cmdline)
2. Input mode detection (-e, file, stdin)
3. Compilation pipeline orchestration
4. File naming logic
5. Temp file cleanup

**Dependencies**:
- `compile-driver.ss` (shared functions)
- `config.ss` (toolchain paths - already generated by configure)
- `compiler.ss` (existing compiler entry point)

### Phase 4: Flags and Features

**Implement flags**:
1. `-S` (compile-only) - Stop after assembly generation
2. `-c` (no-link) - Stop after object file generation
3. `-o` (output) - Custom executable name
4. `-k` (keep-temps) - Preserve intermediate files
5. `-v` (verbose) - Show executed commands

**Auto-execution logic**:
- Run by default unless `-S`, `-c`, or custom `-o` specified
- Auto-print result using C runtime print_ptr

### Phase 5: Integration and Testing

**Makefile integration**:
1. Add `ins` target to `s/Mf-base`
2. Add `config.ss` dependency (already exists)
3. Optional: Add raco exe compilation target

**Testing approach**:
1. Test with existing test cases: `ins t/fixnums.ss`
2. Verify expression mode: `ins -e "(+ 1 2)"`
3. Test flag combinations: `ins -S -k file.scm`
4. Compare test-driver.ss output with ins output (should be
   identical)

### Phase 6: Advanced Features (Deferred)

**Future enhancements**:
1. `--passes=parse,closure` - Selective pass tracing
2. Command-line argument support for Scheme programs (argv)
3. REPL mode (interactive read-eval-print loop)
4. Preamble/standard library auto-inclusion
5. Compilation caching (~/.cache/incr/)

## Success Criteria

1. **Functionality**: `ins -e "(+ 1 2)"` prints `3`
2. **File compilation**: `ins program.scm` creates and runs `a.out`
3. **Intermediate outputs**: `ins -S file.scm` produces `file.s`
4. **Code sharing**: test-driver.ss uses same compilation pipeline
   as ins
5. **No duplication**: Toolchain paths defined once in Makefiles
6. **Dual mode**: Works both as interpreted script and compiled
   executable
7. **Both architectures**: Works identically in arm32le and rv64le
   workareas

## Design Rationale

### Why Extract Shared Code?

Matt prioritized avoiding duplication over other approaches. This
ensures:
- Test suite validates same compilation path users experience
- Bug fixes in compilation logic apply to both tools
- Single source of truth for linking/assembly logic

### Why Generate arch-tools.ss from Makefile?

Alternative considered: Hardcode toolchain paths in ins.ss.

Rejected because:
- Duplication: Makefile already has this information
- Drift risk: Changes to cross-compiler names require multiple
  edits
- Absolute driver.o path: Generated file can include absolute path
  to driver.o

### Why Interpreter-Like Default Behavior?

Default compile+run behavior optimizes for quick experimentation:
- `ins -e "(+ 1 2)"` immediately shows result
- `ins file.scm` immediately runs the program
- Matches user expectations from Python, Ruby, etc.

Traditional compiler behavior (create binary, don't run) available
via explicit `-o` flag.

### Why Keep Temps by Default (Then Delete)?

Current-directory intermediate files (test.scm → test.s, test.o)
enable easy debugging:
- `cat test.s` to inspect generated assembly
- `objdump -d test.o` to verify object code

But cleanup by default prevents workspace clutter. Use `-k` when
debugging compiler.

### Why Raw Racket Exception Traces?

Alternative considered: Filter exceptions to show user-friendly
messages.

Deferred because:
- Compiler is evolving - error types change frequently
- Users (Matt) are familiar with compiler internals
- Exception traces show which pass failed (valuable for debugging)
- Can be improved incrementally based on usage

## Open Questions (Resolved During Interview)

1. ✓ Tool name: `ins` (incremental scheme)
2. ✓ Build per-arch: Yes, one binary per workarea
3. ✓ Auto-print in -e mode: Yes, print last value
4. ✓ Default behavior: Compile and run (interpreter-like)
5. ✓ Temp file location: Current directory (match source name)
6. ✓ Code sharing approach: Extract to compile-driver.ss
7. ✓ Cross-compiler discovery: Generate arch-tools.ss from
   Makefile
8. ✓ Error handling: Raw Racket traces (simple, informative)
9. ✓ Exit codes: Pass through from compiled program
10. ✓ Stdin support: Yes, with `-` flag
11. ✓ No args behavior: Show usage help
12. ✓ Verbose mode: Yes, `-v` flag
13. ✓ argv support: No, not initially
14. ✓ Preamble support: No, future project

## Critical Files

**New files**:
- `s/compile-driver.ss` - Shared compilation functions
- `s/ins.ss` - Standalone tool script

**Modified files**:
- `s/config.ss.in` - Add driver-object-path provider
- `configure` - Add DRIVER_O substitution
- `s/test-driver.ss` - Use compile-driver.ss shared functions
- `s/Mf-base` - Add ins build target

**Generated files** (per workarea):
- `arm32le/s/config.ss` - ARM32 toolchain + driver.o paths
  (extended)
- `rv64le/s/config.ss` - RISC-V toolchain + driver.o paths
  (extended)
- `arm32le/ins` - ARM32 executable/script
- `rv64le/ins` - RISC-V executable/script
