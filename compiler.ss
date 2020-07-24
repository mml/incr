(load "test-driver.ss")

(define (compile-program x)
  (define fixnum-shift 2)
  (define (shift n-bits val)
    (arithmetic-shift val n-bits))

  (define (emit-prologue)
    (emit ".arch armv6")
    (emit ".eabi_attribute 28, 1")
    (emit ".eabi_attribute 20, 1")
    (emit ".eabi_attribute 21, 1")
    (emit ".eabi_attribute 23, 3")
    (emit ".eabi_attribute 24, 1")
    (emit ".eabi_attribute 25, 1")
    (emit ".eabi_attribute 26, 2")
    (emit ".eabi_attribute 30, 2")
    (emit ".eabi_attribute 34, 1")
    (emit ".eabi_attribute 18, 4")
    (emit ".file \"foo.ss\"")
    (emit ".text"))

  (define (emit-begin-function name)
    (emit ".align 2")
    (emit ".global ~a" name)
    (emit ".arch armv6")
    (emit ".syntax unified")
    (emit ".arm")
    (emit ".fpu vfp")
    (emit ".type ~a, %function" name)
    (emit "~a:" name))

  (define (emit-end-function name)
    (emit ".size ~a, .-~a" name name))

  (define (immediate-rep x)
    (cond [(integer? x) (shift fixnum-shift x)]
	  [else (error 'compile-program "Unsupported immediate ~s" (pretty-format x))]))

  (define (emit-program x)
    (emit-prologue)

    (emit-begin-function "scheme_entry")
    (emit "mov r0, #~a" (immediate-rep x))
    (emit "bx lr")
    (emit-end-function "scheme_entry")

    (emit ".ident \"mml scheme compiler\"")
    (emit ".section .note.GNU-stack,\"\",%progbits"))

  (emit-program x))
