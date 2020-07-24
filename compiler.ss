(load "test-driver.ss")

(define (compile-program x)
  (define (shift n-bits val)
    (arithmetic-shift val n-bits))
  (define bitwise-or bitwise-ior)

  ;;; Fixnums end in #b00.
  ;;; All other types end in #b1111
  (define fixnum-shift 2)
  (define boolean-mask #b10111111)
  (define boolean-tag #b00101111)
  (define false-value (bitwise-or #b1111 (shift 4 #b0010)))
  (define true-value (bitwise-or #b1111 (shift 4 #b0110)))

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

  (define (immediate? x)
    (cond [(integer? x) #t]
          [(boolean? x) #t]
          [else #f]))

  (define (primcall? x)
    (if (list? x)
        (case (car x)
          [(add1) #t]
          [else #f])
        #f))

  (define primcall-op car)
  (define primcall-operand1 cadr)

  (define (immediate-rep x)
    (cond [(integer? x) (shift fixnum-shift x)]
          [(boolean? x)
           (case x
             [(#f) false-value]
             [(#t) true-value])]
          [else (error 'compile-program "Unsupported immediate ~s" (pretty-format x))]))

  (define (emit-expr expr)
    (cond
      [(immediate? expr)
       (emit "mov r0, #~a" (immediate-rep expr))]
      [(primcall? expr)
       (case (primcall-op expr)
         [(add1)
          (emit-expr (primcall-operand1 expr))
          (emit "add r0,r0,#~a" (immediate-rep 1))]
         [else (error 'compile-program "Unsupported primcall in ~s" (pretty-format expr))])]
      [else (error 'compile-program "Unsupported expression ~s" (pretty-format expr))]))

  (define (emit-program x)
    (emit-prologue)

    (emit-begin-function "scheme_entry")
    (emit-expr x)

    (emit "bx lr")
    (emit-end-function "scheme_entry")

    (emit ".ident \"mml scheme compiler\"")
    (emit ".section .note.GNU-stack,\"\",%progbits"))

  (emit-program x))
