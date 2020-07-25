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
  (define char-mask #b11111111)
  (define char-tag #b00001111)
  (define char-shift 8)
  (define null-value #b00111111)

  (define (emit-prologue)
    (emit "  .arch armv8-a")
    (emit "  .syntax unified")
    (emit "  .arm")
    (emit "  .fpu vfp")
    (emit "  .eabi_attribute 28, 1")
    (emit "  .eabi_attribute 20, 1")
    (emit "  .eabi_attribute 21, 1")
    (emit "  .eabi_attribute 23, 3")
    (emit "  .eabi_attribute 24, 1")
    (emit "  .eabi_attribute 25, 1")
    (emit "  .eabi_attribute 26, 2")
    (emit "  .eabi_attribute 30, 2")
    (emit "  .eabi_attribute 34, 1")
    (emit "  .eabi_attribute 18, 4")
    (emit "  .text"))

  (define (emit-begin-function name)
    (emit "")
    (emit "/* begin function ~a */" name)
    (emit "  .align 2")
    (emit "  .global ~a" name)
    (emit "  .type ~a, %function" name)
    (emit "~a:" name))

  (define (emit-end-function name)
    (emit ".size ~a, .-~a" name name)
    (emit "/* end function ~a */" name)
    (emit ""))

  (define (immediate? x)
    (cond [(integer? x) #t]
          [(boolean? x) #t]
          [(char? x) #t]
          [(null? x) #t]
          [else #f]))

  (define (primcall? x)
    (if (list? x)
        (cond
          [(null? x) #f]
          [else
            (case (car x)
              [(add1 sub1 integer->char char->integer zero? not) #t]
              [else #f])])
            #f))

  (define primcall-op car)
  (define primcall-operand1 cadr)

  (define (immediate-rep x)
    (cond [(integer? x) (shift fixnum-shift x)]
          [(boolean? x)
           (case x
             [(#f) false-value]
             [(#t) true-value])]
          [(char? x)
           (bitwise-or
             (shift char-shift (char->integer x))
             char-tag)]
          [(null? x) null-value]
          [else (error 'compile-program "Unsupported immediate ~s" (pretty-format x))]))

  (define (padbits x n-digits)
    (list->string
      (reverse
        (take (reverse (append (make-list n-digits #\0) (string->list (format "~b" x))))
              n-digits))))

  (define (register->number s)
    (let ([cs (string->list s)])
      (cond
        [(null? cs) (error 'compile-program "Internal error: empty string is not a valid register")]
        [else
          (case (car cs)
            [(#\r) (string->number (list->string (cdr cs)))]
            [else (error 'compile-program "Don't understand register ~s." s)])])))



  (define emit-move32
    (case-lambda
      [(dest val) (emit-move32 'always dest val)]
      [(condition dest val)
       (let ([cbits (case condition
                      [(always) "1110"]
                      [(eq) "0000"]
                      [(ne) "0001"]
                      [else (error 'compile-program "Unsupported MOV condition ~a" condition)])])
         (define mov  "00110000")
         (define movt "00110100")
         (emit "  /* move32 ~a <- ~a */" dest val)
         (let ([rd (padbits (register->number dest) 4)]
               [lo12 (bitwise-and val #xfff)]
               [lo4 (shift -12 (bitwise-and val #xf000))]
               [hi (shift -16 val)])
           (emit "  /* mov ~a,#~a */" dest (bitwise-and val #xffff))
           (emit "  .word 0b~a~a~a~a~a" cbits mov (padbits lo4 4) rd (padbits lo12 12))
           (unless (zero? hi)
             (let ([hi12 (bitwise-and hi #xfff)]
                   [hi4 (shift -12 (bitwise-and hi #xf000))])
               (emit "  /* movt ~a,#~a */" dest hi)
               (emit "  .word 0b~a~a~a~a~a" cbits movt (padbits hi4 4) rd (padbits hi12 12))))))]))

  (define (emit-move dest val)
    (emit-move32 dest val))

  (define (emit-expr expr)
    (cond
      [(immediate? expr)
       (emit-move "r0" (immediate-rep expr))]
      [(primcall? expr)
       (case (primcall-op expr)
         [(add1)
          (emit-expr (primcall-operand1 expr))
          (emit "add r0,r0,#~a" (immediate-rep 1))]
         [(sub1)
          (emit-expr (primcall-operand1 expr))
          (emit "sub r0,r0,#~a" (immediate-rep 1))]
         [(zero?)
          (emit-expr (primcall-operand1 expr))
          (emit "cmp r0,#~a" (immediate-rep 0))
          (emit-move32 'eq "r0" (immediate-rep #t))
          (emit-move32 'ne "r0" (immediate-rep #f))]
         [(not)
          (emit-expr (primcall-operand1 expr))
          (emit "cmp r0,#~a" (immediate-rep #f))
          (emit-move32 'eq "r0" (immediate-rep #t))
          (emit-move32 'ne "r0" (immediate-rep #f))]
         [(integer->char)
          (emit-expr (primcall-operand1 expr))
          (emit "lsl r0,r0,#~a" (- char-shift fixnum-shift))
          (emit "orr r0,r0,#~a" char-tag)]
         [(char->integer)
          (emit-expr (primcall-operand1 expr))
          (emit "asr r0,r0,#~a" (- char-shift fixnum-shift))]
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
