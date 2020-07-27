(load "test-driver.ss")

(define (compile-program x)
  (define (shift n-bits val)
    (arithmetic-shift val n-bits))
  (define bitwise-or bitwise-ior)
  (define (wordsize) 4)
  (define (empty-env) '())
  (define (extend-env name index env)
    (cons (cons name index) env))
  (define (lookup x env)
    (cdr (assoc x env)))

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

  (define (make-labeler)
    (let ([x 0])
      (lambda ()
        (set! x (+ x 1))
        (format "l~a" x))))
  (define unique-label (make-labeler))

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
    (emit-label name))

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
              [(add1 sub1 integer->char char->integer zero? not null? + - = * <) #t]
              [else #f])])
            #f))

  (define (let? x)
    (and (list? x)
         (eq? 'let (car x))))
  (define (variable-ref? x)
    (symbol? x))

  (define let-bindings cadr)
  (define let-body caddr)
  (define lhs car)
  (define rhs cadr)

  (define (if? x)
    (and (list? x)
         (eq? 'if (car x))))

  (define if-test cadr)
  (define if-conseq caddr)
  (define if-altern cadddr)

  (define primcall-op car)
  (define primcall-operand1 cadr)
  (define primcall-operand2 caddr)

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
        [(null? cs) (error 'compile-program
                           "Internal error: empty string is not a valid register")]
        [else
          (case (car cs)
            [(#\r) (string->number (list->string (cdr cs)))]
            [else (error 'compile-program "Don't understand register ~s." s)])])))

  (define emit-b
    (case-lambda
      [(addr) (emit-bx 'always addr)]
      [(condition addr)
       (let ([instruction (case condition
                            [(always) "b"]
                            [(eq) "beq"])])
         (emit "  ~a ~a" instruction addr))]))

  (define emit-move32
    (case-lambda
      [(dest val) (emit-move32 'always dest val)]
      [(condition dest val)
       (let ([cbits (case condition
                      [(always) "1110"]
                      [(eq) "0000"]
                      [(ne) "0001"]
                      [(ge) "1010"]
                      [(lt) "1011"]
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
               (emit "  .word 0b~a~a~a~a~a" cbits movt (padbits hi4 4) rd (padbits hi12 12))))))]
      ))

  (define (emit-move dest val)
    (emit-move32 dest val))

  (define (emit-label l)
    (emit "~a:" l))

  (define (emit-primitive-call expr si env)
    (case (primcall-op expr)
      [(add1)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "add r0,r0,#~a" (immediate-rep 1))]
      [(sub1)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "sub r0,r0,#~a" (immediate-rep 1))]
      [(zero?)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "cmp r0,#~a" (immediate-rep 0))
       (emit-move32 'eq "r0" (immediate-rep #t))
       (emit-move32 'ne "r0" (immediate-rep #f))]
      [(not)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "cmp r0,#~a" (immediate-rep #f))
       (emit-move32 'eq "r0" (immediate-rep #t))
       (emit-move32 'ne "r0" (immediate-rep #f))]
      [(null?)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "cmp r0,#~a" (immediate-rep '()))
       (emit-move32 'eq "r0" (immediate-rep #t))
       (emit-move32 'ne "r0" (immediate-rep #f))]
      [(integer->char)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "lsl r0,r0,#~a" (- char-shift fixnum-shift))
       (emit "orr r0,r0,#~a" char-tag)]
      [(char->integer)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "asr r0,r0,#~a" (- char-shift fixnum-shift))]
      [(+)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "  str r0, [sp,#~a]" si)
       (emit-expr (primcall-operand2 expr) (- si (wordsize)) env)
       (emit "  ldr r1, [sp,#~a]" si)
       (emit "  add r0,r0,r1")]
      [(-)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "  str r0, [sp,#~a]" si)
       (emit-expr (primcall-operand2 expr) (- si (wordsize)) env)
       (emit "  ldr r1, [sp,#~a]" si)
       (emit "  sub r0,r1,r0")]
      [(=)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "  str r0, [sp,#~a]" si)
       (emit-expr (primcall-operand2 expr) (- si (wordsize)) env)
       (emit "  ldr r1, [sp,#~a]" si)
       (emit "  cmp r0,r1")
       (emit-move32 'eq "r0" (immediate-rep #t))
       (emit-move32 'ne "r0" (immediate-rep #f))]
      [(<)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "  str r0, [sp,#~a]" si)
       (emit-expr (primcall-operand2 expr) (- si (wordsize)) env)
       (emit "  ldr r1, [sp,#~a]" si)
       (emit "  cmp r1,r0")
       (emit-move32 'lt "r0" (immediate-rep #t))
       (emit-move32 'ge "r0" (immediate-rep #f))]
      [(*)
       (emit-expr (primcall-operand1 expr) si env)
       (emit "  str r0, [sp,#~a]" si)
       (emit-expr (primcall-operand2 expr) (- si (wordsize)) env)
       (emit "  ldr r1, [sp,#~a]" si)
       (emit "  asr r0,r0,#~a" fixnum-shift) ; Can we combine with ldr?
       (emit "  mul r0,r0,r1")]
      [else (error 'compile-program "Unsupported primcall in ~s" (pretty-format expr))]))

  (define (emit-let bindings body si env)
    (let f ([b* bindings]
            [new-env env]
            [si si])
      (cond
        [(null? b*) (emit-expr body si new-env)]
        [else
          (let ([b (car b*)])
            (emit-expr (rhs b) si env)
            (emit "  str r0, [sp,#~a]" si)
            (f (cdr b*)
               (extend-env (lhs b) si new-env)
               (- si (wordsize))))])))

  (define (emit-if test conseq altern si env)
    (let ([L0 (unique-label)]
          [L1 (unique-label)])
      (emit-expr test si env)
      (emit "  cmp r0,#~a" (immediate-rep #f))
      (emit-b 'eq L0)
      (emit-expr conseq si env)
      (emit-b 'always L1)
      (emit-label L0)
      (emit-expr altern si env)
      (emit-label L1)))

  (define (emit-expr expr si env)
    (cond
      [(immediate? expr)
       (emit-move "r0" (immediate-rep expr))]
      [(primcall? expr) (emit-primitive-call expr si env)]
      [(let? expr) (emit-let (let-bindings expr) (let-body expr) si env)]
      [(if? expr) (emit-if (if-test expr) (if-conseq expr) (if-altern expr) si env)]
      [(variable-ref? expr) (emit "  ldr r0, [sp,#~a]" (lookup expr env))]
      [else (error 'compile-program "Unsupported expression ~s" (pretty-format expr))]))

  (define (emit-program x)
    (emit-prologue)

    (emit-begin-function "scheme_entry")
    (emit-expr x (- 0 (wordsize)) (empty-env))

    (emit "bx lr")
    (emit-end-function "scheme_entry")

    (emit ".ident \"mml scheme compiler\"")
    (emit ".section .note.GNU-stack,\"\",%progbits"))

  (emit-program x))
