#lang racket

(require racket/match)
(provide compile-program)
(provide compile-port)
(provide scramble-link-register?)

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))


(define scramble-link-register?
  (make-parameter #f))

(define (shift n-bits val)
  (arithmetic-shift val n-bits))
(define bitwise-or bitwise-ior)
(define (wordsize) 4)
(define (cellsize) (* 2 wordsize))
(define (empty-env) '())
(define (extend-env name index env)
  (cons (cons name index) env))
(define (lookup x env)
  (cdr (assoc x env)))
(define heap-register "r8")

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
(define pair-tag #b001)
(define vector-tag #b010)

; TODO: maybe make a fresh on one each compile-program invocation?
(define (make-labeler)
  (let ([x 0])
    (lambda (prefix)
      (set! x (+ x 1))
      (format "~a~a" prefix x))))

(define /unique-label (make-labeler))
(define unique-label
  (case-lambda
    [() (/unique-label "L")]
    [(prefix) (/unique-label prefix)]))

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
            [(add1 sub1 integer->char char->integer zero? not null? + - = * <
              cons car cdr cadr cddr caddr make-vector vector-ref vector-set!) #t]
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
(define primcall-operand1
  (case-lambda
    [(expr) (cadr expr)]
    [(expr default) (if (null? (cdr expr))
                        default
                        (primcall-operand1 expr))]))

(define primcall-operand2
  (case-lambda
    [(expr) (caddr expr)]
    [(expr default) (if (null? (cddr expr))
                        default
                        (primcall-operand2 expr))]))

(define (labelcall? x)
  (eq? 'labelcall (car x)))

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
      [(eq? "lr" s) #b1110]
      [else
        (case (car cs)
          [(#\r) (string->number (list->string (cdr cs)))]
          [else (error 'compile-program "Don't understand register ~s." s)])])))

(define emit-b
  (case-lambda
    [(addr) (emit-b 'always addr)]
    [(condition addr)
     (let ([instruction (case condition
                          [(always) "b"]
                          [(eq mi) (string-append "b" (symbol->string condition))])])
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

(define (emit-allocation-primcall op expr si env)
  (case op
    [(cons)
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  str r0, [sp,#~a]" si) ; save car on the stack
     (emit-expr (primcall-operand2 expr) (- si (wordsize)) env)
     (emit "  str r0, [~a,#~a]" heap-register (wordsize)) ; store cdr
     (emit "  ldr r0, [sp,#~a]" si) ; recover car
     (emit "  str r0, [~a]" heap-register)
     (emit "  add r0,~a,#~a" heap-register pair-tag)
     (emit "  add ~a,~a,#~a" heap-register heap-register (* 2 (wordsize)))]
    [(make-vector) ; using only r0 and r1... could be more efficient
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  lsr r0, #~a" fixnum-shift) ; turn fixnum into int
     (emit "  str r0, [sp,#~a]" si) ; save size on stack
     (emit "  add r0, r0, #11") ; align size to next
     (emit "  and r0, r0, #-8") ;    object boundary
     (emit "  str ~a, [sp,#~a]" heap-register (- si (wordsize))) ; save address on stack
     (emit "  add ~a,~a,r0" heap-register heap-register)
     (emit-expr (primcall-operand2 expr (shift (- fixnum-shift)  #xdead0))
                (- si (wordsize) (wordsize)) env) ; r0 = initial value
     (emit "  ldr r2, [sp,#~a]" (- si (wordsize))) ; r2 = pointer
     (emit "  ldr r1, [sp,#~a]" si) ; r1 = size
     (emit "  str r1, [r2],#~a" (wordsize)) ; write size
     (let ([loop (unique-label)] [break (unique-label)])
       (emit-label loop)
       (emit "  subs r1,r1,#1") ; decrement r1
       (emit-b 'mi break) ; if r1 < 0 goto break
       (emit "  str r0, [r2],#~a" (wordsize)) ; initialize word
       (emit-b 'always loop) ;
       (emit-label break))
     (emit "  ldr r0, [sp,#~a]" (- si (wordsize)))  ; return the pointer
     (emit "  orr r0,r0,#~a" vector-tag)]            ; with appropriate tag
    ))

(define (emit-primitive-call expr si env)
  (let ([op (primcall-op expr)])
  (case op
    [(cons make-vector)
     (emit-allocation-primcall op expr si env)]
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
    [(car)
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  ldr r0,[r0,#~a]" (- pair-tag))]
    [(cdr)
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  ldr r0,[r0,#~a]" (- (wordsize) pair-tag))]
    [(cadr)
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  ldr r0,[r0,#~a]" (- (wordsize) pair-tag))
     (emit "  ldr r0,[r0,#~a]" (- pair-tag))]
    [(cddr)
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  ldr r0,[r0,#~a]" (- (wordsize) pair-tag))
     (emit "  ldr r0,[r0,#~a]" (- (wordsize) pair-tag))]
    [(caddr)
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  ldr r0,[r0,#~a]" (- (wordsize) pair-tag))
     (emit "  ldr r0,[r0,#~a]" (- (wordsize) pair-tag))
     (emit "  ldr r0,[r0,#~a]" (- pair-tag))]
    [(vector-ref)
     (emit-expr (primcall-operand1 expr) si env)
     (emit "  str r0, [sp,#~a]" si)
     (emit-expr (primcall-operand2 expr) (- si (wordsize)) env)
     (emit "  add r0,r0,#~a" (wordsize)) ; skip over vector size
     (emit "  ldr r1, [sp,#~a]" si)
     (emit "  ldr r0, [r1,r0,LSR #~a]" fixnum-shift)]
    [else (error 'compile-program "Unsupported primcall in ~s" (pretty-format expr))])))

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

(define (emit-code lexpr env) (match lexpr
  [(list 'code (list formals ___) expr)
   (let f ([formals formals] [si 0] [env env])
     (cond [(null? formals) (emit "  str lr,[sp,#~a]" si) ; Save LR
                            (emit-expr expr (- si (wordsize)) env)
                            (emit "  ldr lr,[sp,#~a]" si) ; Restore LR
                            (emit "  bx lr")]
           [else (f (cdr formals)
                    (- si (wordsize))
                    (extend-env (car formals) si env))])
     )]))

(define (emit-ldef ldef env) (match ldef
  [(list [list lvars lexprs] ___)
   (let ([env (append (map (lambda (lvar) (cons lvar (unique-label))) lvars) env)])
     (let f ([lvars lvars] [lexprs lexprs])
       (cond [(null? lvars) env]
             [else
               (emit "/* label ~a */" (car lvars))
               (emit-label (lookup (car lvars) env))
               (emit-code (car lexprs) env)
               (f (cdr lvars) (cdr lexprs))])))]))

; Right before the jump, the stack should look like this
;       argN
;       :
;       :
;       arg2
;       arg1
; SP -> arg0
;       old SP
(define (emit-labelcall expr si env) (match expr
  [(list 'labelcall lvar args ___)
   (let f ([args args] [new-si si])
     (cond [(null? args)
            (emit "  sub sp,sp,#~a" (- si))
            (emit "  bl ~a" (lookup lvar env))
            (emit "  add sp,sp,#~a" (- si))]
           [else (emit-expr (car args) new-si env)
                 (emit "  str r0, [sp,#~a]" new-si)
                 (f (cdr args) (- new-si (wordsize)))]))]))

(define (emit-expr expr si env)
  (cond
    [(immediate? expr)
     (emit-move "r0" (immediate-rep expr))]
    [(primcall? expr) (emit-primitive-call expr si env)]
    [(let? expr) (emit-let (let-bindings expr) (let-body expr) si env)]
    [(if? expr) (emit-if (if-test expr) (if-conseq expr) (if-altern expr) si env)]
    [(variable-ref? expr) (emit "  ldr r0, [sp,#~a]" (lookup expr env))]
    [(labelcall? expr) (emit-labelcall expr si env)]
    [else (error 'compile-program "Unsupported expression ~s" (pretty-format expr))]))

(define (emit-program ldef x)
  (emit-prologue)

  (let ([initial-env (emit-ldef ldef (empty-env))])
    (emit-begin-function "scheme_entry")
    (emit "  stmfd sp!, {lr}") ; Save LR
    (when (scramble-link-register?)
      (emit-move32 "lr" #xdeadbeef))
    (emit "  mov ~a,r0" heap-register) ; Save heap base
    (emit-expr x (- 0 (wordsize)) initial-env)
    (emit "  ldmfd sp!, {lr}") ;; Restore LR
    (emit "  bx lr")
    (emit-end-function "scheme_entry"))

  (emit ".ident \"mml scheme compiler\"")
  (emit ".section .note.GNU-stack,\"\",%progbits"))

(define (compile-program prog)
  (match prog
         [(list 'labels (list ldef ___) expr)
          (emit-program ldef expr)]
         [expr (emit-program '() expr)]))
