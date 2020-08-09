#lang racket

(provide compile-program)
(provide compile-port)
(provide scramble-link-register?)

(require racket/match)
(require racket/trace)
(require "pass/all.ss")

(define (compile-program prog)
  (let ([labels (identify-tail-calls
                  (collect-code
                    (uncover-free
                      (make-begin-explicit
                        (parse-and-rename prog)))))])
    (emit-prologue)
    (emit-Labels labels)
    (emit-epilogue)))

(define (emit-Labels expr) (match expr
  [`(labels ([,x* ,code*] ___) ,body)
    (let ([env (emit-Def* x* code*)])
      (emit-scheme-entry body env))]))

(define (emit-Def* x* code*)
  (let ([env (map (lambda (x) (cons x (unique-label))) x*)])
    (let loop ([x* x*] [code* code*])
      (cond [(null? x*) env]
            [else
              (emit-Def (car x*) (car code*) env)
              (loop (cdr x*) (cdr code*))]))))

(define (emit-Def x code env)
  (emit "/* label ~a */" x)
  (emit-label (lookup x env))
  (emit-Code code env))

; emit-Code emits a subroutine
(define (emit-Code code env) (match code
  [`(code (,x* ___) (,y* ___) ,body) ; free variables not implemented yet
    (emit "  str lr,[sp,#~a]" link-register-index (// "save LR"))
    (emit "  ldr ~a, [sp,#~a]" closure-register closure-index
          (// "load closure address"))
    (let-values ([(si env) (arg-env x* env)])
      (let ([env (clovar-env y* env)])
        (emit-expr body si env)))
    (emit "  ldr lr,[sp,#~a]" link-register-index) ; restore LR
    (emit "  bx lr")
    ]))

(define (arg-env x* env)
  (let loop ([x* x*] [arg-count 0] [arg-index arg0-index] [env env])
    (cond [(null? x*)
           (values arg-index env)]
          [else
            (loop (cdr x*)
                  (add1 arg-count)
                  (- arg-index (wordsize))
                  (extend-env (car x*) (cons "sp" arg-index) env))])))

(define (clovar-env y* env)
  (let loop ([y* y*] [clovar-count 0] [clovar-index clovar0-index] [env env])
    (cond [(null? y*)
           env]
          [else
            (loop (cdr y*)
                  (add1 clovar-count)
                  (+ clovar-index (wordsize))
                  (extend-env (car y*) (cons closure-register clovar-index) env))])))

(define (emit-scheme-entry expr env)
  (emit-begin-function "scheme_entry")
  (emit "  stmfd sp!, {lr}") ; Save LR
  (when (scramble-link-register?)
    (emit-move32 "lr" #xdeadbeef))
  (emit "  mov ~a,r0" heap-register) ; Save heap base
  (emit-expr expr (- 0 (wordsize)) env)
  (emit "  ldmfd sp!, {lr}") ;; Restore LR
  (emit "  bx lr")
  (emit-end-function "scheme_entry"))

(define (emit-epilogue)
  (emit ".ident \"mml scheme compiler\"")
  (emit ".section .note.GNU-stack,\"\",%progbits"))

(define (emit-varref x env)
  (let ([loc (lookup x env)])
    (if loc
        (emit "  ldr r0,[~a,#~a]" (car loc) (cdr loc) (// "~a" x))
        (error 'compile-program "Internal error: unbound variable ~a" x))))

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))

(define-syntax (with-saved-registers stx)
  (syntax-case stx ()
    [(_ [si ()] expr ...)
     #'(let ([si si]) expr ...)]
    [(_ [si (reg)] expr ...)
     #'(begin
         (emit "  str ~a,[sp,#~a] /* save ~a */" reg si reg)
         (let ([si (- si (wordsize))])
           (with-saved-registers [si ()] expr ...))
         (emit "  ldr ~a,[sp,#~a] /* restore ~a */" reg si reg))]
    [(_ [si (r1 r2 ...)] expr ...)
     #'(begin
         (emit "  str ~a,[sp,#~a] /* save ~a */" r1 si r1)
         (let ([si (- si (wordsize))])
           (with-saved-registers [si (r2 ...)] expr ...))
         (emit "  ldr ~a,[sp,#~a] /* restore ~a */" r1 si r1))]))

(define-syntax (emit stx)
  (syntax-case stx (//)
    [(_ ifmt iarg ... (// cfmt carg ...))
     #'(emit/ (list cfmt carg ...) ifmt iarg ...)]
    [(_ ifmt iarg ...)
     #'(emit/ #f ifmt iarg ...)]))

(define (emit/ comment . args)
  (let ([ins (apply format args)])
    (if comment
     (let* ([comment (apply format comment)]
            [wslen (max 0 (- 24 (string-length ins)))]
            [ws (make-string wslen #\space)])
       (fprintf (compile-port) "~a~a@ ~a" ins ws comment))
      (apply fprintf (compile-port) args)))
  (newline (compile-port)))


(define scramble-link-register?
  (make-parameter #f))

(define (shift n-bits val)
  (arithmetic-shift val n-bits))
(define bitwise-or bitwise-ior)
(define (wordsize) 4)
(define (wordsize-shift) 2)
(define (cellsize) (* 2 wordsize))
(define (empty-env) '())
(define (extend-env name index env)
  (cons (cons name index) env))
(define (lookup x env)
  (cond
    [(assq x env) => cdr]
    [else #f]))

;;; Fixnums end in #b00.
;;; All other integral types end in #b1111
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
(define closure-tag #b110)

;;; Register roles
;;; See also https://static.docs.arm.com/ihi0042/j/IHI0042J_2020Q2_aapcs32.pdf
;;; Chapter 6, Section 6.1
;; Caller-saved
; r0-r3 are argument and retun value registers also called a1-a4
; r12 should be treated as a scratch register... it could be altered by long
; jumps, in which case it's called the intra-procedure-call scratch register
; (IP)

;; Callee-saved
; r4-r8,r10 are variable registers also called v1-v5,v7
; r9 may have special meaning, but if not it can be treated as v6
;   under Linux, it doesn't appear to be special
; r11 is the frame pointer (FP) if used, otherwise v8
; r13 is the stack pointer (SP)

;; Special
; r14 is the link register (LR)
; r15 is the program counter (PC)

(define heap-register "v5")
(define closure-register "v7")
;;; Scheme procedure calls
; Our calling convention expects
; sp-4 to be empty (we'll save the LR there)
; sp-8 to be a closure object
; sp-12 to be our first argument
(define link-register-index (* -1 (wordsize)))
(define closure-index (* -2 (wordsize)))
(define arg0-index (* -3 (wordsize)))
(define clovar0-index (wordsize))
(define (arg-index arg-count)
  (- arg0-index (* (wordsize) arg-count)))

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
              cons car cdr cadr cddr caddr make-vector vector-ref vector-set!)
             #t]
            [else #f])])
          #f))

(define (let? x)
  (and (list? x)
       (eq? 'let (car x))))
(define (begin? x)
  (and (list? x)
       (not (null? x))
       (eq? 'begin (car x))))

(define (variable-ref? x)
  (symbol? x))

(define let-bindings cadr)
(define (let-body expr)
  `(begin ,@(cddr expr)))
(define begin-exprs cdr)
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

(define primcall-operand3 cadddr)

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
             [hi (shift -16 val)]
             [ilabel (if (eq? 'always condition) "" (symbol->string condition))])
         (emit "  /* movw~a ~a,#~a */" ilabel dest (bitwise-and val #xffff))
         (emit "  .word 0b~a~a~a~a~a" cbits mov (padbits lo4 4) rd (padbits lo12 12))
         (unless (zero? hi)
           (let ([hi12 (bitwise-and hi #xfff)]
                 [hi4 (shift -12 (bitwise-and hi #xf000))])
             (emit "  /* movt~a ~a,#~a */" ilabel dest hi)
             (emit "  .word 0b~a~a~a~a~a" cbits movt (padbits hi4 4) rd (padbits hi12 12))))))]
    ))

(define (emit-move dest val)
  (emit-move32 dest val))

(define (emit-label l)
  (emit "~a:" l))

(define (emit-Closure label free* si env)
  (let* ([nfree (length free*)]               ; number of free variables
         [size (add1 nfree)]                  ; plus 1 for the label address
         [size (* (wordsize) size)]           ; in words
         [size (bitwise-and -8 (+ 11 size))]) ; align to next dword
    (emit "  @ closure ~a ~a{{{" label free*)
    (emit "  ldr r0,=~a" (lookup label env))  ; pseudo-instruction
    (emit "  str r0,[~a]" heap-register)      ; write pointer
    (let loop ([free* free*] [index (wordsize)])
      (unless (null? free*)
        (emit-varref (car free*) env)
        (emit "  str r0,[~a,#~a]" heap-register index)
        (loop (cdr free*) (+ index (wordsize)))))
    (emit "  orr r0,~a,#~a" heap-register closure-tag)
    (emit "  add ~a,~a,#~a" heap-register heap-register size)
    (emit "  @ closure}}}")))

; emit-Tailcall sets up a tail call and then branches
(define (emit-Tailcall f e* si env)
  ; We are clobbering our incoming stack frame.  Even the LR, which we really
  ; should save.  We restore it first as a kludge.  In the future, we should
  ; emit a special Lx_tail label pointing to the spot right after where we save
  ; the LR on the stack.  Then we can branch to there instead.
  (emit "  @ tailcall")
  (emit-expr f si env) ; r0 = closure
  (emit "  BIC r0,r0,#~a" closure-tag      (// "clear tag bits"))
  (emit "  str r0, [sp,#~a]" closure-index (// "put closure on stack"))
  ; We now have to evaluate all arguments and gradually add their values to the stack.
  (let loop ([e* e*] [arg-index arg0-index] [arg-count 0] [si arg0-index])
    (cond
      [(null? e*)
       (emit "  ldr r0,[sp,#~a]" closure-index       (// "load closure into r0"))
       (emit "  BIC r0,r0,#~a" closure-tag           (// "zero out tag"))
       (emit "  LDR r0,[r0]"                         (// "load branch target"))
       (emit "  LDR LR,[sp,#~a]" link-register-index (// "hack to preserve LR"))
       (emit "  bx r0")]
      [else
        (emit-expr (car e*) si env)
        (emit "  str r0, [sp,#~a]" arg-index   (// "store arg ~a" arg-count))
        (loop (cdr e*) (- arg-index (wordsize)) (add1 arg-count) (- si (wordsize)))])))

; emit-Funcall sets up a call and then branches
(define (emit-Funcall f e* si env)
  (emit "  @ funcall")
  (emit-expr f si env) ; r0 = closure
  (let ([psi si]) ; procedure SI
    (emit "  BIC r0,r0,#~a" closure-tag              (// "clear tag bits"))
    (emit "  str r0, [sp,#~a]" (+ psi closure-index) (// "put closure on stack"))
    (let loop ([e* e*] [arg-index (+ psi arg0-index)] [arg-count 0] [si (+ psi arg0-index)])
      (cond
        [(null? e*)
         (emit "  ldr ~a, [sp,#~a]" closure-register (+ psi closure-index)
               (// "load closure address from stack"))
         (emit "  ldr ~a, [~a]" closure-register closure-register
               (// "dereference code pointer"))
         (emit "  sub sp,sp,#~a" (- psi) (// "set procedure SP"))
         (emit "  blx ~a" closure-register)
         (emit "  add sp,sp,#~a /* restore SP */" (- psi))
         (emit "  ldr ~a, [sp,#~a]" closure-register closure-index
               (// "restore closure address"))]
        [else
          (emit-expr (car e*) si env)
          (emit "  str r0, [sp,#~a] /* store arg ~a */"
                arg-index arg-count)
          (loop (cdr e*) (- arg-index (wordsize)) (add1 arg-count) (- si (wordsize)))]))))

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
     (emit "  @ make-vector{{{")
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
     (let ([loop (unique-label "loop")] [break (unique-label "break")])
       (emit-label loop)
       (emit "  subs r1,r1,#1") ; decrement r1
       (emit-b 'mi break) ; if r1 < 0 goto break
       (emit "  str r0, [r2],#~a" (wordsize)) ; initialize word
       (emit-b 'always loop) ;
       (emit-label break))
     (emit "  ldr r0, [sp,#~a]" (- si (wordsize)))  ; return the pointer
     (emit "  orr r0,r0,#~a" vector-tag)             ; with appropriate tag
     (emit "  @ make-vector}}}")]
    ))

(define (emit-side-effect-primcall op expr si env)
  (case op
    [(vector-set!) ; vec n obj
     (with-saved-registers [si ("r4" "r5")]
       (emit-expr (primcall-operand1 expr) si env)
       (emit "  sub r0, #~a" vector-tag)
       (emit "  mov r4,r0") ; address in r4
       (emit-expr (primcall-operand2 expr) si env)
       (emit "  LSR r5,r0,#~a" fixnum-shift) ; fixnum->int (and move to r5)
       (emit "  add r5,r5,#1") ; skip over vector size
       (emit "  LSL r5,r5,#~a" (wordsize-shift)) ; multiply by wordsize
       (emit-expr (primcall-operand3 expr) si env) ; value in r0
       (emit "  str r0, [r4,r5]"))]
    [else (error 'compile-program
                 "Unsupported primcall ~s in ~s" (pretty-format op) (pretty-format expr))]))

(define (emit-primitive-call expr si env)
  (let ([op (primcall-op expr)])
  (case op
    [(cons make-vector)
     (emit-allocation-primcall op expr si env)]
    [(vector-set!)
     (emit-side-effect-primcall op expr si env)]
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
     (emit "  @ vector-ref{{{")
     (with-saved-registers [si ("r4")]
       (emit-expr (primcall-operand1 expr) si env)
       (emit "  sub r0, #~a" vector-tag)
       (emit "  mov r4,r0") ; address in r4
       (emit-expr (primcall-operand2 expr) si env) ; offset in r0
       (emit "  LSR r1,r0,#~a" fixnum-shift) ; fixnum->int (and move to r1)
       (emit "  add r1,r1,#1") ; skip over vector size
       (emit "  LSL r1,r1,#~a" (wordsize-shift)) ; multiply by wordsize
       (emit "  ldr r0, [r4,r1]")) ; put return value in r0
     (emit "  @ vector-ref}}}")]
    [else (error 'compile-program "Unsupported primcall in ~s" (pretty-format expr))])))

(define (emit-begin exprs si env)
  (cond
    [(null? exprs) (void)]
    [else (emit-expr (car exprs) si env)
          (emit-begin (cdr exprs) si env)]))

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
             (extend-env (lhs b) (cons "sp" si) new-env)
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

(define (emit-expr expr si env) (match expr
  [`(quote ,c) (emit-move "r0" (immediate-rep c))]
  [(? symbol? x) (emit-varref x env)]
  [`(primcall ,pr ,e* ___) (emit-primitive-call `(,pr ,@e*) si env)]
  [`(begin ,e* ___) (emit-begin e* si env)]
  [`(let ,bindings ,body) (emit-let bindings body si env)]
  [`(if ,test ,conseq ,altern) (emit-if test conseq altern si env)]
  [`(closure ,label ,free* ___) (emit-Closure label free* si env)]
  [`(funcall ,f ,e* ___) (emit-Funcall f e* si env)]
  [`(tailcall ,f ,e* ___) (emit-Tailcall f e* si env)]))

