#lang racket

(provide compile-port)
(provide scramble-link-register?)
(provide set-constant lookup-constant define-constant constant)
(provide bitwise-or shift)
(provide
  primcall-op primcall-operand1 primcall-operand2 primcall-operand3
  primcall-operand-count primcall-operands)
(provide lhs)
(provide rhs)
(provide extend-env)
(provide lookup)
(provide arg-env)
(provide clovar-env)
(provide get-stack-index)
(provide emit-Labels)
(require racket/lazy-require)
(lazy-require
  ["machine.ss" (emit-label emit-Code emit-scheme-entry emit-bss unique-label)])

(define (emit-Def x code env)
  (emit-label (lookup x env))
  (emit-Code code env))

(define (emit-Def* x* code*)
  (let ([env (map (lambda (x) (cons x (unique-label (string-append "C" (symbol->string x) "_")))) x*)])
    (let loop ([x* x*] [code* code*])
      (cond [(null? x*) env]
            [else
              (emit-Def (car x*) (car code*) env)
              (loop (cdr x*) (cdr code*))]))))

(define (emit-Labels expr)
  (match expr
    [`(labels ([,x* ,code*] ___) ,body)
      (let ([env (emit-Def* x* code*)])
        (emit-scheme-entry body env))]))

(define scramble-link-register?
  (make-parameter #f))

(define compile-port
  (make-parameter
    (current-output-port)
    (lambda (p)
      (unless (output-port? p)
        (error 'compile-port (format "Not an output port ~s." p)))
      p)))

(define constants '())

(define (set-constant k v)
  (unless (symbol? k) (error 'set-constant "~s is not a symbol" k))
  (set! constants (cons (cons k v) constants)))

(define (lookup-constant k)
  (unless (symbol? k) (error 'lookup-constant "~s is not a symbol" k))
  (let ([pair (assq k constants)])
    (if pair
      (cdr pair)
      (error 'lookup-constant "undefined constant ~s" k))))

(define-syntax (define-constant stx)
  (syntax-case stx ()
               [(_ k v)
                (identifier? #'k)
                #'(set-constant 'k v)]))

(define-syntax (constant stx)
  (syntax-case stx ()
               [(_ k)
                (identifier? #'k)
                #'(lookup-constant 'k)]))

(define bitwise-or bitwise-ior)
(define (shift n-bits val)
  (arithmetic-shift val n-bits))

(define (extend-env name index env)
  (cons (cons name index) env))
(define lhs car)
(define rhs cadr)

;;; Scheme procedure calls
; Our calling convention expects
; sp-<word> to be empty (we'll save the LR there)
; sp-<2word> to be a closure object
; sp-<3word> to be our first argument
(define get-stack-index
  (case-lambda
    [(key)
     (case key
       [(link-register) (* -1 (constant wordsize))]
       [(closure) (* -2 (constant wordsize))])]
    [(key n)
     (case key
       [(arg) (* (constant wordsize) (- -3 n))]
       [(clovar) (* (constant wordsize) (add1 n))])]))


(define (arg-env x* env)
  (let loop ([x* x*] [arg-count 0] [arg-index (get-stack-index 'arg 0)] [env env])
    (cond [(null? x*)
           (values arg-index env)]
          [else
            (loop (cdr x*)
                  (add1 arg-count)
                  (- arg-index (constant wordsize))
                  (extend-env (car x*) (cons "sp" arg-index) env))])))
(define (clovar-env y* env)
  (let loop ([y* y*] [clovar-count 0] [clovar-index (get-stack-index 'clovar 0)] [env env])
    (cond [(null? y*)
           env]
          [else
            (loop (cdr y*)
                  (add1 clovar-count)
                  (+ clovar-index (constant wordsize))
                  (extend-env (car y*) (cons (constant closure-register) clovar-index) env))])))


(define-constant false-value (bitwise-or #b1111 (shift 4 #b0010)))
(define-constant true-value (bitwise-or #b1111 (shift 4 #b0110)))
(define-constant char-mask #b11111111)
(define-constant char-tag #b00001111)
(define-constant void-value #b00011111)
(define-constant char-shift 8)
(define-constant null-value #b00111111)
(define-constant fixnum-shift 2)
(define-constant pair-tag #b001)
(define-constant vector-tag #b010)
(define-constant string-tag #b011)
(define-constant symbol-tag #b100)
(define-constant ratnum-tag #b101)
(define-constant closure-tag #b110)
(define-constant ptr-mask #b111)

(module+ test
  (require rackunit)
  (let ([vals '(false-value true-value void-value null-value)]
        [tags '(char-tag pair-tag vector-tag string-tag ratnum-tag closure-tag symbol-tag)]
        [masks '(char-mask ptr-mask ptr-mask ptr-mask ptr-mask ptr-mask ptr-mask)])
    ; none of the values matches any of the tag/mask combos
    (for-each (lambda (k)
                (let ([val (lookup-constant k)])
                  (do ([tags tags (cdr tags)]
                       [masks masks (cdr masks)])
                    ((null? tags) (void))
                    (let ([mask (lookup-constant (car masks))]
                          [tag (lookup-constant (car tags))])
                    (check-not-equal? (bitwise-and val mask) tag)))))
              vals)

    ; no tag is repeated
    (do ([tags tags (cdr tags)])
      [(null? tags) (void)]
      (let ([t1 (car tags)])
        (for-each (lambda (t2)
                    (check-not-equal? t1 t2))
                  (cdr tags))))

    ; with an 8-byte-aligned pointer value, verify all the ptr-masks work
    (let ([addr #xfffffff8])
      (do ([tags tags (cdr tags)]
           [masks masks (cdr masks)])
        [(null? tags) (void)]
        (when (eq? (car masks) 'ptr-mask)
          (let* ([mask (lookup-constant (car masks))]
                 [tag (lookup-constant (car tags))]
                 [tagged (bitwise-or addr tag)])
            (check-equal? (bitwise-and addr tag) 0 (format "~s ~s" (car masks) (car tags)))
            (check-equal? (bitwise-and mask tagged) tag)
            (check-equal? (bitwise-and (bitwise-not tag)
                                       tagged)
                          addr)))))
    ))

(define (primcall-operand-count expr)
  (length (primcall-operands expr)))

(define primcall-operands cdr)

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

(define (lookup x env)
  (cond
    [(assq x env) => cdr]
    [else #f]))




(module+ test
  (require rackunit)

  (set-constant 'foo 9)
  (check-equal? (lookup-constant 'foo) 9)
  (set-constant 'bar 'foo)
  (check-equal? (lookup-constant 'bar) 'foo)
  (set-constant 'foo 22)
  (check-equal? (lookup-constant 'foo) 22)
  (check-exn
    exn:fail?
    (lambda ()
      (lookup-constant 'baz)))
  (define-constant baz 99)
  (check-equal? (lookup-constant 'baz) 99)
  (check-equal? (constant baz) 99))
