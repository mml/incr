#lang racket

(provide compile-port)
(provide scramble-link-register?)
(provide set-constant)
(provide lookup-constant)
(provide define-constant)
(provide constant)
(provide bitwise-or)
(provide shift)

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

(define-constant false-value (bitwise-or #b1111 (shift 4 #b0010)))
(define-constant true-value (bitwise-or #b1111 (shift 4 #b0110)))
(define-constant char-mask #b11111111)
(define-constant char-tag #b00001111)
(define-constant char-shift 8)
(define-constant null-value #b00111111)

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
