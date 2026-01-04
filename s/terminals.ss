#lang racket

(provide immediate? variable? primitives primitive? datum?)

(define (immediate? x)
  (cond [(integer? x) #t]
        [(boolean? x) #t]
        [(char? x) #t]
        [(null? x) #t]
        [else #f]))

; From tspl4 https://www.scheme.com/tspl4/grammar.html
(define (datum? x)
  (or (boolean? x)
      (char? x)
      (symbol? x)
      (string? x)
      (number? x)
      (list? x)
      (vector? x)
      #;(bytevector? x)))

(define (variable? x)
  (and (symbol? x)
       (not (primitive? x))))

; TODO: a function (primcall 'foo . args) which checks that foo is in the list
; of primitives, used with the right number of args, and emits it in a
; '(primcall ) form.
(define nullary-primitives '(void))

(define unary-primitives
  '(add1 sub1 integer->char char->integer zero? not null? list? vector? car cdr cadr cddr caddr vector-length
    abs odd? even? positive? negative? rational? number? exact? integer? numerator denominator))

; TODO: technically set! isn't a primitive (a procedure) because it doesn't
; evaluate its first argument.  set! is syntax.  This isn't true of
; vector-set! or set-car! or many others because they *do* evaluate that first
; argument.  Unlike set!, they aren't changing the value of a name.  Rather,
; they're manipulating heap memory.
(define binary-primitives
  '(+ - = * < > <= >= min max cons make-vector vector-ref string-ref set! bitwise-arithmetic-shift bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right eq? eqv? quotient remainder modulo / make-ratnum gcd))

(define ternary-primitives
  '(vector-set!))

;;; variable-arity-spec = ()        if any number of parameters allowed
;;;                     | (min)     if [min,Inf) parameters allowed
;;;                     | (min max) if [min,max] parameters allowed
(define variable-arity-primitives
  '([string ()]
    [and ()]
    [or ()]))

(define primitives
  (append (map (lambda (pr) (cons pr 0)) nullary-primitives)
          (map (lambda (pr) (cons pr 1)) unary-primitives)
          (map (lambda (pr) (cons pr 2)) binary-primitives)
          (map (lambda (pr) (cons pr 3)) ternary-primitives)
          variable-arity-primitives))

(define (primitive? x) (assq x primitives))
