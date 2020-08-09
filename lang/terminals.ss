#lang racket

(provide immediate? variable? primitives primitive?)

(define (immediate? x)
  (cond [(integer? x) #t]
        [(boolean? x) #t]
        [(char? x) #t]
        [(null? x) #t]
        [else #f]))

(define (variable? x)
  (and (symbol? x)
       (not (primitive? x))))

(define unary-primitives
  '(add1 sub1 integer->char char->integer zero? not null? car cdr cadr cddr caddr))

(define binary-primitives
  '(+ - = * < cons make-vector vector-ref vector-set! set!))

(define primitives
  (append (map (lambda (pr) (cons pr 1)) unary-primitives)
          (map (lambda (pr) (cons pr 2)) binary-primitives)))

(define (primitive? x) (assq x primitives))
