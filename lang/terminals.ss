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

(define primitives
  '(add1 sub1 integer->char char->integer zero? not null? + - = * < cons car
    cdr cadr cddr caddr make-vector vector-ref vector-set!))

(define (primitive? x) (memq x primitives))
