#lang racket

;;; Beginnings of language-specific test cases.

(require "langs.ss")

(define cases
  '([(let ([x 1] [y 2]) x y (+ x y))
     . (let ([x 1] [y 2]) (begin x y (+ x y)))]
    [(begin (+ 1 2) (+ 3 4) (+ 5 6))
     . (begin (+ 1 2) (+ 3 4) (+ 5 6))]
    [(lambda (x) (add1 x))
     . (lambda (x) (add1 x))]
    [(begin (begin (+ 1 2) (+ 3 4)) (+ 5 6))
     . (begin (+ 1 2) (+ 3 4) (+ 5 6))]
    )
  )

(let loop ([cases cases])
  (cond [(null? cases) (void)]
        [else (let ([in (caar cases)] [out (cdar cases)])
                (let ([got (unparse-L1 (make-begin-explicit (parse-Lsrc in)))])
                  (if (equal? out got)
                      (loop (cdr cases))
                      (error 'test "Expected ~a~n      but got  ~a" out got))))]))
