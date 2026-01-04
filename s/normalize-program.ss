#lang racket

(provide normalize-program)

(require racket/match)

(define (normalize-program expr*)
  (match expr*
    [`(,expr) expr]  ; Single expression - unwrap
    [_ `(let () ,@expr*)]))  ; Multiple - wrap in let

(module+ test
  (require rackunit)
  (check-equal? (normalize-program '((+ 1 2)))
                '(+ 1 2))
  (check-equal? (normalize-program '((+ 1 2) (* 3 4)))
                '(let () (+ 1 2) (* 3 4))))
