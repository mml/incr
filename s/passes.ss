#lang racket

(provide (all-defined-out))
(provide make-begin-explicit uncover-settable remove-set! uncover-free parse-and-rename collect-code identify-tail-calls simplify-conditionals remove-memv remove-complex-constants)

(require "make-begin-explicit.ss")
(require "uncover-settable.ss")
(require "remove-set.ss")
(require "uncover-free.ss")
(require "parse-and-rename.ss")
(require "collect-code.ss")
(require "identify-tail-calls.ss")
(require "simplify-conditionals.ss")
(require "remove-memv.ss")
(require "remove-complex-constants.ss")

(define passes (list parse-and-rename make-begin-explicit uncover-settable remove-set! uncover-free collect-code identify-tail-calls))

(define (apply-passes p* e)
  (cond
    [(null? p*) e]
    [else
      (apply-passes (cdr p*) ((car p*) e))]))

(define (all-passes e)
  (apply-passes passes e))

(define (passes-up-to p e)
  (let loop ([passes passes] [e e])
    (cond
      [(null? passes)
       (error 'passes-up-to "Never found pass")]
      [(eq? p (car passes))
       ((car passes) e)]
      [else
        (loop (cdr passes) ((car passes) e))])))
