#lang racket

(provide all-passes)
(provide make-begin-explicit uncover-free parse-and-rename collect-code identify-tail-calls)

(require "make-begin-explicit.ss")
(require "uncover-free.ss")
(require "parse-and-rename.ss")
(require "collect-code.ss")
(require "identify-tail-calls.ss")

(define (all-passes e)
  (identify-tail-calls
    (collect-code
      (uncover-free
        (make-begin-explicit
          (parse-and-rename e))))))
