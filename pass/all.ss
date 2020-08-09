#lang racket

(provide all-passes)
(provide make-begin-explicit uncover-settable remove-set! uncover-free parse-and-rename collect-code identify-tail-calls)

(require "make-begin-explicit.ss")
(require "uncover-settable.ss")
(require "remove-set.ss")
(require "uncover-free.ss")
(require "parse-and-rename.ss")
(require "collect-code.ss")
(require "identify-tail-calls.ss")

(define (all-passes e)
  (identify-tail-calls
    (collect-code
      (uncover-free
        (remove-set!
          (uncover-settable
            (make-begin-explicit
              (parse-and-rename e))))))))
