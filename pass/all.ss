#lang racket

(provide all-passes)
(provide make-begin-explicit uncover-free parse-and-rename collect-code)

(require "make-begin-explicit.ss")
(require "uncover-free.ss")
(require "parse-and-rename.ss")
(require "collect-code.ss")

(define (all-passes e)
  (collect-code
    (uncover-free
      (make-begin-explicit
        (parse-and-rename e)))))
