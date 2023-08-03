#lang racket

(provide compile-program)

(require racket/match)
(require racket/trace)
(require "compile-shared.ss")
(require "passes.ss")
(require "machine.ss")

(define (compile-program prog)
  (let ([labels (identify-tail-calls
                  (collect-code
                    (uncover-free
                      (remove-set!
                        (uncover-settable
                          (make-begin-explicit
                            (parse-and-rename prog)))))))])
    (emit-prologue)
    (emit-Labels labels)
    (emit-epilogue)))
