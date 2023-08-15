#lang racket

(provide compile-program)

(require racket/match)
(require racket/trace)
(require "machine.ss")
(require "compile-shared.ss")
(require "passes.ss")

(define (compile-program prog)
  (let ([labels (identify-tail-calls
                    (collect-code
                      (uncover-free
                        (remove-set!
                          (uncover-settable
                            (make-begin-explicit
                              (remove-complex-constants
                                (remove-memv
                                  (simplify-conditionals
                                    (parse-and-rename prog))))))))))
          ])
    (emit-prologue)
    (emit-Labels labels)
    (emit-epilogue)))
