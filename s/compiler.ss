#lang racket

(provide compile-program)

(require racket/match)
(require racket/trace)
(require "machine.ss")
(require "compile-shared.ss")
(require "passes.ss")

(define (compile-program prog-list)
  (let* ([prog (normalize-program prog-list)]
         [labels (identify-tail-calls
                   (collect-code
                     (uncover-free
                       (remove-set!
                         (uncover-settable
                           (remove-complex-constants
                             (make-begin-explicit
                               (remove-memv
                                 (simplify-binding-forms
                                   (simplify-conditionals
                                     (parse-and-rename prog)))))))))))
          ])
    (emit-prologue)
    (emit-Labels labels)
    (emit-epilogue)))
