#lang racket

(provide compile-program)

(require racket/match)
(require racket/trace)
(require "machine.ss")
(require "compile-shared.ss")
(require "passes.ss")

(define (find-preamble-path)
  ; Try multiple possible locations since we might be loaded via symlinks
  (let ([candidates '("../lib/preamble.ss"
                      "../../lib/preamble.ss"
                      "../../../lib/preamble.ss")])
    (let loop ([paths candidates])
      (cond [(null? paths)
             #f]
            [(file-exists? (car paths))
             (car paths)]
            [else
             (loop (cdr paths))]))))

(define (read-preamble)
  (let ([preamble-path (find-preamble-path)])
    (if preamble-path
        (call-with-input-file preamble-path
          (lambda (port)
            (let loop ([exprs '()])
              (let ([expr (read port)])
                (if (eof-object? expr)
                    (reverse exprs)
                    (loop (cons expr exprs)))))))
        '())))

(define (compile-program prog-list)
  (let* ([preamble (read-preamble)]
         [combined (append preamble prog-list)]
         [prog (normalize-program combined)]
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
