#lang racket

(define exprs '())
(define expecteds '())
(provide test-cases)
(provide /test-cases)

(define-syntax test-cases
  (lambda (stx)
    (syntax-case stx (skip test-case)
      [(_ desc (test-case expr expected) ...)
       #'(/test-cases desc (list (quote expr) ...) (list expected ...) #f)]
      [(_ skip desc (test-case expr expected) ...)
       #'(/test-cases desc (list (quote expr) ...) (list expected ...) #t)])))

(define (/test-cases desc c-exprs c-expecteds skip?)
  (printf "[~a] ~a~n" desc (length exprs))
  (cond
    [skip?]
    [else
      (set! exprs (append c-exprs exprs))
      (set! expecteds (append c-expecteds expecteds))]))
 
(require "pass/all.ss")
(provide main)
 
(define tests
  '((let ([v (make-vector 5 0)])
      (vector-set! v 1 20)
      v)
    (let ([a 10][b 20]) (+ a b))))

(define (chain p* output-channel)
  (let loop ([rpasses (reverse p*)] [output-channel output-channel] [n 0])
        (cond
          [(null? rpasses) output-channel]
          [else
            (printf "~a...~n" n)
            (let ([p (place ch
                       (let ([output-channel (place-channel-get ch)]
                             [n (place-channel-get ch)])
                         (let loop ()
                           (let ([prog (place-channel-get ch)])
                             (place-channel-put
                               output-channel
                               ((list-ref (reverse passes) n) prog)))
                           (loop))))])
              (place-channel-put p output-channel)
              (place-channel-put p n)
              (loop (cdr rpasses) p (add1 n)))])))

(define (main)
  (let-values ([(here there) (place-channel)])
    (let ([input (chain passes there)])
      (let loop ([tests exprs])
        (cond [(null? tests)]
              [else
                (place-channel-put input (car tests))
                (loop (cdr tests))])))
    (let loop ()
      (printf "~a~n" (place-channel-get here))
      (loop))))
