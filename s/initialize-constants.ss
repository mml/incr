#lang racket

(provide initialize-constants)

(require "terminals.ss")
(require racket/trace)

(define (initialize-constants prog)
  (Labels prog))

(define (Labels prog) (match prog
  [`(labels ,clabels ,dlabels ,body)
    (let-values ([(name* init*) (Dlabel* dlabels)])
      `(labels ,clabels ,name* (begin ,@init* ,body)))]))

(define (Dlabel* dlabel*) (match dlabel*
  ['() (values '() '())]
  [`((,name . ,init) ,@dlabel*)
    (let-values ([(name* init*) (Dlabel* dlabel*)])
      (values (cons name name*) (cons `(constant-init ,name ,(Init init)) init*)))]))

(define (Init init) (match init
  ['() '(quote ())]
  [(? immediate? c) `(quote ,init)]
  [(? string? c) (String c)]
  [`(,hd . ,tl) `(primcall cons ,(Init hd) ,(Init tl))]))

(define (String s)
  (define (q c) `',c)
  `(primcall string ,@(map q (string->list s))))

(module+ test
  (require rackunit)

  (check-equal? (String "") '(primcall string))
  (check-equal? (String "abc") '(primcall string '#\a '#\b '#\c))
)
