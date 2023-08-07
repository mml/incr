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
  [`(,hd . ,tl) `(primcall cons ,(Init hd) ,(Init tl))]))
