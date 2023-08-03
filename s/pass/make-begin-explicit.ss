#lang racket

(provide make-begin-explicit)

(require racket/match)
(require "../lang/terminals.ss")

(define (make-begin-explicit expr) (match expr
  [`(quote ,c) expr]
  [(? variable? x) x]
  [`(begin ,expr* __1)
    `(begin ,@(map make-begin-explicit expr*))]
  [`(if ,test ,conseq ,altern)
    `(if ,(make-begin-explicit test)
         ,(make-begin-explicit conseq)
         ,(make-begin-explicit altern))]
  [`(let ([,x* ,e*] ___) ,body* __1) 
    (let* ([e* (map make-begin-explicit e*)]
           [bindings (map list x* e*)])
      `(let ,bindings (begin ,@(map make-begin-explicit body*))))]
  [`(lambda ,formal* ,body* __1)
    `(lambda ,formal* (begin ,@(map make-begin-explicit body*)))]
  [`(funcall ,e0 ,e* ___)
    `(funcall ,(make-begin-explicit e0) ,@(map make-begin-explicit e*))]
  [`(primcall ,pr ,e* ___)
    `(primcall ,pr ,@(map make-begin-explicit e*))]
  ))
