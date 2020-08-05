#lang racket

(provide make-begin-explicit)

(require racket/match)
(require "../lang/terminals.ss")

(define (make-begin-explicit expr) (match expr
  [(? immediate? c) c]
  [(? primitive? pr) pr]
  [(? variable? x) x]
  [`(begin ,expr* __1)
    `(begin ,@(map make-begin-explicit expr*))]
  [`(let ([,x* ,e*] ___) ,body* __1) 
    (let* ([e* (map make-begin-explicit e*)]
           [bindings (map list x* e*)])
      `(let ,bindings (begin ,@(map make-begin-explicit body*))))]
  [`(lambda ,formal* ,body* __1)
    `(lambda ,formal* (begin ,@(map make-begin-explicit body*)))]
  [`(,e0 ,e* ___)
    `(,(make-begin-explicit e0) ,@(map make-begin-explicit e*))]
  ))
