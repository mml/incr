#lang racket

;;; This pass rewrites memv

(provide remove-memv)

(require racket/match)
(require racket/trace)
(require "generators.ss")
(require "terminals.ss")

(define (remove-memv expr) (Expr expr))

(define Expr
  (lambda (expr)
    (match expr
      [`(quote ,datum) `(quote ,datum)]
      [(? string? s) s]
      [`(if ,test ,conseq ,altern) `(if ,(Expr test) ,(Expr conseq) ,(Expr altern))]
      [(? symbol? x) x]
      [`(begin ,expr* __1) `(begin ,@(map Expr expr*))]
      [`(let ([,x* ,e*] ___) ,body)
        (let ([e* (map Expr e*)]
              [body (Expr body)])
          `(let ,(map list x* e*) ,body))]
      [`(lambda (,x* ___) ,body)
        `(lambda (,@x*) ,(Expr body))]
      [`(primcall memv ,needle ,haystack)
        (Memv needle haystack)]
      [`(primcall ,p ,e* ___)
        `(primcall ,p ,@(map Expr e*))]
      [`(funcall ,e0 ,e* ___)
        `(funcall ,(Expr e0) ,@(map Expr e*))])))

(define Memv
  (lambda (needle haystack)
    (let ([n (tmp)] [h (tmp)] [f (tmp)] [l (tmp)])
      `(let ([,n ,needle] [,h ,haystack])
         (let ([,f (primcall void)])
           (begin
             (primcall set! ,f (lambda (,l)
                                 (if (primcall null? ,l) '#f
                                   (if (primcall eqv? (primcall car ,l) ,n) ,l
                                     (funcall ,f (primcall cdr ,l))))))
             (funcall ,f ,h)))))))
