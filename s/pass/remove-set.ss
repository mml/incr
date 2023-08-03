#lang racket

(provide remove-set!)

(require racket/match)
(require racket/trace)
(require "generators.ss")
(require "../lang/terminals.ss")

(define (remove-set! expr)
  (Expr expr '()))

(define (Expr* expr* env)
  (cond
    [(null? expr*)
     '()]
    [else
      (cons (Expr (car expr*) env)
            (Expr* (cdr expr*) env))]))

(define (settable-bindings t* x*)
  (let loop ([rbinding* '()] [t* t*] [x* x*])
    (cond
      [(null? t*)
       (reverse rbinding*)]
      [else
        (loop (cons
                `(,(car t*)
                   (primcall make-vector '1 ,(car x*)))
                rbinding*)
              (cdr t*) (cdr x*))])))

(define (Expr expr env) (match expr
  [`(quote ,c) expr]
  [(? variable? x)
   (cond
     [(assq x env) =>
      (lambda (as)
        `(primcall vector-ref ,(cdr as) '0))]
     [else expr])]
  [`(settable (,x* ___) ,e)
    (let* ([t* (map unique-box x*)]
           [bindings (settable-bindings t* x*)])
      `(let (,@bindings) ,(Expr e (append (map cons x* t*) env))))]
  [`(begin ,expr* __1)
    `(begin ,@(Expr* expr* env))]
  [`(let ([,x* ,e*] ___) ,body)
    (let ([e* (Expr* e* env)])
      `(let ,(map list x* e*) ,(Expr body env)))]
  [`(lambda (,x* ___) ,body)
    `(lambda (,@x*) ,(Expr body env))]
  [`(if ,test ,conseq ,altern)
    `(if ,(Expr test env) ,(Expr conseq env) ,(Expr altern env))]
  [`(primcall set! ,x ,e)
    (let ([t (cdr (assq x env))])
      `(primcall vector-set! ,t '0 ,(Expr e env)))]
  [`(primcall ,pr ,e* ___)
    `(primcall ,pr ,@(Expr* e* env))]
  [`(funcall ,e* __1)
    `(funcall ,@(Expr* e* env))]
  ))
