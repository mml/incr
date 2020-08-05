#lang racket

(provide uncover-free)

(require racket/match)
(require "../lang/terminals.ss")

(define (uncover-free expr)
  (let-values ([(e free) (Expr expr)])
    (unless (set-empty? free)
      (error 'uncover-free "internal bug: free variables ~a at top of expr ~a"
             free expr))
    e))

(define (Expr* expr*)
  (if (null? expr*)
      (error 'uncover-free "internal bug: Expr* called with empty list")
      (let loop ([expr (car expr*)] [expr* (cdr expr*)] [rexpr* '()] [rfree* '()])
        (let-values ([(rexpr rfree) (Expr expr)])
          (if (null? expr*)
              (values (reverse (cons rexpr rexpr*)) (reverse (cons rfree rfree*)))
              (loop (car expr*) (cdr expr*) (cons rexpr rexpr*) (cons rfree rfree*)))))))

(define (Expr expr) (match expr
  [(? immediate? c) (values c (set))]
  [(? primitive? pr) (values pr (set))]
  [(? variable? x) (values x (set x))]
  [`(begin ,expr* __1)
    (let-values ([(expr* free*) (Expr* expr*)])
      (values `(begin ,@expr*) (apply set-union free*)))]
  [`(let ([,x* ,e*] ___) ,body)
    (let-values ([(e* free*) (Expr* e*)])
      (let-values ([(body free) (Expr body)])
        (let* ([ufree (set-union free (apply set-union free*))]
               [free (set-subtract ufree (apply set x*))])
          (let ([bindings (map list x* e*)])
            (values `(let ,bindings ,body) free)))))]
  [`(lambda (,x* ___) ,body)
    (let-values ([(body free) (Expr body)])
      (let ([free (set-subtract free (apply set x*))])
        (values `(lambda ,x* (free (,@(set->list free)) ,body)) (set))))]
  [`(,e* __1)
    (let-values ([(e* free*) (Expr* e*)])
      (values e* (apply set-union free*)))]
  ))

