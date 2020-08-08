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
  (let loop ([expr* expr*] [rexpr* '()] [rfree* '()])
    (cond
      [(null? expr*)
       (values (reverse rexpr*) (reverse rfree*))]
      [else
        (let-values ([(rexpr rfree) (Expr (car expr*))])
          (loop (cdr expr*) (cons rexpr rexpr*) (cons rfree rfree*)))])))

(define (Expr expr) (match expr
  [`(quote ,c) (values expr (set))]
  [(? primitive? pr) (values pr (set))]
  [(? variable? x) (values x (set x))]
  [`(begin ,expr* __1)
    (let-values ([(expr* free*) (Expr* expr*)])
      (values `(begin ,@expr*) (apply set-union free*)))]
  [`(let ([,x* ,e*] ___) ,body)
    (let-values ([(e* free*) (Expr* e*)])
      (let-values ([(body free) (Expr body)])
        (let* ([ufree (apply set-union (cons free free*))]
               [free (set-subtract ufree (apply set x*))])
          (let ([bindings (map list x* e*)])
            (values `(let ,bindings ,body) free)))))]
  [`(lambda (,x* ___) ,body)
    (let-values ([(body free) (Expr body)])
      (let ([free (set-subtract free (apply set x*))])
        (values `(lambda ,x* (free (,@(set->list free)) ,body)) (set))))]
  [`(if ,test ,conseq ,altern)
    (let-values ([(test tfree) (Expr test)]
                 [(conseq cfree) (Expr conseq)]
                 [(altern afree) (Expr altern)])
      (values `(if ,test ,conseq ,altern)
              (apply set-union (list tfree cfree afree))))]
  [`(,e* __1)
    (let-values ([(e* free*) (Expr* e*)])
      (values e* (apply set-union free*)))]
  ))

