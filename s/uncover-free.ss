#lang racket

(provide uncover-free)

(require racket/match)
(require "terminals.ss")

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
  [(? string? s) (values expr (set))]
  [(? variable? x) (values x (set x))]
  [`(begin ,expr* __1)
    (let-values ([(expr* free*) (Expr* expr*)])
      (values `(begin ,@expr*) (set-union* free*)))]
  [`(let ([,x* ,e*] ___) ,body)
    (let-values ([(e* free*) (Expr* e*)])
      (let-values ([(body free) (Expr body)])
        (let* ([ufree (set-union* (cons free free*))]
               [free (set-subtract ufree (apply set x*))])
          (let ([bindings (map list x* e*)])
            (values `(let ,bindings ,body) free)))))]
  [`(lambda (,x* ___) ,body)
    (let-values ([(body free) (Expr body)])
      (let ([free (set-subtract free (apply set x*))])
        (values `(lambda ,x* (free (,@(set->list free)) ,body)) free)))]
  [`(if ,test ,conseq ,altern)
    (let-values ([(test tfree) (Expr test)]
                 [(conseq cfree) (Expr conseq)]
                 [(altern afree) (Expr altern)])
      (values `(if ,test ,conseq ,altern)
              (set-union* (list tfree cfree afree))))]
  [`(primcall ,pr ,e* ___)
    (let-values ([(e* free*) (Expr* e*)])
      (values `(primcall ,pr ,@e*) (set-union* free*)))]
  [`(funcall ,e* __1)
    (let-values ([(e* free*) (Expr* e*)])
      (values `(funcall ,@e*) (set-union* free*)))]
  ))

(define (set-union* sets)
  (if (null? sets)
    (set)
    (apply set-union sets)))


(module+ test
  (require rackunit)

  (define cases
    '(
      ('9 . '9)
      ((funcall
         (funcall
           (funcall
             (lambda (x0)
               (lambda (x1)
                 (lambda (x2)
                   (primcall + x2 (primcall + x0 x1)))))
             '10) '20) '30)
       .
       (funcall
         (funcall
           (funcall
             (lambda (x0) (free ()
               (lambda (x1) (free (x0)
                 (lambda (x2) (free (x0 x1)
                   (primcall + x2 (primcall + x0 x1))))))))
             '10) '20) '30))
      )
    )
 
  (let loop ([cases cases])
    (cond
      [(null? cases)
       #t]
      [else
        (let ([case (car cases)])
          (check-equal? (uncover-free (car case)) (cdr case))
          (loop (cdr cases)))]))

  (check-equal?
    (uncover-free '(primcall string))
    '(primcall string))
)
