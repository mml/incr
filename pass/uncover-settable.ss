#lang racket

(provide uncover-settable)

(require racket/match)
(require racket/trace)
(require "../lang/terminals.ss")

(define (uncover-settable expr)
  (let-values ([(e settable) (Expr expr)])
    (unless (set-empty? settable)
      (error 'uncover-settable "internal bug: free variables ~a at top of expr ~a"
             settable expr))
    e))

(define (Expr* expr*)
  (let loop ([expr* expr*] [rexpr* '()] [rsettable* '()])
    (cond
      [(null? expr*)
       (values (reverse rexpr*) (reverse rsettable*))]
      [else
        (let-values ([(rexpr rsettable) (Expr (car expr*))])
          (loop (cdr expr*) (cons rexpr rexpr*) (cons rsettable rsettable*)))])))

(define (Expr expr) (match expr
  [`(quote ,c) (values expr (set))]
  [`(set! ,x ,e) (values expr (set x))]
  [(? primitive? pr) (values pr (set))]
  [(? variable? x) (values expr (set))]
  [`(begin ,expr* __1)
    (let-values ([(expr* settable*) (Expr* expr*)])
      (values `(begin ,@expr*) (apply set-union settable*)))]
  [`(let ([,x* ,e*] ___) ,body)
    (let-values ([(e* settable*) (Expr* e*)])
      (let-values ([(body settable) (Expr body)])
        (let* ([usettable (apply set-union (cons settable settable*))]
               [lsettable (set-intersect usettable (apply set x*))]
               [settable (set-subtract usettable lsettable)])
          (let ([bindings (map list x* e*)])
            (values `(let ,bindings
                       (settable (,@(set->list lsettable))
                                 ,body))
                    settable)))))]
  [`(lambda (,x* ___) ,body)
    (let-values ([(body settable) (Expr body)])
      (let* ([lsettable (set-intersect settable (apply set x*))]
             [settable (set-subtract settable lsettable)])
        (values `(lambda ,x*
                   (settable (,@(set->list lsettable))
                             ,body)) settable)))]
  [`(if ,test ,conseq ,altern)
    (let-values ([(test tsettable) (Expr test)]
                 [(conseq csettable) (Expr conseq)]
                 [(altern asettable) (Expr altern)])
      (values `(if ,test ,conseq ,altern)
              (apply set-union (list tsettable csettable asettable))))]
  [`(,e* __1)
    (let-values ([(e* settable*) (Expr* e*)])
      (values e* (apply set-union settable*)))]
  ))

(module+ test
  (require rackunit)

  (define cases
    '(
      ('9 . '9)
      (((((lambda (x0)
              (let ([r '#f])
                (lambda (x1)
                  (lambda (x2)
                    (begin
                      (set! r (+ x0 (+ x1 x2)))
                      r)))))
          '10) '20) '30)
       .
       ((((lambda (x0) (settable ()
              (let ([r '#f]) (settable (r)
                (lambda (x1) (settable ()
                  (lambda (x2) (settable ()
                    (begin
                      (set! r (+ x0 (+ x1 x2)))
                      r)))))))))
          '10) '20) '30))
      )
    )
 
  (let loop ([cases cases])
    (cond
      [(null? cases)
       #t]
      [else
        (let ([case (car cases)])
          (check-equal? (uncover-settable (car case)) (cdr case))
          (loop (cdr cases)))]))

)
