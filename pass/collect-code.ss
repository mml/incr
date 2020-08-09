#lang racket

(provide collect-code)

(require racket/match)
(require racket/trace)
(require "../lang/terminals.ss")

(define anon
  (let ()
    (define i 0)
    (lambda ()
      (let ([n i])
        (set! i (add1 i))
        (string->symbol
          (string-append "anon" (number->string n)))))))

(define (collect-code expr)
  (let-values ([(expr labels) (Expr expr)])
    `(labels ,labels ,expr)))

(define (Expr* expr*)
  (let loop ([expr* expr*] [rexpr* '()] [labels '()])
    (cond
      [(null? expr*) (values (reverse rexpr*) labels)]
      [else
        (let-values ([(expr elabels) (Expr (car expr*))])
          (loop (cdr expr*) (cons expr rexpr*) (append elabels labels)))])))

(define (Expr expr) (match expr
  [`(quote ,c) (values expr '())]
  [(? primitive? pr) (values pr '())]
  [(? variable? x) (values x '())]
  [`(begin ,expr* __1)
    (let-values ([(expr* labels) (Expr* expr*)])
      (values `(begin ,@expr*) labels))]
  [`(let ([,x* ,e*] ___) ,body)
    (let-values ([(e* elabels) (Expr* e*)])
      (let-values ([(body blabels) (Expr body)])
        (values `(let ,(map list x* e*) ,body)
                (append elabels blabels))))]
  [`(lambda (,x* ___) (free (,y* ___) ,body))
    (let ([label (anon)])
      (let-values ([(body labels) (Expr body)])
        (values `(closure ,label ,@y*)
                (cons `(,label (code ,x* ,y* ,body)) labels))))]
  [`(if ,test ,conseq ,altern)
    (let-values ([(test tlabels) (Expr test)]
                 [(conseq clabels) (Expr conseq)]
                 [(altern alabels) (Expr altern)])
      (values `(if ,test ,conseq ,altern)
              (append tlabels clabels alabels)))]
  [`(,(? primitive? pr) ,e* ___)
    (let-values ([(e* labels) (Expr* e*)])
      (values `(primcall ,pr ,@e*) labels))]
  [`(,f ,e* ___)
    (let-values ([(f flabels) (Expr f)]
                 [(e* elabels) (Expr* e*)])
      (values `(funcall ,f ,@e*)
              (append elabels flabels)))]
  ))

(module+ test
  (require rackunit)

  (define cases
    '(
      ('9 . (labels () '9))
      )
    )
 
  (let loop ([cases cases])
    (cond
      [(null? cases)
       #t]
      [else
        (let ([case (car cases)])
          (check-equal? (collect-code (car case)) (cdr case))
          (loop (cdr cases)))]))
  )
