#lang racket

(provide collect-code)

(require racket/match)
(require racket/trace)
(require "terminals.ss")

(define anon
  (let ()
    (define i 0)
    (lambda ()
      (let ([n i])
        (set! i (add1 i))
        (string->symbol
          (string-append "anon" (number->string n)))))))

(define (collect-code expr)
  (let-values ([(expr clabels) (Expr expr)])
   `(labels ,clabels ,expr)))

(define (Expr* expr*)
  (let loop ([expr* expr*] [rexpr* '()] [clabels '()])
    (cond
      [(null? expr*) (values (reverse rexpr*) clabels)]
      [else
        (let-values ([(expr eclabels) (Expr (car expr*))])
          (loop (cdr expr*) (cons expr rexpr*) (append eclabels clabels)))])))

(define (Expr expr) (match expr
  [`(quote ,c) (values expr '())]
  [(? variable? x) (values x '())]
  [`(begin ,expr* __1)
    (let-values ([(expr* clabels) (Expr* expr*)])
      (values `(begin ,@expr*) clabels))]
  [`(let ([,x* ,e*] ___) ,body)
    (let-values ([(e* eclabels) (Expr* e*)])
      (let-values ([(body bclabels) (Expr body)])
        (values `(let ,(map list x* e*) ,body)
                (append eclabels bclabels))))]
  [`(lambda (,x* ___) (free (,y* ___) ,body))
    (let ([label (anon)])
      (let-values ([(body clabels) (Expr body)])
        (values `(closure ,label ,@y*)
                (cons `(,label (code ,x* ,y* ,body)) clabels))))]
  [`(if ,test ,conseq ,altern)
    (let-values ([(test tclabels) (Expr test)]
                 [(conseq cclabels) (Expr conseq)]
                 [(altern aclabels) (Expr altern)])
      (values `(if ,test ,conseq ,altern)
              (append tclabels cclabels aclabels)))]
  [`(primcall ,pr ,e* ___)
    (let-values ([(e* clabels) (Expr* e*)])
      (values `(primcall ,pr ,@e*) clabels))]
  [`(funcall ,f ,e* ___)
    (let-values ([(f fclabels) (Expr f)]
                 [(e* eclabels) (Expr* e*)])
      (values `(funcall ,f ,@e*)
              (append eclabels fclabels)))]
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
