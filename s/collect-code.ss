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
  (let-values ([(expr clabels dlabels) (Expr expr)])
   `(labels ,clabels ,dlabels ,expr)))

(define (Expr* expr*)
  (let loop ([expr* expr*] [rexpr* '()] [clabels '()] [dlabels '()])
    (cond
      [(null? expr*) (values (reverse rexpr*) clabels dlabels)]
      [else
        (let-values ([(expr eclabels edlabels) (Expr (car expr*))])
          (loop (cdr expr*) (cons expr rexpr*) (append eclabels clabels) (append edlabels dlabels)))])))

(define (Expr expr) (match expr
  [`(quote ,c) (values expr '() '())]
  [`(datum ,name ,d) (values `(constant-ref ,name) '() `((,name . ,d)))]
  [(? variable? x) (values x '() '())]
  [`(begin ,expr* __1)
    (let-values ([(expr* clabels dlabels) (Expr* expr*)])
      (values `(begin ,@expr*) clabels dlabels))]
  [`(let ([,x* ,e*] ___) ,body)
    (let-values ([(e* eclabels edlabels) (Expr* e*)])
      (let-values ([(body bclabels bdlabels) (Expr body)])
        (values `(let ,(map list x* e*) ,body)
                (append eclabels bclabels)
                (append edlabels bdlabels))))]
  [`(lambda (,x* ___) (free (,y* ___) ,body))
    (let ([label (anon)])
      (let-values ([(body clabels dlabels) (Expr body)])
        (values `(closure ,label ,@y*)
                (cons `(,label (code ,x* ,y* ,body)) clabels)
                dlabels)))]
  [`(if ,test ,conseq ,altern)
    (let-values ([(test tclabels tdlabels) (Expr test)]
                 [(conseq cclabels cdlabels) (Expr conseq)]
                 [(altern aclabels adlabels) (Expr altern)])
      (values `(if ,test ,conseq ,altern)
              (append tclabels cclabels aclabels)
              (append tdlabels cdlabels adlabels)))]
  [`(primcall ,pr ,e* ___)
    (let-values ([(e* clabels dlabels) (Expr* e*)])
      (values `(primcall ,pr ,@e*) clabels dlabels))]
  [`(funcall ,f ,e* ___)
    (let-values ([(f fclabels fdlabels) (Expr f)]
                 [(e* eclabels edlabels) (Expr* e*)])
      (values `(funcall ,f ,@e*)
              (append eclabels fclabels)
              (append edlabels fdlabels)))]
  ))

(module+ test
  (require rackunit)

  (define cases
    '(
      ('9 . (labels () () '9))
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

  (check-equal?
    (collect-code '(datum const0 (2 . 5)))
    '(labels () ([const0 . (2 . 5)]) (constant-ref const0)))
  )
