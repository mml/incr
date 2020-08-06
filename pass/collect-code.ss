#lang racket

(provide collect-code)

(require racket/match)
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
  (if (null? expr*)
      (error 'collect-code "internal bug: Expr* called with empty list")
      (let loop ([expr (car expr*)] [expr* (cdr expr*)] [rexpr* '()] [labels '()])
        (let-values ([(rexpr elabels) (Expr expr)])
          (if (null? expr*)
              (values (reverse (cons rexpr rexpr*)) (append elabels labels))
              (loop (car expr*) (cdr expr*) (cons rexpr rexpr*) (append elabels labels)))))))

(define (Expr expr) (match expr
  [(? immediate? c) (values c '())]
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
  [`(,e* __1)
    (Expr* e*)]
  ))

