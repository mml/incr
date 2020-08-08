#lang racket

(provide identify-tail-calls)

(require racket/match)
(require racket/trace)
(require "../lang/terminals.ss")

(define (identify-tail-calls prog)
  (Labels prog))

(define (Labels prog) (match prog
  [`(labels ([,x* ,code*] ___) ,body)
    `(labels ,(map list x* (map Code code*)) ,(Expr body #f))]))

(define (Code code) (match code
  [`(code (,x* ___) (,y* ___) ,body)
    `(code (,@x*) (,@y*) ,(Expr body #t))]))

(define (Expr* expr* tail?)
  (if (null? expr*)
      '()
      (let loop ([expr (car expr*)] [expr* (cdr expr*)] [rexpr* '()])
        (cond
          [(null? expr*)
           (reverse (cons (Expr expr tail?) rexpr*))]
          [else
            (loop (car expr*) (cdr expr*) (cons (Expr expr #f) rexpr*))]))))

(define (tailcall-bindings e*)
  (let loop ([e* e*] [bindings '()])
    (if (null? e*)
        (reverse bindings)
        (loop (cdr e*) (cons (list (tmp) (car e*)) bindings)))))

(define (Expr expr tail?) (match expr
  [(? immediate? c) c]
  [(? primitive? pr) pr]
  [(? variable? x) x]
  [`(closure ,label ,y* ___)
    `(closure ,label ,@y*)]
  [`(funcall ,f ,e* ___)
    (if tail?
        ; In order to make sure we don't clobber our own environment while
        ; setting up the stack for a tail call, we introduce temporary local
        ; variables to get fresh stack allocations.  We are also counting on
        ; the fact that the stack arguments will be set up left ot right.  This
        ; tweak probably doesn't belong in this pass, but for now it works.
        (let ([bindings (tailcall-bindings e*)])
          `(let ,bindings
             (tailcall ,f ,@(map car bindings))))
        `(funcall ,f ,@e*))]
  [`(begin ,expr* __1)
    `(begin ,@(Expr* expr* tail?))]
  [`(let ([,x* ,e*] ___) ,body)
    `(let ,(map list x* (Expr* e* #f)) ,(Expr body tail?))]
  [`(if ,test ,conseq ,altern)
    `(if ,(Expr test #f) ,(Expr conseq tail?) ,(Expr altern tail?))]
  [`(primcall ,pr ,e* ___)
    `(primcall ,pr ,@(Expr* e* #f))]
  ))

(define tmp
  (let ()
    (define i 0)
    (lambda ()
      (let ([n i])
        (set! i (add1 i))
        (string->symbol
          (string-append "tmp" (number->string n)))))))
