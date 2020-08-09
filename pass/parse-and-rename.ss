#lang racket

(provide parse-and-rename)

(require racket/match)
(require racket/trace)
(require "../lang/terminals.ss")

(define (parse-and-rename expr)
  (Expr expr primitives))

(module+ test
  (require rackunit)

  (check-equal? (parse-and-rename 9) ''9)
  (check-equal? (parse-and-rename #t) ''#t)
  )

(define unique-variable
  (let ()
    (define i 0)
    (lambda (x)
      (let ([n i])
        (set! i (add1 i))
        (string->symbol
          (string-append (symbol->string x) "." (number->string n)))))))

(define (Expr* expr* env)
  (map (lambda (expr) (Expr expr env)) expr*))

(define (App e0 e* env)
  (let ([as (assq e0 env)])
    (unless as
      (error 'parse-and-rename "undefined variable ~a" e0))
    (let ([x (cdr as)])
      (cond
        [(symbol? x) 
         `(,x ,@(Expr* e* env))]
        [(number? x)
         (if (= x (length e*))
             `(,e0 ,@(Expr* e* env))
             (error 'parse-and-rename "arity mismatch: ~a expects ~a got ~a"
                    e0 x (length e*)))]
        [else
          (error 'parse-and-rename
                 "internal bug: unexpected value in environment for ~a: ~a"
                 e0 x)]))))

(define (Expr expr env) (match expr
  [(? immediate? c) `',c]
  [`(quote ,(? immediate? c)) expr]
  [(? symbol? x)
   (cond [(assq x env) => cdr]
         [else (error 'parse-and-rename "undefined variable ~a" x)])]
  [`(begin ,expr* __1)
    `(begin ,@(Expr* expr* env))]
  [`(let ([,x* ,e*] ___) ,body* __1) 
    (let* ([ux* (map unique-variable x*)]
           [e* (Expr* e* env)]
           [bindings (map list ux* e*)]
           [env (append (map cons x* ux*) env)])
      `(let ,bindings ,@(Expr* body* env)))]
  [`(lambda (,x* ___) ,body* __1)
    (let* ([ux* (map unique-variable x*)]
           [env (append (map cons x* ux*) env)])
      `(lambda ,ux* ,@(Expr* body* env)))]
  [`(if ,test ,conseq ,altern)
    `(if ,(Expr test env) ,(Expr conseq env) ,(Expr altern env))]
  [`(,(? symbol? e0) ,e* ___)
    (App e0 e* env)]
  [`(,e0 ,e* ___)
    `(,(Expr e0 env) ,@(Expr* e* env))]
  ))
