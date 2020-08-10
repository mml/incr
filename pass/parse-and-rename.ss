#lang racket

(provide parse-and-rename)

(require racket/match)
(require racket/trace)
(require "generators.ss")
(require "../lang/terminals.ss")

(define (parse-and-rename expr)
  (Expr expr primitives))

(module+ test
  (require rackunit)

  (check-equal? (parse-and-rename 9) ''9)
  (check-equal? (parse-and-rename #t) ''#t)
  (check-exn exn:fail? (lambda () (parse-and-rename '(+ x 9))))
  (check-not-exn (lambda () (parse-and-rename '(let ([x 10]) (+ x 9)))))

  )

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

(define (Cond cond* env) (match cond*
  [`([else ,result* __1])
    `(begin ,@(Expr* result* env))]
  [`([,test => ,result])
    (let ([t (tmp)])
      `(let ([,t ,(Expr test env)])
         (if ,t (,(Expr result env) ,t) '#f)))] ; altern unspecified
  [`([,test => ,result] ,clause* __1)
    (let ([t (tmp)])
      `(let ([,t ,(Expr test env)])
         (if ,t
             (,(Expr result env) ,t)
             ,(Cond clause* env))))]
  [`([,test]) (Expr test env)]
  [`([,test] ,clause* __1)
    (let ([t (tmp)])
      `(let ([,t ,(Expr test env)])
         (if ,t
             ,t
             ,(Cond clause* env))))]
  [`([,test ,result* __1])
    `(if ,(Expr test env)
         (begin ,@(Expr* result* env))
         '#f)]
  [`([,test ,result* __1] ,clause* __1)
      `(if ,(Expr test env)
           (begin ,@(Expr* result* env))
           ,(Cond clause* env))]
  ))

(module+ test
  (check-equal? (Cond '([else 1 2 3]) primitives)
                '(begin '1 '2 '3))
  (check-equal? (Cond '([(null? '()) => (lambda (x) 10)]) primitives)
                '(let ([tmp0 (null? '())])
                   (if tmp0 ((lambda (x.1) '10) tmp0) '#f)))
  (check-equal? (Cond '([(null? '()) => (lambda (x) 10)]
                        [else 1 2 3]) primitives)
                '(let ([tmp1 (null? '())])
                   (if tmp1
                       ((lambda (x.2) '10) tmp1)
                       (begin '1 '2 '3))))
  (check-equal? (Cond '([(zero? (add1 0))]) primitives)
                '(zero? (add1 '0)))
  (check-equal? (Cond '([(zero? (add1 0))]
                        [else 1 2 3]) primitives)
                '(let ([tmp2 (zero? (add1 '0))])
                   (if tmp2
                       tmp2
                       (begin '1 '2 '3))))
  (check-equal? (Cond '([(zero? 0) 1 2 3]) primitives)
                '(if (zero? '0) (begin '1 '2 '3) '#f))
  (check-equal? (Cond '([(zero? 0) 1 2 3]
                        [(zero? 1) 4 5 6]
                        [(zero? 2) 7 8 9]
                        [else '()]) primitives)
                '(if (zero? '0)
                     (begin '1 '2 '3)
                     (if (zero? '1)
                         (begin '4 '5 '6)
                         (if (zero? '2)
                             (begin '7 '8 '9)
                             (begin '())))))
  )


(define (And expr* env) (match expr*
  ['() `'#t]
  [`(,test) (Expr test env)]
  [`(,test ,test* __1) `(if ,(Expr test env) ,(And test* env) '#f)]))

(module+ test
  (check-equal? (And '() primitives) ''#t)
  (check-equal? (And '(1) primitives) ''1)
  (check-equal? (And '(1 2) primitives) '(if '1 '2 '#f))
  (check-equal? (And '(1 2 3) primitives) '(if '1 (if '2 '3 '#f) '#f))
  )

(define (Expr expr env) (match expr
  [(? immediate? c) `',c]
  [`(quote ,(? immediate? c)) expr]
  [(? symbol? x)
   (cond [(assq x env) => cdr]
         [else (error 'parse-and-rename "undefined variable ~a" x)])]
  [`(and ,expr* ___)
    (And expr* env)]
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
