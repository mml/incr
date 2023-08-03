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
         `(funcall ,x ,@(Expr* e* env))]
        [(number? x)
         (if (= x (length e*))
             `(primcall ,e0 ,@(Expr* e* env))
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
         (if ,t (funcall ,(Expr result env) ,t) '#f)))] ; altern unspecified
  [`([,test => ,result] ,clause* __1)
    (let ([t (tmp)])
      `(let ([,t ,(Expr test env)])
         (if ,t
             (funcall ,(Expr result env) ,t)
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
                '(let ([tmp0 (primcall null? '())])
                   (if tmp0 (funcall (lambda (x.1) '10) tmp0) '#f)))
  (check-equal? (Cond '([(null? '()) => (lambda (x) 10)]
                        [else 1 2 3]) primitives)
                '(let ([tmp1 (primcall null? '())])
                   (if tmp1
                       (funcall (lambda (x.2) '10) tmp1)
                       (begin '1 '2 '3))))
  (check-equal? (Cond '([(zero? (add1 0))]) primitives)
                '(primcall zero? (primcall add1 '0)))
  (check-equal? (Cond '([(zero? (add1 0))]
                        [else 1 2 3]) primitives)
                '(let ([tmp2 (primcall zero? (primcall add1 '0))])
                   (if tmp2
                       tmp2
                       (begin '1 '2 '3))))
  (check-equal? (Cond '([(zero? 0) 1 2 3]) primitives)
                '(if (primcall zero? '0) (begin '1 '2 '3) '#f))
  (check-equal? (Cond '([(zero? 0) 1 2 3]
                        [(zero? 1) 4 5 6]
                        [(zero? 2) 7 8 9]
                        [else '()]) primitives)
                '(if (primcall zero? '0)
                     (begin '1 '2 '3)
                     (if (primcall zero? '1)
                         (begin '4 '5 '6)
                         (if (primcall zero? '2)
                             (begin '7 '8 '9)
                             (begin '())))))
  )

(define (Or expr* env) (match expr*
  ['() `'#f]
  [`(,test) (Expr test env)]
  [`(,test ,test* __1) 
    (let ([t (tmp)])
      `(let ([,t ,(Expr test env)])
         (if ,t ,t ,(Or test* env))))]))

(module+ test
  (check-equal? (Or '() primitives) ''#f)
  (check-equal? (Or '(1) primitives) ''1)
  (check-equal? (Or '(1 2) primitives)
                '(let ([tmp3 '1])
                   (if tmp3 tmp3 '2))))

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

(define (List expr* env) (match expr*
  ['() ''()]
  [`(,hd ,tl* ___) `(primcall cons ,(Expr hd env) ,(List tl* env))]))

(define (Letrec x* e* body* env)
  (let* ([ux* (map unique-variable x*)]
         [xbindings (map (lambda (x) (list x ''#f)) ux*)]
         [t* (map (lambda (x) (tmp)) ux*)]
         [tbindings (map (lambda (t e) (list t e)) t* e*)]
         [set-expr* (map (lambda (x t)
                           `(set! ,x ,t))
                         x* t*)]
         [env (append (map cons t* t*) (map cons x* ux*) env)])
    `(let ,xbindings
       (let ,(map list t* (Expr* e* env))
         ,@(Expr* set-expr* env)
         ,@(Expr* body* env)))))

(module+ test
  (check-equal?
    (Letrec '(foo bar) '(9 (+ 1 baz)) '((+ foo bar))
            (cons '(baz . baz.1000) primitives))
    '(let ([foo.3 '#f] [bar.4 '#f])
       (let ([tmp4 '9] [tmp5 (primcall + '1 baz.1000)])
         (primcall set! foo.3 tmp4)
         (primcall set! bar.4 tmp5)
         (primcall + foo.3 bar.4)))))

(define (Let* binding* body* env) (match binding*
  ['() `(let () ,@(Expr* body* env))]
  [`([,x ,e]) (Expr `(let ([,x ,e]) ,@body*) env)]
  [`([,x ,e] ,binding* __1) (Expr `(let ([,x ,e])
                                  (let* ,binding* ,@body*)) env)]))

(define (Expr expr env) (match expr
  [(? immediate? c) `',c]
  [`(quote ,(? immediate? c)) expr]
  [(? symbol? x)
   (cond [(assq x env) => cdr]
         [else (error 'parse-and-rename "undefined variable ~a" x)])]
  [`(and ,expr* ___)
    (And expr* env)]
  [`(or ,expr* ___)
    (Or expr* env)]
  [`(cond ,clause* __1)
    (Cond clause* env)]
  [`(begin ,expr* __1)
    `(begin ,@(Expr* expr* env))]
  [`(list ,expr* ___)
    (List expr* env)]
  [`(letrec ([,x* ,e*] ___) ,body* __1)
    (Letrec x* e* body* env)]
  [`(let* ,binding* ,body* __1)
    (Let* binding* body* env)]
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
    `(funcall ,(Expr e0 env) ,@(Expr* e* env))]
  ))
