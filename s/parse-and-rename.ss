#lang racket

(provide parse-and-rename)

(require racket/match)
(require racket/trace)
(require "generators.ss")
(require "terminals.ss")

(define (initial-env) primitives)

(define (parse-and-rename expr)
  (Expr expr (initial-env)))

(module+ test
  (require rackunit)
  (define (syms-unique? . args)
    (let loop ([syms args])
      (if (null? syms)
        #t
        (and (not (memv (car syms) (cdr syms)))
             (loop (cdr syms))))))

  (check-equal? (parse-and-rename 9) ''9)
  (check-equal? (parse-and-rename #t) ''#t)
  (check-exn exn:fail? (lambda () (parse-and-rename '(+ x 9))))
  (check-not-exn (lambda () (parse-and-rename '(let ([x 10]) (+ x 9))))))

(define (Expr* expr* env)
  (map (lambda (expr) (Expr expr env)) expr*))

(define (lambda-body expr* env) (match expr*
  [`((define ,k* ,x*) __1 ,body* ___)
    (Letrec* k* x* body* env)]
  [`(,body* __1)
    `(begin ,@(Expr* body* env))]))

(define (make-begin expr* env)
  (if (null? (cdr expr*))
    (Expr (car expr*) env)
    `(begin ,@(Expr* expr* env))))

(module+ test
  (check-equal? (make-begin '((car '())) primitives)
                '(primcall car '()))
  (check-equal? (make-begin '((car '()) (cdr '())) primitives)
                '(begin (primcall car '()) (primcall cdr '()))))

(module+ test
  (check-equal? (Expr* '((+ 1 2)) primitives)
                '((primcall + '1 '2))))

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
        [(procedure? x)
         (x e* env)]
        ['() `(primcall ,e0 ,@(Expr* e* env))]
        [else
          (error 'parse-and-rename
                 "internal bug: unexpected value in environment for ~a: ~a"
                 e0 x)]))))

(define Case
  (let ()
    (define Clause
      (lambda (clause env)
        (match clause
          [`((,datum* __1) ,expr* __1)
           `((,@datum*) ,(make-begin expr* env))]
          [`(else ,expr* __1)
            `(else ,(make-begin expr* env))])))

    (lambda (expr clause* env)
      (match clause*
        [(and
           `(,_ __1) ; at least one clause
           `(
             ((,(? datum? _) __1) ,_ __1) ___
             .
             ,(or
                `((else ,_ __1))
                '())))
         `(case ,(Expr expr env) ,@(map (lambda (clause) (Clause clause env))
                                        clause*))]))))

(define Cond
  (let ()
    (define Clause
      (lambda (clause env)
        (match clause
          [`(else ,expr* __1) `(else ,(make-begin expr* env))]
          [`(,test) `(,(Expr test env))]
          [`(,test => ,expr) `(,(Expr test env) => ,(Expr expr env))]
          [`(,test ,expr* __1) `(,(Expr test env) ,(make-begin expr* env))])))

    (lambda (clause* env)
      (match clause*
        [(or
           `( ,(or `(,_) `(,_ ,_ __1) `(,_ => ,_)) ___
                                                `(else ,_ __1))
           `( ,(or `(,_) `(,_ ,_ __1) `(,_ => ,_)) __1))
         `(cond ,@(map (lambda (clause) (Clause clause env))
                       clause*))]))))

(module+ test
  (check-equal? (Expr '(+ 1 1) primitives)
                '(primcall + '1 '1))
  (check-equal? (Expr '(let () (+ 1 1)) primitives)
                '(let () (begin (primcall + '1 '1))))

  (check-equal? (Expr '(cond [else '#t]) primitives)
                '(cond [else '#t]))

  (check-equal? (Expr '(cond [(null? '())]) primitives)
                '(cond [(primcall null? '())]))
  (check-equal? (Expr '(cond [(null? '()) '1 '2 '3]) primitives)
                '(cond [(primcall null? '())
                        (begin '1 '2 '3)]))

  (check-match (Expr '(cond [(null? '()) => (lambda (l) (cons l l))]) primitives)
               `(cond [(primcall null? '()) => (lambda (,l) (begin (primcall cons ,l ,l)))]))

  #|
  (check-equal? (Cond '((even? '1) (odd? '1)) primitives)
                '(cond
                   [(even? '1)]
                   [(odd? '1)))
                    |#

  #;(check-match (Cond '([(null? '()) => (lambda (x) 10)]) primitives)
                     `(cond
                        [(null? '()) => (lambda (,x) 10)]))
  )

(define (List expr* env) (match expr*
  ['() ''()]
  [`(,hd ,tl* ___) `(primcall cons ,(Expr hd env) ,(List tl* env))]))

(define (Letrec* x* e* body* env)
  (let* ([ux* (map unique-variable x*)]
         [xbindings (map (lambda (x) (list x ''#f)) ux*)]
         [set-expr* (map (lambda (x e)
                           `(set! ,x ,e))
                         x* e*)]
         [env (append (map cons x* ux*) env)])
    `(let ,xbindings
       (begin
         ,@(Expr* set-expr* env)
         ,(lambda-body body* env)))))

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
         (begin
           ,@(Expr* set-expr* env)
           ,(lambda-body body* env))))))

(module+ test
  (check-match
    (Letrec '(foo bar) '(9 (+ 1 baz)) '((+ foo bar))
            (cons '(baz . baz.1000) primitives))
    `(let ([,foo '#f] [,bar '#f])
       (let ([,t1 '9] [,t2 (primcall + '1 baz.1000)])
         (begin
           (primcall set! ,foo ,t1)
           (primcall set! ,bar ,t2)
           (begin (primcall + ,foo ,bar)))))
    (syms-unique? foo bar t1 t2))

  (check-match
    (Letrec '(a b) '(10 (+ a a)) '((+ a b)) primitives)
    `(let ([,a '#f] [,b '#f])
       (let ([,t1 '10] [,t2 (primcall + ,a ,a)])
         (begin
           (primcall set! ,a ,t1)
           (primcall set! ,b ,t2)
           (begin (primcall + ,a ,b)))))
    (syms-unique? a b t1 t2)))

(define (Let* binding* body* env) (match binding*
  ['() `(let () ,(lambda-body body* env))]
  [`([,x ,e]) (Expr `(let ([,x ,e]) ,@body*) env)]
  [`([,x ,e] ,binding* __1)
    (Expr `(let ([,x ,e])
             (let* ,binding* ,@body*)) env)]))

(module+ test
  (check-equal?
    (Let* '() '((+ 1 1)) primitives)
    '(let () (begin (primcall + '1 '1))))

  (check-match
    (Let* '([a 10] [b (+ a a)]) '((+ a b)) primitives)
    `(let ([,a '10])
       (begin
         (let ([,b (primcall + ,a ,a)])
           (begin (primcall + ,a ,b)))))
    (syms-unique? a b))
  )

(define (Expr expr env) (match expr
  [(? immediate? c) `',c]
  [(? symbol? x)
   (cond [(assq x env) => cdr]
         [else (error 'parse-and-rename "undefined variable ~a" x)])]
  [`(cond ,clause* __1)
    (Cond clause* env)]
  [`(case ,expr ,clause* __1)
    (Case expr clause* env)]
  [`(begin ,expr* __1)
    `(begin ,@(Expr* expr* env))]
  [`(list ,expr* ___)
    (List expr* env)]
  [`(letrec ([,x* ,e*] ___) ,body* __1)
    (Letrec x* e* body* env)]
  [`(letrec* ([,x* ,e*] ___) ,body* __1)
    (Letrec* x* e* body* env)]
  [`(let* ,binding* ,body* __1)
    (Let* binding* body* env)]
  [`(let ([,x* ,e*] ___) ,body* __1)
    (let* ([ux* (map unique-variable x*)]
           [e* (Expr* e* env)]
           [bindings (map list ux* e*)]
           [env (append (map cons x* ux*) env)])
      `(let ,bindings ,(lambda-body body* env)))]
  [`(lambda (,x* ___) ,body* __1)
    (let* ([ux* (map unique-variable x*)]
           [env (append (map cons x* ux*) env)])
      `(lambda ,ux* ,(lambda-body body* env)))]
  [`(if ,test ,conseq)
    `(if ,(Expr test env) ,(Expr conseq env))]
  [`(if ,test ,conseq ,altern)
    `(if ,(Expr test env) ,(Expr conseq env) ,(Expr altern env))]
  [`(when ,test ,conseq)
    (Expr `(if ,test ,conseq (void)) env)]
  [`(unless ,test ,altern)
    (Expr `(if ,test (void) ,altern) env)]
  [`(quote ,_) expr]
  [(? string? c) c]
  [`(,(? symbol? e0) ,e* ___)
    (App e0 e* env)]
  [`(,e0 ,e* ___)
    `(funcall ,(Expr e0 env) ,@(Expr* e* env))]
  ))

(module+ test
  (check-equal? (Expr '(quote 5) primitives) ''5)
  ;(check-equal? (Expr '(quote (2 . 5)) primitives) '(datum const0 (2 . 5)))
  ;(check-equal? (Expr '(quote (2 3 4)) primitives) '(datum const1 (2 3 4)))
  ;(check-equal? (Expr "foo" primitives) '(datum const2 "foo"))
  (check-equal? (Expr '(string) primitives) '(primcall string))
  (check-equal? (Expr '((lambda () '10)) primitives)
                '(funcall (lambda () (begin '10))))
  ;(check-equal? (Expr '(string #\a) primitives) '(primcall string #\a))
  ;(check-equal? (Expr '(quote foo) primitives) '(datum const2 foo))
  ;(check-equal? (Expr '(quote (a b c)) primitives) '(datum const3 (a b c)))
  ;(check-equal? (Expr '(quote (if x)) primitives) '(datum const4 (if x)))
  )

(module+ test
  #|
  (check-match (Expr '(let ([v (make-vector 5 0)])
                        (letrec
                          ([uv (lambda (v n)
                                 (cond
                                   [(< n 0) v]
                                   [else
                                     (vector-set! v n n)
                                     (uv v (sub1 n))]))])
                          (uv v 4)))
                     primitives)
               `(let ([,v (primcall make-vector '5 '0)])
                  (begin
                    (let ([,uv ,_])
                      (let ([,t (lambda (,xv ,xn)
                                  (begin
                                    (cond [(primcall < ,xn '0) ,xv]
                                          [else
                                            (begin
                                              (primcall vector-set! ,xv ,xn ,xn)
                                              (funcall ,uv ,v (primcall sub1 ,xn)))])))])
                        (begin
                          (primcall set! ,uv ,t)
                          (begin
                            (funcall ,uv ,v '4)))))))
               (syms-unique? v uv xv xn))
  |#

  #|
  (check-equal?
    (Case '10 '([else 999]) (initial-env))
    '(let ([tmp8 '10]) (begin '999)))
  (check-equal?
    (Case '10
          '([(1 2 3) 3]
            [else 999])
          (initial-env))
    '(let ([tmp9 '10])
       (if (let ([tmp10.9 '#f])
             (let ([tmp11 (lambda (x.10 ls.11)
                            (begin
                              (if (primcall null? ls.11)
                                (begin '#f)
                                (if (primcall eqv? (primcall car ls.11) x.10)
                                  (begin ls.11)
                                  (begin (funcall tmp10.9 x.10 (primcall cdr ls.11)))))))])
               (primcall set! tmp10.9 tmp11)
               (begin
                 (funcall tmp10.9 tmp9 (datum const3 (1 2 3))))))
         (begin '3)
         (begin '999))))
  |#
  )

