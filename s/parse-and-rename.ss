#lang racket

(provide parse-and-rename)

(require racket/match)
(require racket/trace)
(require "generators.ss")
(require "terminals.ss")

(define (initial-env)
  (cons (cons 'memv Memv) primitives))

(define (parse-and-rename expr)
  (Expr expr (initial-env)))

(module+ test
  (require rackunit)

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

(define (Case expr clause* env)
  (let* ([t (tmp)]
         [env (cons (cons t t) env)]
         [keys* (map car clause*)]
         [expr** (map (lambda (expr*) (Expr* expr* env))
                      (map cdr clause*))])
    `(let ([,t ,(Expr expr env)])
       ,(let loop ([keys (car keys*)]
                   [keys* (cdr keys*)]
                   [expr* (car expr**)]
                   [expr** (cdr expr**)])
          (if (null? keys*)
            (match keys
              [else `(begin ,@expr*)]
              [`(,k* __1)
                `(if ,(Memv (list t `'(,@keys)) env)
                   (begin ,@expr*)
                   (primcall 'void))])
            ; Skipping over a match here... this will make a syntax error a little obscure
            `(if ,(Memv (list t `'(,@keys)) env)
               (begin ,@expr*)
               ,(loop (car keys*) (cdr keys*)
                      (car expr**) (cdr expr**))))))))

; TODO: add tests for memv
(define (Memv arg* env)
  (let* ([t (tmp)])
    (Expr `(letrec ([,t (lambda (x ls)
                          (cond
                            [(null? ls) #f]
                            [(eqv? (car ls) x) ls]
                            [else (,t x (cdr ls))]))])
             (,t ,@arg*)) env)))

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
  (check-equal? (Expr '(+ 1 1) primitives)
                '(primcall + '1 '1))
  (check-equal? (Expr '(let () (+ 1 1)) primitives)
                '(let () (begin (primcall + '1 '1))))
  (check-equal? (Cond '([else 1 2 3]) primitives)
                '(begin '1 '2 '3))
  (check-equal? (Cond '([(null? '()) => (lambda (x) 10)]) primitives)
                '(let ([tmp0 (primcall null? '())])
                   (if tmp0 (funcall (lambda (x.1) (begin '10)) tmp0) '#f)))
  (check-equal? (Cond '([(null? '()) => (lambda (x) 10)]
                        [else 1 2 3]) primitives)
                '(let ([tmp1 (primcall null? '())])
                   (if tmp1
                       (funcall (lambda (x.2) (begin '10)) tmp1)
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

(define (Letrec* x* e* body* env)
  (let* ([ux* (map unique-variable x*)]
         [xbindings (map (lambda (x) (list x ''#f)) ux*)]
         [set-expr* (map (lambda (x e)
                           `(set! ,x ,e))
                         x* e*)]
         [env (append (map cons x* ux*) env)])
    `(let ,xbindings
       ,@(Expr* set-expr* env)
       ,(lambda-body body* env))))

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
         ,(lambda-body body* env)))))

(module+ test
  (check-equal?
    (Letrec '(foo bar) '(9 (+ 1 baz)) '((+ foo bar))
            (cons '(baz . baz.1000) primitives))
    '(let ([foo.3 '#f] [bar.4 '#f])
       (let ([tmp4 '9] [tmp5 (primcall + '1 baz.1000)])
         (primcall set! foo.3 tmp4)
         (primcall set! bar.4 tmp5)
         (begin (primcall + foo.3 bar.4))))))

(module+ test
  (check-equal?
    (Letrec '(a b) '(10 (+ a a)) '((+ a b)) primitives)
    '(let ([a.5 '#f] [b.6 '#f])
      (let ([tmp6 '10] [tmp7 (primcall + a.5 a.5)])
       (primcall set! a.5 tmp6)
       (primcall set! b.6 tmp7)
       (begin (primcall + a.5 b.6))))))


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

  (check-equal?
    (Let* '([a 10] [b (+ a a)]) '((+ a b)) primitives)
    '(let ([a.7 '10])
       (begin
         (let ([b.8 (primcall + a.7 a.7)])
           (begin (primcall + a.7 b.8)))))
    ))

(define (Complex expr env) (match expr
  [`(,tl) (cons (Complex tl env) '())]
  [`(,hd . ,tl) (cons (Complex hd env) (Complex tl env))]
  [(? symbol? c) `(primcall quote ,c)]
  [(? string? c) c]
  [(? immediate? c) c]))

(define (Expr expr env) (match expr
  [(? immediate? c) `',c]
  [(? symbol? x)
   (cond [(assq x env) => cdr]
         [else (error 'parse-and-rename "undefined variable ~a" x)])]
  [`(and ,expr* ___)
    (And expr* env)]
  [`(or ,expr* ___)
    (Or expr* env)]
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
  [`(if ,test ,conseq ,altern)
    `(if ,(Expr test env) ,(Expr conseq env) ,(Expr altern env))]
  [`(when ,test ,conseq)
    (Expr `(if ,test ,conseq (void)) env)]
  [`(unless ,test ,altern)
    (Expr `(if ,test (void) ,altern) env)]
  [`(quote ,(? immediate? c)) expr]
  [`(quote ,(? pair? c)) `(datum ,(unique-const) ,(Complex c env))]
  [`(quote ,(? symbol? c)) `(datum ,(unique-const) ,(Complex c env))]
  [`(quote ,x* ___) (error 'parse-and-rename "unsupported quote: ~a" expr)]
  [(? string? c) `(datum ,(unique-const) ,(Complex c env))]
  [`(,(? symbol? e0) ,e* ___)
    (App e0 e* env)]
  [`(,e0 ,e* ___)
    `(funcall ,(Expr e0 env) ,@(Expr* e* env))]
  ))

(module+ test
  (check-equal? (Expr '(quote 5) primitives) ''5)
  (check-equal? (Expr '(quote (2 . 5)) primitives) '(datum const0 (2 . 5)))
  (check-equal? (Expr '(quote (2 3 4)) primitives) '(datum const1 (2 3 4)))
  (check-equal? (Expr "foo" primitives) '(datum const2 "foo"))
  (check-equal? (Expr '(string) primitives) '(primcall string))
  ;(check-equal? (Expr '(string #\a) primitives) '(primcall string #\a))
  ;(check-equal? (Expr '(quote foo) primitives) '(datum const2 foo))
  ;(check-equal? (Expr '(quote (a b c)) primitives) '(datum const3 (a b c)))
  ;(check-equal? (Expr '(quote (if x)) primitives) '(datum const4 (if x)))
  )

(module+ test
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
  )

