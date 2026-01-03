#lang racket

(provide parse-and-rename)

(require racket/match)
(require racket/trace)
(require "generators.ss")
(require "terminals.ss")

(define (initial-env) primitives)
(define (extend-env env k v)
  (cons (cons k v) env))

(define (extend-env* env k* v*)
  (let loop ([k* k*] [v* v*] [env env])
    (if (null? k*)
      env
      (loop (cdr k*)
            (cdr v*)
            (cons (cons (car k*) (car v*)) env)))))

(define (parse-and-rename expr)
  (Expr expr (initial-env)))

(module+ test ; parse-and-rename
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

(module+ test ;Expr*
  (check-equal? (Expr* '((+ 1 2)) primitives)
                '((primcall + '1 '2))))

(define (lambda-body expr* env) (match expr*
  [`((define ,k* ,x*) __1 ,body* ___)
    (Letrec* k* x* body* env)]
  [`(,body* __1)
    `(begin ,@(Expr* body* env))]))

(define (make-begin expr* env)
  (if (null? (cdr expr*))
    (Expr (car expr*) env)
    `(begin ,@(Expr* expr* env))))

(module+ test ; make-begin
  (check-equal? (make-begin '((car '())) primitives)
                '(primcall car '()))
  (check-equal? (make-begin '((car '()) (cdr '())) primitives)
                '(begin (primcall car '()) (primcall cdr '()))))

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

(module+ test ; Case
  #;(check-equal?
    (Case '10 '([else 999]) (initial-env))
    '(let ([tmp8 '10]) (begin '999)))
  #;(check-equal?
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

(module+ test ;Cond
  (check-equal? (Expr '(cond [else '#t]) primitives)
                '(cond [else '#t]))

  (check-equal? (Expr '(cond [(null? '())]) primitives)
                '(cond [(primcall null? '())]))
  (check-equal? (Expr '(cond [(null? '()) '1 '2 '3]) primitives)
                '(cond [(primcall null? '())
                        (begin '1 '2 '3)]))

  (check-match (Expr '(cond [(null? '()) => (lambda (l) (cons l l))]) primitives)
               `(cond [(primcall null? '()) => (lambda (,l) (primcall cons ,l ,l))]))

  #;(check-equal? (Cond '((even? '1) (odd? '1)) primitives)
                '(cond
                   [(even? '1)]
                   [(odd? '1)]))


  (check-match (Cond '([(null? '()) => (lambda (x) 10)]) primitives)
                     `(cond
                        [(primcall null? '()) => (lambda (,x) '10)]))
  )

(define (List expr* env) (match expr*
  ['() ''()]
  [`(,hd ,tl* ___) `(primcall cons ,(Expr hd env) ,(List tl* env))]))

(define (Vector expr* env)
  (let ([count (length expr*)]
        [vec-var (gensym 'vec)])
    (if (zero? count)
        `(primcall make-vector '0 (primcall void))
        `(let ([,vec-var (primcall make-vector ',count (primcall void))])
           ,(let build-sets ([exprs (Expr* expr* env)] [i 0])
              (if (null? exprs)
                  vec-var
                  `(let ([,(gensym '_) (primcall vector-set! ,vec-var ',i ,(car exprs))])
                     ,(build-sets (cdr exprs) (add1 i)))))))))

(define (Letrec* x* e* body* env)
  (let* ([ux* (map unique-variable x*)]
         [env (extend-env* env x* ux*)]
         [e* (Expr* e* env)])
    `(letrec* ,(map list ux* e*)
       ,@(Expr* body* env))))

(define (Letrec x* e* body* env)
  (let* ([ux* (map unique-variable x*)]
         [env (extend-env* env x* ux*)]
         [e* (Expr* e* env)])
    `(letrec ,(map list ux* e*)
       ,@(Expr* body* env))))

(module+ test ;Letrec
  (check-match
    (Letrec '(foo bar) '(9 (+ 1 baz)) '((+ foo bar))
            (cons '(baz . baz.1000) primitives))
    `(letrec ([,foo '9] [,bar (primcall + '1 baz.1000)])
       (primcall + ,foo ,bar))
    (syms-unique? foo bar))

  (check-match
    (Letrec '(a b) '(10 (+ a a)) '((+ a b)) primitives)
    `(letrec ([,a '10] [,b (primcall + ,a ,a)])
       (primcall + ,a ,b))
    (syms-unique? a b)))

; TODO: this should only be renaming variables, not rewriting let*
(define (Let* binding* body* env)
  (match binding*
    ['() `(let () ,@(Expr* body* env))]
    [`([,x ,e])
      (let ([ux (unique-variable x)])
        `(let ([,ux ,(Expr e env)])
           ,@(Expr* body* (extend-env env x ux))))]
    [`([,x ,e] ,binding* __1)
      (let ([ux (unique-variable x)])
        `(let ([,ux ,(Expr e env)])
           ,(Let* binding* body* (extend-env env x ux))))]))

(module+ test ; Let*
  (check-equal?
    (Let* '() '((+ 1 1)) primitives)
    '(let () (primcall + '1 '1)))

  (check-match
    (Let* '([a 10] [b (+ a a)]) '((+ a b)) primitives)
    `(let ([,a '10])
       (let ([,b (primcall + ,a ,a)])
         (primcall + ,a ,b)))
    (syms-unique? a b))
  )

(define Expr
  (lambda (expr env)
    (match expr
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
      [`(vector ,expr* ___)
        (Vector expr* env)]
      [`(letrec ([,x* ,e*] ___) ,body* __1)
        (Letrec x* e* body* env)]
      [`(letrec* ([,x* ,e*] ___) ,body* __1)
        (Letrec* x* e* body* env)]
      [`(let* ,binding* ,body* __1)
        (Let* binding* body* env)]
      [`(let ([,(? symbol? x*) ,e*] ___) ,body* __1)
        (let ([ux* (map unique-variable x*)])
          `(let ,(map list ux* (Expr* e* env))
             ,@(Expr* body* (extend-env* env x* ux*))))]
      [`(lambda (,x* ___) ,body* __1)
        (let ([ux* (map unique-variable x*)])
          `(lambda ,ux* ,@(Expr* body* (extend-env* env x* ux*))))]
      [`(if ,test ,conseq)
        `(if ,(Expr test env) ,(Expr conseq env))]
      [`(if ,test ,conseq ,altern)
        `(if ,(Expr test env) ,(Expr conseq env) ,(Expr altern env))]
      [`(when ,test ,conseq)
        `(when ,(Expr test env) ,(Expr conseq env))]
      [`(unless ,test ,altern)
        `(unless ,(Expr test env) ,(Expr altern env))]
      [`(quote ,_) expr]
      [(? string? c) c]
      [`(,(? symbol? e0) ,e* ___)
        (App e0 e* env)]
      [`(,e0 ,e* ___)
        `(funcall ,(Expr e0 env) ,@(Expr* e* env))]
      )))

(module+ test ; Expr
  (check-equal? (Expr '(+ 1 1) primitives)
                '(primcall + '1 '1))
  (check-equal? (Expr '(let () (+ 1 1)) primitives)
                '(let () (primcall + '1 '1)))

  (check-equal? (Expr '(quote 5) primitives) ''5)
  ;(check-equal? (Expr '(quote (2 . 5)) primitives) '(datum const0 (2 . 5)))
  ;(check-equal? (Expr '(quote (2 3 4)) primitives) '(datum const1 (2 3 4)))
  ;(check-equal? (Expr "foo" primitives) '(datum const2 "foo"))
  (check-equal? (Expr '(string) primitives) '(primcall string))
  (check-equal? (Expr '((lambda () '10)) primitives)
                '(funcall (lambda () '10)))
  ;(check-equal? (Expr '(string #\a) primitives) '(primcall string #\a))
  ;(check-equal? (Expr '(quote foo) primitives) '(datum const2 foo))
  ;(check-equal? (Expr '(quote (a b c)) primitives) '(datum const3 (a b c)))
  ;(check-equal? (Expr '(quote (if x)) primitives) '(datum const4 (if x)))
  )

