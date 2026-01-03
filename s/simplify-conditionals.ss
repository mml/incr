#lang racket

;;; This pass removes the following kinds of conditionals, leaving only a
;;; standard 2-armed if.
;;;
;;; - 1-armed if
;;; - when/unless
;;; - cond
;;; - case
;;; - and, or

(provide simplify-conditionals)

(require racket/match)
(require racket/trace)
(require "generators.ss")
(require "terminals.ss")

(define (simplify-conditionals expr)
  (Expr expr))

(define Expr
  (lambda (expr)
    (match expr
      [`(quote ,datum) `(quote ,datum)]
      [(? string? s) s]
      [(or
         `(if ,test ,conseq)
         `(when ,test ,conseq))
       `(if ,(Expr test) ,(Expr conseq) (primcall void))]
      [`(if ,test ,conseq ,altern) `(if ,(Expr test) ,(Expr conseq) ,(Expr altern))]
      [`(unless ,test ,altern) `(if ,(Expr test) (primcall void) ,(Expr altern))]
      [(? symbol? x) x]
      [`(cond ,clause* __1) (Cond clause*)]
      [`(case ,expr ,clause* __1)
        (Case expr clause*)]
      [`(begin ,expr* __1) `(begin ,@(map Expr expr*))]
      [`(let ([,x* ,e*] ___) ,body* __1)
        (let ([e* (map Expr e*)]
              [body* (map Expr body*)])
          `(let ,(map list x* e*) ,@body*))]
      [`(letrec ([,x* ,e*] ___) ,body* __1)
        `(letrec ,(map list x* (map Expr e*)) ,@(map Expr body*))]
      [`(letrec* ([,x* ,e*] ___) ,body* __1)
        `(letrec* ,(map list x* (map Expr e*)) ,@(map Expr body*))]
      [`(let* ([,x* ,e*] ___) ,body* __1)
        `(let* ,(map list x* (map Expr e*)) ,@(map Expr body*))]
      [`(lambda (,x* ___) ,body* __1)
        `(lambda (,@x*) ,@(map Expr body*))]
      [`(primcall and ,e* ___) (And e*)]
      [`(primcall or ,e* ___) (Or e*)]
      [`(primcall ,p ,e* ___)
        `(primcall ,p ,@(map Expr e*))]
      [`(funcall ,e0 ,e* ___)
        `(funcall ,(Expr e0) ,@(map Expr e*))])))

(define Cond
  (lambda (clause*)
    (if (null? clause*)
      '(primcall void)
      (match (car clause*)
        [`(else ,expr) (Expr expr)]
        [`(,test)
          (let ([t (tmp)])
            `(let ([,t ,(Expr test)])
               (if ,t ,t ,(Cond (cdr clause*)))))]
        [`(,test ,expr)
          `(if ,test ,expr ,(Cond (cdr clause*)))]
        [`(,test => ,expr)
          (let ([t (tmp)])
            `(let ([,t ,(Expr test)])
               (if ,t (funcall ,(Expr expr) ,t) ,(Cond (cdr clause*)))))]
        ))))

(define Case
  (let ()
    (define Clause*
      (lambda (t clause*)
        (if (null? clause*)
          '(primcall void)
          (match (car clause*)
            [`((,datum* __1) ,expr)
              `(if
                 (primcall memv ,t ',datum*)
                 ,(Expr expr)
                 ,(Clause* t (cdr clause*)))]
            [`(else ,expr) (Expr expr)]))))

    (lambda (expr clause*)
      (let ([t (tmp)])
        `(let ([,t ,(Expr expr)])
           ,(Clause* t clause*))))))

(define (And e*) (match e*
  ['() `'#t]
  [`(,e) (Expr e)]
  [`(,e ,e* __1) `(if ,(Expr e) ,(And e*) '#f)]))

(define (Or e*) (match e*
  ['() `'#f]
  [`(,e) (Expr e)]
  [`(,e ,e* __1)
    (let ([t (tmp)])
      `(let ([,t ,(Expr e)])
         (if ,t ,t ,(Or e*))))]))

(module+ test
  (require rackunit)
  (check-equal? (And '()) ''#t)
  (check-equal? (And '('1)) ''1)
  (check-equal? (And '('1 '2)) '(if '1 '2 '#f))
  (check-equal? (And '('1 '2 '3)) '(if '1 (if '2 '3 '#f) '#f))

  (check-equal? (Or '()) ''#f)
  (check-equal? (Or '('1)) ''1)
  (check-equal? (Or '('1 '2))
                '(let ([tmp0 '1])
                   (if tmp0 tmp0 '2)))

  (check-equal? (Expr '(when '1 '2)) '(if '1 '2 (primcall void)))
  (check-equal? (Expr '(unless '1 '2)) '(if '1 (primcall void) '2))

  (check-equal? (Expr '(cond [else '9])) ''9)
  (check-equal? (Expr '(cond [(primcall < x y)]
                             [(primcall > x y) z]
                             [(primcall list? x) => (lambda (x) (primcall not x))]
                             [else 'none-of-the-above]))
                '(let ([tmp1 (primcall < x y)])
                   (if tmp1 tmp1
                     (if (primcall > x y) z
                       (let ([tmp2 (primcall list? x)])
                         (if tmp2 (funcall (lambda (x) (primcall not x)) tmp2)
                           'none-of-the-above))))))

  (check-equal? (Expr '(case (primcall + x y)
                         [(1 3 5 7 9) 'odd]
                         [(0 2 4 6 8) 'even]
                         [else 'out-of-range]))
                '(let ([tmp3 (primcall + x y)])
                   (if (primcall memv tmp3 '(1 3 5 7 9))
                     'odd
                     (if (primcall memv tmp3 '(0 2 4 6 8))
                       'even
                       'out-of-range))))
  (check-equal? (Cond '([else (begin '1 '2 '3)]))
                '(begin '1 '2 '3))

  (check-match (Cond '([(primcall null? '()) => (lambda (x) '10)]))
                `(let ([,tmp (primcall null? '())])
                   (if ,tmp (funcall (lambda (x) '10) ,t) ,_)))
  (check-match (Cond '([(primcall null? '()) => (lambda (x) '10)]
                       [else '3]))
               `(let ([,t (primcall null? '())])
                   (if ,t
                       (funcall (lambda (,x) '10) ,t)
                       '3)))
  (check-match (Cond '([(primcall zero? (primcall add1 '0))]))
               `(let ([,t (primcall zero? (primcall add1 '0))])
                  (if ,t ,t ,_)))
  (check-match (Cond '([(primcall zero? (primcall add1 '0))]
                        [else '3]))
                `(let ([,t (primcall zero? (primcall add1 '0))])
                   (if ,t ,t '3)))
  (check-equal? (Cond '([(primcall zero? '0) '3]))
                '(if (primcall zero? '0) '3 (primcall void)))

  (check-equal? (Cond '([(primcall zero? '0) '1]
                        [(primcall zero? '1) '4]
                        [(primcall zero? '2) '7]
                        [else '()]))
                '(if (primcall zero? '0)
                   '1
                   (if (primcall zero? '1)
                     '4
                     (if (primcall zero? '2)
                       '7
                       '()))))

  (check-match (Expr '(case '10 [else '999]))
               `(let ([,t '10]) '999))

  (check-match (Expr '(case '10 [(1 2 3) '3] [else '999]))
               `(let ([,t '10])
                  (if (primcall memv ,t '(1 2 3))
                    '3
                    '999)))
)
