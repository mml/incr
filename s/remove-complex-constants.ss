#lang racket

;;; This pass interns complex constants as described in Ghuloum's paper section 3.11.
;;; Input: (labels (...) body) from collect-code
;;; Output: (labels (...datum-labels...) (begin (constant-init ...) body))
;;;
;;; Constants are collected, assigned shared labels, and replaced with constant-ref calls.
;;; Initialization code is generated using constant-init primitives.

(provide remove-complex-constants)

(require racket/match)
(require racket/trace)
(require "terminals.ss")
(require "generators.ss")

(struct constant-env (constants-map counter datum-labels) #:transparent)

(define (empty-env)
  (constant-env '() 0 '()))

(define (env-lookup env datum)
  (assoc datum (constant-env-constants-map env)))

(define (env-add-constant env datum)
  (let* ([label (string->symbol (format "datum_~a" (constant-env-counter env)))]
         [new-map (cons (cons datum label) (constant-env-constants-map env))]
         [new-datum-labels (cons (list label '(datum)) (constant-env-datum-labels env))]
         [new-counter (add1 (constant-env-counter env))])
    (values (constant-env new-map new-counter new-datum-labels) label)))

(define (lookup-or-add env datum)
  (cond
    [(env-lookup env datum)
     => (lambda (entry)
          (values env (cdr entry)))]
    [else (let-values ([(env label) (env-add-constant env datum)])
            (values env label))]))

(trace-define (remove-complex-constants expr)
  (match expr
    [`(labels ,labels ,body)
     (let*-values ([(labels env) (Labels labels (empty-env))]
                   [(body env) (Expr body env)])
       (let ([init-code (generate-initializers (constant-env-constants-map env))]
             [datum-labels (reverse (constant-env-datum-labels env))])
         `(labels ,(append labels datum-labels)
            (begin ,@init-code ,body))))]
    [_ (error 'remove-complex-constants "Expected (labels ...) form, got ~a" expr)]))

(define (Labels labels env)
  (let loop ([labels labels] [env env] [result '()])
    (if (null? labels)
      (values (reverse result) env)
      (let* ([label-name (caar labels)]
             [code (cadar labels)])
        (let-values ([(code env) (Expr code env)])
          (loop (cdr labels)
                env
                (cons (list label-name code) result)))))))

(define (Expr expr env)
  (match expr
    [(or `(quote ,(? complex-datum? ex))
         (? string? ex))
      (let-values ([(env label) (lookup-or-add env ex)])
        (values `(primcall constant-ref ,label) env))]
    [`(code ,formals ,free ,body)
     (let-values ([(body env) (Expr body env)])
       (values `(code ,formals ,free ,body) env))]
    [`(lambda ,formals ,body)
     (let-values ([(body env) (Expr body env)])
       (values `(lambda ,formals ,body) env))]
    [`(if ,test ,conseq ,altern)
     (let*-values ([(test env) (Expr test env)]
                   [(conseq env) (Expr conseq env)]
                   [(altern env) (Expr altern env)])
       (values `(if ,test ,conseq ,altern) env))]
    [`(let ([,x* ,e*] ...) ,body)
     (let*-values ([(e* env) (Expr* e* env)]
                   [(body env) (Expr body env)])
       (values `(let ,(map list x* e*) ,body) env))]
    [`(begin ,expr* ...)
     (let-values ([(expr* env) (Expr* expr* env)])
       (values `(begin ,@expr*) env))]
    [`(funcall ,f ,e* ...)
     (let*-values ([(f env) (Expr f env)]
                   [(e* env) (Expr* e* env)])
       (values `(funcall ,f ,@e*) env))]
    [`(primcall ,pr ,e* ...)
     (let-values ([(e* env) (Expr* e* env)])
       (values `(primcall ,pr ,@e*) env))]
    [`(closure ,label ,free* ...)
     (let-values ([(free* env) (Expr* free* env)])
       (values `(closure ,label ,@free*) env))]
    [`(tailcall ,f ,e* ...)
     (let*-values ([(f env) (Expr f env)]
                   [(e* env) (Expr* e* env)])
       (values `(tailcall ,f ,@e*) env))]
    [_ (values expr env)]))

(define (Expr* expr* env)
  (let loop ([expr* expr*] [env env] [result '()])
    (if (null? expr*)
      (values (reverse result) env)
      (let-values ([(expr env) (Expr (car expr*) env)])
        (loop (cdr expr*) env (cons expr result))))))

(define (complex-datum? datum)
  (or (pair? datum)
      (vector? datum)
      (string? datum)
      (symbol? datum)))

(define (generate-initializers interning-map)
  (reverse
    (map (lambda (entry)
           (let ([datum (car entry)]
                 [label (cdr entry)])
             `(primcall constant-init ,label
                        ,(expand-to-construction datum interning-map))))
         interning-map)))

(define (expand-to-construction datum interning-map)
  (cond
    [(pair? datum)
     (let ([car-entry (assoc (car datum) interning-map)]
           [cdr-entry (assoc (cdr datum) interning-map)])
       `(primcall cons
                  ,(if car-entry
                     `(primcall constant-ref ,(cdr car-entry))
                     (expand-to-construction (car datum) interning-map))
                  ,(if cdr-entry
                     `(primcall constant-ref ,(cdr cdr-entry))
                     (expand-to-construction (cdr datum) interning-map))))]
    [(vector? datum)
     (let ([len (vector-length datum)])
       (if (= len 0)
         `(primcall make-vector '0 (primcall void))
         `(let ([v (primcall make-vector ',len (primcall void))])
            (begin
              ,@(for/list ([i (in-range len)])
                  `(primcall vector-set! v ',i
                             ,(expand-to-construction (vector-ref datum i) interning-map)))
              v))))]
    [(string? datum)
     `(primcall string ,@(map (lambda (c) `',c)
                              (string->list datum)))]
    [(symbol? datum)
     `(primcall string->symbol
                ,(expand-to-construction (symbol->string datum) interning-map))]
    [else
     `(quote ,datum)]))

(module+ test
  (require rackunit)
  (check-equal?
    (remove-complex-constants
      '(labels ((f (code (x) () (primcall + x '1))))
         (primcall f '42)))
    '(labels ((f (code (x) () (primcall + x '1))))
       (begin (primcall f '42))))
  (check-match
    (remove-complex-constants
      '(labels ()
         '(1 2 3)))
    `(labels ((,label (datum)))
       (begin
         (primcall constant-init ,label ,_)
         (primcall constant-ref ,label)))))
