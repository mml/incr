#lang nanopass

;;; Beginnings of importing nanopass.
;;; Goal here is not to rewrite the whole compiler (yet), but to use it mainly
;;; to make writing uncover-free and convert-closures easier.

(require racket/trace)

(require (for-syntax syntax/parse
                     syntax/stx
                     nanopass/base
                     racket/syntax))

(provide parse-Lsrc unparse-Lsrc
         parse-and-rename
         make-begin-explicit
         parse-L1 unparse-L1
         )

(define (immediate-constant? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)))

(define unary-primitives '(add1 sub1 zero? not null? integer->char char->integer))
(define binary-primitives '(+ - = < * cons car cdr cadr caddr cadddr))
(define primitives (append (map (lambda (pr) (cons pr 1)) unary-primitives)
                           (map (lambda (pr) (cons pr 2)) binary-primitives)))

(define (primitive? x)
  (assq x primitives))

(define (variable? x)
  (and (symbol? x)
       (not (primitive? x)))) ; substitute for properly tracking bindings

(define unique-var
  (let ()
    (define next 0)
    (lambda (var)
      (let ([suffix next])
        (set! next (add1 next))
        (string->symbol
          (string-append (symbol->string var) "." (number->string suffix)))))))

(define-language Lsrc
  (terminals
    (immediate-constant (c))
    (variable (x))
    (primitive (pr)))
  (Expr (e body)
    c
    x
    (pr e* ...)
    (begin e* ... e)
    (let ([x* e*] ...) body* ... body)
    (if e0 e1 e2)
    (lambda (x* ...) body* ... body)))
(define-parser parse-Lsrc Lsrc)

(trace-define-pass parse-and-rename : * (e) -> Lsrc ()
  (definitions
    (define (Expr* e* env)
      (map (lambda (e) (Expr e env)) e*))
    (define (build-primitive as)
      (let ([pr (car as)] [n (cdr as)])
        (cons pr (lambda (env . e1...)
                   (if (= n (length e1...))
                       `(,pr ,(Expr* e1... env) ...)
                       (error pr "arity-mismatch: expected ~a got ~a"
                              n (length e1...)))))))
    (define (process-bindings env bindings)
      (let loop ([new-env env] [bindings bindings] [rx* '()] [re* '()])
        (if (null? bindings)
            (values (reverse rx*) (reverse re*) new-env)
            (let* ([x (caar bindings)]
                   [e (cadar bindings)]
                   [ux (unique-var x)])
              (loop (cons (cons x ux) new-env) (cdr bindings)
                    (cons ux rx*) (cons (Expr e env) re*))))))
    (define (process-body env body*)
      (let loop ([body (car body*)] [body* (cdr body*)] [rbody* '()])
        (if (null? body*)
            (values (reverse rbody*) (Expr body env))
            (loop (car body*) (cdr body*) (cons (Expr body env) rbody*)))))
    (define initial-env
      (cons (cons 'let (trace-lambda (env bindings . body*)
                         (let-values ([(x* e* env) (process-bindings env bindings)])
                           (let-values ([(body* body) (process-body env body*)])
                             `(let ([,x* ,e*] ...) ,body* ... ,body)))))
            (map build-primitive primitives))))
  (Expr : * (e env) -> Expr ()
    (cond
      [(pair? e)
       (let ([e0 (car e)] [e1... (cdr e)])
         (cond
           [(assq e0 env) =>
            (lambda (as)
              (let ([v (cdr as)])
                (cond
                  [(number? v)
                   (if (= v (length e1...))
                       `(,e0 ,(Expr* e1... env) ...)
                       (error
                         e0
                         "~a: ~a takes ~a parameters but got ~a"
                         e e0 v (length (cdr e))))]
                  [(procedure? v)
                   (apply v env e1...)]
                  [(symbol? v) `,v]
                  [else (error who "unexpected value ~a" v)])))]
           [else (error who "internal error: ~a unimplemented" e0)]))]
      [(symbol? e) (cond 
                     [(assq e env) => cdr]
                     [else (error who "undefined variable: ~a" e)])]
      [(immediate-constant? e) `,e]
      [else (error who "invalid expression ~a" e)]))
  (printf "~a" initial-env)
  (Expr e initial-env))

;;; As above, but we remove any implicit begins.
(define-language L1
  (extends Lsrc)
  (Expr (e body)
    (- (lambda (x* ...) body* ... body)
       (let ([x* e*] ...) body* ... body))
    (+ (lambda (x* ...) body)
       (let ([x* e*] ...) body))))
(define-parser parse-L1 L1)

(define-pass make-begin-explicit : Lsrc (expr) -> L1 ()
  (Expr : Expr (expr) -> Expr ()
    [(let ([,x ,[e*]] ...) ,[body*] ... ,[body])
     `(let ([,x ,e*] ...) (begin ,body* ... ,body))]
    [(lambda (,x* ...) ,[body*] ... ,[body])
     `(lambda (,x* ...) (begin ,body* ... ,body))]))
