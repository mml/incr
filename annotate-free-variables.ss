#lang racket

(require racket/match)
(require racket/set)
(require racket/trace)
(provide annotate-free-variables)

(define (annotate-free-variables expr)
  (let-values ([(aexpr env) (annotate-expr expr)])
    aexpr))

(define (binop? x) (member x '(* +)))

; annotate-expr is called with expr and env (a list of names bound in enclosing forms)
; it returns two values: an expression and a list of free variables found in enclosed forms
(define annotate-expr (lambda (expr) (match expr
  [(? number? x)
   (values x (set))]
  [(? symbol? ident)
   (values ident (set ident))]
  [`(,(? binop? op) ,x ,y)
    (let-values ([(x xrefs) (annotate-expr x)]
                 [(y yrefs) (annotate-expr y)])
      (values `(,op ,x ,y)
              (set-union xrefs yrefs)))]
  [`(let () ,x)
    (let-values ([(x refs) (annotate-expr x)])
      (values `(let () ,x)
              refs))]
  [`(let ([,var ,val]) ,x)
    (let-values ([(x xrefs) (annotate-expr x)]
                 [(val vrefs) (annotate-expr val)])
      (values `(let ([,var ,val]) ,x)
              (set-remove (set-union xrefs vrefs) var)))]
  [`(lambda (,formal ___) ,body)
    (let-values ([(body refs) (annotate-expr body)])
      (let f ([refs refs] [free '()])
        (cond
          [(set-empty? refs)
           (values `(lambda (,@formal) (,@free) ,body)
                   (set))]
          [else (let* ([ref (set-first refs)]
                       [new-free (if (member ref formal)
                                     free
                                     (cons ref free))])
                  (f (set-rest refs) new-free))]
          )))]
  [`(,f ,arg ___)
    (let-values
      ([(arg* ref*)
        (for/lists (args refs)
                   ([arg arg])
                   (annotate-expr arg))])
      (let-values ([(f fref) (annotate-expr f)])
        (values `(,f ,@arg*) (set-union fref (apply set-union ref*)))))]
  )))
