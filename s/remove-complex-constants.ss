#lang racket

;;; This pass lifts all complex constants to a single enclosing let.
;;; After this pass, the only arguments to quote are immediates.

(provide remove-complex-constants)

(require racket/match)
(require racket/trace)
(require srfi/1)
(require "generators.ss")
(require "terminals.ss")

(define (remove-complex-constants expr)
  (let-values ([(expr bindings) (Expr expr)])
    (if (null? bindings)
      expr
      `(let ,bindings ,expr))))

(define (datum->code x)
  (match x
    [`(,x . ,y) `(primcall cons ,(datum->code x) ,(datum->code y))]
    [(? string? s)
     (let ([c* (string->list s)])
       `(primcall string ,@(map datum->code c*)))]
    [(? symbol? sym)
     (let ([c* (string->list (symbol->string sym))])
       `(primcall string->symbol (primcall string ,@(map datum->code c*))))]
    ['#() `(primcall make-vector '0 (primcall void))]
    [`#(,x* __1)
     (let* ([vec-var (tmp)]
            [set-cmds (let loop ([i 0] [elems x*])
                        (if (null? elems)
                            '()
                            (cons `(primcall vector-set! ,vec-var ',i ,(datum->code (car elems)))
                                  (loop (add1 i) (cdr elems)))))])
       `(let ([,vec-var (primcall make-vector ',(length x*) (primcall void))])
          (begin ,@set-cmds ,vec-var)))]
    [(? immediate? v) `',v]))

(define (Expr expr)
  (match expr
    [`(quote ,(? immediate? datum))
      (values expr '())]
    [`(quote ,datum)
      (let ([t (tmp)])
        (values t `((,t ,(datum->code datum)))))]
    [(? string? s)
     (let ([t (tmp)])
       (values t `((,t ,(datum->code s)))))]
    [(? variable? x) (values expr '())]
    [`(if ,test ,conseq ,altern)
      (let-values ([(t tb) (Expr test)]
                   [(c cb) (Expr conseq)]
                   [(a ab) (Expr altern)])
        (values `(if ,t ,c ,a)
                (append tb cb ab)))]
    [`(begin ,expr* __1)
      (let-values ([(expr* bind) (Expr* expr*)])
        (values `(begin ,@expr*) bind))]
    [`(let ([,x* ,e*] ___) ,body)
      (let-values ([(e* eb) (Expr* e*)]
                   [(body bb) (Expr body)])
        (let ([bindings (map list x* e*)])
          (values `(let ,bindings ,body)
                  (append eb bb))))]
    [`(lambda ,formal* ,body)
      (let-values ([(body bb) (Expr body)])
        (values `(lambda ,formal* ,body) bb))]
    [`(funcall ,e* ___)
      (let-values ([(e* eb) (Expr* e*)])
        (values `(funcall ,@e*) eb))]
    [`(primcall ,pr ,e* ___)
      (let-values ([(e* eb) (Expr* e*)])
        (values `(primcall ,pr ,@e*) eb))]
    ))

(define (Expr* expr*)
  (let loop ([expr* expr*] [rexpr* '()] [bind '()])
    (if (null? expr*)
      (values (reverse rexpr*) bind)
      (let-values ([(expr b) (Expr (car expr*))])
        (loop (cdr expr*) (cons expr rexpr*) (append bind b))))))

(module+ test
  (require rackunit)

  (check-equal? (remove-complex-constants ''5) ''5)
  (check-equal? (remove-complex-constants ''()) ''())
  (check-match (remove-complex-constants ''(1 . 2))
               `(let ([,t (primcall cons '1 '2)])
                  ,t))
  (check-match (remove-complex-constants ''(1))
               `(let ([,t (primcall cons '1 '())])
                  ,t))

  (check-match (remove-complex-constants ''(1 2))
               `(let ([,t (primcall cons '1 (primcall cons '2 '()))])
                  ,t))

  (check-match (remove-complex-constants '"")
               `(let ([,t (primcall string)])
                  ,t))

  (check-match (remove-complex-constants '"a")
               `(let ([,t (primcall string '#\a)])
                  ,t))

  (check-match (remove-complex-constants ''foo)
               `(let ([,t (primcall string->symbol (primcall string '#\f '#\o '#\o))])
                  ,t))

  (check-match (remove-complex-constants ''#())
               `(let ([,t (primcall make-vector '0 (primcall void))])
                  ,t))

  (check-match (remove-complex-constants ''#(1))
               `(let ([,t (let ([,vec (primcall make-vector '1 (primcall void))])
                            (begin (primcall vector-set! ,vec '0 '1) ,vec))])
                  ,t))
  )
