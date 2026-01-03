#lang racket

(provide simplify-binding-forms)

(require rackunit)
(require racket/match)
(require racket/trace)
(require "generators.ss")

;;; This pass eliminates let*, letrec*, and letrec
;;; This pass expects all conditionals reduced to if
;;; But probably works best before we make begin explicit

(define (simplify-binding-forms expr) (Expr expr))

(define Expr
  (lambda (expr)
    (match expr
      [`(quote ,_) expr]
      [(? symbol? s) s]
      [(? string? s) s]
      [`(lambda ,args ,body* __1)
        `(lambda ,args ,@(Expr* body*))]
      [`(begin ,expr* ___)
        `(begin ,@(Expr* expr*))]
      [`(if ,test ,conseq ,altern)
        `(if ,(Expr test) ,(Expr conseq) ,(Expr altern))]
      [`(let ([,x* ,e*] ___) ,expr* __1)
        `(let ,(map list x* (Expr* e*)) ,@(Expr* expr*))]
      [`(let* ([,x* ,e*] ___) ,expr* __1)
        (Let* x* e* expr*)]
      [`(letrec ([,x* ,e*] ___) ,expr* __1)
        (Letrec x* e* expr*)]
      [`(letrec* ([,x* ,e*] ___) ,expr* __1)
        (Letrec* x* e* expr*)]
      [`(primcall ,p ,e* ___)
        `(primcall ,p ,@(Expr* e*))]
      [`(funcall ,f ,e* ___)
        `(funcall ,(Expr f) ,@(Expr* e*))]
      )))

(define Expr*
  (lambda (expr*)
    (map Expr expr*)))

(define Let*
  (lambda (x* e* expr*)
    (cond
      [(null? x*) `(let () ,@(Expr* expr*))]
      [(null? (cdr x*)) `(let ([,(car x*) ,(Expr (car e*))]) ,@(Expr* expr*))]
      [else
        `(let ([,(car x*) ,(Expr (car e*))])
           ,(Let* (cdr x*) (cdr e*) expr*))])))

(define Letrec
  (lambda (x* e* body*)
    (let* ([xbinding* (map (lambda (x) `[,x (primcall void)]) x*)]
           [t* (map (lambda (x) (tmp)) x*)]
           [tbinding* (map (lambda (t e) `[,t ,e])
                           t* (Expr* e*))]
           [s!* (map (lambda (x t) `(primcall set! ,x ,t)) x* t*)])
      `(let ,xbinding*
         (let ,tbinding*
           ,@s!*
           (let () ,@(Expr* body*)))))))

(define Letrec*
  (lambda (x* e* body*)
    (let* ([xbinding* (map (lambda (x) `[,x (primcall void)]) x*)]
           [s!* (map (lambda (x e) `(primcall set! ,x ,e)) x* (Expr* e*))])
      `(let ,xbinding*
         ,@s!*
         (let () ,@(Expr* body*))))))
(module+ test ; Expr
  ; Not sure why failing
  (check-match (Expr '(let ([v (primcall make-vector '5 '0)])
                        (letrec
                          ([uv (lambda (v2 n)
                                 (if (primcall < n '0)
                                     v2
                                     (begin
                                       (primcall vector-set! v2 n n)
                                       (funcall uv v2 (primcall sub1 n)))))])
                          (funcall uv v '4))))
               `(let ([v (primcall make-vector '5 '0)])
                  (let ([uv (primcall void)])
                    (let ([,t (lambda (v2 n)
                                (if (primcall < n '0)
                                    v2
                                    (begin
                                      (primcall vector-set! v2 n n)
                                      (funcall uv v2 (primcall sub1 n)))))])
                      (primcall set! uv ,t)
                      (let () (funcall uv v '4)))))))
