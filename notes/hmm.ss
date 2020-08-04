(require nanopass/base)
(require racket/trace)

(define (variable? x)
  (and (symbol? x)
       (not (primitive? x))))

(define (primitive? x)
  (memq x '(+ plus)))

(define (immediate-constant? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)))

(define-language L0
  (terminals
    (primitive (pr))
    (immediate-constant (c))
    (variable (x y)))
  (Expr (e body)
    pr
    c
    x
    (begin body* ... body)
    (let ([x* e*] ...) body* ... body)
    (lambda (x* ...) body* ... body)
    (e0 e1 ...)))

(define-parser parse-L0 L0)

(define-language L1
  (extends L0)
  (Expr (e body)
    (+ (lambda (x* ...) (y* ...) body* ... body))
    (- (lambda (x* ...) body* ... body))))

(define-parser parse-L1 L1)

(trace-define-pass uncover-free : L0 (e) -> L1 ()
  (definitions
    (define (set-cons x set)
      (if (memq x set)
          set
          (cons x set)))
    (define (union . set*)
      (if (null? set*)
          '()
          (foldl (lambda (setb seta)
                   (let loop ([setb setb] [seta seta])
                     (if (null? setb)
                         seta
                         (loop (cdr setb) (set-cons (car setb) seta)))))
                 (car set*) (cdr set*))))
    (define (intersect . set*)
      (if (null? set*)
          '()
          (foldl (lambda (setb seta)
                   (let loop ([seta seta] [fset '()])
                     (if (null? seta)
                         fset
                         (let ([a (car seta)])
                           (if (memq a setb)
                               (loop (cdr seta) (cons a fset))
                               (loop (cdr seta) fset))))))
                 (car set*) (cdr set*))))
    (define (difference . set*)
      (if (null? set*)
          '()
          (foldr (lambda (setb seta)
                   (let loop ([seta seta] [fset '()])
                     (if (null? seta)
                         fset
                         (let ([a (car seta)])
                           (if (memq a setb)
                               (loop (cdr seta) fset)
                               (loop (cdr seta) (cons a fset)))))))
                 (car set*) (cdr set*)))))
  (Expr : Expr (e) -> Expr (free*)
    [,pr (values pr '())]
    [,c (values c '())]
    [,x (values x (list x))]
    [(begin ,[body* free**] ... ,[body free*])
     (let ([free* (union free* (apply union free**))])
       (values `(begin ,body* ... ,body) free*))]
    [(,[e0 free*] ,[e1 free**] ...)
     (values `(,e0 ,e1 ...)
             (union free* (apply union free**)))]
    [(lambda (,x* ...) ,[body* free**] ... ,[body free*])
     (let ([free* (difference (union free* (apply union free**)) x*)])
       (printf "free* is ->~a<-~n" free*)
       (values `(lambda (,x* ...) (,free* ...) ,body* ... ,body)
               '()))])
  (let-values ([(e f) (Expr e)])
    (unless (null? f) (err 'uncover-free "Free variables at top: ~a" f))
    e))
