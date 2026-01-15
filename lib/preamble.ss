;; Standard library preamble - included in all compiled programs

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

;;; Comparison Operators with Ratnum Support

;; = : Equality comparison
;; Dispatch: fixnum/fixnum -> fx=
;;           ratnum/ratnum -> compare numerators and denominators
;;           mixed types   -> #f (different types are unequal)
(define (= x y)
  (cond
    [(fixnum? x) (and (fixnum? y) (fx= x y))]
    [(fixnum? y) #f]
    [else (and (= (numerator x) (numerator y))
               (= (denominator x) (denominator y)))]))

;; < : Less-than comparison
;; Cross-multiplication for ratnum comparisons:
;;   a/b < c/d  ⟺  a*d < c*b
(define (< x y)
  (cond
    [(fixnum? x)
     (if (fixnum? y)
         (fx< x y)
         (< (* (denominator y) x) (numerator y)))]
    [(fixnum? y) (< (numerator x) (* (denominator x) y))]
    [else (< (* (numerator x) (denominator y))
             (* (numerator y) (denominator x)))]))

;; > : Greater-than comparison
;; Cross-multiplication for ratnum comparisons:
;;   a/b > c/d  ⟺  a*d > c*b
(define (> x y)
  (cond
    [(fixnum? x)
     (if (fixnum? y)
         (fx> x y)
         (> (* (denominator y) x) (numerator y)))]
    [(fixnum? y) (> (numerator x) (* (denominator x) y))]
    [else (> (* (numerator x) (denominator y))
             (* (numerator y) (denominator x)))]))

;; <= : Less-than-or-equal comparison (defined as not(>))
(define (<= x y)
  (not (> x y)))

;; >= : Greater-than-or-equal comparison (defined as not(<))
(define (>= x y)
  (not (< x y)))

;;; Compound car/cdr Accessors

;; cadr : Get second element of a list
;; Equivalent to (car (cdr x))
(define (cadr x)
  (car (cdr x)))

;; cddr : Get the rest of the list starting from the third element
;; Equivalent to (cdr (cdr x))
(define (cddr x)
  (cdr (cdr x)))

;; caddr : Get third element of a list
;; Equivalent to (car (cdr (cdr x)))
(define (caddr x)
  (car (cdr (cdr x))))
