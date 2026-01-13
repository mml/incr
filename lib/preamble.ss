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
  (if (fixnum? x)
      (if (fixnum? y)
          (fx= x y)
          #f)
      (if (fixnum? y)
          #f
          (and (= (numerator x) (numerator y))
               (= (denominator x) (denominator y))))))

;; < : Less-than comparison
;; Cross-multiplication for ratnum comparisons:
;;   a/b < c/d  ⟺  a*d < c*b
(define (< x y)
  (if (fixnum? x)
      (if (fixnum? y)
          (fx< x y)
          ; x < c/d  ⟺  x*d < c
          (< (* (denominator y) x) (numerator y)))
      ; a/b < y
      (if (fixnum? y)
          ; a/b < y  ⟺  a < b*y
          (< (numerator x) (* (denominator x) y))
          ; a/b < c/d  ⟺  a*d < c*b
          (< (* (numerator x) (denominator y))
             (* (numerator y) (denominator x))))))

;; > : Greater-than comparison
;; Cross-multiplication for ratnum comparisons:
;;   a/b > c/d  ⟺  a*d > c*b
(define (> x y)
  (if (fixnum? x)
      (if (fixnum? y)
          (fx> x y)
          ; x > c/d  ⟺  x*d > c
          (> (* (denominator y) x) (numerator y)))
      ; a/b > y
      (if (fixnum? y)
          ; a/b > y  ⟺  a > b*y
          (> (numerator x) (* (denominator x) y))
          ; a/b > c/d  ⟺  a*d > c*b
          (> (* (numerator x) (denominator y))
             (* (numerator y) (denominator x))))))

;; <= : Less-than-or-equal comparison (defined as not(>))
(define (<= x y)
  (not (> x y)))

;; >= : Greater-than-or-equal comparison (defined as not(<))
(define (>= x y)
  (not (< x y)))
