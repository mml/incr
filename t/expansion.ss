(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")

  (define (runtests)
    (test-cases "macro expansion"
      (test-case (and) "#t")
      (test-case (and 1) "1")
      (test-case (and 1 2) "2")
      (test-case (and 1 2 3) "3")
      (test-case (and 1 2 3 #f) "#f")

      (test-case (or) "#f")
      (test-case (or 1) "1")
      (test-case (or #f 2) "2")
      (test-case (or #f 2 3) "2")
      (test-case (or #f #f 3) "3")
      (test-case (or #f #f #f 4) "4")

      (test-case
        (let ([sum (lambda (x y sum)
                     (cond
                       [(zero? x) y]
                       [(zero? y) x]
                       [(< y x) (sum (add1 x) (sub1 y) sum)]
                       [(< x y) (sum (add1 y) (sub1 x) sum)]
                       [else (* 2 x)]))])
          (cons (sum 10 0 sum)
                (cons (sum 0 10 sum)
                      (cons (sum 8 2 sum)
                            (cons (sum 2 8 sum)
                                  (cons (sum 5 5 sum) '()))))))
        "(10 10 10 10 10)")
        
      (test-case
        (list 1 2 3 4 5 6 7 8 9 10)
        "(1 2 3 4 5 6 7 8 9 10)")

      (test-case
        (let ([=q-helper
                (lambda (x alist =q-helper)
                  (cond
                    [(null? alist)
                     #f]
                    [(= (car (car alist)) x)
                     (car alist)]
                    [else (=q-helper x (cdr alist))]))])
          (let ([=q (lambda (x alist) (=q-helper x alist =q-helper))]
                [al (cons (cons 1 2) (cons (cons 3 4) '()))])
            (let ([val (lambda (x)
                         (cond
                           [(=q x al) => (lambda (as) (cdr as))]
                           [else #f]))])
              (cons (val 1)
                    (cons (val 3)
                          (cons (val 5) '()))))))
        "(2 4 #f)")

      (test-case
        (letrec ([len (lambda (l)
                        (cond
                          [(null? l) 0]
                          [else (add1 (len (cdr l)))]))])
          (len (list 1 2 3 4 5 6)))
        "6")

      ;;; Letrec bug
      ; TODO(mml): both of these should fail with a runtime exception about
      ; using a variable before it's defined
      #;(test-case (letrec ([a 10] [b (+ a a)]) (+ a b)) "30")
      #;(test-case
        (let* ([size 10000] [v (make-vector size 1)])
          (vector-length v))
        "10000")
    )))
