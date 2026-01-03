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

      (test-case        ; from tspl4
        (letrec* ([sum (lambda (x)
                         (if (zero? x)
                           0
                           (+ x (sum (- x 1)))))]
                  [f (lambda () (cons n n-sum))]
                  [n 15]
                  [n-sum (sum n)])
          (f))
        "(15 . 120)")

      (test-case
        (let* () (+ 1 1))
        "2")

      (test-case
        (let* ([x 2]) x)
        "2")

      (test-case
        (let* ([n 0]
               [m 0]
               [incr (lambda ()
                       (set! n (add1 n))
                       (when (< m 10)
                         (set! m (add1 m))))])
          (letrec ([doit (lambda (x)
                           (if (zero? x)
                             (cons n m)
                             (begin
                               (incr)
                               (doit (sub1 x)))))])
            (doit 20)))
        "(20 . 10)")

      (test-case

        (let* ([n 0]
               [m 0]
               [incr (lambda ()
                       (set! n (add1 n))
                       (unless (> n 6)
                         (set! m (+ n m))))])
          (letrec ([doit (lambda (x)
                           (if (zero? x)
                             (cons n m)
                             (begin
                               (incr)
                               (doit (sub1 x)))))])
            (doit 20)))
        "(20 . 21)")

       (test-case
         (case 0
           [(0) #f]
           [(1) #t])
         "#f")
 
       (test-case
         (case 1
           [(0) #f]
           [(1) #t])
         "#t")
 
       (test-case
         (case 2
           [(0) #f]
           [(1) #t]
           [else "neither"])
         "\"neither\"")

       (test-case
         (case 10
           [else 999])
         "999")

       (test-case
         (case 2
           [(1 2 3) "few"]
           [else "many"])
         "\"few\"")

       #;(test-case
         (let ()
           (define x 10)
           (+ x x))
         "20")
 
       #;(test-case
         (let ([x 10])
           (define x 20)
           (define y (+ x x))
           y)
         "40")

       #;(test-case
         (let ()
           (define l '(1 2 3 4 5 6 7 8 9 10))
           (define even?
             (lambda (n)
               (cond
                 [(> n 1) (even? (- n 2))]
                 [(< n -1) (even? (+ n 2))]
                 [else (zero? n)])))
           (define odd?
             (lambda (n)
               (not (even? n))))
           (define sum-odd (lambda (sum)
             (if (null? l)
               sum
               (let ([carl (car l)])
                 (if (odd? carl)
                   (begin
                     (set! l (cdr l))
                     (sum-even (+ (* 10 carl) sum)))
                   (sum-even sum))))))
           (define sum-even (lambda (sum)
             (if (null? l)
               sum
               (let ([carl (car l)])
                 (if (even? carl)
                   (begin
                     (set! l (cdr l))
                     (sum-odd (+ carl sum)))
                   (sum-odd sum))))))
           (sum-even 0))
         "280")

      (test-case
        (let ([lookup (lambda (n)
                        (case n
                          [(1) "one"]
                          [(2) "two"]
                          [(3 4 5 6 7 8 9) "single-digit"]
                          [else n]))])
          (letrec ([maplookup (lambda (l)
                                (if (null? l)
                                  '()
                                  (cons (lookup (car l)) (maplookup (cdr l)))))])
            (maplookup '(10 20 30 5 2 1 0))))
        "(10 20 30 \"single-digit\" \"two\" \"one\" 0)")

      ;;; Letrec bug
      ; TODO(mml): both of these should fail with a runtime exception about
      ; using a variable before it's defined
      #;(test-case (letrec ([a 10] [b (+ a a)]) (+ a b)) "30")

      ; Large vector test - works but slow (adds ~1-2s per test run)
      #;(test-case
        (let* ([size 10000] [v (make-vector size 1)])
          (vector-length v))
        "10000")
    )))
