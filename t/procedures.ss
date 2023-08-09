(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
      (test-cases "procedures"
        (test-case
          (let ([ten (lambda () 10)])
            (ten))
          "10")

        (test-case
          (let ([eleven (lambda () (add1 10))])
            (eleven))
          "11")

        (test-case
          (let ([double (lambda (x) (* x 2))])
            (double 10))
          "20")

        (test-case
          (let ([double (lambda (x) (* x 2))])
            (double (double 10)))
          "40")

        (test-case
          (let ([double (lambda (x) (* x 2))]
                [triple (lambda (x) (* x 3))])
            (= (double (triple #xff0000))
               (triple (double #xff0000))))
          "#t")

        (test-case
          (let ([add (lambda (a b) (+ a b))])
            (add 20 20))
          "40")

        (test-case
          (let ([g (lambda (f) (f 20 20))]
                [add (lambda (a b) (+ a b))])
            (g add))
          "40")

        (test-case (let ([add (lambda (x y) (+ x y))]
                         [mul (lambda (x y) (* x y))])
                     (mul
                       (add 10 15)
                       (add 20 25)))
                   "1125")
        
        (test-case (let ([fxid (lambda (n self) (if (zero? n) n (add1 (self (sub1 n) self))))])
                     (fxid 0 fxid))
                   "0")

        (test-case (let ([fxid (lambda (n self) (if (zero? n) n (add1 (self (sub1 n) self))))])
                     (fxid 1 fxid))
                   "1")

        (test-case (let ([fxid (lambda (n self) (if (zero? n) n (add1 (self (sub1 n) self))))])
                     (fxid 2 fxid))
                   "2")

        (test-case (let ([len (lambda (l len) (if (null? l) 0 (+ 1 (len (cdr l) len))))])
                     (len '() len))
                   "0")

        (test-case (let ([mkl (lambda (n self) (if (zero? n) '() (cons #f (self (sub1 n) self))))])
                     (mkl 5 mkl))
                   "(#f #f #f #f #f)")
        (test-case
          (let ([mkl (lambda (n self) (if (zero? n) '() (cons #f (self (sub1 n) self))))]
                [len (lambda (l self) (if (null? l) 0 (add1 (self (cdr l) self))))])
            (len (mkl 5 mkl) len))
          "5")
        (test-case
          (let ([fib (lambda (n self)
                       (if (zero? n) 1
                           (if (= 1 n) 1
                               (+ (self (- n 1) self)
                                  (self (- n 2) self)))))])
            (fib 33 fib))
          "5702887")
        (test-case
          (let ([add (lambda (a b) (+ a b))])
            (let ([c 10] [d 20] [e 30] [f 40] [g 50] [h 60] [i 70] [j 80])
              (* (add (add (add c d) e) f)
                 (add (add (add g h) i) j))))
          "26000")
        )
    ))
