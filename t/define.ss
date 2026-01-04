(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "Internal defines"
      ; Internal defines in lambda - simple function definitions
      (test-case
        ((lambda ()
           (define (add2 x) (add1 (add1 x)))
           (define (add1 x) (+ 1 x))
           (add2 5)))
        "7")

      ; Internal defines with variable definitions
      (test-case
        ((lambda (x)
           (define y (+ x 5))
           y)
         10)
        "15")

      ; Mutual recursion with internal defines
      (test-case
        ((lambda ()
           (define (even? n)
             (if (zero? n) #t (odd? (- n 1))))
           (define (odd? n)
             (if (zero? n) #f (even? (- n 1))))
           (even? 4)))
        "#t")

      ; Function recursion with internal defines
      (test-case
        ((lambda ()
           (define (fact n)
             (if (zero? n) 1 (* n (fact (- n 1)))))
           (fact 5)))
        "120")

      ; Shadowing with internal defines
      (test-case
        ((lambda ()
           (define x 10)
           (define x 20)
           x))
        "20")
      )))
