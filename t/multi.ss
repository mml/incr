(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "Multiple expressions"
      ; Two expressions - last is result
      (test-case
        (+ 1 2)
        (* 3 4)
        "12")

      ; Variable definition then use
      (test-case
        (define x 10)
        (+ x 5)
        "15")

      ; Multiple definitions then expression
      (test-case
        (define x 5)
        (define y 10)
        (+ x y)
        "15")

      ; Function definition and call
      (test-case
        (define (double x) (* 2 x))
        (double 21)
        "42"))))
