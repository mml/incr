(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "Additional numeric primitives"
      ; <=
      (test-case (<= 1 2) "#t")
      (test-case (<= 2 2) "#t")
      (test-case (<= 2 1) "#f")
      (test-case (<= 0 0) "#t")
      (test-case (<= -5 -3) "#t")
      (test-case (<= -3 -5) "#f")

      ; >=
      (test-case (>= 2 1) "#t")
      (test-case (>= 2 2) "#t")
      (test-case (>= 1 2) "#f")
      (test-case (>= 0 0) "#t")
      (test-case (>= -3 -5) "#t")
      (test-case (>= -5 -3) "#f")

      ; min
      (test-case (min 3 5) "3")
      (test-case (min 5 3) "3")
      (test-case (min 7 7) "7")
      (test-case (min -10 -20) "-20")
      (test-case (min 0 5) "0")
      (test-case (min -5 0) "-5")

      ; max
      (test-case (max 3 5) "5")
      (test-case (max 5 3) "5")
      (test-case (max 7 7) "7")
      (test-case (max -10 -20) "-10")
      (test-case (max 0 5) "5")
      (test-case (max -5 0) "0")

      ; abs
      (test-case (abs 5) "5")
      (test-case (abs -5) "5")
      (test-case (abs 0) "0")
      (test-case (abs 123) "123")
      (test-case (abs -123) "123")

      ; odd?
      (test-case (odd? 1) "#t")
      (test-case (odd? 2) "#f")
      (test-case (odd? 0) "#f")
      (test-case (odd? -1) "#t")
      (test-case (odd? -2) "#f")
      (test-case (odd? 99) "#t")
      (test-case (odd? 100) "#f")

      ; even?
      (test-case (even? 1) "#f")
      (test-case (even? 2) "#t")
      (test-case (even? 0) "#t")
      (test-case (even? -1) "#f")
      (test-case (even? -2) "#t")
      (test-case (even? 99) "#f")
      (test-case (even? 100) "#t")

      ; positive?
      (test-case (positive? 1) "#t")
      (test-case (positive? 0) "#f")
      (test-case (positive? -1) "#f")
      (test-case (positive? 100) "#t")
      (test-case (positive? -100) "#f")

      ; negative?
      (test-case (negative? 1) "#f")
      (test-case (negative? 0) "#f")
      (test-case (negative? -1) "#t")
      (test-case (negative? 100) "#f")
      (test-case (negative? -100) "#t")

      ; Combined tests
      (test-case (min (abs -10) (abs 5)) "5")
      (test-case (max (abs -10) (abs 5)) "10")
      (test-case (odd? (min 3 4)) "#t")
      (test-case (even? (max 3 4)) "#t")
      (test-case (positive? (abs -5)) "#t")
      (test-case (negative? (abs -5)) "#f")
      )))
