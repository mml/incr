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

      ; Thunk
      (test-case
        ((lambda ()
           (define (x) 10)
           (x)))
        "10")

      ; Shadowing with calls
      (test-case
        ((lambda ()
           (define (f x) (* 100 x))
           (define (g x) (f (f x)))
           (define (f x) (* 20 x))
           (g 1)))
        "400")
      )

    (test-cases "Internal defines in let"
      ; Internal defines in let - simple function definitions
      (test-case
        (let ()
          (define (add2 x) (add1 (add1 x)))
          (define (add1 x) (+ 1 x))
          (add2 5))
        "7")

      ; Internal defines with variable definitions in let
      (test-case
        (let ()
          (define y (+ 10 5))
          y)
        "15")

      ; Mutual recursion with internal defines in let
      (test-case
        (let ()
          (define (even? n)
            (if (zero? n) #t (odd? (- n 1))))
          (define (odd? n)
            (if (zero? n) #f (even? (- n 1))))
          (even? 4))
        "#t")

      ; Function recursion with internal defines in let
      (test-case
        (let ()
          (define (fact n)
            (if (zero? n) 1 (* n (fact (- n 1)))))
          (fact 5))
        "120")

      ; Shadowing with internal defines in let
      (test-case
        (let ()
          (define x 10)
          (define x 20)
          x)
        "20")

      ; Thunk in let
      (test-case
        (let ()
          (define (x) 10)
          (x))
        "10")

      ; Shadowing with calls in let
      (test-case
        (let ()
          (define (f x) (* 100 x))
          (define (g x) (f (f x)))
          (define (f x) (* 20 x))
          (g 1))
        "400")
      )

    (test-cases "Let bindings shadowed by defines"
      ; Define shadows let binding (variable)
      (test-case
        (let ([x 10])
          (define x 20)
          x)
        "20")

      ; Define shadows let binding (function)
      (test-case
        (let ([f (lambda (x) (+ 1 x))])
          (define f (lambda (x) (* 2 x)))
          (f 5))
        "10")

      ; Multiple let bindings, some shadowed
      (test-case
        (let ([x 10] [y 20])
          (define x 100)
          (+ x y))
        "120")

      ; Define references shadowed let binding before shadowing
      (test-case
        (let ([y (lambda () 5)])
          (define (x) (y))
          (define (y) 10)
          (+ (x) (y)))
        "20")

      ; Nested lets with shadowing - inside inner let
      (test-case
        (let ([x 1])
          (let ()
            (define x 2)
            x))
        "2")

      ; Nested lets with shadowing - after inner let
      (test-case
        (let ([x 1])
          (let ()
            (define x 2)
            x)
          x)
        "1")
      )

    (test-cases "Internal defines in let*"
      ; Simple variable definition in let*
      (test-case
        (let* ()
          (define x 10)
          x)
        "10")

      ; Forward references in let* with defines
      (test-case
        (let* ()
          (define x 5)
          (define y (+ x 10))
          y)
        "15")

      ; Function definition in let*
      (test-case
        (let* ()
          (define (double x) (* 2 x))
          (double 5))
        "10")

      ; Mutual recursion in let*
      (test-case
        (let* ()
          (define (even? n)
            (if (zero? n) #t (odd? (- n 1))))
          (define (odd? n)
            (if (zero? n) #f (even? (- n 1))))
          (even? 4))
        "#t")
      )

    (test-cases "Internal defines in letrec"
      ; Simple variable definition in letrec
      (test-case
        (letrec ()
          (define x 10)
          x)
        "10")

      ; Function definition in letrec
      (test-case
        (letrec ()
          (define (f x) (+ x 1))
          (f 5))
        "6")

      ; Mutual recursion in letrec
      (test-case
        (letrec ()
          (define (even? n)
            (if (zero? n) #t (odd? (- n 1))))
          (define (odd? n)
            (if (zero? n) #f (even? (- n 1))))
          (even? 4))
        "#t")
      )

    (test-cases "Internal defines in letrec*"
      ; Simple variable definition in letrec*
      (test-case
        (letrec* ()
          (define x 10)
          x)
        "10")

      ; Function definition in letrec*
      (test-case
        (letrec* ()
          (define (f x) (+ x 1))
          (f 5))
        "6")

      ; Mutual recursion in letrec*
      (test-case
        (letrec* ()
          (define (even? n)
            (if (zero? n) #t (odd? (- n 1))))
          (define (odd? n)
            (if (zero? n) #f (even? (- n 1))))
          (even? 4))
        "#t")
      )))
