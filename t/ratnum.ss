(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "Division returning fixnums (exact division)"
      ; Basic exact division - returns fixnum
      (test-case
        (/ 6 2)
        "3")

      ; Exact division with negative numbers
      (test-case
        (/ -10 5)
        "-2")

      ; Exact division with both negative
      (test-case
        (/ -8 -2)
        "4")

      ; Division by 1 returns original
      (test-case
        (/ 7 1)
        "7")

      ; Division by -1
      (test-case
        (/ 6 -1)
        "-6")

      ; Zero divided by positive
      (test-case
        (/ 0 5)
        "0"))

    (test-cases "Division returning ratnums (inexact division)"
      ; Basic ratnum - 3/4
      (test-case
        (/ 3 4)
        "3/4")

      ; Ratnum with negative numerator
      (test-case
        (/ -3 4)
        "-3/4")

      ; Ratnum with negative denominator (sign in numerator)
      (test-case
        (/ 3 -4)
        "-3/4")

      ; Both negative (positive result)
      (test-case
        (/ -3 -4)
        "3/4")

      ; Larger ratnum
      (test-case
        (/ 7 11)
        "7/11"))

    (test-cases "GCD normalization"
      ; GCD reduces to lowest terms - (6 8) -> gcd=2 -> 3/4
      (test-case
        (/ 6 8)
        "3/4")

      ; GCD with larger numbers
      (test-case
        (/ 20 30)
        "2/3")

      ; GCD with prime numerator
      (test-case
        (/ 5 15)
        "1/3")

      ; Already in lowest terms
      (test-case
        (/ 7 9)
        "7/9"))

    (test-cases "Predicate tests"
      ; rational? true for fixnums
      (test-case
        (rational? 5)
        "#t")

      ; rational? false for non-numbers
      (test-case
        (rational? #t)
        "#f")

      ; number? true for fixnums
      (test-case
        (number? 10)
        "#t")

      ; exact? true for fixnums
      (test-case
        (exact? 7)
        "#t")

      ; integer? true for fixnums
      (test-case
        (integer? -3)
        "#t"))

    (test-cases "Accessor tests"
      ; numerator of fixnum is itself
      (test-case
        (numerator 5)
        "5")

      ; denominator of fixnum is 1
      (test-case
        (denominator 5)
        "1")

      ; numerator of negative fixnum
      (test-case
        (numerator -7)
        "-7")

      ; denominator of negative fixnum
      (test-case
        (denominator -7)
        "1"))

    #;(test-cases "Fixnum arithmetic with division"
      ; Chain of divisions
      (test-case
        (/ (/ 12 2) 3)
        "2")

      ; Division in arithmetic
      (test-case
        (+ (/ 1 2) (/ 1 2))
        "1"))

))
